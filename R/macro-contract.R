#' Empty macro-contract parameter table
#' @noRd
empty_macro_parameters <- function() {
  tibble::tibble(
    position = integer(), name = character(), kind = character(),
    sas_default = character(), default_status = character(),
    default_key = character(), r_default = list()
  )
}

#' Stable representation for a numeric default
#' @noRd
macro_numeric_key <- function(value) {
  as.character(as.numeric(value))
}

#' A normalized macro default that can be checked against an R AST literal
#' @noRd
known_macro_default <- function(value, type) {
  key_value <- if (identical(type, "numeric")) {
    macro_numeric_key(value)
  } else {
    as.character(value)
  }
  list(
    default_status = "known",
    default_key = paste0(type, ":", key_value),
    r_default = value
  )
}

#' A macro default whose expansion or expression cannot be proven
#' @noRd
unresolved_macro_default <- function(source) {
  list(
    default_status = "unresolved",
    default_key = paste0("unresolved:", source),
    r_default = NULL
  )
}

#' Whether one SAS quoted literal can be decoded without macro expansion
#' @noRd
is_simple_sas_quoted_literal <- function(value) {
  if (!is.character(value) || length(value) != 1L || nchar(value) < 2L) {
    return(FALSE)
  }
  quote <- substr(value, 1L, 1L)
  if (!quote %in% c("'", '"') || !identical(substr(value, nchar(value), nchar(value)), quote)) {
    return(FALSE)
  }
  body <- substr(value, 2L, nchar(value) - 1L)
  chars <- strsplit(body, "", fixed = TRUE)[[1]]
  i <- 1L
  while (i <= length(chars)) {
    if (identical(chars[i], quote)) {
      if (i == length(chars) || !identical(chars[i + 1L], quote)) return(FALSE)
      i <- i + 2L
    } else {
      i <- i + 1L
    }
  }
  if (identical(quote, '"') &&
      grepl("(&|%)[A-Za-z_][A-Za-z0-9_]*", body)) return(FALSE)
  TRUE
}

#' Decode a SAS single- or double-quoted literal already checked as simple
#' @noRd
decode_simple_sas_quoted_literal <- function(value) {
  quote <- substr(value, 1L, 1L)
  body <- substr(value, 2L, nchar(value) - 1L)
  gsub(paste0(quote, quote), quote, body, fixed = TRUE)
}

#' Abort with the macro-contract error convention
#' @noRd
abort_macro_contract <- function(message) {
  cli::cli_abort(message, class = "sas2r_macro_contract_error")
}

#' Split a source parameter list using its one lexical scan
#' @noRd
macro_parameter_ranges <- function(params, chars, mask) {
  if (!length(chars) || !nzchar(trimws(params))) return(matrix(integer(), ncol = 2L))

  starts <- integer()
  ends <- integer()
  start <- 1L
  depth <- 0L
  for (i in seq_along(chars)) {
    if (!identical(mask[i], "c")) next
    if (identical(chars[i], "(")) {
      depth <- depth + 1L
    } else if (identical(chars[i], ")")) {
      if (depth == 0L) abort_macro_contract("macro parameter list has an unmatched closing parenthesis")
      depth <- depth - 1L
    } else if (identical(chars[i], ",") && depth == 0L) {
      starts <- c(starts, start)
      ends <- c(ends, i - 1L)
      start <- i + 1L
    }
  }
  if (depth != 0L) abort_macro_contract("macro parameter list has an unmatched opening parenthesis")
  cbind(start = c(starts, start), end = c(ends, length(chars)))
}

#' Locate the first top-level code equals sign in one parameter range
#' @noRd
macro_parameter_equals <- function(chars, mask, start, end) {
  depth <- 0L
  for (i in seq.int(start, end)) {
    if (!identical(mask[i], "c")) next
    if (identical(chars[i], "(")) {
      depth <- depth + 1L
    } else if (identical(chars[i], ")")) {
      depth <- depth - 1L
    } else if (identical(chars[i], "=") && depth == 0L) {
      return(i)
    }
  }
  NA_integer_
}

#' Normalize one SAS macro default only when it is statically provable
#' @noRd
normalize_macro_default <- function(source) {
  if (!nzchar(source)) return(known_macro_default("", "character"))
  if (grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", source, perl = TRUE)) {
    return(known_macro_default(as.numeric(source), "numeric"))
  }
  if (is_simple_sas_quoted_literal(source)) {
    return(known_macro_default(decode_simple_sas_quoted_literal(source), "character"))
  }
  if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", source)) {
    return(known_macro_default(source, "character"))
  }
  unresolved_macro_default(source)
}

#' Parse the public interface of one SAS macro definition
#' @noRd
parse_macro_contract <- function(name, params) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
    abort_macro_contract("macro name is malformed")
  }
  if (!is.character(params) || length(params) != 1L || is.na(params)) {
    abort_macro_contract("macro parameters must be one character string")
  }

  scan <- sas_scan(params)
  ranges <- macro_parameter_ranges(params, scan$chars, scan$mask)
  if (!nrow(ranges)) {
    return(list(name = name, parameters = empty_macro_parameters()))
  }

  rows <- vector("list", nrow(ranges))
  saw_keyword <- FALSE
  parameter_names <- character()
  for (i in seq_len(nrow(ranges))) {
    start <- ranges[i, "start"]
    end <- ranges[i, "end"]
    raw <- trimws(paste(scan$chars[seq.int(start, end)], collapse = ""))
    if (!nzchar(raw)) abort_macro_contract("macro parameter list contains an empty parameter")

    equals <- macro_parameter_equals(scan$chars, scan$mask, start, end)
    kind <- if (is.na(equals)) "positional" else "keyword"
    if (identical(kind, "positional") && saw_keyword) {
      abort_macro_contract("a positional macro parameter cannot follow a keyword parameter")
    }
    if (identical(kind, "keyword")) saw_keyword <- TRUE

    parameter_name <- if (is.na(equals)) raw else {
      trimws(paste(scan$chars[seq.int(start, equals - 1L)], collapse = ""))
    }
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", parameter_name)) {
      abort_macro_contract("macro parameter name is malformed")
    }
    normalized_name <- tolower(parameter_name)
    if (normalized_name %in% parameter_names) {
      abort_macro_contract("macro parameter names must be unique")
    }
    parameter_names <- c(parameter_names, normalized_name)

    sas_default <- if (is.na(equals)) "" else {
      if (equals == end) "" else trimws(paste(scan$chars[seq.int(equals + 1L, end)], collapse = ""))
    }
    default <- normalize_macro_default(sas_default)
    rows[[i]] <- tibble::tibble(
      position = as.integer(i), name = parameter_name, kind = kind,
      sas_default = sas_default, default_status = default$default_status,
      default_key = default$default_key, r_default = list(default$r_default)
    )
  }

  list(name = name, parameters = do.call(rbind, rows))
}

#' Build the parsed macro contract for exactly one current project unit
#' @noRd
macro_contract_for_unit <- function(project, unit_id) {
  defs <- project$macros$defs
  if (is.null(defs) || !all(c("name", "params", "unit_id") %in% names(defs))) {
    abort_macro_contract("project has no usable macro definitions")
  }
  matches <- defs[defs$unit_id == unit_id, , drop = FALSE]
  if (nrow(matches) != 1L) {
    abort_macro_contract(
      paste0("unit ", unit_id,
             " must contain exactly one macro definition to build a contract")
    )
  }
  parse_macro_contract(matches$name[[1L]], matches$params[[1L]])
}

#' Normalize one R AST literal to the same key used by known macro defaults
#' @noRd
r_ast_default_key <- function(expr) {
  if (is.character(expr) && length(expr) == 1L) {
    return(paste0("character:", expr))
  }
  if ((is.double(expr) || is.integer(expr)) && length(expr) == 1L && !is.na(expr)) {
    return(paste0("numeric:", macro_numeric_key(expr)))
  }
  if (is.call(expr) && length(expr) == 2L &&
      as.character(expr[[1L]]) %in% c("+", "-") &&
      (is.double(expr[[2L]]) || is.integer(expr[[2L]])) &&
      length(expr[[2L]]) == 1L && !is.na(expr[[2L]])) {
    value <- as.numeric(expr[[2L]])
    if (identical(as.character(expr[[1L]]), "-")) value <- -value
    return(paste0("numeric:", macro_numeric_key(value)))
  }
  NULL
}

#' Whether a function formal omits a default expression
#' @noRd
is_missing_formal_default <- function(expr) {
  identical(expr, quote(expr = ))
}

#' Return only the macro interface validation result
#' @noRd
macro_contract_validation <- function(errors, unresolved) {
  list(
    pass = !length(errors),
    errors = as.character(errors),
    unresolved = as.character(unresolved)
  )
}

#' Validate generated R function formals against a macro contract without execution
#' @noRd
validate_macro_contract <- function(r_code, contract) {
  parameters <- contract$parameters
  unresolved <- parameters$name[parameters$default_status == "unresolved"]
  if (!is.character(r_code) || length(r_code) != 1L || is.na(r_code)) {
    return(macro_contract_validation("R code must be one character string", unresolved))
  }

  expressions <- tryCatch(
    parse(text = r_code),
    error = function(error) error
  )
  if (inherits(expressions, "error")) {
    return(macro_contract_validation(
      paste0("R code could not parse: ", conditionMessage(expressions)), unresolved
    ))
  }

  candidates <- Filter(function(expr) {
    is.call(expr) && length(expr) == 3L &&
      as.character(expr[[1L]]) %in% c("<-", "=") &&
      is.name(expr[[2L]]) && identical(as.character(expr[[2L]]), contract$name) &&
      is.call(expr[[3L]]) && identical(as.character(expr[[3L]][[1L]]), "function")
  }, as.list(expressions))
  if (length(candidates) != 1L) {
    return(macro_contract_validation(
      paste0("expected exactly one top-level assignment of ", contract$name, " to function(...); found ", length(candidates)),
      unresolved
    ))
  }

  formals <- candidates[[1L]][[3L]][[2L]]
  actual <- as.list(formals)
  actual_names <- names(actual)
  if (is.null(actual_names)) actual_names <- character()
  expected_names <- parameters$name
  if (!identical(actual_names, expected_names)) {
    return(macro_contract_validation(
      paste0(
        "formal parameter order does not match macro contract: expected ",
        paste(expected_names, collapse = ", "), "; found ",
        paste(actual_names, collapse = ", ")
      ),
      unresolved
    ))
  }

  errors <- character()
  known <- which(parameters$default_status == "known")
  for (i in known) {
    actual_key <- if (is_missing_formal_default(actual[[i]])) NULL else {
      r_ast_default_key(actual[[i]])
    }
    if (!identical(actual_key, parameters$default_key[i])) {
      found <- if (is.null(actual_key)) "no supported literal" else actual_key
      errors <- c(
        errors,
        paste0(
          "formal ", parameters$name[i], " default does not match ",
          parameters$default_key[i], " (found ", found, ")"
        )
      )
    }
  }
  macro_contract_validation(errors, unresolved)
}
