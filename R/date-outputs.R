# SAS dates in delivered datasets
#
# SAS stores a date as a number of days from 1960-01-01 plus a format that
# says to show the number as a date. Generated R keeps the number, so SAS date
# arithmetic translates unchanged. The generated program says which columns of
# a dataset it writes carry a SAS format, in the `formats` argument of
# lib_write(), as the SAS FORMAT statement did. lib_write() stores a column
# given a date format as an R `Date`, and lib_read() hands a later program the
# day number again (R/runtime-data.R).
#
# This file does three things with those declarations, all by parsing the
# generated R, which parses exactly:
#   1. lists a bundle's declared dates for `_sas2r_formats.R`, so a program run
#      in another session reads them back as numbers;
#   2. checks that a declared date format is one the SAS source gives, so a
#      column cannot be typed to match a reference;
#   3. records, for the run report, what was declared and what was stored.
# A reference dataset decides nothing here. It is compared afterwards.
#
# Nothing here follows a variable through the SAS source. How a format is
# inherited, kept and lost is translation knowledge, and lives in the
# `sas-formats-and-dates` skill the translator, reviewer and fixer read.

# The SAS format names haven reads back as `Date`. A reference dataset is read
# with haven, so a format typed here and not there would turn every such column
# into a type difference. lib_write() carries the same list, because the
# runtime is a standalone file; tests/testthat/test-date-outputs.R holds the
# two together, and both to what haven does.
SAS_DATE_FORMAT_PATTERN <-
  "^(DATE|WEEKDATE|(YYMMDD|MMDDYY|DDMMYY)[BCDNPS]?|E8601DA|B8601DA|IS8601DA)[0-9]*$"

#' The date format a SAS format token names, if it names one
#'
#' @param token One format token as written in SAS, such as `yymmdd10.`.
#' @return The upper-case format name without its dot, or `NA_character_`.
#' @noRd
sas_date_format_name <- function(token) {
  hit <- regmatches(token, regexec("^([A-Za-z_][A-Za-z0-9_]*)\\.$", token))[[1]]
  if (length(hit) != 2L) return(NA_character_)
  name <- toupper(hit[2])
  if (grepl(SAS_DATE_FORMAT_PATTERN, name)) name else NA_character_
}

is_sas_format_token <- function(token) grepl("^\\$?[A-Za-z0-9_]*\\.[0-9]*$", token)
is_sas_plain_name <- function(token) {
  grepl("^[A-Za-z_][A-Za-z0-9_]*$", token) &
    !tolower(token) %in% c("_all_", "_numeric_", "_character_", "_char_")
}

# Text that macro processing could change is never read as a declaration.
has_macro_trigger <- function(text) {
  grepl("[%&]", vapply(text, mask_strings, "", keep_double = TRUE, USE.NAMES = FALSE))
}

#' Read the format each variable gets from one FORMAT or ATTRIB statement
#'
#' @param text Statement text without its semicolon.
#' @return `NULL` when the statement cannot be read exactly: a macro reference,
#'   a variable list such as `a--b`, `x1-x5`, `dt:` or `_numeric_`, or any
#'   token outside the plain grammar. Otherwise a named list, one entry per
#'   variable the statement changes, each `list(kind, format)` with `kind`
#'   `"date"`, `"other"` or `"none"` (the format is removed).
#' @noRd
parse_format_declaration <- function(text) {
  if (has_macro_trigger(text)) return(NULL)
  masked <- mask_strings(text)
  masked <- gsub("'[^']*'|\"[^\"]*\"", " Q ", masked)
  keyword <- tolower(sub("^\\s*([A-Za-z]+).*$", "\\1", masked))
  body <- sub("^\\s*[A-Za-z]+", "", masked)
  if (grepl("[^A-Za-z0-9_$.=\\s]", body, perl = TRUE)) return(NULL)
  body <- gsub("\\s*=\\s*", "=", body)
  tokens <- strsplit(trimws(body), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  out <- list()
  entry <- function(token) {
    name <- sas_date_format_name(token)
    if (is.na(name)) list(kind = "other", format = toupper(sub("\\.$", "", token)))
    else list(kind = "date", format = name)
  }
  if (identical(keyword, "format")) {
    pending <- character()
    for (token in tokens) {
      if (grepl("=", token, fixed = TRUE)) return(NULL)
      if (is_sas_format_token(token)) {
        if (!length(pending)) return(NULL)
        for (v in pending) out[[v]] <- entry(token)
        pending <- character()
      } else if (is_sas_plain_name(token)) {
        pending <- c(pending, tolower(token))
      } else return(NULL)
    }
    for (v in pending) out[[v]] <- list(kind = "none", format = NA_character_)
    return(out)
  }
  if (identical(keyword, "attrib")) {
    group <- character()
    closed <- FALSE
    for (token in tokens) {
      if (grepl("=", token, fixed = TRUE)) {
        parts <- strsplit(token, "=", fixed = TRUE)[[1]]
        if (length(parts) != 2L || !length(group)) return(NULL)
        key <- tolower(parts[1])
        if (!key %in% c("label", "format", "informat", "length", "transcode")) return(NULL)
        if (key == "format") {
          if (!is_sas_format_token(parts[2])) return(NULL)
          for (v in group) out[[v]] <- entry(parts[2])
        }
        closed <- TRUE
      } else if (is_sas_plain_name(token)) {
        if (closed) { group <- character(); closed <- FALSE }
        group <- c(group, tolower(token))
      } else return(NULL)
    }
    return(out)
  }
  NULL
}

#' The date format a format written in generated R names, if it names one
#'
#' The reading lib_write() applies to its `formats` argument: `"yymmdd10."`,
#' with or without the period, in any letter case.
#'
#' @param token One format as written in a `formats` argument.
#' @return The upper-case format name without its period, or `NA_character_`.
#' @noRd
declared_date_format_name <- function(token) {
  if (!is.character(token) || length(token) != 1L || is.na(token)) return(NA_character_)
  name <- toupper(sub("[.][0-9]*$", "", trimws(token)))
  if (grepl(SAS_DATE_FORMAT_PATTERN, name)) name else NA_character_
}

#' The formats generated R declares where it writes datasets
#'
#' The code is parsed and never run. Each `lib_write()` call that has a
#' `formats` argument gives one record.
#'
#' @param code R code as one string or several lines.
#' @return A list of `list(dataset, formats)`. `dataset` is `lib.member` in
#'   lower case, or `NA` when the libref or member is not a string constant.
#'   `formats` is a named character vector, lower-case column name to the
#'   format as written, or `NULL` when the argument is not written out as
#'   `c(column = "format.", ...)` with string constants.
#' @noRd
declared_write_formats <- function(code) {
  code <- paste(code %||% "", collapse = "\n")
  exprs <- if (nzchar(trimws(code))) tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(list())
  found <- list()
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  # The parts of a call, without the empty argument of one such as x[, "a"].
  parts <- function(x) {
    x <- as.list(x)
    x[!vapply(seq_along(x), function(i) identical(x[[i]], quote(expr = )), logical(1))]
  }
  head_name <- function(e) {
    head <- e[[1L]]
    if (is.name(head)) return(as.character(head))
    if (is.call(head) && length(head) == 3L && is.name(head[[1L]]) &&
        as.character(head[[1L]]) %in% c("::", ":::") && is.name(head[[3L]])) return(as.character(head[[3L]]))
    ""
  }
  written_out <- function(value) {
    if (!is.call(value) || !identical(head_name(value), "c")) return(NULL)
    entries <- as.list(value)[-1L]
    labels <- names(entries) %||% character()
    if (!length(entries) || length(labels) != length(entries) || !all(nzchar(labels)) ||
        !all(vapply(entries, string, logical(1)))) return(NULL)
    stats::setNames(vapply(entries, identity, ""), tolower(labels))
  }
  visit <- function(e) {
    if (is.call(e) && identical(head_name(e), "lib_write")) {
      arguments <- as.list(e)[-1L]
      labels <- names(arguments) %||% rep("", length(arguments))
      if ("formats" %in% labels) {
        positional <- arguments[!nzchar(labels)]
        pick <- function(label, at) if (label %in% labels) arguments[[match(label, labels)]] else
          if (length(positional) >= at) positional[[at]]
        libref <- pick("libref", 2L)
        member <- pick("member", 3L)
        found[[length(found) + 1L]] <<- list(
          dataset = if (string(libref) && string(member)) tolower(paste(libref, member, sep = ".")) else NA_character_,
          formats = written_out(arguments[[match("formats", labels)]]))
      }
    }
    if (is.call(e) || is.expression(e) || is.pairlist(e)) for (child in parts(e)) visit(child)
  }
  tryCatch(visit(exprs), error = function(e) NULL)
  found
}

#' The date columns a bundle declares, for `_sas2r_formats.R`
#'
#' Built from the generated programs of the bundle. A failure here must not
#' stop a run: lib_write() still stores the dates, and only a program run in
#' another session would read them back as dates.
#'
#' @param code The R code of each program in the bundle, in running order: a
#'   list or character vector with one program per element.
#' @return A named list, output dataset to a named character vector of
#'   column name and SAS date format. Empty when nothing is declared.
#' @noRd
output_date_registry <- function(code = list()) {
  tryCatch({
    registry <- list()
    for (program in code) for (call in declared_write_formats(program)) {
      if (is.na(call$dataset) || is.null(call$formats)) next
      dates <- vapply(call$formats, declared_date_format_name, "")
      # A later write of the same dataset replaces what an earlier one stored.
      registry[[call$dataset]] <- if (any(!is.na(dates))) dates[!is.na(dates)] else NULL
    }
    if (length(registry)) registry else list()
  }, error = function(e) list())
}

#' The date formats the SAS source of a project gives
#'
#' A name-and-format reading of the scanned statements, macro definitions
#' included. It says what the source declares, not where a declaration ends up.
#'
#' @param project A `sas2r_project`.
#' @return `list(variables, formats)`: the variables a `FORMAT` or `ATTRIB`
#'   statement gives a date format by name, and the date formats used in a
#'   statement whose variables cannot be read by name (a macro reference, a
#'   list such as `x1-x5`) or in a `format=` option.
#' @noRd
source_date_formats <- function(project) {
  statements <- project$statements
  empty <- list(variables = character(), formats = character())
  if (!is.data.frame(statements) || !nrow(statements)) return(empty)
  statements <- statements[statements$type == "code", , drop = FALSE]
  declares <- statements$first_token %in% c("format", "attrib")
  option <- grepl("\\bformat\\s*=", statements$text, ignore.case = TRUE, perl = TRUE)
  token <- "[$]?[A-Za-z_][A-Za-z0-9_]*[.][0-9]*"
  date_names <- function(text, pattern) {
    masked <- mask_strings(text)
    hits <- regmatches(masked, gregexpr(pattern, masked, ignore.case = TRUE, perl = TRUE))[[1L]]
    found <- vapply(sub("^.*=\\s*", "", hits), declared_date_format_name, "", USE.NAMES = FALSE)
    found[!is.na(found)]
  }
  variables <- character()
  formats <- character()
  for (text in statements$text[declares]) {
    read <- parse_format_declaration(text)
    if (is.null(read)) {
      formats <- c(formats, date_names(text, token))
    } else for (variable in names(read)) {
      if (identical(read[[variable]]$kind, "date")) variables <- c(variables, variable)
    }
  }
  for (text in statements$text[option & !declares]) {
    formats <- c(formats, date_names(text, paste0("\\bformat\\s*=\\s*", token)))
  }
  list(variables = unique(variables), formats = unique(formats))
}

#' Date formats generated R declares that the SAS source does not give
#'
#' The check behind the `formats` argument of lib_write(). A column a program
#' declares as a date must be a variable the SAS source gives a date format by
#' name, or the format must be one the source uses in a statement whose
#' variables cannot be read by name. A format declared with neither was not
#' taken from the source.
#'
#' This does not show that the format reaches the output: a variable the
#' source formats and later drops could still be declared by a mistaken
#' translation, like any other mistranslated statement.
#'
#' @param code R code of one generated program.
#' @param project The `sas2r_project` it was translated from, or `NULL`, in
#'   which case nothing is checked.
#' @return Problems as a character vector; empty when every declaration is
#'   backed.
#' @noRd
check_declared_date_formats <- function(code, project) {
  if (is.null(project) || !is.data.frame(project$statements)) return(character())
  declared <- list()
  for (call in declared_write_formats(code)) {
    for (column in names(call$formats)) {
      name <- declared_date_format_name(call$formats[[column]])
      if (!is.na(name)) declared[[length(declared) + 1L]] <- list(column = column,
        written = call$formats[[column]], name = name)
    }
  }
  if (!length(declared)) return(character())
  source <- source_date_formats(project)
  unbacked <- Filter(function(x) !x$column %in% source$variables && !x$name %in% source$formats, declared)
  unique(vapply(unbacked, function(x) paste0(
    "unbacked_date_format: lib_write() gives the column ", x$column, " the date format ", x$written,
    ", which the SAS source gives no variable of that name. Declare only the formats the SAS source gives"), ""))
}

#' The kind of each column in a stored dataset
#'
#' Used for a reference and for the dataset a bundle wrote. Reads no rows
#' from a SAS file: the column types are in its header.
#'
#' @return A named character vector, lower-case column name to
#'   [col_kind()], or `NULL` when the file is absent or unreadable.
#' @noRd
reference_column_kinds <- function(path) {
  if (!is_scalar_character(path) || is.na(path) || !nzchar(path) || !file.exists(path) || dir.exists(path)) return(NULL)
  data <- tryCatch(switch(tolower(tools::file_ext(path)),
    rds = readRDS(path),
    xpt = haven::read_xpt(path, n_max = 0L),
    sas7bdat = haven::read_sas(path, n_max = 0L),
    NULL), error = function(e) NULL)
  if (!inherits(data, "data.frame")) return(NULL)
  stats::setNames(vapply(data, col_kind, character(1)), tolower(names(data)))
}

#' The dates the bundle of an attempt declares
#'
#' @param attempt An attempt record with `attempt_dir`.
#' @return The registry of [output_date_registry()] for the generated programs
#'   in the attempt's bundle; empty when there is no bundle.
#' @noRd
attempt_declared_dates <- function(attempt) {
  dir <- attempt$attempt_dir
  if (!is_scalar_character(dir) || is.na(dir)) return(list())
  dir <- file.path(dir, "bundle")
  if (!dir.exists(dir)) return(list())
  files <- sort(list.files(dir, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE))
  files <- files[!basename(files) %in% SAS2R_BUNDLE_FILES]
  output_date_registry(lapply(files, function(file)
    tryCatch(paste(readLines(file, warn = FALSE), collapse = "\n"), error = function(e) "")))
}

#' One output's date columns as plain records, for the run report
#'
#' Reporting only: it changes no check.
#'
#' @param declared The output's entry in [attempt_declared_dates()]: column
#'   name to SAS date format, or `NULL`.
#' @param candidate_path,reference_path The dataset the bundle wrote and its
#'   reference.
#' @return `NULL` when there is nothing to say, otherwise `list(typed,
#'   deferred)`. `typed` lists the declared columns stored as dates.
#'   `deferred` lists the declared columns that were not, and the columns the
#'   reference stores as dates that the program does not declare.
#' @noRd
date_columns_record <- function(declared = NULL, candidate_path = NA_character_,
                                reference_path = NA_character_) {
  reference <- reference_column_kinds(reference_path)
  reference_dates <- names(reference)[reference %in% "date"]
  if (!length(declared) && !length(reference_dates)) return(NULL)
  stored <- reference_column_kinds(candidate_path)
  if (is.null(stored)) return(NULL)
  typed <- deferred <- list()
  for (column in names(declared)) {
    kind <- if (column %in% names(stored)) stored[[column]] else NA_character_
    if (identical(kind, "date")) {
      typed[[length(typed) + 1L]] <- list(var = column, format = declared[[column]], evidence = "declared")
    } else {
      deferred[[length(deferred) + 1L]] <- list(var = column, format = declared[[column]], evidence = "declared",
        reason = if (is.na(kind)) "the program declares a date format for this column, but the dataset it wrote has no such column"
        else paste0("the program declares a date format for this column, but its values are not whole SAS day numbers ",
                    "in a plain numeric column, so it was stored as ", kind))
    }
  }
  for (column in setdiff(reference_dates, names(declared))) {
    if (!column %in% names(stored) || identical(stored[[column]], "date")) next
    deferred[[length(deferred) + 1L]] <- list(var = column, evidence = "reference",
      reason = "the reference stores a date, but the generated program declares no date format for this column")
  }
  if (!length(typed) && !length(deferred)) return(NULL)
  list(typed = typed, deferred = deferred)
}

#' Report lines for the date columns of every output
#'
#' @param output_assessments Named list of output assessments.
#' @return Markdown lines, empty when no output has a declared or deferred
#'   date column.
#' @noRd
date_columns_report_lines <- function(output_assessments) {
  lines <- character()
  for (key in names(output_assessments)) {
    cols <- output_assessments[[key]]$date_columns
    for (x in cols$typed) lines <- c(lines, paste0(
      "- **", key, "** `", x$var, "`: stored as a date (SAS format ", x$format,
      ", declared by the generated program)."))
    for (x in cols$deferred) lines <- c(lines, paste0(
      "- **", key, "** `", x$var, "`: left as a SAS day number. ",
      toupper(substr(x$reason, 1L, 1L)), substring(x$reason, 2L), "."))
  }
  if (!length(lines)) return(character())
  c("## Date Columns", "",
    paste("SAS stores a date as a number of days from 1960-01-01 and a format that shows it as a date.",
          "A column the generated program gives a date format, where it writes the dataset, is stored as an R `Date`;",
          "the generated programs still compute with the SAS day number."),
    "", lines, "")
}
