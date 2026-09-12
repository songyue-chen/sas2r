#' Define reusable dataset quality requirements
#'
#' Assign a profile a name in `outputs$profiles`, then select it with
#' `outputs$assertions[["adam.adsl"]]$profile`. Target assertions override
#' whole profile fields. These are explicit study requirements, not an
#' automatic CDISC compliance assessment or evidence of SAS equivalence.
#'
#' @param required_columns Character vector of required column names.
#' @param labels,formats Named character vectors of exact required variable
#'   labels and `format.sas` attributes. Missing attributes fail the check.
#' @param types Named character vector: `numeric` (integer or double),
#'   `double`, `integer`, `character`, `factor` (including ordered factors),
#'   `logical`, `Date`, or `POSIXct`.
#' @param column_order Exact ordered vector of all expected column names.
#' @param keys Row alignment columns, matched case-insensitively. Requiring
#'   uniqueness also requires nonmissing, nonblank key values.
#' @param unique_keys Whether the combined key must be nonmissing and unique.
#' @param row_count,min_rows,max_rows Exact, minimum, or maximum row count.
#' @param numeric_tolerance Absolute tolerance for reference comparison. When
#'   set, the relative tolerance defaults to zero, as for target assertions.
#' @param tolerances Named variable overrides with `abs` and/or `rel`, using
#'   the same policy as [compare_profile()]. Only evaluated with a reference.
#' @return A plain list containing only explicitly supplied, non-NULL fields.
#'   Omitted fields inherit global settings. Explicit `FALSE` and empty lists
#'   override them. Types describe physical R columns: factors do not satisfy
#'   `character`, even when their values compare equal to character data.
#' @examples
#' adsl_qc <- qc_profile(
#'   required_columns = c("USUBJID", "AGE"),
#'   keys = "USUBJID", unique_keys = TRUE,
#'   types = c(USUBJID = "character", AGE = "numeric"),
#'   min_rows = 1, tolerances = list(AGE = list(abs = 0, rel = 0))
#' )
#' @export
qc_profile <- function(required_columns = NULL, labels = NULL, formats = NULL,
                       types = NULL, column_order = NULL, keys = NULL,
                       unique_keys = NULL, row_count = NULL, min_rows = NULL,
                       max_rows = NULL, numeric_tolerance = NULL,
                       tolerances = NULL) {
  value <- as.list(environment())
  validate_qc_assertions(value)
}

validate_qc_assertions <- function(x, complete = FALSE) {
  x <- unclass(x)
  x <- x[!vapply(x, is.null, logical(1))]
  fail <- function(field, detail) {
    cli::cli_abort("QC {.field {field}} {detail}", class = "sas2r_output_contract_error")
  }
  for (field in c("required_columns", "column_order", "keys")) {
    v <- x[[field]]
    if (!is.null(v) && (!is.character(v) || anyNA(v) || any(!nzchar(v)) ||
                       anyDuplicated(tolower(v)))) fail(field, "must contain distinct column names")
    if (!is.null(v)) x[[field]] <- unname(v)
  }
  for (field in c("labels", "formats", "types")) {
    v <- x[[field]]
    if (is.null(v)) next
    if (!length(v)) { x[[field]] <- list(); next }
    if (is.list(v) && any(!vapply(v, function(value) {
      is_scalar_character(value, allow_empty = TRUE)
    }, logical(1)))) fail(field, "must contain character values; quote YAML labels, formats and types")
    if (is.list(v)) v <- vapply(v, unname, character(1))
    if (!is.character(v) || is.null(names(v)) || anyNA(names(v)) || anyNA(v) ||
        any(!nzchar(names(v))) || anyDuplicated(tolower(names(v)))) {
      fail(field, "must be a named character mapping")
    }
    if (field == "types" && any(!v %in% c("numeric", "double", "integer",
                                         "character", "factor", "logical", "Date", "POSIXct"))) {
      fail(field, "contains an unsupported column type")
    }
    x[[field]] <- as.list(v)
  }
  if (!is.null(x$unique_keys) &&
      (!is.logical(x$unique_keys) || length(x$unique_keys) != 1L || is.na(x$unique_keys))) {
    fail("unique_keys", "must be true or false")
  }
  if (complete && isTRUE(x$unique_keys) && !length(x$keys)) fail("unique_keys", "requires keys")
  for (field in c("row_count", "min_rows", "max_rows")) {
    v <- x[[field]]
    if (is_scalar_character(v)) v <- suppressWarnings(as.numeric(v))
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || !is.finite(v) ||
                       v < 0 || v != floor(v))) fail(field, "must be a nonnegative integer")
    if (!is.null(v)) x[[field]] <- unname(v)
  }
  if ((x$min_rows %||% 0) > (x$max_rows %||% Inf) ||
      (!is.null(x$row_count) && (x$row_count < (x$min_rows %||% 0) ||
                               x$row_count > (x$max_rows %||% Inf)))) {
    fail("row_count", "requirements contradict each other")
  }
  # The comparison profile remains the authority for numeric tolerance policy.
  if (!is.null(x$numeric_tolerance) || !is.null(x$tolerances)) {
    profile <- tryCatch(compare_profile(abs = x$numeric_tolerance %||% 1e-8,
                         overrides = x$tolerances %||% list()),
      error = function(e) fail("tolerances", conditionMessage(e)))
    if (!is.null(x$numeric_tolerance)) x$numeric_tolerance <- profile$numeric$abs
    if (!is.null(x$tolerances)) x$tolerances <- profile$overrides
  }
  x
}

resolve_qc_profiles <- function(overrides) {
  profiles <- overrides$profiles %||% list()
  if (!is.list(profiles) || (length(profiles) &&
      (is.null(names(profiles)) || any(!nzchar(names(profiles))) || anyDuplicated(names(profiles))))) {
    cli::cli_abort("outputs$profiles must be a named list", class = "sas2r_output_contract_error")
  }
  for (name in names(profiles)) {
    p <- profiles[[name]]
    assert_exact_names(p, names(formals(qc_profile)), paste0("outputs.profiles.", name),
                       class = "sas2r_output_contract_error")
    profiles[[name]] <- validate_qc_assertions(p)
  }
  for (target in names(overrides$assertions)) {
    a <- overrides$assertions[[target]] %||% list()
    allowed <- if (classify_target_kind(target) == "dataset")
      c(names(formals(qc_profile)), "profile") else "required_text"
    assert_exact_names(a, allowed, paste0("outputs.assertions.", target),
                       class = "sas2r_output_contract_error")
    if (!is.null(a$profile)) {
      name <- a$profile
      if (!is_scalar_character(name) || !name %in% names(profiles)) {
        cli::cli_abort("Unknown QC profile for {.val {target}}: {.val {name}}",
                       class = "sas2r_output_contract_error")
      }
      if (classify_target_kind(target) != "dataset") {
        cli::cli_abort("Dataset QC profiles cannot be applied to TLF targets", class = "sas2r_output_contract_error")
      }
      # A field is the unit of override; vectors such as column order are not
      # recursively combined. Keep the selected name visible in the contract.
      a <- effective_dataset_policy(profiles[[name]], a)
    }
    overrides$assertions[[target]] <- validate_qc_assertions(a)
  }
  if (!is.null(overrides$profiles)) overrides$profiles <- profiles
  overrides
}

check_dataset_qc <- function(data, assertions) {
  checks <- list()
  add <- function(name, passed, ...) {
    checks[[name]] <<- c(list(name = name, passed = isTRUE(passed)), list(...))
  }
  columns <- tolower(names(data))
  required <- tolower(assertions$required_columns %||% character())
  if (length(required)) {
    missing <- setdiff(required, columns)
    add("required_columns", !length(missing), missing_columns = missing,
        details = if (length(missing)) paste("Missing required columns:", paste(missing, collapse = ", ")) else "All required columns present")
  }
  for (field in c("labels", "formats", "types")) {
    expected <- unlist(assertions[[field]], use.names = TRUE)
    if (!length(expected)) next
    actual <- lapply(names(expected), function(nm) {
      idx <- match(tolower(nm), columns)
      if (is.na(idx)) return(NA_character_)
      v <- data[[idx]]
      if (field == "types") {
        if (is.factor(v)) "factor"
        else if (inherits(v, "Date")) "Date"
        else if (inherits(v, "POSIXct")) "POSIXct"
        else if (is.object(v) && !inherits(v, "haven_labelled")) class(v)[1L]
        else typeof(v)
      } else attr(v, if (field == "labels") "label" else "format.sas", exact = TRUE) %||% NA_character_
    })
    names(actual) <- names(expected)
    matches <- vapply(seq_along(expected), function(i) {
      value <- actual[[i]]
      is_scalar_character(value, allow_empty = TRUE) &&
        (identical(unname(value), unname(expected[i])) ||
         (field == "types" && expected[i] == "numeric" && value %in% c("double", "integer")))
    }, logical(1))
    add(field, all(matches), expected = as.list(expected), actual = as.list(actual),
        failed_columns = names(expected)[!matches],
        details = if (all(matches)) paste("Required", field, "match") else
          paste("Required", field, "differ:", paste(names(expected)[!matches], collapse = ", ")))
  }
  if (!is.null(assertions$column_order)) {
    add("column_order", identical(columns, unname(tolower(assertions$column_order))),
        expected = assertions$column_order, actual = names(data),
        details = paste0("Expected column order [", paste(assertions$column_order, collapse = ", "),
                         "]; got [", paste(names(data), collapse = ", "), "]"))
  }
  for (field in c("row_count", "min_rows", "max_rows")) {
    expected <- assertions[[field]]
    if (is.null(expected)) next
    ok <- switch(field, row_count = nrow(data) == expected,
                 min_rows = nrow(data) >= expected, max_rows = nrow(data) <= expected)
    add(field, ok, expected = expected, actual = nrow(data),
        details = paste(field, "requirement:", expected, "; actual rows:", nrow(data)))
  }
  keys <- tolower(assertions$keys %||% character())
  if (length(keys) && isTRUE(assertions$unique_keys)) {
    missing <- setdiff(keys, columns)
    add("keys_present", !length(missing), missing_columns = missing,
        details = if (length(missing)) paste("Missing key columns:", paste(missing, collapse = ", ")) else "All key columns present")
    if (!length(missing)) {
      # Reuse the comparator's SAS padding and value normalization so a key
      # such as "01 " cannot evade uniqueness against "01".
      key_data <- normalize_output_frame(data[match(keys, columns)])
      absent <- vapply(key_data, function(x) sum(if (is.character(x)) is.na(x) | trimws(x) == "" else is.na(x)), numeric(1))
      add("keys_nonmissing", !any(absent > 0), missing_counts = as.list(absent),
          details = if (any(absent > 0)) paste("Missing or blank key values:", paste(names(absent)[absent > 0], collapse = ", ")) else "All key values nonmissing")
      duplicates <- sum(duplicated(key_data))
      add("unique_keys", duplicates == 0L, duplicate_rows = duplicates,
          details = paste("Duplicate rows beyond first occurrence of combined key:", duplicates))
    }
  }
  checks
}

# Whole fields override global defaults; omitted/NULL fields inherit them.
effective_dataset_policy <- function(global, assertions) {
  assertions <- assertions[!vapply(assertions, is.null, logical(1))]
  # The target's numeric_tolerance is a complete policy; legacy global
  # tol_abs/tol_rel must not silently weaken an explicit target requirement.
  if (!is.null(assertions$numeric_tolerance)) {
    global$tol_abs <- NULL
    global$tol_rel <- NULL
  }
  global[names(assertions)] <- assertions
  global
}

dataset_comparison_profile <- function(policy) {
  abs <- policy$tol_abs %||% policy$numeric_tolerance
  rel <- policy$tol_rel
  args <- list(overrides = policy$tolerances %||% list())
  if (!is.null(abs) || !is.null(rel)) {
    args$abs <- abs %||% 0
    args$rel <- rel %||% 0
  }
  do.call(compare_profile, args)
}

normalize_comparison_rules <- function(rules, base = NULL) {
  if (is.null(rules)) return(list())
  # Older configurations can carry a `tolerance` mapping, which is not used by
  # the migration gate. Retain it without changing its existing meaning.
  allowed <- c(names(formals(qc_profile)), "tol_abs", "tol_rel", "reference_path",
               "references", "required_text", "force_text_extractor_unavailable", "tolerance")
  assert_exact_names(rules, allowed, "comparison_rules", class = "sas2r_output_contract_error")
  rules <- rules[!vapply(rules, is.null, logical(1))]
  fields <- intersect(names(rules), names(formals(qc_profile)))
  rules[fields] <- validate_qc_assertions(rules[fields])
  if (!is.null(rules$tol_abs) || !is.null(rules$tol_rel)) {
    profile <- tryCatch(dataset_comparison_profile(rules), error = function(e) {
      cli::cli_abort(conditionMessage(e), class = "sas2r_output_contract_error")
    })
    if (!is.null(rules$tol_abs)) rules$tol_abs <- profile$numeric$abs
    if (!is.null(rules$tol_rel)) rules$tol_rel <- profile$numeric$rel
  }
  validate_reference_paths(rules$references, "comparison_rules$references")
  if (!is.null(rules$reference_path) && !is_scalar_character(rules$reference_path)) {
    cli::cli_abort("comparison_rules$reference_path must be one nonempty path",
                   class = "sas2r_output_contract_error")
  }
  if (!is.null(base)) {
    if (!is.null(rules$reference_path)) rules$reference_path <- config_anchor_paths(rules$reference_path, base)
    if (length(rules$references)) rules$references <- lapply(rules$references, config_anchor_paths, base = base)
  }
  rules
}

validate_effective_qc <- function(outputs, rules, contracts = NULL) {
  targets <- if (!is.null(contracts)) stats::setNames(contracts$assertions, contracts$target_key) else
    if (is.list(outputs)) outputs$assertions else list()
  for (target in names(targets)) {
    if (classify_target_kind(target) == "dataset") {
      validate_qc_assertions(effective_dataset_policy(rules, targets[[target]]), complete = TRUE)
    }
  }
  invisible(outputs)
}

validate_reference_paths <- function(paths, context) {
  if (is.null(paths) || !length(paths)) return(invisible(NULL))
  if ((!is.list(paths) && !is.character(paths)) || is.null(names(paths)) ||
      anyNA(names(paths)) || any(!nzchar(names(paths))) ||
      any(!vapply(paths, is_scalar_character, logical(1)))) {
    cli::cli_abort("{.field {context}} must contain one nonempty path per named target",
                   class = "sas2r_output_contract_error")
  }
  invisible(NULL)
}

output_checks_reason <- function(checks) {
  if (!length(checks)) return("No output checks available")
  failed <- Filter(function(chk) isFALSE(chk$passed), checks)
  if (!length(failed)) {
    unavailable <- Filter(function(chk) !isTRUE(chk$passed), checks)
    if (length(unavailable)) return(paste0("Available checks passed; not evaluated: ",
      paste(vapply(unavailable, function(chk) paste0(chk$name, " (", chk$details %||% "unavailable", ")"), character(1)), collapse = "; ")))
    return("All configured output checks passed")
  }
  paste(vapply(failed, function(chk) paste0(chk$name, ": ",
    chk$details %||% chk$status %||% "requirement failed"), character(1)), collapse = "; ")
}
