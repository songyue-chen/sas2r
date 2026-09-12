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
#'   `double`, `integer`, `character`, `logical`, `Date`, or `POSIXct`.
#' @param column_order Exact ordered vector of all expected column names.
#' @param keys Row alignment columns, matched case-insensitively. Requiring
#'   uniqueness also requires nonmissing, nonblank key values.
#' @param unique_keys Whether the combined key must be nonmissing and unique.
#' @param row_count,min_rows,max_rows Exact, minimum, or maximum row count.
#' @param numeric_tolerance Absolute tolerance for reference comparison. When
#'   set, the relative tolerance defaults to zero, as for target assertions.
#' @param tolerances Named variable overrides with `abs` and/or `rel`, using
#'   the same policy as [compare_profile()]. Only evaluated with a reference.
#' @return A list of assertions of class `sas2r_qc_profile`.
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
                       unique_keys = FALSE, row_count = NULL, min_rows = NULL,
                       max_rows = NULL, numeric_tolerance = NULL,
                       tolerances = list()) {
  value <- as.list(environment())
  value <- value[!vapply(value, is.null, logical(1))]
  for (field in c("labels", "formats", "types")) {
    if (!is.null(value[[field]])) value[[field]] <- as.list(value[[field]])
  }
  validate_qc_assertions(value)
  structure(value, class = "sas2r_qc_profile")
}

validate_qc_assertions <- function(x) {
  fail <- function(field, detail) {
    cli::cli_abort("QC {.field {field}} {detail}", class = "sas2r_output_contract_error")
  }
  for (field in c("required_columns", "column_order", "keys")) {
    v <- x[[field]]
    if (!is.null(v) && (!is.character(v) || anyNA(v) || any(!nzchar(v)) ||
                       anyDuplicated(tolower(v)))) fail(field, "must contain distinct column names")
  }
  for (field in c("labels", "formats", "types")) {
    v <- x[[field]]
    if (is.null(v)) next
    if (is.list(v)) v <- unlist(v, use.names = TRUE)
    if (!is.character(v) || is.null(names(v)) || anyNA(v) ||
        any(!nzchar(names(v))) || anyDuplicated(tolower(names(v)))) {
      fail(field, "must be a named character mapping")
    }
    if (field == "types" && any(!v %in% c("numeric", "double", "integer",
                                         "character", "logical", "Date", "POSIXct"))) {
      fail(field, "contains an unsupported column type")
    }
  }
  if (!is.null(x$unique_keys) &&
      (!is.logical(x$unique_keys) || length(x$unique_keys) != 1L || is.na(x$unique_keys))) {
    fail("unique_keys", "must be true or false")
  }
  if (isTRUE(x$unique_keys) && !length(x$keys)) fail("unique_keys", "requires keys")
  for (field in c("row_count", "min_rows", "max_rows")) {
    v <- x[[field]]
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || !is.finite(v) ||
                       v < 0 || v != floor(v))) fail(field, "must be a nonnegative integer")
  }
  if ((x$min_rows %||% 0) > (x$max_rows %||% Inf) ||
      (!is.null(x$row_count) && (x$row_count < (x$min_rows %||% 0) ||
                               x$row_count > (x$max_rows %||% Inf)))) {
    fail("row_count", "requirements contradict each other")
  }
  # The comparison profile remains the authority for numeric tolerance policy.
  compare_profile(abs = x$numeric_tolerance %||% 1e-8,
                  overrides = x$tolerances %||% list())
  invisible(x)
}

resolve_qc_profiles <- function(overrides) {
  profiles <- overrides$profiles %||% list()
  if (!is.list(profiles) || (length(profiles) &&
      (is.null(names(profiles)) || any(!nzchar(names(profiles))) || anyDuplicated(names(profiles))))) {
    cli::cli_abort("outputs$profiles must be a named list", class = "sas2r_output_contract_error")
  }
  for (name in names(profiles)) {
    p <- profiles[[name]]
    if (!is.list(p)) cli::cli_abort("QC profile {.val {name}} must be a mapping")
    assert_exact_names(p, names(formals(qc_profile)), paste0("outputs.profiles.", name))
    validate_qc_assertions(p)
  }
  for (target in names(overrides$assertions)) {
    a <- overrides$assertions[[target]]
    if (!is.list(a)) a <- as.list(a)
    if (!is.null(a$profile)) {
      name <- a$profile
      if (!is.character(name) || length(name) != 1L || is.na(name) || !name %in% names(profiles)) {
        cli::cli_abort("Unknown QC profile for {.val {target}}: {.val {name}}",
                       class = "sas2r_output_contract_error")
      }
      if (classify_target_kind(target) != "dataset") {
        cli::cli_abort("Dataset QC profiles cannot be applied to TLF targets")
      }
      # A field is the unit of override; vectors such as column order are not
      # recursively combined. Keep the selected name visible in the contract.
      p <- unclass(profiles[[name]])
      p[names(a)] <- a
      a <- p
    }
    validate_qc_assertions(a)
    overrides$assertions[[target]] <- a
  }
  overrides
}

check_dataset_qc <- function(data, assertions) {
  validate_qc_assertions(assertions)
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
    actual <- vapply(names(expected), function(nm) {
      idx <- match(tolower(nm), columns)
      if (is.na(idx)) return(NA_character_)
      v <- data[[idx]]
      if (field == "types") {
        if (inherits(v, "Date")) "Date"
        else if (inherits(v, "POSIXct")) "POSIXct"
        else if (is.object(v) && !inherits(v, "haven_labelled")) class(v)[1L]
        else typeof(v)
      } else as.character(attr(v, if (field == "labels") "label" else "format.sas", exact = TRUE) %||% NA_character_)
    }, character(1))
    matches <- !is.na(actual) & actual == expected
    if (field == "types") matches <- matches | (expected == "numeric" & actual %in% c("double", "integer"))
    add(field, all(matches), expected = as.list(expected), actual = as.list(actual),
        failed_columns = names(expected)[!matches],
        details = if (all(matches)) paste("Required", field, "match") else
          paste("Required", field, "differ:", paste(names(expected)[!matches], collapse = ", ")))
  }
  if (!is.null(assertions$column_order)) {
    add("column_order", identical(columns, tolower(assertions$column_order)),
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
  if (length(keys)) {
    missing <- setdiff(keys, columns)
    add("keys_present", !length(missing), missing_columns = missing,
        details = if (length(missing)) paste("Missing key columns:", paste(missing, collapse = ", ")) else "All key columns present")
    if (!length(missing) && isTRUE(assertions$unique_keys)) {
      # Reuse the comparator's SAS padding and value normalization so a key
      # such as "01 " cannot evade uniqueness against "01".
      key_data <- normalize_output_frame(data[match(keys, columns)])
      absent <- vapply(key_data, function(x) sum(is.na(x) | (is.character(x) & trimws(as.character(x)) == "")), numeric(1))
      add("keys_nonmissing", !any(absent > 0), missing_counts = as.list(absent),
          details = if (any(absent > 0)) paste("Missing or blank key values:", paste(names(absent)[absent > 0], collapse = ", ")) else "All key values nonmissing")
      duplicates <- sum(duplicated(key_data))
      add("unique_keys", duplicates == 0L, duplicate_rows = duplicates,
          details = paste("Duplicate rows beyond first occurrence of combined key:", duplicates))
    }
  }
  checks
}
