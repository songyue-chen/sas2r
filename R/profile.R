check_enum <- function(val, choices, arg) {
  tryCatch(
    match.arg(val, choices),
    error = function(e) {
      cli::cli_abort(c("x" = "Invalid {.arg {arg}} value: {.val {val}}.",
                       "i" = "Must be one of {.val {choices}}."))
    }
  )
}

#' Resolve effective numeric tolerance for a specific variable
#'
#' @param profile An object of class `sas2r_profile`.
#' @param var Variable name (character).
#' @return A list with `abs` and `rel` numeric tolerances.
#' @noRd
effective_tol <- function(profile, var) {
  tol <- profile$numeric
  v <- tolower(var)
  if (!is.null(profile$overrides[[v]])) {
    ov <- profile$overrides[[v]]
    tol <- utils::modifyList(tol, ov)
  }
  tol
}

#' Build a comparison tolerance profile
#'
#' Every equivalence judgement the comparator makes is parameterized here
#' and nowhere else. Defaults implement the accepted SAS-to-R tolerance
#' rules: SAS null == NA, special missings reported, trailing padding
#' cosmetic, combined absolute + relative numeric tolerance.
#'
#' @param abs Absolute numeric tolerance (scalar >= 0).
#' @param rel Relative numeric tolerance (scalar >= 0).
#' @param sas_null_equals_na Whether SAS missing character values (empty string `""` and all-space strings) equal R NA (logical).
#' @param na_tags Tagged-NA handling: `"report"`, `"ignore"`, or `"strict"`.
#' @param padding Trailing blank padding handling: `"cosmetic"` or `"strict"`.
#' @param attrs_cosmetic Character vector of attribute names whose differences are cosmetic.
#' @param overrides Named list of per-variable numeric tolerance overrides.
#' @param keys Character vector of key column names, or `NULL`.
#' @return An object of class `sas2r_profile`.
#' @examples
#' # Defaults implement the accepted SAS-to-R tolerance rules.
#' p <- compare_profile()
#' p$numeric
#'
#' # Tighten numeric tolerance and make trailing blanks significant.
#' strict <- compare_profile(abs = 1e-12, rel = 1e-12, padding = "strict")
#'
#' # Per-variable tolerance override, plus keys used for row matching.
#' compare_profile(overrides = list(aval = list(abs = 1e-6)),
#'                 keys = c("usubjid", "paramcd"))
#' @export
compare_profile <- function(abs = 1e-8, rel = 1e-8,
                            sas_null_equals_na = TRUE,
                            na_tags = c("report", "ignore", "strict"),
                            padding = c("cosmetic", "strict"),
                            attrs_cosmetic = c("label", "format.sas"),
                            overrides = list(), keys = NULL) {
  if (is.character(abs) && length(abs) == 1L && !is.na(suppressWarnings(as.numeric(abs)))) {
    abs <- as.numeric(abs)
  }
  if (is.character(rel) && length(rel) == 1L && !is.na(suppressWarnings(as.numeric(rel)))) {
    rel <- as.numeric(rel)
  }
  if (!is.numeric(abs) || length(abs) != 1L || is.na(abs) || abs < 0) {
    cli::cli_abort("{.arg abs} must be a single non-negative number.")
  }
  if (!is.numeric(rel) || length(rel) != 1L || is.na(rel) || rel < 0) {
    cli::cli_abort("{.arg rel} must be a single non-negative number.")
  }
  if (!is.logical(sas_null_equals_na) || length(sas_null_equals_na) != 1L || is.na(sas_null_equals_na)) {
    cli::cli_abort("{.arg sas_null_equals_na} must be TRUE or FALSE.")
  }
  na_tags <- check_enum(na_tags, c("report", "ignore", "strict"), "na_tags")
  padding <- check_enum(padding, c("cosmetic", "strict"), "padding")

  if (length(overrides)) {
    if (!is.list(overrides) || is.null(names(overrides)) || any(names(overrides) == "")) {
      cli::cli_abort("{.arg overrides} must be a named list.")
    }
    names(overrides) <- tolower(names(overrides))
    for (v in names(overrides)) {
      ov <- overrides[[v]]
      if (!is.list(ov)) {
        cli::cli_abort("Override for variable {.val {v}} must be a list with {.arg abs} and/or {.arg rel}.")
      }
      invalid_names <- setdiff(names(ov), c("abs", "rel"))
      if (length(invalid_names)) {
        cli::cli_abort("Invalid override parameter(s) for variable {.val {v}}: {.val {invalid_names}}. Must be {.val abs} and/or {.val rel}.")
      }

      if ("abs" %in% names(ov)) {
        if (!is.numeric(ov$abs) || length(ov$abs) != 1L || is.na(ov$abs) || ov$abs < 0) {
          cli::cli_abort("Override {.arg abs} for variable {.val {v}} must be a single non-negative number.")
        }
      }
      if ("rel" %in% names(ov)) {
        if (!is.numeric(ov$rel) || length(ov$rel) != 1L || is.na(ov$rel) || ov$rel < 0) {
          cli::cli_abort("Override {.arg rel} for variable {.val {v}} must be a single non-negative number.")
        }
      }
    }
  }
  structure(list(
    numeric = list(abs = abs, rel = rel),
    sas_null_equals_na = sas_null_equals_na,
    na_tags = na_tags,
    padding = padding,
    attrs_cosmetic = attrs_cosmetic,
    overrides = overrides,
    keys = if (is.null(keys)) NULL else tolower(keys),
    version = PROFILE_VERSION
  ), class = "sas2r_profile")
}


#' @export
print.sas2r_profile <- function(x, ...) {
  cli::cli_h1("sas2r comparison profile (v{x$version})")
  cli::cli_text("numeric tolerance: abs {x$numeric$abs}, rel {x$numeric$rel}")
  cli::cli_text("SAS null == NA: {x$sas_null_equals_na}")
  cli::cli_text("special-missing tags: {x$na_tags}; padding: {x$padding}")
  cli::cli_text("cosmetic attrs: {toString(x$attrs_cosmetic)}")
  if (length(x$overrides)) cli::cli_text("overrides: {toString(names(x$overrides))}")
  if (!is.null(x$keys)) cli::cli_text("keys: {toString(x$keys)}")
  invisible(x)
}

