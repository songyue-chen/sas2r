DIGEST_VAR_COLS <- c(
  "var", "kind", "n_compared", "n_mismatch", "n_cosmetic",
  "n_na_diff", "n_case", "max_abs_diff", "mean_abs_diff",
  "tag_mismatches"
)

#' Build the redacted diff digest (the only comparison artifact that may
#' cross to an LLM). Names, counts, magnitudes, and enumerated hints only --
#' never cell values, never key values.
#'
#' @param x An object of class `sas2r_comparison`.
#' @param label Label for the dataset comparison.
#' @return An object of class `sas2r_diff_digest`.
#' @examples
#' sas <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.0))
#' r <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.5))
#' cmp <- compare_datasets(sas, r, keys = "usubjid")
#'
#' # Names, counts and magnitudes only -- never cell values, never key values.
#' d <- diff_digest(cmp, label = "adsl")
#' d$vars
#' d$pattern_hints
#' @export
diff_digest <- function(x, label = "dataset") {
  stopifnot(inherits(x, "sas2r_comparison"))
  hints <- list()
  add <- function(var, hint) hints[[length(hints) + 1L]] <<-
    tibble::tibble(var = var, hint = hint)

  for (k in seq_len(nrow(x$vars))) {
    v <- x$vars[k, ]
    tol <- effective_tol(x$profile, v$var)
    if (v$n_mismatch > 0L) {
      if (v$kind %in% c("numeric", "date", "datetime", "time") && v$n_mismatch >= 3L) {
        if (!is.na(v$diff_sd) && v$diff_sd <= tol$abs) {
          add(v$var, "CONSTANT_OFFSET")
        }
      }
      if (v$n_mismatch == v$n_case && v$n_case > 0L) add(v$var, "CASE_ONLY_DIFF")
      if (v$n_mismatch == v$n_na_diff && v$n_na_diff > 0L) add(v$var, "NA_PATTERN_DIFF")
    } else if (v$n_cosmetic > 0L) {
      add(v$var, "PADDING_ONLY")
    }
  }
  if (isTRUE(x$structure$dup_fanout)) add(NA_character_, "DUPLICATE_KEYS")
  row_delta <- x$structure$rows_only_base + x$structure$rows_only_comp
  if (row_delta > 0L) add(NA_character_, "ROW_COUNT_DELTA")
  attr_n <- x$summary$value[x$summary$metric == "attr_diffs"]
  if (attr_n > 0L && sum(x$vars$n_mismatch) == 0L) add(NA_character_, "ATTR_ONLY")
  if (!is.null(x$structure$unsupported_kinds) && nrow(x$structure$unsupported_kinds) > 0L) {
    add(NA_character_, "UNSUPPORTED_KIND")
  }

  missing_cols <- setdiff(DIGEST_VAR_COLS, names(x$vars))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Comparison object is missing required digest column{?s}: {.val {missing_cols}}.")
  }

  # Hard allowlist of exported variables columns (F15)
  exported_vars <- x$vars[DIGEST_VAR_COLS]

  structure(list(
    digest_version = DIGEST_VERSION,
    label = label,
    passed = x$passed,
    profile_version = x$profile$version,
    rows = list(
      base = x$summary$value[x$summary$metric == "rows_base"],
      comp = x$summary$value[x$summary$metric == "rows_comp"],
      matched = x$summary$value[x$summary$metric == "rows_matched"],
      only_base = x$structure$rows_only_base,
      only_comp = x$structure$rows_only_comp),
    structure = list(
      vars_only_base = I(x$structure$only_base),
      vars_only_comp = I(x$structure$only_comp),
      kind_mismatch = x$structure$kind_mismatch,
      unsupported_kinds = if (is.null(x$structure$unsupported_kinds))
        tibble::tibble(var = character(), kind = character())
      else x$structure$unsupported_kinds),
    vars = exported_vars,
    key_names = I(if (is.null(x$keys)) character() else x$keys),
    pattern_hints = if (length(hints)) do.call(rbind, hints) else
      tibble::tibble(var = character(), hint = character())
  ), class = "sas2r_diff_digest")
}

#' Serialize a diff digest to JSON
#'
#' @param digest An object of class `sas2r_diff_digest`.
#' @return A JSON string representing the diff digest.
#' @examples
#' sas <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.0))
#' r <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.5))
#' digest <- diff_digest(compare_datasets(sas, r, keys = "usubjid"))
#'
#' # The redacted JSON payload that may cross to an LLM.
#' json <- as_digest_json(digest)
#' substr(json, 1, 60)
#' @export
as_digest_json <- function(digest) {
  stopifnot(inherits(digest, "sas2r_diff_digest"))
  jsonlite::toJSON(unclass(digest), auto_unbox = TRUE, na = "null", digits = 8)
}


