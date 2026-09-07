# Runtime helpers: formats. Part of the runtime every translated program
# carries; see ?sas2r_runtime.

#' Apply a SAS format
#'
#' `apply_format()` applies a compiled format catalog entry (as written to a
#' bundle's `_sas2r_formats.R`): exact `values` first, then `ranges` (each a
#' list with `lo`, `hi`, `label`), then `other` for anything unmatched.
#' Missing input stays missing unless the format defines `other`. `sas_put()`
#' is `PUT(x, fmt.)` and does the same.
#'
#' @param x The vector to format.
#' @param fmt A compiled format: a list with any of `values` (a named
#'   character vector), `ranges`, and `other`; `NULL` formats as character.
#' @return A character vector.
#' @family runtime helpers
#' @examples
#' sex <- list(values = c(M = "Male", F = "Female"), other = "Unknown")
#' apply_format(c("M", "F", "X", NA), sex)
#' age <- list(ranges = list(list(lo = 0, hi = 17, label = "<18"),
#'                           list(lo = 18, hi = 200, label = "18+")))
#' sas_put(c(5, 40, NA), age)
#' @export
apply_format <- function(x, fmt) {
  if (is.null(fmt)) return(as.character(x))
  fmt_key <- function(v) {
    if (is.na(v)) NA_character_
    else if (is.numeric(v)) format(v, scientific = FALSE, trim = TRUE)
    else as.character(v)
  }
  key <- if (is.numeric(x)) vapply(x, fmt_key, character(1)) else as.character(x)
  out <- if (!is.null(fmt$other)) rep(fmt$other, length(x)) else key
  hit <- !is.na(x) & key %in% names(fmt$values)
  out[hit] <- unname(fmt$values[key[hit]])
  if (!is.null(fmt$ranges)) {
    for (r in fmt$ranges) {
      inr <- !is.na(x) & x >= r$lo & x <= r$hi & !hit
      out[inr] <- r$label
    }
  }
  out[is.na(x) & is.null(fmt$other)] <- NA_character_
  out
}

#' @rdname apply_format
#' @export
sas_put <- function(x, fmt) apply_format(x, fmt)
