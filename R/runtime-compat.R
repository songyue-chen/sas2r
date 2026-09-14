# Runtime helpers: generated-code plumbing. Part of the runtime every
# translated program carries; see ?sas2r_runtime.

#' Split a SAS dataset name into its library and member
#'
#' This internal helper is available in a generated bundle's runtime.
#' Use the returned names exactly; it returns a character vector, not a list.
#' Accessing an absent name with `[[` raises an error, rather than returning NULL.
#'
#' @param ds A character scalar such as `"source.measurements"` or `"measurements"`.
#' @param macro_vars Named character vector of macro variables to substitute
#'   before splitting, without the leading ampersand in each name.
#' @return A named character vector of length two, with elements `lib` and
#'   `member`. A one-level name uses `lib = "work"`. There is no `libref` element.
#' @examples
#' split_ds <- getFromNamespace("split_ds", "sas2r")
#' parts <- split_ds("source.measurements")
#' parts[["lib"]]     # "source"
#' parts[["member"]]  # "measurements"
#' split_ds("measurements") # c(lib = "work", member = "measurements")
#' @keywords internal
split_ds <- function(ds, macro_vars = character()) {
  if (length(macro_vars) > 0L && is.character(ds) && length(ds) == 1L) {
    for (nm in names(macro_vars)) {
      if (nzchar(nm)) {
        ds <- gsub(paste0("&", nm, "\\b"), macro_vars[[nm]], ds)
      }
    }
  }
  p <- strsplit(ds, ".", fixed = TRUE)[[1]]
  if (length(p) == 1L) c(lib = "work", member = p[1])
  else c(lib = p[1], member = p[2])
}
