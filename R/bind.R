#' Fast row-binding of homogenous tibbles/data.frames
#'
#' @param lst List of tibbles or data.frames.
#' @param default_tibble Empty prototype tibble returned when lst is empty.
#' @return Combined tibble.
#' @noRd
fast_bind <- function(lst, default_tibble) {
  if (!length(lst)) return(default_tibble)
  cols <- names(default_tibble)
  if (is.null(cols) || length(cols) == 0L) {
    return(tibble::as_tibble(lst[[1]]))
  }
  non_empty <- lst[vapply(lst, function(x) nrow(x) > 0L, logical(1))]
  if (length(non_empty) == 0L) return(default_tibble)
  res <- vector("list", length(cols))
  names(res) <- cols
  for (nm in cols) {
    proto_val <- default_tibble[[nm]]
    col_vals <- lapply(non_empty, function(x) {
      if (is.null(x[[nm]])) rep(proto_val[NA_integer_], nrow(x)) else x[[nm]]
    })
    val <- unlist(col_vals, use.names = FALSE)
    res[[nm]] <- if (is.null(val)) default_tibble[[nm]] else val
  }
  tibble::as_tibble(res)
}
