# Runtime helpers: PROC SORT and MERGE. Part of the runtime every translated
# program carries; see ?sas2r_runtime.

#' Sort a dataset the way PROC SORT does
#'
#' Stable, with missing values lowest under SAS collation; `by` is matched to
#' column names case-insensitively, and variables named in `descending` are
#' sorted in reverse.
#'
#' @param df A data frame.
#' @param by Character vector of BY variables.
#' @param descending Character vector of those BY variables to sort descending.
#' @param ... Ignored.
#' @return `df` reordered.
#' @family runtime helpers
#' @examples
#' sas_sort(data.frame(id = c(2, NA, 1)), by = "id")
#' sas_sort(data.frame(id = c(2, NA, 1)), by = "id", descending = "id")
#' @export
# NOTE: NODUPKEY dedups adjacent sorted keys; distinct() semantics match only after sorting -- which this pipeline guarantees.
sas_sort <- function(df, by, descending = character(), ...) {
  if (nrow(df) == 0L) return(df)
  cols <- lapply(by, function(v) {
    idx <- match(tolower(v), tolower(names(df)))
    if (is.na(idx)) {
      stop("sas_sort: by variable not found in dataset: ", v, call. = FALSE)
    }
    x <- df[[idx]]
    desc <- tolower(v) %in% tolower(descending)
    if (desc) {
      rank(-xtfrm(x), na.last = TRUE, ties.method = "min")
    } else {
      rank(xtfrm(x), na.last = FALSE, ties.method = "min")
    }
  })
  ord <- do.call(order, c(cols, list(method = "radix")))
  df[ord, , drop = FALSE]
}

#' Merge two datasets the way MERGE ... BY does
#'
#' `MERGE a b; BY by;` with `IN=` semantics selected by `keep`: `"both"`
#' (`in_a and in_b`), `"left"` (`in_a`), `"right"` (`in_b`), `"left_only"`,
#' `"right_only"`, `"full"`. Where both datasets carry a non-key column the
#' later dataset's value wins, columns follow statement order, and rows follow
#' SAS BY ordering. Many-to-many keys, and duplicate keys with shared non-key
#' columns, are refused: those cases require SAS observation-by-observation
#' semantics that this join-based helper does not implement.
#'
#' @param a,b Data frames, in statement order.
#' @param by Character vector of BY variables.
#' @param keep Which rows to keep; see Description.
#' @param ... Ignored.
#' @return The merged data frame.
#' @family runtime helpers
#' @examples
#' a <- data.frame(id = c(1, 2), x = c("a", "b"))
#' b <- data.frame(id = c(2, 3), y = c("B", "C"))
#' sas_merge(a, b, by = "id", keep = "full")
#' sas_merge(a, b, by = "id", keep = "both")
#' @export
sas_merge <- function(a, b, by,
                      keep = c("both", "left", "right", "left_only",
                               "right_only", "full"), ...) {
  keep <- match.arg(keep)
  dup_a <- anyDuplicated(a[by]) > 0L
  dup_b <- anyDuplicated(b[by]) > 0L
  if (dup_a && dup_b)
    stop("sas_merge: many-to-many merge on keys (", paste(by, collapse = ", "),
         ") -- SAS row-walking semantics cannot be reproduced by a join. ",
         "This unit requires the PDV-faithful path.", call. = FALSE)
  overlap <- setdiff(intersect(names(a), names(b)), by)
  if ((dup_a || dup_b) && length(overlap)) {
    stop("sas_merge: duplicate keys with shared non-key columns (",
         paste(overlap, collapse = ", "),
         ") require SAS observation-by-observation MERGE semantics; defer this unit",
         call. = FALSE)
  }
  a_cols <- names(a); b_cols <- names(b)
  a$.in_a <- TRUE; b$.in_b <- TRUE
  m <- merge(a, b, by = by, all = TRUE, suffixes = c(".sas2r_a", ""))
  # SAS overlap rule: the later dataset's value wins where both contribute
  for (v in setdiff(intersect(names(a), names(b)), c(by, ".in_a", ".in_b"))) {
    left <- m[[paste0(v, ".sas2r_a")]]
    miss <- is.na(m$.in_b)
    if (any(miss)) m[[v]][miss] <- left[miss]
    m[[paste0(v, ".sas2r_a")]] <- NULL
  }
  ina <- !is.na(m$.in_a); inb <- !is.na(m$.in_b)
  rows <- switch(keep, both = ina & inb, left = ina, right = inb,
                 left_only = ina & !inb, right_only = inb & !ina, full = ina | inb)
  m <- m[rows, , drop = FALSE]
  m$.in_a <- NULL; m$.in_b <- NULL
  # SAS output shape, not base::merge's: columns follow the contributing
  # datasets in statement order, and rows follow the BY ordering under SAS
  # collation (missing lowest, stable), which sas_sort already implements.
  m <- m[, c(a_cols, setdiff(b_cols, a_cols)), drop = FALSE]
  sas_sort(m, by = by)
}
