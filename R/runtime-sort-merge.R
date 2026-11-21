# Runtime helpers: PROC SORT and MERGE. Part of the runtime every translated
# program carries; see ?sas2r_runtime.

#' Sort a dataset the way PROC SORT does
#'
#' Stable, with missing values lowest under SAS collation; `by` is matched to
#' column names case-insensitively, and variables named in `descending` are
#' sorted in reverse. Special missing values sort as SAS orders them: `._`,
#' `.`, then `.A` to `.Z`, all below any number.
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
  # Rank of each SAS missing value, used to order BY values: NA where x is not
  # missing, 0 for ._, 1 for . (an untagged NA or NaN), 2-27 for .A-.Z. Special
  # missing values are haven tagged NAs; haven reads them with lower-case tags
  # while R code may write upper-case ones, so tags compare case-insensitively.
  # A tag outside ._ and .A-.Z ranks as an ordinary missing value.
  sas2r_missing_rank <- function(x) {
    out <- rep(NA_integer_, length(x))
    miss <- is.na(x)
    out[miss] <- 1L
    if (is.double(x) && any(miss)) {
      tag <- tolower(haven::na_tag(x[miss]))
      tag[is.na(tag)] <- "."
      rank <- match(tag, c("_", ".", letters)) - 1L
      rank[is.na(rank)] <- 1L
      out[miss] <- rank
    }
    out
  }
  if (nrow(df) == 0L) return(df)
  cols <- lapply(by, function(v) {
    idx <- match(tolower(v), tolower(names(df)))
    if (is.na(idx)) {
      stop("sas_sort: by variable not found in dataset: ", v, call. = FALSE)
    }
    x <- df[[idx]]
    desc <- tolower(v) %in% tolower(descending)
    if (is.double(x)) {
      # SAS orders ._ < . < .A < ... < .Z < every number: missing values take
      # their rank 0-27 and numbers rank above them.
      key <- as.double(sas2r_missing_rank(x))
      present <- is.na(key)
      key[present] <- 28 + rank(xtfrm(x[present]), ties.method = "min")
      return(if (desc) -key else key)
    }
    if (is.character(x)) {
      x <- sub(" +$", "", x)
      x[x == "" & !is.na(x)] <- NA_character_
      x <- match(x, sort(unique(x), method = "radix", na.last = NA))
    }
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
#' SAS BY ordering. BY values match as SAS compares them: trailing blanks are
#' ignored, a blank character value equals a missing one, special missing
#' values are distinct, and variable names match without regard to case.
#' Many-to-many keys, and duplicate keys with shared non-key
#' columns, are refused: those cases require SAS observation-by-observation
#' semantics that this join-based helper does not implement.
#' A BY variable that is numeric in one input and character in the other is
#' refused, as SAS refuses it, and so are column names whose case makes the
#' match ambiguous (a BY variable matching two columns of one input, or two
#' columns of `b` that differ only by case matching one column of `a`).
#' Do not pre-create missing columns on one input when the other input supplies
#' them: that invents an overlap. Preserve genuine overlapping source columns;
#' their unsupported repeated-key semantics must not be silently dropped.
#' Values from an input can be retained within a BY group, but reset to missing
#' at the next BY group. If an input has no row for a key, its unique columns
#' are missing for that key; never fill them from a preceding key.
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
#' events <- data.frame(id = c(1, 1, 2), event = c("a", "b", "c"))
#' subjects <- data.frame(id = c(1, 2), base = c(10, 20))
#' sas_merge(events, subjects, by = "id", keep = "left") # three events remain
#' @export
sas_merge <- function(a, b, by,
                      keep = c("full", "both", "left", "right", "left_only",
                               "right_only"), ...) {
  # The BY columns of df rewritten for matching only, the way SAS compares BY
  # values. Character (and factor) values: trailing blanks never distinguish
  # values, and a blank value is the missing one, so trailing spaces are dropped
  # and NA becomes "". Leading blanks still count. Double values: every missing
  # value becomes one NA and a companion column carries its lower-case special
  # missing tag, because .A and . are distinct BY values that vctrs would
  # otherwise treat as equal. Other columns are unchanged.
  sas2r_by_key <- function(df, cols) {
    key <- as.data.frame(df[cols])
    for (i in seq_along(cols)) {
      x <- key[[i]]
      if (is.character(x) || is.factor(x)) {
        x <- sub(" +$", "", as.character(x))
        x[is.na(x)] <- ""
        key[[i]] <- x
      } else if (is.double(x)) {
        key[[paste0(".sas2r_tag_", i)]] <- tolower(haven::na_tag(x))
        x[is.na(x)] <- NA_real_
        key[[i]] <- x
      }
    }
    key
  }
  keep <- match.arg(keep)
  # SAS variable names are case-insensitive: find each BY variable in both
  # inputs, then spell every column of b the way a spells it.
  idx_a <- match(tolower(by), tolower(names(a)))
  idx_b <- match(tolower(by), tolower(names(b)))
  absent <- by[is.na(idx_a) | is.na(idx_b)]
  if (length(absent)) {
    stop("sas_merge: by variable not found in dataset: ", absent[1], call. = FALSE)
  }
  # Refuse what case-insensitive matching cannot decide: a BY variable that
  # matches two columns of one input, or two columns of b that a's spelling
  # would merge into one. Case-only pairs nothing renames are left alone.
  for (v in by) for (nm in list(names(a), names(b))) {
    hits <- nm[tolower(nm) == tolower(v)]
    if (length(hits) > 1L) {
      stop("sas_merge: by variable ", v, " matches more than one column in a dataset: ",
           paste(hits, collapse = ", "), call. = FALSE)
    }
  }
  a_pos <- match(tolower(names(b)), tolower(names(a)))
  renamed <- names(b)
  renamed[!is.na(a_pos)] <- names(a)[a_pos[!is.na(a_pos)]]
  for (v in unique(renamed[duplicated(renamed)])) {
    from <- unique(names(b)[renamed == v])
    if (length(from) > 1L) {
      stop("sas_merge: columns ", paste(from, collapse = ", "),
           " of the second dataset both match column ", v, " of the first",
           call. = FALSE)
    }
  }
  names(b) <- renamed
  # SAS refuses a BY variable that is character in one input and numeric in
  # the other. Dates count as numeric; factors count as character.
  is_chr <- function(x) is.character(x) || is.factor(x)
  is_num <- function(x) is.numeric(x) || is.double(x)
  for (i in seq_along(by)) {
    xa <- a[[idx_a[i]]]; xb <- b[[idx_b[i]]]
    if ((is_chr(xa) && is_num(xb)) || (is_num(xa) && is_chr(xb))) {
      stop("sas_merge: by variable ", by[i],
           " is numeric in one dataset and character in the other", call. = FALSE)
    }
  }
  by <- names(a)[idx_a]
  # Keys are compared through sas2r_by_key(); the returned values stay as given.
  # One group id per row serves both the duplicate checks and the join.
  ids <- vctrs::vec_group_id(sas2r_by_key(rbind(a[by], b[by]), by))
  ids_a <- ids[seq_len(nrow(a))]
  ids_b <- ids[nrow(a) + seq_len(nrow(b))]
  dup_a <- anyDuplicated(ids_a) > 0L
  dup_b <- anyDuplicated(ids_b) > 0L
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
  a$.in_a <- rep(TRUE, nrow(a)); b$.in_b <- rep(TRUE, nrow(b))
  key_id <- ".sas2r_merge_key"
  while (key_id %in% c(names(a), names(b))) key_id <- paste0(key_id, "_")
  a[[key_id]] <- ids_a
  b[[key_id]] <- ids_b
  m <- merge(a, b, by = key_id, all = TRUE, suffixes = c(".sas2r_a", ""))
  for (v in by) {
    missing_right <- is.na(m$.in_b)
    m[[v]][missing_right] <- m[[paste0(v, ".sas2r_a")]][missing_right]
    m[[paste0(v, ".sas2r_a")]] <- NULL
  }
  m[[key_id]] <- NULL
  a[[key_id]] <- NULL; b[[key_id]] <- NULL
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
