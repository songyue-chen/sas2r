#' Match rows between two data frames
#'
#' @param base Base data frame.
#' @param comp Comparison data frame.
#' @param keys Character vector of key column names, or NULL for row-order matching.
#'
#' @return A list with elements:
#'   \item{base_idx}{Integer vector of row indices in base that have matches in comp.}
#'   \item{comp_idx}{Integer vector of matching row indices in comp.}
#'   \item{only_base}{Integer vector of row indices only in base.}
#'   \item{only_comp}{Integer vector of row indices only in comp.}
#'   \item{dup_fanout}{Logical indicating whether duplicate keys were detected in either dataset.}
#' @noRd
match_rows <- function(base, comp, keys) {
  if (is.null(keys)) {
    k <- min(nrow(base), nrow(comp))
    return(list(
      base_idx = seq_len(k),
      comp_idx = seq_len(k),
      only_base = if (nrow(base) > k) (k + 1L):nrow(base) else integer(),
      only_comp = if (nrow(comp) > k) (k + 1L):nrow(comp) else integer(),
      dup_fanout = FALSE
    ))
  }

  keys <- tolower(keys)
  names(base) <- tolower(names(base))
  names(comp) <- tolower(names(comp))

  missing_base <- setdiff(keys, names(base))
  if (length(missing_base)) {
    cli::cli_abort("Key column{?s} missing in base dataset: {.val {missing_base}}.")
  }
  missing_comp <- setdiff(keys, names(comp))
  if (length(missing_comp)) {
    cli::cli_abort("Key column{?s} missing in comparison dataset: {.val {missing_comp}}.")
  }

  # Guard against key column kind mismatch (R5)
  for (k in keys) {
    kb <- col_kind(base[[k]])
    kc <- col_kind(comp[[k]])
    if (kb != kc) {
      cli::cli_abort(
        "Key column {.val {k}} has mismatched types between base ({.val {kb}}) and comparison ({.val {kc}}) datasets."
      )
    }
  }

  base_keys <- base[keys]
  comp_keys <- comp[keys]

  dup_base <- vctrs::vec_duplicate_any(base_keys)
  dup_comp <- vctrs::vec_duplicate_any(comp_keys)
  dup_fanout <- dup_base || dup_comp

  if (!dup_fanout) {
    m <- vctrs::vec_match(base_keys, comp_keys)
  } else {
    get_occ <- function(df) {
      if (nrow(df) == 0L) return(integer(0))
      g <- vctrs::vec_group_id(df)
      o <- order(g, method = "radix")
      occ <- integer(nrow(df))
      occ[o] <- sequence(tabulate(g))
      occ
    }
    base_aug <- vctrs::data_frame(k = base_keys, .__occ = get_occ(base_keys))
    comp_aug <- vctrs::data_frame(k = comp_keys, .__occ = get_occ(comp_keys))
    m <- vctrs::vec_match(base_aug, comp_aug)
  }

  matched_base <- which(!is.na(m))
  matched_comp <- m[matched_base]

  comp_matched_mask <- logical(nrow(comp))
  comp_matched_mask[matched_comp] <- TRUE

  list(
    base_idx = matched_base,
    comp_idx = matched_comp,
    only_base = which(is.na(m)),
    only_comp = which(!comp_matched_mask),
    dup_fanout = dup_fanout
  )
}


