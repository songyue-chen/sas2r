#' Test numeric equality under combined absolute and relative tolerance
#'
#' @param b Numeric vector from base dataset.
#' @param comp Numeric vector from comparison dataset.
#' @param abs_tol Absolute tolerance (scalar >= 0).
#' @param rel_tol Relative tolerance (scalar >= 0).
#' @return Logical vector.
#' @noRd
num_equal <- function(b, comp, abs_tol, rel_tol) {
  both_na <- is.na(b) & is.na(comp)
  inf_b <- is.infinite(b)
  inf_comp <- is.infinite(comp)
  both_inf <- inf_b & inf_comp & (sign(b) == sign(comp))

  both_finite <- is.finite(b) & is.finite(comp)
  diff_ok <- both_finite & (abs(b - comp) <= (abs_tol + rel_tol * pmax(abs(b), abs(comp))))

  both_na | both_inf | diff_ok
}

#' Check if special missing tags match
#'
#' @param b Vector from base dataset.
#' @param comp Vector from comparison dataset.
#' @return Logical vector.
#' @noRd
na_tags_match <- function(b, comp) {
  tb <- if (is.double(b)) haven::na_tag(b) else rep(NA_character_, length(b))
  tc <- if (is.double(comp)) haven::na_tag(comp) else rep(NA_character_, length(comp))
  both_na <- is.na(b) & is.na(comp)
  same_tag <- (is.na(tb) & is.na(tc)) | (!is.na(tb) & !is.na(tc) & tb == tc)
  !both_na | same_tag
}

#' Classify character differences
#'
#' @param b Character vector from base dataset.
#' @param comp Character vector from comparison dataset.
#' @param sas_null_equals_na Logical, whether "" and all-space strings equal NA.
#' @return Character vector with values: "equal", "padding", "case", "na_diff", "diff".
#' @noRd
chr_classify <- function(b, comp, sas_null_equals_na = TRUE) {
  n <- length(b)
  out <- rep("diff", n)

  is_blank <- function(x) {
    if (isTRUE(sas_null_equals_na)) {
      blank <- is.na(x) | !nzchar(x)
      sp <- which(startsWith(x, " "))
      if (length(sp) > 0L) blank[sp] <- blank[sp] | grepl("^ *$", x[sp])
      blank
    } else {
      is.na(x)
    }
  }



  blank_b <- is_blank(b)
  blank_comp <- is_blank(comp)

  both_blank <- blank_b & blank_comp
  one_blank <- xor(blank_b, blank_comp)

  out[both_blank] <- "equal"
  out[one_blank] <- "na_diff"

  cand <- which(!both_blank & !one_blank)
  if (length(cand) == 0L) return(out)

  bc <- b[cand]
  cc <- comp[cand]

  eq <- bc == cc
  cand_out <- rep("diff", length(cand))
  cand_out[eq] <- "equal"

  neq_idx <- which(!eq)
  if (length(neq_idx) > 0L) {
    bc_neq <- bc[neq_idx]
    cc_neq <- cc[neq_idx]

    strip_space <- function(x) sub(" +$", "", x)
    bc_strip <- strip_space(bc_neq)
    cc_strip <- strip_space(cc_neq)

    pad <- bc_strip == cc_strip
    case_diff <- !pad & tolower(bc_strip) == tolower(cc_strip)

    cand_out[neq_idx[pad]] <- "padding"
    cand_out[neq_idx[case_diff]] <- "case"
  }

  out[cand] <- cand_out
  out
}

#' NA-safe equality helper for non-numeric types
#'
#' @param b Vector from base dataset.
#' @param comp Vector from comparison dataset.
#' @return Logical vector.
#' @noRd
na_equal <- function(b, comp) {
  (is.na(b) & is.na(comp)) | (!is.na(b) & !is.na(comp) & b == comp)
}


