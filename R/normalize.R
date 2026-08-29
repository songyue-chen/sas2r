#' Detect column kind for comparison
#'
#' @param x A vector.
#' @return A character scalar identifying the column kind.
#' @noRd
col_kind <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXct")) return("datetime")
  if (inherits(x, "difftime") || inherits(x, "hms")) return("time")
  if (is.factor(x)) return("character")
  if (is.numeric(x)) return("numeric")
  if (is.character(x)) return("character")
  if (is.logical(x)) return("logical")
  if (is.list(x)) return("list")
  class(x)[1]
}

#' Normalize column values and record changes
#'
#' @param x A vector.
#' @return A list with `x` (the normalized vector) and `notes` (character vector of operations performed).
#' @noRd
normalize_col <- function(x) {
  notes <- character()
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
    notes <- c(notes, "zap_labels")
  }
  if (is.factor(x)) {
    x <- as.character(x)
    notes <- c(notes, "factor_to_character")
  }
  if (is.character(x)) {
    enc <- Encoding(x)
    conv <- enc != "bytes"
    if (any(conv)) {
      x[conv] <- enc2utf8(x[conv])
    }
    if (any(enc != "UTF-8" & enc != "unknown" & enc != "bytes")) {
      notes <- c(notes, "enc2utf8")
    }
  }
  list(x = x, notes = notes)
}



#' Align columns between base and comparison datasets
#'
#' @param base Base data.frame or tibble.
#' @param comp Comparison data.frame or tibble.
#' @return A list with `base`, `comp`, `common`, `only_base`, `only_comp`, and `kind_mismatch`.
#' @noRd
align_columns <- function(base, comp) {
  bn <- tolower(names(base))
  cn <- tolower(names(comp))

  if (anyDuplicated(bn)) {
    dup_b <- unique(bn[duplicated(bn)])
    cli::cli_abort("Base dataset contains duplicated column names after case-folding: {.val {dup_b}}.")
  }
  if (anyDuplicated(cn)) {
    dup_c <- unique(cn[duplicated(cn)])
    cli::cli_abort("Comparison dataset contains duplicated column names after case-folding: {.val {dup_c}}.")
  }

  names(base) <- bn
  names(comp) <- cn
  common <- intersect(bn, cn)

  kinds <- lapply(common, function(v) {
    c(base = col_kind(base[[v]]), comp = col_kind(comp[[v]]))
  })
  bad <- vapply(kinds, function(k) k["base"] != k["comp"], logical(1))

  list(
    base = base,
    comp = comp,
    common = common,
    only_base = setdiff(bn, cn),
    only_comp = setdiff(cn, bn),
    kind_mismatch = tibble::tibble(
      var = common[bad],
      base_kind = vapply(kinds[bad], `[[`, character(1), "base"),
      comp_kind = vapply(kinds[bad], `[[`, character(1), "comp")
    )
  )
}

