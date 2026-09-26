# Runtime helpers: SAS value semantics. Part of the runtime every translated
# program carries; see ?sas2r_runtime. R/runtime-*.R is the source of truth
# and inst/templates/sas2r-helpers.R is generated from it.

# SAS || concatenation with SAS missing semantics: a missing operand contributes
# nothing instead of "NA".
# NOTE: SAS || pads fixed-width operands; %+% does not. If source code relies on padded concatenation, review output widths.
`%+%` <- function(a, b) {
  paste0(ifelse(is.na(a), "", sas_display(a)), ifelse(is.na(b), "", sas_display(b)))
}

# SAS . not in (1, 2) is TRUE; NA %in% c(1, 2) is FALSE, so !(NA %in% ...) is TRUE
`%notin%` <- function(a, b) {
  if (is.character(a) || is.character(b)) {
    a <- if (is.character(a)) sub(" +$", "", a) else a
    b <- if (is.character(b)) sub(" +$", "", b) else b
    a[is.na(a)] <- ""; b[is.na(b)] <- ""
  }
  !(a %in% b)
}

#' Compare values the way SAS does
#'
#' SAS comparison of possibly-missing values. Character operands ignore
#' trailing blanks, and a missing character value (`""` or `NA`) sorts below
#' every non-missing one; numeric missing sorts below every number.
#'
#' @param a,b Vectors to compare; recycled to a common length.
#' @param op `NULL` for a three-way comparison, or one of `"=="`, `"!="`,
#'   `"<"`, `"<="`, `">"`, `">="`. Translate SAS equality (`=` or `EQ`) as
#'   `op = "=="`; `op = "="` is unsupported. Invalid operators raise an error.
#' @return With `op` `NULL`, an integer vector of `-1`, `0`, `1` (missing
#'   equals missing). With an operator, the logical SAS would produce -- never
#'   `NA`.
#' @details
#' Supply an explicit operator in Boolean conditions. Omitting `op` returns
#' an ordering result: equality is `0`, which R treats as false. To test
#' equality, use `chr_cmp(a, b, op = "==")`.
#' @family runtime helpers
#' @examples
#' chr_cmp(c("A", NA, "B  "), c("A", NA, "B"))
#' chr_cmp(c("A", NA, "B  "), c("A", NA, "B"), op = "==")
#' chr_cmp(c(61, 70, NA), c(61, 60, NA), op = "==")
#' chr_cmp(c(1, NA, 3), c(1, 2, 2), op = "<")
#' @export
chr_cmp <- function(a, b, op = NULL) {
  if (!is.null(op) && (!is.character(op) || length(op) != 1L || is.na(op) ||
      !op %in% c("==", "!=", "<", "<=", ">", ">="))) {
    stop('chr_cmp: op must be NULL or one of "==", "!=", "<", "<=", ">", ">=". Use op = "==" for equality.',
         call. = FALSE)
  }
  if (missing(op) || is.null(op)) {
    if (is.character(a) || is.character(b)) {
      strip <- function(x) sub(" +$", "", x)
      a_chr <- strip(as.character(a))
      b_chr <- strip(as.character(b))
      levels <- sort(unique(c(a_chr, b_chr)), method = "radix", na.last = NA)
      a_rank <- match(a_chr, levels); b_rank <- match(b_chr, levels)
      a_na <- is.na(a) | a_chr == ""
      b_na <- is.na(b) | b_chr == ""

      res <- integer(max(length(a), length(b)))
      both_na <- a_na & b_na
      only_a_na <- a_na & !b_na
      only_b_na <- !a_na & b_na
      neither_na <- !a_na & !b_na

      res[both_na] <- 0L
      res[only_a_na] <- -1L
      res[only_b_na] <- 1L
      if (any(neither_na)) {
        lt <- a_rank < b_rank
        gt <- a_rank > b_rank
        res[neither_na & lt] <- -1L
        res[neither_na & gt] <- 1L
        res[neither_na & !lt & !gt] <- 0L
      }
      return(res)
    } else {
      a_val <- ifelse(is.na(a), -Inf, a)
      b_val <- ifelse(is.na(b), -Inf, b)
      res <- integer(max(length(a), length(b)))
      lt <- a_val < b_val
      gt <- a_val > b_val
      res[lt] <- -1L
      res[gt] <- 1L
      res[!lt & !gt] <- 0L
      return(res)
    }
  }
  if (is.character(a) || is.character(b)) {
    strip <- function(x) sub(" +$", "", x)
    a_chr <- strip(as.character(a))
    b_chr <- strip(as.character(b))
    levels <- sort(unique(c(a_chr, b_chr)), method = "radix", na.last = NA)
    a_rank <- match(a_chr, levels); b_rank <- match(b_chr, levels)
    a_na <- is.na(a) | a_chr == ""
    b_na <- is.na(b) | b_chr == ""

    both_na <- a_na & b_na
    only_a_na <- a_na & !b_na
    only_b_na <- !a_na & b_na
    neither_na <- !a_na & !b_na

    res <- logical(max(length(a), length(b)))

    if (op %in% c("==", "<=", ">=")) res[both_na] <- TRUE
    if (op %in% c("!=", "<", ">")) res[both_na] <- FALSE

    if (op %in% c("<", "<=", "!=")) res[only_a_na] <- TRUE
    if (op %in% c("==", ">", ">=")) res[only_a_na] <- FALSE

    if (op %in% c(">", ">=", "!=")) res[only_b_na] <- TRUE
    if (op %in% c("==", "<", "<=")) res[only_b_na] <- FALSE

    if (any(neither_na)) {
      cmp <- switch(op,
        "==" = a_chr == b_chr,
        "!=" = a_chr != b_chr,
        "<"  = a_rank < b_rank,
        "<=" = a_rank <= b_rank,
        ">"  = a_rank > b_rank,
        ">=" = a_rank >= b_rank
      )
      res[neither_na] <- cmp[neither_na]
    }
    return(res)
  } else {
    a_val <- ifelse(is.na(a), -Inf, a)
    b_val <- ifelse(is.na(b), -Inf, b)
    switch(op,
      "==" = a_val == b_val,
      "!=" = a_val != b_val,
      "<"  = a_val < b_val,
      "<=" = a_val <= b_val,
      ">"  = a_val > b_val,
      ">=" = a_val >= b_val
    )
  }
}

#' Row-wise SUM and MEAN with SAS missing semantics
#'
#' SAS `SUM()` and `MEAN()` are row-wise across their arguments: each
#' observation gets the sum or mean of its non-missing arguments, and only an
#' observation whose arguments are all missing yields missing. Column
#' aggregation (`PROC MEANS`, `PROC SQL`) is emitted inline by those emitters
#' and never routed through these.
#'
#' @param ... Numeric vectors, recycled to a common length.
#' @return A numeric vector of that length.
#' @family runtime helpers
#' @examples
#' sas_sum(c(1, NA, 2), c(1, NA, NA))
#' sas_mean(c(1, NA, 3), c(3, NA, NA))
#' @export
sas_sum <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(numeric(0))
  n <- max(vapply(args, length, integer(1)))
  if (n == 0L) return(numeric(0))
  total <- rep(0, n)
  any_ok <- rep(FALSE, n)
  for (a in args) {
    if (length(a) == 0L) next
    a <- rep_len(as.numeric(a), n)
    ok <- !is.na(a)
    total[ok] <- total[ok] + a[ok]
    any_ok <- any_ok | ok
  }
  total[!any_ok] <- NA_real_
  total
}

#' @rdname sas_sum
#' @export
sas_mean <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(numeric(0))
  n <- max(vapply(args, length, integer(1)))
  if (n == 0L) return(numeric(0))
  total <- rep(0, n)
  cnt <- rep(0L, n)
  for (a in args) {
    if (length(a) == 0L) next
    a <- rep_len(as.numeric(a), n)
    ok <- !is.na(a)
    total[ok] <- total[ok] + a[ok]
    cnt[ok] <- cnt[ok] + 1L
  }
  out <- rep(NA_real_, n)
  has <- cnt > 0L
  out[has] <- total[has] / cnt[has]
  out
}

#' Round the way SAS does
#'
#' SAS `ROUND` rounds half away from zero, to a rounding unit; R's `round()`
#' is banker's rounding.
#'
#' @param x A numeric vector.
#' @param unit The rounding unit (SAS's second argument).
#' @return `x` rounded to the nearest multiple of `unit`.
#' @family runtime helpers
#' @examples
#' sas_round(c(0.5, 1.5, -0.5))
#' sas_round(12.345, 0.01)
#' @export
sas_round <- function(x, unit = 1) {
  if (any(!is.na(unit) & (!is.finite(unit) | unit <= 0)))
    stop("sas_round: rounding units must be positive and finite; nonpositive units are unsupported", call. = FALSE)
  q <- abs(x) / unit
  q <- ifelse(is.finite(q) & q < 1e15, signif(q, 15), q)
  rounded <- ifelse(q >= 2^52, q, floor(q + 0.5))
  inverse <- 1 / unit
  decimal <- rep_len(is.finite(inverse) & abs(inverse - round(inverse)) < 1e-9, length(q))
  sign(x) * ifelse(decimal, rounded / round(inverse), rounded * unit)
}

#' SAS character functions
#'
#' `sas_compress()` is `COMPRESS`: every character listed in `chars` is
#' removed literally, including `]` and `-`. SAS modifiers are unsupported. `sas_substr()` is `SUBSTR`; a `NULL`
#' `len` reads to the end of the string. `sas_length()` is `LENGTH`: trailing
#' blanks do not count, and a missing or blank value has length 1, as in SAS.
#'
#' @param x A character vector (or a vector coerced to character).
#' @param chars Characters to remove.
#' @param pos,len Start position and length, as in SAS.
#' @return `sas_compress()` and `sas_substr()` return character vectors;
#'   `sas_length()` an integer vector.
#' @family runtime helpers
#' @examples
#' sas_compress("a b c")
#' sas_substr("abcdef", 2, 3)
#' sas_length(c("abc  ", "", NA))
#' @export
sas_compress <- function(x, chars = " ") {
  x <- as.character(x)
  for (char in unique(strsplit(chars, "", fixed = TRUE)[[1L]])) x <- gsub(char, "", x, fixed = TRUE)
  x
}

#' @rdname sas_compress
#' @export
sas_substr <- function(x, pos, len = NULL) {
  if (is.null(len)) substr(x, pos, nchar(x))
  else substr(x, pos, pmax(pos + len - 1L, pos - 1L))
}

#' Row-wise MIN and MAX with SAS missing semantics
#'
#' Row-wise across the arguments, ignoring missing values; an observation
#' whose arguments are all missing yields missing, and a single argument
#' passes through elementwise -- SAS `MIN(x)` is never a column aggregate.
#'
#' @param ... Numeric vectors, recycled to a common length.
#' @return A numeric vector of that length.
#' @family runtime helpers
#' @examples
#' sas_min(c(1, NA, 3), c(2, NA, 1))
#' sas_max(c(1, NA, 3), c(2, NA, 1))
#' @export
sas_min <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(numeric(0))
  # SAS MIN(x) with one argument is x, elementwise -- never a column aggregate.
  if (length(args) == 1L) return(args[[1]])
  res <- do.call(pmin, c(args, list(na.rm = TRUE)))
  all_na <- Reduce(`&`, lapply(args, is.na))
  res[all_na] <- NA_real_
  res
}

#' @rdname sas_min
#' @export
sas_max <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(numeric(0))
  if (length(args) == 1L) return(args[[1]])
  res <- do.call(pmax, c(args, list(na.rm = TRUE)))
  all_na <- Reduce(`&`, lapply(args, is.na))
  res[all_na] <- NA_real_
  res
}

#' @rdname sas_compress
#' @export
sas_length <- function(x) {
  if (is.character(x)) {
    s <- sub(" +$", "", x)
    ifelse(is.na(s) | s == "", 1L, nchar(s))
  } else {
    rep.int(12L, length(x))
  }
}

#' IF/THEN/ELSE assignment with SAS semantics
#'
#' A missing condition is false in SAS. The result keeps the class (`Date`,
#' labelled) of the branch that carries it, which `ifelse()` would strip.
#'
#' @param cond A logical vector.
#' @param yes,no Values for true and false conditions; recycled.
#' @return A vector of the common length.
#' @family runtime helpers
#' @examples
#' sas_if_else(c(TRUE, NA, FALSE), "yes", "no")
#' sas_if_else(c(TRUE, FALSE), as.Date("2026-01-01"), NA)
#' @export
sas_if_else <- function(cond, yes, no) {
  n <- max(length(cond), length(yes), length(no))
  cond <- rep_len(cond, n)
  idx <- !is.na(cond) & cond
  if (length(no) == 1L && is.logical(no) && is.na(no)) {
    # New-variable form: the false branch is bare NA, so the yes branch is the
    # only carrier of class information.
    out <- rep_len(yes, n)
    out[!idx] <- NA
    return(out)
  }
  yes <- rep_len(yes, n)
  out <- rep_len(no, n)
  out[idx] <- yes[idx]
  out
}

#' Display values the way SAS prints them
#'
#' Numeric missing is `"."`, character missing is blank, and numbers are never
#' in scientific notation.
#'
#' @param x A vector.
#' @return A character vector.
#' @family runtime helpers
#' @examples
#' sas_display(c(1, NA, 1e6))
#' sas_display(c("a", NA))
#' @export
sas_display <- function(x) {
  if (is.numeric(x)) {
    out <- character(length(x))
    na_idx <- is.na(x)
    out[na_idx] <- "."
    if (any(!na_idx)) {
      out[!na_idx] <- vapply(x[!na_idx], function(value) format(value, digits = 12, trim = TRUE, scientific = FALSE), "")
    }
  } else {
    out <- as.character(x)
    out[is.na(x)] <- ""
  }
  out
}

#' SAS missing values and Boolean conditions
#'
#' Character missing is NA or all spaces. Numeric missing is NA (including
#' tagged missing values). A missing or zero numeric condition is false.
#' Character conditions convert to numeric; nonnumeric text is false.
#' @param x A vector.
#' @return A logical vector with no missing values.
#' @family runtime helpers
#' @examples
#' sas_missing(c("", " ", NA, "A"))
#' sas_true(c(NA, 0, 1, -1))
#' @export
sas_missing <- function(x) is.na(x) | (is.character(x) & !is.na(x) & grepl("^ *$", x))

#' @rdname sas_missing
#' @export
sas_true <- function(x) {
  if (is.character(x)) x <- suppressWarnings(as.numeric(trimws(x)))
  !is.na(x) & x != 0
}
