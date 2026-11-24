# The scanner as it stood before %str()/%nrstr() arguments and the two
# instream-data ending rules were followed (R/scan.R at develop 42ad8c51),
# kept verbatim as an independent reference. test-scan-equivalence.R uses it to
# show that sources containing none of those constructs are classified exactly
# as before, and that the fallback pass reproduces the previous rules. Do not
# edit it to follow the live scanner: it is only useful while it stays frozen.
legacy_sas_scan <- function(text) {
  stopifnot(is.character(text), length(text) == 1L)
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  whitespace <- grepl("[[:space:]]", chars)
  mask <- character(n)
  state <- "code"
  macro_comment <- FALSE
  comment_quote <- ""
  at_stmt_start <- TRUE
  last_split <- 0L
  line_start_pos <- 1L
  first_token_of <- function(from, to) {
    if (from > to) return("")
    idx <- from:to
    keep <- idx[mask[idx] %in% c("c", "s", "q", "k")]
    if (!length(keep)) return("")
    token_chars <- chars[keep]
    token_chars[mask[keep] == "k"] <- " "
    stmt <- paste(token_chars, collapse = "")
    stmt <- sub("^[[:space:]]+", "", stmt)
    m <- regmatches(stmt, regexpr("^%?[A-Za-z_][A-Za-z0-9_]*", stmt))
    if (length(m)) tolower(m) else ""
  }
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    nxt <- if (i < n) chars[i + 1L] else ""
    if (state == "code") {
      if (ch == "'") { state <- "sq"; mask[i] <- "s" }
      else if (ch == '"') { state <- "dq"; mask[i] <- "q" }
      else if (ch == "/" && nxt == "*") { state <- "blockc"; mask[i] <- "k"; mask[i + 1L] <- "k"; i <- i + 1L }
      else if (at_stmt_start && ch == "*") { state <- "starc"; macro_comment <- FALSE; mask[i] <- "k" }
      else if (ch == "%" && nxt == "*") { state <- "starc"; macro_comment <- TRUE; comment_quote <- ""; mask[i] <- "k"; mask[i + 1L] <- "k"; i <- i + 1L }
      else {
        mask[i] <- "c"
        if (ch == ";") {
          tok <- first_token_of(last_split + 1L, i - 1L)
          if (tok %in% DL_TOKENS) state <- "datalines"
          last_split <- i
          at_stmt_start <- TRUE
        } else if (!whitespace[i]) at_stmt_start <- FALSE
      }
    } else if (state == "sq") {
      mask[i] <- "s"
      if (ch == "'") { if (nxt == "'") { mask[i + 1L] <- "s"; i <- i + 1L } else state <- "code" }
    } else if (state == "dq") {
      mask[i] <- "q"
      if (ch == '"') { if (nxt == '"') { mask[i + 1L] <- "q"; i <- i + 1L } else state <- "code" }
    } else if (state == "blockc") {
      mask[i] <- "k"
      if (ch == "*" && nxt == "/") { mask[i + 1L] <- "k"; i <- i + 1L; state <- "code" }
    } else if (state == "starc") {
      if (macro_comment && ch %in% c("'", '"')) {
        if (comment_quote == "") comment_quote <- ch
        else if (comment_quote == ch) comment_quote <- ""
      }
      if (ch == ";" && (!macro_comment || comment_quote == "")) {
        mask[i] <- "c"
        state <- "code"
        at_stmt_start <- TRUE
        last_split <- i
      } else {
        mask[i] <- "k"
      }
    } else { # datalines
      if (ch == ";") {
        seg <- if (line_start_pos <= i - 1L) paste(chars[line_start_pos:(i - 1L)], collapse = "") else ""
        if (grepl("^[[:space:]]*$", seg)) {
          mask[i] <- "c"; state <- "code"; at_stmt_start <- TRUE; last_split <- i
        } else mask[i] <- "d"
      } else mask[i] <- "d"
    }
    if (ch == "\n") line_start_pos <- i + 1L
    i <- i + 1L
  }
  list(chars = chars, mask = mask)
}
