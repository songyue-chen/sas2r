DL_TOKENS <- c("datalines", "cards", "datalines4", "cards4", "parmcards", "parmcards4")

#' Classify every character of SAS source code
#'
#' Walks the source character-by-character and classifies every character
#' as code (`c`), single-quoted string (`s`), double-quoted string (`q`),
#' comment (`k`), or datalines data (`d`).
#'
#' @param text A length-1 character string representing SAS source code.
#' @return A list with two components:
#'   \item{chars}{Character vector of individual characters.}
#'   \item{mask}{Character vector of single-character type tags:
#'     `"c"` (code), `"s"` (single-quoted string), `"q"` (double-quoted string),
#'     `"k"` (comment), `"d"` (datalines data).}
#'
#' @section Known Limitations (v1):
#' \itemize{
#'   \item `datalines4` and `cards4` accept a lone `;` terminator instead of requiring `;;;;`.
#'   \item Macro-quoted semicolons (e.g. `%str(;)`) are treated as code semicolons.
#' }
#' Both limitations are tracked as flags in downstream risk analysis.
#'
#' @noRd
sas_scan <- function(text) {
  stopifnot(is.character(text), length(text) == 1L)
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  mask <- character(n)
  state <- "code"
  at_stmt_start <- TRUE
  last_split <- 0L
  line_start_pos <- 1L
  first_token_of <- function(from, to) {
    if (from > to) return("")
    idx <- from:to
    keep <- idx[mask[idx] %in% c("c", "s", "q")]
    if (!length(keep)) return("")
    stmt <- paste(chars[keep], collapse = "")
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
      else if (at_stmt_start && ch == "*") { state <- "starc"; mask[i] <- "k" }
      else if (at_stmt_start && ch == "%" && nxt == "*") { state <- "starc"; mask[i] <- "k"; mask[i + 1L] <- "k"; i <- i + 1L }
      else {
        mask[i] <- "c"
        if (ch == ";") {
          tok <- first_token_of(last_split + 1L, i - 1L)
          if (tok %in% DL_TOKENS) state <- "datalines"
          last_split <- i
          at_stmt_start <- TRUE
        } else if (!grepl("[[:space:]]", ch)) at_stmt_start <- FALSE
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
      if (ch == ";") {
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

#' Blank string literal contents while preserving string delimiters and character offsets
#'
#' Replaces characters inside string literals with spaces so regexes or character scans
#' do not match syntax elements (such as parentheses or macro triggers) inside strings.
#' Delimiters (`'` and `"`) are preserved so character offsets map 1:1 to the original text.
#'
#' @param text Length-1 character: SAS code text.
#' @param keep_double Logical: if `TRUE`, double-quoted string contents are preserved
#'   (because SAS macro calls expand inside double quotes). Defaults to `FALSE`.
#' @return Length-1 character string of identical length with masked string contents.
#' @noRd
mask_strings <- function(text, keep_double = FALSE) {
  sc <- sas_scan(text)
  chars <- sc$chars
  drop <- if (keep_double) sc$mask == "s" else sc$mask %in% c("s", "q")
  body <- drop & !(chars %in% c("'", '"'))
  chars[body] <- " "
  paste(chars, collapse = "")
}

NON_CALL_MACRO_KEYWORDS <- c(
  "let", "if", "then", "else", "do", "end", "macro", "mend",
  "global", "local", "goto", "return", "abort",
  "include", "put", "input", "display", "window", "sysexec",
  "symdel", "sysrput", "syslput", "sysmacdelete", "copy", "syscall"
)

# Helper to split a code statement that contains an unsemicoloned macro call
split_macro_statement <- function(txt, l_start, l_end, positions) {
  # Match a leading %macro_call(...) or %macro_call followed by newline/whitespace and any remainder
  m_tok <- regexec("^%([A-Za-z_][A-Za-z0-9_]*)", txt)
  m_match <- regmatches(txt, m_tok)[[1]]
  if (length(m_match) >= 2L && nzchar(m_match[1])) {
    mac_name <- tolower(m_match[2])
    if (!mac_name %in% NON_CALL_MACRO_KEYWORDS) {
      name_len <- attr(m_tok[[1]], "match.length")[1]

      after_name <- substring(txt, name_len + 1L)
      m_paren <- regexpr("^[ \t\r\n]*\\(", after_name, perl = TRUE)

      if (m_paren == 1L) {
        open_paren_offset <- attr(m_paren, "match.length")
        open_paren_pos <- name_len + open_paren_offset

        masked <- mask_strings(txt)
        # Scan parens using masked string to ignore parens inside quotes
        chars_m <- strsplit(masked, "", fixed = TRUE)[[1]]
        n_m <- length(chars_m)
        depth <- 0L
        close_paren_pos <- NA_integer_
        for (j in open_paren_pos:n_m) {
          ch <- chars_m[j]
          if (ch == "(") depth <- depth + 1L
          else if (ch == ")") {
            depth <- depth - 1L
            if (depth == 0L) {
              close_paren_pos <- j
              break
            }
          }
        }

        if (!is.na(close_paren_pos)) {
          after_call <- substring(txt, close_paren_pos + 1L)
          m_sep <- regexpr("^[ \t]*\r?\n[ \t\r\n]*|^[ \t]+", after_call, perl = TRUE)
          if (m_sep == 1L) {
            sep_len <- attr(m_sep, "match.length")
            rest_txt <- substring(after_call, sep_len + 1L)
            if (nzchar(trimws(rest_txt))) {
              call_positions <- positions[seq_len(close_paren_pos)]
              call_txt <- trimws(substr(txt, 1L, close_paren_pos))
              consumed_prefix <- substr(txt, 1L, close_paren_pos + sep_len)

              call_nl <- gregexpr("\n", call_txt, fixed = TRUE)[[1]]
              call_newlines <- if (length(call_nl) == 1L && call_nl[1] == -1L) 0L else length(call_nl)
              split_line <- l_start + call_newlines

              prefix_nl <- gregexpr("\n", consumed_prefix, fixed = TRUE)[[1]]
              prefix_newlines <- if (length(prefix_nl) == 1L && prefix_nl[1] == -1L) 0L else length(prefix_nl)
              rem_start <- l_start + prefix_newlines

              first_stmt <- tibble::tibble(
                text = call_txt,
                first_token = paste0("%", mac_name),
                type = "code",
                line_start = l_start,
                line_end = split_line,
                char_start = min(call_positions),
                char_end = max(call_positions)
              )
              rest_positions <- positions[(close_paren_pos + sep_len + 1L):length(positions)]
              rest_stmts <- split_macro_statement(rest_txt, rem_start, l_end, rest_positions)
              return(rbind(first_stmt, rest_stmts))
            }
          }
        }
      } else {
        # No parens: require newline before remainder
        m_nl <- regexpr("^[ \t]*\r?\n[ \t\r\n]*", after_name, perl = TRUE)
        if (m_nl == 1L) {
          sep_len <- attr(m_nl, "match.length")
          rest_txt <- substring(after_name, sep_len + 1L)
          if (nzchar(trimws(rest_txt))) {
            call_positions <- positions[seq_len(name_len)]
            call_txt <- trimws(substr(txt, 1L, name_len))
            consumed_prefix <- substr(txt, 1L, name_len + sep_len)

            call_nl <- gregexpr("\n", call_txt, fixed = TRUE)[[1]]
            call_newlines <- if (length(call_nl) == 1L && call_nl[1] == -1L) 0L else length(call_nl)
            split_line <- l_start + call_newlines

            prefix_nl <- gregexpr("\n", consumed_prefix, fixed = TRUE)[[1]]
            prefix_newlines <- if (length(prefix_nl) == 1L && prefix_nl[1] == -1L) 0L else length(prefix_nl)
            rem_start <- l_start + prefix_newlines

            first_stmt <- tibble::tibble(
              text = call_txt,
              first_token = paste0("%", mac_name),
              type = "code",
              line_start = l_start,
              line_end = split_line,
              char_start = min(call_positions),
              char_end = max(call_positions)
            )
            rest_positions <- positions[(name_len + sep_len + 1L):length(positions)]
            rest_stmts <- split_macro_statement(rest_txt, rem_start, l_end, rest_positions)
            return(rbind(first_stmt, rest_stmts))
          }
        }
      }
    }
  }

  tok <- regmatches(txt, regexpr("^%?[A-Za-z_][A-Za-z0-9_]*", txt))
  tibble::tibble(
    text = txt,
    first_token = if (length(tok)) tolower(tok) else "",
    type = "code",
    line_start = l_start,
    line_end = l_end,
    char_start = min(positions),
    char_end = max(positions)
  )
}

empty_sas_statements <- function() {
  tibble::tibble(
    stmt_id = integer(), text = character(), first_token = character(),
    type = character(), line_start = integer(), line_end = integer(),
    char_start = integer(), char_end = integer()
  )
}

empty_sas_comments <- function() {
  tibble::tibble(
    comment_id = integer(), text = character(), kind = character(),
    line_start = integer(), line_end = integer(),
    char_start = integer(), char_end = integer()
  )
}

comment_kind <- function(text) {
  trimmed <- trimws(text)
  if (startsWith(trimmed, "/*")) "block"
  else if (startsWith(trimmed, "%*")) "macro"
  else "statement"
}

comment_records_from_scan <- function(chars, mask, line_no) {
  at <- which(mask == "k")
  if (!length(at)) return(empty_sas_comments())
  spans <- list()
  i <- at[1]
  while (i <= length(chars)) {
    if (!identical(mask[i], "k")) {
      i <- i + 1L
      next
    }
    start <- i
    if (identical(chars[i], "/") && i < length(chars) &&
        identical(chars[i + 1L], "*")) {
      end <- i + 1L
      while (end < length(chars) &&
          !(identical(chars[end], "*") && identical(chars[end + 1L], "/"))) {
        end <- end + 1L
      }
      if (end < length(chars)) end <- end + 1L
    } else {
      end <- i
      while (end < length(chars) && identical(mask[end + 1L], "k")) {
        end <- end + 1L
      }
    }
    spans[[length(spans) + 1L]] <- start:end
    i <- end + 1L
  }
  rows <- lapply(seq_along(spans), function(i) {
    span <- spans[[i]]
    start <- min(span)
    end <- max(span)
    raw <- paste(chars[span], collapse = "")
    kind <- comment_kind(raw)
    if (kind %in% c("statement", "macro") && end < length(chars) &&
        identical(chars[end + 1L], ";") && identical(mask[end + 1L], "c")) {
      end <- end + 1L
      raw <- paste0(raw, ";")
    }
    tibble::tibble(
      comment_id = as.integer(i), text = raw, kind = kind,
      line_start = as.integer(line_no[start]),
      line_end = as.integer(line_no[end]),
      char_start = as.integer(start), char_end = as.integer(end)
    )
  })
  do.call(rbind, rows)
}

trim_statement_positions <- function(chars, positions, is_data) {
  if (!length(positions)) return(integer())
  trim_char <- if (is_data) chars[positions] %in% c("\r", "\n") else
    grepl("^[[:space:]]$", chars[positions])
  start <- 1L
  end <- length(positions)
  while (start <= end && trim_char[start]) start <- start + 1L
  while (end >= start && trim_char[end]) end <- end - 1L
  if (start > end) integer() else positions[start:end]
}

#' Split SAS source into statements
#'
#' Splits on semicolons that are real code (never inside strings, comments,
#' or datalines blocks). Comment text is stripped from statement text.
#' @param text Length-1 character: full SAS source.
#' @return A tibble with columns stmt_id, text, first_token, type,
#'   line_start, line_end.
#' @noRd
sas_source_records <- function(text) {
  sc <- sas_scan(text)
  chars <- sc$chars; mask <- sc$mask
  n <- length(chars)
  comments <- comment_records_from_scan(chars, mask, cumsum(chars == "\n") + 1L)
  if (n == 0L) {
    return(list(statements = empty_sas_statements(), comments = comments))
  }
  line_no <- cumsum(chars == "\n") + 1L
  splits <- which(chars == ";" & mask == "c")
  bounds_start <- c(1L, utils::head(splits, -1L) + 1L)
  bounds_end <- splits
  if (!length(splits)) { bounds_start <- 1L; bounds_end <- n }
  else if (max(splits) < n) {
    bounds_start <- c(bounds_start, max(splits) + 1L)
    bounds_end <- c(bounds_end, n)
  }
  out <- lapply(seq_along(bounds_start), function(k) {
    idx <- bounds_start[k]:bounds_end[k]
    is_data <- any(mask[idx] == "d")
    keep <- if (is_data) idx[mask[idx] == "d"] else idx[mask[idx] %in% c("c", "s", "q")]
    if (!is_data && length(keep) && chars[keep[length(keep)]] == ";" && mask[keep[length(keep)]] == "c")
      keep <- keep[-length(keep)]

    non_ws <- keep[!grepl("^[[:space:]]$", chars[keep])]
    l_start <- if (length(non_ws)) line_no[non_ws[1]] else (if (length(keep)) line_no[keep[1]] else line_no[bounds_start[k]])
    l_end <- if (length(non_ws)) line_no[non_ws[length(non_ws)]] else (if (length(keep)) line_no[keep[length(keep)]] else line_no[bounds_end[k]])
    positions <- trim_statement_positions(chars, keep, is_data)
    txt_trim <- if (length(positions)) paste(chars[positions], collapse = "") else ""

    if (is_data) {
      span_positions <- if (length(positions)) positions else keep
      tibble::tibble(
        text = txt_trim,
        first_token = "",
        type = "datalines_data",
        line_start = l_start,
        line_end = l_end,
        char_start = min(span_positions),
        char_end = max(span_positions)
      )
    } else if (nzchar(txt_trim)) {
      split_macro_statement(txt_trim, l_start, l_end, positions)
    } else {
      tibble::tibble(
        text = character(),
        first_token = character(),
        type = character(),
        line_start = integer(),
        line_end = integer(),
        char_start = integer(),
        char_end = integer()
      )
    }
  })

  res <- do.call(rbind, out)
  res <- res[!(res$type == "code" & res$text == ""), ]
  if (is.null(res) || nrow(res) == 0L) {
    return(list(statements = empty_sas_statements(), comments = comments))
  }
  res$stmt_id <- seq_len(nrow(res))
  list(
    statements = tibble::as_tibble(res[, c(
      "stmt_id", "text", "first_token", "type", "line_start", "line_end",
      "char_start", "char_end"
    )]),
    comments = comments
  )
}

#' Split SAS source into statements
#'
#' Splits on semicolons that are real code (never inside strings, comments,
#' or datalines blocks). Comment text is stripped from statement text.
#' @param text Length-1 character: full SAS source.
#' @return A tibble with columns stmt_id, text, first_token, type,
#'   line_start, line_end.
#' @noRd
sas_statements <- function(text) {
  statements <- sas_source_records(text)$statements
  tibble::as_tibble(statements[, c(
    "stmt_id", "text", "first_token", "type", "line_start", "line_end"
  ), drop = FALSE])
}

#' Format SAS statement lines into valid, semicolon-terminated SAS text
#' @param text Character vector of SAS statements or statement text.
#' @return Length-1 character string of formatted SAS statements.
#' @noRd
format_sas_statements <- function(text) {
  if (is.null(text) || !length(text)) return("")
  text <- as.character(text)
  text <- text[!is.na(text) & nzchar(trimws(text))]
  if (!length(text)) return("")
  needs_semi <- !grepl(";\\s*$", text)
  text[needs_semi] <- paste0(text[needs_semi], ";")
  paste(text, collapse = "\n")
}

