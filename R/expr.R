tokenize_expr <- function(txt) {
  pat <- paste0(
    "('([^']|'')*')|(\"([^\"]|\"\")*\")",
    "|(\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?)|(\\.\\d+(?:[eE][+-]?\\d+)?)",
    "|([A-Za-z_]\\w*)",
    "|(\\^=|~=|<=|>=|=|<|>|\\|\\||&|\\||!|\\(|\\)|,|\\+|\\-|\\*\\*|\\*|/|\\.)"
  )
  # Check for unsupported characters/tokens (Fix F9)
  stripped <- trimws(gsub(pat, "", txt, perl = TRUE))
  if (nzchar(stripped)) {
    cli::cli_abort("unsupported syntax or characters in expression: {.code {txt}}",
                    class = "sas2r_expr_parse_error")
  }
  m <- gregexpr(pat, txt, perl = TRUE)[[1]]
  if (m[1] == -1) return(character())
  regmatches(txt, list(m))[[1]]
}

translate_expr <- function(txt) {
  rb <- load_rulebook()
  toks <- tokenize_expr(txt)
  out <- character(length(toks))
  for (i in seq_along(toks)) {
    t <- toks[i]; tl <- tolower(t)
    nxt <- if (i < length(toks)) toks[i + 1] else ""
    if (grepl("^['\"]", t)) out[i] <- t
    # A bare . is the SAS numeric missing literal; NA_real_ keeps the column
    # numeric, and wrap_missing routes comparisons against it through chr_cmp.
    else if (t == ".") out[i] <- "NA_real_"
    else if (grepl("^[0-9.]", t)) out[i] <- t
    else if (tl %in% names(rb$operators)) out[i] <- rb$operators[[tl]]
    else if (tl %in% names(rb$functions) && nxt == "(") out[i] <- rb$functions[[tl]]
    else if (grepl("^[A-Za-z_]", t) && nxt == "(") {
      # In a SAS expression a name before ( is a SAS function (or array)
      # reference. Passing it through verbatim can bind to an unrelated base R
      # function -- SCAN(str, 2) would become base::scan() -- so an unmapped
      # name refuses instead.
      cli::cli_abort(
        "unmapped SAS function in expression: {.code {t}}",
        class = c("sas2r_expr_unmapped_function", "sas2r_expr_parse_error"),
        fname = tl
      )
    }
    # Bare names are SAS variables, and SAS resolves them case-insensitively.
    # Deterministic pipelines fold frames to lowercase names at entry, so
    # every emitted reference folds to match.
    else if (grepl("^[A-Za-z_]", t)) out[i] <- tl
    else out[i] <- t
  }
  i <- 1L; merged <- character()
  while (i <= length(out)) {
    if (i < length(out) && out[i] == "!" && out[i + 1L] == "%in%") {
      merged <- c(merged, "%notin%"); i <- i + 2L
    } else { merged <- c(merged, out[i]); i <- i + 1L }
  }
  paste(merged, collapse = " ")
}

#' Comma-separate the elements of a translated `%in%` / `%notin%` list
#'
#' SAS accepts a space-delimited membership list -- `x in (815 817)` -- which
#' has no R equivalent, so the elements have to be joined explicitly. Splitting
#' is quote-aware: `in ('A B' 'C')` is two elements, not three. Elements that
#' are already comma-separated pass through, so a mixed list normalizes.
#'
#' @param body Text between the parentheses of a membership list.
#' @return The same elements, comma-separated.
#' @noRd
comma_separate_in_list <- function(body) {
  chars <- strsplit(body, "", fixed = TRUE)[[1]]
  if (!length(chars)) return(body)
  out <- character(0)
  cur <- character(0)
  quote_ch <- ""
  depth <- 0L
  flush <- function() {
    tok <- trimws(paste(cur, collapse = ""))
    if (nzchar(tok)) out <<- c(out, tok)
    cur <<- character(0)
  }
  for (ch in chars) {
    if (nzchar(quote_ch)) {
      cur <- c(cur, ch)
      if (ch == quote_ch) quote_ch <- ""
    } else if (ch %in% c("'", "\"")) {
      quote_ch <- ch
      cur <- c(cur, ch)
    } else if (ch == "(") {
      depth <- depth + 1L; cur <- c(cur, ch)
    } else if (ch == ")") {
      depth <- depth - 1L; cur <- c(cur, ch)
    } else if (depth == 0L && (ch == "," || grepl("\\s", ch))) {
      flush()
    } else {
      cur <- c(cur, ch)
    }
  }
  flush()
  paste(out, collapse = ", ")
}

#' Rewrite every membership list in a translated expression
#' @noRd
fix_in_lists <- function(x) {
  repeat {
    m <- regexpr("%(not)?in% \\(", x)
    if (m[[1]] == -1L) break
    open <- m[[1]] + attr(m, "match.length") - 1L
    depth <- 0L; close <- NA_integer_; quote_ch <- ""
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    for (i in seq(open, length(chars))) {
      ch <- chars[i]
      if (nzchar(quote_ch)) { if (ch == quote_ch) quote_ch <- "" ; next }
      if (ch %in% c("'", "\"")) { quote_ch <- ch; next }
      if (ch == "(") depth <- depth + 1L
      else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) { close <- i; break } }
    }
    if (is.na(close)) break
    body <- substr(x, open + 1L, close - 1L)
    op <- substr(x, m[[1]], open - 2L)
    x <- paste0(substr(x, 1L, m[[1]] - 1L), op, " c(",
                comma_separate_in_list(body), ")",
                substr(x, close + 1L, nchar(x)))
  }
  x
}

tidy_expr <- function(r_expr) {
  x <- fix_in_lists(r_expr)
  x <- gsub("\\(\\s+", "(", x); x <- gsub("\\s+\\)", ")", x)
  x <- gsub("([A-Za-z_0-9.])\\s+\\(", "\\1(", x)   # fn ( -> fn(
  x <- gsub("\\s+,", ",", x); x <- gsub(",\\s*", ", ", x)
  x <- gsub("!\\s+", "!", x)
  x <- trimws(gsub("\\s+", " ", x))
  ok <- tryCatch({ parse(text = x); TRUE }, error = function(e) FALSE)
  if (!ok) cli::cli_abort("translated expression does not parse: {.code {x}}",
                          class = "sas2r_expr_parse_error")
  x
}

wrap_missing <- function(r_expr, vars) {
  if (!length(vars)) return(r_expr)
  e <- tryCatch(parse(text = r_expr)[[1]], error = function(err) {
    cli::cli_abort("expression does not parse in wrap_missing: {.code {r_expr}}",
                   class = "sas2r_expr_parse_error")
  })

  transform_node <- function(node) {
    if (!is.call(node)) return(node)
    fn <- as.character(node[[1]])[1]
    if (fn %in% c("%in%", "%notin%")) {
      lhs <- transform_node(node[[2]])
      rhs <- transform_node(node[[3]])
      is_char_rhs <- FALSE
      if (is.call(rhs) && as.character(rhs[[1]])[1] == "c") {
        for (j in seq_along(rhs)[-1]) {
          if (is.character(rhs[[j]])) {
            is_char_rhs <- TRUE
            rhs[[j]] <- sub("\\s+$", "", rhs[[j]])
          }
        }
      } else if (is.character(rhs)) {
        is_char_rhs <- TRUE
        rhs <- sub("\\s+$", "", rhs)
      }
      if (is_char_rhs && is.name(lhs) && tolower(as.character(lhs)) %in% vars) {
        lhs <- substitute(sub("\\s+$", "", X), list(X = lhs))
      }
      node[[2]] <- lhs
      node[[3]] <- rhs
      return(node)
    }
    if (fn %in% c("<", "<=", "!=", ">", ">=", "==")) {
      lhs <- transform_node(node[[2]])
      rhs <- transform_node(node[[3]])
      lhs_var <- if (is.name(lhs) && tolower(as.character(lhs)) %in% vars) as.character(lhs) else NULL
      rhs_var <- if (is.name(rhs) && tolower(as.character(rhs)) %in% vars) as.character(rhs) else NULL

      if (is.character(lhs) || is.character(rhs)) {
        if (is.character(lhs)) lhs <- sub("\\s+$", "", lhs) else lhs <- substitute(sub("\\s+$", "", X), list(X = lhs))
        if (is.character(rhs)) rhs <- sub("\\s+$", "", rhs) else rhs <- substitute(sub("\\s+$", "", X), list(X = rhs))
        return(substitute(chr_cmp(LHS, RHS, OP), list(LHS = lhs, RHS = rhs, OP = fn)))
      }

      is_arithmetic <- function(x) {
        # An NA literal is the translated SAS missing (.), not arithmetic: it
        # must fall through to chr_cmp, where missing sorts lowest, because the
        # is.na()-wrap around x == NA would evaluate to NA on every row.
        (is.numeric(x) && length(x) == 1L && !is.na(x)) ||
          (is.call(x) && as.character(x[[1]])[1] %in% c("+", "-", "*", "/", "^", "%%", "%/%", "sas_sum", "sas_mean", "sas_min", "sas_max", "sas_round"))
      }

      if (!is.null(lhs_var) && !is.null(rhs_var)) {
        return(substitute(chr_cmp(LHS, RHS, OP), list(LHS = lhs, RHS = rhs, OP = fn)))
      } else if (!is.null(lhs_var)) {
        v <- lhs_var
        if (!is_arithmetic(rhs)) {
          return(substitute(chr_cmp(LHS, RHS, OP), list(LHS = lhs, RHS = rhs, OP = fn)))
        }
        cmp_clause <- switch(fn,
          "<"  = substitute(LHS < RHS, list(LHS = lhs, RHS = rhs)),
          "<=" = substitute(LHS <= RHS, list(LHS = lhs, RHS = rhs)),
          "!=" = substitute(LHS != RHS, list(LHS = lhs, RHS = rhs)),
          ">"  = substitute(LHS > RHS, list(LHS = lhs, RHS = rhs)),
          ">=" = substitute(LHS >= RHS, list(LHS = lhs, RHS = rhs)),
          "==" = substitute(LHS == RHS, list(LHS = lhs, RHS = rhs))
        )
        new_call <- switch(fn,
          "<"  = substitute((is.na(v) | CMP), list(v = as.name(v), CMP = cmp_clause)),
          "<=" = substitute((is.na(v) | CMP), list(v = as.name(v), CMP = cmp_clause)),
          "!=" = substitute((is.na(v) | CMP), list(v = as.name(v), CMP = cmp_clause)),
          ">"  = substitute((!is.na(v) & CMP), list(v = as.name(v), CMP = cmp_clause)),
          ">=" = substitute((!is.na(v) & CMP), list(v = as.name(v), CMP = cmp_clause)),
          "==" = substitute((!is.na(v) & CMP), list(v = as.name(v), CMP = cmp_clause))
        )
        return(new_call)
      } else if (!is.null(rhs_var)) {
        v <- rhs_var
        if (!is_arithmetic(lhs)) {
          return(substitute(chr_cmp(LHS, RHS, OP), list(LHS = lhs, RHS = rhs, OP = fn)))
        }
        cmp_clause <- switch(fn,
          "<"  = substitute(LHS < RHS, list(LHS = lhs, RHS = rhs)),
          "<=" = substitute(LHS <= RHS, list(LHS = lhs, RHS = rhs)),
          "!=" = substitute(LHS != RHS, list(LHS = lhs, RHS = rhs)),
          ">"  = substitute(LHS > RHS, list(LHS = lhs, RHS = rhs)),
          ">=" = substitute(LHS >= RHS, list(LHS = lhs, RHS = rhs)),
          "==" = substitute(LHS == RHS, list(LHS = lhs, RHS = rhs))
        )
        new_call <- switch(fn,
          "<"  = substitute((!is.na(v) & CMP), list(v = as.name(v), CMP = cmp_clause)),
          "<=" = substitute((!is.na(v) & CMP), list(v = as.name(v), CMP = cmp_clause)),
          "!=" = substitute((is.na(v) | CMP), list(v = as.name(v), CMP = cmp_clause)),
          ">"  = substitute((is.na(v) | CMP), list(v = as.name(v), CMP = cmp_clause)),
          ">=" = substitute((is.na(v) | CMP), list(v = as.name(v), CMP = cmp_clause)),
          "==" = substitute((!is.na(v) & CMP), list(v = as.name(v), CMP = cmp_clause))
        )
        return(new_call)
      } else {
        node[[2]] <- lhs
        node[[3]] <- rhs
        return(node)
      }
    }
    for (i in seq_along(node)[-1]) {
      node[[i]] <- transform_node(node[[i]])
    }
    node
  }

  res <- transform_node(e)
  paste(deparse(res, width.cutoff = 500L), collapse = " ")
}

expr_vars <- function(txt, r_expr = NULL) {
  if (is.null(r_expr)) r_expr <- tidy_expr(translate_expr(txt))
  e <- tryCatch(parse(text = r_expr)[[1]], error = function(err) {
    cli::cli_abort("expression does not parse in expr_vars: {.code {r_expr}}",
                   class = "sas2r_expr_parse_error")
  })
  vars <- character()
  walk <- function(node) {
    if (is.name(node)) {
      v <- as.character(node)
      if (!v %in% c("c", "list", "is.na", "TRUE", "FALSE", "NA", "NULL", "%in%", "%notin%")) {
        vars <<- c(vars, v)
      }
      return()
    }
    if (!is.call(node)) return()
    for (i in seq_along(node)[-1]) walk(node[[i]])
  }
  walk(e)
  unique(tolower(vars))
}

