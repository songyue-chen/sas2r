#' Normalize dataset name
#'
#' Converts dataset names to lowercase and prepends `work.` if single-level.
#' @param x Character vector of dataset names.
#' @return Character vector of normalized dataset names.
#' @noRd
norm_ds <- function(x) {
  if (!length(x)) return(character(0))
  x <- tolower(x)
  ifelse(grepl("\\.", x), x, paste0("work.", x))
}

#' Extract libname assign and clear events
#'
#' A `LIBNAME` statement is an event in an execution, not a declaration: it
#' binds a libref to a location, and `libname adam clear;` unbinds it. Both are
#' returned, in source order, because a libref's meaning at a point of use
#' depends on which of them ran last -- dropping the clears would leave a
#' cleared libref looking permanently bound.
#'
#' `unit_id` is the translation unit the statement sits in, which anchors the
#' event to its site, and `unit_type` is what kind of unit that is. Both are
#' `NA` when `stmts` has not been through [sas_units()] yet. `unit_type`
#' matters downstream because a `LIBNAME` inside a `"macro_def"` unit only runs
#' if something calls that macro, so it is a conditional event rather than an
#' established binding -- see [libref_binding_at()].
#'
#' `path` is kept as a compatibility alias of `path_expression`; both hold the
#' literal text as written in the source, and both are `NA` for a clear, which
#' binds no path.
#'
#' @param stmts A tibble from [sas_statements()] or [sas_units()].
#' @return A tibble with columns libref, action, engine, path_expression, path,
#'   line, unit_id, unit_type.
#' @noRd
extract_librefs <- function(stmts) {
  empty <- tibble::tibble(
    libref = character(), action = character(), engine = character(),
    path_expression = character(), path = character(), line = integer(),
    unit_id = integer(), unit_type = character()
  )
  idx <- which(stmts$first_token == "libname")
  if (length(idx) == 0L) return(empty)
  txt_vec <- stmts$text[idx]
  line_vec <- stmts$line_start[idx]
  # `[[` rather than `$`: a tibble warns when `$` names a column it lacks.
  unit_col <- stmts[["unit_id"]]
  unit_vec <- if (is.null(unit_col)) {
    rep(NA_integer_, length(idx))
  } else {
    as.integer(unit_col)[idx]
  }
  type_col <- stmts[["unit_type"]]
  type_vec <- if (is.null(type_col)) {
    rep(NA_character_, length(idx))
  } else {
    as.character(type_col)[idx]
  }
  rows <- lapply(seq_along(idx), function(k) {
    m <- regmatches(txt_vec[k], regexec(
      "^libname\\s+([A-Za-z_]\\w*)\\s+(?:([A-Za-z_]\\w*)\\s+)?(['\"])(.*?)\\3",
      txt_vec[k], ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(m) >= 5L && m[1] != "") {
      return(list(libref = tolower(m[2]), action = "assign",
                  engine = tolower(m[3]), path_expression = m[5],
                  line = line_vec[k], unit_id = unit_vec[k],
                  unit_type = type_vec[k]))
    }
    cl <- regmatches(txt_vec[k], regexec(
      "^libname\\s+([A-Za-z_]\\w*)\\s+clear\\b", txt_vec[k],
      ignore.case = TRUE))[[1]]
    if (length(cl) >= 2L && cl[1] != "") {
      return(list(libref = tolower(cl[2]), action = "clear", engine = "",
                  path_expression = NA_character_, line = line_vec[k],
                  unit_id = unit_vec[k], unit_type = type_vec[k]))
    }
    NULL
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(empty)
  path_expression <- vapply(rows, `[[`, character(1), "path_expression")
  tibble::tibble(
    libref = vapply(rows, `[[`, character(1), "libref"),
    action = vapply(rows, `[[`, character(1), "action"),
    engine = vapply(rows, `[[`, character(1), "engine"),
    path_expression = path_expression,
    path = path_expression,
    line = vapply(rows, `[[`, integer(1), "line"),
    unit_id = vapply(rows, `[[`, integer(1), "unit_id"),
    unit_type = vapply(rows, `[[`, character(1), "unit_type")
  )
}

#' Extract %include statements
#'
#' `unit_id` is the translation unit the `%include` sits in, which anchors each
#' include occurrence to its site. It is `NA_integer_` when `stmts` has not been
#' through [sas_units()] yet.
#'
#' @param stmts A tibble from [sas_statements()] or [sas_units()].
#' @return A tibble with columns target, quoted, line, unit_id.
#' @noRd
extract_includes <- function(stmts) {
  idx <- which(stmts$first_token == "%include")
  if (length(idx) == 0L) {
    return(tibble::tibble(target = character(), quoted = logical(),
                          line = integer(), unit_id = integer()))
  }
  txt_vec <- stmts$text[idx]
  line_vec <- stmts$line_start[idx]
  # `[[` rather than `$`: a tibble warns when `$` names a column it lacks.
  unit_col <- stmts[["unit_id"]]
  unit_vec <- if (is.null(unit_col)) {
    rep(NA_integer_, length(idx))
  } else {
    as.integer(unit_col)[idx]
  }
  rows <- lapply(seq_along(idx), function(k) {
    m <- regmatches(txt_vec[k],
      regexec("^%(?:include|inc)\\s+(['\"])(.*?)\\1", txt_vec[k], ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(m) >= 3L && m[1] != "") {
      rest <- trimws(sub(m[1], "", txt_vec[k], fixed = TRUE))
      if (grepl("[\"']", rest)) list(target = trimws(sub("^%(?:include|inc)\\s+", "", txt_vec[k], ignore.case = TRUE, perl = TRUE)), quoted = FALSE, line = line_vec[k])
      else list(target = m[3], quoted = TRUE, line = line_vec[k])
    } else {
      rest <- trimws(sub("^%(?:include|inc)\\s+", "", txt_vec[k], ignore.case = TRUE, perl = TRUE))
      list(target = rest, quoted = FALSE, line = line_vec[k])
    }
  })
  tibble::tibble(
    target = vapply(rows, `[[`, character(1), "target"),
    quoted = vapply(rows, `[[`, logical(1), "quoted"),
    line = vapply(rows, `[[`, integer(1), "line"),
    unit_id = unit_vec
  )
}

# Read dataset positions once for both static lineage and unresolved-name
# findings. Quoted physical names stay intact; parenthesized dataset options
# are skipped without interpreting their expressions.
dataset_candidate_tokens <- function() c("data", "set", "merge", "update", "proc",
  "output", "table", "tables", "create", "select", "insert", "delete", "append")

dataset_tokens <- function(text) {
  lex <- regmatches(text, gregexpr(
    "\"(?:[^\"]|\"\")*\"[nN]?|'(?:[^']|'')*'[nN]?|[(),=;]|[^[:space:](),=;]+",
    text, perl = TRUE))[[1L]]
  # Strip balanced options, preserving quoted strings (which are single tokens).
  depth <- 0L
  keep <- logical(length(lex))
  for (i in seq_along(lex)) {
    if (lex[i] == "(") depth <- depth + 1L
    keep[i] <- depth == 0L && lex[i] != ")"
    if (lex[i] == ")") depth <- max(0L, depth - 1L)
  }
  flat <- lex[keep & lex != ";"]
  list(lex = lex, flat = flat)
}

dataset_option_values <- function(flat, key) {
  low <- tolower(flat)
  i <- which(low == key & c(utils::tail(low, -1L), "") == "=") + 2L
  flat[i[i <= length(flat)]]
}

dataset_statement_refs <- function(text, token, unit_type) {
  empty <- list(creates = character(), reads = character(), proc = "")
  if (!token %in% dataset_candidate_tokens() || unit_type == "macro_def") return(empty)
  tokens <- dataset_tokens(text)
  lex <- tokens$lex
  flat <- tokens$flat
  low <- tolower(flat)
  option <- function(key) dataset_option_values(flat, key)
  bare <- function() {
    values <- flat[-1L]
    equals <- match("=", values)
    if (!is.na(equals)) values <- if (equals > 2L) values[seq_len(equals - 2L)] else character()
    values[!values %in% c(",", "/")]
  }
  if (unit_type == "data_step") {
    if (token == "data") empty$creates <- setdiff(bare(), "_null_")
    if (token %in% c("set", "merge", "update")) empty$reads <- bare()
  } else if (unit_type == "proc_step") {
    if (token %in% c("proc", "append")) {
      empty$proc <- if (token == "append") "append" else if (length(low) >= 2L) low[2L] else ""
      if (empty$proc == "copy") return(empty) # IN/OUT name libraries, not datasets.
      empty$reads <- option("data")
      if (empty$proc == "compare") empty$reads <- c(empty$reads, option("base"), option("compare"))
      if (empty$proc == "append") empty$reads <- c(empty$reads, option("base"))
      empty$creates <- if (empty$proc == "append") option("base") else option("out")
    } else if (token %in% c("output", "table", "tables")) {
      empty$creates <- option("out")
    } else {
      # FROM/JOIN positions include comma-separated tables; SET assignment
      # values and FREQ/TABULATE variables are not dataset positions.
      from <- FALSE
      want <- FALSE
      after_dataset <- FALSE
      option_depth <- 0L
      for (i in seq_along(lex)) {
        word <- tolower(lex[i])
        if (option_depth > 0L || (after_dataset && word == "(")) {
          if (word == "(") option_depth <- option_depth + 1L
          if (word == ")") option_depth <- option_depth - 1L
          after_dataset <- FALSE
          next
        }
        after_dataset <- FALSE
        if (want && !word %in% c("(", ")", ",")) {
          if (!word %in% c("select", "table", "view")) {
            empty$reads <- c(empty$reads, lex[i])
            after_dataset <- TRUE
          }
          want <- FALSE
        }
        if (word %in% c("select", "where", "on", "group", "order", "having", "set", "union", ";")) from <- FALSE
        if (word %in% c("from", "join")) { want <- TRUE; from <- TRUE }
        if (word == "," && from) want <- TRUE
      }
      if (token == "create" && length(flat) >= 3L && low[2L] %in% c("table", "view")) empty$creates <- flat[3L]
      if (token == "update" && length(flat) >= 2L) empty$reads <- c(flat[2L], empty$reads)
      if (token == "insert" && length(flat) >= 3L && low[2L] == "into") empty$creates <- flat[3L]
    }
  }
  empty
}

static_dataset_names <- function(x) {
  x[grepl("^[A-Za-z_]\\w*(\\.[A-Za-z_]\\w*)?$", x) & tolower(x) != "_null_"]
}

# Compatibility for deterministic rules that already use these token helpers.
ds_tokens <- function(rest) {
  static_dataset_names(dataset_statement_refs(paste("set", rest), "set", "data_step")$reads)
}

eq_captures <- function(text, key) {
  static_dataset_names(dataset_option_values(dataset_tokens(text)$flat, key))
}

#' Extract dataset references and lineage
#'
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns unit_id, dataset, role, line.
#' @noRd
extract_dataset_refs <- function(units) {
  code <- units[units$type == "code", ]
  n <- nrow(code)
  if (n == 0L) {
    return(tibble::tibble(unit_id = integer(), dataset = character(),
                          role = character(), line = integer(),
                          proc = character()))
  }
  first_token_vec <- code$first_token
  unit_type_vec <- code$unit_type
  text_vec <- code$text
  unit_id_vec <- code$unit_id
  line_start_vec <- code$line_start

  cand_idx <- which(first_token_vec %in% dataset_candidate_tokens() & unit_type_vec != "macro_def")
  if (length(cand_idx) == 0L) {
    return(tibble::tibble(unit_id = integer(), dataset = character(),
                          role = character(), line = integer(),
                          proc = character()))
  }

  rows <- lapply(cand_idx, function(k) {
    tok <- first_token_vec[k]
    ut <- unit_type_vec[k]
    txt <- text_vec[k]
    refs <- dataset_statement_refs(txt, tok, ut)
    creates <- static_dataset_names(refs$creates)
    reads <- static_dataset_names(refs$reads)
    proc_name <- refs$proc
    if (!length(creates) && !length(reads)) return(NULL)
    total_len <- length(creates) + length(reads)
    list(
      unit_id = rep(unit_id_vec[k], total_len),
      dataset = norm_ds(c(creates, reads)),
      role = c(rep("creates", length(creates)), rep("reads", length(reads))),
      line = rep(line_start_vec[k], total_len),
      proc = rep(proc_name, total_len)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(tibble::tibble(unit_id = integer(), dataset = character(),
                          role = character(), line = integer(),
                          proc = character()))
  }
  out <- tibble::tibble(
    unit_id = unlist(lapply(rows, `[[`, "unit_id"), use.names = FALSE),
    dataset = unlist(lapply(rows, `[[`, "dataset"), use.names = FALSE),
    role = unlist(lapply(rows, `[[`, "role"), use.names = FALSE),
    line = unlist(lapply(rows, `[[`, "line"), use.names = FALSE),
    proc = unlist(lapply(rows, `[[`, "proc"), use.names = FALSE)
  )
  unique(out)
}

#' Check if a statement contains a bare option keyword
#'
#' Strips `key=value` patterns to prevent false matches against library/dataset names.
#' @param stmt A character string containing the SAS statement.
#' @param opt Option keyword to test (case-insensitive).
#' @return Logical scalar.
#' @noRd
has_bare_option <- function(stmt, opt) {
  if (is.null(stmt) || !length(stmt) || is.na(stmt[1])) return(FALSE)
  clean <- gsub("[A-Za-z_]\\w*\\s*=\\s*\\S+", "", stmt, ignore.case = TRUE)
  grepl(paste0("\\b", opt, "\\b"), clean, ignore.case = TRUE)
}

#' Extract filename statements
#'
#' @param stmts A tibble from [sas_statements()].
#' @return A list with fileref -> path mapping.
#' @noRd
extract_filerefs <- function(stmts) {
  fr <- stmts[tolower(stmts$first_token) == "filename", ]
  filerefs <- list()
  if (nrow(fr) > 0L) {
    for (k in seq_len(nrow(fr))) {
      m <- regmatches(fr$text[k], regexec(
        "^filename\\s+([A-Za-z_]\\w*)\\s+(['\"])(.*?)\\2",
        fr$text[k], ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(m) >= 4L && m[1] != "") filerefs[[tolower(m[2])]] <- m[4]
    }
  }
  filerefs
}

#' Extract sasautos paths from options statements
#'
#' @param stmts A tibble from [sas_statements()].
#' @return Character vector of harvested sasautos paths.
#' @noRd
extract_sasautos_options <- function(stmts) {
  opt <- stmts$text[tolower(stmts$first_token) == "options"]
  paths <- character()
  for (o in opt) {
    m <- regmatches(o, regexec("sasautos\\s*=\\s*(?:\\(([^)]*)\\)|'([^']*)'|\"([^\"]*)\")", o,
                               ignore.case = TRUE))[[1]]
    if (length(m) >= 2L) {
      if (nzchar(m[2])) {
        p_raw <- regmatches(m[2], gregexpr("'[^']*'|\"[^\"]*\"", m[2]))[[1]]
        paths <- c(paths, gsub("^['\"]|['\"]$", "", p_raw))
      } else if (length(m) >= 3L && nzchar(m[3])) {
        paths <- c(paths, m[3])
      } else if (length(m) >= 4L && nzchar(m[4])) {
        paths <- c(paths, m[4])
      }
    }
  }
  unique(paths)
}

#' Extract format definitions
#'
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns name, type, line, unit_id, file.
#' @noRd
extract_format_defs <- function(units) {
  empty <- tibble::tibble(
    name = character(), type = character(), line = integer(),
    unit_id = integer(), file = character()
  )
  if (is.null(units) || nrow(units) == 0L) return(empty)

  has_file <- "file" %in% names(units)
  file_col <- if (has_file) units$file else character(nrow(units))
  has_unit_id <- "unit_id" %in% names(units)
  unit_id_col <- if (has_unit_id) units$unit_id else integer(nrow(units))

  rows <- list()
  for (i in seq_len(nrow(units))) {
    txt <- units$text[i]
    tok <- tolower(units$first_token[i])
    u_type <- tolower(units$unit_type[i])
    if (u_type != "proc_step") next

    # Check for value / invalue / picture statements
    hits <- regmatches(txt, gregexpr("\\b(value|invalue|picture)\\s+([A-Za-z_$]\\w*)", txt, ignore.case = TRUE))[[1]]
    if (length(hits) > 0L) {
      for (h in hits) {
        parts <- strsplit(trimws(h), "\\s+")[[1]]
        if (length(parts) >= 2L) {
          f_type <- tolower(parts[1])
          f_name <- tolower(sub("^\\$", "", parts[2]))
          src_f <- if (has_file) as.character(file_col[i]) else ""
          u_id <- if (has_unit_id) as.integer(unit_id_col[i]) else NA_integer_
          rows[[length(rows) + 1L]] <- list(
            name = f_name,
            type = f_type,
            line = as.integer(units$line_start[i]),
            unit_id = u_id,
            file = src_f
          )
        }
      }
    }
  }
  if (length(rows) == 0L) return(empty)
  tibble::tibble(
    name = vapply(rows, `[[`, character(1), "name"),
    type = vapply(rows, `[[`, character(1), "type"),
    line = vapply(rows, `[[`, integer(1), "line"),
    unit_id = vapply(rows, `[[`, integer(1), "unit_id"),
    file = vapply(rows, `[[`, character(1), "file")
  )
}

#' Extract format uses
#'
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns name, line, unit_id, file.
#' @noRd
extract_format_uses <- function(units) {
  empty <- tibble::tibble(
    name = character(), line = integer(), unit_id = integer(), file = character()
  )
  if (is.null(units) || nrow(units) == 0L) return(empty)

  has_file <- "file" %in% names(units)
  file_col <- if (has_file) units$file else character(nrow(units))
  has_unit_id <- "unit_id" %in% names(units)
  unit_id_col <- if (has_unit_id) units$unit_id else integer(nrow(units))

  rows <- list()
  for (i in seq_len(nrow(units))) {
    if (tolower(units$type[i]) != "code") next
    txt <- units$text[i]
    tok <- tolower(units$first_token[i])
    src_f <- if (has_file) as.character(file_col[i]) else ""
    u_id <- if (has_unit_id) as.integer(unit_id_col[i]) else NA_integer_
    line_val <- as.integer(units$line_start[i])

    # 1. format var fmt.; or format var1 var2 $fmt.;
    if (tok == "format") {
      rest <- sub("^format\\s+", "", txt, ignore.case = TRUE)
      fmt_tokens <- regmatches(rest, gregexpr("([A-Za-z_$]\\w*)\\.", rest))[[1]]
      for (ft in fmt_tokens) {
        clean_fmt <- tolower(sub("\\.$", "", sub("^\\$", "", ft)))
        if (nzchar(clean_fmt)) {
          rows[[length(rows) + 1L]] <- list(
            name = clean_fmt, line = line_val, unit_id = u_id, file = src_f
          )
        }
      }
    }

    # 2. put(var, fmt.) or input(var, fmt.) in expressions
    call_hits <- regmatches(txt, gregexpr("\\b(?:put|input)\\s*\\([^,]+,\\s*([A-Za-z_$]\\w*)\\.", txt, ignore.case = TRUE))[[1]]
    if (length(call_hits) > 0L) {
      for (ch in call_hits) {
        m <- regmatches(ch, regexec("\\b(?:put|input)\\s*\\([^,]+,\\s*([A-Za-z_$]\\w*)\\.", ch, ignore.case = TRUE))[[1]]
        if (length(m) >= 2L && nzchar(m[2])) {
          clean_fmt <- tolower(sub("^\\$", "", m[2]))
          rows[[length(rows) + 1L]] <- list(
            name = clean_fmt, line = line_val, unit_id = u_id, file = src_f
          )
        }
      }
    }
  }

  if (length(rows) == 0L) return(empty)
  tibble::tibble(
    name = vapply(rows, `[[`, character(1), "name"),
    line = vapply(rows, `[[`, integer(1), "line"),
    unit_id = vapply(rows, `[[`, integer(1), "unit_id"),
    file = vapply(rows, `[[`, character(1), "file")
  )
}

#' Extract function definitions
#'
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns name, line, unit_id, file.
#' @noRd
extract_function_defs <- function(units) {
  empty <- tibble::tibble(
    name = character(), line = integer(), unit_id = integer(), file = character()
  )
  if (is.null(units) || nrow(units) == 0L) return(empty)

  has_file <- "file" %in% names(units)
  file_col <- if (has_file) units$file else character(nrow(units))
  has_unit_id <- "unit_id" %in% names(units)
  unit_id_col <- if (has_unit_id) units$unit_id else integer(nrow(units))

  rows <- list()
  for (i in seq_len(nrow(units))) {
    txt <- units$text[i]
    m <- regmatches(txt, gregexpr("\\bfunction\\s+([A-Za-z_]\\w*)\\s*\\(", txt, ignore.case = TRUE))[[1]]
    if (length(m) > 0L) {
      for (fn in m) {
        fn_name <- tolower(sub("^function\\s+", "", sub("\\s*\\($", "", fn, ignore.case = TRUE), ignore.case = TRUE))
        src_f <- if (has_file) as.character(file_col[i]) else ""
        u_id <- if (has_unit_id) as.integer(unit_id_col[i]) else NA_integer_
        rows[[length(rows) + 1L]] <- list(
          name = fn_name, line = as.integer(units$line_start[i]),
          unit_id = u_id, file = src_f
        )
      }
    }
  }
  if (length(rows) == 0L) return(empty)
  tibble::tibble(
    name = vapply(rows, `[[`, character(1), "name"),
    line = vapply(rows, `[[`, integer(1), "line"),
    unit_id = vapply(rows, `[[`, integer(1), "unit_id"),
    file = vapply(rows, `[[`, character(1), "file")
  )
}

#' Extract function uses
#'
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns name, line, unit_id, file.
#' @noRd
extract_function_uses <- function(units) {
  tibble::tibble(
    name = character(), line = integer(), unit_id = integer(), file = character()
  )
}


# Macro expansion is outside static lineage analysis. Preserve uncertainty at
# dataset positions instead of silently dropping those references. Dataset
# options/expressions are excluded so `where=(x=&limit)` is not a dynamic name.
dynamic_dataset_findings <- function(statements) {
  candidates <- which(statements$type == "code" & statements$unit_type != "macro_def" &
    statements$first_token %in% dataset_candidate_tokens() & grepl("[&%]", statements$text))
  selected <- candidates[vapply(candidates, function(i) {
    refs <- dataset_statement_refs(statements$text[i], statements$first_token[i], statements$unit_type[i])
    any(grepl("[&%]", c(refs$creates, refs$reads)))
  }, logical(1))]
  tibble::tibble(kind = rep("dynamic_dataset_reference", length(selected)),
    detail = paste0(statements$file[selected], ":", statements$line_start[selected],
                    ": ", statements$text[selected]))
}

# These forms need control-flow or macro expansion, beyond the static dataset
# grammar. Keep the uncertainty visible instead of inventing an input/target.
deferred_dataset_findings <- function(statements, defs, resolution) {
  active <- statements$type == "code" & statements$unit_type != "macro_def"
  text <- statements$text
  masked <- text
  candidates <- which(active & (statements$first_token %in% c("if", "else", "proc") |
    grepl("^[A-Za-z_]\\w*:", text)))
  masked[candidates] <- vapply(text[candidates], mask_strings, character(1))
  conditional <- active & statements$unit_type == "data_step" &
    grepl("(?:\\bthen\\s+|^else\\s+|^[A-Za-z_]\\w*:\\s*)(set|merge|update)\\b", masked,
      ignore.case = TRUE, perl = TRUE)
  copy <- active & grepl("^proc\\s+(copy|datasets)\\b", masked, ignore.case = TRUE)
  literal <- active & statements$first_token %in% dataset_candidate_tokens() &
    grepl("['\"][nN]\\b", text, perl = TRUE)
  rows <- which(conditional | copy | literal)
  deferred <- tibble::tibble(kind = rep("dataset_statement_deferred", length(rows)),
    detail = paste0(statements$file[rows], ":", statements$line_start[rows], ": ", text[rows]))
  top_calls <- extract_macro_calls(statements[active, ])
  invoked <- defs$unit_id[defs$name %in% top_calls$name]
  macro_rows <- which(statements$unit_id %in% invoked &
    (statements$first_token %in% dataset_candidate_tokens() |
      (grepl("%", text, fixed = TRUE) & !statements$first_token %in% c("%macro", "%mend"))))
  macro_rows <- macro_rows[!duplicated(statements$unit_id[macro_rows])]
  rbind(deferred, tibble::tibble(kind = rep("macro_data_flow_deferred", length(macro_rows)),
    detail = paste0(statements$file[macro_rows], ":", statements$line_start[macro_rows],
      ": invoked macro data flow requires expansion and review")))
}
