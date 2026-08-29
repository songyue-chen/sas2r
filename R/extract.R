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
      txt_vec[k], ignore.case = TRUE))[[1]]
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
      regexec("^%include\\s+(['\"])(.*?)\\1", txt_vec[k], ignore.case = TRUE))[[1]]
    if (length(m) >= 3L && m[1] != "") {
      list(target = m[3], quoted = TRUE, line = line_vec[k])
    } else {
      rest <- trimws(sub("^%include\\s+", "", txt_vec[k], ignore.case = TRUE))
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

#' Tokenize dataset names from statement body
#'
#' Iteratively strips nested parentheses to avoid leaking options tokens.
#' @param rest Statement string after initial keyword.
#' @return Character vector of dataset token names.
#' @noRd
ds_tokens <- function(rest) {
  while (grepl("\\([^()]*\\)", rest)) {
    rest <- gsub("\\([^()]*\\)", " ", rest)
  }
  toks <- strsplit(trimws(rest), "\\s+")[[1]]
  toks[grepl("^[A-Za-z_]\\w*(\\.[A-Za-z_]\\w*)?$", toks) & !grepl("=", toks)]
}

#' Extract key=value option targets
#'
#' @param text Statement text.
#' @param key Parameter keyword (e.g. "data", "out").
#' @return Character vector of dataset names.
#' @noRd
eq_captures <- function(text, key) {
  hits <- regmatches(text, gregexpr(
    paste0("\\b", key, "\\s*=\\s*([A-Za-z_]\\w*(\\.[A-Za-z_]\\w*)?)"),
    text, ignore.case = TRUE))[[1]]
  if (!length(hits)) return(character(0))
  sub("^.*?=\\s*", "", hits)
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

  cand_idx <- which(first_token_vec %in% c("data", "set", "merge", "update", "proc", "output", "table", "tables", "create", "select", "insert", "delete"))
  if (length(cand_idx) == 0L) {
    return(tibble::tibble(unit_id = integer(), dataset = character(),
                          role = character(), line = integer(),
                          proc = character()))
  }

  rows <- lapply(cand_idx, function(k) {
    tok <- first_token_vec[k]
    ut <- unit_type_vec[k]
    txt <- text_vec[k]
    creates <- character(); reads <- character()
    proc_name <- ""
    if (ut == "data_step" && tok == "data") {
      t <- ds_tokens(sub("^data(\\s+|$)", "", txt, ignore.case = TRUE))
      creates <- t[tolower(t) != "_null_"]
    } else if (ut == "data_step" && tok %in% c("set", "merge", "update")) {
      reads <- ds_tokens(sub("^(set|merge|update)(\\s+|$)", "", txt, ignore.case = TRUE))
    } else if (ut == "proc_step" && tok == "proc") {
      m_p <- regmatches(txt, regexec("^proc\\s+([A-Za-z_]\\w*)", txt, ignore.case = TRUE))[[1]]
      if (length(m_p) >= 2L && m_p[1] != "") proc_name <- tolower(m_p[2])
      reads <- eq_captures(txt, "data")
      creates <- eq_captures(txt, "out")
    } else if (ut == "proc_step" && tok %in% c("output", "table", "tables")) {
      creates <- eq_captures(txt, "out")
    } else if (ut == "proc_step" && tok %in% c("create", "select", "insert", "update", "delete")) {
      if (tok == "create") {
        m <- regmatches(txt, regexec(
          "\\bcreate\\s+(?:table|view)\\s+([A-Za-z_]\\w*(\\.[A-Za-z_]\\w*)?)",
          txt, ignore.case = TRUE))[[1]]
        if (length(m) >= 2L && m[1] != "") creates <- m[2]
      }
      fr <- regmatches(txt, gregexpr(
        "\\b(?:from|join)\\s+([A-Za-z_]\\w*(?:\\.[A-Za-z_]\\w*)?)",
        txt, ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(fr) && fr[1] != "") {
        reads <- sub("^\\S+\\s+", "", fr)
      }
    }
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
        fr$text[k], ignore.case = TRUE))[[1]]
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

