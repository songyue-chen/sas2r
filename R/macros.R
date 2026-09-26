#' Percent-prefixed statements, distinct from user macro invocations
#' @noRd
NON_CALL_MACRO_KEYWORDS <- c(
  "let", "if", "then", "else", "do", "end", "macro", "mend",
  "global", "local", "goto", "go", "return", "abort",
  "put", "input", "display", "window", "sysexec", "cms", "tso",
  "symdel", "sysrput", "syslput", "sysmacdelete", "sysmstoreclear", "copy", "syscall",
  # SAS statements outside the macro facility.
  "include", "inc", "list", "run"
)

#' Built-in macro functions and statement/control keywords
#'
#' Names that should never be identified as user macro calls.
#' @noRd
MACRO_BUILTINS <- c(
  NON_CALL_MACRO_KEYWORDS, "to", "by", "while", "until", "eval", "sysevalf",
  "sysfunc", "qsysfunc", "str", "nrstr", "scan", "substr", "upcase", "lowcase", "index",
  "length", "quote", "nrquote", "bquote", "nrbquote", "superq", "unquote",
  "sysget", "symexist", "symglobl", "symlocal", "qscan",
  "qsubstr", "qupcase", "qlowcase", "qtrim", "qleft", "qcmpres", "trim",
  "left", "cmpres", "verify", "datatyp", "input", "sysprod", "sysmacdelete",
  "sysmacexec", "sysmacexist", "sysmexecdepth", "sysmexecname",
  # SAS NLS macro functions and supplied autocall macros. Do not infer Q
  # variants by prefix: e.g. QKLENGTH is not a documented macro function.
  "kcmpres", "kindex", "kleft", "klength", "klowcase", "kscan", "ksubstr",
  "ktrim", "kupcase", "kverify", "qkleft", "qklowcas", "qkscan", "qksubstr",
  "qktrim", "qkupcase",
  "qkcmpres" # Reserved by the SAS macro facility, even where not implemented.
)

# Lexical masking only: do not evaluate macro variables or execute a macro
# processor. Work per file so quoted semicolons split by the statement scanner
# do not turn subsequent literal text into apparent calls.
macro_call_scan <- function(units) {
  visible_text <- function(text) {
    sc <- sas_scan(text)
    keep <- sc$mask %in% c("c", "q") | (sc$mask == "s" & sc$chars == "'")
    sc$chars[!keep] <- " "
    paste(sc$chars, collapse = "")
  }
  text <- units$text
  findings <- tibble::tibble(kind = character(), detail = character())
  if (!any(grepl("%", text, fixed = TRUE))) return(list(text = text, findings = findings))
  files <- if ("file" %in% names(units)) units$file else rep("", nrow(units))
  for (rows in split(seq_len(nrow(units)), files)) {
    parts <- paste0(text[rows], ";\n")
    ends <- cumsum(nchar(parts))
    if (!any(grepl("%", text[rows], fixed = TRUE))) next
    starts <- c(1L, utils::head(ends, -1L) + 1L)
    raw <- paste(parts, collapse = "")
    chars <- strsplit(raw, "", fixed = TRUE)[[1L]]
    clean <- chars
    deferred <- integer()
    unverified <- integer()
    # These compilation-time quoting spans are literal for dependency lookup.
    # Runtime unquoting can reactivate their contents and is deferred below.
    quotes <- gregexpr("%(?:nrstr|str|unquote)\\s*\\(", visible_text(raw),
                       ignore.case = TRUE, perl = TRUE)[[1L]]
    lengths <- attr(quotes, "match.length")
    for (k in which(quotes > 0L)) {
      pos <- quotes[k]
      if (clean[pos] != "%") next
      name <- tolower(sub("^%([a-z]+).*", "\\1", substr(raw, pos, pos + lengths[k] - 1L),
                          ignore.case = TRUE))
      open <- pos + lengths[k] - 1L
      depth <- 1L
      close <- NA_integer_
      quote <- ""
      j <- open + 1L
      while (j <= length(chars)) {
        if (chars[j] == "%" && j < length(chars) && chars[j + 1L] %in% c("(", ")", "'", '"', "%")) {
          if (name == "str") clean[c(j, j + 1L)] <- " "
          j <- j + 2L
          next
        }
        if (chars[j] %in% c("'", '"')) {
          if (quote == "") quote <- chars[j]
          else if (quote == chars[j]) quote <- ""
        }
        if (quote == "" && chars[j] == "(") depth <- depth + 1L
        if (quote == "" && chars[j] == ")") depth <- depth - 1L
        if (depth == 0L) { close <- j; break }
        j <- j + 1L
      }
      if (is.na(close)) {
        deferred <- c(deferred, pos)
        break
      }
      body <- substr(raw, open + 1L, close - 1L)
      if (name == "nrstr") clean[seq.int(pos, close)] <- " "
      if (name == "unquote" && grepl("&", body) && !grepl("%", body)) unverified <- c(unverified, pos)
      if (name == "unquote" && grepl("%", body)) deferred <- c(deferred, pos)
    }
    clean <- visible_text(paste(clean, collapse = ""))
    for (pos in unique(deferred)) {
      owner <- which(ends >= pos)[1L]
      findings <- rbind(findings, tibble::tibble(
        kind = "macro_dependency_analysis_deferred",
        detail = sprintf("%s:%s: macro quoting or unquoting requires expansion before dependencies can be determined",
                         files[rows[owner]], units$line_start[rows[owner]])))
    }
    for (pos in unique(unverified)) {
      owner <- which(ends >= pos)[1L]
      findings <- rbind(findings, tibble::tibble(
        kind = "macro_expansion_unverified",
        detail = sprintf("%s:%s: runtime unquoting of a macro variable is not statically expanded; review the generated text",
                         files[rows[owner]], units$line_start[rows[owner]])))
    }
    if (length(deferred)) {
      # Retain known calls before the incomplete portion. Later interpretation
      # is unknown, not a set of missing files, until that portion is expanded.
      substr(clean, min(deferred), nchar(clean)) <- paste(rep(" ", nchar(clean) - min(deferred) + 1L), collapse = "")
    }
    text[rows] <- substring(clean, starts, starts + nchar(units$text[rows]) - 1L)
  }
  list(text = text, findings = findings)
}

#' Extract macro definitions
#'
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns name, params, line_start, line_end.
#' @noRd
extract_macro_defs <- function(units) {
  idx <- which(units$first_token == "%macro" & units$type == "code")
  if (length(idx) == 0L) {
    return(tibble::tibble(name = character(), params = character(),
                          line_start = integer(), line_end = integer(),
                          file = character(), unit_id = integer()))
  }
  txt_vec <- units$text[idx]
  uid_vec <- units$unit_id[idx]
  has_file <- "file" %in% names(units)
  file_col <- if (has_file) units$file else character(nrow(units))

  rows <- lapply(seq_along(idx), function(k) {
    m <- regmatches(txt_vec[k], regexec(
      "^%macro\\s+([A-Za-z_]\\w*)\\s*", txt_vec[k],
      ignore.case = TRUE))[[1]]
    if (length(m) < 2L || m[1] == "") return(NULL)
    u_idx <- which(units$unit_id == uid_vec[k])
    tail <- substring(txt_vec[k], nchar(m[1]) + 1L)
    param_str <- ""
    if (startsWith(tail, "(")) {
      # End at the matching parameter-list delimiter, before / options.
      # Quoted paths/descriptions and nested defaults can themselves contain
      # slashes and parentheses, so stripping slash suffixes is incorrect.
      sc <- sas_scan(tail)
      depth <- 0L
      close <- NA_integer_
      for (j in which(sc$mask == "c")) {
        if (sc$chars[j] == "(") depth <- depth + 1L
        if (sc$chars[j] == ")") {
          depth <- depth - 1L
          if (depth == 0L) { close <- j; break }
        }
      }
      if (is.na(close)) abort_macro_contract(paste0(
        "Unterminated parameter list for macro ", m[2], " at ",
        if (has_file) paste0(file_col[idx[k]], ":") else "line ",
        units$line_start[idx[k]], "; check parentheses and quotation marks."
      ))
      param_str <- trimws(substr(tail, 2L, close - 1L))
    }
    file_val <- if (has_file) file_col[u_idx[1]] else NA_character_
    list(name = tolower(m[2]),
         params = param_str,
         line_start = min(units$line_start[u_idx]),
         line_end = max(units$line_end[u_idx]),
         file = file_val,
         unit_id = as.integer(uid_vec[k]))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(tibble::tibble(name = character(), params = character(),
                          line_start = integer(), line_end = integer(),
                          file = character(), unit_id = integer()))
  }
  tibble::tibble(
    name = vapply(rows, `[[`, character(1), "name"),
    params = vapply(rows, `[[`, character(1), "params"),
    line_start = vapply(rows, `[[`, integer(1), "line_start"),
    line_end = vapply(rows, `[[`, integer(1), "line_end"),
    file = vapply(rows, `[[`, character(1), "file"),
    unit_id = vapply(rows, `[[`, integer(1), "unit_id")
  )
}

#' Empty prototype for macro calls
#' @noRd
empty_macro_calls <- function() {
  tibble::tibble(
    call_id = character(),
    component_id = character(),
    source_file = character(),
    line = integer(),
    column = integer(),
    name = character(),
    call_text = character()
  )
}

#' Extract macro calls
#'
#' Extracts macro invocations (excluding macro-language builtins) from code.
#' Single-quoted strings are stripped to avoid false positives.
#' @param units A tibble from [sas_units()].
#' @return A tibble with columns call_id, component_id, source_file, line, column, name, call_text.
#' @noRd
extract_macro_calls <- function(units, scan = NULL) {
  empty <- empty_macro_calls()
  if (is.null(units) || nrow(units) == 0L) {
    return(empty)
  }

  if (is.null(scan)) scan <- macro_call_scan(units)
  idx <- which(units$type == "code" & grepl("%", scan$text, fixed = TRUE))
  if (length(idx) == 0L) {
    return(empty)
  }

  txt_vec <- units$text[idx]
  line_vec <- units$line_start[idx]
  has_file <- "file" %in% names(units)
  file_col <- if (has_file) units$file else character(nrow(units))
  has_unit_id <- "unit_id" %in% names(units)
  unit_id_col <- if (has_unit_id) units$unit_id else integer(nrow(units))

  rows <- list()
  for (k in seq_along(idx)) {
    raw_txt <- txt_vec[k]
    clean_txt <- scan$text[idx[k]]
    token_pattern <- "%[A-Za-z_&]\\w*(?:&+\\w*\\.?\\w*)*"
    matches <- gregexpr(token_pattern, clean_txt, perl = TRUE)[[1]]
    if (length(matches) == 1L && matches[1] == -1L) next

    match_lengths <- attr(matches, "match.length")
    src_file <- if (has_file) as.character(file_col[idx[k]]) else ""
    if (is.na(src_file)) src_file <- ""
    comp_id <- if (nzchar(src_file)) tools::file_path_sans_ext(basename(src_file)) else ""
    line_val <- as.integer(line_vec[k])

    for (m_idx in seq_along(matches)) {
      pos <- matches[m_idx]
      len <- match_lengths[m_idx]
      matched_token <- substr(clean_txt, pos, pos + len - 1L)
      mac_name <- tolower(sub("^%", "", matched_token))

      if (mac_name %in% MACRO_BUILTINS) next
      # A colon introduces a macro label in code. In a double-quoted string,
      # %name: still invokes the macro and appends a colon to its result.
      if (grepl("^\\s*:", substring(clean_txt, pos + len)) &&
          sas_scan(clean_txt)$mask[pos] == "c") next

      # Extract raw call text
      sub_txt <- substring(raw_txt, pos)
      m_call <- regmatches(sub_txt, regexec(paste0("^", token_pattern, "(?:\\s*\\([^;]*\\))?"), sub_txt, perl = TRUE))[[1]]
      raw_call <- if (length(m_call) > 0L && nzchar(m_call[1])) m_call[1] else matched_token

      col_val <- as.integer(pos)
      call_key <- list(
        file = src_file,
        line = line_val,
        column = col_val,
        name = mac_name
      )
      c_id <- paste0("macro_call_", substr(migration_hash(call_key), 1L, 16L))

      rows[[length(rows) + 1L]] <- list(
        call_id = c_id,
        component_id = comp_id,
        source_file = src_file,
        line = line_val,
        column = col_val,
        name = mac_name,
        call_text = raw_call
      )
    }
  }

  if (length(rows) == 0L) return(empty)

  tibble::tibble(
    call_id = vapply(rows, `[[`, character(1), "call_id"),
    component_id = vapply(rows, `[[`, character(1), "component_id"),
    source_file = vapply(rows, `[[`, character(1), "source_file"),
    line = vapply(rows, `[[`, integer(1), "line"),
    column = vapply(rows, `[[`, integer(1), "column"),
    name = vapply(rows, `[[`, character(1), "name"),
    call_text = vapply(rows, `[[`, character(1), "call_text")
  )
}

#' Resolve macro calls against definitions and search path
#'
#' Resolves macro calls against project-defined macros and the search path.
#' Resolution proceeds across three rungs:
#' 1. Project definitions (`status = "resolved_project"`).
#' 2. Filename convention in search path (`status = "resolved_path"`).
#' 3. Content index lookup in search path (`status = "resolved_content"`).
#'
#' Preserves all call sites without collapsing duplicates by macro name.
#'
#' @param calls A tibble of macro calls.
#' @param defs A tibble of macro definitions.
#' @param config A sas2r_config object containing macro_search_path.
#' @param project_dir The project base directory for resolving relative paths.
#' @return A tibble with columns call_id, component_id, source_file, line, column, name, call_text, status, source, n_matches, shadowed.
#' @noRd
resolve_macro_calls <- function(calls, defs, config, project_dir = ".") {
  empty <- tibble::tibble(
    call_id = character(),
    component_id = character(),
    source_file = character(),
    line = integer(),
    column = integer(),
    name = character(),
    call_text = character(),
    status = character(),
    source = character(),
    n_matches = integer(),
    shadowed = character()
  )
  if (is.null(calls) || nrow(calls) == 0L) return(empty)

  resolved_dirs <- if (length(config$macro_search_path)) {
    vapply(config$macro_search_path, function(d) {
      if (is_anchored_path(d)) d else file.path(project_dir, d)
    }, character(1), USE.NAMES = FALSE)
  } else {
    character()
  }

  macro_idx <- NULL
  get_macro_idx <- function() {
    if (is.null(macro_idx)) {
      # Scanning must stay read-only until the effective QC plan is valid.
      # Agent-time macro tools may persist their index after setup succeeds.
      macro_idx <<- build_macro_index(resolved_dirs)
    }
    macro_idx
  }

  rows <- lapply(seq_len(nrow(calls)), function(i) {
    nm <- calls$name[i]
    call_id <- if ("call_id" %in% names(calls)) calls$call_id[i] else paste0("call_", i)
    comp_id <- if ("component_id" %in% names(calls)) calls$component_id[i] else ""
    src_file <- if ("source_file" %in% names(calls)) calls$source_file[i] else ""
    line_val <- if ("line" %in% names(calls)) calls$line[i] else NA_integer_
    col_val <- if ("column" %in% names(calls)) calls$column[i] else NA_integer_
    call_txt <- if ("call_text" %in% names(calls)) calls$call_text[i] else paste0("%", nm)

    if (grepl("&", nm)) {
      return(tibble::tibble(
        call_id = call_id, component_id = comp_id, source_file = src_file,
        line = line_val, column = col_val, name = nm, call_text = call_txt,
        status = "dynamic", source = "", n_matches = 0L, shadowed = ""
      ))
    }

    if (nm %in% defs$name) {
      def_row <- defs[defs$name == nm, ][1, ]
      def_src <- if ("file" %in% names(def_row)) def_row$file else ""
      return(tibble::tibble(
        call_id = call_id, component_id = comp_id, source_file = src_file,
        line = line_val, column = col_val, name = nm, call_text = call_txt,
        status = "resolved_project", source = def_src %||% "", n_matches = 1L, shadowed = ""
      ))
    }

    hits <- character()
    for (dd in resolved_dirs) {
      f <- file.path(dd, paste0(nm, ".sas"))
      if (file.exists(f)) {
        hits <- c(hits, f)
      } else if (dir.exists(dd)) {
        matched <- list.files(dd, pattern = paste0("^", nm, "\\.sas$"),
                              ignore.case = TRUE, full.names = TRUE)
        if (length(matched)) hits <- c(hits, matched[1])
      }
    }

    if (length(hits) > 0L) {
      return(tibble::tibble(
        call_id = call_id, component_id = comp_id, source_file = src_file,
        line = line_val, column = col_val, name = nm, call_text = call_txt,
        status = "resolved_path", source = hits[1],
        n_matches = length(hits),
        shadowed = paste(hits[-1], collapse = ",")
      ))
    }

    idx <- get_macro_idx()
    if (nrow(idx) > 0L) {
      content_hits <- idx$file[idx$name == nm]
      if (length(content_hits) > 0L) {
        return(tibble::tibble(
          call_id = call_id, component_id = comp_id, source_file = src_file,
          line = line_val, column = col_val, name = nm, call_text = call_txt,
          status = "resolved_content",
          source = content_hits[1],
          n_matches = length(content_hits),
          shadowed = paste(content_hits[-1], collapse = ",")
        ))
      }
    }

    tibble::tibble(
      call_id = call_id, component_id = comp_id, source_file = src_file,
      line = line_val, column = col_val, name = nm, call_text = call_txt,
      status = "unresolved",
      source = "", n_matches = 0L, shadowed = ""
    )
  })

  out <- do.call(rbind, rows)
  if (is.null(out)) empty else out
}
