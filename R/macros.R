#' Built-in SAS macro language keywords
#'
#' Names that should never be identified as user macro calls.
#' @noRd
MACRO_BUILTINS <- c(
  "let", "if", "then", "else", "do", "end", "to", "by", "while", "until",
  "macro", "mend", "put", "include", "global", "local", "eval", "sysevalf",
  "sysfunc", "str", "nrstr", "scan", "substr", "upcase", "lowcase", "index",
  "length", "quote", "nrquote", "bquote", "nrbquote", "superq", "unquote",
  "sysget", "abort", "goto", "return", "symdel", "sysrput", "syslput", "window",
  "display", "sysexec", "symexist", "symglobl", "symlocal", "qscan",
  "qsubstr", "qupcase", "qlowcase", "qtrim", "qleft", "qcmpres", "trim",
  "left", "cmpres", "verify", "datatyp", "input", "sysprod", "sysmacdelete"
)

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
      "^%macro\\s+([A-Za-z_]\\w*)\\s*(?:\\((.*)\\))?", txt_vec[k],
      ignore.case = TRUE))[[1]]
    if (length(m) < 2L || m[1] == "") return(NULL)
    u_idx <- which(units$unit_id == uid_vec[k])
    param_str <- if (length(m) >= 3L && !is.na(m[3])) trimws(m[3]) else ""
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
extract_macro_calls <- function(units) {
  empty <- empty_macro_calls()
  if (is.null(units) || nrow(units) == 0L) {
    return(empty)
  }

  idx <- which(units$type == "code" & units$first_token != "%macro" & grepl("%", units$text, fixed = TRUE))
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
    clean_txt <- mask_strings(raw_txt, keep_double = TRUE)
    matches <- gregexpr("%([A-Za-z_&]\\w*)", clean_txt)[[1]]
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

      # Extract raw call text
      sub_txt <- substring(raw_txt, pos)
      m_call <- regmatches(sub_txt, regexec("^%[A-Za-z_&]\\w*(?:\\s*\\([^;]*\\))?", sub_txt))[[1]]
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
      cache_dir <- file.path(project_dir, ".sas2r")
      macro_idx <<- build_macro_index(resolved_dirs, cache_dir = cache_dir)
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
