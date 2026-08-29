#' Emit R code for a PROC SORT step
#'
#' Translates a SAS `proc sort` step into a `sas_sort()` call with library I/O,
#' optional descending sort variables, and optional deduplication via `dplyr::distinct()`.
#'
#' @param stmts A tibble of statements corresponding to a single `proc_step` unit.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_proc_sort <- function(stmts) {
  code_rows <- stmts[stmts$type == "code", ]
  proc <- code_rows$text[code_rows$first_token == "proc"][1]
  data_in <- eq_captures(proc, "data")
  out <- eq_captures(proc, "out")
  has_nodupkey <- grepl("\\bnodupkey\\b", proc, ignore.case = TRUE)
  has_nodup <- grepl("\\bnodup(recs)?\\b", proc, ignore.case = TRUE) && !has_nodupkey
  by_txt <- code_rows$text[code_rows$first_token == "by"][1]

  if (!length(data_in) || !nzchar(data_in[1])) {
    return(list(code = NA_character_,
                stmt_map = as.integer(code_rows$stmt_id),
                flags = "sort_missing_data"))
  }

  # eq_captures() reads data=x and ignores a trailing (where=/keep=/rename=)
  # group, so options would be dropped without a trace. Refuse until modeled.
  if (grepl("\\(", proc)) {
    return(list(code = NA_character_,
                stmt_map = as.integer(code_rows$stmt_id),
                flags = "sort_dataset_options"))
  }

  if (is.na(by_txt) || !nzchar(trimws(by_txt))) {
    return(list(code = NA_character_,
                stmt_map = as.integer(code_rows$stmt_id),
                flags = "sort_missing_by"))
  }

  toks <- strsplit(trimws(sub("^by\\s+", "", by_txt, ignore.case = TRUE)), "\\s+")[[1]]
  desc_next <- tolower(toks) == "descending"
  by <- tolower(toks[!desc_next])

  if (length(by) == 0L) {
    return(list(code = NA_character_,
                stmt_map = as.integer(code_rows$stmt_id),
                flags = "sort_missing_by"))
  }

  desc_indices <- which(desc_next) + 1L
  desc_indices <- desc_indices[desc_indices <= length(toks)]
  descending <- tolower(toks[desc_indices])
  descending <- descending[!is.na(descending) & nzchar(descending)]

  src <- split_ds(norm_ds(data_in[1]))
  target <- if (length(out) && nzchar(out[1])) split_ds(norm_ds(out[1])) else src

  code <- sprintf('%s <- sas_sort(sas2r_fold_names(lib_read("%s", "%s")), by = c(%s)%s)',
                  target[["member"]], src[["lib"]], src[["member"]],
                  paste(sprintf('"%s"', by), collapse = ", "),
                  if (length(descending))
                    sprintf(', descending = c(%s)',
                            paste(sprintf('"%s"', descending), collapse = ", "))
                  else "")


  if (has_nodupkey) {
    code <- paste0(code,
      sprintf(' |>\n  dplyr::distinct(dplyr::pick(dplyr::all_of(c(%s))), .keep_all = TRUE)',
              paste(sprintf('"%s"', by), collapse = ", ")))
  } else if (has_nodup) {
    code <- paste0(code, ' |>\n  dplyr::distinct(.keep_all = TRUE)')
  }

  writes <- emit_lib_writes(target[["member"]], paste0(target[["lib"]], ".", target[["member"]]))
  code <- paste0(code, "\n", paste(writes, collapse = "\n"))


  list(code = code, stmt_map = as.integer(code_rows$stmt_id), flags = character())
}

#' Compile format catalog from a SAS project
#'
#' Scans all `proc format` units in a project and extracts discrete mappings,
#' numeric ranges, and `other=` defaults. Flags unsupported features like `picture`,
#' `invalue`, and `multilabel`.
#'
#' @param project A `sas2r_project` object.
#' @return A list with elements `catalog` (named list of format definitions) and `flags` (tibble).
#' @noRd
compile_format_catalog <- function(project) {
  st <- project$statements
  fmt_units <- unique(st$unit_id[st$first_token == "proc" &
    grepl("^proc\\s+format\\b", st$text, ignore.case = TRUE)])

  catalog <- list()
  flags <- list()

  for (uid in fmt_units) {
    us <- st[st$unit_id == uid & st$type == "code", ]
    for (k in seq_len(nrow(us))) {
      tok <- us$first_token[k]
      txt <- us$text[k]

      if (tok %in% c("picture", "invalue")) {
        nm <- tolower(sub(paste0("^", tok, "\\s+(\\$?\\w+).*$"), "\\1", txt,
                          ignore.case = TRUE))
        flags[[length(flags) + 1L]] <- tibble::tibble(
          unit_id = uid, reason = paste0("format_unsupported:", nm))
      } else if (tok == "value") {
        nm <- tolower(regmatches(txt, regexec("^value\\s+(\\$?[A-Za-z_]\\w*)",
                                              txt, ignore.case = TRUE))[[1]][2])
        if (grepl("\\bmultilabel\\b", txt, ignore.case = TRUE)) {
          flags[[length(flags) + 1L]] <- tibble::tibble(
            unit_id = uid, reason = paste0("format_unsupported:", nm))
        }

        body <- sub("^value\\s+\\$?[A-Za-z_]\\w*\\s*(\\([^)]*\\))?\\s*", "", txt, ignore.case = TRUE)
        pattern <- "((?:'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\"|[^=])+?)\\s*=\\s*(?:'((?:[^']|'')*)'|\"((?:[^\"]|\"\")*)\"|([^\\s;]+))"
        m <- gregexec(pattern, body, perl = TRUE)
        matches <- regmatches(body, m)[[1]]

        values <- character()
        ranges <- list()
        other <- NULL

        if (length(matches) > 0L && ncol(matches) > 0L) {
          for (i in seq_len(ncol(matches))) {
            raw_lhs <- trimws(matches[2, i])
            lab <- if (nzchar(matches[3, i])) gsub("''", "'", matches[3, i])
                   else if (nzchar(matches[4, i])) gsub('""', '"', matches[4, i])
                   else matches[5, i]

            if (tolower(raw_lhs) == "other") {
              other <- lab
            } else if (!grepl("^['\"]", raw_lhs) && grepl("(?:<-?|-<?)", raw_lhs)) {
              parts <- strsplit(raw_lhs, "\\s*(?:<-?|-<?)\\s*")[[1]]
              if (length(parts) == 2L) {
                p1 <- tolower(trimws(parts[1]))
                p2 <- tolower(trimws(parts[2]))
                lo <- if (p1 == "low") -Inf else suppressWarnings(as.numeric(p1))
                hi <- if (p2 == "high") Inf else suppressWarnings(as.numeric(p2))
                if (!is.na(lo) && !is.na(hi)) {
                  ranges[[length(ranges) + 1L]] <- list(lo = lo, hi = hi, label = lab)
                  next
                }
              }
              items <- trimws(strsplit(raw_lhs, "\\s*,\\s*")[[1]])
              for (item in items) {
                val <- gsub("^['\"]|['\"]$", "", item)
                values[val] <- lab
              }
            } else {
              items <- trimws(strsplit(raw_lhs, "\\s*,\\s*")[[1]])
              for (item in items) {
                val <- gsub("^['\"]|['\"]$", "", item)
                values[val] <- lab
              }
            }
          }
        }

        catalog[[nm]] <- list(
          values = if (length(values)) values else c(),
          ranges = if (length(ranges)) ranges else NULL,
          other = other
        )
      }
    }
  }

  list(
    catalog = catalog,
    flags = if (length(flags)) do.call(rbind, flags) else
      tibble::tibble(unit_id = integer(), reason = character())
  )
}

#' Write compiled format catalog to file
#'
#' Emits `_sas2r_formats.R` containing `.sas2r_formats`.
#'
#' @param catalog Named list of format definitions from `compile_format_catalog()`.
#' @param out_dir Output directory path.
#' @return Invisible destination path.
#' @noRd
write_formats <- function(catalog, out_dir) {
  path <- file.path(out_dir, "_sas2r_formats.R")
  writeLines(c("# Generated by sas2r -- format catalog.",
               paste0(".sas2r_formats <- ",
                      paste(deparse(catalog, control = "all"), collapse = "\n"))),
             path)
  invisible(path)
}

#' Emit placeholder code for a PROC FORMAT unit
#'
#' Formats are compiled at the project level into `_sas2r_formats.R`.
#'
#' @param us A tibble of statements corresponding to a single `proc_step` unit.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_proc_format <- function(us) {
  list(code = "# formats compiled into _sas2r_formats.R",
       stmt_map = us$stmt_id, flags = character())
}

#' Emit placeholder/flag for display-only procs
#'
#' @param us A tibble of statements corresponding to a single `proc_step` unit.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_proc_note <- function(us) {
  list(code = NA_character_, stmt_map = us$stmt_id, flags = "display_only")
}
