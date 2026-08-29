#' Emit R code for a PROC FREQ unit
#'
#' Translates supported single-variable PROC FREQ units with NOPRINT
#' and TABLES ... / OUT= statements into a dplyr count and percent pipeline.
#'
#' @param us A tibble of statements corresponding to a single `proc_step` unit.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_proc_freq <- function(us) {
  code_rows <- us[us$type == "code", ]
  proc <- code_rows$text[code_rows$first_token == "proc"][1]
  tbl <- code_rows$text[code_rows$first_token == "tables"]
  reject <- function(flag) list(code = NA_character_,
                                stmt_map = as.integer(code_rows$stmt_id),
                                flags = flag)

  if (is.na(proc) || !grepl("\\bnoprint\\b", proc, ignore.case = TRUE) || length(tbl) != 1L)
    return(reject("freq_not_t1"))

  allowed_tokens <- c("proc", "tables", "run", "quit")
  if (!all(tolower(code_rows$first_token) %in% allowed_tokens))
    return(reject("freq_not_t1"))

  m <- regmatches(tbl[1], regexec(
    "^tables\\s+([^/]+?)\\s*/\\s*(.*)$", tbl[1], ignore.case = TRUE))[[1]]
  if (length(m) < 3L) return(reject("freq_not_t1"))
  if (grepl("\\*", m[2])) return(reject("freq_multiway_deferred"))

  raw_var <- trimws(m[2])
  if (!grepl("^[A-Za-z_]\\w*$", raw_var)) return(reject("freq_not_t1"))
  v <- tolower(raw_var)
  opts <- tolower(m[3])

  out_ds <- eq_captures(opts, "out")
  if (!length(out_ds) || !nzchar(out_ds[1])) return(reject("freq_not_t1"))

  data_in <- eq_captures(proc, "data")
  if (!length(data_in) || !nzchar(data_in[1])) return(reject("freq_not_t1"))

  inc_missing <- has_bare_option(opts, "missing")
  src <- split_ds(norm_ds(data_in[1]))
  target <- split_ds(norm_ds(out_ds[1]))
  lines <- c(sprintf('%s <- lib_read("%s", "%s")', target[["member"]], src[["lib"]], src[["member"]]),
             "  sas2r_fold_names()",
             if (!inc_missing)
               sprintf("  dplyr::filter(!is.na(%s) & (!is.character(%s) | trimws(%s) != \"\"))", v, v, v),
             sprintf("  dplyr::count(%s, name = \"count\")", v),
             "  dplyr::mutate(percent = 100 * count / sum(count))")
  code <- paste(lines, collapse = " |>\n")
  code <- paste0(code, sprintf('\nlib_write(%s, "%s", "%s")',
                               target[["member"]], target[["lib"]], target[["member"]]))

  list(code = code, stmt_map = as.integer(code_rows$stmt_id), flags = character(),
       parse = list(var = v, missing = inc_missing))
}

#' Base-R oracle for PROC FREQ
#'
#' @param input Input data frame / tibble.
#' @param var Column name for one-way frequency table.
#' @param missing Logical indicating whether NA values should be included in counts and percent base.
#' @return A tibble with columns `var`, `count`, `percent`.
#' @noRd
freq_oracle <- function(input, var, missing) {
  x <- input[[var]]
  if (!missing) {
    valid <- !is.na(x) & (!is.character(x) | trimws(x) != "")
    x <- x[valid]
  }
  tab <- table(x, useNA = if (missing) "ifany" else "no")
  levels_vec <- names(tab)
  if (is.numeric(input[[var]])) {
    levels_vec <- as.numeric(levels_vec)
  }
  out <- tibble::tibble(level = levels_vec, count = as.integer(tab),
                        percent = 100 * as.integer(tab) / sum(tab))
  names(out)[1] <- var
  out
}
