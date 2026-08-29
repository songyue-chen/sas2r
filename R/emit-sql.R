#' Emit R pipeline code for a PROC SQL CREATE TABLE statement
#'
#' Translates a supported single-table PROC SQL CREATE TABLE statement into a dplyr
#' pipeline with library I/O, filtering, grouping, aggregation, column selection,
#' and ordering.
#'
#' @param stmt_text A string containing the SQL statement text.
#' @param stmt_id Integer statement identifier.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_sql_create <- function(stmt_text, stmt_id) {
  reject <- list(code = NA_character_, stmt_map = stmt_id, flags = "sql_not_t1")
  stmt_text <- trimws(sub(";\\s*$", "", stmt_text))
  stmt_text <- gsub("\\s+", " ", stmt_text)   # SQL statements span lines
  low <- tolower(mask_strings(stmt_text, keep_double = FALSE))
  if (grepl("\\bjoin\\b|\\bcalculated\\b|\\binto\\s*:|from\\s*\\(|\\bhaving\\b|\\bdistinct\\b|\\bdesc\\b|\\basc\\b", low))
    return(reject)
  m <- regmatches(stmt_text, regexec(
    "^create\\s+table\\s+([\\w.]+)\\s+as\\s+select\\s+(.*?)\\s+from\\s+([\\w.]+)(?:\\s+where\\s+(.*?))?(?:\\s+group\\s+by\\s+(.*?))?(?:\\s+order\\s+by\\s+(.*?))?$",
    stmt_text, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) < 4L || m[1] == "") return(reject)
  target <- split_ds(norm_ds(m[2]))
  sel <- trimws(m[3])
  src <- split_ds(norm_ds(m[4]))
  where <- trimws(m[5])
  grp <- trimws(m[6])
  ord <- trimws(m[7])

  agg_pat <- "(count\\(\\s*\\*\\s*\\)|sum\\(\\s*\\w+\\s*\\)|avg\\(\\s*\\w+\\s*\\)|min\\(\\s*\\w+\\s*\\)|max\\(\\s*\\w+\\s*\\))\\s+as\\s+(\\w+)"
  has_agg <- grepl(agg_pat, sel, ignore.case = TRUE)
  lines <- c(sprintf('%s <- lib_read("%s", "%s")', target[["member"]], src[["lib"]], src[["member"]]),
             "  sas2r_fold_names()")

  if (nzchar(where))
    lines <- c(lines, sprintf("  dplyr::filter(%s)", sas_cond_to_r(where)))
  if (has_agg) {
    # Check if there are non-aggregate columns not accounted for by group by
    non_agg_str <- trimws(gsub("(?:^|,)\\s*(?:count\\(\\s*\\*\\s*\\)|sum\\(\\s*\\w+\\s*\\)|avg\\(\\s*\\w+\\s*\\)|min\\(\\s*\\w+\\s*\\)|max\\(\\s*\\w+\\s*\\))\\s+as\\s+\\w+\\s*", "", sel, ignore.case = TRUE))
    non_agg_str <- trimws(gsub("(^\\s*,+|\\s*,+\\s*$)", "", non_agg_str))
    if (nzchar(non_agg_str)) {
      sel_cols <- tolower(trimws(strsplit(non_agg_str, "\\s*,+\\s*")[[1]]))
      sel_cols <- sel_cols[nzchar(sel_cols)]
      grp_cols <- if (nzchar(grp)) tolower(trimws(strsplit(grp, "\\s*,+\\s*")[[1]])) else character()
      if (length(sel_cols) > 0L && !all(sel_cols %in% grp_cols)) {
        return(reject)
      }
    }
    if (nzchar(grp))
      lines <- c(lines, sprintf("  dplyr::group_by(%s)",
                                paste(trimws(strsplit(grp, "\\s*,\\s*")[[1]]), collapse = ", ")))
    aggs <- regmatches(sel, gregexpr(agg_pat, sel, ignore.case = TRUE))[[1]]
    parts <- vapply(aggs, function(a) {
      g <- regmatches(a, regexec(agg_pat, a, ignore.case = TRUE))[[1]]
      fn <- tolower(g[2]); alias <- g[3]
      body <- if (grepl("^count", fn)) "dplyr::n()"
        else {
          v <- sub("^\\w+\\(\\s*(\\w+)\\s*\\)$", "\\1", fn)
          # SQL aggregates ignore NULLs and return NULL over an all-NULL group;
          # the all-NA guard also keeps min/max from warning Inf/-Inf.
          switch(sub("\\(.*$", "", fn),
                 sum = sprintf("if (all(is.na(%s))) NA_real_ else sum(%s, na.rm = TRUE)", v, v),
                 avg = sprintf("if (all(is.na(%s))) NA_real_ else mean(%s, na.rm = TRUE)", v, v),
                 min = sprintf("if (all(is.na(%s))) NA_real_ else min(%s, na.rm = TRUE)", v, v),
                 max = sprintf("if (all(is.na(%s))) NA_real_ else max(%s, na.rm = TRUE)", v, v))
        }
      sprintf("%s = %s", alias, body)
    }, character(1))
    lines <- c(lines, sprintf("  dplyr::summarise(%s, .groups = \"drop\")",
                              paste(parts, collapse = ", ")))
  } else if (sel != "*") {
    sel_items <- trimws(strsplit(sel, "\\s*,\\s*")[[1]])
    if (!all(grepl("^[A-Za-z_][A-Za-z0-9_]*$", sel_items))) return(reject)
    lines <- c(lines, sprintf("  dplyr::select(%s)",
                              paste(sel_items, collapse = ", ")))
  }
  if (nzchar(ord))
    lines <- c(lines, sprintf("  dplyr::arrange(%s)",
                              paste(trimws(strsplit(ord, "\\s*,\\s*")[[1]]), collapse = ", ")))
  code <- paste(lines, collapse = " |>\n")
  writes <- emit_lib_writes(target[["member"]],
                            paste0(target[["lib"]], ".", target[["member"]]))
  code <- paste0(code, "\n", paste(writes, collapse = "\n"))
  list(code = code, stmt_map = stmt_id,
       flags = character())
}

#' Emit R code for a PROC SQL unit
#'
#' Wraps `emit_sql_create` by locating the single `create` statement in a `proc sql` unit.
#'
#' @param us A tibble of statements corresponding to a single `proc_step` unit.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_proc_sql <- function(us) {
  cr <- us[us$first_token == "create", ]
  if (nrow(cr) != 1L)
    return(list(code = NA_character_, stmt_map = us$stmt_id, flags = "sql_not_t1"))
  emit_sql_create(cr$text[1], cr$stmt_id[1])
}
