MEANS_STATS <- c("n", "mean", "std", "min", "max", "sum", "median")

means_stat_expr <- function(stat, v) switch(stat,
  n = sprintf("sum(!is.na(%s))", v),
  mean = sprintf("if (all(is.na(%s))) NA_real_ else mean(%s, na.rm = TRUE)", v, v),
  sum = sprintf("if (all(is.na(%s))) NA_real_ else sum(%s, na.rm = TRUE)", v, v),
  std = sprintf("if (sum(!is.na(%s)) < 2) NA_real_ else stats::sd(%s, na.rm = TRUE)", v, v),
  min = sprintf("if (all(is.na(%s))) NA_real_ else min(%s, na.rm = TRUE)", v, v),
  max = sprintf("if (all(is.na(%s))) NA_real_ else max(%s, na.rm = TRUE)", v, v),
  median = sprintf("if (all(is.na(%s))) NA_real_ else stats::median(%s, na.rm = TRUE)", v, v))

#' Emit R code for a PROC MEANS unit
#'
#' Translates supported single-class, single-variable PROC MEANS units with NOPRINT
#' and OUTPUT OUT= statements into a grouped dplyr summarise pipeline with SAS NA semantics.
#'
#' @param us A tibble of statements corresponding to a single `proc_step` unit.
#' @return A list with elements `code` (character), `stmt_map` (integer), and `flags` (character).
#' @noRd
emit_proc_means <- function(us) {
  reject <- list(code = NA_character_, stmt_map = as.integer(us$stmt_id), flags = "means_not_t1")
  code_rows <- us[us$type == "code", ]
  proc <- code_rows$text[code_rows$first_token == "proc"][1]
  if (is.na(proc) || !grepl("\\bnoprint\\b", proc, ignore.case = TRUE)) return(reject)
  data_in <- eq_captures(proc, "data")
  if (!length(data_in) || !nzchar(data_in[1])) return(reject)

  cls_txt <- code_rows$text[code_rows$first_token == "class"]
  if (length(cls_txt) > 0L) {
    if (length(cls_txt) > 1L) return(reject)
    cls_body <- sub("/.*$", "", cls_txt[1])
    cls_opt <- if (grepl("/", cls_txt[1])) trimws(sub("^.*?/", "", cls_txt[1])) else ""
    if (nzchar(cls_opt)) {
      opt_tokens <- tolower(strsplit(cls_opt, "\\s+")[[1]])
      opt_tokens <- opt_tokens[nzchar(opt_tokens)]
      if (!all(opt_tokens == "missing")) return(reject)
    }
    cls <- tolower(strsplit(trimws(sub("^class\\s+", "", cls_body,
                                        ignore.case = TRUE)), "\\s+")[[1]])
    cls <- cls[nzchar(cls)]
    if (length(cls) != 1L) return(reject)
    if (!has_bare_option(proc, "nway")) {
      return(list(code = NA_character_, stmt_map = as.integer(code_rows$stmt_id),
                  flags = "means_class_requires_nway"))
    }
  } else {
    cls <- character()
    cls_opt <- ""
  }

  var_txt <- code_rows$text[code_rows$first_token == "var"]
  if (length(var_txt) != 1L) return(reject)
  v <- tolower(strsplit(trimws(sub("^var\\s+", "", var_txt[1],
                                   ignore.case = TRUE)), "\\s+")[[1]])
  v <- v[nzchar(v)]
  if (length(v) != 1L) return(reject)

  out_txt <- code_rows$text[code_rows$first_token == "output"]
  if (length(out_txt) != 1L) return(reject)
  out_ds <- eq_captures(out_txt[1], "out")
  if (!length(out_ds) || !nzchar(out_ds[1])) return(reject)

  prs <- regmatches(out_txt[1], gregexpr(
    "\\b(n|mean|std|min|max|sum|median)\\s*=\\s*([A-Za-z_]\\w*)",
    out_txt[1], ignore.case = TRUE))[[1]]
  prs <- prs[!grepl("^out\\s*=", prs, ignore.case = TRUE)]
  if (!length(prs)) return(reject)

  remaining <- trimws(sub("^output\\s+", "", out_txt[1], ignore.case = TRUE))
  if (!proc_options_supported(proc, "means", "data", c("noprint", "nway", "missing", MEANS_STATS)) ||
      !proc_options_supported(paste("proc means", remaining), "means", c("out", MEANS_STATS))) return(reject)
  if (!deterministic_names(c(cls, v))) return(reject)
  aliases <- character(); stat_ids <- character()
  for (p in prs) {
    g <- regmatches(p, regexec("(\\w+)\\s*=\\s*(\\w+)", p))[[1]]
    stat_ids <- c(stat_ids, tolower(g[2])); aliases <- c(aliases, tolower(g[3]))
  }

  inc_missing <- has_bare_option(proc, "missing") ||
                 (nzchar(cls_opt) && has_bare_option(cls_opt, "missing"))

  allowed_tokens <- c("proc", "class", "var", "output", "run", "quit")
  if (!all(tolower(code_rows$first_token) %in% allowed_tokens)) return(reject)

  src <- split_ds(norm_ds(data_in[1]))
  target <- split_ds(norm_ds(out_ds[1]))
  parts <- vapply(seq_along(stat_ids), function(k) {
    sprintf("%s = %s", aliases[k], means_stat_expr(stat_ids[k], v))
  }, character(1))

  lines <- c(sprintf('%s <- lib_read("%s", "%s")', target[["member"]], src[["lib"]], src[["member"]]),
             "  sas2r_fold_names()",
             if (length(cls) && !inc_missing)
               sprintf("  dplyr::filter(!is.na(%s) & (!is.character(%s) | trimws(%s) != \"\"))", cls, cls, cls),
             if (length(cls)) sprintf("  dplyr::group_by(%s)", cls),
             sprintf("  dplyr::summarise(%s, `_freq_` = dplyr::n(), .groups = \"drop\")",
                     paste(parts, collapse = ", ")))
  code <- paste(lines, collapse = " |>\n")
  code <- paste0(code, sprintf('\nlib_write(%s, "%s", "%s")',
                                target[["member"]], target[["lib"]], target[["member"]]))


  named_stats <- stats::setNames(stat_ids, aliases)
  list(code = code, stmt_map = as.integer(code_rows$stmt_id),
       flags = "means_no_type",
       parse = list(class = cls, var = v, stats = named_stats, missing = inc_missing))
}
