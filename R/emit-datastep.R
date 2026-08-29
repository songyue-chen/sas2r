#' Translate SAS condition to R with SAS missing-value truth table
#'
#' @param cond A string SAS condition.
#' @return A string R condition expression.
#' @noRd
sas_cond_to_r <- function(cond) {
  tx <- tidy_expr(translate_expr(cond))
  vars <- expr_vars(cond, r_expr = tx)
  wrap_missing(tx, vars) |> tidy_expr()
}

#' Split dataset identifier into library and member
#'
#' @param ds Character dataset name (e.g. "work.out" or "adam.adsl").
#' @param macro_vars Optional named character vector of macro variables to substitute.
#' @return A named character vector with elements `lib` and `member`.
#' @noRd
split_ds <- function(ds, macro_vars = character()) {
  if (length(macro_vars) > 0L && is.character(ds) && length(ds) == 1L) {
    for (nm in names(macro_vars)) {
      if (nzchar(nm)) {
        ds <- gsub(paste0("&", nm, "\\b"), macro_vars[[nm]], ds)
      }
    }
  }
  p <- strsplit(ds, ".", fixed = TRUE)[[1]]
  if (length(p) == 1L) c(lib = "work", member = p[1])
  else c(lib = p[1], member = p[2])
}

#' Emit R pipeline code for a DATA step
#'
#' @param ir A `sas2r_ir` object with route `"datastep"` and 0 blockers.
#' @param src_file Optional source file path for metadata/traceability.
#' @return A list with elements `code` (character), `stmt_map` (integer), `flags` (character).
#' @noRd
emit_data_step <- function(ir, src_file = "") {
  stopifnot(ir$route == "datastep", nrow(ir$blockers) == 0L)
  if (length(ir$inputs) == 0L || is.na(ir$inputs[1])) {
    return(list(code = NA_character_, stmt_map = integer(), flags = "no_input_dataset"))
  }
  if (length(ir$outputs) == 0L || is.na(ir$outputs[1])) {
    return(list(code = NA_character_, stmt_map = integer(), flags = "null_step_deferred"))
  }
  outp <- split_ds(ir$outputs[1])
  inp <- split_ds(ir$inputs[1])
  pieces <- sprintf('%s <- lib_read("%s", "%s") |>\n  sas2r_fold_names()',
                    outp[["member"]], inp[["lib"]], inp[["member"]])
  cmts <- ""
  stmt_map <- integer(); tail_steps <- list()
  add_piece <- function(piece, line) {
    pieces <<- c(pieces, paste0("  ", piece))
    cmts <<- c(cmts, sprintf("  # sas L%d", line))
  }
  for (s in ir$steps) {
    stmt_map <- c(stmt_map, s$stmt_id)
    piece <- switch(s$kind,
      where = sprintf("dplyr::filter(%s)", sas_cond_to_r(s$cond)),
      if_delete = sprintf("dplyr::filter(!(%s))", sas_cond_to_r(s$cond)),
      assign = sprintf("dplyr::mutate(%s = %s)", s$var,
                       tidy_expr(translate_expr(s$expr))),
      if_assign = {
        cond_r <- sas_cond_to_r(s$cond)
        expr_r <- tidy_expr(translate_expr(s$expr))
        prior_assigned <- any(vapply(ir$steps, function(x)
          x$kind %in% c("assign", "if_assign") && identical(x$var, s$var) &&
          x$stmt_id < s$stmt_id, logical(1)))
        if (prior_assigned) {
          sprintf("dplyr::mutate(%s = sas_if_else(%s, %s, %s))", s$var, cond_r, expr_r, s$var)
        } else {
          sprintf("dplyr::mutate(%s = if (\"%s\" %%in%% names(dplyr::pick(dplyr::everything()))) sas_if_else(%s, %s, %s) else sas_if_else(%s, %s, NA))",
                  s$var, s$var, cond_r, expr_r, s$var, cond_r, expr_r)
        }
      },
      keep = , drop = , rename = { tail_steps[[length(tail_steps) + 1L]] <- s; NULL }
    )
    if (!is.null(piece)) add_piece(piece, s$line)
  }
  for (s in tail_steps) {
    piece <- switch(s$kind,
      keep = sprintf("dplyr::select(%s)", paste(s$vars, collapse = ", ")),
      drop = sprintf("dplyr::select(-c(%s))", paste(s$vars, collapse = ", ")),
      rename = sprintf("dplyr::rename(%s)",
        paste(sprintf("%s = %s", s$pairs[, "new"], s$pairs[, "old"]),
              collapse = ", ")))
    add_piece(piece, s$line)
  }
  n <- length(pieces)
  # the pipe must precede the trailing comment, or the comment swallows it
  lines <- vapply(seq_len(n), function(i) {
    paste0(pieces[i], if (i < n) " |>" else "", cmts[i])
  }, character(1))
  code <- paste(lines, collapse = "\n")
  writes <- emit_lib_writes(outp[["member"]], ir$outputs)
  code <- paste0(code, "\n", paste(writes, collapse = "\n"))
  list(code = code, stmt_map = as.integer(stmt_map), flags = character())
}

