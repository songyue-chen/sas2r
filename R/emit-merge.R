#' Emit R pipeline code for a MERGE DATA step
#'
#' @param ir A `sas2r_ir` object with route `"merge"`.
#' @return A list with elements `code` (character), `stmt_map` (integer), `flags` (character).
#' @noRd
emit_merge_step <- function(ir) {
  flags <- "merge_cardinality_unproven"
  if (length(ir$inputs) != 2L || !length(ir$by) || !length(ir$outputs)) {
    return(list(code = NA_character_, stmt_map = integer(),
                flags = "merge_unsupported_shape"))
  }
  fa <- unname(ir$in_flags[1]); fb <- unname(ir$in_flags[2])
  filt <- Filter(function(s) s$kind == "merge_filter", ir$steps)
  keep <- "full"
  if (length(filt)) {
    c0 <- tolower(gsub("\\s+", " ", trimws(filt[[1]]$cond)))
    keep <- if (!is.na(fa) && c0 == fa) "left"
      else if (!is.na(fb) && c0 == fb) "right"
      else if (!is.na(fa) && !is.na(fb) && c0 %in% paste(c(fa, fb), "and", c(fb, fa))) "both"
      else if (!is.na(fa) && !is.na(fb) && c0 == paste(fa, "and not", fb)) "left_only"
      else if (!is.na(fa) && !is.na(fb) && c0 == paste(fb, "and not", fa)) "right_only"
      else NA_character_
    if (is.na(keep)) {
      return(list(code = NA_character_, stmt_map = integer(),
                  flags = "merge_unsupported_shape"))
    }
  }
  a <- split_ds(ir$inputs[1]); b <- split_ds(ir$inputs[2])
  outp <- split_ds(ir$outputs[1])
  code <- sprintf(
    '%s <- sas_merge(sas2r_fold_names(lib_read("%s", "%s")), sas2r_fold_names(lib_read("%s", "%s")), by = c(%s), keep = "%s")',
    outp[["member"]], a[["lib"]], a[["member"]], b[["lib"]], b[["member"]],
    paste(sprintf('"%s"', ir$by), collapse = ", "), keep)
  writes <- emit_lib_writes(outp[["member"]], ir$outputs)

  code <- paste0(code, "\n", paste(writes, collapse = "\n"))
  list(code = code,
       stmt_map = if (length(ir$steps)) as.integer(vapply(ir$steps, function(s) s$stmt_id, integer(1))) else integer(),
       flags = flags)
}
