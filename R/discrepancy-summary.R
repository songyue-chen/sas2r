# Closed projections of comparison results already computed locally. These
# functions never open datasets or comparison-detail files. Cell examples,
# keys, arbitrary check messages and reference paths are not copied.
DISCREPANCY_SUMMARY_VERSION <- "2"

comparison_records <- function(x) {
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  x %||% list()
}

dataset_discrepancy_summary <- function(target, key = target$target_key) {
  metrics <- comparison_records(target$checks$reference_comparison$summary)
  metric <- function(name) {
    row <- Filter(function(x) identical(x$metric, name), metrics)
    if (length(row)) as.numeric(row[[1L]]$value) else NA_real_
  }
  digest <- target$differences$digest
  hints <- comparison_records(digest$pattern_hints)
  kinds <- comparison_records(target$differences$structure$kind_mismatch %||% digest$structure$kind_mismatch)
  variables <- lapply(comparison_records(digest$vars), function(v) {
    patterns <- vapply(Filter(function(h) identical(h$var, v$var), hints), `[[`, "", "hint")
    patterns <- intersect(patterns, c("CONSTANT_OFFSET", "NA_PATTERN_DIFF", "CASE_ONLY_DIFF", "PADDING_ONLY"))
    list(name = v$var, kind = v$kind, compared = as.numeric(v$n_compared),
      mismatches = as.numeric(v$n_mismatch), missing_differences = as.numeric(v$n_na_diff),
      patterns = patterns,
      absolute_offset = if ("CONSTANT_OFFSET" %in% patterns) v$mean_abs_diff else NULL)
  })
  variables <- Filter(function(v) isTRUE(v$mismatches > 0), variables)
  common <- metric("vars_common")
  generated_rows <- metric("rows_comp")
  generated_columns <- common + metric("vars_only_comp")
  if (!is.finite(generated_rows)) generated_rows <- target$dimensions$rows %||% NA_real_
  if (!is.finite(generated_columns)) generated_columns <- target$dimensions$columns %||% NA_real_
  list(schema_version = DISCREPANCY_SUMMARY_VERSION,
    target = key, kind = target$kind %||% "dataset", status = target$status %||% "unverified",
    reference_present = isTRUE(target$has_reference),
    generated = list(rows = generated_rows, columns = generated_columns),
    reference = list(rows = metric("rows_base"), columns = common + metric("vars_only_base")),
    rows_aligned = metric("rows_matched"), columns_common = common,
    value_mismatches = metric("value_mismatch_cells"), variables = variables,
    type_differences = lapply(kinds, function(x) list(name = x$var,
      reference_type = x$base_kind, generated_type = x$comp_kind)),
    alignment = target$differences$structure$alignment_resource_state %||% "unknown")
}

# Investigation only. Counts and offset magnitudes belong to human reports.
# This projection deliberately contains no numeric comparison targets.
reviewer_discrepancy_summary <- function(target, key = target$target_key) {
  s <- dataset_discrepancy_summary(target, key)
  relative <- function(a, b) {
    if (any(!is.finite(c(a, b)))) return("unknown")
    if (a == b) "same" else if (a > b) "generated_more" else "generated_fewer"
  }
  list(schema_version = DISCREPANCY_SUMMARY_VERSION, target = s$target,
    kind = s$kind, status = s$status, reference_present = s$reference_present,
    rows = relative(s$generated$rows, s$reference$rows),
    columns = relative(s$generated$columns, s$reference$columns),
    row_pairing = if (any(!is.finite(c(s$rows_aligned, s$generated$rows, s$reference$rows)))) "unknown" else
      if (s$rows_aligned == s$generated$rows && s$rows_aligned == s$reference$rows) "complete" else "partial",
    variables = lapply(s$variables, function(v) list(name = v$name, kind = v$kind,
      differences = if (isTRUE(v$mismatches == v$missing_differences)) "missing_only" else
        if (isTRUE(v$mismatches == v$compared)) "all" else "some",
      patterns = v$patterns)), type_differences = s$type_differences,
    alignment = s$alignment)
}

discrepancy_description <- function(summary) {
  parts <- vapply(summary$variables, function(v) {
    detail <- if ("CONSTANT_OFFSET" %in% v$patterns) paste0("constant difference",
      if (length(v$absolute_offset) && is.finite(v$absolute_offset))
        paste0(" (absolute ", format(v$absolute_offset, trim = TRUE), ")")) else
      if (isTRUE(v$missing_differences == v$mismatches)) "missing-value differences" else "value differences"
    paste0(v$name, ": ", v$mismatches, " ", detail)
  }, "")
  if (length(summary$type_differences)) parts <- c(parts, paste0(
    "Type differs: ", paste(vapply(summary$type_differences, function(v)
      paste0(v$name, " (", v$generated_type, " vs ", v$reference_type, ")"), ""), collapse = ", ")))
  if (is.finite(summary$rows_aligned) &&
      (summary$rows_aligned < summary$generated$rows || summary$rows_aligned < summary$reference$rows))
    parts <- c(parts, "Some rows could not be paired")
  if (is.finite(summary$columns_common) &&
      (summary$columns_common < summary$generated$columns || summary$columns_common < summary$reference$columns))
    parts <- c(parts, "Some column names occur on only one side")
  if (!length(parts)) return(if (summary$status == "passed") "No required differences found" else
    "Cause unresolved; inspect the checks and source/code evidence")
  paste(parts, collapse = "; ")
}

discrepancy_table <- function(summaries) {
  dimension <- function(x) if (any(!is.finite(c(x$rows, x$columns)))) "Unavailable" else
    paste(x$rows, "x", x$columns)
  match_text <- function(n, a, b) {
    if (any(!is.finite(c(n, a, b)))) return("Unavailable")
    total <- max(a, b)
    if (!total) return("0/0 (both empty)")
    sprintf("%.1f%% (%s/%s)", 100 * n / total, n, total)
  }
  rows <- lapply(summaries, function(s) data.frame(
    `Target dataset` = s$target, Status = s$status,
    `Generated dimension` = dimension(s$generated), `Reference dimension` = dimension(s$reference),
    `Rows aligned` = match_text(s$rows_aligned, s$generated$rows, s$reference$rows),
    `Columns present in both` = match_text(s$columns_common, s$generated$columns, s$reference$columns),
    `Primary discrepancy` = discrepancy_description(s), check.names = FALSE))
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

report_repair_table <- function(state) {
  rows <- list()
  add <- function(cid, revision, result, reason, reference_triggered = FALSE) {
    if (isTRUE(reference_triggered)) reason <- c(
      "Source review prompted by a reference discrepancy; candidate acceptance remains reference-blind.", reason)
    rows[[length(rows) + 1L]] <<- data.frame(Component = cid %||% "Unknown",
      Revision = revision %||% "Unknown", Result = result,
      Reason = paste(reason %||% "Not recorded", collapse = "; "), check.names = FALSE)
  }
  for (repair in state$repairs %||% state$repair_history %||% list())
    add(repair$component_id, repair$revision_id, "Accepted for rerun", repair$summary %||% repair$diagnosis,
      repair$reference_triggered_review)
  for (repair in state$diagnostics$rejected_repairs %||% list())
    add(repair$component_id, repair$revision_id, "Rejected; previous code retained", repair$errors,
      repair$reference_triggered_review)
  for (cid in names(state$histories)) {
    history <- state$histories[[cid]]
    if (length(history$revisions) < 2L) next
    for (revision in history$revisions[-1L]) {
      add(cid, revision$revision_id, if (identical(revision$revision_id, history$active_revision_id))
        "Current component revision" else "Earlier component candidate",
        paste("Evidence:", revision$level %||% "unverified", "; blockers:",
          paste(revision$blockers %||% "none", collapse = ", ")))
    }
  }
  if (length(rows)) unique(do.call(rbind, rows)) else data.frame()
}

report_explanation_lines <- function(diagnosis) {
  if (is.null(diagnosis)) return("Explanatory diagnosis was not performed for this report.")
  c(diagnosis$reason, unlist(lapply(diagnosis$explanations, function(f) c(
    paste0("Target: ", f$target, "; component: ", f$component_id),
    paste("Possible cause:", f$possible_cause),
    paste("SAS evidence:", f$sas_evidence), paste("R evidence:", f$r_evidence),
    paste("Next action:", f$next_action))), use.names = FALSE))
}
