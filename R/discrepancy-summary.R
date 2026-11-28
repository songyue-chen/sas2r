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

# The closed format of the summary a model may see. Anything outside it stays
# in the human report: counts, magnitudes, cell values, keys, reference paths.
REVIEWER_SUMMARY_FIELDS <- c("schema_version", "target", "kind", "status",
  "reference_present", "rows", "columns", "row_pairing", "variables",
  "type_differences", "alignment")
REVIEWER_SUMMARY_VARIABLE_FIELDS <- c("name", "kind", "differences", "patterns")
REVIEWER_SUMMARY_TYPE_FIELDS <- c("name", "reference_type", "generated_type")
REVIEWER_SUMMARY_VOCABULARY <- list(
  relation = c("same", "generated_more", "generated_fewer", "unknown"),
  row_pairing = c("complete", "partial", "unknown"),
  differences = c("missing_only", "all", "some"),
  patterns = c("CONSTANT_OFFSET", "NA_PATTERN_DIFF", "CASE_ONLY_DIFF", "PADDING_ONLY"))

# Why a summary does not fit the closed format; empty when it does. The reasons
# name fields only. They never quote a value, which could be the very thing
# that should not leave the machine.
reviewer_summary_violations <- function(summary, reference_path = NULL) {
  if (!is.list(summary)) return("the summary is not a list")
  vocabulary <- REVIEWER_SUMMARY_VOCABULARY
  field_names <- function(x) names(x) %||% character()
  unexpected <- function(found, allowed, where) {
    extra <- setdiff(found, allowed)
    safe <- extra[grepl("^[A-Za-z_][A-Za-z0-9_.]{0,39}$", extra)]
    c(if (length(extra)) paste0("unexpected field in ", where,
        if (length(safe)) paste0(" (", paste(safe, collapse = ", "), ")")),
      if (length(setdiff(allowed, found))) paste0("missing field in ", where))
  }
  # A name or label: one short single-line string. NULL means not recorded.
  label <- function(x) is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x) &&
    nchar(x) <= 200L && !grepl("[\r\n]", x))
  one_of <- function(x, allowed) is.character(x) && length(x) == 1L && !is.na(x) && x %in% allowed
  rows <- function(x) if (is.list(x) && is.null(names(x))) x else NULL
  variables <- rows(summary$variables)
  types <- rows(summary$type_differences)
  leaves <- unlist(summary, use.names = FALSE)
  problems <- c(
    unexpected(field_names(summary), REVIEWER_SUMMARY_FIELDS, "the summary"),
    if (!identical(summary$schema_version, DISCREPANCY_SUMMARY_VERSION)) "schema_version is not the current one",
    if (!all(vapply(summary[c("target", "kind", "status", "alignment")], label, logical(1))))
      "target, kind, status or alignment is not a short label",
    if (!(is.logical(summary$reference_present) && length(summary$reference_present) == 1L &&
        !is.na(summary$reference_present))) "reference_present is not TRUE or FALSE",
    if (!one_of(summary$rows, vocabulary$relation) || !one_of(summary$columns, vocabulary$relation))
      "rows or columns is outside the fixed vocabulary",
    if (!one_of(summary$row_pairing, vocabulary$row_pairing)) "row_pairing is outside the fixed vocabulary",
    if (is.null(variables) && length(summary$variables)) "variables is not a list of records",
    unlist(lapply(variables, function(v) c(
      unexpected(field_names(v), REVIEWER_SUMMARY_VARIABLE_FIELDS, "a variable record"),
      if (!label(v$name) || !label(v$kind)) "a variable name or kind is not a short label",
      if (!one_of(v$differences, vocabulary$differences)) "a variable's differences value is outside the fixed vocabulary",
      if (!(is.character(v$patterns) || is.null(v$patterns)) || !all(v$patterns %in% vocabulary$patterns))
        "a variable's patterns are outside the fixed vocabulary"))),
    if (is.null(types) && length(summary$type_differences)) "type_differences is not a list of records",
    unlist(lapply(types, function(v) c(
      unexpected(field_names(v), REVIEWER_SUMMARY_TYPE_FIELDS, "a type-difference record"),
      if (!label(v$name) || !label(v$reference_type) || !label(v$generated_type))
        "a type-difference name or type is not a short label"))),
    if (any(unlist(rapply(summary, is.numeric, how = "list")))) "a numeric value is present",
    if (is.character(reference_path) && length(reference_path) == 1L && !is.na(reference_path) &&
        nzchar(reference_path) && is.character(leaves) &&
        any(grepl(reference_path, leaves, fixed = TRUE))) "a reference path is present"
  )
  unique(as.character(problems))
}

# A summary that fails the closed format never reaches a model. The run goes on
# without it and the omission is recorded where the caller keeps diagnostics.
# Tests run with the strict option set, so a defect in the constructor fails
# there loudly instead of dropping summaries quietly.
withhold_reviewer_summary <- function(key, problems, omitted = NULL) {
  key <- if (is.character(key) && length(key) == 1L && !is.na(key)) key else "unknown"
  if (isTRUE(getOption("sas2r.strict_discrepancy_summary", FALSE))) {
    cli::cli_abort(c("The reviewer discrepancy summary for {.val {key}} is outside its closed format.",
      "x" = "{problems}"), class = "sas2r_discrepancy_summary_violation")
  }
  if (is.environment(omitted)) {
    omitted$items <- c(omitted$items, list(list(type = "discrepancy_summary_omitted",
      target = key, reasons = problems)))
  }
  cli::cli_warn(c("The comparison summary for {.val {key}} was withheld from model requests.",
    "i" = "It did not fit the closed format a model is allowed to see: {problems}.",
    "i" = "The run continues without it. Please report this as a sas2r defect."),
    class = "sas2r_discrepancy_summary_omitted")
  NULL
}

# Investigation only. Counts and offset magnitudes belong to human reports.
# This projection deliberately contains no numeric comparison targets, and the
# check below holds it to that before it is returned. `omitted` is an
# environment the caller supplies to collect what was withheld.
reviewer_discrepancy_summary <- function(target, key = target$target_key, omitted = NULL) {
  s <- dataset_discrepancy_summary(target, key)
  relative <- function(a, b) {
    if (any(!is.finite(c(a, b)))) return("unknown")
    if (a == b) "same" else if (a > b) "generated_more" else "generated_fewer"
  }
  summary <- list(schema_version = DISCREPANCY_SUMMARY_VERSION, target = s$target,
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
  problems <- reviewer_summary_violations(summary, target$reference_path)
  if (!length(problems)) return(summary)
  withhold_reviewer_summary(key, problems, omitted)
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
  for (outcome in state$diagnostics$bundle_repair$outcomes %||% list())
    if (identical(outcome$outcome, "identical_patch"))
      add(outcome$component_id, outcome$revision_id, "No change; previous code retained",
        c(paste("Outcome run:", outcome$run_id %||% "Unknown"),
          paste("Diagnosis (unverified):", outcome$diagnosis),
          paste("Remaining uncertainty:", paste(outcome$remaining_uncertainty, collapse = "; ")),
          paste("Evidence:", paste(outcome$evidence_ids, collapse = ", "))),
        outcome$reference_triggered_review)
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
