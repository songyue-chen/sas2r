# Small HTML projections of the canonical human report, with escaped content.
run_html_table <- function(table) {
  if (!nrow(table)) return("<p>No records available.</p>")
  rows <- apply(table, 1L, function(row) paste0("<tr><td>",
    paste(run_html_escape(row), collapse = "</td><td>"), "</td></tr>"))
  paste0('<div style="overflow-x:auto"><table><thead><tr><th>',
    paste(run_html_escape(names(table)), collapse = "</th><th>"),
    "</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

run_dataset_details <- function(report, components, comparisons, link) {
  vapply(names(report$discrepancy_summaries), function(key) {
    summary <- report$discrepancy_summaries[[key]]
    related <- Filter(function(component) key %in% component$output_targets, components)
    advice <- Filter(function(f) identical(f$target, key), report$report_diagnosis$explanations %||% list())
    reference <- report$output_assessments[[key]]$reference_path
    provenance <- if (is.null(reference) || is.na(reference) || !nzchar(reference))
      "Reference provenance: unknown." else paste("Reference file:", reference,
        "(recorded path only; generating SAS version and input provenance are not established).")
    paste0("<details><summary>", run_html_escape(key), " - evidence and next steps</summary>",
      "<p><strong>Observed difference:</strong> ", run_html_escape(discrepancy_description(summary)), "</p>",
      "<p>", run_html_escape(provenance), "</p>",
      if (length(advice)) paste(vapply(advice, function(f) paste0(
        "<p><strong>Possible cause:</strong> ", run_html_escape(f$possible_cause), "</p>",
        "<p>Component: ", run_html_escape(f$component_id), "</p>",
        "<pre>SAS evidence: ", run_html_escape(f$sas_evidence), "\nR evidence: ", run_html_escape(f$r_evidence), "</pre>",
        "<p><strong>Next action:</strong> ", run_html_escape(f$next_action), "</p>"), ""), collapse = "") else
        "<p><strong>Cause unresolved.</strong> Compare the source derivation with the selected R and its upstream programs. If R follows SAS, investigate source/input/reference consistency; do not change the derivation merely to match the reference.</p>",
      "<p>Related code (dataset-level relationships; variable-level causation is unverified): ",
      paste(vapply(related, function(component) paste(c(
        link(component$code_listing %||% component$code, paste(component$component_id, "R lines")),
        vapply(seq_along(component$source), function(i)
          link(component$source_links[[i]], paste(component$component_id, "SAS")), "")), collapse = " | "), ""), collapse = "; "),
      "</p><p>", link(comparisons[[key]], "Full local comparison details"), "</p></details>")
  }, "")
}

run_advisory_groups <- function(details) {
  kind <- ifelse(grepl("[Dd]ependency|producer", details), "Dependencies",
    ifelse(grepl("[Rr]eadiness|[Ii]nput|[Ll]ibrary", details), "Inputs and readiness",
      ifelse(grepl("[Rr]eview|human judgment", details), "Source reviews", "Execution and other diagnostics")))
  vapply(split(details, kind), function(lines) paste0("<details><summary>",
    run_html_escape(kind[match(lines[[1L]], details)]), " (", length(lines), ")</summary><pre>",
    run_html_escape(paste(lines, collapse = "\n")), "</pre></details>"), "")
}

# Numbered code copies let humans cite exact lines without opening a data file.
write_report_code_listing <- function(code, path, title) {
  lines <- strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c('<!doctype html><html lang="en"><meta charset="utf-8">',
    paste0('<title>', run_html_escape(title), '</title>'),
    '<style>body{font:14px/1.5 monospace}pre{white-space:pre-wrap}.line:target{background:#fff2b3}a{color:#555;text-decoration:none;display:inline-block;min-width:4em}</style>',
    paste0('<h1>', run_html_escape(title), '</h1><pre>'),
    vapply(seq_along(lines), function(i) paste0('<span class="line" id="L', i,
      '"><a href="#L', i, '">', i, '</a>', run_html_escape(lines[i]), '</span>'), ""), '</pre></html>'), path)
}
