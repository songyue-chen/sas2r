# Local navigation is a projection of the existing report, never a second gate.
run_html_escape <- function(text) {
  text <- as.character(text %||% "")
  for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"),
                    c('"', "&quot;"), c("'", "&#39;"))) {
    text <- gsub(pair[[1]], pair[[2]], text, fixed = TRUE)
  }
  text
}

run_html_link <- function(path, label, root) {
  if (is.null(path) || is.na(path) || !file.exists(file.path(root, path))) {
    return(paste0(run_html_escape(label), " (not available or pruned)"))
  }
  url <- paste(vapply(strsplit(path, "/", fixed = TRUE)[[1]], utils::URLencode,
                      character(1), reserved = TRUE), collapse = "/")
  paste0('<a href="', run_html_escape(url), '">', run_html_escape(label), '</a>')
}

write_run_navigation <- function(state, report) {
  paths <- state$paths
  order_path <- file.path(paths$bundle, "run-order.json")
  order <- if (file.exists(order_path)) read_json_record(order_path) else list()
  components <- list()
  sources <- if (!is.null(state$graph$nodes)) unique(state$graph$nodes[["source_file"]]) else character()
  sources <- as.character(sources)
  sources <- sources[!is.na(sources) & file.exists(sources)]
  source_links <- stats::setNames(file.path("report", "sources", sprintf("source-%03d.sas", seq_along(sources))), sources)
  for (source in sources) {
    target <- file.path(paths$run_root, source_links[[source]])
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, target, overwrite = TRUE)) cli::cli_abort("Could not copy source {.file {source}}")
  }
  ids <- unique(c(state$schedule$component_id, names(state$selected_revisions),
                  names(report$component_evidence)))
  for (id in ids) {
    rev <- state$selected_revisions[[id]]
    evidence <- report$component_evidence[[id]] %||% list()
    staged <- rev$staged_file %||% rev$contract$staged_file %||% paste0(id, ".R")
    code_path <- file.path("bundle", user_bundle_path(staged))
    generated <- file.exists(file.path(paths$run_root, code_path))
    nodes <- state$graph$nodes
    source <- if (!is.null(nodes)) unique(nodes[["source_file"]][nodes$component_id %in% id]) else character()
    source <- source[!is.na(source)]
    deps <- if (!is.null(state$graph)) dependency_closure(state$graph, id) else character()
    comparison <- Filter(function(x) id %in% x$upstream_components,
                          state$assessment$lineage_by_target %||% list())
    targets <- unique(c(names(comparison), state$output_contracts$target_key[
      state$output_contracts$source_file %in% source]))
    components[[id]] <- list(component_id = id, source = source,
      code = if (generated) code_path else NULL,
      source_links = unname(source_links[source]),
      revision_id = rev$revision_id %||% NULL, revision_path = rev$r_path %||% NULL,
      dependencies = I(deps),
      generated = generated, mechanical_checks = evidence$mechanical_checks,
      review = evidence$review_status %||% "not reviewed",
      execution = paste0("smoke: ", evidence$smoke_status %||% "unexecuted", "; bundle: ",
        report$bundle_execution[[id]]$summary %||% "unexecuted"),
      execution_details = evidence$smoke_execution,
      bundle_execution = report$bundle_execution[[id]],
      output_targets = targets, blockers = evidence$blockers %||% list(),
      next_action = if (!generated) paste("Not generated.", report$status_reason %||% "See diagnostics.") else
        if (length(evidence$blockers)) "Resolve the reported blockers and rerun affected dependencies." else
          "Inspect code and output comparisons before use; human edits require new QC.")
  }
  comparisons <- list()
  for (i in seq_along(report$output_assessments)) {
    key <- names(report$output_assessments)[[i]]
    relative <- file.path("report", "comparison-details", sprintf("target-%03d.json", i))
    atomic_write_json(report$output_assessments[[i]], file.path(paths$run_root, relative))
    comparisons[[key]] <- relative
  }
  log_file <- file.path(paths$logs, "llm_log.jsonl")
  logs <- if (file.exists(log_file)) lapply(readLines(log_file, warn = FALSE), function(line) {
    jsonlite::fromJSON(line, simplifyVector = FALSE)
  }) else list()
  settings <- lapply(Filter(function(x) identical(x$agent, "settings"), logs), function(x) {
    x[c("provider", "resolved_model", "type", "requested_parameters", "effective_parameters")]
  })
  records <- Filter(function(x) identical(x$run_id, report$run_id), state$usage_budget$records %||% list())
  if (length(records)) {
    dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
    writeLines(vapply(records, function(record) as.character(jsonlite::toJSON(record,
      auto_unbox = TRUE, null = "null", force = TRUE)), character(1)), file.path(paths$logs, "usage.jsonl"))
  }
  manifest <- list(schema_version = 1L, run_id = report$run_id, status = report$status,
    status_reason = report$status_reason, outcome = report$outcome,
    selected_attempt_id = report$selected_attempt_id,
    executed_bundle = if (!is.null(state$selected_attempt$attempt_dir)) file.path(state$selected_attempt$attempt_dir, "bundle") else NULL,
    paths = list(start_here = "START_HERE.html", bundle = "bundle", report = "report/translation.md",
      machine_report = "report/report.json", outputs = "outputs", diagnostics = "diagnostics"),
    settings = settings, components = components, outputs = state$saved_outputs %||% list(), comparisons = comparisons,
    execution_order = as.character(unlist(order$programs)))
  atomic_write_json(manifest, paths$manifest)
  link <- function(path, label) run_html_link(path, label, paths$run_root)
  log_link <- function(path, label) {
    root <- paste0(normalizePath(paths$run_root, winslash = "/", mustWork = FALSE), "/")
    full <- if (!is.null(path)) normalizePath(path, winslash = "/", mustWork = FALSE) else ""
    relative <- if (startsWith(full, root)) substring(full, nchar(root) + 1L) else NULL
    link(relative, label)
  }
  rows <- vapply(components, function(component) {
    checks <- component$mechanical_checks
    mechanical <- if (is.null(checks)) "unverified" else if (isTRUE(checks$pass)) "passed" else "failed"
    comparison <- vapply(component$output_targets, function(key) {
      ass <- report$output_assessments[[key]]
      paste(key, ass$status %||% "unverified", sep = ": ")
    }, character(1))
    paste0("<tr><td>", run_html_escape(component$component_id), "<br>",
      if (component$generated) link(component$code, "Open R script") else "Not generated",
      "<br>", paste(vapply(seq_along(component$source), function(i) {
        link(component$source_links[[i]], component$source[[i]])
      }, character(1)), collapse = "<br>"),
      "</td><td>", run_html_escape(paste(component$dependencies, collapse = ", ")),
      "</td><td>", run_html_escape(mechanical), "</td><td>", run_html_escape(component$review),
      "</td><td>", run_html_escape(component$execution), "</td><td>",
      run_html_escape(if (length(comparison)) paste(comparison, collapse = "; ") else "No direct comparison recorded"),
      "</td><td>", run_html_escape(component$next_action), "<details><summary>Diagnostics</summary><pre>",
      run_html_escape(jsonlite::toJSON(list(blockers = component$blockers,
        checks = checks, execution = component$execution_details, bundle = component$bundle_execution), auto_unbox = TRUE, pretty = TRUE, null = "null", force = TRUE)),
      "</pre></details></td></tr>")
  }, character(1))
  output_rows <- vapply(seq_len(nrow(state$output_contracts %||% empty_output_contracts())), function(i) {
    target <- state$output_contracts$target_key[[i]]
    saved <- manifest$outputs[[target]]
    ass <- report$output_assessments[[target]]
    paste0("<li>", if (!is.null(saved)) link(saved$path, target) else
      paste0(run_html_escape(target), if (identical(ass$status, "unresolved_target"))
        " - source expression; concrete outputs unknown" else " - not generated"), " - ", run_html_escape(ass$status %||% "unverified"),
      " | ", if (!is.null(comparisons[[target]])) link(comparisons[[target]], "Comparison details") else "No comparison recorded", "</li>")
  }, character(1))
  unresolved_rows <- vapply(state$output_contracts$target_key, function(key) {
    identical(report$output_assessments[[key]]$status, "unresolved_target")
  }, logical(1))
  instructions <- if (file.exists(file.path(paths$bundle, "README.md"))) {
    readLines(file.path(paths$bundle, "README.md"), warn = FALSE)
  } else "No bundle is available yet. Resolve the error below and start a new translation."
  # Preserve the same guide text, with lightweight headings and preformatted code.
  guide <- character(); in_code <- FALSE
  for (line in instructions) {
    if (identical(line, "```r") || identical(line, "```")) {
      guide <- c(guide, if (in_code) "</code></pre>" else "<pre><code>")
      in_code <- !in_code
    } else if (in_code) guide <- c(guide, run_html_escape(line)) else if (startsWith(line, "## ")) {
      guide <- c(guide, paste0("<h3>", run_html_escape(substring(line, 4L)), "</h3>"))
    } else if (nzchar(line) && !startsWith(line, "# ")) guide <- c(guide, paste0("<p>", run_html_escape(line), "</p>"))
  }
  writeLines(c('<!doctype html><html lang="en"><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>sas2r migration run</title>',
    '<style>body{font:16px/1.5 system-ui,sans-serif;color:#203039;background:#fafbfc;margin:2rem auto;padding:0 1.5rem;max-width:1400px}h1,h2,h3{line-height:1.2}a{color:#075c91}table{border-collapse:collapse;width:100%}th,td{text-align:left;vertical-align:top;padding:.7rem;border:1px solid #d3dde2}th{background:#eaf0f4}pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#edf2f5;padding:1rem}td{overflow-wrap:anywhere}details{max-width:38rem}.status{font-size:1.25rem;font-weight:600}</style>',
    '<style>.outcome{border:2px solid #236348;border-radius:8px;padding:1rem 1.3rem;margin:1rem 0;background:#edf8f1}.outcome.error{border-color:#a51d29;background:#fff0f1}.outcome.warning{border-color:#946100;background:#fff8e5}.outcome h2{margin-top:0}.outcome pre{background:transparent;padding:0;margin:.6rem 0}</style>',
    '<h1>sas2r migration run</h1>', paste0('<p>', run_html_escape(report$run_id), '</p>'),
    paste0('<section class="outcome ', report$outcome$severity, '" aria-label="Run outcome">'),
    paste0('<h2>', run_html_escape(migration_outcome_lines(report$outcome)[1L]), '</h2>'),
    paste0('<pre>', run_html_escape(paste(migration_outcome_lines(report$outcome)[-1L], collapse = "\n")), '</pre>'),
    paste0('<p><a href="#components">Affected components and checks</a> | ',
      link("report/translation.md", "Full report"), ' | ', link("diagnostics", "Diagnostics"),
      paste(vapply(names(report$outcome$bundle_logs), function(name)
        paste0(' | ', log_link(report$outcome$bundle_logs[[name]], paste("Last bundle", name))), ""), collapse = ""),
      '</p></section>'),
    paste0('<p>Recorded status: ', run_html_escape(report$status), '</p>'),
    '<p>Execution, independent review, and reference equivalence are separate results. Saved outputs may be partial or unvalidated. Manual edits and reruns do not change this report.</p>',
    paste0('<nav>', paste(c(link("bundle/README.md", "Bundle instructions"), link("report/translation.md", "Translation report"),
      link("report/report.json", "Machine report"), link("manifest.json", "Run manifest"),
      link("diagnostics", "Diagnostics")), collapse = " | "), '</nav>'),
    '<h2 id="components">Programs and called macros</h2>',
    if (length(rows)) c('<div style="overflow-x:auto"><table><thead><tr><th>Component / source</th><th>Dependencies</th><th>Checks</th><th>Review</th><th>Execution</th><th>Output assessment</th><th>Next action</th></tr></thead><tbody>', rows, '</tbody></table></div>') else '<p>No component code was generated.</p>',
    '<h2>Saved outputs</h2>', if (any(!unresolved_rows)) c('<ul>', output_rows[!unresolved_rows], '</ul>') else '<p>No concrete output targets were recorded.</p>',
    if (any(unresolved_rows)) c('<h2>Unresolved output expressions</h2>',
      '<p>These source expressions are not additional missing files. Concrete filenames and complete coverage remain unverified.</p>',
      '<ul>', output_rows[unresolved_rows], '</ul>'),
    '<h2>Model, settings, and usage</h2><pre>',
    run_html_escape(jsonlite::toJSON(settings, auto_unbox = TRUE, pretty = TRUE, null = "null")),
    run_html_escape(paste(c(migration_environment_lines(report$environment), migration_usage_lines(report$usage)), collapse = "\n")), '</pre>',
    '<h2>Use and edit your translated scripts</h2>', guide,
    '<h2>Run diagnostics</h2><pre>', run_html_escape(jsonlite::toJSON(report$diagnostics, pretty = TRUE, auto_unbox = TRUE, null = "null", force = TRUE)),
    '</pre></html>'), paths$start_here)
  invisible(manifest)
}
