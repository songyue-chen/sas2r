# Terminal advice. Keep this independent of repair, selection and validation.
migration_package_bug_diagnosis <- function(state, attempts, failure = NULL) {
  result <- list(schema_version = 1L, status = "disabled", findings = list())
  if (!isTRUE(state$config$migration$bug_reports)) return(result)

  findings <- list()
  if (inherits(failure, "sas2r_scheduler_unaccounted_components")) {
    findings <- list(list(code = "scheduler_unaccounted_components",
      pending_count = failure$pending_count, resume_requested = state$resume_requested,
      order = if (is.null(state$graph$execution_order)) "inferred" else "configured"))
  } else if (is.null(failure)) {
    # Assessments belong to the selected attempt. A rejected or older attempt
    # may have run different code, even when its component IDs are the same.
    id <- state$selected_attempt$attempt_id
    attempt <- if (!is.null(id)) attempts$completed_attempts[[id]] else NULL
    if (isTRUE(attempt$passed) && !isTRUE(attempt$deferred) &&
        identical(attempt$attempt_dir, state$selected_attempt$attempt_dir) &&
        identical(attempt$run_id, state$paths$run_id)) {
      findings <- Filter(Negate(is.null), lapply(state$assessment$targets, function(target) {
        diagnose_pdf_destination(state, attempt, target)
      }))
    }
  }

  result$status <- "completed"
  result$findings <- unname(findings)
  if (!length(findings)) return(result)
  description <- file.path(getNamespaceInfo(asNamespace("sas2r"), "path"), "DESCRIPTION")
  revision <- if (file.exists(description)) read.dcf(description, fields = "RemoteSha")[1L, 1L] else NA_character_
  result$environment <- list(
    sas2r = state$environment$versions$sas2r %||% as.character(getNamespaceVersion("sas2r")),
    R = state$environment$versions$R %||% as.character(getRversion()),
    platform = R.version$platform,
    revision = if (!is.na(revision)) revision,
    workers = state$environment$parallel$effective %||% state$parallel$effective)
  result
}

# A wrong-format output alone does not identify the writer. For the beta,
# require a direct bundled helper call with a literal destination that resolves
# to the assessed file. Indirect or dynamic writers need investigation.
diagnose_pdf_destination <- function(state, attempt, target) {
  if (!identical(target$kind, "tlf") || !target$status %in% c("failed", "needs_review")) return(NULL)
  if (!is_scalar_character(target$candidate_path)) return(NULL)
  format <- tolower(tools::file_ext(target$target_key))
  if (!is_pdf_destination_finding_format(format)) return(NULL)

  contracts <- state$assessment$contracts
  producer <- contracts$producer_node_id[contracts$target_key == target$target_key]
  cid <- graph_output_writers(state$graph, target$target_key, producer)
  if (length(cid) != 1L || !cid %in% attempt$executed_component_ids ||
      is.null(attempt$helper_hash)) return(NULL)
  revision <- state$selected_revisions[[cid]]
  executed_revision <- attempt$revision_manifest[[cid]]
  if (is.null(executed_revision$revision_id) ||
      !identical(revision$revision_id, executed_revision$revision_id) ||
      !identical(migration_hash(revision_code(revision)), executed_revision$r_hash)) return(NULL)

  # A program can call the installed sas2r namespace explicitly. Only bare
  # calls are candidates for the bundled-runtime contract checked here.
  calls <- Filter(function(expr) is.call(expr) && identical(expr[[1L]], quote(sas_print_table)),
    as.list(smoke_program_expressions(revision_code(revision))))
  for (call in calls) {
    args <- tryCatch(match.call(sas_print_table, call), error = function(e) NULL)
    path <- as.list(args)$file
    if (!is.call(path) || !identical(path[[1L]], quote(sas_output_path)) || length(path) != 2L) next
    logical_path <- as.list(tryCatch(match.call(sas_output_path, path), error = function(e) NULL))$path
    if (!is_scalar_character(logical_path)) next
    relative <- tryCatch(sas2r_tlf_relative_path(logical_path), error = function(e) NULL)
    if (is.null(relative)) next
    dest <- normalizePath(file.path(attempt$attempt_dir, "tlf", relative),
      winslash = "/", mustWork = FALSE)
    if (!identical(dest, target$candidate_path) ||
        !identical(tolower(tools::file_ext(dest)), format) || !file.exists(dest)) next
    header <- tryCatch(readBin(dest, "raw", n = 5L), error = function(e) raw())
    if (identical(header, charToRaw("%PDF-"))) {
      return(list(code = "pdf_written_to_non_pdf_destination",
        format = format, component_id = cid, target_key = target$target_key,
        attempt_id = attempt$attempt_id,
        revision_id = executed_revision$revision_id, helper_hash = attempt$helper_hash))
    }
  }
  NULL
}

is_pdf_destination_finding_format <- function(format) {
  is_scalar_character(format) && format %in% c("html", "htm", "rtf", "docx", "png", "jpg", "jpeg")
}

package_bug_finding_summary <- function(finding) {
  switch(finding$code,
    pdf_written_to_non_pdf_destination = paste0("Bundled sas_print_table() call matched a .",
      finding$format, " output containing PDF bytes (", finding$component_id, ", ",
      finding$target_key, ", ", finding$attempt_id, ")."),
    scheduler_unaccounted_components = "The translation scheduler stopped with components still pending.")
}

package_bug_report_lines <- function(diagnosis) {
  if (identical(diagnosis$status, "unavailable"))
    return(c("Beta package-bug diagnosis unavailable. See the machine report for details.", ""))
  if (!length(diagnosis$findings)) return(character())
  c("## Possible sas2r bugs (beta)", "",
    vapply(diagnosis$findings, function(finding) paste("-", package_bug_finding_summary(finding)), ""),
    "", "Create an issue draft with `sas2r::sas_bug_report()` using this run's `report.json`.", "")
}
