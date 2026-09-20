# A shared presentation of recorded activity, not another acceptance gate.
migration_run_outcome <- function(state, report, attempts) {
  failure <- report$diagnostics$failure
  status <- report$current_run_status %||% report$status
  severity <- if (!is.null(failure) || identical(status, "blocked")) "error" else
    if (identical(status, "needs_review")) "warning" else "info"
  title <- if (!is.null(failure)) "Run incomplete - failed" else switch(status,
    blocked = "Run incomplete - blocked", needs_review = "Run requires review",
    migration_ready = "Run migration-ready", validated = "Run validated", paste("Run status:", status))
  findings <- report$diagnostics$dependency_findings %||% list()
  blocking <- Filter(function(x) length(x$findings), findings)
  advisory <- Filter(function(x) length(x$advisory), findings)
  judgment <- human_judgment_findings(report$component_evidence %||% list())
  readiness <- report$diagnostics$readiness
  failures <- report$diagnostics$component_failures %||% list()
  bundles <- Filter(function(x) identical(x$kind, "bundle"), attempts$completed_attempts)
  latest <- if (length(bundles)) utils::tail(bundles, 1L)[[1L]] else NULL
  stopped <- if (!is.null(latest) && !isTRUE(latest$passed) && !isTRUE(latest$deferred)) latest$condition else NULL
  affected <- unique(unlist(lapply(c(blocking,
    Filter(function(x) isTRUE(x$blocks_execution), readiness$warnings), failures), `[[`, "affected"), use.names = FALSE))
  affected <- unique(c(affected, stopped$component_id))
  pending <- names(Filter(function(x) x$review_status %in% c("repair_required", "review_unavailable"),
    report$component_evidence %||% list()))
  details <- c(
    if (!is.null(failure$stage)) paste("Stopped during:", failure$stage),
    if (!is.null(stopped$message)) paste0("Bundle execution stopped in ", stopped$component_id %||% "unknown component",
      " (", latest$attempt_id, "): ", stopped$message),
    if (!is.null(stopped$message)) {
      missing <- missing_source_dataset(state, stopped$component_id, stopped)
      recorded <- recorded_dataset_output(latest, missing)
      if (length(recorded)) paste("The attempt recorded", paste(recorded, collapse = ", "),
        "but this reader could not find it. Check the effective library bindings; output contents remain unverified.")
    },
    if (!is.null(stopped$message)) paste("Bundle logs:", latest$stdout_path, latest$stderr_path),
    if (length(pending)) paste0("Separate outstanding static reviews (not proof these paths executed): ",
      paste(pending, collapse = ", ")),
    if (!is.null(report$diagnostics$pipeline)) pipeline_coverage_lines(report$diagnostics$pipeline),
    if (length(report$diagnostics$execution_deferred)) paste("Execution unavailable:",
      paste(report$diagnostics$execution_deferred, collapse = "; ")),
    readiness_warning_lines(readiness),
    vapply(names(blocking), function(id) paste0("Dependency findings for ", id, ": ",
      paste(blocking[[id]]$findings, collapse = ", ")), ""),
    vapply(names(advisory), function(id) paste0("Reported names without a scanned producer for ", id,
      " (not blocking execution): ", paste(advisory[[id]]$advisory, collapse = ", ")), ""),
    if (length(judgment)) paste0("Findings for human judgment (no automated repair addresses them): ",
      paste(judgment, collapse = "; ")),
    vapply(names(failures), function(id) paste0("Component could not finish: ", id, " (",
      failures[[id]]$phase, "): ", failures[[id]]$reason,
      if (!is.null(failures[[id]]$logs)) paste0("; logs: ", failures[[id]]$logs)), ""),
    if (length(affected)) paste0(length(affected), " components with unresolved findings: ", paste(affected, collapse = ", ")))

  # Attempt records are run-scoped. A prepared/interrupted or deferred attempt
  # must not be counted as executed, even when a prior bundle remains selected.
  incomplete <- sum(startsWith(attempts$incomplete_attempt_ids, "bundle_attempt_"))
  deferred <- sum(vapply(bundles, function(x) isTRUE(x$deferred), logical(1)))
  executed <- length(bundles) - deferred
  passed <- sum(vapply(bundles, function(x) !isTRUE(x$deferred) && isTRUE(x$passed), logical(1)))
  bundle_status <- if (executed > 0L) sprintf(
    "EXECUTED (%d attempt%s; %d ran to completion, %d failed; %d deferred, %d incomplete)",
    executed, if (executed == 1L) "" else "s", passed, executed - passed, deferred, incomplete) else if (incomplete > 0L)
      sprintf("INCOMPLETE (%d prepared attempts; execution completion unknown)", incomplete) else if (deferred > 0L)
        sprintf("NOT RUN (%d deferred attempt%s)", deferred, if (deferred == 1L) "" else "s") else "NOT RUN (0 attempts)"
  skipped <- if (!is.null(latest)) as.character(unlist(latest$deferred_component_ids %||% character())) else character()
  if (length(skipped)) bundle_status <- paste0(bundle_status, "; not executed: ", paste(skipped, collapse = ", "))
  if (isFALSE(state$execute)) bundle_status <- paste0(bundle_status, "; execution disabled")

  # A bundle fixer has an attempt_id; immediate component fixes do not. Count
  # invocations, not tool-loop HTTP requests or accepted patches. Resume can load
  # previous usage, so only this run's request records contribute.
  records <- Filter(function(x) identical(x$run_id, report$run_id) &&
    identical(x$record_type, "request_started") && identical(x$agent, "fixer"),
    state$usage_budget$records %||% state$usage$records %||% list())
  fixes <- function(bundle) {
    selected <- Filter(function(x) identical(!is.null(x$attempt_id), bundle), records)
    ids <- unique(vapply(selected, function(x) x$invocation_id %||% x$request_id, ""))
    if (length(ids)) sprintf("INVOKED (%d fixer invocation%s; see review results)",
      length(ids), if (length(ids) == 1L) "" else "s") else "NOT INVOKED"
  }
  parallel <- report$diagnostics$parallel %||% report$environment$parallel
  total <- length(state$schedule$component_id)
  generated <- length(intersect(state$schedule$component_id, names(state$selected_revisions)))
  stages <- c(
    if (!is.null(parallel)) c("Translation workers" = paste0(parallel$effective, " effective",
      if (!is.null(parallel$observed$peak_workers)) paste0("; peak observed ", parallel$observed$peak_workers))),
    "Translation" = sprintf("%d of %d components have saved code; %d could not finish", generated, total, length(failures)),
    "Component fixes" = fixes(FALSE), "Bundle execution" = bundle_status,
    "Bundle-level fixes" = fixes(TRUE),
    "Required validation" = if (severity == "error") {
      if (executed == 0L || !is.null(failure) || length(affected)) "NOT COMPLETED" else "NOT PASSED"
    } else if (status == "needs_review") "REVIEW REQUIRED" else "PASSED (see reference coverage)")
  next_action <- if (length(failures)) "Resolve the component failures and any dependency findings before rerunning." else
    if (!is.null(stopped$message)) "Repair the reported bundle failure and resolve outstanding review findings before rerunning." else
    if (length(affected)) "Resolve the dependency findings before rerunning." else
    if (severity == "error") "Resolve the reported error or failed checks before rerunning." else
      if (severity == "warning") "Review the outstanding evidence and reported findings before use." else
        "Inspect output and reference coverage before use."
  list(severity = severity, title = title, reason = failure$message %||% report$status_reason,
    details = details, stages = as.list(stages), affected_components = affected,
    bundle_logs = if (!is.null(stopped$message)) list(stdout = latest$stdout_path, stderr = latest$stderr_path) else list(),
    next_action = next_action)
}

migration_outcome_lines <- function(outcome) {
  c(paste0(toupper(outcome$severity), ": ", outcome$title), outcome$reason,
    outcome$details, paste(names(outcome$stages), unlist(outcome$stages), sep = ": "),
    paste("Next action:", outcome$next_action))
}

# Reviewer findings that no fixer round can address: claims about the SAS
# source itself, and context requests that only name components already in
# this run. They are listed so a person sees them without opening each review.
human_judgment_findings <- function(evidence) {
  clip <- function(x, n = 240L) if (nchar(x) > n) paste0(substr(x, 1L, n - 3L), "...") else x
  unlist(lapply(names(evidence), function(cid) {
    revisions <- evidence[[cid]]$revisions %||% list()
    active <- evidence[[cid]]$active_revision_id
    ids <- vapply(revisions, function(r) r$revision_id %||% "", "")
    if (!is.null(active) && active %in% ids) revisions <- revisions[ids == active]
    events <- unlist(lapply(revisions, function(r) r$events %||% list()), recursive = FALSE)
    reviews <- Filter(function(e) identical(e$type, "review_completed"), events %||% list())
    if (!length(reviews)) return(NULL)
    findings <- Filter(function(f) (f$repair_disposition %||% "") %in%
      c("source_syntax_claim_only", "context_available") || identical(f$category, "source_syntax_claim"),
      utils::tail(reviews, 1L)[[1L]]$findings %||% list())
    if (!length(findings)) return(NULL)
    paste0(cid, ": ", paste(vapply(findings, function(f) clip(as.character(
      f$sas_evidence %||% f$r_evidence %||% "(no evidence text)")[1L]), ""), collapse = " | "))
  }), use.names = FALSE)
}
