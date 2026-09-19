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
  affected <- unique(c(report$diagnostics$parallel_deferred,
    unlist(lapply(findings, `[[`, "affected"), use.names = FALSE)))
  details <- c(
    if (!is.null(failure$stage)) paste("Stopped during:", failure$stage),
    if (!is.null(report$diagnostics$pipeline)) pipeline_coverage_lines(report$diagnostics$pipeline),
    if (length(report$diagnostics$parallel_deferred)) "Blocked before: bundle execution",
    vapply(names(findings), function(id) paste0("Dependency findings for ", id, ": ",
      paste(findings[[id]]$findings, collapse = ", ")), ""),
    if (length(affected)) paste0(length(affected), " components deferred: ", paste(affected, collapse = ", ")))

  # Attempt records are run-scoped. A prepared/interrupted or deferred attempt
  # must not be counted as executed, even when a prior bundle remains selected.
  bundles <- Filter(function(x) identical(x$kind, "bundle"), attempts$completed_attempts)
  incomplete <- sum(startsWith(attempts$incomplete_attempt_ids, "bundle_attempt_"))
  deferred <- sum(vapply(bundles, function(x) isTRUE(x$deferred), logical(1)))
  executed <- length(bundles) - deferred
  passed <- sum(vapply(bundles, function(x) !isTRUE(x$deferred) && isTRUE(x$passed), logical(1)))
  bundle_status <- if (executed > 0L) sprintf(
    "EXECUTED (%d attempt%s; %d ran to completion, %d failed; %d deferred, %d incomplete)",
    executed, if (executed == 1L) "" else "s", passed, executed - passed, deferred, incomplete) else if (incomplete > 0L)
      sprintf("INCOMPLETE (%d prepared attempts; execution completion unknown)", incomplete) else if (deferred > 0L)
        sprintf("NOT RUN (%d deferred attempt%s)", deferred, if (deferred == 1L) "" else "s") else "NOT RUN (0 attempts)"
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
  stages <- c(
    if (!is.null(parallel)) c("Translation workers" = paste0(parallel$effective, " effective",
      if (!is.null(parallel$observed$peak_workers)) paste0("; peak observed ", parallel$observed$peak_workers))),
    "Component fixes" = fixes(FALSE), "Bundle execution" = bundle_status,
    "Bundle-level fixes" = fixes(TRUE),
    "Required validation" = if (severity == "error") {
      if (executed == 0L || !is.null(failure) || length(affected)) "NOT COMPLETED" else "NOT PASSED"
    } else if (status == "needs_review") "REVIEW REQUIRED" else "PASSED (see reference coverage)")
  next_action <- if (length(affected)) "Resolve the dependency findings before rerunning." else
    if (severity == "error") "Resolve the reported error or failed checks before rerunning." else
      if (severity == "warning") "Review the outstanding evidence and reported findings before use." else
        "Inspect output and reference coverage before use."
  list(severity = severity, title = title, reason = failure$message %||% report$status_reason,
    details = details, stages = as.list(stages), affected_components = affected,
    next_action = next_action)
}

migration_outcome_lines <- function(outcome) {
  c(paste0(toupper(outcome$severity), ": ", outcome$title), outcome$reason,
    outcome$details, paste(names(outcome$stages), unlist(outcome$stages), sep = ": "),
    paste("Next action:", outcome$next_action))
}
