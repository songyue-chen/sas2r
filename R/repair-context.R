# Provider-facing repair context is projected from known execution records.
# Never serialize an attempt, contract, or repair packet wholesale.
repair_revision_note <- function(execution, component_id, revision_id) {
  if (is.null(execution)) return(NULL)
  if (!is.null(execution$diagnostic_execution)) execution <- execution$diagnostic_execution
  if (identical(execution$scope, "program_smoke")) {
    return(paste("Smoke execution", execution$execution_id %||% "unknown",
      "component", component_id, "snapshot file SHA-256",
      execution$code_hashes[[component_id]] %||% "unavailable",
      "; shown selected revision", revision_id,
      "; executed revision ID is not recorded. Do not infer a bundle revision binding."))
  }
  recorded <- execution$revision_manifest[[component_id]]$revision_id
  attempt_id <- execution$attempt_id %||% execution$execution_id %||% "unknown"
  if (!is.null(recorded) && identical(recorded, revision_id)) {
    return(paste("Attempt", attempt_id, "component", component_id, "revision", revision_id,
      "matches snapshot (not execution proof)."))
  }
  paste("Attempt", attempt_id,
    "component", component_id, "snapshot revision", recorded %||% "unavailable",
    "; shown selected revision", revision_id,
    if (is.null(recorded)) "; execution binding is unestablished." else
      "; differs from the attempt (not the executed revision).")
}

# Only the startup file at the executor-owned snapshot location is read here.
# General artifact browsing and paging are a later increment.
repair_startup_context <- function(bundle, helper_code, limit = 12000L) {
  execution <- bundle$diagnostic_execution %||% bundle
  smoke <- identical(execution$scope, "program_smoke")
  directory <- if (smoke) execution$attempt_dir else execution$bundle_dir
  scope <- paste(if (smoke) "Smoke execution" else "Bundle attempt",
    execution$execution_id %||% execution$attempt_id %||% "unknown")
  if (is.null(directory)) return(paste(scope, "startup snapshot location unavailable."))
  path <- file.path(directory, "autoexec.R")
  if (!file.exists(path)) return(paste(scope, "startup snapshot unavailable at", path,
    "; absence alone does not establish pruning."))
  startup <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
    error = function(e) NULL)
  if (is.null(startup)) return(paste(scope, "startup snapshot unreadable at", path))
  helper_path <- file.path(directory, "sas2r-helpers.R")
  executed_helpers <- if (file.exists(helper_path))
    tryCatch(paste(readLines(helper_path, warn = FALSE), collapse = "\n"), error = function(e) NULL) else NULL
  helper_note <- if (is.null(executed_helpers)) "Helper snapshot unavailable." else
    paste("Shown current helpers", if (identical(executed_helpers, helper_code)) "match" else "differ from",
      "the execution snapshot at", helper_path, ".")
  paste(scope, "startup snapshot:", path, helper_note,
    paste0("Startup characters 1-", min(nchar(startup), limit), " of ", nchar(startup),
      if (nchar(startup) > limit) "; truncated. No startup continuation tool is available in this increment." else "; complete."),
    substr(startup, 1L, limit), sep = "\n")
}

# Prior outcomes are claims, not repair authority. Unknown and reference-aware
# origins are excluded as a class, along with other components and prior runs.
repair_history_context <- function(outcomes, run_id, component_id, limit = 12000L) {
  if (is.null(run_id)) return("")
  eligible <- Filter(function(x) identical(x$run_id, run_id) &&
    identical(x$component_id, component_id) && isFALSE(x$reference_triggered_review), outcomes)
  if (!length(eligible)) return("")
  records <- lapply(eligible, function(x) list(
    run_id = x$run_id, component_id = x$component_id, attempt_id = x$attempt_id,
    execution_id = x$execution_id, revision_id = x$revision_id,
    prior_revision_id = x$prior_revision_id, outcome = x$outcome,
    evidence_ids = x$evidence_ids, diagnosis = x$diagnosis,
    remaining_uncertainty = x$remaining_uncertainty, rejection_reasons = x$rejection_reasons))
  text <- as.character(jsonlite::toJSON(records, auto_unbox = TRUE, null = "null"))
  paste("Prior same-component repair outcomes (unverified claims; verify against source and execution evidence):",
    substr(text, 1L, limit), if (nchar(text) > limit) "[History truncated; omitted claims are unknown.]" else "",
    sep = "\n")
}
