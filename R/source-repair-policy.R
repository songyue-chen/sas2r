# Reference comparisons are observations, never repair instructions or a score
# for choosing code. Keep their complete records in local assessments.
non_reference_checks <- function(target) {
  checks <- target$checks %||% list()
  checks[!startsWith(names(checks), "reference_")]
}

artifact_failure_checks <- function(target) {
  checks <- target$checks %||% list()
  allowed <- c("candidate_exists", "candidate_readable", "file_nonempty",
    "header_valid", "trailer_valid", "envelope_valid", "structure_valid",
    "signature_valid", "dimensions_valid")
  Filter(function(x) isFALSE(x$passed), checks[intersect(names(checks), allowed)])
}

non_translation_runtime_reason <- function(state, cid, condition) {
  if (any(grepl("timeout", condition$class %||% character(), fixed = TRUE)))
    return("execution_timeout; no translation defect established")
  message <- condition$message %||% ""
  if (startsWith(message, "Dataset not found: ")) {
    dataset <- tolower(sub("\n.*", "", substring(message, nchar("Dataset not found: ") + 1L)))
    statements <- component_statements(state$project, cid)
    lineage <- state$project$lineage
    reads <- lineage$dataset[lineage$role == "reads" & lineage$unit_id %in% statements$unit_id]
    if (dataset %in% tolower(reads)) return(paste("source_input_unavailable:", dataset))
  }
  NULL
}

passed_output_checks <- function(assessment) {
  unlist(lapply(names(assessment$targets), function(key) {
    target <- assessment$targets[[key]]
    if (!isTRUE(target$required)) return(character())
    checks <- non_reference_checks(target)
    passed <- names(Filter(function(x) isTRUE(x$passed), checks))
    if (length(passed)) paste(key, passed, sep = ":") else character()
  }), use.names = FALSE) %||% character()
}

passed_population_checks <- function(execution) {
  checks <- execution$population_checks %||% list()
  unlist(lapply(names(checks), function(cid) {
    vapply(Filter(function(x) identical(x$status, "passed"), checks[[cid]]),
      function(x) paste(cid, x$unit_id, paste(x$outputs, collapse = ","), sep = ":"), "")
  }), use.names = FALSE) %||% character()
}

source_review_regressed <- function(previous, candidate) {
  identical(previous, "reviewed_no_material_finding") &&
    !identical(candidate, "reviewed_no_material_finding")
}

source_history_regressions <- function(previous, candidate) {
  reasons <- character()
  for (cid in names(previous)) {
    old <- current_component_evidence(previous[[cid]])
    new <- current_component_evidence(candidate[[cid]])
    # Different source is a new translation task, not a candidate repair of it.
    if (!is.null(old$binding$source_hash) && !is.null(new$binding$source_hash) &&
        !identical(old$binding$source_hash, new$binding$source_hash)) next
    if (!identical(old$binding, new$binding) &&
        source_review_regressed(component_review_verdict(previous[[cid]]),
                               component_review_verdict(candidate[[cid]]))) {
      reasons <- c(reasons, paste(cid, "review regressed"))
    }
    if (isTRUE(old$mechanical_check$pass) && !isTRUE(new$mechanical_check$pass)) {
      reasons <- c(reasons, paste(cid, "mechanical checks regressed"))
    }
  }
  reasons
}

# Reference-only configuration cannot invalidate source review. Source inputs
# are still hashed independently, including files also used as references.
source_review_config <- function(config) {
  config$comparison_rules <- NULL
  if (is.list(config$outputs)) config$outputs$references <- NULL
  config
}

source_evidence_summary <- function(history, population) {
  evidence <- current_component_evidence(history)
  statuses <- vapply(population %||% list(), function(x) x$status %||% "unverified", "")
  list(review_verdict = component_review_verdict(history),
    basis_ids = evidence$basis_ids %||% character(),
    source_checks = list(passed = sum(statuses == "passed"), failed = sum(statuses == "failed"),
                         unverified = sum(!statuses %in% c("passed", "failed"))))
}

# One grouped investigation for unchanged source/code/input context. This uses
# the existing review history and survives checkpoints without a second ledger.
review_bundle_mismatches <- function(state, attempt, assessment, round) {
  if (!isTRUE(attempt$passed) || is.null(state$reviewer_llm)) return(state)
  queue <- bundle_repair_queue(state, attempt, assessment, list(failures = list()))
  for (cid in names(queue)) {
    if (!isTRUE(queue[[cid]]$source_review_only) ||
        !usage_budget_allows_future(state$usage_budget)) next
    ancestors <- dependency_closure(state$graph, cid)
    if (any(vapply(queue[intersect(ancestors, names(queue))],
        function(x) !isTRUE(x$source_review_only), logical(1)))) next
    rev <- state$selected_revisions[[cid]]
    history <- state$histories[[cid]]
    old <- current_component_evidence(history)
    if (identical(component_review_verdict(history), "repair_required")) next
    key <- migration_hash(list(
      code = rev$r_code, binding = old$binding,
      dependencies = lapply(state$selected_revisions[ancestors], revision_code),
      inputs = state$input_manifest %||% input_hash_manifest(state$project),
      config = source_review_config(state$config),
      reviewer = state$reviewer_llm[c("provider", "model", "model_parameters")],
      worker = worker_binding_hash("reviewer", skills = agent_skill_catalog(),
        project_dir = state$project$project_dir)))
    events <- unlist(lapply(history$revisions, `[[`, "events"), recursive = FALSE)
    if (any(vapply(events, function(x) identical(x$type, "source_mismatch_review") &&
        identical(x$context_key, key), logical(1)))) next
    targets <- unique(vapply(queue[[cid]]$failed_targets, `[[`, "", "target_key"))
    review <- tryCatch(review_program_revision(rev, context = list(
      sas_source = component_source_text(state$graph, cid), project = state$project,
      config = state$config, focus_outputs = targets), llm = state$reviewer_llm,
      usage = state$usage_budget, paths = state$paths, history = history,
      round = round, attempt_id = attempt$attempt_id), error = function(e) {
        if (inherits(e, "sas2r_llm_settings_error")) stop(e)
        list(verdict = "review_unavailable", reason = conditionMessage(e),
          history = record_review_unavailable(history, conditionMessage(e)))
      })
    h <- review$history
    idx <- match(h$active_revision_id, vapply(h$revisions, `[[`, "", "revision_id"))
    # The exact unchanged binding still owns its prior runtime observations.
    if (identical(review$verdict, "reviewed_no_material_finding") &&
        !length(old$blockers) && !is.null(old$level)) {
      h$revisions[[idx]]$level <- old$level
    }
    h$revisions[[idx]]$events <- c(h$revisions[[idx]]$events, list(list(
      type = "source_mismatch_review", context_key = key, target_keys = targets,
      basis_id = review$review_id, verdict = review$verdict)))
    state$histories[[cid]] <- h
    signal_bundle_event("bundle_source_review_completed", component_id = cid,
      attempt_id = attempt$attempt_id, reason = review$verdict)
  }
  state
}
