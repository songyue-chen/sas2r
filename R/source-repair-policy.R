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

source_output_writers <- function(state, key) {
  edges <- state$graph$edges
  from <- edges$from[edges$type %in% c("writes_dataset", "writes_output") &
    edges$detail == key & edges$resolution == "resolved"]
  unique(stats::na.omit(state$graph$nodes$component_id[match(from, state$graph$nodes$node_id)]))
}

missing_source_dataset <- function(state, cid, condition) {
  message <- condition$message %||% ""
  if (startsWith(message, "Dataset not found: ")) {
    dataset <- tolower(sub("\n.*", "", substring(message, nchar("Dataset not found: ") + 1L)))
    statements <- component_statements(state$project, cid)
    lineage <- state$project$lineage
    reads <- lineage$dataset[lineage$role == "reads" & lineage$unit_id %in% statements$unit_id]
    if (dataset %in% tolower(reads)) return(dataset)
  }
  NULL
}

non_translation_runtime_reason <- function(state, cid, condition) {
  if (any(grepl("timeout", condition$class %||% character(), fixed = TRUE)))
    return("execution_timeout; no translation defect established")
  dataset <- missing_source_dataset(state, cid, condition)
  if (!is.null(dataset) && !length(source_output_writers(state, dataset)))
    return(paste("source_input_unavailable:", dataset))
  NULL
}

passed_output_checks <- function(assessment, targets = names(assessment$targets)) {
  unlist(lapply(targets, function(key) {
    target <- assessment$targets[[key]]
    if (!isTRUE(target$required)) return(character())
    checks <- non_reference_checks(target)
    passed <- names(Filter(function(x) isTRUE(x$passed), checks))
    if (length(passed)) paste(key, passed, sep = ":") else character()
  }), use.names = FALSE) %||% character()
}

passed_population_checks <- function(execution, components = names(execution$population_checks)) {
  checks <- execution$population_checks %||% list()
  unlist(lapply(intersect(components, names(checks)), function(cid) {
    vapply(Filter(function(x) identical(x$status, "passed"), checks[[cid]]),
      function(x) paste(cid, paste(x$outputs, collapse = ","), sep = ":"), "")
  }), use.names = FALSE) %||% character()
}

unchanged_source_components <- function(previous, candidate) {
  common <- intersect(names(previous), names(candidate))
  common[vapply(common, function(cid) {
    old <- current_component_evidence(previous[[cid]])$binding$source_hash
    new <- current_component_evidence(candidate[[cid]])$binding$source_hash
    !is.null(old) && identical(old, new)
  }, logical(1))]
}

# A target's old checks apply only while its source lineage is still the same.
comparable_output_targets <- function(previous, candidate, components) {
  common <- intersect(names(previous$targets), names(candidate$targets))
  common[vapply(common, function(key) {
    old <- previous$lineage_by_target[[key]]$upstream_components %||% character()
    new <- candidate$lineage_by_target[[key]]$upstream_components %||% character()
    # Lineage also lists dataset nodes, which do not own source-review records.
    owners <- union(intersect(old, names(previous$evidence_histories)),
                    intersect(new, names(candidate$evidence_histories)))
    setequal(old, new) && all(owners %in% components)
  }, logical(1))]
}

source_review_regressed <- function(previous, candidate) {
  identical(previous, "reviewed_no_material_finding") &&
    !identical(candidate, "reviewed_no_material_finding")
}

source_history_regressions <- function(previous, candidate) {
  reasons <- character()
  for (cid in unchanged_source_components(previous, candidate)) {
    old <- current_component_evidence(previous[[cid]])
    new <- current_component_evidence(candidate[[cid]])
    if (!identical(old$binding$binding_hash, new$binding$binding_hash) &&
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
    # An extra review without an actionable finding is not evidence against a
    # completed review of unchanged code. Keep its outcome as an observation.
    actionable <- identical(review$verdict, "repair_required") &&
      length(source_grounded_review_findings(review)) > 0L
    h <- if (actionable) review$history else history
    idx <- match(h$active_revision_id, vapply(h$revisions, `[[`, "", "revision_id"))
    h$revisions[[idx]]$events <- c(h$revisions[[idx]]$events, list(list(
      type = "source_mismatch_review", context_key = key, target_keys = targets,
      basis_id = review$review_id, verdict = review$verdict, reason = review$reason)))
    state$histories[[cid]] <- h
    signal_bundle_event("bundle_source_review_completed", component_id = cid,
      attempt_id = attempt$attempt_id, reason = review$verdict)
  }
  state
}
