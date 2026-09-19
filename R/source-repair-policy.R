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
  # Actual reviewer/model settings are keyed separately. Request transport and
  # spending limits do not change the source facts supplied to a reviewer.
  config$llm <- NULL
  config$budget <- NULL
  config$usage_limits <- NULL
  config$migration$max_parallel_translations <- NULL
  if (!length(config$migration)) config$migration <- NULL
  if (is.list(config$raw)) {
    config$raw$migration$max_parallel_translations <- NULL
    if (!length(config$raw$migration)) config$raw$migration <- NULL
  }
  if (is.list(config$outputs)) {
    config$outputs$references <- NULL
    if (!length(config$outputs)) config$outputs <- NULL
  }
  if (length(config)) config else list()
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
    if ((!isTRUE(queue[[cid]]$source_review_only) && !isTRUE(queue[[cid]]$artifact_investigation)) ||
        !usage_budget_allows_future(state$usage_budget)) next
    ancestors <- dependency_closure(state$graph, cid)
    if (any(vapply(queue[intersect(ancestors, names(queue))],
        function(x) !isTRUE(x$source_review_only), logical(1)))) next
    rev <- state$selected_revisions[[cid]]
    history <- state$histories[[cid]]
    old <- current_component_evidence(history)
    if (identical(component_review_verdict(history), "repair_required") && !isTRUE(queue[[cid]]$artifact_investigation)) next
    full_review <- identical(component_review_verdict(history), "review_unavailable")
    targets <- sort(unique(vapply(c(queue[[cid]]$failed_targets, queue[[cid]]$investigation_targets), `[[`, "", "target_key")))
    key <- migration_hash(list(
      code = rev$r_code, binding = old$binding,
      guidance = build_agent_guidance(state$project, cid, rev$contract,
        state$selected_revisions, state$graph, config = state$config,
        include_consumers = TRUE,
        priority_dependencies = review_context_dependencies(history))$identity,
      dependencies = lapply(state$selected_revisions[ancestors], revision_code),
      inputs = state$input_manifest %||% input_hash_manifest(state$project),
      config = source_review_config(state$config),
      phase = "bundle", focus = targets, scope = if (full_review) "full" else "focused",
      helper = runtime_helper_code(state$runtime),
      helper_reference = helper_reference(), rulebook = load_rulebook(),
      reviewer = state$reviewer_llm[c("provider", "model", "model_parameters", "endpoint", "api_version")],
      worker = worker_binding_hash("reviewer", skills = agent_skill_catalog(),
        project_dir = state$project$project_dir, schema = "program_review_v1")))
    events <- unlist(lapply(history$revisions, `[[`, "events"), recursive = FALSE)
    if (any(vapply(events, function(x) identical(x$type, "source_mismatch_review") &&
        identical(x$context_key, key) && (!full_review || identical(x$review_scope, "full")), logical(1)))) next
    review <- tryCatch(review_program_revision(rev, context = list(
      sas_source = component_source_text(state$graph, cid), project = state$project,
      config = state$config, selected_revisions = state$selected_revisions, focus_outputs = targets,
      helper_code = runtime_helper_code(state$runtime), full_review = full_review,
      phase = "bundle", source_input_identity = state$input_manifest %||% input_hash_manifest(state$project)), llm = state$reviewer_llm,
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
    current_guidance <- build_agent_guidance(state$project, cid, rev$contract,
      state$selected_revisions, state$graph, config = state$config,
        include_consumers = TRUE,
        priority_dependencies = review_context_dependencies(history))$identity
    recovered <- full_review && identical(review$review_scope, "full") &&
      identical(review$verdict, "reviewed_no_material_finding") &&
      identical(review$binding_hash, old$binding$binding_hash) &&
      identical(review$context_identity, current_guidance)
    h <- if (actionable || recovered) review$history else history
    idx <- match(h$active_revision_id, vapply(h$revisions, `[[`, "", "revision_id"))
    h$revisions[[idx]]$events <- c(h$revisions[[idx]]$events, list(list(
      type = "source_mismatch_review", context_key = key, target_keys = targets,
      basis_id = review$review_id, verdict = review$verdict, reason = review$reason,
      review_scope = review$review_scope %||% "focused", adopted = isTRUE(actionable) || recovered)))
    state$histories[[cid]] <- h
    signal_bundle_event("bundle_source_review_completed", component_id = cid,
      attempt_id = attempt$attempt_id, reason = review$verdict)
  }
  state
}

# Local diagnostic only. Read only already inventoried, source-named generated
# intermediates from this attempt. No input/reference fallback or directory scan.
observe_candidate_inputs <- function(state, attempt, limits = output_evidence_limits(),
                                     components = names(state$selected_revisions)) {
  cache <- list()
  observations <- list()
  for (cid in intersect(components, names(state$selected_revisions))) {
    statements <- component_statements(state$project, cid)
    lineage <- state$project$lineage
    reads <- unique(lineage$dataset[lineage$role == "reads" & lineage$unit_id %in% statements$unit_id])
    reads <- reads[vapply(reads, function(key) length(source_output_writers(state, key)) > 0L, logical(1))]
    for (key in reads) {
      if (is.null(cache[[key]])) {
        result <- list(dataset = key, status = "unknown", reason = "candidate evidence unavailable")
        bits <- split_ds(key)
        paths <- paste0(bits[["lib"]], "/", bits[["member"]], ".", OUTPUT_CANDIDATE_FORMATS)
        rel <- intersect(paths, names(attempt$output_hashes))
        if (length(rel) && length(cache) < limits$max_files_per_root) {
          result <- tryCatch({
            path <- confine_evidence_path(file.path(attempt$attempt_dir, rel[1L]), attempt$attempt_dir)
            value <- read_evidence_dataset(path, tools::file_ext(path), limits)
            list(dataset = key, candidate_path = path, nrow = nrow(value),
              status = if (nrow(value) == 0L) "observed_empty_candidate_input" else "observed_nonempty_candidate_input")
          }, error = function(e) list(dataset = key, status = "unknown", reason = conditionMessage(e)))
        }
        cache[[key]] <- result
      }
      observations[[cid]][[key]] <- cache[[key]]
    }
  }
  observations
}
