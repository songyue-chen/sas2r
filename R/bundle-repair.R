# Apply one bounded, evidence-grounded repair and refresh its review.
repair_bundle_component <- function(state, packet, attempt_rec, round) {
  attempt_rec <- packet$attempt %||% attempt_rec
  primary_cid <- packet$primary_component_id
  if (is.null(primary_cid) || !primary_cid %in% names(state$selected_revisions)) {
    stop_reason <- "no_causal_evidence"
    return(list(state = state, applied = FALSE, reason = stop_reason))
  }

  primary_rev <- state$selected_revisions[[primary_cid]]

  bundle_ev <- attempt_rec
  bundle_ev$bundle_id <- attempt_rec$attempt_id
  bundle_ev$execution_id <- attempt_rec$execution_id %||% attempt_rec$attempt_id
  bundle_ev$failing_outputs <- vapply(packet$failed_targets, function(t) t$target_key, character(1))


  signal_bundle_event(
    "bundle_fixer_invoked",
    attempt_id = attempt_rec$attempt_id,
    round = round + 1L,
    component_id = primary_cid
  )

  # 6. Invoke fixer
  fixed_rev <- tryCatch(
    fix_program_revision(
      revision = primary_rev,
      review = packet$review,
      checks = packet$checks,
      bundle = bundle_ev,
      mode = "bundle",
      llm = state$fixer_llm,
      usage = state$usage_budget,
      paths = state$paths,
      project = state$project,
      selected_revisions = state$selected_revisions,
      helper_code = runtime_helper_code(state$runtime),
      config = state$config,
      round = round + 1L,
      attempt_id = attempt_rec$attempt_id,
      evidence_ids = packet$evidence_ids
    ),
    error = function(e) {
      if (critical_translation_error(e)) stop(e)
      list(status = "repair_failed", message = conditionMessage(e))
    }
  )

  if (is.null(fixed_rev) || identical(fixed_rev$status, "repair_failed")) {
    stop_reason <- "repair_failed"
    return(list(state = state, applied = FALSE, reason = stop_reason))
  }

  if (!isTRUE(fixed_rev$checks$pass)) {
    state$diagnostics$rejected_repairs <- c(state$diagnostics$rejected_repairs, list(list(
      component_id = primary_cid, revision_id = fixed_rev$revision_id,
      r_path = fixed_rev$r_path, mechanical_retry = fixed_rev$mechanical_retry,
      candidate_review = "unreviewed", errors = fixed_rev$checks$errors)))
    return(list(state = state, applied = FALSE, reason = paste(
      "repair_mechanical_checks_failed", paste(fixed_rev$checks$errors, collapse = "; "), sep = ": ")))
  }

  # Check for identical / no-op patch
  is_identical_code <- identical(trimws(fixed_rev$r_code %||% ""), trimws(primary_rev$r_code %||% ""))
  is_identical_patch <- !is.null(fixed_rev$patch_hash) && !is.null(primary_rev$contract$patch_hash) && identical(fixed_rev$patch_hash, primary_rev$contract$patch_hash)
  has_helper_patch <- isTRUE(fixed_rev$helper_changed)

  if ((is_identical_code || is_identical_patch) && !has_helper_patch) {
    stop_reason <- "identical_patch"
    return(list(state = state, applied = FALSE, reason = stop_reason))
  }

  # Candidate files live with the revision; retained runtime files are never
  # overwritten. R list state is local, while the usage budget remains shared.
  retained <- state
  affected <- if (has_helper_patch) names(state$selected_revisions) else primary_cid
  state$selected_revisions[[primary_cid]] <- fixed_rev
  if (has_helper_patch) state <- stage_helper_candidate(state, fixed_rev)
  hp_dest <- fixed_rev$helper_path
  rejection <- tryCatch({
    refreshed <- review_helper_consumers(state, retained, affected, round + 1L,
      phase = "bundle", execution = bundle_ev)
    state <- refreshed$state
    refreshed$reasons
  }, error = function(e) {
    if (critical_translation_error(e)) stop(e)
    paste("candidate review unavailable:", conditionMessage(e))
  })
  if (length(rejection)) {
    rejected_revisions <- list()
    for (cid in affected) {
      h <- state$histories[[cid]]
      old <- retained$histories[[cid]]
      rejected_id <- h$active_revision_id
      if (identical(rejected_id, old$active_revision_id)) next
      candidate <- state$selected_revisions[[cid]]
      rejected_revisions[[cid]] <- list(evidence_revision_id = rejected_id,
        artifact_revision_id = candidate$revision_id, r_path = candidate$r_path,
        mechanical_retry = candidate$mechanical_retry, candidate_review = component_review_verdict(h))
      h$active_revision_id <- old$active_revision_id
      idx <- match(h$active_revision_id, vapply(h$revisions, `[[`, "", "revision_id"))
      h$revisions[[idx]]$events <- c(h$revisions[[idx]]$events, list(list(
        type = "repair_rejected", candidate_revision_id = rejected_id,
        candidate_artifact_revision_id = candidate$revision_id, reasons = rejection)))
      retained$histories[[cid]] <- h
    }
    retained$diagnostics$rejected_repairs <- c(retained$diagnostics$rejected_repairs,
      list(list(component_id = primary_cid, revision_id = fixed_rev$revision_id,
        revisions = rejected_revisions,
        r_path = fixed_rev$r_path, helper_path = if (has_helper_patch) hp_dest else NULL,
        errors = rejection)))
    signal_bundle_event("bundle_repair_rejected", component_id = primary_cid,
      attempt_id = attempt_rec$attempt_id, reason = paste(rejection, collapse = "; "))
    return(list(state = retained, applied = FALSE, reason = "repair_review_regressed"))
  }
  fixed_rev <- state$selected_revisions[[primary_cid]]

  # Record repair
  repair_rec <- list(
    round = round + 1L,
    component_id = fixed_rev$component_id %||% primary_cid,
    revision_id = fixed_rev$revision_id,
    diagnosis = fixed_rev$diagnosis,
    summary = fixed_rev$summary,
    patch_hash = fixed_rev$patch_hash,
    helper_patch = fixed_rev$bundle_helper_patch,
    mechanical_retry = fixed_rev$mechanical_retry,
    candidate_review = component_review_verdict(state$histories[[primary_cid]]),
    changed_interfaces = fixed_rev$changed_interfaces,
    affected_outputs = fixed_rev$affected_outputs,
    spend_usd = fixed_rev$spend_usd %||% 0
  )

  signal_bundle_event(
    "bundle_fixer_completed",
    attempt_id = attempt_rec$attempt_id,
    round = round + 1L,
    component_id = primary_cid,
    cost = fixed_rev$spend_usd
  )

  list(state = state, applied = TRUE, repair = repair_rec,
       helper_changed = has_helper_patch)
}

# Repair ceilings are counts, not timeouts or unlimited-loop switches.
bundle_repair_limit <- function(value, name, allow_null = FALSE) {
  if (is.null(value) && allow_null) return(NULL)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0 || value != floor(value) ||
      value > .Machine$integer.max) {
    cli::cli_abort("{.arg {name}} must be a non-negative finite integer",
                   class = "sas2r_invalid_argument")
  }
  as.integer(value)
}

# A failed authoritative run remains failed. Diagnose unvisited branches using
# existing smoke isolation, never the partial files/session from that run.
collect_bundle_diagnostics <- function(state, attempt) {
  failures <- attempt$mechanical_failures %||% list()
  if (!is.null(attempt$condition$component_id)) {
    failures[[attempt$condition$component_id]] <- attempt$condition
  }
  records <- list()
  blocked <- list()
  if (isTRUE(attempt$passed)) return(list(failures = failures, executions = records, blocked = blocked))
  pending <- setdiff(attempt$execution_order, c(attempt$executed_component_ids, names(failures)))
  for (cid in pending) {
    deps <- dependency_closure(state$graph, cid)
    blockers <- intersect(deps, names(failures))
    if (length(blockers)) {
      blocked[[cid]] <- blockers
      next
    }
    if (!usage_budget_allows_future(state$usage_budget)) break
    plan <- build_program_smoke_plan(state$graph, cid, state$selected_revisions)
    if (!identical(plan$status, "runnable")) {
      blocked[[cid]] <- plan$reason
      next
    }
    # The completed bundle attempt owns its diagnostics, independently of the
    # initial program-smoke attempt stored on the migration state.
    prepared <- prepare_program_smoke(state, plan, attempt$attempt_dir)
    execution <- run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir)
    records[[cid]] <- execution
    signal_bundle_event("bundle_diagnostic_completed", component_id = cid,
      attempt_id = attempt$attempt_id, passed = execution$passed,
      reason = execution$condition$message %||% execution$reason)
    if (!isTRUE(execution$passed) && !isTRUE(execution$deferred)) {
      failed <- execution$failed_component_id %||% cid
      condition <- execution$condition
      condition$component_id <- failed
      failures[[failed]] <- condition
    }
  }
  list(failures = failures, executions = records, blocked = blocked)
}

# Group findings by known failing component or the source-derived output writer.
# An absent downstream output after a crash is not an independent defect.
source_grounded_review_findings <- function(review) {
  Filter(function(f) {
    (f$severity %||% "") %in% c("material", "high") && nzchar(f$sas_evidence %||% "") &&
      nzchar(f$r_evidence %||% "") && !length(f$unresolved_dependencies)
  }, actionable_review_findings(review))
}

combine_repair_checks <- function(previous, current) {
  if (is.null(previous)) return(current)
  if (is.null(current)) return(previous)
  list(check_id = paste(unique(c(previous$check_id, current$check_id)), collapse = ", "),
    pass = FALSE, errors = unique(c(previous$errors, current$errors)))
}

bundle_repair_queue <- function(state, attempt, assessment, diagnostic,
                                previous_disposition = NULL) {
  failures <- diagnostic$failures
  target_lineage <- function(key) {
    assessment$lineage_by_target[[key]]$upstream_components %||%
      evidence_for_output_lineage(state$graph, state$histories, key)$upstream_components
  }
  packets <- list()
  for (cid in names(failures)) {
    if (!is.null(non_translation_runtime_reason(state, cid, failures[[cid]]))) next
    repair_cid <- cid
    artifact_checks <- NULL
    missing <- missing_source_dataset(state, cid, failures[[cid]])
    recorded <- recorded_dataset_output(attempt, missing)
    writers <- if (!is.null(missing)) source_output_writers(state, missing) else character()
    if (length(recorded)) {
      artifact_checks <- list(check_id = paste0("reader_binding:", missing), pass = FALSE,
        errors = paste("The attempt recorded", paste(recorded, collapse = ", "),
          "but", cid, "could not read", missing,
          ". Trace this reader's LIBNAME assignments against the supplied resolved library plan and executor search paths.",
          "Recorded presence does not establish correct contents; do not rewrite the producer solely because this lookup failed."))
    } else if (length(writers) == 1L && writers != cid &&
        writers %in% dependency_closure(state$graph, cid)) {
      # A reader's missing intermediate identifies a producer defect only when
      # that producer completed. Preserve the actual reader error and log paths.
      if (!writers %in% attempt$executed_component_ids) next
      repair_cid <- writers
      artifact_checks <- list(check_id = paste0("artifact:", missing), pass = FALSE,
        errors = paste("Source-declared output", missing, "was not available after", writers,
                       "completed; required by", cid))
    }
    relevant <- names(assessment$targets)[vapply(names(assessment$targets), function(key) {
      repair_cid %in% target_lineage(key)
    }, logical(1))]
    subset <- assessment
    subset$targets <- assessment$targets[relevant]
    failed_attempt <- attempt
    failed_attempt$condition <- failures[[cid]]
    # Prefer the diagnostic's own logs when its failure was found separately.
    matches <- Filter(function(x) identical(x$failed_component_id, cid), diagnostic$executions)
    if (length(matches)) {
      execution <- matches[[1L]]
      failed_attempt$stdout_path <- execution$stdout_path
      failed_attempt$stderr_path <- execution$stderr_path
      failed_attempt$execution_id <- execution$execution_id
    }
    packet <- build_bundle_repair_packet(state, failed_attempt, subset, previous_disposition)
    packet$primary_component_id <- repair_cid
    packet$attempt <- failed_attempt
    packet$checks <- artifact_checks
    packet$attributable_execution_failure <- TRUE
    if (is.null(packets[[repair_cid]])) packets[[repair_cid]] <- packet else
      packets[[repair_cid]]$checks <- combine_repair_checks(packets[[repair_cid]]$checks, artifact_checks)
  }
  # After a complete run, every failed target can contribute a repair candidate.
  # After a crash, only targets whose writer actually completed can do so.
  for (key in names(assessment$targets)) {
    target <- assessment$targets[[key]]
    if (isTRUE(target$passed) || identical(target$status, "unresolved_target")) next
    artifact_errors <- artifact_failure_checks(target)
    other_failed <- any(vapply(non_reference_checks(target), function(x) isFALSE(x$passed), logical(1)))
    if (!length(artifact_errors) && !other_failed &&
        !isFALSE(target$checks$reference_comparison$passed)) next
    lineage <- target_lineage(key)
    if (length(intersect(lineage, names(failures)))) next
    subset <- assessment
    subset$targets <- assessment$targets[key]
    clean_attempt <- attempt
    clean_attempt$condition <- NULL
    clean_attempt$passed <- TRUE
    packet <- build_bundle_repair_packet(state, clean_attempt, subset, previous_disposition)
    writers <- source_output_writers(state, target$target_key)
    writers <- intersect(writers, names(state$selected_revisions))
    cid <- if (length(writers) == 1L) writers[[1L]] else packet$primary_component_id
    if (is.null(cid) || !cid %in% names(state$selected_revisions)) next
    if (!isTRUE(attempt$passed) && !cid %in% attempt$executed_component_ids) next
    packet$primary_component_id <- cid
    observations <- attempt$candidate_input_observations[[cid]] %||% list()
    empty_input <- any(vapply(observations, function(x)
      identical(x$status, "observed_empty_candidate_input"), logical(1)))
    investigate <- length(artifact_errors) > 0L && empty_input
    packet$artifact_investigation <- investigate
    packet$source_review_only <- !length(artifact_errors) || investigate
    if (length(artifact_errors)) packet$checks <- list(
      check_id = paste0("artifact:", key), pass = FALSE,
      errors = vapply(artifact_errors, function(x) paste0(key, ": ", x$details), ""))
    if (investigate) {
      # Keep this issue local and visible, but never turn an observed dimension
      # into a repair instruction, including mixed independent code defects.
      packet$investigation_targets <- packet$failed_targets
      packet$checks <- NULL
      packet$failed_targets <- list()
    }
    packet$attempt <- clean_attempt
    if (is.null(packets[[cid]])) packets[[cid]] <- packet else {
      packets[[cid]]$source_review_only <- isTRUE(packets[[cid]]$source_review_only) && isTRUE(packet$source_review_only)
      packets[[cid]]$artifact_investigation <- isTRUE(packets[[cid]]$artifact_investigation) || investigate
      packets[[cid]]$investigation_targets <- c(packets[[cid]]$investigation_targets, packet$investigation_targets)
      packets[[cid]]$checks <- combine_repair_checks(packets[[cid]]$checks, packet$checks)
      packets[[cid]]$failed_targets <- c(packets[[cid]]$failed_targets, packet$failed_targets)
      packets[[cid]]$evidence_ids <- unique(c(packets[[cid]]$evidence_ids, packet$evidence_ids))
    }
  }
  # Static source/code findings remain actionable even when upstream output
  # comparisons are unresolved. Use only the active revision's saved evidence.
  for (cid in names(state$selected_revisions)) {
    history <- state$histories[[cid]]
    active <- current_component_evidence(history)
    reviews <- Filter(function(ev) identical(ev$type, "review_completed"), active$events %||% list())
    review <- if (length(reviews)) reviews[[length(reviews)]] else NULL
    pending_review <- identical(component_review_verdict(history), "repair_required") &&
      length(source_grounded_review_findings(review)) > 0L
    checks <- state$selected_revisions[[cid]]$checks
    pending_checks <- !is.null(checks) && !isTRUE(checks$pass)
    if (!pending_review && !pending_checks) next
    if (is.null(packets[[cid]])) {
      clean_attempt <- attempt
      clean_attempt$condition <- NULL
      clean_attempt$passed <- TRUE
      subset <- assessment
      subset$targets <- list()
      packets[[cid]] <- build_bundle_repair_packet(state, clean_attempt, subset, previous_disposition)
      packets[[cid]]$primary_component_id <- cid
      packets[[cid]]$attempt <- clean_attempt
    }
    packets[[cid]]$code_local <- pending_checks || (pending_review && length(source_grounded_review_findings(review)) > 0L)
    if (isTRUE(packets[[cid]]$code_local)) packets[[cid]]$source_review_only <- FALSE
    if (pending_review) {
      review$findings <- source_grounded_review_findings(review)
      review$review_id <- review$basis_id %||% paste0("review:", cid, ":", active$revision_id)
      packets[[cid]]$review <- review
      packets[[cid]]$evidence_ids <- unique(c(packets[[cid]]$evidence_ids, active$basis_ids))
    }
    if (pending_checks) packets[[cid]]$checks <- checks
  }
  scheduled <- state$schedule$component_id %||% names(state$selected_revisions)
  packets <- packets[intersect(scheduled, names(packets))]
  required_path_defect <- function(cid) {
    packet <- packets[[cid]]
    affected <- unique(unlist(lapply(source_grounded_review_findings(packet$review),
      function(f) f$affected_outputs %||% character()), use.names = FALSE))
    mechanical <- !is.null(state$selected_revisions[[cid]]$checks) &&
      !isTRUE(state$selected_revisions[[cid]]$checks$pass)
    any(vapply(names(assessment$targets), function(key) {
      isTRUE(assessment$targets[[key]]$required) && cid %in% target_lineage(key) &&
        (mechanical || key %in% affected)
    }, logical(1)))
  }
  priority <- vapply(names(packets), function(cid) {
    p <- packets[[cid]]
    if (isTRUE(p$source_review_only)) return(4L)
    if (isTRUE(p$attributable_execution_failure)) return(0L)
    if (isTRUE(p$code_local) && required_path_defect(cid)) {
      runnability <- p$review$review_record$static_runnability %||% p$review$static_runnability
      if (identical(runnability, "known_blocker")) return(1L)
      return(2L)
    }
    3L
  }, integer(1))
  packets[order(priority, seq_along(packets))]
}
