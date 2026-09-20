# Local component checks are shared by initial work and context-only revisits.
check_component_revision <- function(state, component_id) {
  rev <- state$selected_revisions[[component_id]]
  rev_id <- rev$revision_id
  registry_p <- if (!is.null(state$runtime)) state$runtime$registry else NULL
  checks <- check_program_revision(rev$r_path, contract = rev$contract, registry = registry_p,
    helper_patch = candidate_helper_patch(rev, state$runtime), allowlist = state$config$allowlist)
  checks$check_id <- paste0("check_", substr(migration_hash(list(
    component_id, rev_id, rev$r_code, checks)), 1L, 16L))

  if (isTRUE(checks$pass)) {
    state$events <- c(state$events, paste0("mechanical_pass:", rev_id))
    signal_immediate_coordinator_event("mechanical_pass", component_id, rev_id)
    rev$status <- "ok"
  } else {
    state$events <- c(state$events, paste0("mechanical_fail:", rev_id))
    signal_immediate_coordinator_event("mechanical_fail", component_id, rev_id,
                                       reason = paste(checks$errors, collapse = "; "))
    rev$status <- "check_failed"
  }
  rev$checks <- checks
  state$selected_revisions[[component_id]] <- rev
  state$histories[[component_id]] <- record_program_checks(
    state$histories[[component_id]], checks
  )

  state
}

review_component_revision <- function(state, component_id, round = 0L, reuse_only = FALSE, initial_review = NULL) {
  rev <- state$selected_revisions[[component_id]]
  rev_id <- rev$revision_id
  checks <- rev$checks
  ctx <- list(
    component_id = component_id,
    revision_id = rev_id,
    r_code = rev$r_code,
    r_path = rev$r_path,
    contract = rev$contract,
    binding = rev$binding %||% rev$contract$binding,
    history = state$histories[[component_id]],
    helper_code = runtime_helper_code(state$runtime),
    sas_source = component_source_text(state$graph, component_id),
    project = state$project,
    selected_revisions = state$selected_revisions,
    config = state$config %||% list()
  )

  args <- list(revision = rev, context = ctx, llm = state$reviewer_llm,
    usage = state$usage_budget, paths = state$paths, round = round,
    history = state$histories[[component_id]])
  cached <- if (isTRUE(checks$pass)) do.call(review_program_revision,
    c(args, list(reuse_only = TRUE))) else NULL
  if (isTRUE(reuse_only)) return(cached)
  if (is.null(cached) && !is.null(initial_review$review_key) &&
      identical(initial_review$review_key, do.call(review_program_revision, c(args, list(identity_only = TRUE))))) {
    cached <- initial_review
    cached$reused <- TRUE
    cached$spend_usd <- 0
  }
  reason <- if (!isTRUE(checks$pass)) "mechanical_checks_failed; repair before semantic review" else
    if (!usage_budget_allows_future(state$usage_budget)) "budget_exhausted" else NULL
  review <- if (!is.null(cached)) cached else if (!is.null(reason)) {
    list(verdict = "review_unavailable", reason = reason,
      history = record_review_unavailable(state$histories[[component_id]], reason))
  } else tryCatch(do.call(review_program_revision, args), error = function(e) {
    if (inherits(e, "sas2r_llm_settings_error")) stop(e)
    list(verdict = "review_unavailable", reason = conditionMessage(e),
      history = record_review_unavailable(state$histories[[component_id]], conditionMessage(e)))
  })

  if (!is.null(review$history)) {
    state$histories[[component_id]] <- review$history
  }
  if (identical(review$verdict, "review_unavailable")) {
    state$events <- c(state$events, paste0("review_unavailable:", rev_id))
  } else {
    state$events <- c(state$events, paste0("reviewed:", rev_id))
  }
  signal_immediate_coordinator_event(
    if (identical(review$verdict, "review_unavailable")) "review_unavailable"
    else if (isTRUE(review$reused)) "review_reused" else "program_reviewed",
    component_id, rev_id, reason = review$reason %||% review$verdict
  )

  list(state = state, review = review)
}

# Promote only after both kinds of evidence are available on the active binding.
promote_reviewed_smoke <- function(state, component_id) {
  smoke <- state$selected_revisions[[component_id]]$smoke
  ev <- current_component_evidence(state$histories[[component_id]])
  if (isTRUE(smoke$passed) && identical(ev$level, "reviewed_only") && !length(ev$blockers)) {
    state$histories[[component_id]] <- promote_component_evidence(
      state$histories[[component_id]], "runtime_verified", coverage = paste0("call:", component_id),
      basis_id = smoke$execution_id)
  }
  state
}

smoke_component_revision <- function(state, component_id, execute = TRUE) {
  state <- record_dependency_finding(state, component_id, parallel_dependency_findings(state, component_id))
  rev <- state$selected_revisions[[component_id]]
  unavailable <- component_execution_reasons(state, component_id)
  plan <- if (!isTRUE(rev$checks$pass)) {
    list(status = "deferred", reason = "mechanical_checks_failed")
  } else if (isTRUE(execute) && length(unavailable)) {
    list(status = "deferred", reason = paste("Dependencies unavailable:", paste(unavailable, collapse = "; ")))
  } else build_program_smoke_plan(state$graph, component_id,
    state$selected_revisions, execute = execute)
  plan$population_specs <- source_population_specs(state$project, c(plan$dependency_prefix, component_id))
  ids <- c(plan$dependency_prefix, component_id)
  inputs <- input_hash_manifest(state$project)
  formats <- state$runtime$formats %||% file.path(dirname(state$runtime$helpers), "_sas2r_formats.R")
  context_key <- migration_hash(list(
    plan = plan[setdiff(names(plan), "selected_revisions")],
    programs = lapply(state$selected_revisions[ids], revision_code),
    helper = runtime_helper_code(state$runtime),
    inputs = inputs,
    libraries = state$project$config$libraries,
    formats = if (file.exists(formats)) readLines(formats, warn = FALSE) else NULL,
    R = as.character(getRversion())))
  if (identical(rev$smoke$context_key, context_key) &&
      (isTRUE(rev$smoke$passed) || isTRUE(rev$smoke$deferred))) {
    ev <- current_component_evidence(state$histories[[component_id]])
    recorded <- any(vapply(ev$events, function(e) identical(e$type, "program_smoke") &&
      identical(e$execution_id, rev$smoke$execution_id), logical(1)))
    if (isTRUE(rev$smoke$passed) && !recorded) {
      state$histories[[component_id]] <- record_program_smoke(state$histories[[component_id]], rev$smoke)
    } else if (isTRUE(rev$smoke$deferred) && is.null(ev$runtime_deferred)) {
      state$histories[[component_id]] <- record_runtime_deferred(state$histories[[component_id]], rev$smoke$reason)
    }
    signal_program_smoke_event("program_smoke_reused", component_id,
      execution_id = rev$smoke$execution_id, reason = rev$smoke$reason)
    return(promote_reviewed_smoke(state, component_id))
  }
  if (identical(plan$status, "deferred")) {
    result <- list(passed = FALSE, deferred = TRUE, reason = plan$reason, waiting_on = plan$waiting_on)
    state$histories[[component_id]] <- record_runtime_deferred(state$histories[[component_id]], plan$reason)
    state$events <- c(state$events, paste0("smoke_deferred:", rev$revision_id))
    signal_program_smoke_event("program_smoke_deferred", component_id, reason = plan$reason)
  } else {
    dir <- state[["attempt"]]$attempt_dir %||%
      if (!is.null(state$paths$smoke_tests)) file.path(state$paths$smoke_tests, "smoke_attempt_001") else tempdir()
    smoke_state <- state
    smoke_state$input_manifest <- inputs
    prepared <- prepare_program_smoke(smoke_state, plan, dir)
    result <- run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir)
    state$histories[[component_id]] <- record_program_smoke(state$histories[[component_id]], result)
    event <- if (isTRUE(result$passed)) "smoke_passed:" else
      if (!is.null(result$blocked_by)) "smoke_blocked:" else "smoke_failed:"
    state$events <- c(state$events, paste0(event, rev$revision_id))
  }
  result$context_key <- context_key
  state$selected_revisions[[component_id]]$smoke <- result
  promote_reviewed_smoke(state, component_id)
}

# The scheduler uses content identity, not repair round or artifact revision ID,
# to distinguish actual component edits from shared-context revisits.
component_content_identity <- function(revision) {
  if (is.null(revision)) return(NULL)
  contract <- revision$contract
  contract$binding <- NULL
  migration_hash(list(code = revision_code(revision), contract = contract))
}

revisit_component_runtime <- function(state, component_id, execute = TRUE) {
  state <- refresh_component_runtime_binding(state, component_id)
  state <- check_component_revision(state, component_id)
  if (is.null(review_component_revision(state, component_id, reuse_only = TRUE))) {
    signal_immediate_coordinator_event("review_pending", component_id,
      reason = "scheduled before bundle execution")
  }
  smoke_component_revision(state, component_id, execute)
}

# No mutations to code/helpers and no fixer calls during this sweep. The common
# reviewer entry point decides reuse from its complete request identity/history.
finalize_component_reviews <- function(state) {
  ids <- intersect(state$schedule$component_id %||% names(state$selected_revisions),
    names(state$selected_revisions))
  ids <- setdiff(ids, names(state$diagnostics$component_failures))
  signal_immediate_coordinator_event("component_review_checkpoint_started", "all components")
  reused <- completed <- unavailable <- 0L
  for (cid in ids) {
    cached <- review_component_revision(state, cid, reuse_only = TRUE)
    result <- if (!is.null(cached)) list(state = state, review = cached) else
      review_component_revision(state, cid, state$repair_counts[[cid]] %||% 0L)
    state <- promote_reviewed_smoke(result$state, cid)
    if (isTRUE(result$review$reused)) reused <- reused + 1L else
      if (identical(result$review$verdict, "review_unavailable")) unavailable <- unavailable + 1L else
        completed <- completed + 1L
    if (!is.null(state$resume_fingerprint)) write_migration_checkpoint(state, state$resume_fingerprint)
  }
  signal_immediate_coordinator_event("component_review_checkpoint_completed", "all components",
    reason = sprintf("%d reviewed, %d reused, %d unavailable", completed, reused, unavailable))
  state
}
