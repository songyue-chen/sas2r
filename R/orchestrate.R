#' Dependency-Aware SAS-to-R Migration Orchestrator
#'
#' Coordinates dependency-ordered program translation, immediate parse/lint/interface
#' checks, independent review, meaningful program smoke execution, evidence-grounded
#' immediate repair, and targeted graph-driven revisit.

#' Initialize a new migration state
#'
#' @param project A `sas2r_project` object or directory path.
#' @param out_dir Output directory path.
#' @param llm Optional `sas2r_llm` instance.
#' @param config Project configuration list.
#' @param execute Logical indicating if execution is enabled (default TRUE).
#' @param max_program_repair_rounds Maximum repair rounds per program component (default 1L).
#' @param max_bundle_repair_rounds Optional overall bundle fixer-call cap.
#' @param max_bundle_repairs_per_component Maximum bundle fixer calls per component.
#' @param usage_budget Optional shared usage budget.
#' @return A `sas2r_migration_state` list object.
#' @param plan Optional resolved contracts, graph and schedule from translation setup.
#' @noRd
new_migration_state <- function(
  project,
  out_dir,
  llm = NULL,
  config = list(),
  execute = TRUE,
  max_program_repair_rounds = 1L,
  max_bundle_repair_rounds = NULL,
  usage_budget = NULL,
  plan = NULL,
  max_bundle_repairs_per_component = 2L
) {
  p <- if (inherits(project, "sas2r_project")) project else sas_project(project)
  # The budget carries the run identifier, and attempt directories are scoped
  # by it (attempts/<run_id>/...): every invocation -- including resume = TRUE,
  # which reconstructs spend but mints a fresh run_id -- gets its own attempts
  # tree, so a rerun into the same out_dir can never silently overwrite or
  # interleave with a previous run's attempts.
  budget <- usage_budget %||% new_usage_budget(
    ledger_path = file.path(migration_paths(out_dir)$state, "usage.jsonl")
  )
  paths <- init_migration_paths(out_dir, run_id = budget$run_id)

  baseline <- sas_transpile(p, paths$staging)
  plan <- plan %||% translation_plan(p, p$config$outputs)
  graph <- plan$graph
  schedule <- plan$schedule
  p$readiness <- p$readiness %||% translation_readiness(p, plan)
  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)

  staged_dir <- file.path(attempt$attempt_dir, "staged")
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)
  write_helpers(staged_dir)

  lib_map <- build_attempt_library_map(p, attempt$attempt_dir)
  write_autoexec(p, staged_dir, library_map = lib_map)

  runtime <- list(
    autoexec = file.path(staged_dir, "autoexec.R"),
    helpers = file.path(staged_dir, "sas2r-helpers.R")
  )

  state <- list(
    project = p,
    input_manifest = input_hash_manifest(p),
    baseline = baseline,
    graph = graph,
    schedule = schedule,
    paths = paths,
    attempt = attempt,
    runtime = runtime,
    reviewer_llm = llm,
    fixer_llm = llm,
    translator_llm = llm,
    usage_budget = budget,
    config = config,
    execute = isTRUE(execute),
    max_program_repair_rounds = as.integer(max_program_repair_rounds),
    max_bundle_repair_rounds = max_bundle_repair_rounds,
    max_bundle_repairs_per_component = max_bundle_repairs_per_component,
    events = character(),
    selected_revisions = list(),
    histories = list(),
    active_revision = NULL
  )

  structure(state, class = c("sas2r_migration_state", "list"))
}

#' Normalize migration state input
#'
#' @param state Existing state list, project, or path.
#' @param project Optional project object.
#' @param out_dir Optional output directory.
#' @param llm Optional LLM instance.
#' @param config Configuration list.
#' @param execute Logical indicating if execution is enabled.
#' @param max_program_repair_rounds Integer maximum repair rounds.
#' @param ... Additional arguments.
#' @return Normalized `sas2r_migration_state` list object.
#' @noRd
normalize_migration_state <- function(
  state,
  project = NULL,
  out_dir = NULL,
  llm = NULL,
  config = list(),
  execute = TRUE,
  max_program_repair_rounds = 1L,
  ...
) {
  if (inherits(state, "sas2r_program_pipeline_result") || inherits(state, "sas2r_migration_state")) {
    return(state)
  }

  if (is.null(state) || inherits(state, "sas2r_project") || is.character(state)) {
    p <- if (inherits(state, "sas2r_project")) state else if (is.character(state)) sas_project(state) else project
    if (is.null(p) && !is.null(project)) p <- project
    if (is.null(p)) {
      cli::cli_abort("Missing project to initialize migration state", class = "sas2r_invalid_argument")
    }
    od <- out_dir %||% tempfile(pattern = "sas2r_out_")
    return(new_migration_state(
      project = p,
      out_dir = od,
      llm = llm,
      config = config,
      execute = execute,
      max_program_repair_rounds = max_program_repair_rounds
    ))
  }

  if (is.list(state)) {
    if (is.null(state$events)) state$events <- character()
    if (is.null(state$selected_revisions)) state$selected_revisions <- list()
    if (is.null(state$histories)) state$histories <- list()
    if (is.null(state$usage_budget)) state$usage_budget <- new_usage_budget()
    if (is.null(state$config)) state$config <- config %||% list()
    if (is.null(state$schedule) && !is.null(state$graph)) {
      state$schedule <- stable_dependency_schedule(state$graph)
    }
    if (is.null(state$reviewer_llm) && !is.null(state$llm)) state$reviewer_llm <- state$llm
    if (is.null(state$fixer_llm) && !is.null(state$llm)) state$fixer_llm <- state$llm
    if (is.null(state$translator_llm) && !is.null(state$llm)) state$translator_llm <- state$llm
    if (is.null(state$runtime) && !is.null(state$paths)) {
      helpers_file <- system.file("templates", "sas2r-helpers.R", package = "sas2r")
      state$runtime <- list(autoexec = file.path(state$paths$staging, "autoexec.R"),
                            helpers = helpers_file)
    }
  }

  structure(state, class = c("sas2r_migration_state", "list"))
}

#' Process a single program component through immediate review, smoke, and repair
#'
#' State machine:
#' 1. Generate/activate revision -> record "generated:<rev_id>"
#' 2. Mechanical checks -> record "mechanical_pass:<rev_id>" or "mechanical_fail:<rev_id>"
#' 3. Independent review -> record "reviewed:<rev_id>" or "review_unavailable:<rev_id>"
#' 4. Smoke -> record "smoke_passed:<rev_id>", "smoke_failed:<rev_id>", "smoke_blocked:<rev_id>", or "smoke_deferred:<rev_id>"
#' 5. Combine evidence -> fix if material and budget/rounds remain -> record "fixed:<next_rev_id>"
#' 6. Repeat checks/review/smoke after patch
#'
#' @param state Migration state object.
#' @param component_id Unique component identifier.
#' @param execute Logical indicating if execution is enabled (default TRUE).
#' @param max_program_repair_rounds Integer maximum repair rounds (default 1L).
#' @return Updated migration state object.
#' @noRd
initialize_program_component <- function(state, component_id) {
  # 1. Initial generation / activation
  rev <- state$selected_revisions[[component_id]]
  if (is.null(rev)) {
    rev <- generate_program_revision(
      component_id = component_id,
      project = state$project,
      baseline = state$baseline,
      graph = state$graph,
      schedule = state$schedule,
      outputs = state$output_contracts,
      llm = state$translator_llm,
      paths = state$paths,
      resolved_contracts = lapply(state$selected_revisions, function(revision) revision$contract),
      selected_revisions = state$selected_revisions,
      revision_id = "r1",
      config = state$config,
      helper_code = runtime_helper_code(state$runtime),
      usage_budget = state$usage_budget
    )
    rev_id <- rev$revision_id %||% "r1"
    state$selected_revisions[[component_id]] <- rev
    gen_ev <- paste0("generated:", rev_id)
    state$events <- c(state$events, gen_ev)
    signal_immediate_coordinator_event("program_generated", component_id, rev_id)

    # The revision fell back to the deterministic baseline because the agent
    # was unreachable; record it so the run cannot read as purely deterministic.
    if (!is.null(rev$agent_status) && !is.na(rev$agent_status) &&
        !identical(rev$agent_status, "ok")) {
      if (is.null(state$diagnostics)) state$diagnostics <- list()
      state$diagnostics$agent_degraded[[component_id]] <- rev$agent_status
      signal_immediate_coordinator_event(
        "agent_degraded", component_id, rev_id, reason = rev$agent_status
      )
    }

    raw_b <- rev$contract$binding %||% rev$binding
    b <- if (!is.null(raw_b) && (inherits(raw_b, "sas2r_component_binding") || !is.null(raw_b$binding_hash))) {
      raw_b
    } else {
      new_component_binding(
        source_hash = if (!is.null(raw_b$source_hash) && nzchar(raw_b$source_hash)) raw_b$source_hash else migration_hash(rev$contract$sas_text %||% ""),
        r_hash = if (!is.null(raw_b$r_hash) && nzchar(raw_b$r_hash)) raw_b$r_hash else migration_hash(rev$r_code %||% ""),
        helper_hash = if (!is.null(raw_b$helper_hash) && nzchar(raw_b$helper_hash)) raw_b$helper_hash else migration_hash(""),
        prompt_skill_hash = if (!is.null(raw_b$prompt_skill_hash) && nzchar(raw_b$prompt_skill_hash)) raw_b$prompt_skill_hash else migration_hash("translator"),
        dependency_closure_hash = if (!is.null(raw_b$dependency_closure_hash) && nzchar(raw_b$dependency_closure_hash)) raw_b$dependency_closure_hash else migration_hash("closure")
      )
    }
    state$histories[[component_id]] <- new_component_evidence_history(component_id, binding = b)
  } else {
    rev_id <- rev$revision_id %||% "r1"
    gen_ev <- paste0("generated:", rev_id)
    if (!gen_ev %in% state$events) {
      state$events <- c(state$events, gen_ev)
      signal_immediate_coordinator_event("program_generated", component_id, rev_id)
    }
    if (is.null(state$histories[[component_id]])) {
      raw_b <- rev$contract$binding %||% rev$binding
      b <- if (!is.null(raw_b) && (inherits(raw_b, "sas2r_component_binding") || !is.null(raw_b$binding_hash))) {
        raw_b
      } else {
        new_component_binding(
          source_hash = if (!is.null(raw_b$source_hash) && nzchar(raw_b$source_hash)) raw_b$source_hash else migration_hash(rev$contract$sas_text %||% ""),
          r_hash = if (!is.null(raw_b$r_hash) && nzchar(raw_b$r_hash)) raw_b$r_hash else migration_hash(rev$r_code %||% ""),
          helper_hash = if (!is.null(raw_b$helper_hash) && nzchar(raw_b$helper_hash)) raw_b$helper_hash else migration_hash(""),
          prompt_skill_hash = if (!is.null(raw_b$prompt_skill_hash) && nzchar(raw_b$prompt_skill_hash)) raw_b$prompt_skill_hash else migration_hash("translator"),
          dependency_closure_hash = if (!is.null(raw_b$dependency_closure_hash) && nzchar(raw_b$dependency_closure_hash)) raw_b$dependency_closure_hash else migration_hash("closure")
        )
      }
      state$histories[[component_id]] <- new_component_evidence_history(component_id, binding = b)
    }
  }

  state
}

process_program_component <- function(
  state,
  component_id,
  execute = TRUE,
  max_program_repair_rounds = 1L,
  initial_review = NULL
) {
  if (!is.list(state)) {
    cli::cli_abort("{.arg state} must be a migration state list", class = "sas2r_invalid_argument")
  }
  if (!is.character(component_id) || length(component_id) != 1L || !nzchar(component_id)) {
    cli::cli_abort("{.arg component_id} must be a non-empty string", class = "sas2r_invalid_argument")
  }

  tryCatch({
  state$component_stage[[component_id]] <- "processing"
  state <- initialize_program_component(state, component_id)

  # 2. Repair loop
  round <- state$repair_counts[[component_id]] %||% 0L
  prior_candidate <- NULL
  repeat {
    rev <- state$selected_revisions[[component_id]]
    rev_id <- rev$revision_id %||% paste0("r", round + 1L)

    state <- check_component_revision(state, component_id)
    reviewed <- review_component_revision(state, component_id, round, initial_review = initial_review)
    initial_review <- NULL
    state <- smoke_component_revision(reviewed$state, component_id, execute)
    rev <- state$selected_revisions[[component_id]]
    checks <- rev$checks
    review <- reviewed$review
    smoke_res <- rev$smoke
    # Candidates are checked, reviewed, and executed before replacing the
    # previous selection. Keep both histories, including unresolved findings.
    if (!is.null(prior_candidate)) {
      regressions <- program_repair_regressions(prior_candidate, rev, review)
      if (!is.null(prior_candidate$retained_state) && !length(regressions)) {
        consumer_check <- tryCatch(check_helper_consumers(state, prior_candidate$retained_state,
          setdiff(names(state$selected_revisions), component_id), execute),
          error = function(e) {
            if (critical_translation_error(e)) stop(e)
            list(state = state, reasons = conditionMessage(e))
          })
        state <- consumer_check$state
        regressions <- consumer_check$reasons
      }
      if (length(regressions)) {
        history <- state$histories[[component_id]]
        rejected <- history$active_revision_id
        history$active_revision_id <- prior_candidate$active_revision_id
        idx <- match(history$active_revision_id, vapply(history$revisions, `[[`, character(1), "revision_id"))
        history$revisions[[idx]]$events <- c(history$revisions[[idx]]$events, list(list(
          type = "repair_rejected", candidate_revision_id = rejected,
          mechanical_retry = rev$mechanical_retry, candidate_review = review$verdict,
          reasons = regressions
        )))
        if (!is.null(prior_candidate$retained_state)) {
          candidate_diagnostic <- list(component_id = component_id, r_path = rev$r_path,
            helper_path = rev$helper_path, errors = regressions, histories = state$histories,
            smoke = rev$smoke, candidate_review = review$verdict)
          state <- prior_candidate$retained_state
          state$repair_counts[[component_id]] <- round
          state$diagnostics$rejected_repairs <- c(state$diagnostics$rejected_repairs, list(candidate_diagnostic))
        }
        state$histories[[component_id]] <- history
        state$selected_revisions[[component_id]] <- prior_candidate$revision
        state$events <- c(state$events, paste0("repair_rejected:", rev_id))
        signal_immediate_coordinator_event("repair_rejected", component_id,
          prior_candidate$revision$revision_id, reason = paste(regressions, collapse = "; "))
        break
      }
      prior_candidate <- NULL
    }

    # An upstream crash is evidence against that dependency, not this program.
    if (!is.null(smoke_res$blocked_by)) break

    # Step D: Check if repair is required
    has_review_issue <- program_review_needs_repair(review)
    has_smoke_failure <- !is.null(smoke_res) && !isTRUE(smoke_res$passed) && !isTRUE(smoke_res$deferred)
    needs_repair <- (has_review_issue || has_smoke_failure || !isTRUE(checks$pass))

    if (!needs_repair) {
      # No issues found, loop complete
      break
    }

    # Check early stop conditions
    if (round >= max_program_repair_rounds) {
      break
    }
    if (!usage_budget_allows_future(state$usage_budget)) {
      break
    }
    if (is.null(state$fixer_llm)) {
      break
    }

    # Step E: Invoke Fixer with combined evidence
    next_round <- round + 1L
    state$repair_counts[[component_id]] <- next_round
    if (!is.null(state$resume_fingerprint)) write_migration_checkpoint(state, state$resume_fingerprint)
    next_rev_id <- paste0("r", next_round + 1L)

    fixed_rev <- tryCatch(
      fix_program_revision(
        revision = rev,
        review = if (has_review_issue) review else NULL,
        smoke = if (has_smoke_failure) smoke_res else NULL,
        checks = if (!isTRUE(checks$pass)) checks else NULL,
        mode = "program",
        llm = state$fixer_llm,
        usage = state$usage_budget,
        paths = state$paths,
        round = next_round,
        revision_id = next_rev_id,
        project = state$project,
        selected_revisions = state$selected_revisions,
        helper_code = runtime_helper_code(state$runtime),
        config = state$config
      ),
      error = function(e) {
        if (critical_translation_error(e)) stop(e)
        list(status = "repair_failed", message = conditionMessage(e))
      }
    )

    if (is.null(fixed_rev) || identical(fixed_rev$status, "repair_failed")) {
      break
    }
    if (!isTRUE(fixed_rev$checks$pass)) {
      state$diagnostics$rejected_repairs <- c(state$diagnostics$rejected_repairs, list(list(
        component_id = component_id, r_path = fixed_rev$r_path, helper_path = fixed_rev$helper_path,
        errors = fixed_rev$checks$errors, mechanical_retry = fixed_rev$mechanical_retry)))
      break
    }
    if (!isTRUE(fixed_rev$helper_changed) && ((identical(fixed_rev$r_code, rev$r_code) &&
         identical(fixed_rev$contract$helper_use %||% character(), rev$contract$helper_use %||% character())) ||
        identical(fixed_rev$patch_hash, rev$contract$patch_hash))) {
      break
    }

    fixed_rev$revision_id <- next_rev_id
    state$events <- c(state$events, paste0("fixed:", next_rev_id))
    signal_immediate_coordinator_event("program_fixed", component_id, next_rev_id)

    # Activate new component binding in history
    new_b <- fixed_rev$contract$binding %||% new_component_binding(
      source_hash = rev$binding$source_hash %||% migration_hash(rev$contract$sas_text %||% ""),
      r_hash = migration_hash(fixed_rev$r_code),
      helper_hash = rev$binding$helper_hash %||% migration_hash(""),
      prompt_skill_hash = rev$binding$prompt_skill_hash %||% migration_hash("fixer"),
      dependency_closure_hash = rev$binding$dependency_closure_hash %||% migration_hash("closure")
    )
    # Keep closure identity current before reviewing the new code. Otherwise a
    # resume-only runtime revisit would invalidate this review a second time.
    hashes <- vapply(state$selected_revisions, function(x) migration_hash(revision_code(x)), "")
    hashes[[component_id]] <- migration_hash(fixed_rev$r_code)
    closure <- dependency_closure_hashes(state$graph, hashes, new_b$helper_hash,
      new_b$prompt_skill_hash)[[component_id]]
    new_b <- new_component_binding(new_b$source_hash, new_b$r_hash, new_b$helper_hash,
      new_b$prompt_skill_hash, closure)
    fixed_rev$binding <- new_b
    if (!is.null(fixed_rev$contract)) fixed_rev$contract$binding <- new_b

    retained_state <- state
    prior_candidate <- list(revision = rev, verdict = review$verdict,
      active_revision_id = state$histories[[component_id]]$active_revision_id)
    state$histories[[component_id]] <- activate_component_binding(state$histories[[component_id]], new_b)
    state$selected_revisions[[component_id]] <- fixed_rev
    if (isTRUE(fixed_rev$helper_changed) && isTRUE(fixed_rev$checks$pass)) {
      prior_candidate$retained_state <- retained_state
      state <- stage_helper_candidate(state, fixed_rev)
    }
    round <- next_round
  }

  state$component_stage[[component_id]] <- "settled"
  state$active_revision <- state$selected_revisions[[component_id]]$revision_id
  state
  }, error = function(error) {
    # Preserve the last completed phase if a later component step fails.
    record_component_failure(state, component_id, error)
  })
}

#' Run the full program pipeline with graph-driven targeted revisit
#'
#' Schedules components in stable topological dependency order, processes each
#' immediately through review/smoke/repair, and enqueues affected downstream or
#' previously deferred components when upstream dependencies change.
#'
#' @param state Migration state object or project.
#' @param max_program_repair_rounds Maximum repair rounds per component (default 1L).
#' @param execute Logical indicating if execution is enabled (default TRUE).
#' @param ... Additional arguments passed to normalize_migration_state.
#' @return A `sas2r_program_pipeline_result` list object.
#' @noRd
run_program_pipeline <- function(
  state,
  max_program_repair_rounds = 1L,
  execute = TRUE,
  ...
) {
  state <- normalize_migration_state(
    state = state,
    execute = execute,
    max_program_repair_rounds = max_program_repair_rounds,
    ...
  )

  schedule <- state$schedule %||% stable_dependency_schedule(state$graph)
  cids <- if (nrow(schedule) > 0L) schedule$component_id else names(state$selected_revisions) %||% character()
  tryCatch({
  # Resume reassesses observations, rather than carrying forward old blocks.
  state$diagnostics[c("parallel_deferred", "dependency_findings", "component_failures",
    "execution_deferred")] <- NULL
  state$project$dependency_findings <- NULL
  for (cid in names(state$selected_revisions)) state <- record_dependency_finding(
    state, cid, parallel_dependency_findings(state, cid))

  if ((state$parallel$effective %||% 1L) > 1L) {
    return(run_parallel_program_pipeline(state, cids, execute, max_program_repair_rounds))
  }

  revisit_queue <- cids
  # Only context-triggered visits of already processed, unchanged components
  # skip agents. A first visit or an actual component edit still gets review.
  processed <- lapply(state$selected_revisions[state$resumed_components %||% character()],
    component_content_identity)
  revisit_count <- state$revisit_counts %||% stats::setNames(rep(0L, length(cids)), cids)
  state$revisit_counts <- revisit_count
  max_revisits_per_comp <- 3L

  old_hashes <- stats::setNames(character(length(cids)), cids)
  for (cid in cids) {
    old_hashes[[cid]] <- state$selected_revisions[[cid]]$binding$binding_hash %||% ""
  }

  while (length(revisit_queue) > 0L) {
    cid <- revisit_queue[1L]
    revisit_queue <- revisit_queue[-1L]

    old_cid_hash <- state$selected_revisions[[cid]]$binding$binding_hash %||% ""

    content <- component_content_identity(state$selected_revisions[[cid]])
    if (!is.null(content) && identical(processed[[cid]], content)) {
      state <- tryCatch(revisit_component_runtime(state, cid, execute),
        error = function(error) record_component_failure(state, cid, error, "revisit"))
    } else {
      state <- process_program_component(state, cid, execute, max_program_repair_rounds)
    }
    processed[[cid]] <- component_content_identity(state$selected_revisions[[cid]])
    if (is.null(state$diagnostics$component_failures[[cid]])) state$component_stage[[cid]] <- "settled"

    new_cid_hash <- state$selected_revisions[[cid]]$binding$binding_hash %||% ""

    # Targeted Revisit: if component revision hash changed, requeue affected dependents
    if (!identical(old_cid_hash, new_cid_hash)) {
      new_hashes <- stats::setNames(
        vapply(cids, function(k) state$selected_revisions[[k]]$binding$binding_hash %||% "", character(1)),
        cids
      )

      deferred_cids <- cids[vapply(cids, function(k) {
        ev <- current_component_evidence(state$histories[[k]])
        !is.null(ev$runtime_deferred) && nzchar(ev$runtime_deferred)
      }, logical(1))]

      waiting_on <- lapply(state$selected_revisions, function(rev) rev$smoke$waiting_on)
      requeue <- requeue_components(state$graph, old_hashes, new_hashes,
        runtime_deferred = deferred_cids, waiting_on = waiting_on)
      requeue <- setdiff(requeue, cid)

      for (rq_cid in requeue) {
        if (is.null(state$diagnostics$component_failures[[rq_cid]]) &&
            !is.na(revisit_count[[rq_cid]]) && revisit_count[[rq_cid]] < max_revisits_per_comp && !rq_cid %in% revisit_queue) {
          revisit_count[[rq_cid]] <- revisit_count[[rq_cid]] + 1L
          revisit_queue <- c(revisit_queue, rq_cid)
          signal_immediate_coordinator_event("component_revisited", rq_cid)
        }
      }
      old_hashes <- new_hashes
    }
    state$revisit_counts <- revisit_count
    if (!is.null(state$resume_fingerprint)) write_migration_checkpoint(state, state$resume_fingerprint)
  }

  if (length(state$selected_revisions) > 0L && is.null(state$active_revision)) {
    state$active_revision <- state$selected_revisions[[length(state$selected_revisions)]]$revision_id
  }

  state <- finalize_component_reviews(state)
  structure(state, class = c("sas2r_program_pipeline_result", "sas2r_migration_state", "list"))
  }, error = function(error) {
    error$migration_state <- error$migration_state %||% state
    stop(error)
  })
}

#' Build a causal repair packet for bundle-level repair
#'
#' Gathers the first stopping runtime condition, bounded local logs, all failed target
#' checks and diffs, contributing dependency closure, behavioral contracts, helper guarantees,
#' and previous repair disposition. Never includes unrelated outputs or whole datasets.
#'
#' @param state Migration state object.
#' @param attempt Completed bundle attempt record.
#' @param assessment Final output assessment record.
#' @param previous_disposition Optional previous repair disposition summary.
#' @return Named list representing the causal repair packet.
#' @noRd
build_bundle_repair_packet <- function(
  state,
  attempt,
  assessment,
  previous_disposition = NULL
) {
  # 1. First stopping runtime condition & component
  stopping_cond <- attempt$condition
  stopping_cid <- if (!is.null(stopping_cond) && !is.null(stopping_cond$component_id)) {
    stopping_cond$component_id
  } else if (!isTRUE(attempt$passed)) {
    setdiff(attempt$execution_order %||% character(), attempt$executed_component_ids %||% character())[1L]
  } else {
    NULL
  }

  # 2. Failed target checks and diffs
  failed_targets <- list()
  if (!is.null(assessment$targets) && length(assessment$targets) > 0L) {
    for (t_key in names(assessment$targets)) {
      t <- assessment$targets[[t_key]]
      if (!isTRUE(t$passed)) {
        failed_targets[[t_key]] <- list(
          target_id = t$target_id,
          target_key = t$target_key,
          kind = t$kind,
          required = t$required,
          status = t$status,
          checks = t$checks,
          differences = t$differences,
          # Full comparison evidence remains local; the fixer receives only
          # source-grounded review and non-reference execution/check evidence.
          candidate_path = t$candidate_path %||% NA_character_,
          reference_path = t$reference_path %||% NA_character_
        )
      }
    }
  }

  # 3. Implicated components from stopping error and failed targets lineage
  implicated_cids <- character()
  if (!is.null(stopping_cid) && nzchar(stopping_cid)) {
    implicated_cids <- c(implicated_cids, stopping_cid)
  }

  for (t_key in names(failed_targets)) {
    lin <- if (!is.null(assessment$lineage_by_target[[t_key]])) {
      assessment$lineage_by_target[[t_key]]$upstream_components
    } else if (!is.null(state$graph)) {
      evidence_for_output_lineage(state$graph, state$histories, t_key)$upstream_components
    } else {
      character()
    }
    implicated_cids <- c(implicated_cids, lin)
  }
  implicated_cids <- unique(implicated_cids[!is.na(implicated_cids) & nzchar(implicated_cids)])

  # 4. Primary implicated component
  primary_cid <- NULL
  if (!is.null(stopping_cid) && nzchar(stopping_cid) && stopping_cid %in% names(state$selected_revisions)) {
    primary_cid <- stopping_cid
  } else if (length(implicated_cids) > 0L) {
    sched <- state$schedule %||% (if (!is.null(state$graph)) stable_dependency_schedule(state$graph) else NULL)
    sched_order <- if (!is.null(sched) && nrow(sched) > 0L) sched$component_id else names(state$selected_revisions)
    ordered <- intersect(sched_order, implicated_cids)
    primary_cid <- if (length(ordered) > 0L) ordered[1L] else implicated_cids[1L]
  }

  # 5. Bounded diagnostics
  diag <- bounded_agent_diagnostics(
    attempt,
    policy = state$agent_evidence %||% state$config$agent_evidence %||% "code_only"
  )

  # 6. Evidence IDs
  evidence_ids <- unique(c(
    attempt$attempt_id,
    vapply(failed_targets, function(t) paste0("target:", t$target_key), character(1))
  ))
  evidence_ids <- evidence_ids[!is.na(evidence_ids) & nzchar(evidence_ids)]

  list(
    primary_component_id = primary_cid,
    implicated_components = implicated_cids,
    stopping_condition = stopping_cond,
    stopping_component_id = stopping_cid,
    failed_targets = failed_targets,
    bounded_diagnostics = diag,
    evidence_ids = evidence_ids,
    previous_disposition = previous_disposition
  )
}

#' Run the full bundle execution and output-driven repair loop with fresh complete reruns
#'
#' Coordinates full bundle attempt execution, comprehensive final output assessment,
#' deterministic four-state bundle status determination, causal repair packet generation,
#' worker patching, closure invalidation, and fresh complete reruns.
#'
#' @param state Migration state object, project, or output directory path.
#' @param max_bundle_repair_rounds Optional overall bundle fixer-call cap.
#' @param max_bundle_repairs_per_component Maximum bundle fixer calls per component.
#' @param execute Logical indicating if execution is enabled (default TRUE).
#' @param ... Additional arguments passed to normalize_migration_state.
#' @return A `sas2r_bundle_pipeline_result` list object.
#' @noRd
run_bundle_pipeline <- function(
  state,
  max_bundle_repair_rounds = NULL,
  execute = TRUE,
  max_bundle_repairs_per_component = 2L,
  ...
) {
  state <- normalize_migration_state(
    state = state,
    execute = execute,
    max_bundle_repair_rounds = max_bundle_repair_rounds,
    ...
  )

  if (is.null(state$output_contracts)) {
    state$output_contracts <- if (!is.null(state$project)) infer_output_contracts(state$project) else empty_output_contracts()
  }
  if (is.null(state$graph) && !is.null(state$project)) {
    state$graph <- build_dependency_graph(state$project, output_contracts = state$output_contracts)
  }
  if (is.null(state$schedule) && !is.null(state$graph)) {
    state$schedule <- stable_dependency_schedule(state$graph)
  }

  if (is.null(state$histories)) state$histories <- list()
  for (cid in names(state$selected_revisions)) {
    if (is.null(state$histories[[cid]])) {
      c_rev <- state$selected_revisions[[cid]]
      raw_b <- c_rev$contract$binding %||% c_rev$binding
      b <- if (!is.null(raw_b) && (inherits(raw_b, "sas2r_component_binding") || !is.null(raw_b$binding_hash))) {
        raw_b
      } else {
        new_component_binding(
          source_hash = if (!is.null(raw_b$source_hash) && nzchar(raw_b$source_hash)) raw_b$source_hash else migration_hash(c_rev$contract$sas_text %||% ""),
          r_hash = if (!is.null(raw_b$r_hash) && nzchar(raw_b$r_hash)) raw_b$r_hash else migration_hash(c_rev$r_code %||% ""),
          helper_hash = if (!is.null(raw_b$helper_hash) && nzchar(raw_b$helper_hash)) raw_b$helper_hash else migration_hash(""),
          prompt_skill_hash = if (!is.null(raw_b$prompt_skill_hash) && nzchar(raw_b$prompt_skill_hash)) raw_b$prompt_skill_hash else migration_hash("translator"),
          dependency_closure_hash = if (!is.null(raw_b$dependency_closure_hash) && nzchar(raw_b$dependency_closure_hash)) raw_b$dependency_closure_hash else migration_hash("closure")
        )
      }
      state$histories[[cid]] <- new_component_evidence_history(cid, binding = b)
    }
  }

  component_limit <- bundle_repair_limit(max_bundle_repairs_per_component,
    "max_bundle_repairs_per_component")
  explicit_limit <- bundle_repair_limit(max_bundle_repair_rounds,
    "max_bundle_repair_rounds", allow_null = TRUE)
  total_limit <- explicit_limit %||% (as.double(component_limit) * length(state$selected_revisions))
  repair_counts <- state$diagnostics$bundle_repair$repair_counts %||% list()
  deferred <- list()
  diagnostic_history <- list()
  round <- as.integer(sum(unlist(repair_counts)))
  attempt_seq <- 1L
  attempts_summary <- list()
  repairs <- list()
  selected_attempt <- NULL
  selected_assessment <- NULL
  selected_revisions <- NULL
  selected_histories <- NULL
  selected_runtime <- NULL
  latest_assessment <- NULL
  latest_attempt <- NULL
  latest_diagnosis <- NULL
  latest_status <- "blocked"
  stop_reason <- NULL
  is_regression <- FALSE

  repeat {
    signal_bundle_event(
      "bundle_round_started",
      attempt_id = new_attempt_id("bundle", attempt_seq),
      round = round,
      status = latest_status
    )
    signal_bundle_event(
      "bundle_attempt_started",
      attempt_id = new_attempt_id("bundle", attempt_seq),
      round = round
    )

    # 1. Execute full attempt
    attempt_rec <- if (isTRUE(execute)) {
      run_bundle_attempt(
        state = state,
        sequence = attempt_seq,
        parent_attempt_id = if (!is.null(latest_attempt)) latest_attempt$attempt_id else NULL
      )
    } else {
      att <- init_attempt(state$paths, kind = "bundle", sequence = attempt_seq)
      b_dir <- snapshot_selected_bundle(state, att)
      complete_attempt(
        att,
        passed = FALSE,
        exit_status = 0L,
        deferred = TRUE,
        reason = "execute_disabled",
        execution_order = if (!is.null(state$graph)) build_bundle_execution_plan(state$graph)$execution_order else character(),
        executed_component_ids = character(0),
        input_hashes_before = input_hash_manifest(state$project %||% state),
        input_hashes_after = input_hash_manifest(state$project %||% state),
        output_hashes = list()
      )
    }

    latest_attempt <- attempt_rec
    signal_bundle_event(
      "bundle_attempt_completed",
      attempt_id = attempt_rec$attempt_id,
      round = round,
      passed = isTRUE(attempt_rec$passed),
      deferred = isTRUE(attempt_rec$deferred),
      reason = attempt_rec$reason %||% attempt_rec$condition$message
    )

    # 2. Assess all outputs
    assessment <- assess_final_outputs(
      contracts = state$output_contracts %||% empty_output_contracts(),
      attempt = attempt_rec,
      graph = state$graph,
      evidence_histories = state$histories,
      comparison_rules = state$comparison_rules %||% state$config$comparison_rules %||% list()
    )

    missing_artifacts <- Filter(function(t) length(artifact_failure_checks(t)) > 0L, assessment$targets)
    investigate_components <- unique(unlist(lapply(missing_artifacts, function(t)
      source_output_writers(state, t$target_key)), use.names = FALSE))
    attempt_rec$candidate_input_observations <- observe_candidate_inputs(state, attempt_rec,
      components = investigate_components)
    state$diagnostics$candidate_input_observations[[attempt_rec$attempt_id]] <- attempt_rec$candidate_input_observations

    if (!is.null(assessment$evidence_histories)) {
      state$histories <- assessment$evidence_histories
    }
    if (isTRUE(execute) && round < total_limit && !is.null(state$fixer_llm)) {
      previous_histories <- state$histories
      state <- review_bundle_mismatches(state, attempt_rec, assessment, round)
      if (!identical(previous_histories, state$histories)) {
        assessment <- assess_final_outputs(state$output_contracts %||% empty_output_contracts(),
          attempt_rec, state$graph, state$histories,
          state$comparison_rules %||% state$config$comparison_rules %||% list(),
          target_results = assessment$targets)
        state$histories <- assessment$evidence_histories
      }
    }
    latest_assessment <- assessment
    status <- assessment$status
    latest_status <- status

    signal_bundle_event(
      "bundle_gate_evaluated",
      attempt_id = attempt_rec$attempt_id,
      round = round,
      status = status
    )

    # 3. Deterministic selection
    cand_selection <- tryCatch(
      select_attempt(
        state$paths,
        candidate = attempt_rec,
        assessment = assessment,
        previous = selected_attempt
      ),
      error = function(e) e
    )

    if (inherits(cand_selection, "sas2r_selected_attempt")) {
      selected_attempt <- cand_selection
      selected_assessment <- assessment
      selected_revisions <- state$selected_revisions
      selected_histories <- state$histories
      selected_runtime <- state$runtime
      signal_bundle_event(
        "bundle_attempt_selected",
        attempt_id = attempt_rec$attempt_id,
        round = round,
        status = status
      )
    } else if (inherits(cand_selection, "sas2r_regressive_selection")) {
      state$diagnostics$selection_rejections[[attempt_rec$attempt_id]] <- conditionMessage(cand_selection)
      # A selected attempt from this pipeline protects against regressive
      # repairs. An older run's selection protects publication only: give the
      # new run its bounded repair allowance before considering replacement.
      is_regression <- !is.null(selected_attempt)
      if (is_regression) {
        signal_bundle_event(
          "bundle_early_stop",
          attempt_id = attempt_rec$attempt_id,
          round = round,
          reason = "regressive_attempt"
        )
      } else if (file.exists(state$paths$selected)) {
        previous <- jsonlite::read_json(state$paths$selected, simplifyVector = FALSE)
        signal_bundle_event("bundle_previous_selection_retained", round = round,
          reason = paste0(previous$attempt_id, " at ", previous$attempt_dir, "; ", conditionMessage(cand_selection)))
      }
    }

    # Record attempt in summary
    target_count <- sum(vapply(assessment$targets, function(t) !identical(t$status, "unresolved_target"), logical(1)))
    attempts_summary[[length(attempts_summary) + 1L]] <- list(
      sequence = as.integer(attempt_seq),
      attempt_id = attempt_rec$attempt_id,
      fresh_work = TRUE,
      assessed_target_count = target_count,
      status = status,
      passed = isTRUE(attempt_rec$passed)
    )

    # 4. Stop condition checks
    if (status %in% c("migration_ready", "validated")) {
      stop_reason <- "ready_or_validated"
      break
    }
    if (round >= total_limit) {
      stop_reason <- if (is.null(explicit_limit)) "bundle_component_repair_limits_reached" else "max_bundle_repair_rounds_reached"
      break
    }
    if (!isTRUE(execute)) {
      stop_reason <- "execute_disabled"
      break
    }
    if (is_regression) {
      stop_reason <- "regression_detected"
      break
    }
    if (!usage_budget_allows_future(state$usage_budget)) {
      stop_reason <- "budget_exhausted"
      break
    }
    if (is.null(state$fixer_llm)) {
      stop_reason <- "no_fixer_llm"
      break
    }

    diagnostic <- collect_bundle_diagnostics(state, attempt_rec)
    diagnostic$non_translation_failures <- Filter(Negate(is.null), stats::setNames(
      lapply(names(diagnostic$failures), function(cid)
        non_translation_runtime_reason(state, cid, diagnostic$failures[[cid]])), names(diagnostic$failures)))
    diagnostic_history[[attempt_rec$attempt_id]] <- diagnostic
    queue <- bundle_repair_queue(state, attempt_rec, assessment, diagnostic,
                                 previous_disposition = latest_diagnosis)
    eligible <- names(queue)[vapply(names(queue), function(cid) {
      !isTRUE(queue[[cid]]$source_review_only) &&
        (repair_counts[[cid]] %||% 0L) < component_limit && is.null(deferred[[cid]])
    }, logical(1))]
    if (!length(eligible)) {
      skipped <- as.character(unlist(attempt_rec$deferred_component_ids %||% character()))
      stop_reason <- if (!length(queue) && length(skipped)) paste0("Bundle executed without ",
          paste(skipped, collapse = ", "), "; unresolved: ", paste(unique(as.character(unlist(
          attempt_rec$deferred_reasons))), collapse = "; ")) else
        if (!length(queue) && length(diagnostic$non_translation_failures))
        paste(unique(unlist(diagnostic$non_translation_failures)), collapse = "; ") else
        if (!length(queue)) "no_causal_evidence" else
        if (all(vapply(queue, function(x) isTRUE(x$source_review_only), logical(1)))) "no_source_grounded_repair" else
        if (length(deferred)) unname(deferred[[1L]]) else "bundle_component_repair_limits_reached"
      break
    }
    signal_bundle_event("bundle_repair_queue", round = round,
      reason = paste(eligible, collapse = ", "))
    changed <- FALSE
    changed_components <- character()
    for (primary_cid in eligible) {
      if (round >= total_limit || !usage_budget_allows_future(state$usage_budget)) break
      # Any upstream repair makes this candidate's evidence stale. It will be
      # reconsidered after a fresh run rather than repaired for inherited errors.
      ancestors <- dependency_closure(state$graph, primary_cid)
      if (length(intersect(ancestors, changed_components))) next
      upstream_pending <- any(vapply(queue[intersect(ancestors, names(queue))],
        function(x) !isTRUE(x$source_review_only), logical(1)))
      packet <- queue[[primary_cid]]
      if (upstream_pending && !isTRUE(packet$code_local)) next
      if (upstream_pending) {
        # Repair the independent static defect only; inherited output evidence
        # cannot yet establish a downstream translation error.
        packet$failed_targets <- list()
        if (!is.null(packet$review)) packet$review$findings <- source_grounded_review_findings(packet$review)
        packet$attempt <- attempt_rec
        packet$attempt$condition <- NULL
        packet$attempt$passed <- TRUE
      }
      repair_counts[[primary_cid]] <- (repair_counts[[primary_cid]] %||% 0L) + 1L
      state$diagnostics$bundle_repair$repair_counts <- repair_counts
      if (!is.null(state$resume_fingerprint)) write_migration_checkpoint(state, state$resume_fingerprint)
      outcome <- repair_bundle_component(state, packet, attempt_rec, round)
      round <- round + 1L
      state <- outcome$state
      if (!is.null(state$resume_fingerprint)) write_migration_checkpoint(state, state$resume_fingerprint)
      if (!isTRUE(outcome$applied)) {
        for (cid in names(selected_histories)) {
          if (identical(selected_histories[[cid]]$active_revision_id, state$histories[[cid]]$active_revision_id))
            selected_histories[[cid]] <- state$histories[[cid]]
        }
        deferred[[primary_cid]] <- outcome$reason
        signal_bundle_event("bundle_component_deferred", round = round,
          component_id = primary_cid, reason = outcome$reason)
        next
      }
      changed <- TRUE
      changed_components <- c(changed_components, primary_cid)
      repairs[[length(repairs) + 1L]] <- outcome$repair
      latest_diagnosis <- outcome$repair$diagnosis
      # A shared helper change invalidates every queued component's evidence.
      if (isTRUE(outcome$helper_changed)) break
    }
    if (!changed) {
      stop_reason <- if (!usage_budget_allows_future(state$usage_budget)) "budget_exhausted" else
        if (length(deferred)) unname(deferred[[1L]]) else "bundle_component_repair_limits_reached"
      break
    }
    attempt_seq <- attempt_seq + 1L
  }

  attempts_df <- tibble::tibble(
    sequence = if (length(attempts_summary) > 0L) vapply(attempts_summary, function(x) as.integer(x$sequence), integer(1)) else integer(),
    attempt_id = if (length(attempts_summary) > 0L) vapply(attempts_summary, function(x) as.character(x$attempt_id), character(1)) else character(),
    fresh_work = if (length(attempts_summary) > 0L) vapply(attempts_summary, function(x) isTRUE(x$fresh_work), logical(1)) else logical(),
    assessed_target_count = if (length(attempts_summary) > 0L) vapply(attempts_summary, function(x) as.integer(x$assessed_target_count), integer(1)) else integer(),
    status = if (length(attempts_summary) > 0L) vapply(attempts_summary, function(x) as.character(x$status), character(1)) else character(),
    passed = if (length(attempts_summary) > 0L) vapply(attempts_summary, function(x) isTRUE(x$passed), logical(1)) else logical()
  )

  final_status <- if (!is.null(selected_attempt)) {
    selected_attempt$status %||% selected_assessment$status %||% latest_status
  } else {
    latest_status
  }

  status_reason <- if (final_status %in% c("migration_ready", "validated")) {
    NULL
  } else {
    stop_reason %||% latest_diagnosis %||% "Bundle outputs or execution did not satisfy gate requirements"
  }

  res <- list(
    status = final_status,
    status_reason = status_reason,
    attempts = attempts_df,
    selected_attempt = selected_attempt,
    attempt = latest_attempt,
    current_run_status = latest_status,
    assessment = selected_assessment %||% latest_assessment,
    repairs = repairs,
    # Merge onto what the program pipeline recorded (e.g. agent_degraded)
    # instead of clobbering it.
    diagnostics = utils::modifyList(
      state$diagnostics %||% list(),
      list(stop_reason = stop_reason, latest_diagnosis = latest_diagnosis,
        bundle_repair = list(per_component_limit = component_limit,
          overall_limit = total_limit, repair_counts = repair_counts,
          deferred = deferred, attempts = diagnostic_history)),
      keep.null = TRUE
    ),
    project = state$project,
    graph = state$graph,
    schedule = state$schedule,
    paths = state$paths,
    output_contracts = state$output_contracts,
    selected_revisions = selected_revisions %||% state$selected_revisions,
    histories = selected_histories %||% state$histories,
    runtime = selected_runtime %||% state$runtime,
    execute = isTRUE(execute),
    environment = state$environment,
    usage_budget = state$usage_budget,
    config = state$config,
    repair_counts = state$repair_counts,
    resume_fingerprint = state$resume_fingerprint,
    events = state$events
  )

  structure(res, class = c("sas2r_bundle_pipeline_result", "sas2r_migration_state", "list"))
}

# Evidence dimensions are compared individually: a clean review must not hide
# a new crash, failed mechanical check, or lost semantic-check coverage.
program_repair_regressions <- function(previous, candidate, review, execution = TRUE) {
  old <- previous$revision
  reasons <- character()
  if (isTRUE(old$checks$pass) && !isTRUE(candidate$checks$pass)) reasons <- c(reasons, "mechanical checks regressed")
  if (isTRUE(execution) && isTRUE(old$smoke$passed) && !isTRUE(candidate$smoke$passed)) reasons <- c(reasons, "execution regressed")
  if (identical(previous$verdict, "reviewed_no_material_finding") &&
      !identical(review$verdict, "reviewed_no_material_finding")) reasons <- c(reasons, "review regressed")
  if (isTRUE(execution) && length(setdiff(passed_population_checks(old$smoke),
      passed_population_checks(candidate$smoke)))) reasons <- c(reasons, "source population coverage regressed")
  reasons
}
