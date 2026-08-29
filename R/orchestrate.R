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
#' @param max_bundle_repair_rounds Maximum repair rounds for full bundle (default 2L).
#' @param usage_budget Optional shared usage budget.
#' @return A `sas2r_migration_state` list object.
#' @noRd
new_migration_state <- function(
  project,
  out_dir,
  llm = NULL,
  config = list(),
  execute = TRUE,
  max_program_repair_rounds = 1L,
  max_bundle_repair_rounds = 2L,
  usage_budget = NULL
) {
  p <- if (inherits(project, "sas2r_project")) project else sas_project(project)
  paths <- migration_paths(out_dir)
  init_migration_paths(out_dir)

  baseline <- sas_transpile(p, paths$root)
  graph <- build_dependency_graph(p)
  schedule <- stable_dependency_schedule(graph)
  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)

  staged_dir <- file.path(attempt$attempt_dir, "staged")
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)
  write_helpers(staged_dir)

  lib_map <- build_attempt_library_map(p, attempt$attempt_dir)
  write_registry(p, staged_dir, library_map = lib_map)

  runtime <- list(
    registry = file.path(staged_dir, "_sas2r_registry.R"),
    helpers = file.path(staged_dir, "sas2r-helpers.R")
  )

  budget <- usage_budget %||% new_usage_budget(
    ledger_path = file.path(paths$state, "usage.jsonl")
  )

  state <- list(
    project = p,
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
    max_bundle_repair_rounds = as.integer(max_bundle_repair_rounds),
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
      reg_file <- file.path(state$paths$state, "_sas2r_registry.R")
      state$runtime <- list(registry = reg_file, helpers = helpers_file)
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
#' 4. Meaningful smoke / defer -> record "smoke_passed:<rev_id>", "smoke_failed:<rev_id>", or "smoke_deferred:<rev_id>"
#' 5. Combine evidence -> fix if material and budget/rounds remain -> record "fixed:<next_rev_id>"
#' 6. Repeat checks/review/smoke after patch
#'
#' @param state Migration state object.
#' @param component_id Unique component identifier.
#' @param execute Logical indicating if execution is enabled (default TRUE).
#' @param max_program_repair_rounds Integer maximum repair rounds (default 1L).
#' @return Updated migration state object.
#' @noRd
process_program_component <- function(
  state,
  component_id,
  execute = TRUE,
  max_program_repair_rounds = 1L
) {
  if (!is.list(state)) {
    cli::cli_abort("{.arg state} must be a migration state list", class = "sas2r_invalid_argument")
  }
  if (!is.character(component_id) || length(component_id) != 1L || !nzchar(component_id)) {
    cli::cli_abort("{.arg component_id} must be a non-empty string", class = "sas2r_invalid_argument")
  }

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
      revision_id = "r1",
      config = state$config,
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

  # 2. Repair loop
  round <- 0L
  repeat {
    rev <- state$selected_revisions[[component_id]]
    rev_id <- rev$revision_id %||% paste0("r", round + 1L)

    # Step A: Mechanical checks
    registry_p <- if (!is.null(state$runtime)) state$runtime$registry else NULL
    checks <- check_program_revision(rev$r_path, contract = rev$contract, registry = registry_p)

    if (isTRUE(checks$pass)) {
      state$events <- c(state$events, paste0("mechanical_pass:", rev_id))
      signal_immediate_coordinator_event("mechanical_pass", component_id, rev_id)
      rev$status <- "ok"
    } else {
      state$events <- c(state$events, paste0("mechanical_fail:", rev_id))
      signal_immediate_coordinator_event("mechanical_fail", component_id, rev_id)
      rev$status <- "check_failed"
    }

    # Step B: Independent review
    ctx <- list(
      component_id = component_id,
      revision_id = rev_id,
      r_code = rev$r_code,
      r_path = rev$r_path,
      contract = rev$contract,
      binding = rev$binding %||% rev$contract$binding,
      history = state$histories[[component_id]],
      sas_source = rev$contract$sas_text %||% "",
      project = state$project,
      config = state$config %||% list()
    )

    review <- review_program_revision(
      revision = rev,
      context = ctx,
      llm = state$reviewer_llm,
      usage = state$usage_budget,
      paths = state$paths,
      round = round,
      history = state$histories[[component_id]]
    )

    if (!is.null(review$history)) {
      state$histories[[component_id]] <- review$history
    }

    if (identical(review$verdict, "review_unavailable")) {
      state$events <- c(state$events, paste0("review_unavailable:", rev_id))
    } else {
      state$events <- c(state$events, paste0("reviewed:", rev_id))
    }
    signal_immediate_coordinator_event("program_reviewed", component_id, rev_id)

    # Step C: Meaningful smoke / defer
    smoke_res <- NULL
    if (!isTRUE(execute)) {
      state$histories[[component_id]] <- record_runtime_deferred(
        state$histories[[component_id]],
        reason = "execute_disabled"
      )
      state$events <- c(state$events, paste0("smoke_deferred:", rev_id))
      smoke_res <- list(passed = FALSE, deferred = TRUE, reason = "execute_disabled")
    } else {
      plan <- build_program_smoke_plan(
        graph = state$graph,
        component_id = component_id,
        selected_revisions = state$selected_revisions,
        execute = TRUE
      )

      if (identical(plan$status, "deferred")) {
        state$histories[[component_id]] <- record_runtime_deferred(
          state$histories[[component_id]],
          reason = plan$reason
        )
        state$events <- c(state$events, paste0("smoke_deferred:", rev_id))
        smoke_res <- list(passed = FALSE, deferred = TRUE, reason = plan$reason)
      } else if (identical(plan$status, "runnable")) {
        attempt_dir <- if (!is.null(state$attempt) && !is.null(state$attempt$attempt_dir)) {
          state$attempt$attempt_dir
        } else if (!is.null(state$paths) && !is.null(state$paths$attempts)) {
          file.path(state$paths$attempts, "smoke_attempt_001")
        } else {
          tempdir()
        }

        smoke_res <- run_program_smoke(plan, state$runtime, attempt_dir)

        if (isTRUE(smoke_res$passed)) {
          state$events <- c(state$events, paste0("smoke_passed:", rev_id))
          # Promote evidence level if review completed cleanly
          curr_ev <- current_component_evidence(state$histories[[component_id]])
          if (!is.null(curr_ev) && identical(curr_ev$level, "reviewed_only") && length(curr_ev$blockers) == 0L) {
            coverage_id <- paste0("call:", component_id)
            state$histories[[component_id]] <- promote_component_evidence(
              state$histories[[component_id]],
              target_level = "runtime_verified",
              coverage = coverage_id,
              basis_id = smoke_res$execution_id
            )
          }
        } else {
          state$events <- c(state$events, paste0("smoke_failed:", rev_id))
          active_idx <- which(vapply(state$histories[[component_id]]$revisions, function(r) {
            identical(r$revision_id, state$histories[[component_id]]$active_revision_id)
          }, logical(1)))
          if (length(active_idx)) {
            state$histories[[component_id]]$revisions[[active_idx]]$blockers <- unique(c(
              state$histories[[component_id]]$revisions[[active_idx]]$blockers,
              "smoke_failed"
            ))
          }
        }
      }
    }

    # Step D: Check if repair is required
    has_review_issue <- identical(review$verdict, "repair_required")
    has_smoke_failure <- !is.null(smoke_res) && !isTRUE(smoke_res$passed) && !isTRUE(smoke_res$deferred)
    needs_repair <- (has_review_issue || has_smoke_failure)

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
    next_rev_id <- paste0("r", next_round + 1L)

    fixed_rev <- tryCatch(
      fix_program_revision(
        revision = rev,
        review = if (has_review_issue) review else NULL,
        smoke = if (has_smoke_failure) smoke_res else NULL,
        mode = "program",
        llm = state$fixer_llm,
        usage = state$usage_budget,
        paths = state$paths,
        round = next_round,
        revision_id = next_rev_id,
        project = state$project,
        config = state$config
      ),
      error = function(e) list(status = "repair_failed", message = conditionMessage(e))
    )

    if (is.null(fixed_rev) || identical(fixed_rev$status, "repair_failed")) {
      break
    }
    if (identical(fixed_rev$r_code, rev$r_code) || identical(fixed_rev$patch_hash, rev$contract$patch_hash)) {
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
    fixed_rev$binding <- new_b
    if (!is.null(fixed_rev$contract)) fixed_rev$contract$binding <- new_b

    state$histories[[component_id]] <- activate_component_binding(state$histories[[component_id]], new_b)
    state$selected_revisions[[component_id]] <- fixed_rev
    round <- next_round
  }

  state$active_revision <- state$selected_revisions[[component_id]]$revision_id
  state
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

  revisit_queue <- cids
  revisit_count <- stats::setNames(rep(0L, length(cids)), cids)
  max_revisits_per_comp <- 3L

  old_hashes <- stats::setNames(character(length(cids)), cids)
  for (cid in cids) {
    old_hashes[[cid]] <- state$selected_revisions[[cid]]$binding$binding_hash %||% ""
  }

  while (length(revisit_queue) > 0L) {
    cid <- revisit_queue[1L]
    revisit_queue <- revisit_queue[-1L]

    old_cid_hash <- state$selected_revisions[[cid]]$binding$binding_hash %||% ""

    state <- process_program_component(
      state = state,
      component_id = cid,
      execute = execute,
      max_program_repair_rounds = max_program_repair_rounds
    )

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

      requeue <- requeue_components(state$graph, old_hashes, new_hashes, runtime_deferred = deferred_cids)
      requeue <- setdiff(requeue, cid)

      for (rq_cid in requeue) {
        if (revisit_count[[rq_cid]] < max_revisits_per_comp && !rq_cid %in% revisit_queue) {
          revisit_count[[rq_cid]] <- revisit_count[[rq_cid]] + 1L
          revisit_queue <- c(revisit_queue, rq_cid)
          signal_immediate_coordinator_event("component_revisited", rq_cid)
        }
      }
      old_hashes <- new_hashes
    }
  }

  if (length(state$selected_revisions) > 0L && is.null(state$active_revision)) {
    state$active_revision <- state$selected_revisions[[length(state$selected_revisions)]]$revision_id
  }

  structure(state, class = c("sas2r_program_pipeline_result", "sas2r_migration_state", "list"))
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
          # Local paths, for rebuilding the bounded comparison report at
          # repair time; they stay in local artifacts and never reach a model.
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
#' @param max_bundle_repair_rounds Maximum repair rounds for full bundle (default 2L).
#' @param execute Logical indicating if execution is enabled (default TRUE).
#' @param ... Additional arguments passed to normalize_migration_state.
#' @return A `sas2r_bundle_pipeline_result` list object.
#' @noRd
run_bundle_pipeline <- function(
  state,
  max_bundle_repair_rounds = 2L,
  execute = TRUE,
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

  round <- 0L
  attempt_seq <- 1L
  attempts_summary <- list()
  repairs <- list()
  selected_attempt <- NULL
  selected_assessment <- NULL
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
      passed = isTRUE(attempt_rec$passed)
    )

    # 2. Assess all outputs
    assessment <- assess_final_outputs(
      contracts = state$output_contracts %||% empty_output_contracts(),
      attempt = attempt_rec,
      graph = state$graph,
      evidence_histories = state$histories,
      comparison_rules = state$comparison_rules %||% state$config$comparison_rules %||% list()
    )

    if (!is.null(assessment$evidence_histories)) {
      state$histories <- assessment$evidence_histories
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
      signal_bundle_event(
        "bundle_attempt_selected",
        attempt_id = attempt_rec$attempt_id,
        round = round,
        status = status
      )
    } else if (inherits(cand_selection, "sas2r_regressive_selection")) {
      is_regression <- TRUE
      signal_bundle_event(
        "bundle_early_stop",
        attempt_id = attempt_rec$attempt_id,
        round = round,
        reason = "regressive_attempt"
      )
    }

    # Record attempt in summary
    target_count <- if (!is.null(assessment$targets)) length(assessment$targets) else 0L
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
    if (round >= max_bundle_repair_rounds) {
      stop_reason <- "max_bundle_repair_rounds_reached"
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

    # 5. Build causal repair packet
    packet <- build_bundle_repair_packet(
      state = state,
      attempt = attempt_rec,
      assessment = assessment,
      previous_disposition = latest_diagnosis
    )

    primary_cid <- packet$primary_component_id
    if (is.null(primary_cid) || !primary_cid %in% names(state$selected_revisions)) {
      stop_reason <- "no_causal_evidence"
      break
    }

    primary_rev <- state$selected_revisions[[primary_cid]]

    bundle_ev <- list(
      bundle_id = attempt_rec$attempt_id,
      execution_id = attempt_rec$attempt_id,
      failing_outputs = vapply(packet$failed_targets, function(t) t$target_key, character(1)),
      error = attempt_rec$condition$message %||% packet$bounded_diagnostics$condition_message %||% "(execution error)",
      log = packet$bounded_diagnostics$log_excerpt %||% "(none)"
    )

    # Bounded comparison reports: the sanctioned, capped surface that may
    # carry example differences. Each failed dataset target with both files
    # still on disk gets one, registered for the fixer's
    # read_comparison_report tool; only the report id enters the prompt, and
    # the examples cross the boundary solely when the model requests them.
    report_registry <- new.env(parent = emptyenv())
    read_target_frame <- function(path) {
      if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
      tryCatch(
        switch(tolower(tools::file_ext(path)),
               rds = readRDS(path),
               xpt = haven::read_xpt(path),
               sas7bdat = haven::read_sas(path),
               NULL),
        error = function(e) NULL
      )
    }

    outputs_ev <- lapply(packet$failed_targets, function(t) {
      # Model boundary: the raw mismatch table holds exact cell values with
      # row numbers and must never reach the fixer. The redacted digest the
      # gate computed (diff_digest: names, counts, magnitudes, pattern hints),
      # plus structural and cosmetic summaries, is the whole difference
      # evidence an LLM may see. The full table stays in the local assessment
      # and report for human review.
      diffs <- t$differences
      if (is.list(diffs)) diffs$mismatches <- NULL
      if (identical(t$kind, "dataset")) {
        ref_data <- read_target_frame(t$reference_path)
        cand_data <- read_target_frame(t$candidate_path)
        if (!is.null(ref_data) && !is.null(cand_data)) {
          rep <- tryCatch(
            compare_aligned_outputs(ref_data, cand_data, target = list(
              target_id = t$target_key,
              logical_dataset = t$target_key,
              role = "output",
              contributing_unit_ids = integer()
            )),
            error = function(e) NULL
          )
          if (!is.null(rep)) {
            assign(rep$report_id, rep, envir = report_registry)
            if (is.list(diffs)) diffs$comparison_report_id <- rep$report_id
          }
        }
      }
      list(
        target_key = t$target_key,
        kind = t$kind,
        status = t$status,
        checks = t$checks,
        differences = diffs
      )
    })

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
        bundle = bundle_ev,
        outputs = outputs_ev,
        mode = "bundle",
        llm = state$fixer_llm,
        usage = state$usage_budget,
        paths = state$paths,
        project = state$project,
        config = state$config,
        round = round + 1L,
        attempt_id = attempt_rec$attempt_id,
        evidence_ids = packet$evidence_ids,
        report_registry = report_registry
      ),
      error = function(e) list(status = "repair_failed", message = conditionMessage(e))
    )

    if (is.null(fixed_rev) || identical(fixed_rev$status, "repair_failed")) {
      stop_reason <- "repair_failed"
      break
    }

    # Check for identical / no-op patch
    is_identical_code <- identical(trimws(fixed_rev$r_code %||% ""), trimws(primary_rev$r_code %||% ""))
    is_identical_patch <- !is.null(fixed_rev$patch_hash) && !is.null(primary_rev$contract$patch_hash) && identical(fixed_rev$patch_hash, primary_rev$contract$patch_hash)
    has_helper_patch <- !is.null(fixed_rev$bundle_helper_patch)

    if ((is_identical_code || is_identical_patch) && !has_helper_patch) {
      stop_reason <- "identical_patch"
      break
    }

    # 7. Apply patch & invalidate bindings
    if (has_helper_patch) {
      hp <- fixed_rev$bundle_helper_patch
      hp_path <- hp$path %||% "sas2r-helpers.R"
      hp_content <- hp$content %||% ""

      if (!is.null(state$paths) && !is.null(state$paths$state)) {
        hp_dest <- file.path(state$paths$state, hp_path)
        dir.create(dirname(hp_dest), recursive = TRUE, showWarnings = FALSE)
        writeLines(hp_content, hp_dest)
        state$runtime$helpers <- hp_dest
      }

      new_h_hash <- migration_hash(hp_content)
      for (cid in names(state$selected_revisions)) {
        c_rev <- state$selected_revisions[[cid]]
        old_b <- c_rev$contract$binding %||% c_rev$binding
        new_b <- new_component_binding(
          source_hash = old_b$source_hash %||% migration_hash(c_rev$contract$sas_text %||% ""),
          r_hash = old_b$r_hash %||% migration_hash(c_rev$r_code %||% ""),
          helper_hash = new_h_hash,
          prompt_skill_hash = old_b$prompt_skill_hash %||% migration_hash("fixer"),
          dependency_closure_hash = old_b$dependency_closure_hash %||% migration_hash("closure")
        )
        c_rev$binding <- new_b
        if (!is.null(c_rev$contract)) c_rev$contract$binding <- new_b
        state$selected_revisions[[cid]] <- c_rev
        state$histories[[cid]] <- activate_component_binding(state$histories[[cid]], new_b)

        check_program_revision(c_rev$r_path, contract = c_rev$contract)
        if (!is.null(state$reviewer_llm)) {
          ctx <- list(
            component_id = cid,
            revision_id = c_rev$revision_id,
            r_code = c_rev$r_code,
            r_path = c_rev$r_path,
            contract = c_rev$contract,
            binding = new_b,
            history = state$histories[[cid]],
            sas_source = c_rev$contract$sas_text %||% "",
            project = state$project,
            config = state$config %||% list()
          )
          rev_res <- review_program_revision(
            c_rev,
            context = ctx,
            llm = state$reviewer_llm,
            usage = state$usage_budget,
            paths = state$paths,
            round = round + 1L,
            history = state$histories[[cid]]
          )
          if (!is.null(rev_res$history)) state$histories[[cid]] <- rev_res$history
        }
      }
    }

    if (!is_identical_code) {
      state$selected_revisions[[primary_cid]] <- fixed_rev
      new_b <- fixed_rev$contract$binding %||% fixed_rev$binding
      state$histories[[primary_cid]] <- activate_component_binding(state$histories[[primary_cid]], new_b)

      checks <- check_program_revision(fixed_rev$r_path, contract = fixed_rev$contract)
      if (!isTRUE(checks$pass)) fixed_rev$status <- "check_failed"

      if (!is.null(state$reviewer_llm)) {
        ctx <- list(
          component_id = primary_cid,
          revision_id = fixed_rev$revision_id,
          r_code = fixed_rev$r_code,
          r_path = fixed_rev$r_path,
          contract = fixed_rev$contract,
          binding = new_b,
          history = state$histories[[primary_cid]],
          sas_source = fixed_rev$contract$sas_text %||% "",
          project = state$project,
          config = state$config %||% list()
        )
        rev_res <- review_program_revision(
          fixed_rev,
          context = ctx,
          llm = state$reviewer_llm,
          usage = state$usage_budget,
          paths = state$paths,
          round = round + 1L,
          history = state$histories[[primary_cid]]
        )
        if (!is.null(rev_res$history)) state$histories[[primary_cid]] <- rev_res$history
      }
      state$selected_revisions[[primary_cid]] <- fixed_rev
    }

    # Record repair
    repair_rec <- list(
      round = round + 1L,
      component_id = fixed_rev$component_id %||% primary_cid,
      revision_id = fixed_rev$revision_id,
      diagnosis = fixed_rev$diagnosis,
      summary = fixed_rev$summary,
      patch_hash = fixed_rev$patch_hash,
      helper_patch = fixed_rev$bundle_helper_patch,
      changed_interfaces = fixed_rev$changed_interfaces,
      affected_outputs = fixed_rev$affected_outputs,
      spend_usd = fixed_rev$spend_usd %||% 0
    )
    repairs[[length(repairs) + 1L]] <- repair_rec
    latest_diagnosis <- fixed_rev$diagnosis

    signal_bundle_event(
      "bundle_fixer_completed",
      attempt_id = attempt_rec$attempt_id,
      round = round + 1L,
      component_id = primary_cid,
      cost = fixed_rev$spend_usd
    )

    round <- round + 1L
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
    assessment = selected_assessment %||% latest_assessment,
    repairs = repairs,
    # Merge onto what the program pipeline recorded (e.g. agent_degraded)
    # instead of clobbering it.
    diagnostics = utils::modifyList(
      state$diagnostics %||% list(),
      list(stop_reason = stop_reason, latest_diagnosis = latest_diagnosis),
      keep.null = TRUE
    ),
    project = state$project,
    graph = state$graph,
    schedule = state$schedule,
    paths = state$paths,
    output_contracts = state$output_contracts,
    selected_revisions = state$selected_revisions,
    histories = state$histories,
    runtime = state$runtime,
    usage_budget = state$usage_budget,
    config = state$config,
    events = state$events
  )

  structure(res, class = c("sas2r_bundle_pipeline_result", "sas2r_migration_state", "list"))
}
