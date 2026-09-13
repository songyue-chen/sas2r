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
    error = function(e) {
      if (inherits(e, "sas2r_llm_settings_error")) stop(e)
      list(status = "repair_failed", message = conditionMessage(e))
    }
  )

  if (is.null(fixed_rev) || identical(fixed_rev$status, "repair_failed")) {
    stop_reason <- "repair_failed"
    return(list(state = state, applied = FALSE, reason = stop_reason))
  }

  # Check for identical / no-op patch
  is_identical_code <- identical(trimws(fixed_rev$r_code %||% ""), trimws(primary_rev$r_code %||% ""))
  is_identical_patch <- !is.null(fixed_rev$patch_hash) && !is.null(primary_rev$contract$patch_hash) && identical(fixed_rev$patch_hash, primary_rev$contract$patch_hash)
  has_helper_patch <- !is.null(fixed_rev$bundle_helper_patch)

  if ((is_identical_code || is_identical_patch) && !has_helper_patch) {
    stop_reason <- "identical_patch"
    return(list(state = state, applied = FALSE, reason = stop_reason))
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

      c_rev$checks <- check_program_revision(c_rev$r_path, contract = c_rev$contract)
      c_rev$status <- if (isTRUE(c_rev$checks$pass)) "ok" else "check_failed"
      state$selected_revisions[[cid]] <- c_rev
      state$histories[[cid]] <- record_program_checks(state$histories[[cid]], c_rev$checks)
      if (!is.null(state$reviewer_llm)) {
        ctx <- list(
          component_id = cid,
          revision_id = c_rev$revision_id,
          r_code = c_rev$r_code,
          r_path = c_rev$r_path,
          contract = c_rev$contract,
          binding = new_b,
          history = state$histories[[cid]],
          sas_source = component_source_text(state$graph, cid),
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
    fixed_rev$checks <- checks
    fixed_rev$status <- if (isTRUE(checks$pass)) "ok" else "check_failed"
    state$histories[[primary_cid]] <- record_program_checks(state$histories[[primary_cid]], checks)

    if (!is.null(state$reviewer_llm)) {
      ctx <- list(
        component_id = primary_cid,
        revision_id = fixed_rev$revision_id,
        r_code = fixed_rev$r_code,
        r_path = fixed_rev$r_path,
        contract = fixed_rev$contract,
        binding = new_b,
        history = state$histories[[primary_cid]],
        sas_source = component_source_text(state$graph, primary_cid),
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
    prepared <- prepare_program_smoke(state, plan, state$attempt$attempt_dir)
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
bundle_repair_queue <- function(state, attempt, assessment, diagnostic,
                                previous_disposition = NULL) {
  failures <- diagnostic$failures
  target_lineage <- function(key) {
    assessment$lineage_by_target[[key]]$upstream_components %||%
      evidence_for_output_lineage(state$graph, state$histories, key)$upstream_components
  }
  packets <- list()
  for (cid in names(failures)) {
    relevant <- names(assessment$targets)[vapply(names(assessment$targets), function(key) {
      cid %in% target_lineage(key)
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
    packets[[cid]] <- build_bundle_repair_packet(state, failed_attempt, subset, previous_disposition)
    packets[[cid]]$primary_component_id <- cid
    packets[[cid]]$attempt <- failed_attempt
  }
  # After a complete run, every failed target can contribute a repair candidate.
  # After a crash, only targets whose writer actually completed can do so.
  for (key in names(assessment$targets)) {
    target <- assessment$targets[[key]]
    if (isTRUE(target$passed)) next
    lineage <- target_lineage(key)
    if (length(intersect(lineage, names(failures)))) next
    subset <- assessment
    subset$targets <- assessment$targets[key]
    clean_attempt <- attempt
    clean_attempt$condition <- NULL
    clean_attempt$passed <- TRUE
    packet <- build_bundle_repair_packet(state, clean_attempt, subset, previous_disposition)
    graph <- state$graph
    edges <- graph$edges
    writers <- if (!is.null(edges) && nrow(edges)) {
      from <- edges$from[edges$type %in% c("writes_dataset", "writes_output") &
        edges$detail == target$target_key & edges$resolution == "resolved"]
      unique(graph$nodes$component_id[match(from, graph$nodes$node_id)])
    } else character()
    writers <- intersect(writers, names(state$selected_revisions))
    cid <- if (length(writers) == 1L) writers[[1L]] else packet$primary_component_id
    if (is.null(cid) || !cid %in% names(state$selected_revisions)) next
    if (!isTRUE(attempt$passed) && !cid %in% attempt$executed_component_ids) next
    packet$primary_component_id <- cid
    packet$attempt <- clean_attempt
    if (is.null(packets[[cid]])) packets[[cid]] <- packet else {
      packets[[cid]]$failed_targets <- c(packets[[cid]]$failed_targets, packet$failed_targets)
      packets[[cid]]$evidence_ids <- unique(c(packets[[cid]]$evidence_ids, packet$evidence_ids))
    }
  }
  order <- state$schedule$component_id %||% names(state$selected_revisions)
  packets[intersect(order, names(packets))]
}
