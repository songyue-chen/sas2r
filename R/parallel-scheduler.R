# Ready components may draft concurrently. Selection changes are serialized
# around a complete settle transaction, including helper-consumer regression.
parallel_component_order <- function(graph, ids) {
  providers <- stats::setNames(lapply(ids, function(cid) intersect(dependency_closure(graph, cid), ids)), ids)
  height <- stats::setNames(rep(0L, length(ids)), ids)
  for (iteration in seq_along(ids)) {
    next_height <- vapply(ids, function(cid) {
      consumers <- ids[vapply(providers, function(deps) cid %in% deps, logical(1))]
      if (length(consumers)) 1L + max(height[consumers]) else 0L
    }, integer(1))
    if (identical(next_height, height)) break
    height <- next_height
  }
  ids[order(-height, seq_along(ids))]
}

parallel_binding_hashes <- function(state, ids) {
  stats::setNames(vapply(ids, function(cid)
    state$selected_revisions[[cid]]$binding$binding_hash %||%
      state$selected_revisions[[cid]]$contract$binding$binding_hash %||% "", ""), ids)
}

parallel_save_checkpoint <- function(pool) {
  if (!is.null(pool$state$resume_fingerprint))
    write_migration_checkpoint(pool$state, pool$state$resume_fingerprint)
}

parallel_dependency_findings <- function(state, cid) {
  contract <- state$selected_revisions[[cid]]$contract
  reported <- unique(trimws(c(contract$suspected_dependencies, contract$discovered_dependencies)))
  # These schema fields also contain prose assumptions. Only plain identifiers,
  # lib.member names, %macro names and &variable names can request reconciliation.
  # Preserve other observations in the contract and existing semantic reviews.
  reported <- reported[grepl("^(%?[A-Za-z_][A-Za-z0-9_]*|&[A-Za-z_][A-Za-z0-9_]*[.]?|[A-Za-z_][A-Za-z0-9_]*[.][A-Za-z_][A-Za-z0-9_]*)$", reported)]
  # Use the scanner's canonical SAS macro classification, including supplied
  # autocall macros. Environment resources and configured path-only symbols do
  # not add a producer to the schedule; their translation still needs review.
  reported <- filter_dependency_resources(reported, state$project, cid)
  # A confirmation of an existing provider is not a graph correction. Anything
  # else needs source-based reconciliation; no guessed independence/order.
  graph <- state$graph
  owner_nodes <- graph$nodes$node_id[graph$nodes$component_id == cid]
  incoming <- graph$edges[graph$edges$to %in% owner_nodes &
    graph$edges$resolution %in% c("resolved", "external"), , drop = FALSE]
  providers <- graph$nodes$component_id[match(incoming$from, graph$nodes$node_id)]
  known <- unique(c(dependency_closure(graph, cid), providers, incoming$detail))
  macros <- sub("^macro__", "", known[startsWith(known, "macro__")])
  known <- tolower(c(known, macros, paste0("%", macros)))
  reported[!tolower(trimws(reported)) %in% known]
}

parallel_affected_components <- function(graph, cid, ids) {
  union(cid, ids[vapply(ids, function(id) cid %in% dependency_closure(graph, id), logical(1))])
}

run_parallel_program_pipeline <- function(state, ids, execute, repair_cap) {
  # A resumed run must reassess saved observations against the current source
  # and resolver. The previous run's report retains its original blockers.
  pool <- parallel_new_pool(state)
  on.exit(parallel_stop_pool(pool), add = TRUE)
  tryCatch({
  pending <- parallel_component_order(state$graph, ids)
  providers <- translation_providers(state$graph, state$schedule, ids)
  done <- active <- character()
  drafts <- list()
  settle <- NULL
  processed <- lapply(state$selected_revisions[state$resumed_components %||% character()], component_content_identity)
  counts <- state$revisit_counts %||% stats::setNames(rep(0L, length(ids)), ids)
  pool$state$revisit_counts <- counts
  old_hashes <- parallel_binding_hashes(state, ids)
  limit <- state$parallel$effective
  while (length(pending) || length(active)) {
    completed <- parallel_poll(pool)
    for (entry in completed) {
      cid <- entry$job$component_id
      pool$state <- record_dependency_finding(pool$state, cid, entry$result$dependency_findings)
      if (entry$job$kind == "draft") {
        drafts[[cid]] <- entry$result
      } else {
        pool$state <- parallel_apply_result(pool$state, entry$result)
        pool$state$component_stage[[cid]] <- if (is.null(pool$state$diagnostics$component_failures[[cid]])) "settled" else "failed"
        done <- union(done, cid)
        active <- setdiff(active, cid)
        settle <- NULL
        processed[[cid]] <- component_content_identity(pool$state$selected_revisions[[cid]])
        hashes <- parallel_binding_hashes(pool$state, ids)
        deferred <- ids[vapply(ids, function(id) !is.null(current_component_evidence(
          pool$state$histories[[id]])$runtime_deferred), logical(1))]
        revisits <- setdiff(intersect(done, requeue_components(state$graph, old_hashes, hashes,
          runtime_deferred = deferred, waiting_on = lapply(pool$state$selected_revisions,
            function(rev) rev$smoke$waiting_on))), cid)
        revisits <- setdiff(revisits, names(pool$state$diagnostics$component_failures))
        for (id in revisits) {
          count <- counts[[id]] %||% 0L
          if (!is.na(count) && count < 3L) {
            counts[[id]] <- count + 1L
            pending <- union(pending, id)
            done <- setdiff(done, id)
          } else pool$state$diagnostics$revisit_deferred <- union(
            pool$state$diagnostics$revisit_deferred, id)
        }
        pool$state$revisit_counts <- counts
        old_hashes <- hashes
        parallel_save_checkpoint(pool)
      }
    }
    failed <- parallel_collect_component_failures(pool)
    if (length(failed)) {
      done <- union(done, failed)
      active <- setdiff(active, failed)
      pending <- setdiff(pending, failed)
      drafts[intersect(names(drafts), failed)] <- NULL
      if (!is.null(settle) && !settle %in% names(pool$jobs)) settle <- NULL
      parallel_save_checkpoint(pool)
    }
    if (length(pool$failures)) {
      # Drain admitted sibling work without dispatching more calls. A running
      # settlement owns the shared selection until it returns; only then save
      # other completed drafts as drafts, not as settled/accepted evidence.
      if (length(pool$jobs)) {
        Sys.sleep(0.025)
        next
      }
      for (cid in names(drafts)) {
        pool$state <- parallel_apply_result(pool$state, drafts[[cid]])
        pool$state$component_stage[[cid]] <- "draft"
      }
      parallel_abort_failure(pool)
    }
    # Keep a repair's snapshot stable until its entire transaction has settled.
    # Other workers can finish calls, but their drafts remain private meanwhile.
    if (is.null(settle) && length(drafts)) {
      cid <- names(drafts)[1L]
      pool$state <- parallel_apply_result(pool$state, drafts[[cid]])
      drafts[[cid]] <- NULL
      pool$state$component_stage[[cid]] <- "draft"
      parallel_save_checkpoint(pool)
      settle <- parallel_start_job(pool, pool$state, cid, "settle", execute, repair_cap)$id
    }
    ready <- pending[vapply(pending, function(cid) all(providers[[cid]] %in% done), logical(1))]
    for (cid in ready) {
      if (length(active) >= limit) break
      content <- component_content_identity(pool$state$selected_revisions[[cid]])
      revisit <- !is.null(content) && identical(processed[[cid]], content)
      if (revisit && !is.null(settle)) next
      kind <- if (revisit) "revisit" else "draft"
      job <- parallel_start_job(pool, pool$state, cid, kind, execute, repair_cap)
      if (revisit) settle <- job$id
      active <- union(active, cid)
      pending <- setdiff(pending, cid)
    }
    if (!length(active) && length(pending)) {
      # A source cycle was already classified by readiness. Unexpected graph
      # omissions are not recoverable here: do not silently discard programs.
      cli::cli_abort("Translation scheduler cannot account for pending components: {paste(pending, collapse = ', ')}",
        class = "sas2r_pipeline_coverage_error")
    }
    if (length(active)) Sys.sleep(0.025)
  }
  state <- pool$state
  state <- finalize_parallel_component_reviews(state, pool)
  structure(state, class = c("sas2r_program_pipeline_result", "sas2r_migration_state", "list"))
  }, error = function(error) {
    error$migration_state <- pool$state
    stop(error)
  })
}

finalize_parallel_component_reviews <- function(state, pool = parallel_new_pool(state)) {
  on.exit(parallel_stop_pool(pool), add = TRUE)
  ids <- setdiff(intersect(state$schedule$component_id, names(state$selected_revisions)),
    names(state$diagnostics$component_failures))
  signal_immediate_coordinator_event("component_review_checkpoint_started", "all components")
  reused <- completed <- unavailable <- 0L
  pending <- character()
  for (cid in ids) {
    cached <- review_component_revision(state, cid, reuse_only = TRUE)
    if (is.null(cached)) pending <- c(pending, cid) else {
      state <- promote_reviewed_smoke(state, cid)
      reused <- reused + 1L
    }
  }
  # Every review receives the same frozen source/code/helper selection.
  frozen <- state
  pool$state <- state
  while (length(pending) || length(pool$jobs)) {
    while (!length(pool$failures) && length(pending) && length(pool$jobs) < state$parallel$effective) {
      cid <- pending[1L]
      pending <- pending[-1L]
      parallel_start_job(pool, frozen, cid, "review", FALSE, 0L)
    }
    for (entry in parallel_poll(pool)) {
      pool$state <- parallel_apply_result(pool$state, entry$result)
      if (length(entry$result$dependency_findings)) pool$state <- record_dependency_finding(
        pool$state, entry$job$component_id, entry$result$dependency_findings)
      if (identical(entry$result$review$verdict, "review_unavailable")) unavailable <- unavailable + 1L else
        completed <- completed + 1L
      parallel_save_checkpoint(pool)
    }
    parallel_collect_component_failures(pool)
    if (!length(pool$jobs)) parallel_abort_failure(pool)
    if (length(pool$jobs)) Sys.sleep(0.025)
  }
  signal_immediate_coordinator_event("component_review_checkpoint_completed", "all components",
    reason = sprintf("%d reviewed, %d reused, %d unavailable", completed, reused, unavailable))
  pool$state$diagnostics[["parallel"]]$observed <- parallel_observations(pool)
  pool$state
}
