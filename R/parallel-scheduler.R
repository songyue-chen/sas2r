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
  events <- current_component_evidence(state$histories[[cid]])$events %||% list()
  reviews <- Filter(function(event) event$type %in% c("review_completed", "review_unavailable"), events)
  latest <- if (length(reviews)) utils::tail(reviews, 1L)[[1L]] else list()
  reported <- unique(c(contract$suspected_dependencies, contract$discovered_dependencies,
    latest$unresolved_dependencies))
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

parallel_defer_finding <- function(state, cid, finding) {
  affected <- parallel_affected_components(state$graph, cid, state$schedule$component_id)
  state$diagnostics$dependency_findings[[cid]] <- list(findings = finding, affected = affected,
    reason = "source_reconciliation_required")
  state$diagnostics$parallel_deferred <- union(state$diagnostics$parallel_deferred, affected)
  state$status <- "blocked"
  state$status_reason <- "Dependency findings require source reconciliation; affected work deferred"
  state
}

run_parallel_program_pipeline <- function(state, ids, execute, repair_cap) {
  pool <- parallel_new_pool(state)
  on.exit(parallel_stop_pool(pool), add = TRUE)
  pending <- parallel_component_order(state$graph, ids)
  providers <- stats::setNames(lapply(ids, function(cid) intersect(dependency_closure(state$graph, cid), ids)), ids)
  unresolved <- state$schedule$component_id[lengths(state$schedule$unresolved_dependencies) > 0L]
  blocked <- unique(c(unresolved, ids[vapply(providers, function(deps) any(deps %in% unresolved), logical(1))]))
  pending <- setdiff(pending, blocked)
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
      if (cid %in% blocked) {
        parallel_job_record(entry$job, "deferred", "source_reconciliation_required")
        next
      }
      if (length(entry$result$dependency_findings)) {
        pool$state <- parallel_defer_finding(pool$state, cid, entry$result$dependency_findings)
        affected <- pool$state$diagnostics$dependency_findings[[cid]]$affected
        blocked <- union(blocked, affected)
        pending <- setdiff(pending, affected)
        done <- setdiff(done, affected)
        active <- setdiff(active, affected)
        drafts[intersect(names(drafts), affected)] <- NULL
        for (id in names(pool$jobs)) {
          job <- pool$jobs[[id]]
          if (!job$component_id %in% affected) next
          if (job$process$is_alive()) job$process$kill_tree()
          parallel_abandon_job(pool, job)
          parallel_job_record(job, "deferred", "source_reconciliation_required")
          pool$jobs[[id]] <- NULL
          if (identical(settle, id)) settle <- NULL
        }
        if (identical(settle, entry$job$id)) settle <- NULL
        parallel_job_record(entry$job, "deferred", "source_reconciliation_required")
        parallel_save_checkpoint(pool)
        next
      }
      if (entry$job$kind == "draft") {
        drafts[[cid]] <- entry$result
      } else {
        pool$state <- parallel_apply_result(pool$state, entry$result)
        pool$state$component_stage[[cid]] <- "settled"
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
      blocked <- union(blocked, pending)
      break
    }
    if (length(active)) Sys.sleep(0.025)
  }
  state <- pool$state
  if (length(blocked)) {
    state$diagnostics$parallel_deferred <- blocked
    state$status <- "blocked"
    state$status_reason <- "Unresolved dependencies or a dependency cycle; affected components require review"
  }
  state <- finalize_parallel_component_reviews(state, pool)
  structure(state, class = c("sas2r_program_pipeline_result", "sas2r_migration_state", "list"))
}

finalize_parallel_component_reviews <- function(state, pool = parallel_new_pool(state)) {
  on.exit(parallel_stop_pool(pool), add = TRUE)
  ids <- setdiff(intersect(state$schedule$component_id, names(state$selected_revisions)),
    state$diagnostics$parallel_deferred)
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
    while (length(pending) && length(pool$jobs) < state$parallel$effective) {
      cid <- pending[1L]
      pending <- pending[-1L]
      parallel_start_job(pool, frozen, cid, "review", FALSE, 0L)
    }
    for (entry in parallel_poll(pool)) {
      pool$state <- parallel_apply_result(pool$state, entry$result)
      if (length(entry$result$dependency_findings)) pool$state <- parallel_defer_finding(
        pool$state, entry$job$component_id, entry$result$dependency_findings)
      if (identical(entry$result$review$verdict, "review_unavailable")) unavailable <- unavailable + 1L else
        completed <- completed + 1L
      parallel_save_checkpoint(pool)
    }
    if (length(pool$jobs)) Sys.sleep(0.025)
  }
  signal_immediate_coordinator_event("component_review_checkpoint_completed", "all components",
    reason = sprintf("%d reviewed, %d reused, %d unavailable", completed, reused, unavailable))
  pool$state$diagnostics[["parallel"]]$observed <- parallel_observations(pool)
  pool$state
}
