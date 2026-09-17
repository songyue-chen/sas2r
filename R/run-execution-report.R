# Report projection only. Attempted execution and the current handoff's
# verification are separate; neither changes selection or evidence gates.
bundle_execution_report <- function(state) {
  records <- resume_migration_attempts(state$paths)$completed_attempts
  records <- Filter(function(r) identical(r$kind, "bundle"), records)
  if (length(records)) records <- records[order(vapply(records, function(r) as.integer(r$sequence), 1L))]
  helper <- state$runtime$helpers
  helper_hash <- if (!is.null(helper) && file.exists(helper)) unname(cli::hash_file_sha256(helper)) else NULL
  ids <- unique(c(state$schedule$component_id, names(state$selected_revisions)))
  stats::setNames(lapply(ids, function(cid) {
    observations <- lapply(records, function(record) {
      failed <- record$condition$component_id
      status <- if (isTRUE(record$deferred)) "deferred" else
        if (cid %in% record$executed_component_ids) "passed" else
        if (identical(cid, failed)) "failed" else
        if (length(failed) && cid %in% record$execution_order) paste("not reached after", failed) else "unexecuted"
      # Include the preceding programs too: a changed earlier program can
      # change the shared state seen by this component, even without a graph edge.
      position <- match(cid, record$execution_order)
      prefix <- if (!is.na(position)) record$execution_order[seq_len(position)] else cid
      context_ids <- unique(c(prefix, unlist(lapply(prefix, function(id) dependency_closure(state$graph, id)))))
      same <- !is.null(helper_hash) && identical(record$helper_hash, helper_hash) &&
        all(vapply(context_ids, function(id) {
          rev <- state$selected_revisions[[id]]
          saved <- record$revision_manifest[[id]]
          !is.null(saved$revision_id) && identical(saved$revision_id, rev$revision_id) &&
            identical(saved$r_hash, migration_hash(revision_code(rev))) &&
            !is.null(saved$source_hash) && identical(saved$source_hash,
              rev$contract$binding$source_hash %||% rev$binding$source_hash)
        }, logical(1)))
      list(attempt_id = record$attempt_id, status = status, matches_current_revision = same,
        selected = identical(record$attempt_dir, state$selected_attempt$attempt_dir),
        revision = record$revision_manifest[[cid]], helper_hash = record$helper_hash,
        record_path = file.path(record$attempt_dir, "record.json"),
        condition = record$condition, stdout_path = record$stdout_path, stderr_path = record$stderr_path)
    })
    current <- Filter(function(x) isTRUE(x$matches_current_revision), observations)
    selected <- Filter(function(x) isTRUE(x$selected) && isTRUE(x$matches_current_revision), observations)
    latest <- if (length(observations)) observations[[length(observations)]] else NULL
    latest_current <- if (length(current)) current[[length(current)]] else NULL
    list(attempts = observations, current_revision = latest_current,
      selected_attempt = if (length(selected)) selected[[length(selected)]] else NULL,
      summary = paste0("latest attempt: ", if (is.null(latest)) "none" else
        paste(latest$attempt_id, latest$status), "; current revision: ",
        latest_current$status %||% "unexecuted (no matching attempt)",
        "; selected attempt: ", if (!length(selected)) "none matching" else selected[[length(selected)]]$attempt_id))
  }), ids)
}
