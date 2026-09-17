# Report-only observations. Neither function is part of authoring context,
# candidate acceptance, a repair queue, or a reference comparison.
attempt_change_observation <- function(previous, current, target_map = list()) {
  result <- list(previous_attempt_id = previous$attempt_id, attempt_id = current$attempt_id,
    status = "not_comparable", reason = "missing or different execution context", changes = list())
  same <- function(field) !is.null(previous[[field]]) && !is.null(current[[field]]) &&
    identical(previous[[field]], current[[field]])
  has_hash <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  source_hashes <- current$execution_context$sources
  environment <- unlist(current$execution_context$environment)
  identified <- length(source_hashes) > 0L && all(vapply(source_hashes, has_hash, logical(1))) &&
    length(environment) > 0L && !anyNA(environment) && !any(environment == "unknown")
  if (!identified || is.null(previous) || isTRUE(previous$deferred) || isTRUE(current$deferred) ||
      !same("execution_context") || !same("execution_order") || !same("executed_component_ids") ||
      !same("input_hashes_before") || !same("input_hashes_after") ||
      !identical(current$input_hashes_before, current$input_hashes_after) ||
      is.null(previous$output_hashes) || is.null(current$output_hashes)) return(result)
  changed_components <- names(current$revision_manifest)[vapply(names(current$revision_manifest), function(cid)
    !identical(previous$revision_manifest[[cid]]$r_hash, current$revision_manifest[[cid]]$r_hash) ||
    !identical(previous$revision_manifest[[cid]]$revision_id, current$revision_manifest[[cid]]$revision_id), logical(1))]
  declared <- unique(unlist(lapply(current$revision_manifest[changed_components], `[[`, "affected_outputs")))
  before <- previous$output_hashes
  after <- current$output_hashes
  for (path in union(names(before), names(after))) {
    prior_present <- path %in% names(before)
    now_present <- path %in% names(after)
    status <- if ((prior_present && !has_hash(before[[path]])) ||
                  (now_present && !has_hash(after[[path]]))) "unknown" else
      if (!prior_present) "added" else if (!now_present) "removed" else
      if (!identical(before[[path]], after[[path]])) "byte_changed" else "unchanged"
    if (status == "unchanged") next
    target <- target_map[[path]]
    result$changes[[length(result$changes) + 1L]] <- list(path = path, status = status,
      target_key = target, declared_affected = if (is.null(target)) "unknown" else target %in% declared)
  }
  result$status <- "comparable"
  result$reason <- "File bytes only: changes may be legitimate or reflect metadata/timestamps; no semantic verdict."
  result
}

repair_report_observations <- function(state, paths) {
  records <- resume_migration_attempts(paths)$completed_attempts
  records <- Filter(function(x) identical(x$kind, "bundle"), records)
  if (length(records)) records <- records[order(vapply(records, function(x) as.integer(x$sequence), integer(1)))]
  mapping <- list()
  root <- state$selected_attempt$attempt_dir
  if (!is.null(root)) for (target in state$assessment$targets %||% list()) {
    path <- target$candidate_path
    prefix <- paste0(root, "/")
    if (is.character(path) && length(path) == 1L && !is.na(path) && startsWith(path, prefix)) {
      mapping[[substring(path, nchar(prefix) + 1L)]] <- target$target_key
    }
  }
  changes <- lapply(seq_along(records), function(i)
    attempt_change_observation(if (i > 1L) records[[i - 1L]] else NULL, records[[i]], mapping))
  notices <- lapply(state$selected_revisions, function(rev) list(
    revision_id = rev$revision_id,
    direct_io = rev$checks$warnings[grepl("direct_io", rev$checks$warnings)],
    nonlocal_assignment = rev$checks$warnings[grepl("nonlocal_assignment", rev$checks$warnings)],
    dependency_symbols = rev$dependency_notices %||% character(),
    mechanical_retry = rev$mechanical_retry))
  list(output_changes = changes, code_notices = notices)
}
