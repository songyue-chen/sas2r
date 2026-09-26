# Source bytes and revision records are shared by generation and resume. An old
# report is evidence, not a recipe for reconstructing a generated program path.
RESUME_CHECKPOINT_VERSION <- 10L

component_source_text <- function(graph, component_id) {
  if (is.null(graph$nodes) || !nrow(graph$nodes)) return("")
  nodes <- graph$nodes[graph$nodes$component_id == component_id, , drop = FALSE]
  sources <- unique(nodes$source_file[!is.na(nodes$source_file)])
  sources <- sources[nzchar(sources)]
  paste(vapply(sources, function(f) {
    if (!file.exists(f)) return("")
    text <- paste(read_sas_source(f), collapse = "\n")
    if (any(nodes$type == "macro")) {
      units <- sas_units(sas_statements(text))
      defs <- extract_macro_defs(units)
      name <- sub("^macro__", "", component_id)
      ids <- defs$unit_id[defs$name == name]
      return(format_sas_statements(units$text[units$unit_id %in% ids]))
    }
    text
  }, character(1)), collapse = "\n")
}

migration_resume_fingerprint <- function(state, version = RESUME_CHECKPOINT_VERSION) {
  roles <- c("translator", "reviewer", "fixer")
  skills <- agent_skill_catalog()
  llm <- state$translator_llm
  source_outputs <- state$output_contracts
  source_outputs$reference_path <- NULL
  migration_hash(list(
    version = version,
    package_version = as.character(utils::packageVersion("sas2r")),
    baseline = state$baseline$manifest[c("unit_id", "file", "code", "tier", "flags")],
    sources = stats::setNames(lapply(state$schedule$component_id, function(cid) {
      component_source_text(state$graph, cid)
    }), state$schedule$component_id),
    graph = state$graph,
    inputs = state$input_manifest %||% input_hash_manifest(state$project),
    config = c(scan_config_fields(state$config),
      state$config[c("dialect", "allowlist", "search_docs")]),
    outputs = source_outputs,
    helper = paste(readLines(state$runtime$helpers, warn = FALSE), collapse = "\n"),
    workers = lapply(roles, worker_binding_hash, skills = skills,
                     project_dir = state$project$project_dir),
    llm = llm[c("provider", "model", "endpoint", "api_version", "model_parameters")],
    evidence_policy = state$agent_evidence
  ))
}

restore_migration_checkpoint <- function(state, fingerprint) {
  path <- file.path(state$paths$state, "resume.rds")
  if (!file.exists(path)) return(state)
  invalidate <- function(reason) {
    state$diagnostics$resume_invalidated <- reason
    cli::cli_inform(c("Resume checkpoint not reused: {reason}",
      "i" = "Translation will regenerate revisions and may make new provider calls."),
      class = "sas2r_resume_invalidated")
    state
  }
  checkpoint <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(checkpoint)) return(invalidate("checkpoint is unreadable"))
  legacy <- identical(checkpoint$version, 8L)
  if (!legacy && !identical(checkpoint$version, RESUME_CHECKPOINT_VERSION)) return(invalidate("checkpoint uses an older planning policy"))
  expected <- if (legacy) migration_resume_fingerprint(state, version = 8L) else fingerprint
  if (!identical(checkpoint$fingerprint, expected)) return(invalidate("sources, inputs, configuration, or worker settings changed"))
  revisions <- checkpoint$selected_revisions
  # Missing or locally edited artifacts are cheap to regenerate. Do not rebuild
  # paths from revision labels, which need not match the on-disk directory name.
  intact <- vapply(revisions, function(rev) {
    if (!is.null(rev$agent_status) && !is.na(rev$agent_status) && rev$agent_status != "ok") return(FALSE)
    !is.null(rev$r_path) && file.exists(rev$r_path) &&
      identical(paste(readLines(rev$r_path, warn = FALSE), collapse = "\n"), rev$r_code)
  }, logical(1))
  if (!any(intact)) return(invalidate("generated revisions are missing, changed, or incomplete"))
  revisions <- revisions[intact]
  state$selected_revisions <- revisions
  state$histories <- checkpoint$histories[names(revisions)]
  for (cid in names(state$histories)) {
    h <- state$histories[[cid]]
    rid <- which(vapply(h$revisions, function(r) identical(r$revision_id, h$active_revision_id), logical(1)))
    if (!length(rid)) next
    rid <- rid[1L]
    rev <- h$revisions[[rid]]
    if (length(rev$level) && rev$level %in% c("output_verified", "reference_validated")) {
      rev$level <- "runtime_verified"
      rev$coverage <- character()
      rev$basis_ids <- character()
      h$revisions[[rid]] <- rev
      state$histories[[cid]] <- h
    }
  }
  state$repair_counts <- checkpoint$repair_counts
  state$revisit_counts <- checkpoint$revisit_counts %||% stats::setNames(
    rep(NA_integer_, nrow(state$schedule)), state$schedule$component_id)
  writeLines(checkpoint$helper_code, state$runtime$helpers)
  state$component_stage <- checkpoint$component_stage[names(revisions)]
  state$resumed_components <- if (is.null(checkpoint$component_stage)) names(revisions) else
    intersect(names(revisions), names(Filter(function(stage) identical(stage, "settled"), checkpoint$component_stage)))
  # Preserve cumulative repair audit/counters, not prior transient failures.
  for (field in c("bundle_repair", "rejected_repairs")) {
    if (is.null(state$diagnostics[[field]])) state$diagnostics[[field]] <- checkpoint$diagnostics[[field]]
  }
  # Prior failures remain in the prior report, not in this run's current diagnostics.
  state$diagnostics$resume_dropped_components <- names(intact)[!intact]
  if (legacy) state$diagnostics$resume_import <- "v8: preserved repairs; previous revisit counts unknown"
  state$diagnostics$resume_invalidated <- NULL
  state$diagnostics$resumed_components <- names(revisions)
  state
}

write_migration_checkpoint <- function(state, fingerprint) {
  if (!is.null(.parallel_worker$client)) {
    parallel_rpc("checkpoint", list(repair_counts = state$repair_counts))
    return(invisible(NULL))
  }
  checkpoint <- list(
    version = RESUME_CHECKPOINT_VERSION,
    fingerprint = fingerprint,
    selected_revisions = state$selected_revisions,
    histories = state$histories,
    # Finalization may prune the original smoke staging directory. Before that
    # point use the working runtime; afterward use the materialized selection.
    helper_code = runtime_helper_code(if (!is.null(state$bundle_dir))
      list(helpers = file.path(state$bundle_dir, "runtime", "sas2r-helpers.R")) else state$runtime),
    repair_counts = state$repair_counts,
    revisit_counts = state$revisit_counts,
    component_stage = state$component_stage,
    diagnostics = state$diagnostics
  )
  atomic_write_file(function(path) saveRDS(checkpoint, path),
                    file.path(state$paths$state, "resume.rds"))
  invisible(NULL)
}
