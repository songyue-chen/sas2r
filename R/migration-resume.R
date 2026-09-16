# Source bytes and revision records are shared by generation and resume. An old
# report is evidence, not a recipe for reconstructing a generated program path.
RESUME_CHECKPOINT_VERSION <- 6L

component_source_text <- function(graph, component_id) {
  if (is.null(graph$nodes) || !nrow(graph$nodes)) return("")
  nodes <- graph$nodes[graph$nodes$component_id == component_id, , drop = FALSE]
  sources <- unique(nodes$source_file[!is.na(nodes$source_file)])
  sources <- sources[nzchar(sources)]
  paste(vapply(sources, function(f) {
    if (!file.exists(f)) return("")
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
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

migration_resume_fingerprint <- function(state) {
  roles <- c("translator", "reviewer", "fixer")
  skills <- agent_skill_catalog()
  llm <- state$translator_llm
  source_outputs <- state$output_contracts
  source_outputs$reference_path <- NULL
  migration_hash(list(
    version = RESUME_CHECKPOINT_VERSION,
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
  if (!identical(checkpoint$version, RESUME_CHECKPOINT_VERSION)) return(invalidate("checkpoint uses an older planning policy"))
  if (!identical(checkpoint$fingerprint, fingerprint)) return(invalidate("sources, inputs, configuration, or worker settings changed"))
  revisions <- checkpoint$selected_revisions
  # Missing or locally edited artifacts are cheap to regenerate. Do not rebuild
  # paths from revision labels, which need not match the on-disk directory name.
  intact <- length(revisions) > 0L && all(vapply(revisions, function(rev) {
    if (!is.null(rev$agent_status) && !is.na(rev$agent_status) && rev$agent_status != "ok") return(FALSE)
    !is.null(rev$r_path) && file.exists(rev$r_path) &&
      identical(paste(readLines(rev$r_path, warn = FALSE), collapse = "\n"), rev$r_code)
  }, logical(1)))
  if (!intact) return(invalidate("generated revisions are missing, changed, or incomplete"))
  state$selected_revisions <- revisions
  state$histories <- checkpoint$histories
  writeLines(checkpoint$helper_code, state$runtime$helpers)
  state$resumed_components <- names(revisions)
  state$diagnostics <- checkpoint$diagnostics
  state$diagnostics$resume_invalidated <- NULL
  state$diagnostics$resumed_components <- names(revisions)
  state
}

write_migration_checkpoint <- function(state, fingerprint) {
  checkpoint <- list(
    version = RESUME_CHECKPOINT_VERSION,
    fingerprint = fingerprint,
    selected_revisions = state$selected_revisions,
    histories = state$histories,
    helper_code = paste(readLines(file.path(state$bundle_dir, "runtime", "sas2r-helpers.R"), warn = FALSE), collapse = "\n"),
    diagnostics = state$diagnostics
  )
  atomic_write_file(function(path) saveRDS(checkpoint, path),
                    file.path(state$paths$state, "resume.rds"))
  invisible(NULL)
}
