# Source bytes and revision records are shared by generation and resume. An old
# report is evidence, not a recipe for reconstructing a generated program path.
component_source_text <- function(graph, component_id) {
  if (is.null(graph$nodes) || !nrow(graph$nodes)) return("")
  nodes <- graph$nodes[graph$nodes$component_id == component_id, , drop = FALSE]
  sources <- unique(nodes$source_file[!is.na(nodes$source_file)])
  sources <- sources[nzchar(sources)]
  paste(vapply(sources, function(f) {
    if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else ""
  }, character(1)), collapse = "\n")
}

migration_resume_fingerprint <- function(state) {
  roles <- c("translator", "reviewer", "fixer")
  skills <- agent_skill_catalog()
  llm <- state$translator_llm
  migration_hash(list(
    version = 1L,
    sources = stats::setNames(lapply(state$schedule$component_id, function(cid) {
      component_source_text(state$graph, cid)
    }), state$schedule$component_id),
    graph = state$graph,
    inputs = input_hash_manifest(state$project),
    config = state$config,
    outputs = state$output_contracts,
    helper = paste(readLines(state$runtime$helpers, warn = FALSE), collapse = "\n"),
    workers = lapply(roles, worker_binding_hash, skills = skills,
                     project_dir = state$project$project_dir),
    llm = llm[c("provider", "model", "endpoint", "api_version", "model_parameters")],
    evidence_policy = state$agent_evidence
  ))
}

restore_migration_checkpoint <- function(state, fingerprint) {
  path <- file.path(state$paths$state, "resume.rds")
  checkpoint <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  if (is.null(checkpoint) || !identical(checkpoint$fingerprint, fingerprint)) return(state)
  revisions <- checkpoint$selected_revisions
  # Missing or locally edited artifacts are cheap to regenerate. Do not rebuild
  # paths from revision labels, which need not match the on-disk directory name.
  intact <- length(revisions) > 0L && all(vapply(revisions, function(rev) {
    if (!is.null(rev$agent_status) && !is.na(rev$agent_status) && rev$agent_status != "ok") return(FALSE)
    !is.null(rev$r_path) && file.exists(rev$r_path) &&
      identical(paste(readLines(rev$r_path, warn = FALSE), collapse = "\n"), rev$r_code)
  }, logical(1)))
  if (!intact) return(state)
  state$selected_revisions <- revisions
  state$histories <- checkpoint$histories
  writeLines(checkpoint$helper_code, state$runtime$helpers)
  state$resumed_components <- names(revisions)
  state$diagnostics <- checkpoint$diagnostics
  state$diagnostics$resumed_components <- names(revisions)
  state
}

write_migration_checkpoint <- function(state, fingerprint) {
  checkpoint <- list(
    fingerprint = fingerprint,
    selected_revisions = state$selected_revisions,
    histories = state$histories,
    helper_code = paste(readLines(file.path(state$bundle_dir, "sas2r-helpers.R"), warn = FALSE), collapse = "\n"),
    diagnostics = state$diagnostics
  )
  atomic_write_file(function(path) saveRDS(checkpoint, path),
                    file.path(state$paths$state, "resume.rds"))
  invisible(NULL)
}
