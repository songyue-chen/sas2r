# A source-defined workflow with seeded generated R execution or value errors.
repair_workflow_fixture <- function(n = 4L, failures = c(1L, 3L), chain = FALSE,
                                    value_errors = integer(), envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  inputs <- file.path(root, "inputs")
  dir.create(inputs)
  saveRDS(data.frame(id = 1:3, value = 10:12), file.path(inputs, "input.rds"))
  config <- list(libraries = list(raw = list(path = inputs, engine = "rds")))
  ids <- sprintf("p%02d", seq_len(n))
  code <- sas <- list()
  for (i in seq_len(n)) {
    id <- ids[i]
    parent <- if (chain && i > 1L) paste0("work.out", i - 1L) else "raw.input"
    sas[[id]] <- sprintf("data work.out%d; set %s; value = value + 1; run;", i, parent)
    writeLines(sas[[id]], file.path(root, paste0(id, ".sas")))
    bits <- strsplit(parent, ".", fixed = TRUE)[[1L]]
    code[[id]] <- sprintf("x <- lib_read('%s', '%s')\nx$value <- x$value + 1\nlib_write(x, 'work', 'out%d')", bits[1], bits[2], i)
  }
  project <- sas_project(root, config = config)
  state <- new_migration_state(project, file.path(root, "migration"), config = config)
  for (i in seq_len(n)) {
    id <- ids[i]
    r <- if (i %in% failures) sprintf("stop('translation fault %s')", id) else code[[id]]
    if (i %in% value_errors) r <- sub("+ 1", "+ 9", r, fixed = TRUE)
    r_path <- file.path(root, paste0(id, ".R"))
    writeLines(r, r_path)
    binding <- new_component_binding(migration_hash(sas[[id]]), migration_hash(r),
      migration_hash("helpers"), migration_hash("review"), migration_hash("closure"))
    state$selected_revisions[[id]] <- list(component_id = id, revision_id = "r1", r_code = r,
      staged_file = paste0(id, ".R"), r_path = r_path, binding = binding,
      contract = list(component_id = id, staged_file = paste0(id, ".R"),
        sas_text = sas[[id]], binding = binding))
    h <- new_component_evidence_history(id, binding)
    state$histories[[id]] <- record_completed_review(h)
  }
  state$output_contracts <- infer_output_contracts(project,
    overrides = list(datasets = paste0("work.out", seq_len(n))))
  state$fixer_llm <- recording_fixer(function(context) {
    valid_program_fix_response(code = code[[context$component_id]],
      diagnosis = paste("Repair", context$component_id), summary = "Restore source derivation")
  })
  state$reviewer_llm <- recording_reviewer(function(context) valid_program_review_response())
  list(state = state, root = root, fixed = code, ids = ids)
}

stage_workflow_revision <- function(state, cid, code, verdict) {
  rev <- state$selected_revisions[[cid]]
  b <- rev$binding
  b <- new_component_binding(b$source_hash, migration_hash(code), b$helper_hash,
                             b$prompt_skill_hash, b$dependency_closure_hash)
  rev$binding <- rev$contract$binding <- b
  rev$revision_id <- paste0("r_", substr(b$r_hash, 1, 8))
  rev$r_code <- code
  rev$r_path <- file.path(dirname(rev$r_path), paste0(cid, "-", rev$revision_id, ".R"))
  writeLines(code, rev$r_path)
  state$selected_revisions[[cid]] <- rev
  h <- activate_component_binding(state$histories[[cid]], b)
  state$histories[[cid]] <- record_completed_review(h, verdict = verdict)
  state
}
