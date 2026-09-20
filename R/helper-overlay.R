# Shared-helper edits are named function overlays, never executable setup code.
# Parsing/deparsing materializes a complete snapshot without running any body.
helper_definitions <- function(code) {
  exprs <- as.list(parse(text = code, keep.source = FALSE))
  names <- vapply(exprs, function(e) {
    if (!is.call(e) || length(e) != 3L || !is.name(e[[1L]]) ||
        !as.character(e[[1L]]) %in% c("<-", "=") ||
        !(is.name(e[[2L]]) || (is.character(e[[2L]]) && length(e[[2L]]) == 1L)) ||
        !is.call(e[[3L]]) || !identical(e[[3L]][[1L]], as.name("function"))) {
      stop("Helper overlays must contain only named top-level function definitions", call. = FALSE)
    }
    as.character(e[[2L]])
  }, "")
  if (anyDuplicated(names)) stop("Helper overlays cannot contain duplicate definitions", call. = FALSE)
  stats::setNames(exprs, names)
}

assemble_helper_overlay <- function(retained, overlay) {
  base <- helper_definitions(retained)
  patch <- helper_definitions(overlay)
  if (!length(patch)) stop("Helper overlay contains no function definitions", call. = FALSE)
  original <- base
  base[names(patch)] <- patch
  if (identical(base, original)) return(retained)
  paste(vapply(base, function(e) paste(deparse(e, width.cutoff = 120L), collapse = "\n"), ""), collapse = "\n\n")
}

runtime_helper_code <- function(runtime = NULL) {
  path <- runtime$helpers %||% system.file("templates", "sas2r-helpers.R", package = "sas2r")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

candidate_helper_patch <- function(revision, runtime = NULL) {
  code <- revision$helper_code %||% runtime_helper_code(runtime)
  list(path = "candidate-helpers.R", content = code)
}

# Rebind every existing consumer of a changed shared runtime. The caller keeps
# the retained state until candidate review/execution has accepted this state.
stage_helper_candidate <- function(state, revision) {
  state$runtime$helpers <- revision$helper_path
  for (cid in names(state$selected_revisions)) {
    state <- refresh_component_runtime_binding(state, cid)
    state$selected_revisions[[cid]]$smoke <- NULL
  }
  state
}

review_helper_consumers <- function(state, retained, components, round,
                                    phase = "program", execution = NULL) {
  reasons <- character()
  for (cid in components) {
    rev <- state$selected_revisions[[cid]]
    rev$binding <- rev$binding %||% rev$contract$binding
    if (!identical(current_component_evidence(state$histories[[cid]])$binding$binding_hash,
                   rev$binding$binding_hash)) {
      state$histories[[cid]] <- activate_component_binding(state$histories[[cid]], rev$binding)
    }
    rev$checks <- check_program_revision(rev$r_path, contract = rev$contract,
      helper_patch = candidate_helper_patch(rev, state$runtime), allowlist = state$config$allowlist)
    rev$status <- if (isTRUE(rev$checks$pass)) "ok" else "check_failed"
    state$selected_revisions[[cid]] <- rev
    state$histories[[cid]] <- record_program_checks(state$histories[[cid]], rev$checks)
    review <- list(verdict = "review_unavailable")
    if (isTRUE(rev$checks$pass) && !is.null(state$reviewer_llm)) {
      review <- tryCatch(review_program_revision(rev, context = list(
        component_id = cid, contract = rev$contract,
        sas_source = component_source_text(state$graph, cid), project = state$project,
        selected_revisions = state$selected_revisions, config = state$config,
        helper_code = runtime_helper_code(state$runtime), phase = phase,
        execution = execution), llm = state$reviewer_llm,
        usage = state$usage_budget, paths = state$paths, round = round,
        history = state$histories[[cid]]), error = function(e) {
          if (critical_translation_error(e)) stop(e)
          list(verdict = "review_unavailable", reason = conditionMessage(e),
            history = record_review_unavailable(state$histories[[cid]], conditionMessage(e)))
        })
      if (!is.null(review$history)) state$histories[[cid]] <- review$history
    } else if (isTRUE(rev$checks$pass)) {
      state$histories[[cid]] <- record_review_unavailable(state$histories[[cid]])
    }
    previous <- list(revision = retained$selected_revisions[[cid]],
      verdict = component_review_verdict(retained$histories[[cid]]))
    regressions <- program_repair_regressions(previous, rev, review, execution = FALSE)
    if (!isTRUE(rev$checks$pass)) regressions <- c(regressions, rev$checks$errors)
    if (length(regressions)) reasons <- c(reasons, paste(cid, regressions, review$reason %||% ""))
  }
  list(state = state, reasons = reasons)
}

# Context-only revisits bind evidence to the actual selected dependency code.
# Use the same closure representation as program generation, retaining history.
refresh_component_runtime_binding <- function(state, component_id) {
  rev <- state$selected_revisions[[component_id]]
  old <- rev$binding %||% rev$contract$binding %||%
    current_component_evidence(state$histories[[component_id]])$binding
  helper <- runtime_helper_code(state$runtime)
  hashes <- vapply(state$selected_revisions, function(x) migration_hash(revision_code(x)), "")
  closure <- dependency_closure_hashes(state$graph, hashes, migration_hash(helper),
    old$prompt_skill_hash)[[component_id]]
  binding <- new_component_binding(old$source_hash, old$r_hash, migration_hash(helper),
    old$prompt_skill_hash, closure)
  if (identical(binding$binding_hash, old$binding_hash)) return(state)
  rev$binding <- rev$contract$binding <- binding
  rev$helper_code <- helper
  rev$helper_path <- state$runtime$helpers
  rev$revision_id <- paste0("rev_", substr(binding$binding_hash, 1L, 16L))
  dir <- file.path(state$paths$component_revisions, component_id, "revisions", rev$revision_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rev$r_path <- file.path(dir, "program.R")
  rev$contract_path <- file.path(dir, "contract.json")
  atomic_write_file(function(path) writeLines(rev$r_code, path), rev$r_path)
  atomic_write_json(rev$contract, rev$contract_path)
  # Retain the smoke result only as a cache candidate; its full local execution
  # identity must match before it can be reused or grant coverage.
  state$selected_revisions[[component_id]] <- rev
  state$histories[[component_id]] <- activate_component_binding(state$histories[[component_id]], binding)
  state
}

check_helper_consumers <- function(state, retained, components, execute = TRUE) {
  signal_immediate_coordinator_event("helper_consumer_checks", "earlier components")
  reasons <- character()
  for (cid in components) {
    state <- revisit_component_runtime(state, cid, execute)
    old <- retained$selected_revisions[[cid]]
    candidate <- state$selected_revisions[[cid]]
    # Compare local evidence only. A deliberately pending semantic review is
    # neither a clean verdict nor an immediate reason to reject the candidate.
    if (!isTRUE(candidate$checks$pass)) reasons <- c(reasons, paste(cid, candidate$checks$errors))
    execution_lost <- isTRUE(old$smoke$passed) && !isTRUE(candidate$smoke$passed)
    coverage_lost <- length(setdiff(passed_population_checks(old$smoke),
      passed_population_checks(candidate$smoke))) > 0L
    if (isTRUE(execute) && (execution_lost || coverage_lost)) {
      # Compare the retained candidate under the same current input/environment
      # before attributing an apparent new failure to the helper edit.
      baseline <- retained
      baseline$input_manifest <- state$input_manifest
      baseline$selected_revisions[[cid]]$smoke <- NULL
      baseline <- smoke_component_revision(baseline, cid, execute)
      retained_smoke <- baseline$selected_revisions[[cid]]$smoke
      if (execution_lost && isTRUE(retained_smoke$passed)) {
        reasons <- c(reasons, paste(cid, "execution regressed"))
      }
      if (length(setdiff(passed_population_checks(retained_smoke),
          passed_population_checks(candidate$smoke)))) {
        reasons <- c(reasons, paste(cid, "source population coverage regressed"))
      }
    }
  }
  list(state = state, reasons = reasons)
}
