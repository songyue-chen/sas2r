test_that("inconclusive focused reviews cannot weaken clean helper consumers", {
  for (verdict in c("review_unavailable", "repair_required")) {
    fx <- repair_workflow_fixture(n = 2L, failures = integer())
    fx$state <- stage_workflow_revision(fx$state, "p02", "x <- lib_read('raw', 'input')",
                                      "reviewed_no_material_finding")
    ref <- file.path(fx$root, "reference.rds")
    saveRDS(data.frame(id = 1:2, value = 91:92), ref)
    fx$state$comparison_rules <- list(references = list(work.out1 = ref))
    old_path <- fx$state$runtime$helpers
    helper <- paste(c(readLines(old_path, warn = FALSE),
      "original_lib_read <- lib_read",
      "lib_read <- function(...) { x <- original_lib_read(...); x$value <- x$value + 99; x }"), collapse = "\n")
    fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
      code = fx$fixed$p02, bundle_helper_patch = list(path = "sas2r-helpers.R",
        content = helper, reason = "repair helper")))
    fx$state$reviewer_llm <- recording_reviewer(function(req) {
      text <- paste(vapply(req$messages, function(m) as.character(m$content), ""), collapse = "\n")
      if (grepl("Focused source review", text, fixed = TRUE))
        return(valid_program_review_response(verdict = verdict))
      if (req$component_id == "p01") return(material_review_response(
        sas_evidence = "value = value + 1", r_evidence = "helper adds 99 on every input read"))
      valid_program_review_response()
    })
    result <- run_bundle_pipeline(fx$state)
    expect_identical(result$runtime$helpers, old_path)
    expect_identical(result$selected_attempt$attempt_id, "bundle_attempt_001")
    expect_identical(component_review_verdict(result$histories$p01), "reviewed_no_material_finding")
    events <- current_component_evidence(result$histories$p01)$events
    focused <- Filter(function(x) identical(x$type, "source_mismatch_review"), events)
    expect_identical(focused[[1L]]$verdict, verdict)
    expect_length(result$diagnostics$rejected_repairs, 1L)
    rejected <- result$diagnostics$rejected_repairs[[1L]]$revisions$p01
    rejection <- Filter(function(x) identical(x$type, "repair_rejected"), events)[[1L]]
    expect_identical(rejected$evidence_revision_id, rejection$candidate_revision_id)
    expect_identical(rejected$artifact_revision_id, rejection$candidate_artifact_revision_id)
    expect_true(file.exists(rejected$r_path))
  }
})

# Rebuild ordinary source bindings after a project edit, as generation does.
edited_repair_workflow <- function(fx, ids = fx$ids) {
  project <- sas_project(fx$root, config = fx$state$config)
  state <- new_migration_state(project, fx$state$paths$root, config = fx$state$config)
  for (cid in ids) {
    rev <- fx$state$selected_revisions[[cid]]
    b <- rev$binding
    sas <- component_source_text(state$graph, cid)
    b <- new_component_binding(migration_hash(sas), b$r_hash, b$helper_hash,
                               b$prompt_skill_hash, b$dependency_closure_hash)
    rev$binding <- rev$contract$binding <- b
    rev$contract$sas_text <- sas
    state$selected_revisions[[cid]] <- rev
    state$histories[[cid]] <- record_completed_review(new_component_evidence_history(cid, b))
  }
  state$output_contracts <- infer_output_contracts(project,
    overrides = list(datasets = paste0("work.out", as.integer(sub("p", "", ids)))))
  state
}

test_that("saved selections can be replaced after source edits and removals", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  first <- run_bundle_attempt(fx$state, sequence = 1L)
  before <- assess_final_outputs(fx$state$output_contracts, first, fx$state$graph, fx$state$histories)
  select_attempt(fx$state$paths, first, before)
  writeLines(c('libname extra "x";', component_source_text(fx$state$graph, "p01")),
             file.path(fx$root, "p01.sas"))
  edited <- edited_repair_workflow(fx)
  second <- run_bundle_attempt(edited, sequence = 2L)
  after <- assess_final_outputs(edited$output_contracts, second, edited$graph, edited$histories)
  expect_false(identical(first$population_checks$p02[[1L]]$unit_id,
                         second$population_checks$p02[[1L]]$unit_id))
  expect_identical(select_attempt(edited$paths, second, after)$attempt_id, "bundle_attempt_002")
  unlink(file.path(fx$root, "p02.sas"))
  removed <- edited_repair_workflow(fx, "p01")
  third <- run_bundle_attempt(removed, sequence = 3L)
  final <- assess_final_outputs(removed$output_contracts, third, removed$graph, removed$histories)
  expect_identical(select_attempt(removed$paths, third, final)$attempt_id, "bundle_attempt_003")
})

test_that("new findings on identical saved code update its evidence after JSON reload", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer(), value_errors = 1L)
  attempt <- run_bundle_attempt(fx$state, sequence = 1L)
  assessment <- assess_final_outputs(fx$state$output_contracts, attempt, fx$state$graph, fx$state$histories)
  select_attempt(fx$state$paths, attempt, assessment)
  h <- record_completed_review(fx$state$histories$p01, verdict = "repair_required",
    findings = list(list(severity = "material", sas_evidence = "value = value + 1", r_evidence = "value + 9")))
  after <- assess_final_outputs(fx$state$output_contracts, attempt, fx$state$graph, list(p01 = h))
  selected <- select_attempt(fx$state$paths, attempt, after)
  expect_identical(component_review_verdict(selected$assessment$evidence_histories$p01), "repair_required")
})

test_that("a missing generated intermediate is repaired at its declared writer", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  fx$state <- stage_workflow_revision(fx$state, "p01",
    sub("'out1'", "'out_one'", fx$fixed$p01, fixed = TRUE), "reviewed_no_material_finding")
  fx$state$output_contracts <- infer_output_contracts(fx$state$project,
    overrides = list(datasets = "work.out2"))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(vapply(result$repairs, `[[`, "", "component_id"), "p01")
  expect_identical(result$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_identical(result$status, "migration_ready")
  request <- fx$state$fixer_llm$requests()[[1L]]
  text <- paste(vapply(request$messages, function(m) as.character(m$content), ""), collapse = "\n")
  expect_match(text, "work.out1", fixed = TRUE)
})

test_that("an unresolved upstream reference cannot hide a downstream source defect", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE, value_errors = 2L)
  ref <- file.path(fx$root, "reference.rds")
  saveRDS(data.frame(id = 1:3, value = 90:92), ref)
  fx$state$comparison_rules <- list(references = list(work.out1 = ref, work.out2 = ref))
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    text <- paste(vapply(req$messages, function(m) as.character(m$content), ""), collapse = "\n")
    if (req$component_id == "p02" && grepl("value + 9", text, fixed = TRUE))
      material_review_response(sas_evidence = "value = value + 1", r_evidence = "value + 9")
    else valid_program_review_response()
  })
  result <- run_bundle_pipeline(fx$state)
  expect_identical(vapply(result$repairs, `[[`, "", "component_id"), "p02")
  expect_identical(result$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_identical(result$selected_revisions$p02$r_code, fx$fixed$p02)
  expect_identical(result$status, "blocked")
})

test_that("all missing artifacts from one writer reach one repair request", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  writeLines("data work.out1 work.out2; set raw.input; value=value+1; run;", file.path(fx$root, "p01.sas"))
  state <- edited_repair_workflow(fx)
  state$output_contracts <- infer_output_contracts(state$project,
    overrides = list(datasets = c("work.out1", "work.out2")))
  state <- stage_workflow_revision(state, "p01", "x <- lib_read('raw', 'input')", "reviewed_no_material_finding")
  attempt <- run_bundle_attempt(state, sequence = 1L)
  assessment <- assess_final_outputs(state$output_contracts, attempt, state$graph, state$histories)
  queue <- bundle_repair_queue(state, attempt, assessment, list(failures = list()))
  expect_length(queue$p01$checks$errors, 2L)
  expect_match(queue$p01$checks$check_id, "artifact:work.out1", fixed = TRUE)
  expect_match(queue$p01$checks$check_id, "artifact:work.out2", fixed = TRUE)
})

test_that("focused-review reassessment reuses immutable target observations", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  ref <- file.path(fx$root, "reference.rds")
  saveRDS(data.frame(id = 1:2, value = 90:91), ref)
  fx$state$comparison_rules <- list(references = list(work.out1 = ref))
  calls <- 0L
  original <- assess_dataset_target
  local_mocked_bindings(assess_dataset_target = function(...) { calls <<- calls + 1L; original(...) })
  result <- run_bundle_pipeline(fx$state)
  expect_equal(calls, 1L)
  expect_identical(result$status, "blocked")
})
