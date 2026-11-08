# The smoke workspace lacks the artifact directory that a full bundle prepares.
# The same selected code fails smoke, then executes successfully in the bundle.
smoke_then_bundle_fixture <- function(envir = parent.frame()) {
  fx <- repair_workflow_fixture(n = 1L, failures = integer(), envir = envir)
  code <- paste(fx$fixed$p01,
    'grDevices::pdf("outputs/table.pdf")',
    'graphics::plot.new()',
    'graphics::text(0.5, 0.5, "Synthetic table")',
    'grDevices::dev.off()', sep = "\n")
  state <- stage_workflow_revision(fx$state, "p01", code, "reviewed_no_material_finding")
  plan <- build_program_smoke_plan(state$graph, "p01", state$selected_revisions, execute = TRUE)
  prep <- prepare_program_smoke(state, plan, withr::local_tempdir(.local_envir = envir))
  smoke <- run_program_smoke(prep$plan, prep$runtime, prep$attempt_dir)
  stopifnot(!smoke$passed, grepl("cannot open file", smoke$condition$message))
  state$histories$p01 <- record_program_smoke(state$histories$p01, smoke)
  state$selected_revisions$p01$smoke <- smoke
  list(state = state, smoke = smoke)
}

test_that("a successful bundle supersedes failed smoke without another repair", {
  fx <- smoke_then_bundle_fixture()
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_true(result$attempt$passed)
  expect_true(result$assessment$all_required_passed)
  expect_identical(result$status, "migration_ready")
  expect_null(result$status_reason)
  expect_length(result$repairs, 0L)
  expect_length(fx$state$fixer_llm$requests(), 0L)
  ev <- current_component_evidence(result$histories$p01)
  expect_length(ev$blockers, 0L)
  expect_identical(ev$level, "output_verified")
  # Historical failure is retained and is not relabelled as a successful smoke.
  smokes <- Filter(function(e) identical(e$type, "program_smoke"), ev$events)
  expect_identical(smokes[[1L]]$status, "failed")
  expect_false(result$selected_revisions$p01$smoke$passed)
})

test_that("bundle reconciliation requires a completed execution of the active binding", {
  fx <- smoke_then_bundle_fixture()
  attempt <- run_bundle_attempt(fx$state)
  history <- fx$state$histories$p01
  expect_true(attempt$passed)
  # Reassessment after review must not duplicate execution events.
  once <- record_bundle_execution(history, attempt)
  twice <- record_bundle_execution(once, attempt)
  expect_identical(once, twice)
  next_run <- attempt
  next_run$attempt_dir <- paste0(attempt$attempt_dir, "-next-run")
  # Attempt IDs restart at bundle_attempt_001 in each run.
  expect_length(Filter(function(e) identical(e$type, "bundle_execution"),
    current_component_evidence(record_bundle_execution(once, next_run))$events), 2L)
  expect_length(Filter(function(e) identical(e$type, "bundle_execution"),
    current_component_evidence(once)$events), 1L)

  for (field in c("completed", "passed", "deferred", "executed_component_ids", "revision_manifest")) {
    other <- attempt
    other[[field]] <- switch(field, completed = FALSE, passed = FALSE, deferred = TRUE,
      executed_component_ids = character(), revision_manifest = list())
    expect_identical(record_bundle_execution(history, other), history, info = field)
  }
  # An older bundle is not evidence for code, helper, or dependency changes.
  for (field in c("r_hash", "helper_hash", "dependency_closure_hash")) {
    binding <- current_component_evidence(history)$binding
    args <- binding[c("source_hash", "r_hash", "helper_hash", "prompt_skill_hash", "dependency_closure_hash")]
    args[[field]] <- paste0(args[[field]], "-changed")
    changed <- activate_component_binding(history, do.call(new_component_binding, args))
    changed <- record_completed_review(changed)
    changed <- record_program_smoke(changed, fx$smoke)
    expect_identical(record_bundle_execution(changed, attempt), changed, info = field)
  }
  blocked <- record_program_smoke(history, utils::modifyList(fx$smoke, list(blocked_by = "upstream")))
  deferred <- record_runtime_deferred(history, "earlier dependency unavailable")
  for (h in list(blocked, deferred)) {
    current <- current_component_evidence(record_bundle_execution(h, attempt))
    expect_length(current$blockers, 0L)
    expect_null(current$runtime_deferred)
    expect_identical(current$level, "runtime_verified")
  }
})

test_that("bundle success preserves review, mechanical and output gates", {
  fx <- smoke_then_bundle_fixture()
  attempt <- run_bundle_attempt(fx$state)
  base <- fx$state$histories$p01
  histories <- list(
    review = record_completed_review(base, verdict = "repair_required", findings = list("source mismatch")),
    unavailable = record_review_unavailable(base, "provider unavailable"),
    mechanical = record_program_checks(base, list(check_id = "bad-code", pass = FALSE))
  )
  for (name in names(histories)) {
    assessment <- assess_final_outputs(fx$state$output_contracts, attempt,
      fx$state$graph, list(p01 = histories[[name]]), project = fx$state$project)
    current <- current_component_evidence(assessment$evidence_histories$p01)
    expect_false("smoke_failed" %in% current$blockers)
    expect_true(length(current$blockers) > 0L, info = name)
    expect_false(assessment$status %in% c("migration_ready", "validated"), info = name)
  }
  # Successful execution cannot make a missing required output pass.
  unlink(file.path(attempt$work_dir, "out1.rds"))
  assessment <- assess_final_outputs(fx$state$output_contracts, attempt,
    fx$state$graph, list(p01 = base), project = fx$state$project)
  expect_false(assessment$all_required_passed)
  expect_identical(assessment$status, "blocked")
})
