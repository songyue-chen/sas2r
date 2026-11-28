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
