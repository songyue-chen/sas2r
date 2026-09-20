test_that("a bundle attempt executes the programs that are not deferred and records its scope", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  state <- fx$state
  state$diagnostics$deferred_components <- list(p02 = "p02 (work.missing)")
  rec <- run_bundle_attempt(state)
  expect_true(rec$passed)
  expect_identical(rec$execution_order, c("p01", "p03"))
  expect_identical(rec$executed_component_ids, c("p01", "p03"))
  expect_identical(rec$execution_scope, "partial")
  expect_identical(rec$deferred_component_ids, "p02")
  expect_identical(rec$deferred_reasons$p02, "p02 (work.missing)")
  outputs <- names(rec$output_hashes)
  expect_true(any(grepl("out1", outputs, fixed = TRUE)))
  expect_true(any(grepl("out3", outputs, fixed = TRUE)))
  expect_false(any(grepl("out2", outputs, fixed = TRUE)))
  assessment <- assess_final_outputs(state$output_contracts, rec, state$graph, state$histories)
  expect_identical(assessment$targets$work.out2$status, "not_executed")
  expect_match(assessment$targets$work.out2$reason, "p02 (work.missing)", fixed = TRUE)
  expect_true(assessment$targets$work.out1$passed)
  expect_true(assessment$targets$work.out3$passed)
  expect_identical(assessment$status, "needs_review")
})

test_that("a complete attempt records its scope and can still become migration-ready", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  rec <- run_bundle_attempt(fx$state)
  expect_identical(rec$execution_scope, "complete")
  expect_length(rec$deferred_component_ids, 0L)
  assessment <- assess_final_outputs(fx$state$output_contracts, rec, fx$state$graph, fx$state$histories)
  expect_identical(assessment$status, "migration_ready")
})

test_that("a partial attempt without deferred targets still requires review", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- fx$state
  state$output_contracts <- infer_output_contracts(state$project, overrides = list(datasets = "work.out1"))
  state$diagnostics$deferred_components <- list(p02 = "p02 (%missing_macro)")
  rec <- run_bundle_attempt(state)
  assessment <- assess_final_outputs(state$output_contracts, rec, state$graph, state$histories)
  expect_true(assessment$targets$work.out1$passed)
  expect_identical(assessment$status, "needs_review")
})

test_that("deferred producers are neither repaired nor reported as executed", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  state <- fx$state
  state$diagnostics$deferred_components <- list(p02 = "p02 (work.missing)")
  state$diagnostics$dependency_findings$p02 <- list(findings = "work.missing", advisory = character(),
    affected = "p02", reason = "source_reconciliation_required")
  calls <- character()
  state$fixer_llm <- recording_fixer(function(context) {
    calls <<- c(calls, context$component_id)
    valid_program_fix_response(code = fx$fixed[[context$component_id]])
  })
  result <- run_bundle_pipeline(state, max_bundle_repair_rounds = 2L)
  expect_identical(result$status, "needs_review")
  expect_match(result$status_reason, "p02", fixed = TRUE)
  expect_length(calls, 0L)
  write_migration_report(result)
  outcome <- read_json_record(result$paths$report_json)$outcome
  expect_match(outcome$stages[["Bundle execution"]], "EXECUTED (1 attempt; 1 ran to completion", fixed = TRUE)
  expect_match(outcome$stages[["Bundle execution"]], "not executed: p02", fixed = TRUE)
  expect_identical(outcome$stages[["Required validation"]], "REVIEW REQUIRED")
})
