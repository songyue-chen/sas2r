static_repair_evidence_fixture <- function(chain = FALSE, envir = parent.frame()) {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L, value_errors = 2L,
    chain = chain, envir = envir)
  fx$state$histories$p02 <- record_completed_review(fx$state$histories$p02,
    verdict = "repair_required", basis_id = "source-derivation", findings = list(list(
      category = "translation_defect", severity = "high",
      sas_evidence = "value = value + 1", r_evidence = "x$value <- x$value + 9",
      affected_outputs = "work.out2", unresolved_dependencies = character())))
  fx
}

test_that("independent static repairs preserve the failed attempt and its attribution", {
  fx <- static_repair_evidence_fixture()
  attempt <- run_bundle_attempt(fx$state)
  assessment <- assess_final_outputs(fx$state$output_contracts, attempt,
    fx$state$graph, fx$state$histories, project = fx$state$project)
  diagnostic <- collect_bundle_diagnostics(fx$state, attempt)
  queue <- bundle_repair_queue(fx$state, attempt, assessment, diagnostic)
  expect_true(queue$p01$attributable_execution_failure)
  expect_false(queue$p02$attributable_execution_failure)
  expect_identical(queue$p02$primary_component_id, "p02")
  expect_identical(queue$p02$attempt, attempt)
  expect_false(queue$p02$bounded_diagnostics$passed)
  expect_equal(queue$p02$bounded_diagnostics$exit_status, 1)
  expect_identical(queue$p02$bounded_diagnostics$failed_component_id, "p01")
  expect_identical(queue$p02$stopping_condition, attempt$condition)
})

test_that("an upstream failure stays visible in a downstream static fix request", {
  fx <- static_repair_evidence_fixture(chain = TRUE)
  fx$state$fixer_llm <- recording_fixer(function(context) {
    # The first repair remains unresolved; the independent source defect in the
    # consumer is still repairable without attributing the crash to it.
    code <- if (context$component_id == "p01") fx$state$selected_revisions$p01$r_code else fx$fixed$p02
    valid_program_fix_response(code)
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  requests <- fx$state$fixer_llm$requests()
  expect_length(requests, 2L)
  for (i in seq_along(requests)) {
    text <- paste(vapply(requests[[i]]$messages, `[[`, "", "content"), collapse = "\n")
    expect_match(text, '"passed": false', fixed = TRUE)
    expect_match(text, '"exit_status": 1', fixed = TRUE)
    expect_match(text, '"failed_component_id": "p01"', fixed = TRUE)
    expect_match(text, paste0('"attributable_execution_failure": ', if (i == 1L) "true" else "false"), fixed = TRUE)
    expect_match(text, paste0('"repair_component_id": "p0', i, '"'), fixed = TRUE)
  }
  expect_identical(result$status, "blocked")
  expect_false(any(result$attempts$status %in% c("migration_ready", "validated")))
})
