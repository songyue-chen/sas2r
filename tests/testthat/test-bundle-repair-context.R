test_that("a repair batch distinguishes a repaired consumer from its executed revision", {
  fx <- repair_workflow_fixture(n = 2L, failures = 2L, value_errors = 1L, chain = TRUE)
  for (cid in fx$ids) {
    fx$state$histories[[cid]] <- record_completed_review(fx$state$histories[[cid]],
      verdict = "repair_required", basis_id = paste0("source-", cid), findings = list(list(
        category = "translation_defect", severity = "material",
        sas_evidence = "value = value + 1", r_evidence = fx$state$selected_revisions[[cid]]$r_code,
        affected_outputs = paste0("work.out", match(cid, fx$ids)), repair_disposition = "unverified")))
  }
  order <- character()
  fx$state$fixer_llm <- recording_fixer(function(ctx) {
    order <<- c(order, ctx$component_id)
    valid_program_fix_response(fx$fixed[[ctx$component_id]])
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_identical(order, c("p02", "p01"))
  request <- fx$state$fixer_llm$requests()[[2L]]
  text <- request_task_text(request)
  expect_match(text, '"failed_component_id": "p02"', fixed = TRUE)
  expect_match(text, "component p02 snapshot revision r1", fixed = TRUE)
  expect_match(text, result$repairs[[1L]]$revision_id, fixed = TRUE)
  expect_match(text, "differs from the attempt (not the executed revision)", fixed = TRUE)
  page <- request$tools$read_dependency_context$call(list(component_id = "p02", language = "r"))
  expect_identical(page$code, fx$fixed$p02)
  expect_match(page$revision_note, "snapshot revision r1", fixed = TRUE)
  expect_match(page$revision_note, "differs from the attempt", fixed = TRUE)
})
