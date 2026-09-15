test_that("dynamic expressions stay unresolved even when matching concrete files exist", {
  root <- withr::local_tempdir()
  writeLines("<html><body>A</body></html>", file.path(root, "panel-a.html"))
  writeLines("<html><body>B</body></html>", file.path(root, "panel-b.html"))
  contracts <- merge_output_overrides(empty_output_contracts(),
    list(tlfs = c("panel-&id..html", "panel-a.html", "panel-b.html")))
  attempt <- list(attempt_dir = root, completed = TRUE, passed = TRUE)
  assessment <- assess_final_outputs(contracts, attempt)
  expect_identical(assessment$targets[["panel-&id..html"]]$status, "unresolved_target")
  expect_false(assessment$targets[["panel-&id..html"]]$passed)
  expect_true(assessment$targets[["panel-a.html"]]$passed)
  expect_true(assessment$targets[["panel-b.html"]]$passed)
  expect_identical(assessment$status, "needs_review")
  coverage <- migration_coverage(assessment$targets)
  expect_equal(coverage$outputs_total, 2)
  expect_equal(coverage$outputs_produced, 2)
  expect_identical(coverage$unresolved_output_expressions, "panel-&id..html")
  expect_match(paste(migration_coverage_lines(coverage), collapse = "\n"), "not counted as concrete targets")
  # Partial/missing families remain failures of concrete contracts, without
  # counting the unexpanded expression as a third missing file.
  unlink(file.path(root, "panel-b.html"))
  partial <- assess_final_outputs(contracts, attempt)
  expect_identical(partial$targets[["panel-b.html"]]$status, "missing_candidate")
  expect_identical(partial$status, "blocked")
})

test_that("unexpanded dataset targets cannot pass by literal filename coincidence", {
  root <- withr::local_tempdir()
  saveRDS(data.frame(x = 1), file.path(root, "&member.rds"))
  contract <- list(target_id = "dynamic", target_key = "work.&member", kind = "dataset",
    logical_name = "work.&member", required = TRUE, resolution = "dynamic")
  result <- assess_dataset_target(contract, list(attempt_dir = root))
  expect_identical(result$status, "unresolved_target")
  expect_false(result$passed)
  expect_match(result$reason, "concrete filenames and family completeness are unknown", fixed = TRUE)
  expect_null(result$checks$candidate_exists)
  expect_true(is.na(find_attempt_candidate_file(contract, list(attempt_dir = root))))
})
