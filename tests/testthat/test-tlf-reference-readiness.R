test_that("preflight and the TLF gate agree on configured reference readiness", {
  root <- withr::local_tempdir()
  file <- review_source(root, "ods rtf file='table.rtf'; ods rtf close;")
  writeLines("{\\rtf1 candidate}", file.path(root, "table.rtf"))
  writeLines("{\\rtf1 reference}", file.path(root, "reference.rtf"))
  dir.create(file.path(root, "directory.rtf"))
  attempt <- list(outputs_dir = root, completed = TRUE, passed = TRUE)
  for (reference in c("reference.rtf", "missing.rtf", "directory.rtf")) {
    for (location in c("outputs", "global", "target")) {
      cfg <- switch(location,
        outputs = list(outputs = list(tlfs = "table.rtf", references = list("table.rtf" = reference))),
        global = list(comparison_rules = list(reference_path = reference)),
        target = list(comparison_rules = list(references = list("table.rtf" = reference))))
      check <- sas_preflight(file, config = cfg)
      available <- identical(reference, "reference.rtf")
      expect_identical(check$references$status, if (available) "available" else "missing")
      expect_identical(check$status, if (available) "ready_for_translation" else "needs_attention")
      assessment <- assess_final_outputs(check$outputs, attempt,
        comparison_rules = check$project$config$comparison_rules)
      target <- assessment$targets[["table.rtf"]]
      expect_identical(target$passed, available)
      expect_identical(target$checks$reference_exists$passed, available)
      expect_identical(target$has_reference, available)
      expect_false(target$reference_passed)
      expect_identical(target$reference_path, check$references$path)
      expect_identical(assessment$status, if (available) "migration_ready" else "blocked")
      if (available) {
        expect_true(is.na(target$checks$reference_comparison$passed))
      } else {
        expect_null(target$checks$reference_comparison)
        expect_match(target$reason, "reference_exists")
      }
    }
  }
  # A reference is optional; no reference does not mean a missing reference.
  check <- sas_preflight(file)
  assessment <- assess_final_outputs(check$outputs, attempt)
  target <- assessment$targets[["table.rtf"]]
  expect_true(target$passed)
  expect_false(target$has_reference)
  expect_null(target$checks$reference_exists)
  expect_true(is.na(target$reference_path))
  expect_identical(assessment$status, "migration_ready")
})
