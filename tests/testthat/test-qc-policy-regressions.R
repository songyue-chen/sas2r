test_that("reference keys and precedence are explicit in the saved plan", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data adam.out; x=1; run;")
  cfg <- list(libraries = list(adam = "."), comparison_rules = list(
    reference_path = "global.rds", references = list("ADAM.OUT" = "target.rds")))
  check <- sas_preflight(file, config = cfg)
  expect_identical(check$outputs$reference_path, file.path(check$project$project_dir, "target.rds"))
  expect_identical(check$references$path, check$outputs$reference_path)
  updated <- sas_preflight(check$project, config = list(comparison_rules = list(reference_path = "new.rds")))
  expect_identical(updated$outputs$reference_path, file.path(check$project$project_dir, "new.rds"))
  explicit <- sas_preflight(file, config = cfg, outputs = list(references = list("adam.out" = "explicit.rds")))
  expect_match(explicit$outputs$reference_path, "explicit.rds$", fixed = FALSE)
  expect_error(sas_preflight(file, config = list(comparison_rules = list(references = list(out = "missing.rds")))),
    "work.out", class = "sas2r_output_contract_error")
  for (field in c("references", "assertions")) {
    value <- if (field == "references") "ref.rds" else list(row_count = 1)
    duplicates <- stats::setNames(list(value, value), c("adam.out", "ADAM.OUT"))
    expect_error(sas_preflight(file, outputs = stats::setNames(list(duplicates), field)),
      class = "sas2r_output_contract_error")
  }
  expect_error(normalize_comparison_rules(list(references = list(out = "a", out = "b"))),
    class = "sas2r_output_contract_error")
})

test_that("persisted contracts use rows and preserve small tolerances", {
  root <- withr::local_tempdir()
  for (n in 0:2) {
    overrides <- if (!n) NULL else list(assertions = stats::setNames(rep(list(qc_profile(
      tolerances = list(x = list(abs = 0.00005)))), n), paste0("work.x", seq_len(n))))
    contracts <- infer_output_contracts(NULL, overrides)
    path <- file.path(root, "contracts.json")
    write_output_contracts(contracts, path)
    saved <- jsonlite::read_json(path)
    expect_length(saved, n)
    if (n) {
      expect_identical(saved[[1]]$target_key, "work.x1")
      expect_identical(saved[[1]]$assertions$tolerances$x$abs, 0.00005)
    }
  }
})

test_that("directory references fail cleanly with actionable details", {
  root <- withr::local_tempdir()
  contract <- list(target_key = "work.out", reference_path = root)
  expect_no_warning(result <- assess_dataset_target(contract, data.frame(x = 1)))
  expect_false(result$checks$reference_exists$passed)
  expect_match(result$reason, "directory")
  writeLines("<html><body>ok</body></html>", file.path(root, "table.html"))
  contract$target_key <- "table.html"
  contract$kind <- "tlf"
  contract$path_expression <- "table.html"
  tlf <- assess_tlf_target(contract, list(outputs_dir = root))
  expect_false(tlf$checks$reference_exists$passed)
  expect_match(tlf$reason, "directory")
})

test_that("tolerance overrides inherit as fields and legacy no-op settings warn", {
  policy <- effective_dataset_policy(list(tolerances = list(x = list(abs = 0.1))),
    qc_profile(numeric_tolerance = 0.01))
  expect_identical(dataset_comparison_profile(policy)$overrides$x$abs, 0.1)
  cleared <- effective_dataset_policy(policy, qc_profile(tolerances = list()))
  expect_length(dataset_comparison_profile(cleared)$overrides, 0L)
  expect_warning(normalize_comparison_rules(list(tolerance = 0.1)), "ignored")
  expect_identical(resolve_qc_profiles(list(profiles = list(empty = NULL)))$profiles$empty, list())
  expect_error(qc_profile(row_count = "0x10"), class = "sas2r_output_contract_error")
  expect_equal(qc_profile(row_count = "1e3")$row_count, 1000)
})
