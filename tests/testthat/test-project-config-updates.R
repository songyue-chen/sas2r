test_that("a project configuration update inherits omitted fields and replaces whole fields", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data adam.out; x=1; run;")
  cfg <- list(libraries = list(adam = "."), include_roots = ".",
    macro_search_path = ".", comparison_rules = list(max_rows = 20),
    llm = list(provider = "openai", model = "unused"))
  first <- sas_preflight(file, config = cfg,
    outputs = list(datasets = "adam.out", assertions = list("adam.out" = list(required_columns = "x"))))
  testthat::local_mocked_bindings(
    build_dependency_graph = function(...) stop("must reuse planned graph"),
    sas_llm = function(...) stop("preflight must not construct an adapter"))

  updated <- sas_preflight(first$project, config = list(comparison_rules = list(min_rows = 10)))
  expect_identical(updated$project$config$comparison_rules, list(min_rows = 10))
  expect_identical(scan_config_fields(updated$project$config), scan_config_fields(first$project$config))
  expect_identical(updated$project$config$llm, first$project$config$llm)
  expect_identical(updated$outputs, first$outputs)
  expect_identical(updated$schedule, first$schedule)

  model_update <- sas_preflight(updated$project, config = list(llm = list(provider = "openai", model = "changed")))
  expect_identical(model_update$project$config$llm$model, "changed")
  expect_identical(model_update$project$config$comparison_rules, updated$project$config$comparison_rules)
  expect_identical(model_update$outputs, first$outputs)
  expect_identical(sas_preflight(model_update$project, config = list())$project$config, model_update$project$config)
})

test_that("translation accepts project updates and explicit NULL resets a field", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data adam.out; x=1; run;")
  ref <- file.path(root, "ref.rds")
  saveRDS(data.frame(x = 1), ref)
  first <- sas_preflight(file, config = list(libraries = list(adam = "."),
    llm = list(provider = "openai", model = "unused")),
    outputs = list(references = list("adam.out" = ref)))
  result <- sas_translate(first$project, out_dir = file.path(root, "migration"),
    config = list(llm = NULL, comparison_rules = list(min_rows = 2)),
    execute = FALSE, usage_limits = list(max_calls = 0))
  expect_null(result$project$config$llm)
  expect_identical(result$project$config$libraries, first$project$config$libraries)
  expect_identical(result$project$output_contracts, first$outputs)
  assessment <- assess_dataset_target(result$project$output_contracts, data.frame(x = 1),
    result$project$config$comparison_rules)
  expect_false(assessment$checks$min_rows$passed)
  cleared <- sas_preflight(result$project, config = list(comparison_rules = NULL))
  expect_identical(cleared$project$config$comparison_rules, list())
})

test_that("explicit scan changes still require a rescan and complete configs replace", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data adam.out; x=1; run;")
  autoexec <- file.path(root, "autoexec.sas")
  writeLines("%let study=demo;", autoexec)
  first <- sas_preflight(file, config = list(libraries = list(adam = "."),
    include_roots = ".", macro_search_path = ".", autoexec = autoexec))
  for (field in names(scan_config_fields(first$project$config))) {
    for (value in list(NULL, list(), "changed")) {
      patch <- stats::setNames(list(value), field)
      expect_error(sas_preflight(first$project, config = patch), class = "sas2r_config_error")
      expect_error(sas_translate(first$project, config = patch, out_dir = file.path(root, "out")),
        class = "sas2r_config_error")
    }
  }
  expect_false(dir.exists(file.path(root, "out")))
  replacement <- first$project$config
  replacement$comparison_rules <- list(min_rows = 3)
  expect_identical(sas_preflight(first$project, config = replacement)$project$config$comparison_rules,
    list(min_rows = 3))
  replacement$libraries <- NULL
  expect_error(sas_preflight(first$project, config = replacement), class = "sas2r_config_error")
  yaml_path <- file.path(root, "replacement.yml")
  writeLines("comparison_rules: {min_rows: 3}", yaml_path)
  expect_error(sas_preflight(first$project, config = yaml_path), class = "sas2r_config_error")
  expect_no_error(sas_preflight(file, config = yaml_path))
})
