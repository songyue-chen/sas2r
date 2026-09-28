test_that("missing data and source preserve drafts while independent parallel work executes", {
  skip_if_not_installed("dplyr")
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  writeLines(c('%include "unavailable.sas";', '%unavailable_macro();',
    'data work.out1; set missing.input; retain marker 1; run;'), file.path(fx$root, "p01.sas"))
  responses <- list(translator = valid_program_translation_response(fx$fixed$p01),
    reviewer = valid_program_review_response())
  result <- sas_translate(fx$root, out_dir = file.path(fx$root, "run"), config = fx$state$config,
    outputs = list(datasets = c("work.out1", "work.out2")),
    llm = parallel_test_llm(responses, delay = 0), max_parallel_translations = 2L,
    execute = TRUE, max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  expect_identical(result$status, "needs_review")
  expect_setequal(names(result$component_evidence), fx$ids)
  expect_true(all(file.exists(file.path(result$bundle_dir, "programs", paste0(fx$ids, ".R")))))
  expect_true(length(result$diagnostics$execution_deferred) > 0L)
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages$Translation, "2 of 2", fixed = TRUE)
  expect_match(report$outcome$stages[["Bundle execution"]], "EXECUTED (1 attempt", fixed = TRUE)
  expect_match(report$outcome$stages[["Bundle execution"]], "not executed: p01", fixed = TRUE)
  expect_setequal(names(result$diagnostics$deferred_components), "p01")
  expect_identical(current_component_evidence(result$component_evidence$p02)$level, "output_verified")
  actual <- readRDS(file.path(result$outputs_dir, "datasets", "work", "out2.rds"))
  expect_equal(actual$value, 11:13)
  check <- sas_preflight(fx$root, config = fx$state$config)
  expect_match(paste(component_readiness_context(check$project, "p01"), collapse = "\n"),
    "missing logic", fixed = TRUE)
})

test_that("critical errors still stop instead of being downgraded to component warnings", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  local_mocked_bindings(check_component_revision = function(...) stop(llm_settings_error("provider unavailable")))
  expect_error(run_program_pipeline(fx$state), class = "sas2r_llm_settings_error")
  empty <- withr::local_tempdir()
  writeLines("/* no active source */", file.path(empty, "empty.sas"))
  expect_error(sas_translate(empty, out_dir = file.path(empty, "out")), class = "sas2r_no_translation_source")
})

test_that("missing references do not select code-only mode or prevent execution", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  result <- sas_translate(fx$root, out_dir = file.path(fx$root, "run"),
    config = fx$state$config, outputs = list(datasets = "work.out1",
      references = list("work.out1" = file.path(fx$root, "absent-reference.rds"))),
    llm = parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0),
    max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages[["Bundle execution"]], "EXECUTED", fixed = TRUE)
  expect_false(result$status %in% c("validated", "migration_ready"))
  expect_length(result$diagnostics$execution_deferred, 0L)
})

test_that("environment recognition is source based and preserves actual data dependencies", {
  root <- withr::local_tempdir()
  writeLines(c('options sasautos=("macros") fmtsearch=(work library);',
    '%put &SYSDATE9 &SYSSCP &SYSVER &SYSLAST;',
    'proc sql; select * from dictionary.tables; quit;',
    'data out; set sashelp.class; run;'), file.path(root, "p.sas"))
  check <- sas_preflight(root)
  expect_identical(check$inputs$status[check$inputs$dataset == "dictionary.tables"], "environment")
  expect_false(check$inputs$status[check$inputs$dataset == "sashelp.class"] == "environment")
  names <- c("SYSDATE9", "&SYSSCP.", "SYSVER", "SYSLAST", "SASAUTOS", "FMTSEARCH")
  real <- c("work.sysdate9", "%sysdate9", "SYSUNKNOWN", "sashelp.class", "sashelp.vfake", "SASHELP.VSVW")
  expect_identical(filter_dependency_resources(c(names, real), check$project, "p"), real)
  context <- paste(render_dependency_resources(check$project, "p"), collapse = "\n")
  expect_match(context, "R's version is not SAS's version", fixed = TRUE)
  expect_match(context, "depend on prior operations", fixed = TRUE)
  writeLines('data out; x=1; run;', file.path(root, "q.sas"))
  expect_identical(filter_dependency_resources(names, sas_project(root), "q"), names)
})
