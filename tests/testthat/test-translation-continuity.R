test_that("missing data and source permit drafts in sequential and parallel runs", {
  skip_if_not_installed("dplyr")
  for (workers in c(1L, 2L)) for (execute in c(FALSE, TRUE)) {
    # CRAN covers serial drafting and parallel execution; CI runs the full cross-product.
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && execute != (workers == 2L)) next
    fx <- repair_workflow_fixture(n = 3L, failures = integer(), chain = TRUE)
    writeLines(c('%include "unavailable.sas";', '%unavailable_macro();',
      'data work.out1; set missing.input; retain marker 1; run;'), file.path(fx$root, "p01.sas"))
    # p03 is independent and should still get its execution checks.
    writeLines('data work.out3; set raw.input; value=value+1; run;', file.path(fx$root, "p03.sas"))
    responses <- list(translator = valid_program_translation_response(fx$fixed$p01),
      reviewer = valid_program_review_response())
    result <- sas_translate(fx$root, out_dir = file.path(fx$root, "run"), config = fx$state$config,
      llm = parallel_test_llm(responses, delay = 0), max_parallel_translations = workers,
      execute = execute, max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
    expect_identical(result$status, "needs_review")
    expect_setequal(names(result$component_evidence), fx$ids)
    expect_true(all(file.exists(file.path(result$bundle_dir, "programs", paste0(fx$ids, ".R")))))
    expect_true(length(result$diagnostics$execution_deferred) > 0L)
    report <- read_json_record(result$report_json_path)
    expect_match(report$outcome$stages$Translation, "3 of 3", fixed = TRUE)
    if (execute) {
      # The independent root runs; the blocked root and its dependent do not.
      expect_match(report$outcome$stages[["Bundle execution"]], "EXECUTED (1 attempt", fixed = TRUE)
      expect_match(report$outcome$stages[["Bundle execution"]], "not executed: p01, p02", fixed = TRUE)
      expect_setequal(names(result$diagnostics$deferred_components), c("p01", "p02"))
    } else {
      expect_match(report$outcome$stages[["Bundle execution"]], "NOT RUN", fixed = TRUE)
    }
    if (execute) {
      expect_identical(current_component_evidence(result$component_evidence$p03)$level, "runtime_verified")
      expect_match(current_component_evidence(result$component_evidence$p02)$runtime_deferred,
        "Dependencies unavailable", fixed = TRUE)
    }
    check <- sas_preflight(fx$root, config = fx$state$config)
    expect_match(paste(component_readiness_context(check$project, "p02"), collapse = "\n"),
      "missing logic", fixed = TRUE)
  }
})

test_that("new worker findings preserve upstream and downstream drafts and their warnings", {
  for (workers in c(1L, 2L)) {
    fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
    state <- fx$state
    state$selected_revisions$p01$contract$discovered_dependencies <- "work.unknown"
    llm <- parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0)
    state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
    state$parallel <- resolve_parallel_execution(state, workers)
    result <- run_program_pipeline(state, execute = TRUE, max_program_repair_rounds = 0L)
    expect_identical(result$component_stage$p01, "settled")
    expect_identical(result$component_stage$p02, "settled")
    expect_setequal(result$diagnostics$dependency_findings$p01$affected, fx$ids)
    expect_match(paste(component_readiness_context(result$project, "p02"), collapse = "\n"), "work.unknown", fixed = TRUE)
    expect_true(result$selected_revisions$p02$smoke$deferred)
  }
})

test_that("cycles allow provisional drafts without executing an invented order", {
  for (workers in c(1L, 2L)) {
    root <- withr::local_tempdir()
    writeLines('data work.a; set work.b; run;', file.path(root, "a.sas"))
    writeLines('data work.b; set work.a; run;', file.path(root, "b.sas"))
    result <- sas_translate(root, out_dir = file.path(root, "out"),
      llm = parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0),
      max_parallel_translations = workers, max_program_repair_rounds = 0L)
    expect_identical(result$status, "needs_review")
    expect_setequal(names(result$component_evidence), c("a", "b"))
    expect_match(paste(result$diagnostics$execution_deferred, collapse = " "), "Dependency cycle")
  }
})

test_that("a sequential component exception preserves its draft and continues other work", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  original <- check_component_revision
  local_mocked_bindings(check_component_revision = function(state, component_id) {
    if (component_id == "p01") stop("component check crashed")
    original(state, component_id)
  })
  result <- run_program_pipeline(fx$state, execute = FALSE)
  expect_identical(result$component_stage$p01, "failed")
  expect_identical(result$component_stage$p02, "settled")
  expect_true(file.exists(result$selected_revisions$p01$r_path))
  expect_match(result$diagnostics$component_failures$p01$reason, "component check crashed")
})

test_that("critical errors still stop instead of being downgraded to component warnings", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  local_mocked_bindings(check_component_revision = function(...) stop(llm_settings_error("provider unavailable")))
  expect_error(run_program_pipeline(fx$state), class = "sas2r_llm_settings_error")
  empty <- withr::local_tempdir()
  writeLines("/* no active source */", file.path(empty, "empty.sas"))
  expect_error(sas_translate(empty, out_dir = file.path(empty, "out")), class = "sas2r_no_translation_source")
})

test_that("a critical provider setup failure remains terminal across process workers", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- fx$state
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0)
  # A source reference retains its whole file, even when the factory is tiny.
  # This used to exceed process-start argument/environment limits on Linux.
  factory <- eval(parse(text = c(paste0("# ", strrep("source metadata ", 70000L)),
    'function() { stop(llm_settings_error("provider configuration unusable")) }'), keep.source = TRUE))
  environment(factory) <- asNamespace("sas2r")
  attr(llm, "parallel_factory") <- factory
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  expect_error(run_program_pipeline(state, execute = FALSE), class = "sas2r_parallel_worker_error")
  expect_identical(state$usage_budget$request_count, 0L)
})

test_that("a critical late stop reports and preserves completed public-run code", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  original <- check_component_revision
  local_mocked_bindings(check_component_revision = function(state, component_id) {
    if (component_id == "p02") stop(llm_settings_error("provider no longer available"))
    original(state, component_id)
  })
  out <- file.path(fx$root, "run")
  expect_error(sas_translate(fx$root, out_dir = out, config = fx$state$config, execute = FALSE),
    class = "sas2r_llm_settings_error")
  report <- read_json_record(list.files(out, pattern = "^report.json$", recursive = TRUE, full.names = TRUE)[1L])
  expect_match(report$outcome$stages$Translation, "2 of 2", fixed = TRUE)
  expect_identical(report$outcome$title, "Run incomplete - failed")
  expect_true(any(grepl("programs/p01.R$", list.files(out, recursive = TRUE))))
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
