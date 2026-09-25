test_that("macro-produced intermediates reach execution in both translation modes", {
  for (workers in c(1L, 2L)) {
    root <- withr::local_tempdir()
    writeLines(c('%macro mk; data work.tmp; x=1; run; %mend;', '%mk;',
      'data work.out; set work.tmp; y=x+1; run;'), file.path(root, "p.sas"))
    responses <- list(
      "translator:p" = valid_program_translation_response(paste(
        "mk <- function() lib_write(data.frame(x = 1), 'work', 'tmp')",
        "mk()", "x <- lib_read('work', 'tmp')", "x$y <- x$x + 1",
        "lib_write(x, 'work', 'out')", sep = "\n")),
      reviewer = valid_program_review_response())
    result <- sas_translate(root, out_dir = file.path(root, "out"), outputs = "work.out",
      llm = parallel_test_llm(responses, delay = 0), max_parallel_translations = workers,
      max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
    advisory <- Filter(function(x) x$kind == "input_no_producer", result$diagnostics$readiness$warnings)
    expect_length(advisory, 1L)
    expect_false(advisory[[1L]]$blocks_execution)
    expect_identical(result$status, "migration_ready")
    expect_length(result$diagnostics$execution_deferred, 0L)
    actual <- readRDS(file.path(result$outputs_dir, "datasets", "work", "out.rds"))
    expect_identical(names(actual), c("x", "y"))
    expect_equal(nrow(actual), 1L)
    expect_equal(actual$x, 1)
    expect_equal(actual$y, 2)
    report <- read_json_record(result$report_json_path)
    expect_match(report$outcome$stages[["Bundle execution"]], "EXECUTED", fixed = TRUE)
  }
})

test_that("unproven producers and order are advisory but runtime failures still fail", {
  root <- withr::local_tempdir()
  writeLines('data work.out; set work.tmp; run; data work.tmp; x=1; run;', file.path(root, "p.sas"))
  check <- sas_preflight(root)
  warning <- Filter(function(x) x$kind == "input_backward_dependency", check$readiness$warnings)
  expect_length(warning, 1L)
  expect_false(warning[[1L]]$blocks_execution)
  writeLines('data work.out; set work.tmp; run;', file.path(root, "p.sas"))
  result <- sas_translate(root, out_dir = file.path(root, "out"), outputs = "work.out",
    llm = parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0),
    max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  expect_identical(result$status, "blocked")
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages[["Bundle execution"]], "1 failed", fixed = TRUE)
})

test_that("atomic write failures stop before processing another component", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  # A file where the output directory is expected produces a real write failure.
  obstacle <- file.path(fx$root, "not-a-directory")
  writeLines("occupied", obstacle)
  visited <- character()
  local_mocked_bindings(check_component_revision = function(state, component_id) {
    visited <<- c(visited, component_id)
    atomic_write_file(function(path) writeLines("revision", path), file.path(obstacle, "revision.R"))
  })
  error <- suppressWarnings(tryCatch(run_program_pipeline(fx$state, execute = FALSE), error = identity))
  expect_s3_class(error, "sas2r_write_failed")
  expect_identical(visited, "p01")
  expect_match(conditionMessage(error), obstacle, fixed = TRUE)
  expect_true(file.exists(error$migration_state$selected_revisions$p01$r_path))
  expect_true(critical_translation_error(tryCatch(
    atomic_write_json(list(x = 1), obstacle, overwrite = FALSE), error = identity)))
})

test_that("a failed generated-revision write is terminal before the next draft", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- new_migration_state(sas_project(fx$root, config = fx$state$config),
    file.path(fx$root, "out"))
  dir.create(state$paths$component_revisions, recursive = TRUE, showWarnings = FALSE)
  writeLines("occupied", file.path(state$paths$component_revisions, "p01"))
  error <- suppressWarnings(tryCatch(run_program_pipeline(state, execute = FALSE), error = identity))
  expect_s3_class(error, "sas2r_write_failed")
  expect_length(error$migration_state$diagnostics$component_failures, 0L)
  expect_false(dir.exists(file.path(state$paths$component_revisions, "p02")))
})

test_that("adapter configuration failures remain terminal in a real process worker", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  llm <- parallel_test_llm(list(), delay = 0)
  factory <- function() llm_config_abort("worker adapter configuration is invalid")
  environment(factory) <- asNamespace("sas2r")
  attr(llm, "parallel_factory") <- factory
  state <- fx$state
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  expect_error(run_program_pipeline(state, execute = FALSE), class = "sas2r_parallel_worker_error")
  expect_identical(state$usage_budget$request_count, 0L)
})

test_that("a write failure inside a process worker component handler stays terminal", {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L)
  llm <- parallel_test_llm(list(fixer = valid_program_fix_response(fx$fixed$p01),
    reviewer = valid_program_review_response()), delay = 0, write_failure_component = "p01")
  state <- fx$state
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  error <- suppressWarnings(tryCatch(run_program_pipeline(state, execute = TRUE,
    max_program_repair_rounds = 1L), error = identity))
  expect_s3_class(error, "sas2r_parallel_worker_error")
  expect_length(error$migration_state$diagnostics$component_failures, 0L)
  failures <- error$migration_state$diagnostics$worker_failures
  expect_length(failures, 1L)
  expect_true(failures[[1L]]$critical)
  expect_identical(failures[[1L]]$phase, "settle")
  expect_match(failures[[1L]]$reason, "failed to write", fixed = TRUE)
  expect_match(failures[[1L]]$reason, "simulated revision write failure", fixed = TRUE)
  expect_true(file.exists(failures[[1L]]$stderr))
  expect_true(state$usage_budget$request_count > 0L)
})

test_that("supplying a reference before resume refreshes console JSON and HTML readiness", {
  skip_if_not_installed("dplyr")
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  ref <- file.path(fx$root, "reference.rds")
  args <- list(path = fx$root, out_dir = file.path(fx$root, "out"), config = fx$state$config,
    outputs = list(datasets = "work.out1", references = list("work.out1" = ref)),
    llm = parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0),
    max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  first <- do.call(sas_translate, args)
  expect_true(any(vapply(first$diagnostics$readiness$warnings,
    function(x) x$kind == "reference_missing", logical(1))))
  saveRDS(data.frame(id = 1:3, value = 11:13), ref)
  output <- capture.output(result <- do.call(sas_translate, c(args, list(resume = TRUE))))
  expect_identical(result$diagnostics$resumed_components, "p01")
  expect_identical(result$status, "validated")
  expect_identical(result$diagnostics$readiness, result$project$readiness)
  expect_false(any(grepl("reference_missing", output, fixed = TRUE)))
  report <- read_json_record(result$report_json_path)
  expect_length(report$diagnostics$readiness$warnings, 0L)
  expect_false(any(grepl("reference_missing", readLines(
    file.path(dirname(dirname(result$report_json_path)), "START_HERE.html")), fixed = TRUE)))
})

test_that("preflight warning text containing braces is printed literally", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  writeLines(c('%include "missing{source}.sas";', readLines(file.path(fx$root, "p01.sas"))),
    file.path(fx$root, "p01.sas"))
  messages <- capture.output(result <- sas_translate(fx$root, out_dir = file.path(fx$root, "out"),
    config = fx$state$config, execute = FALSE, max_program_repair_rounds = 0L), type = "message")
  expect_s3_class(result, "sas2r_translation")
  expect_true(any(grepl("missing{source}.sas", messages, fixed = TRUE)))
})

test_that("macro findings deduplicate and component failure context retains prior findings safely", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  writeLines(c('%missing_helper;', readLines(file.path(fx$root, "p01.sas"))), file.path(fx$root, "p01.sas"))
  state <- new_migration_state(sas_project(fx$root, config = fx$state$config), file.path(fx$root, "out"))
  state <- record_dependency_finding(state, "p01", c("%MISSING_HELPER", "Missing_Helper"))
  expect_length(state$diagnostics$dependency_findings, 0L)
  state <- record_dependency_finding(state, "p01", "work.missing")
  token <- paste0("sk-", strrep("x", 24))
  events <- list()
  state <- withCallingHandlers(record_component_failure(state, "p01",
    simpleError(paste("request failed at /useful/path; Authorization: Bearer", token))),
    sas2r_progress = function(event) events[[length(events) + 1L]] <<- unclass(event))
  context <- paste(component_readiness_context(state$project, "p02"), collapse = "\n")
  expect_match(context, "work.missing", fixed = TRUE)
  expect_match(context, "could not finish", fixed = TRUE)
  expect_match(context, "/useful/path", fixed = TRUE)
  expect_false(grepl(token, context, fixed = TRUE))
  expect_false(any(grepl(token, unlist(events), fixed = TRUE)))
  state <- record_dependency_finding(state, "p01", "work.another")
  context <- paste(component_readiness_context(state$project, "p02"), collapse = "\n")
  expect_match(context, "could not finish", fixed = TRUE)
  expect_match(context, "work.another", fixed = TRUE)
})

test_that("a resume reassessment error keeps the latest pipeline diagnostics", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  fx$state$diagnostics$execution_deferred <- "previous run only"
  local_mocked_bindings(parallel_dependency_findings = function(...)
    cli::cli_abort("library projection failed", class = "sas2r_libref_error"))
  error <- tryCatch(run_program_pipeline(fx$state, execute = FALSE), error = identity)
  expect_s3_class(error, "sas2r_libref_error")
  expect_length(error$migration_state$selected_revisions, 2L)
  expect_null(error$migration_state$diagnostics$execution_deferred)
})

test_that("mechanical failures have one consistent final reason even with missing source", {
  for (execute in c(FALSE, TRUE)) {
    fx <- repair_workflow_fixture(n = 1L, failures = integer())
    writeLines(c('%missing_helper;', readLines(file.path(fx$root, "p01.sas"))), file.path(fx$root, "p01.sas"))
    code <- paste("library(dplyr)", fx$fixed$p01, sep = "\n")
    result <- sas_translate(fx$root, out_dir = file.path(fx$root, "out"), config = fx$state$config,
      llm = parallel_test_llm(list(translator = valid_program_translation_response(code)), delay = 0),
      execute = execute, max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
    expect_identical(result$status, "blocked")
    expect_identical(result$status_reason,
      "Mechanical checks failed for one or more components; other available source was processed.")
    report <- read_json_record(result$report_json_path)
    expect_identical(report$outcome$title, "Run incomplete - blocked")
  }
})
