bug_triage_answer <- function(code = "stopifnot(sas2r::sas_sum(c(1, 2)) == 3)") {
  list(findings = list(list(evidence_id = "E1",
    explanation = "The package could be dispatching the wrong helper.",
    expected = "The supported call should dispatch the documented operation.",
    uncertainty = "The cause needs a test on the reported build.",
    next_step = "Try the operation with synthetic values.",
    repro_r = code)))
}

test_that("opt-in AI triage uses the reviewer and drafts an unrun synthetic example", {
  root <- withr::local_tempdir()
  src <- file.path(root, "sample.sas")
  writeLines("data work.sample; value = 1; run;", src)
  run <- function(enabled) {
    out <- tempfile(tmpdir = root)
    suppressMessages(sas_translate(src, out_dir = out,
      config = list(migration = list(bug_reports = enabled, report_diagnosis = "off")),
      llm = llm, max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L))
  }

  requests <- list()
  llm <- new_llm(function(request, audit_context = list()) {
    is_triage <- identical(request$schema_name, "package_bug_triage_v1")
    if (is_triage) requests[[length(requests) + 1L]] <<- request
    response <- if (is_triage) bug_triage_answer('stop("REPRO_MUST_NOT_RUN")') else valid_program_review_response()
    new_llm_response(status = "completed", action = "final", data = response,
      request = request, provider = "mock", resolved_model = "diagnosis-test")
  }, provider = "mock", model = "diagnosis-test")

  transpile <- sas_transpile
  testthat::local_mocked_bindings(sas_transpile = function(project, out_dir, ...) {
    result <- transpile(project, out_dir, ...)
    staged <- unique(stats::na.omit(result$manifest$staged_file))
    writeLines(c('private_message <- paste0("RAW_", "DIAGNOSTIC_SENTINEL")', 'stop(private_message)'),
      file.path(out_dir, staged))
    result
  })

  disabled <- run(FALSE)
  expect_length(requests, 0L)
  enabled <- run(TRUE)
  report <- read_json_record(enabled$report_json_path, simplifyVector = FALSE)
  expect_identical(report$package_bug_diagnosis$status, "completed",
    info = paste(report$package_bug_diagnosis$reason, enabled$status_reason))
  expect_length(requests, 1L)
  expect_length(requests[[1L]]$tools, 0L)
  expect_match(request_task_text(requests[[1L]]), "Bundle execution failed", fixed = TRUE)
  expect_false(grepl("RAW_DIAGNOSTIC_SENTINEL|stderr|stdout", request_task_text(requests[[1L]])))
  expect_identical(enabled$status, disabled$status)
  finding <- report$package_bug_diagnosis$findings[[1L]]
  expect_identical(finding$binding$attempt_id, report$selected_attempt_id)
  expect_identical(finding$reproduction, "not_run")
  expect_identical(report$package_bug_diagnosis$model, "diagnosis-test")
  expect_true(any(vapply(enabled$usage$records,
    function(x) identical(x$agent, "package_bug_triage"), logical(1))))
  html <- file.path(dirname(dirname(enabled$report_json_path)), "START_HERE.html")
  expect_match(paste(readLines(html), collapse = "\n"), "Reproduction unverified", fixed = TRUE)

  file <- file.path(root, "draft.md")
  expect_error(sas_bug_report(disabled, file), class = "sas2r_bug_diagnosis_disabled")
  expect_false(file.exists(file))
  expect_identical(sas_bug_report(enabled, file), file)
  lines <- readLines(file)
  draft <- paste(lines, collapse = "\n")
  expect_match(draft, "Proposed synthetic reproduction", fixed = TRUE)
  expect_match(draft, "draft R example not run", fixed = TRUE)
  expect_match(draft, "REPRO_MUST_NOT_RUN", fixed = TRUE)
  expect_false(grepl("RAW_DIAGNOSTIC_SENTINEL", draft, fixed = TRUE))
  expect_error(sas_bug_report(enabled, file), class = "sas2r_record_exists")
  expect_identical(readLines(file), lines)
})

test_that("triage preserves the failure and writes evidence when a reproduction is unavailable", {
  root <- withr::local_tempdir()
  writeLines("data work.sample; value = 1; run;", file.path(root, "sample.sas"))
  testthat::local_mocked_bindings(run_program_pipeline = function(...) {
    cli::cli_abort("Scheduler failed with SECRET_RUNTIME_DETAIL", class = "sas2r_pipeline_coverage_error")
  })
  requests <- list()
  llm <- diagnosis_mock(bug_triage_answer(""), function(request) {
    requests[[length(requests) + 1L]] <<- request
  })
  out <- withr::local_tempdir()
  expect_error(suppressMessages(sas_translate(root, out_dir = out, llm = llm,
    config = list(migration = list(bug_reports = TRUE, report_diagnosis = "off")))),
    class = "sas2r_pipeline_coverage_error")
  expect_length(requests, 1L)
  expect_match(request_task_text(requests[[1L]]), "sas2r_pipeline_coverage_error", fixed = TRUE)
  expect_false(grepl("SECRET_RUNTIME_DETAIL", request_task_text(requests[[1L]]), fixed = TRUE))
  report <- list.files(out, pattern = "^report[.]json$", recursive = TRUE, full.names = TRUE)
  expect_length(report, 1L)
  data <- read_json_record(report, simplifyVector = FALSE)
  expect_identical(data$status, "blocked")
  expect_identical(data$package_bug_diagnosis$findings[[1L]]$reproduction, "no_example")
  expect_equal(data$usage$calls, 1)
  file <- file.path(out, "draft.md")
  expect_identical(sas_bug_report(report, file), file)
  draft <- paste(readLines(file), collapse = "\n")
  expect_match(draft, "no runnable example supplied", fixed = TRUE)
  expect_match(draft, "Try the operation with synthetic values", fixed = TRUE)
  expect_false(grepl("SECRET_RUNTIME_DETAIL|```r", draft))
})

test_that("triage respects the budget and retains only findings grounded in supplied IDs", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$config$migration$bug_reports <- TRUE
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()))
  request <- NULL
  answer <- bug_triage_answer("invalid(")
  answer$findings[[2L]] <- answer$findings[[1L]]
  answer$findings[[2L]]$evidence_id <- "E999"
  state$reviewer_llm <- diagnosis_mock(answer, function(req) request <<- req)
  diagnosis <- suppressMessages(migration_package_bug_diagnosis(state))
  expect_identical(diagnosis$status, "completed")
  expect_length(diagnosis$findings, 1L)
  expect_identical(diagnosis$findings[[1L]]$reproduction, "no_example")
  expect_null(diagnosis$findings[[1L]]$repro_r)
  expect_match(request_task_text(request), "CONSTANT_OFFSET", fixed = TRUE)
  expect_false(grepl("RECORD_SENTINEL|REFERENCE_PATH_SENTINEL|3653", request_task_text(request)))

  state$usage_budget$max_calls <- 0
  request <- NULL
  expect_identical(migration_package_bug_diagnosis(state)$status, "budget_unavailable")
  expect_null(request)
  state$reviewer_llm <- NULL
  expect_identical(migration_package_bug_diagnosis(state)$status, "not_configured")
  state$assessment <- NULL
  expect_identical(migration_package_bug_diagnosis(state)$status, "not_needed")
  expect_false(normalize_migration_config(NULL)$bug_reports)
  expect_error(normalize_migration_config(list(bug_reports = "true")),
    class = "sas2r_config_error")
})

test_that("fallback triage gives the model the schema and explains rejected responses", {
  state <- repair_workflow_fixture(n = 1L, failures = integer())$state
  state$config$migration$bug_reports <- TRUE
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()))
  llm <- new_llm(function(request) {
    expect_identical(request$schema_mode, "fallback")
    schema <- jsonlite::toJSON(request$output_schema, auto_unbox = TRUE, pretty = TRUE)
    if (grepl(as.character(schema), request$messages[[1L]]$content, fixed = TRUE)) {
      return(list(type = "final", data = bug_triage_answer()))
    }
    list(type = "final", data = list(findings = list(list(evidence_ids = list("E1"),
      suspected_violation = "Package behavior may differ from its contract."))))
  }, provider = "mock", model = "diagnosis-test",
  capabilities = llm_capabilities(structured_output = "fallback"))

  result <- suppressMessages(migration_package_bug_diagnosis(state, llm))
  expect_identical(result$status, "completed")
  expect_identical(result$findings[[1L]]$reproduction, "not_run")

  wrong_fields <- list(findings = list(list(evidence_ids = list("E1"),
    suspected_violation = "PRIVATE_MODEL_TEXT")))
  result <- suppressMessages(migration_package_bug_diagnosis(state, diagnosis_mock(wrong_fields)))
  expect_identical(result$status, "unavailable")
  expect_true("missing field: evidence_id" %in% result$validation_errors)
  expect_true("Unexpected field in the answer." %in% result$validation_errors)
  expect_false(any(grepl("PRIVATE_MODEL_TEXT", c(result$reason, result$validation_errors))))
  expect_length(result$findings, 0L)

  result <- suppressMessages(migration_package_bug_diagnosis(state,
    diagnosis_mock(list(raw = "PRIVATE_MODEL_TEXT", parse_error = TRUE))))
  expect_identical(result$validation_errors, "The answer could not be parsed as JSON.")
  expect_false(grepl("PRIVATE_MODEL_TEXT", result$reason, fixed = TRUE))
})

test_that("the installed sample drafts a reproduction without a reviewer", {
  demo <- system.file("examples", "bug-report-demo", "report.json", package = "sas2r")
  file <- withr::local_tempfile(fileext = ".md")
  expect_identical(sas_bug_report(demo, file), file)
  lines <- readLines(file)
  expect_true(any(grepl("draft R example not run", lines, fixed = TRUE)))
  code <- lines[seq.int(match("```r", lines) + 1L, match("```", lines) - 1L)]
  expect_message(eval(parse(text = code)), "rejected the destination")
})
