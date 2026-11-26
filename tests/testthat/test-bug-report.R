bug_triage_answer <- function(code = "stopifnot(sas2r::sas_sum(c(1, 2)) == 3)") {
  list(findings = list(list(evidence_id = "E1", responsible_step = "runtime_helpers",
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
    is_triage <- identical(request$schema_name, "package_bug_triage_v2")
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

test_that("after-run triage records a grounded missing variable even without a finding", {
  state <- repair_workflow_fixture(n = 1L, failures = integer())$state
  state$config$migration$bug_reports <- TRUE
  code <- "out <- date_parts('STARTDTC', prefix = 'START')(data)"
  state <- stage_workflow_revision(state, "p01", code, "reviewed_no_material_finding")
  macro <- "macro__date_parts"
  macro_code <- "date_parts <- function(column, prefix) function(data) data"
  state$selected_revisions[[macro]] <- list(revision_id = "macro-r1", r_code = macro_code)
  caller <- state$graph$nodes$node_id[state$graph$nodes$component_id == "p01"][1L]
  state$graph$nodes <- dplyr::bind_rows(state$graph$nodes,
    tibble::tibble(node_id = macro, component_id = macro, type = "macro"))
  state$graph$edges <- dplyr::bind_rows(state$graph$edges,
    tibble::tibble(from = macro, to = caller, type = "macro_call"))
  message <- "date_parts: variable STARTDTC is not in the data"
  state$attempt <- list(kind = "bundle", completed = TRUE, attempt_id = "attempt-1",
    condition = list(component_id = "p01", class = c("simpleError", "error", "condition"),
      message = paste(message, "PRIVATE_RECORD", sep = "\n"), call_names = c("sys.source", "eval")),
    revision_manifest = list(p01 = list(revision_id = state$selected_revisions$p01$revision_id,
      r_hash = migration_hash(code)), macro__date_parts = list(revision_id = "macro-r1",
      r_hash = migration_hash(macro_code))))
  request <- NULL
  llm <- diagnosis_mock(list(findings = list()), function(req) request <<- req)
  diagnosis <- suppressMessages(migration_package_bug_diagnosis(state, llm))
  expect_identical(diagnosis$status, "completed")
  expect_length(diagnosis$findings, 0L)
  task <- request_task_text(request)
  expect_match(task, '"kind":"column_not_found"', fixed = TRUE)
  expect_match(task, '"called_function":"date_parts"', fixed = TRUE)
  expect_match(task, '"identifiers":"STARTDTC"', fixed = TRUE)
  expect_match(task, message, fixed = TRUE)
  expect_match(task, macro_code, fixed = TRUE)
  expect_false(grepl("PRIVATE_RECORD", task, fixed = TRUE))
  expect_identical(diagnosis$observed_failures, paste("Bundle execution failed - p01 -", message))
  lines <- package_bug_report_lines(diagnosis)
  expect_true(any(grepl(message, lines, fixed = TRUE)))

  expect_identical(package_bug_evidence(state, NULL, NULL)$E1$code_ids, c("p01", macro))
  state$attempt$revision_manifest[[macro]]$r_hash <- migration_hash("different macro")
  expect_identical(package_bug_evidence(state, NULL, NULL)$E1$code_ids, "p01")
  state$attempt$revision_manifest$p01$r_hash <- migration_hash("different code")
  item <- package_bug_evidence(state, NULL, NULL)$E1
  expect_identical(item$facts$kind, "unclassified_error")
  expect_null(item$facts$called_function)
  expect_length(item$facts$identifiers, 0L)
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

test_that("after-run triage sees dependency deferrals and source-bound member evidence", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "input"))
  saveRDS(data.frame(value = 1), file.path(root, "input", "member.rds"))
  writeLines(c("%macro copy(indata);", "data work.out; set &indata; run;", "%mend;",
    "%copy(indata=raw.member);"), file.path(root, "derive.sas"))
  setup <- translation_setup(root, list(libraries = list(raw = "input"),
    migration = list(bug_reports = TRUE)), outputs = NULL, recursive = FALSE)
  state <- list(project = setup$project, graph = setup$plan$graph, schedule = setup$plan$schedule,
    config = setup$config, usage_budget = new_usage_budget(max_calls = 1L),
    diagnostics = list(dependency_findings = list(derive = list(
      findings = c("raw.member", "%missing_macro"), affected = "derive",
      reason = "source_reconciliation_required"))))
  request <- NULL
  llm <- diagnosis_mock(bug_triage_answer(""), function(req) request <<- req)
  result <- suppressMessages(migration_package_bug_diagnosis(state, llm))
  expect_identical(result$status, "completed")
  task <- request_task_text(request)
  expect_match(task, "Execution deferred by dependency reconciliation", fixed = TRUE)
  expect_match(task, "raw.member", fixed = TRUE)
  expect_match(task, "available_in_bound_library", fixed = TRUE)
  expect_match(task, "%missing_macro", fixed = TRUE)
  expect_false(grepl(root, task, fixed = TRUE))
  expect_identical(result$findings[[1L]]$reproduction, "no_example")
})

# Attribution facts. Each case below is a made-up migration assessed by the
# real gate, and the model is a stand-in that follows the signs in the prompt.
# What separates the cases is therefore in the request itself.

triage_sas <- "data work.out1; set raw.input; value = value + 1; run;"
triage_r <- "x <- lib_read('raw', 'input')\nx$value <- x$value + 1\nlib_write(x, 'work', 'out1')"
triage_days <- c(21000, 21001, 21002)

test_that("a type difference that holds the same values points at the write and read conversions", {
  stored <- data.frame(id = 1:3, visitdt = triage_days, value = c(11, 12, 13))
  as_dates <- function(days) as.Date(days - 3653, origin = "1970-01-01")
  state <- triage_scenario(triage_sas, triage_r, generated = stored,
    reference = transform(stored, visitdt = as_dates(visitdt)),
    input = data.frame(id = 1:3, visitdt = triage_days, value = c(10, 11, 12)))
  state$assessment$targets$work.out1$date_columns <- list(typed = list(), deferred = list(
    list(var = "visitdt", format = "DATE9.", evidence = "source",
      reason = "there is no reference column to confirm the date format"),
    list(var = "otherdt", format = "DATE9.", evidence = "source", reason = "not a differing column")))
  result <- triage_with_sign_reader(state)
  column <- triage_column(result$facts, "visitdt")
  expect_identical(column[c("difference", "reference_type", "generated_type")],
    list(difference = "type", reference_type = "date", generated_type = "numeric"))
  expect_identical(column$generated_code, "passed_through")
  expect_identical(column$representation_only, "yes")
  expect_identical(column$conversion, "sas_date_day_number")
  dates <- result$facts$evidence$E1$decisions$date_columns
  expect_length(dates, 1L)
  expect_identical(dates[[1L]][c("name", "decision", "basis", "format")],
    list(name = "visitdt", decision = "left_as_day_number", basis = "source", format = "DATE9."))
  finding <- result$diagnosis$findings[[1L]]
  expect_identical(finding$responsible_step, "write_and_read_conversions")
  expect_true(any(grepl("Package step: write and read conversions",
    package_bug_report_lines(result$diagnosis), fixed = TRUE)))
  expect_true("Suspected package step: write and read conversions" %in% bug_report_finding_lines(finding))
  # The flag is the only thing the second look at the data sends.
  expect_false(grepl("21000|21001|2027-|3653", result$request$messages[[2L]]$content))

  # Day numbers that are not the reference dates are a different finding.
  state <- triage_scenario(triage_sas, triage_r, generated = stored,
    reference = transform(stored, visitdt = as_dates(visitdt + 1)))
  result <- triage_with_sign_reader(state)
  expect_identical(triage_column(result$facts, "visitdt")$representation_only, "no")
  expect_length(result$diagnosis$findings, 0L)

  # A labelled number is still a number, on whichever side it is.
  labelled <- stored
  labelled$visitdt <- haven::labelled(triage_days, label = "Visit date")
  state <- triage_scenario(triage_sas, triage_r, generated = transform(stored, visitdt = as_dates(visitdt)),
    reference = labelled)
  column <- package_bug_attribution(state, state$assessment$targets$work.out1, "work.out1", "p01")$columns[[1L]]
  expect_identical(column[c("reference_type", "generated_type", "representation_only")],
    list(reference_type = "numeric", generated_type = "date", representation_only = "yes"))
})

test_that("a storage requirement between integer and double is answered from the generated values", {
  whole <- data.frame(id = 1:3, value = c(11, 12, 13))
  rules <- function(type) list(types = list(value = type))
  state <- triage_scenario(triage_sas, triage_r, whole, whole, rules = rules("integer"))
  target <- state$assessment$targets$work.out1
  expect_identical(target$status, "failed")
  column <- package_bug_attribution(state, target, "work.out1", "p01")$columns[[1L]]
  expect_identical(column[c("name", "checks", "required_type", "generated_type", "representation_only", "conversion")],
    list(name = "value", checks = "types", required_type = "integer", generated_type = "double",
      representation_only = "yes", conversion = "numeric_storage"))
  fraction <- transform(whole, value = value + 0.5)
  state <- triage_scenario(triage_sas, triage_r, fraction, fraction, rules = rules("integer"))
  column <- package_bug_attribution(state, state$assessment$targets$work.out1, "work.out1", "p01")$columns[[1L]]
  expect_identical(column$representation_only, "no")
})

test_that("a column the generated programs pass through and that changed points at reading", {
  kept <- data.frame(id = 1:3, grade = c("CELL_A", "CELL_B", "CELL_C"), value = c(11, 12, 13))
  lost <- transform(kept, grade = c("CELL_A", NA, "CELL_C"))
  state <- triage_scenario(triage_sas, triage_r, generated = lost, reference = kept)
  result <- triage_with_sign_reader(state)
  column <- triage_column(result$facts, "grade")
  expect_identical(column[c("difference", "extent", "sas_source", "generated_code")],
    list(difference = "values", extent = "missing_only", sas_source = "not_named",
      generated_code = "passed_through"))
  expect_null(column$assigned_in)
  expect_identical(result$facts$evidence$E1$external_inputs, "raw.input")
  expect_identical(result$diagnosis$findings[[1L]]$responsible_step, "reading_inputs_and_references")
  expect_false(grepl("CELL_", result$request$messages[[2L]]$content, fixed = TRUE))

  # Code that changes columns without naming them leaves the question open.
  sweeping <- "x <- lib_read('raw', 'input')\nx <- dplyr::mutate(x, dplyr::across(dplyr::everything(), toupper))\nlib_write(x, 'work', 'out1')"
  state <- triage_scenario(triage_sas, sweeping, generated = lost, reference = kept)
  result <- triage_with_sign_reader(state)
  expect_identical(triage_column(result$facts, "grade")$generated_code, "unknown")
  expect_length(result$diagnosis$findings, 0L)
})

test_that("a helper called as documented with a wrong result points at the runtime helpers", {
  sas <- "data work.out1; set raw.input; value = round(value, 0.1); run;"
  r <- "x <- lib_read('raw', 'input')\nx$value <- sas_round(x$value, 0.1)\nlib_write(x, 'work', 'out1')"
  state <- triage_scenario(sas, r,
    generated = data.frame(id = 1:3, value = c(10.1, 11.1, 12.2)),
    reference = data.frame(id = 1:3, value = c(10.1, 11.2, 12.2)))
  result <- triage_with_sign_reader(state)
  column <- triage_column(result$facts, "value")
  expect_identical(column[c("sas_source", "generated_code", "assigned_in", "helpers")],
    list(sas_source = "assigned", generated_code = "assigned", assigned_in = "p01", helpers = "sas_round"))
  expect_identical(result$facts$runtime_helpers, "package")
  # The judgement that the call is faithful needs the helper's documentation
  # and both statements, so the request must carry all three.
  text <- result$request$messages[[2L]]$content
  expect_match(text, "\nsas_round: ", fixed = TRUE)
  expect_match(text, "value = round(value, 0.1)", fixed = TRUE)
  expect_match(text, "sas_round(x$value, 0.1)", fixed = TRUE)
  expect_identical(result$diagnosis$findings[[1L]]$responsible_step, "runtime_helpers")

  # Helpers a model changed during the run are no longer the package's.
  patched <- withr::local_tempfile(fileext = ".R")
  writeLines(assemble_helper_overlay(runtime_helper_code(state$runtime),
    "sas_round <- function(x, unit = 1) x"), patched)
  state$runtime$helpers <- patched
  result <- triage_with_sign_reader(state)
  expect_identical(result$facts$runtime_helpers, "patched_during_run")
  expect_length(result$diagnosis$findings, 0L)
})

test_that("a wrong type given to the translator points at the facts sas2r supplied", {
  # The source compares two character columns. Nothing in it says so, and the
  # inferred input types call both numeric.
  sas <- "data work.out1; set raw.input; if visit > limit then late = 1; run;"
  r <- paste("x <- lib_read('raw', 'input')", "x$visit <- as.numeric(x$visit)",
    "x$late <- ifelse(x$visit > as.numeric(x$limit), 1, NA)", "lib_write(x, 'work', 'out1')", sep = "\n")
  reference <- data.frame(id = 1:3, visit = c("01", "02", "03"), limit = "02", late = c(NA, NA, 1))
  state <- triage_scenario(sas, r, input = reference[c("id", "visit", "limit")],
    generated = transform(reference, visit = c(1, 2, 3)), reference = reference)
  result <- triage_with_sign_reader(state)
  column <- triage_column(result$facts, "visit")
  expect_identical(column$translator_told, list(list(input = "raw.input", type = "numeric")))
  expect_identical(column[c("reference_type", "generated_type", "sas_source", "generated_code")],
    list(reference_type = "character", generated_type = "numeric", sas_source = "named_not_assigned",
      generated_code = "assigned"))
  expect_identical(column$representation_only, "not_checked")
  expect_identical(result$diagnosis$findings[[1L]]$responsible_step, "facts_given_to_translator")
})

test_that("a statement the model left out yields no package finding", {
  sas <- "data work.out1; set raw.input; value = value + 1; flag = 1; run;"
  generated <- data.frame(id = 1:3, value = c(11, 12, 13))
  reference <- transform(generated, flag = 1)
  result <- triage_with_sign_reader(triage_scenario(sas, triage_r, generated, reference))
  column <- triage_column(result$facts, "flag")
  expect_identical(column[c("difference", "sas_source", "generated_code")],
    list(difference = "missing_in_generated", sas_source = "assigned", generated_code = "not_named"))
  expect_identical(result$facts$programs[[1L]]$origin, "model")
  expect_identical(result$diagnosis$status, "completed")
  expect_length(result$diagnosis$findings, 0L)

  # The same omission in a program sas2r's own rules wrote is the package's.
  result <- triage_with_sign_reader(triage_scenario(sas, triage_r, generated, reference,
    origin = NA_character_))
  expect_identical(result$facts$programs[[1L]]$origin, "rule_based")
  expect_identical(result$diagnosis$findings[[1L]]$responsible_step,
    "rule_based_translation_and_support_files")
})

test_that("a difference class repeated across independent programs is rolled up", {
  sas <- c(triage_sas, "data work.out2; set raw.input; value = value * 2; run;")
  r <- c(triage_r, "x <- lib_read('raw', 'input')\nx$value <- x$value * 2\nlib_write(x, 'work', 'out2')")
  generated <- list(data.frame(id = 1:3, visitdt = triage_days, value = c(11, 12, 13)),
    data.frame(id = 1:3, visitdt = triage_days, value = c(20, 22, 24)))
  reference <- lapply(generated, function(data)
    transform(data, visitdt = as.Date(visitdt - 3653, origin = "1970-01-01")))
  state <- triage_scenario(sas, r, generated, reference,
    input = data.frame(id = 1:3, visitdt = triage_days, value = c(10, 11, 12)))
  result <- triage_with_sign_reader(state)
  shared <- result$facts$shared_patterns
  expect_length(shared, 1L)
  expect_identical(shared[[1L]][c("class", "reference_type", "generated_type", "columns", "programs",
    "common_inputs", "share_of_failed_outputs")], list(class = "type_difference", reference_type = "date",
    generated_type = "numeric", columns = "visitdt", programs = "independent", common_inputs = "raw.input",
    share_of_failed_outputs = "all"))
  expect_identical(unlist(shared[[1L]]$targets), c("work.out1", "work.out2"))
  expect_identical(unlist(shared[[1L]]$evidence), c("E1", "E2"))
  # The roll-up comes first, and every fact comes before any code.
  text <- result$request$messages[[2L]]$content
  position <- function(needle) regexpr(needle, text, fixed = TRUE)[[1L]]
  expect_true(all(diff(c(position("Facts:"), position('"shared_patterns":['), position('"evidence":{'),
    position('"programs":[{'), position("Runtime helper documentation"),
    position("Code of the programs involved:"), position("Component: p01"))) > 0))

  # Outputs that share an upstream program say so; one output alone repeats nothing.
  state$assessment$lineage_by_target$work.out2$upstream_components <- c("p02", "p01")
  expect_identical(package_bug_shared_patterns(state)[[1L]][c("programs", "common_programs")],
    list(programs = "shared", common_programs = "p01"))
  state$assessment$targets$work.out2$status <- "passed"
  expect_length(package_bug_shared_patterns(state), 0L)
})

test_that("the comparison's own decisions are recorded as they were made", {
  generated <- data.frame(id = 1:3, rowkey = c("a", "b", "c"), value = c(11, 12, 99))
  state <- triage_scenario(triage_sas, triage_r, generated, transform(generated, value = c(11, 12, 13)),
    rules = list(keys = "rowkey", tol_abs = 0, tolerances = list(value = list(abs = 0.5))))
  target <- state$assessment$targets$work.out1
  # Whatever the gate paired on, the record keeps it apart from what was asked.
  target$differences$structure$selected_keys <- "id"
  decisions <- package_bug_decisions(state, target, "work.out1", "value")
  expect_identical(decisions$row_pairing, list(method = "unique_key", keys_used = "id",
    keys_from = "heuristic", keys_configured = "rowkey", row_order = "not_required"))
  expect_identical(decisions$numeric_tolerance, list(source = "configured", absolute = "zero",
    relative = "zero", column_overrides = "value"))
  expect_identical(decisions[c("reference_file", "generated_file")],
    list(reference_file = "rds", generated_file = "rds"))
  expect_identical(package_bug_fact_violations(list(decisions = decisions)), character())
  # A reason that could carry a path is left out; the decision itself stays.
  dates <- package_bug_date_decisions(list(typed = list(list(var = "VALUE", format = "DATE9.",
    evidence = "reference", reason = "declared in /PRIVATE_PATH/derive.sas"))), "value")
  expect_identical(dates, list(list(name = "value", decision = "stored_as_date", basis = "reference",
    format = "DATE9.")))
})

test_that("the request keeps room for code and names every section it cut", {
  state <- triage_scenario(triage_sas, triage_r, data.frame(id = 1:3, value = c(11, 12, 99)),
    data.frame(id = 1:3, value = c(11, 12, 13)))
  facts <- list(evidence = list(E1 = list(note = strrep("f", 600L))))
  documentation <- list(sas_round = strrep("d", 600L))
  whole <- package_bug_request_context(state, facts, documentation, "p01")
  expect_false(whole$truncated)
  expect_length(whole$truncated_sections, 0L)
  expect_false(grepl("Context truncated", whole$text, fixed = TRUE))
  code <- nchar(whole$text) - regexpr("\nCode of the programs involved:", whole$text, fixed = TRUE)[[1L]] + 1L

  # Too little room for everything: code keeps its share, documentation gives way first.
  tight <- package_bug_request_context(state, facts, documentation, "p01", limit = code + 700L)
  expect_identical(tight$truncated_sections, "helper_documentation")
  expect_match(tight$text, triage_r, fixed = TRUE)
  expect_match(tight$text, "[Context truncated (helper_documentation); omitted content is unknown.]", fixed = TRUE)
  tighter <- package_bug_request_context(state, facts, documentation, "p01", limit = code + 300L)
  expect_identical(tighter$truncated_sections, c("facts", "helper_documentation"))
  expect_match(tighter$text, triage_r, fixed = TRUE)
  smallest <- package_bug_request_context(state, facts, documentation, "p01", limit = 100L)
  expect_identical(smallest$truncated_sections, c("facts", "helper_documentation", "code"))

  # The diagnosis record says what was cut. A long program does not push the
  # facts out: they are whole and still come first.
  long <- paste0(triage_r, "\n", strrep("# a long generated program\n", 1200L))
  state <- triage_scenario(triage_sas, long, data.frame(id = 1:3, value = c(11, 12, 99)),
    data.frame(id = 1:3, value = c(11, 12, 13)))
  result <- triage_with_sign_reader(state)
  expect_true(result$diagnosis$context_truncated)
  expect_identical(result$diagnosis$context_truncated_sections, "code")
  expect_identical(triage_column(result$facts, "value")$generated_code, "assigned")
  expect_lte(nchar(result$request$messages[[2L]]$content), PACKAGE_BUG_CONTEXT_LIMIT + 80L)
})

test_that("triage facts are held to a closed format and withheld when they leave it", {
  sentinel <- "CELL_SENTINEL|/PRIVATE_PATH|3653"
  state <- triage_scenario(triage_sas, triage_r, data.frame(id = 1:3, value = c(11, 12, 99)),
    data.frame(id = 1:3, value = c(11, 12, 13)))
  target <- state$assessment$targets$work.out1
  valid <- package_bug_target_facts(state, target, "work.out1", "p01", NULL, 8L)
  expect_identical(package_bug_fact_violations(valid, c(target$candidate_path, target$reference_path)), character())
  expect_false(any(unlist(rapply(valid, is.numeric, how = "list"))))
  change <- function(edit) { facts <- valid; edit(facts) }
  cases <- list(
    "a numeric value is present" = change(function(f) { f$columns[[1L]]$offset <- 3653; f }),
    "unexpected field in columns (offset)" = change(function(f) { f$columns[[1L]]$offset <- 3653; f }),
    "unexpected field in columns" = change(function(f) { f$columns[[1L]][["CELL_SENTINEL = 3653"]] <- "x"; f }),
    "columns$name is outside its fixed format" = change(function(f) { f$columns[[1L]]$name <- "CELL_SENTINEL value"; f }),
    "columns$generated_code is outside its fixed format" =
      change(function(f) { f$columns[[1L]]$generated_code <- "CELL_SENTINEL"; f }),
    "columns$helpers is outside its fixed format" =
      change(function(f) { f$columns[[1L]]$helpers <- "sas_round\nCELL_SENTINEL"; f }),
    "decisions$row_pairing$keys_used is outside its fixed format" =
      change(function(f) { f$decisions$row_pairing$keys_used <- "CELL_SENTINEL 1"; f }),
    "decisions$reference_file is outside its fixed format" =
      change(function(f) { f$decisions$reference_file <- "/PRIVATE_PATH/data.rds"; f }),
    "columns is not a list of records" = change(function(f) { f$columns <- list(value = "CELL_SENTINEL"); f }),
    "unexpected block in the facts" = change(function(f) { f$examples <- list("CELL_SENTINEL"); f }),
    "a local path is present" = change(function(f) { f$helpers_called <- target$reference_path; f }))
  for (i in seq_along(cases)) {
    problems <- package_bug_fact_violations(cases[[i]], c(target$candidate_path, target$reference_path))
    expect_true(names(cases)[i] %in% problems, info = paste(names(cases)[i], "<-", paste(problems, collapse = " | ")))
    expect_false(any(grepl(sentinel, problems)), info = names(cases)[i])
  }

  # A constructor defect: under test it stops at once, in a run the block is
  # withheld, the omission is recorded and the target keeps its other facts.
  local_mocked_bindings(package_bug_decisions = function(...) list(reference_file = target$reference_path))
  expect_error(package_bug_evidence(state, NULL, NULL, attribute = TRUE),
    class = "sas2r_package_bug_fact_violation")
  withr::local_options(sas2r.strict_discrepancy_summary = FALSE)
  withheld <- new.env(parent = emptyenv())
  warning <- expect_warning(
    evidence <- package_bug_evidence(state, NULL, NULL, omitted = withheld, attribute = TRUE),
    class = "sas2r_package_bug_facts_omitted")
  expect_null(evidence$E1$facts$columns)
  expect_null(evidence$E1$facts$decisions)
  expect_identical(evidence$E1$facts$status, "failed")
  expect_length(evidence$E1$facts$discrepancy$variables, 1L)
  expect_identical(withheld$items[[1L]][c("type", "target")],
    list(type = "package_bug_facts_omitted", target = "work.out1"))
  expect_false(grepl(basename(dirname(target$reference_path)), conditionMessage(warning), fixed = TRUE))
})

test_that("a finding must name one of the package-owned steps", {
  schema <- jsonlite::read_json(system.file("schemas", "package-bug-triage-v2.json", package = "sas2r"))
  steps <- unlist(schema$properties$findings$items$properties$responsible_step$enum)
  expect_identical(steps, PACKAGE_BUG_STEPS)
  prompt <- paste(readLines(system.file("prompts", "package-bug-triage.md", package = "sas2r")), collapse = "\n")
  for (step in steps) expect_match(prompt, paste0(match(step, steps), ". ", step, "."), fixed = TRUE)

  state <- repair_workflow_fixture(n = 1L, failures = integer())$state
  state$config$migration$bug_reports <- TRUE
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()))
  answer <- bug_triage_answer("")
  answer$findings[[1L]]$responsible_step <- "the_translator"
  result <- suppressMessages(migration_package_bug_diagnosis(state, diagnosis_mock(answer)))
  expect_identical(result$status, "unavailable")
  expect_length(result$findings, 0L)
  expect_true(any(grepl("responsible_step not in enum", result$validation_errors, fixed = TRUE)))
  answer$findings[[1L]]$responsible_step <- NULL
  result <- suppressMessages(migration_package_bug_diagnosis(state, diagnosis_mock(answer)))
  expect_true("missing field: responsible_step" %in% result$validation_errors)
  # A report saved before the field existed still renders.
  expect_false(any(grepl("package step", bug_report_finding_lines(list(evidence_id = "E1")), ignore.case = TRUE)))
})

test_that("the compared datasets are read again only when a request can follow", {
  state <- triage_scenario(triage_sas, triage_r, data.frame(id = 1:3, value = c(11, 12, 99)),
    data.frame(id = 1:3, value = c(11, 12, 13)))
  reads <- 0L
  real <- read_assessed_dataset
  local_mocked_bindings(read_assessed_dataset = function(path) { reads <<- reads + 1L; real(path) },
    package_bug_attribution = function(...) { reads <<- reads + 1L; list() })
  expect_identical(migration_package_bug_diagnosis(state, llm = NULL)$status, "not_configured")
  state$usage_budget$max_calls <- 0
  expect_identical(migration_package_bug_diagnosis(state, diagnosis_mock(list(findings = list())))$status,
    "budget_unavailable")
  state$config$migration$bug_reports <- FALSE
  expect_null(migration_package_bug_diagnosis(state, diagnosis_mock(list(findings = list()))))
  expect_identical(reads, 0L)
})

test_that("the scan of generated R tells an assignment from a mention and admits what it cannot see", {
  roles <- function(code, columns) {
    use <- r_column_use(code, columns)
    c(vapply(use$columns, `[[`, "", "role"), unnamed = as.character(use$unnamed))
  }
  code <- paste("x <- lib_read('raw', 'input')", "x$a <- sas_round(x$b, 1)", "x[['c']] <- 1",
    "x <- dplyr::mutate(x, d = e + 1)", "x <- dplyr::filter(x, f > 0)", "attr(x$g, 'label') <- 'G'",
    "x <- sas_sort(x, by = 'h i')", "x <- x[, c('a', 'j')]", sep = "\n")
  expect_identical(roles(code, c("A", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")),
    c(a = "assigned", b = "named", c = "assigned", d = "assigned", e = "named", f = "named",
      g = "assigned", h = "named", i = "named", j = "named", k = "not_named", unnamed = "FALSE"))
  use <- r_column_use(code, c("a", "b", "h", "k"))
  expect_identical(use$columns$a$helpers, "sas_round")
  expect_identical(use$columns$b$helpers, "sas_round")
  expect_identical(use$columns$h$helpers, "sas_sort")
  expect_identical(use$helpers, c("lib_read", "sas_round", "sas_sort"))
  for (dynamic in c("x[[name]] <- 1", "names(x) <- toupper(names(x))",
      "x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))",
      "x <- dplyr::rename_with(x, toupper)", "x[] <- lapply(x, trimws)")) {
    expect_identical(roles(dynamic, "k")[["unnamed"]], "TRUE", info = dynamic)
  }
  broken <- r_column_use("x <- (", "k")
  expect_false(broken$parsed)
  expect_identical(broken$columns$k$role, "unknown")
  expect_identical(r_column_use("", "k")$columns$k$role, "unknown")
})

test_that("the scan of SAS source tells an assignment from a mention and admits what it cannot see", {
  sas <- paste("data work.out; set raw.input(rename=(old=renamed));", "length declared $ 8;",
    "/* commented = 1; */", "if a > 0 then assigned = 1; else other = 2;", "total + 1;",
    "where status = 'x';", "keep kept;", "run;",
    "proc sql; create table t as select b, c * 2 as aliased from x; quit;")
  expect_identical(sas_column_use(sas, c("ASSIGNED", "other", "total", "renamed", "declared", "aliased",
      "status", "kept", "a", "commented", "absent")),
    c(assigned = "assigned", other = "assigned", total = "assigned", renamed = "assigned",
      declared = "assigned", aliased = "assigned", status = "named_not_assigned",
      kept = "named_not_assigned", a = "named_not_assigned", commented = "not_named", absent = "not_named"))
  # Macro text can build a name that is never written out.
  expect_identical(sas_column_use("data out; set in; &prefix.flag = 1; run;", c("flag", "in")),
    c(flag = "unknown", `in` = "named_not_assigned"))
  expect_identical(sas_column_use("", "a"), c(a = "unknown"))
})

test_that("a revision's record says who wrote it", {
  expect_identical(revision_origin(list(agent_status = NA_character_)), "rule_based")
  expect_identical(revision_origin(list(agent_status = "ok")), "model")
  expect_identical(revision_origin(list(agent_status = "ok", mechanical_retry = list(errors = "x"))),
    "model_after_check_retry")
  expect_identical(revision_origin(list(agent_status = "provider_error")), "rule_based_model_unavailable")
  expect_identical(revision_origin(list(prior_revision_id = "r1", agent_status = "ok")), "model_repair")
  expect_identical(revision_origin(list(prior_revision_id = "r1", helper_changed = TRUE)),
    "model_repair_with_helper_patch")
  expect_identical(revision_origin(list(r_code = "x <- 1")), "unknown")
  expect_identical(revision_origin(NULL), "unknown")
})
