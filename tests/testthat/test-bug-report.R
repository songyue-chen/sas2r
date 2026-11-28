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
  page <- paste(readLines(html), collapse = "\n")
  expect_match(page, "Reproduction unverified", fixed = TRUE)
  # The page holds the draft itself: what was recorded, and the example unrun.
  expect_match(page, "<li>Failure stage: Bundle execution failed</li>", fixed = TRUE)
  expect_match(page, "REPRO_MUST_NOT_RUN", fixed = TRUE)
  expect_identical(finding$binding$scope, "Bundle execution failed")

  file <- file.path(root, "draft.md")
  expect_error(sas_bug_report(disabled, file), class = "sas2r_bug_diagnosis_disabled")
  expect_false(file.exists(file))
  expect_identical(sas_bug_report(enabled, file), file)
  lines <- readLines(file)
  draft <- paste(lines, collapse = "\n")
  expect_match(draft, "Proposed synthetic reproduction", fixed = TRUE)
  expect_match(draft, "draft R example not run", fixed = TRUE)
  expect_match(draft, "REPRO_MUST_NOT_RUN", fixed = TRUE)
  expect_true("- Failure stage: Bundle execution failed" %in% lines)
  expect_false(any(grepl("could not be linked", lines, fixed = TRUE)))
  expect_false(grepl("RAW_DIAGNOSTIC_SENTINEL", draft, fixed = TRUE))
  expect_error(sas_bug_report(enabled, file), class = "sas2r_record_exists")
  expect_identical(readLines(file), lines)
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

# Attribution facts. Each case below is a made-up migration assessed by the
# real gate, and the model is a stand-in that follows the signs in the prompt.
# What separates the cases is therefore in the request itself.

triage_sas <- "data work.out1; set raw.input; value = value + 1; run;"
triage_r <- "x <- lib_read('raw', 'input')\nx$value <- x$value + 1\nlib_write(x, 'work', 'out1')"
triage_days <- c(21000, 21001, 21002)

test_that("a type difference that holds the same values points at the write and read conversions", {
  stored <- data.frame(id = 1:3, visitdt = triage_days, otherdt = triage_days, value = c(11, 12, 13))
  as_dates <- function(days) as.Date(days - 3653, origin = "1970-01-01")
  input <- data.frame(id = 1:3, visitdt = triage_days, otherdt = triage_days, value = c(10, 11, 12))
  # The generated program declares both columns as dates where it writes the
  # dataset, and the reference stores both as dates. The generated dataset
  # holds one of them as the SAS day number.
  dated_sas <- "data work.out1; set raw.input; format visitdt otherdt date9.; value = value + 1; run;"
  dated_r <- sub("'out1')", "'out1', formats = c(visitdt = 'DATE9.', otherdt = 'DATE9.'))", triage_r, fixed = TRUE)
  state <- triage_scenario(dated_sas, dated_r,
    generated = transform(stored, otherdt = as_dates(otherdt)),
    reference = transform(stored, visitdt = as_dates(visitdt), otherdt = as_dates(otherdt)), input = input)
  # The gate wrote this record itself; the test does not build it.
  recorded <- state$assessment$targets$work.out1$date_columns
  expect_identical(vapply(recorded$typed, function(x) x$var, ""), "otherdt")
  expect_identical(vapply(recorded$deferred, function(x) x$var, ""), "visitdt")
  result <- triage_with_sign_reader(state)
  column <- triage_column(result$facts, "visitdt")
  expect_identical(column[c("difference", "reference_type", "generated_type")],
    list(difference = "type", reference_type = "date", generated_type = "numeric"))
  # The program names the column only to declare its format, in the call that
  # writes the dataset. That call could have changed how it is stored.
  expect_identical(column$generated_code, "named_not_assigned")
  expect_true("lib_write" %in% unlist(result$facts$evidence$E1$helpers_called))
  expect_identical(column$representation_only, "yes")
  expect_identical(column$conversion, "sas_date_day_number")
  # A SAS format the source names is not an input dataset.
  expect_identical(result$facts$evidence$E1$external_inputs, "raw.input")
  # Only the differing column's decision is sent.
  dates <- result$facts$evidence$E1$decisions$date_columns
  expect_identical(dates, list(list(name = "visitdt", decision = "left_as_day_number",
    basis = "declared", format = "DATE9", reason = recorded$deferred[[1L]]$reason)))
  finding <- result$diagnosis$findings[[1L]]
  expect_identical(finding$responsible_step, "write_and_read_conversions")
  expect_true(any(grepl("Package step: write and read conversions",
    package_bug_report_lines(result$diagnosis), fixed = TRUE)))
  expect_true("Suspected package step: write and read conversions" %in%
    bug_report_markdown(bug_report_finding_blocks(list(), finding, 1L)))
  # The flag is the only thing the second look at the data sends.
  expect_false(grepl("21000|21001|2027-|3653", result$request$messages[[2L]]$content))

  # A program that declares no date format leaves the day number as it is.
  # When a model wrote that program, the missing declaration is not the
  # package's doing; when the rules wrote it, it is.
  reference <- transform(stored, visitdt = as_dates(visitdt))
  result <- triage_with_sign_reader(triage_scenario(dated_sas, triage_r, generated = stored,
    reference = reference, input = input))
  expect_identical(triage_column(result$facts, "visitdt")$representation_only, "yes")
  dates <- result$facts$evidence$E1$decisions$date_columns
  expect_identical(dates[[1L]][c("name", "decision", "basis")],
    list(name = "visitdt", decision = "left_as_day_number", basis = "reference"))
  expect_match(dates[[1L]]$reason, "declares no date format", fixed = TRUE)
  expect_length(result$diagnosis$findings, 0L)
  result <- triage_with_sign_reader(triage_scenario(dated_sas, triage_r, generated = stored,
    reference = reference, input = input, origin = NA_character_))
  expect_identical(result$diagnosis$findings[[1L]]$responsible_step, "rule_based_translation_and_support_files")

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

test_that("a date column left as a number carries the gate's own reason, and never a path", {
  # The program declares a date format for a column its dataset holds as a
  # plain number, and the reference has no such column to compare with.
  dated_r <- sub("'out1')", "'out1', formats = c(visitdt = 'DATE9.'))", triage_r, fixed = TRUE)
  generated <- data.frame(id = 1:3, visitdt = triage_days, value = c(11, 12, 13))
  state <- triage_scenario(triage_sas, dated_r, generated = generated, reference = generated[c("id", "value")],
    input = data.frame(id = 1:3, visitdt = triage_days, value = c(10, 11, 12)))
  recorded <- state$assessment$targets$work.out1$date_columns
  expect_length(recorded$deferred, 1L)
  result <- triage_with_sign_reader(state)
  expect_identical(triage_column(result$facts, "visitdt")$difference, "extra_in_generated")
  dates <- result$facts$evidence$E1$decisions$date_columns
  expect_identical(dates[[1L]][c("name", "decision", "basis", "format")],
    list(name = "visitdt", decision = "left_as_day_number", basis = "declared", format = "DATE9"))
  # The reason is the sentence the gate wrote, one of its fixed sentences.
  expect_identical(dates[[1L]]$reason, recorded$deferred[[1L]]$reason)
  expect_match(dates[[1L]]$reason, "^the program declares a date format for this column")
  expect_setequal(names(dates[[1L]]), c("name", "decision", "basis", "format", "reason"))

  # A reason that held a path, or ran to several lines, would be left out.
  with_reason <- function(reason) {
    record <- recorded
    record$deferred[[1L]]$reason <- reason
    package_bug_date_decisions(record, "visitdt")[[1L]]
  }
  expect_null(with_reason(file.path(tempdir(), "p01.sas"))$reason)
  expect_null(with_reason("C:\\study\\p01.sas")$reason)
  expect_null(with_reason("first line\nsecond line")$reason)
  expect_null(with_reason(strrep("a", 241L))$reason)
  expect_identical(with_reason("a short reason")$reason, "a short reason")
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

  # So does code that rebuilds the whole data frame through a function.
  for (rebuild in c("x <- as.data.frame(lapply(x, trimws))", "x <- purrr::map_df(x, trimws)",
      "cols <- lapply(x, trimws)\nx <- as.data.frame(cols)")) {
    rebuilt <- paste("x <- lib_read('raw', 'input')", rebuild, "lib_write(x, 'work', 'out1')", sep = "\n")
    result <- triage_with_sign_reader(triage_scenario(triage_sas, rebuilt, generated = lost, reference = kept))
    expect_identical(triage_column(result$facts, "grade")$generated_code, "unknown", info = rebuild)
    expect_length(result$diagnosis$findings, 0L)
  }

  # A call that names the column can have changed it, so the column is not
  # called passed through and nothing points at the package.
  for (naming in c("x <- tidyr::fill(x, grade)", "x <- tidyr::replace_na(x, list(grade = ''))")) {
    named <- paste("x <- lib_read('raw', 'input')", naming, "lib_write(x, 'work', 'out1')", sep = "\n")
    result <- triage_with_sign_reader(triage_scenario(triage_sas, named, generated = lost, reference = kept))
    expect_identical(triage_column(result$facts, "grade")$generated_code, "named_not_assigned", info = naming)
    expect_length(result$diagnosis$findings, 0L)
  }

  # Deriving one column through a function leaves the others passed through.
  derived <- "x <- lib_read('raw', 'input')\nx$value <- vapply(x$value, function(v) v + 1, numeric(1))\nlib_write(x, 'work', 'out1')"
  result <- triage_with_sign_reader(triage_scenario(triage_sas, derived, generated = lost, reference = kept))
  expect_identical(triage_column(result$facts, "grade")$generated_code, "passed_through")
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
  decisions <- package_bug_decisions(state, target, "work.out1", "value")
  # The gate paired the rows on the key that was configured.
  expect_identical(decisions$row_pairing, list(method = "unique_key", keys_used = "rowkey",
    keys_from = "configured", keys_configured = "rowkey", row_order = "not_required"))
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

test_that("the keys rows were paired on are told apart from the keys that were configured", {
  # Each case reads the record the gate wrote for a made-up comparison.
  pairing <- function(generated, reference, keys, sas = triage_sas) {
    state <- triage_scenario(sas, triage_r, generated, reference, rules = list(keys = keys))
    target <- state$assessment$targets$work.out1
    expect_identical(target$status, "failed")
    decisions <- package_bug_decisions(state, target, "work.out1", "value")
    expect_identical(package_bug_fact_violations(list(decisions = decisions)), character())
    decisions$row_pairing
  }
  visits <- data.frame(usubjid = rep(c("S1", "S2"), each = 3L), aseq = 1:6, value = c(11, 12, 13, 14, 15, 16))
  wrong <- transform(visits, value = value + 1)

  # A configured key that repeats is refined by a key sas2r chose: here a
  # guess from the column names.
  expect_identical(pairing(wrong, visits, "usubjid"), list(method = "unique_key",
    keys_used = c("usubjid", "aseq"), keys_from = "configured", keys_refined_by = "heuristic",
    keys_configured = "usubjid", row_order = "not_required"))

  # With a source that orders the rows by those columns, the refining key is
  # read from the source instead.
  sorted <- paste("proc sort data=raw.input out=work.s; by usubjid aseq; run;",
    "data work.out1; set work.s; value = value + 1; run;", sep = "\n")
  from_source <- pairing(wrong, visits, "usubjid", sas = sorted)
  expect_identical(from_source$keys_used, c("usubjid", "aseq"))
  expect_false(from_source$keys_refined_by %in% c("heuristic", "unknown"))

  # A group the two sides number differently is matched by content, so the
  # configured key alone is the key used and nothing refined it.
  renumbered <- transform(visits, value = ifelse(usubjid == "S1", value, value + 1),
    aseq = ifelse(usubjid == "S1", aseq + 100L, aseq))
  expect_identical(pairing(renumbered, visits, "usubjid"), list(method = "duplicate_key_multiset",
    keys_used = "usubjid", keys_from = "configured", keys_configured = "usubjid", row_order = "not_required"))

  # A configured key whose column differs in type cannot pair rows. The key
  # used is then one sas2r inferred, and the facts say so.
  typed <- pairing(data.frame(id = 1:3, rowkey = c(1, 2, 3), value = c(11, 12, 99)),
    data.frame(id = 1:3, rowkey = c("1", "2", "3"), value = c(11, 12, 13), stringsAsFactors = FALSE), "rowkey")
  expect_identical(typed[c("keys_used", "keys_from", "keys_configured")],
    list(keys_used = "id", keys_from = "heuristic", keys_configured = "rowkey"))
  expect_null(typed$keys_refined_by)

  # With no keys configured the origin is the inferred one, as before.
  state <- triage_scenario(triage_sas, triage_r, wrong, visits)
  inferred <- package_bug_decisions(state, state$assessment$targets$work.out1, "work.out1", "value")$row_pairing
  expect_identical(inferred$keys_configured, character())
  expect_false(inferred$keys_from %in% c("configured", "none", "unknown"))
  expect_null(inferred$keys_refined_by)

  # The refining origin is held to its fixed words like every other fact.
  bad <- list(decisions = list(row_pairing = list(keys_refined_by = "CELL_SENTINEL 1")))
  expect_identical(package_bug_fact_violations(bad), "decisions$row_pairing$keys_refined_by is outside its fixed format")
  bad$decisions$row_pairing$keys_refined_by <- "configured"
  expect_length(package_bug_fact_violations(bad), 1L)
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
  legacy <- bug_report_markdown(bug_report_finding_blocks(list(), list(evidence_id = "E1"), 1L))
  expect_true("## Finding 1 [E1]" %in% legacy)
  expect_false(any(grepl("package step", legacy, ignore.case = TRUE)))
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
  # A call that works on a whole object can rewrite every column of a data
  # frame, whatever its result is assigned to and however it is wrapped.
  for (whole in c("x <- as.data.frame(lapply(x, as.character))",
      "x <- purrr::modify_if(x, is.numeric, as.character)", "x <- purrr::map_df(x, as.character)",
      "x <- data.frame(sapply(x, as.character))", "x <- as.data.frame(t(apply(x, 1, function(r) r)))",
      "cols <- lapply(x, as.character)\nx <- as.data.frame(cols)", "x <- as.data.frame(as.matrix(x))",
      "x <- as.data.frame(lapply(names(x), function(n) as.character(x[[n]])))",
      "x <- Filter(function(col) !all(is.na(col)), x)", "x <- furrr::future_map_dfc(x, as.character)",
      "x <- dplyr::mutate(.data = as.data.frame(Map(as.character, x)), flag = 1)",
      "x$flag <- 1\ny <- vapply(x, class, '')", "x <- reshape2::dcast(x, id ~ variable)")) {
    expect_identical(roles(whole, "k")[["unnamed"]], "TRUE", info = whole)
  }
  # Its result going into a column written by name changes that column only.
  for (one_column in c("x$flag <- vapply(x$age, function(a) a > 65, logical(1))",
      "x[['flag']] <- unlist(lapply(strsplit(x$name, ' '), `[`, 1))",
      "x <- dplyr::mutate(x, flag = purrr::map_lgl(age, ~ .x > 65))",
      "x <- transform(x, flag = sapply(age, nchar))", "levels(x$flag) <- sapply(levels(x$flag), toupper)",
      "x <- dplyr::filter(x, age > 65)")) {
    expect_identical(roles(one_column, "k")[["unnamed"]], "FALSE", info = one_column)
  }
  # A call can change a column it names without an assignment, and can name it
  # as an argument name. Neither is an assignment, and neither is silence.
  for (naming in c("x <- tidyr::fill(x, k)", "x <- tidyr::replace_na(x, list(k = 0))",
      "x <- tidyr::separate(x, old, into = c('k', 'other'))")) {
    expect_identical(roles(naming, "k")[["k"]], "named", info = naming)
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

# The issue draft. Each case renders the saved report of a made-up run: two
# programs, each with an output the real gate compared with a reference on the
# configured keys. The second output is 177 rows whose key `visit` is missing
# on 60 reference rows and 0 on the generated side, so 117 rows pair.

draft_state <- function(envir = parent.frame()) {
  reference <- data.frame(id = seq_len(177L), visit = c(rep(NA_real_, 60L), rep(1, 117L)))
  generated <- transform(reference, visit = ifelse(is.na(visit), 0, visit))
  small <- data.frame(id = 1:3, visit = c(1, 1, 1), value = c(11, 12, 13))
  state <- triage_scenario(
    c("data work.out1; set raw.input; value = value + 1; run;", "data work.out2; set raw.input; run;"),
    c(triage_r, "lib_write(lib_read('raw', 'input'), 'work', 'out2')"),
    list(transform(small, value = c(11, 12, 99)), generated), list(small, reference),
    rules = list(keys = c("id", "visit")), envir = envir)
  state$selected_attempt <- list(attempt_id = "bundle_attempt_001")
  state
}

# Triage with a stand-in model that returns one finding on `evidence_id`, then
# the report as a run saves it.
draft_run <- function(state, evidence_id, code = "", envir = parent.frame()) {
  answer <- bug_triage_answer(code)
  answer$findings[[1L]]$evidence_id <- evidence_id
  requests <- list()
  llm <- diagnosis_mock(answer, function(request) requests[[length(requests) + 1L]] <<- request)
  state$package_bug_diagnosis <- suppressMessages(migration_package_bug_diagnosis(state, llm))
  suppressMessages(write_migration_report(state))
  # A later run of the same state writes to the same place: keep this one.
  kept <- withr::local_tempdir(.local_envir = envir)
  file.copy(c(state$paths$report_json, state$paths$start_here), kept)
  list(state = state, report = file.path(kept, basename(state$paths$report_json)),
    page = file.path(kept, basename(state$paths$start_here)), requests = function() requests)
}

draft_lines <- function(report, envir = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".md", .local_envir = envir)
  readLines(sas_bug_report(report, file))
}

# A saved report with changes, written back as a run would write it.
draft_changed <- function(report, change, envir = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  atomic_write_json(change(read_json_record(report, simplifyVector = FALSE)), path)
  path
}

test_that("the issue draft finds its output by the name saved with the finding", {
  state <- draft_state()
  # An earlier failure takes the first evidence ID: the outputs are E2 and E3.
  state$diagnostics$component_failures <- list(p01 = list(reason = "made-up failure"))
  third <- draft_run(state, "E3")
  finding <- third$state$package_bug_diagnosis$findings[[1L]]
  expect_identical(finding$binding[c("attempt_id", "scope", "target")],
    list(attempt_id = "bundle_attempt_001", scope = "Output assessment", target = "work.out2"))
  lines <- draft_lines(third$report)
  expect_true(all(c("## Finding 1 [E3]", "- Output: `work.out2`", "- Failed check: `reference_comparison`",
    "- Reference rows: 177") %in% lines))

  # E2 is the first output, although the second saved assessment is the other.
  second <- draft_lines(draft_run(state, "E2")$report)
  expect_true(all(c("- Output: `work.out1`", "- Reference rows: 3", "- Records paired: 3") %in% second))
  # Nothing of the other output. The draft holds a run ID with random
  # characters, so a bare number is not a safe thing to look for.
  expect_false(any(grepl("work.out2|rows: 177", second)))

  # The saved order of the assessments does not matter either.
  reordered <- draft_changed(third$report, function(report) {
    report$output_assessments <- rev(report$output_assessments)
    report
  })
  expect_identical(draft_lines(reordered), lines)

  # A finding about a program names its failure stage and has no output.
  first <- draft_lines(draft_run(state, "E1")$report)
  expect_true(all(c("- Failure stage: Component processing failed", "- Program: `p01`") %in% first))
  expect_false(any(grepl("^- Output:|could not be linked|Records paired", first)))
})

test_that("the start-here page and the Markdown export show the same draft", {
  state <- draft_state()
  marker <- file.path(withr::local_tempdir(), "example-was-run")
  code <- sprintf('file.create("%s")', gsub("\\", "/", marker, fixed = TRUE))
  run <- draft_run(state, "E2", code)
  # Exporting reads the saved report. It asks no model and runs no example.
  local_mocked_bindings(request_report_advice = function(...) stop("the export asked a model"))
  lines <- draft_lines(run$report)
  expect_length(run$requests(), 1L)
  expect_false(file.exists(marker))

  expect_identical(lines[c(1L, 3L)], c("# Possible sas2r bug",
    "This draft was written on your computer and has not been sent anywhere. Read it and decide what to share."))
  recorded <- c("- Output: `work.out2`", "- Failed check: `reference_comparison`",
    "- Keys configured: `id`, `visit`", "- Keys rows were paired on: `id`, `visit`",
    "- Pairing method: `unique_key` (each key value names one row on each side)",
    "- Reference rows: 177", "- Generated rows: 177", "- Records paired: 117",
    "- Reference rows left unpaired: 60", "- Generated rows left unpaired: 60",
    "- Row matching ran to its end: yes",
    paste("- Among the unpaired rows, 60 pairs agree on the other key columns and differ on `visit`.",
      "It is missing in 60 unpaired reference rows and 0 unpaired generated rows."),
    "- Bundle attempt: `bundle_attempt_001`")
  expect_identical(lines[lines %in% recorded], recorded)
  # The key line is what the comparison itself measured and the run saved with
  # the output's assessment: nothing was put into the report by hand.
  saved <- read_json_record(run$report, simplifyVector = FALSE)$output_assessments$work.out2
  expect_identical(saved$differences$structure$unpaired_keys,
    list(list(key = "visit", pair_without = 60L, missing_reference = 60L, missing_generated = 0L)))
  expect_identical(saved$differences$structure$selected_keys, list("id", "visit"))
  expect_identical(saved$status, "failed")
  # It is explained once, and it blames nobody.
  expect_identical(sum(lines == BUG_REPORT_KEY_PAIRS), 1L)
  expect_false(any(grepl("translation|comparator|bug in", lines[grepl("unpaired rows", lines)], ignore.case = TRUE)))
  # What the run recorded comes before what the AI suggested, each under its
  # own heading, and the reader is asked to read and decide: nothing is to be
  # removed for them.
  order <- match(c("### Recorded by sas2r", recorded[[1L]], "### Suggested by the AI (not verified)",
    "Suggested cause: `The package could be dispatching the wrong helper.`", code,
    "### Result on your machine"), lines)
  expect_false(anyNA(order))
  expect_false(is.unsorted(order))
  expect_true(paste("The part below holds your project's dataset and variable names, what the",
    "comparison with the reference measured, the AI's suggested cause and its proposed example.",
    "Read it and decide what to include.") %in% lines)
  # The run report's own sentence for the output; it may say more than this.
  expect_true(any(startsWith(lines, "Observed difference: `Some rows could not be paired")))
  expect_false(any(grepl("\\b(remove|delete|redact)", lines, ignore.case = TRUE)))
  # Names and counts only: no path of the compared files.
  target <- state$assessment$targets$work.out2
  expect_false(any(grepl(dirname(target$reference_path), lines, fixed = TRUE)))

  # The page section is the same draft, line for line, then how to save it.
  html <- readLines(run$page)
  section <- html[seq.int(grep("<h2>Possible sas2r bugs (AI beta)</h2>", html, fixed = TRUE) + 1L,
    grep("^<nav>", html)[[1L]] - 1L)]
  expect_true("<li>Records paired: 117</li>" %in% section)
  expect_true("<li>Row matching ran to its end: yes</li>" %in% section)
  expect_true(paste("<li>Among the unpaired rows, 60 pairs agree on the other key columns and differ on",
    "<code>visit</code>. It is missing in 60 unpaired reference rows and 0 unpaired generated rows.</li>") %in% section)
  expect_true("<li>Keys configured: <code>id</code>, <code>visit</code></li>" %in% section)
  shown <- gsub("<[^>]+>", "", section[!section %in% c("<ul>", "</ul>")])
  for (entity in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", '"'), c("&#39;", "'"), c("&amp;", "&"))) {
    shown <- gsub(entity[[1L]], entity[[2L]], shown, fixed = TRUE)
  }
  exported <- lines[nzchar(lines) & !grepl("^```", lines)][-1L]
  exported <- gsub("`|\\*\\*", "", sub("^(#+|-) ", "", exported))
  expect_identical(utils::head(shown, -1L), exported)
  expect_match(utils::tail(shown, 1L), "To save this draft as a Markdown file", fixed = TRUE)
  expect_false(any(grepl(dirname(target$reference_path), section, fixed = TRUE)))
  expect_false(file.exists(marker))

  # A destination that exists is kept as it is.
  kept <- withr::local_tempfile(fileext = ".md")
  writeLines("mine", kept)
  expect_error(sas_bug_report(run$report, kept), class = "sas2r_record_exists")
  expect_identical(readLines(kept), "mine")
})

test_that("a draft says so when an output's comparison details cannot be linked", {
  run <- draft_run(draft_state(), "E2")
  export <- function(change) draft_lines(draft_changed(run$report, change))
  facts <- "Records paired|Reference rows|Keys configured|Row matching|unpaired rows"
  # The linked draft shows the key line; none of the drafts below may.
  expect_true(any(grepl("^- Among the unpaired rows", draft_lines(run$report))))
  unlinked <- "The comparison details of this output could not be linked to this finding:"

  # A finding saved before the output's name was kept: what is there is
  # exported, and the draft says what could not be linked.
  legacy <- export(function(report) {
    report$package_bug_diagnosis$findings[[1L]]$binding <- list(attempt_id = "bundle_attempt_001")
    report
  })
  expect_true(paste("Comparison details could not be linked to this finding. It was saved without",
    "the name of the output or the failure stage it refers to.") %in% legacy)
  expect_true(all(c("Recorded symptom: `dataset output assessment - failed (reference_comparison)`",
    "- Bundle attempt: `bundle_attempt_001`", "### Suggested by the AI (not verified)") %in% legacy))
  expect_false(any(grepl(paste0(facts, "|^- Output:"), legacy)))

  # The saved assessment belongs to another attempt than the finding.
  other <- export(function(report) {
    report$selected_attempt_id <- "bundle_attempt_002"
    report
  })
  expect_true(all(c("- Output: `work.out2`",
    paste(unlinked, "the finding and the saved assessment belong to different bundle attempts.")) %in% other))
  expect_false(any(grepl(facts, other)))

  # No attempt recorded on one side, and no assessment saved for the output.
  unknown <- export(function(report) {
    report$selected_attempt_id <- NULL
    report
  })
  expect_true(paste(unlinked, "the bundle attempt of the finding or of the saved assessment was not recorded.") %in% unknown)
  absent <- export(function(report) {
    report$output_assessments$work.out2 <- NULL
    report
  })
  expect_true(paste(unlinked, "the saved report holds no assessment for this output.") %in% absent)
  expect_false(any(grepl(facts, c(unknown, absent))))

  # A comparison saved without its row counts keeps the keys and says which
  # counts were not recorded. A count of zero is a count.
  uncounted <- export(function(report) {
    report$output_assessments$work.out2$checks$reference_comparison$summary <- NULL
    report$output_assessments$work.out2$dimensions <- NULL
    report
  })
  expect_true(all(c("- Keys configured: `id`, `visit`", "- Reference rows: not recorded",
    "- Records paired: not recorded", "- Generated rows left unpaired: not recorded",
    "- Row matching ran to its end: yes") %in% uncounted))
  first <- draft_lines(draft_run(draft_state(), "E1")$report)
  expect_true(all(c("- Output: `work.out1`", "- Records paired: 3", "- Reference rows left unpaired: 0",
    "- Generated rows left unpaired: 0") %in% first))
  stopped <- export(function(report) {
    report$output_assessments$work.out2$differences$structure$alignment_resource_state <- "residual_group_limit"
    report
  })
  expect_true("- Row matching ran to its end: no, it stopped at a size limit" %in% stopped)
  expect_true("- Records paired: 117" %in% stopped)

  # An output that is not a dataset has no row comparison to show.
  demo <- draft_lines(system.file("examples", "bug-report-demo", "report.json", package = "sas2r"))
  expect_true(all(c("- Output: `listing.html`", "- Failed check: `structure_valid`") %in% demo))
  expect_false(any(grepl(paste0(facts, "|could not be linked"), demo)))
})

test_that("the draft tells configured keys from the keys used, and shows recorded key counts", {
  visits <- data.frame(usubjid = rep(c("S1", "S2"), each = 3L), aseq = 1:6, value = c(11, 12, 13, 14, 15, 16))
  state <- triage_scenario(triage_sas, triage_r, transform(visits, value = value + 1), visits,
    rules = list(keys = "usubjid"))
  state$selected_attempt <- list(attempt_id = "bundle_attempt_001")
  run <- draft_run(state, "E1")
  lines <- draft_lines(run$report)
  expect_true(all(c("- Keys configured: `usubjid`", "- Keys rows were paired on: `usubjid`, `aseq`",
    "- Records paired: 6", "Observed difference: `value: 6 constant difference (absolute 1)`") %in% lines))

  expect_false(any(grepl("unpaired rows", lines)))
  expect_false(BUG_REPORT_KEY_PAIRS %in% lines)

  # A saved record is shown as recorded, whichever way it reads back, a count
  # of zero included. One pair and one row are worded as one.
  counts <- list(list(key = "aseq", pair_without = 4, missing_reference = 2, missing_generated = 0),
    list(key = "usubjid", pair_without = 1, missing_reference = 1, missing_generated = 1))
  shown <- c(paste("- Among the unpaired rows, 4 pairs agree on the other key columns and differ on `aseq`.",
      "It is missing in 2 unpaired reference rows and 0 unpaired generated rows."),
    paste("- Among the unpaired rows, 1 pair agrees on the other key columns and differs on `usubjid`.",
      "It is missing in 1 unpaired reference row and 1 unpaired generated row."))
  with_record <- function(record) draft_lines(draft_changed(run$report, function(report) {
    report$output_assessments$work.out1$differences$structure$unpaired_keys <- record
    report
  }))
  recorded <- with_record(counts)
  expect_identical(recorded[grepl("Among the unpaired rows", recorded)], shown)
  expect_true(BUG_REPORT_KEY_PAIRS %in% recorded)
  target <- state$assessment$targets$work.out1
  target$differences$structure$unpaired_keys <- data.frame(key = c("aseq", "usubjid"),
    pair_without = c(4, 1), missing_reference = c(2, 1), missing_generated = c(0, 1))
  frame <- bug_report_markdown(bug_report_output_blocks(target, "work.out1", NULL))
  expect_identical(frame[grepl("Among the unpaired rows", frame)], shown)

  # A record that is not a column name with a count of pairs shows nothing:
  # no pair, no count, a name that is not one short line, or no name.
  for (record in list(list(list(key = "aseq", pair_without = 0, missing_reference = 2, missing_generated = 0)),
      list(list(key = "aseq", missing_reference = 2, missing_generated = 0)),
      list(list(key = "aseq", pair_without = "many")),
      list(list(key = "a name\nover two lines", pair_without = 4)),
      list(list(pair_without = 4)), list("aseq"), list())) {
    odd <- with_record(record)
    expect_false(any(grepl("unpaired rows|over two lines", odd)))
    expect_identical(odd, lines)
  }
  # Pairs without the counts of missing values: the pairs alone.
  partial <- with_record(list(list(key = "aseq", pair_without = 4, missing_reference = 2)))
  expect_identical(partial[grepl("Among the unpaired rows", partial)],
    "- Among the unpaired rows, 4 pairs agree on the other key columns and differ on `aseq`.")
})

test_that("what the draft needs is saved beside the finding and is not sent to the model", {
  state <- draft_state()
  state$diagnostics$component_failures <- list(p01 = list(reason = "made-up failure"))
  ask <- function() {
    request <- NULL
    diagnosis <- suppressMessages(migration_package_bug_diagnosis(state,
      diagnosis_mock(bug_triage_answer(), function(req) request <<- req)))
    list(binding = diagnosis$findings[[1L]]$binding, messages = request$messages)
  }
  plain <- ask()
  expect_identical(plain$binding, list(scope = "Component processing failed", component = "p01"))

  # Whatever the local fields hold, the request is the same request.
  evidence <- package_bug_evidence
  local_mocked_bindings(package_bug_evidence = function(...) lapply(evidence(...), function(item) {
    item$binding <- list(attempt_id = "ATTEMPT_SENTINEL", scope = "SCOPE_SENTINEL",
      component = "PROGRAM_SENTINEL", target = "TARGET_SENTINEL")
    item
  }))
  marked <- ask()
  expect_identical(marked$binding$target, "TARGET_SENTINEL")
  expect_identical(marked$messages, plain$messages)
  expect_false(grepl("SENTINEL", paste(unlist(marked$messages), collapse = "\n"), fixed = TRUE))
})

test_that("the key record stays out of the model request, and no row value enters either", {
  # Values that would be recognised anywhere they turned up.
  reference <- data.frame(id = sprintf("ROWVALUE_SENTINEL_%03d", 1:30), visit = c(rep(424242, 12L), rep(1, 18L)),
    stringsAsFactors = FALSE)
  generated <- transform(reference, visit = ifelse(visit == 424242, 737373, visit))
  state <- triage_scenario("data work.out1; set raw.input; run;",
    "lib_write(lib_read('raw', 'input'), 'work', 'out1')", generated, reference,
    rules = list(keys = c("id", "visit")))
  state$selected_attempt <- list(attempt_id = "bundle_attempt_001")
  expect_identical(state$assessment$targets$work.out1$differences$structure$unpaired_keys,
    list(list(key = "visit", pair_without = 12L, missing_reference = 0L, missing_generated = 0L)))

  request_text <- function(state) {
    request <- NULL
    suppressMessages(migration_package_bug_diagnosis(state,
      diagnosis_mock(bug_triage_answer(), function(req) request <<- req)))
    paste(unlist(request$messages), collapse = "\n")
  }
  sent <- request_text(state)
  expect_true(nzchar(sent))
  expect_false(grepl("SENTINEL|424242|737373", sent))
  expect_false(grepl("unpaired_keys|pair_without|missing_reference|missing_generated", sent))
  # The request is the same without the record, and with anything in its place.
  for (record in list(NULL, list(list(key = "KEY_SENTINEL", pair_without = 987654L,
      missing_reference = 987654L, missing_generated = 987654L)))) {
    changed <- state
    changed$assessment$targets$work.out1$differences$structure$unpaired_keys <- record
    expect_identical(request_text(changed), sent)
  }

  # The draft names the column and gives the counts. It holds no row's value.
  run <- draft_run(state, "E1")
  lines <- draft_lines(run$report)
  expect_true(paste("- Among the unpaired rows, 12 pairs agree on the other key columns and differ on `visit`.",
    "It is missing in 0 unpaired reference rows and 0 unpaired generated rows.") %in% lines)
  expect_false(any(grepl("SENTINEL|424242|737373", lines)))
  html <- readLines(run$page)
  section <- html[seq.int(grep("<h2>Possible sas2r bugs (AI beta)</h2>", html, fixed = TRUE) + 1L,
    grep("^<nav>", html)[[1L]] - 1L)]
  expect_true(any(grepl("12 pairs agree on the other key columns", section, fixed = TRUE)))
  expect_false(any(grepl("SENTINEL|424242|737373", section)))
})

test_that("the Markdown export writes the run's and the model's sentences as literal text", {
  run <- draft_run(draft_state(), "E2", code = "x <- 1")
  plain <- draft_lines(run$report)
  # The model's four sentences, saved with everything a Markdown reader acts on.
  hostile <- list(
    explanation = "Fix in step 2.\n\n# Not a heading\n- not a list\r\n1. not a step\tcc @someone see #12  ",
    expected = "use `sas_sum()` then ``` a fence",
    uncertainty = "`x` and `y`",
    next_step = paste("<details><summary>s</summary> <!-- [a](https://example.com)",
      "![i](https://example.com/i.png) _TYPE_ **b** | t | \\ https://example.com"))
  changed <- draft_changed(run$report, function(report) {
    finding <- report$package_bug_diagnosis$findings[[1L]]
    report$package_bug_diagnosis$findings[[1L]] <- utils::modifyList(finding, hostile)
    report
  })
  lines <- draft_lines(changed)
  expected <- c(
    "Suggested cause: `Fix in step 2. # Not a heading - not a list 1. not a step cc @someone see #12`",
    "Expected behavior: ````use `sas_sum()` then ``` a fence````",
    "Uncertainty: `` `x` and `y` ``",
    paste("Next step: `<details><summary>s</summary> <!-- [a](https://example.com)",
      "![i](https://example.com/i.png) _TYPE_ **b** | t | \\ https://example.com`"))
  expect_identical(lines[grepl("^(Suggested cause|Expected behavior|Uncertainty|Next step): ", lines)], expected)
  # Each sentence is one line, so the draft has the lines it had, and only
  # those four differ. No heading, list item or blank line was added.
  expect_length(lines, length(plain))
  expect_identical(which(lines != plain), match(expected, lines))
  expect_identical(lines[grepl("^#", lines)], plain[grepl("^#", plain)])
  expect_identical(grepl("^- ", lines), grepl("^- ", plain))
  # What the run recorded is written the same way; the package's own words are not.
  expect_true(all(c("Recorded symptom: `dataset output assessment - failed (reference_comparison)`",
    "Suspected package step: runtime helpers") %in% lines))
  expect_identical(sum(grepl("^Observed difference: `.*`$", lines)), 1L)
  expect_identical(bug_report_markdown(list(draft_statement("Observed difference",
    "_type_: 3 mismatches", literal = TRUE))), c("Observed difference: `_type_: 3 mismatches`", ""))

  # The page shows the same sentences, escaped, each on one line.
  report <- read_json_record(changed, simplifyVector = FALSE)
  html <- bug_report_html(bug_report_content(report))
  expect_true(all(c("<p>Suggested cause: Fix in step 2. # Not a heading - not a list 1. not a step cc @someone see #12</p>",
    "<p>Uncertainty: `x` and `y`</p>") %in% html))
  expect_true(any(startsWith(html, "<p>Next step: &lt;details&gt;&lt;summary&gt;s&lt;/summary&gt; &lt;!-- [a]")))
  expect_false(any(grepl("<details|<!--|<summary", html)))

  # A sentence of nothing but spaces and line breaks is left out, in both.
  blank <- draft_changed(run$report, function(report) {
    report$package_bug_diagnosis$findings[[1L]]$uncertainty <- " \n\t "
    report
  })
  expect_false(any(grepl("^Uncertainty", draft_lines(blank))))
  expect_false(any(grepl("Uncertainty", bug_report_html(bug_report_content(read_json_record(blank, simplifyVector = FALSE))))))

  # Proposed code sits inside a fence longer than any run of backticks in it.
  fenced <- draft_changed(run$report, function(report) {
    report$package_bug_diagnosis$findings[[1L]]$repro_r <- "x <- '```'\n```` <- 1\n# @someone"
    report
  })
  code <- draft_lines(fenced)
  open <- match("`````r", code)
  expect_false(is.na(open))
  expect_identical(code[open + 1:4], c("x <- '```'", "```` <- 1", "# @someone", "`````"))
  expect_identical(sum(grepl("^`{5,}", code)), 2L)
})
