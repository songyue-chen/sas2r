# The reviewer-facing comparison summary is the one comparison record a model
# may see. These tests hold it to a closed format: fixed fields, fixed
# vocabulary, no numbers, no reference path.

sentinel <- "RECORD_SENTINEL|REFERENCE_PATH_SENTINEL|3653"

valid_reviewer_summary <- function() reviewer_discrepancy_summary(discrepancy_fixture())

test_that("the suite runs with the strict summary check switched on", {
  expect_true(isTRUE(getOption("sas2r.strict_discrepancy_summary")))
})

test_that("a summary built from a comparison fits the closed format", {
  summary <- valid_reviewer_summary()
  expect_identical(reviewer_summary_violations(summary, discrepancy_fixture()$reference_path), character())
  expect_setequal(names(summary), REVIEWER_SUMMARY_FIELDS)
  expect_gt(length(summary$variables), 0L)
  expect_false(any(unlist(rapply(summary, is.numeric, how = "list"))))

  # So does one with nothing to report, and one for a target never compared.
  empty <- reviewer_discrepancy_summary(list(kind = "dataset", target_key = "work.none", status = "unverified"))
  expect_identical(empty$rows, "unknown")
  expect_identical(reviewer_summary_violations(empty), character())
  typed <- discrepancy_fixture()
  typed$differences$structure$kind_mismatch <- data.frame(var = "eventdt", base_kind = "numeric", comp_kind = "date")
  expect_length(reviewer_discrepancy_summary(typed)$type_differences, 1L)
})

test_that("every way out of the closed format is caught, without quoting a value", {
  reference <- discrepancy_fixture()$reference_path
  base <- valid_reviewer_summary()
  change <- function(edit) { summary <- base; edit(summary) }
  cases <- list(
    "unexpected field in the summary (absolute_offset)" = change(function(s) { s$absolute_offset <- 3653; s }),
    "a numeric value is present" = change(function(s) { s$absolute_offset <- 3653; s }),
    "unexpected field in the summary (examples)" = change(function(s) { s$examples <- list("RECORD_SENTINEL_1"); s }),
    "unexpected field in the summary" = change(function(s) { s[["RECORD_SENTINEL_1 = 3653"]] <- "x"; s }),
    "missing field in the summary" = change(function(s) { s$row_pairing <- NULL; s }),
    "schema_version is not the current one" = change(function(s) { s$schema_version <- "1"; s }),
    "target, kind, status or alignment is not a short label" =
      change(function(s) { s$alignment <- paste(rep("RECORD_SENTINEL_1", 40), collapse = " "); s }),
    "target, kind, status or alignment is not a short label" =
      change(function(s) { s$status <- "failed\nRECORD_SENTINEL_1"; s }),
    "reference_present is not TRUE or FALSE" = change(function(s) { s$reference_present <- NA; s }),
    "rows or columns is outside the fixed vocabulary" = change(function(s) { s$rows <- "3653 more"; s }),
    "rows or columns is outside the fixed vocabulary" = change(function(s) { s$columns <- c("same", "same"); s }),
    "row_pairing is outside the fixed vocabulary" = change(function(s) { s$row_pairing <- "3 of 3"; s }),
    "variables is not a list of records" = change(function(s) { s$variables <- list(day = "3653"); s }),
    "unexpected field in a variable record (mismatches)" =
      change(function(s) { s$variables[[1]]$mismatches <- 3; s }),
    "a variable's differences value is outside the fixed vocabulary" =
      change(function(s) { s$variables[[1]]$differences <- "3 of 3"; s }),
    "a variable's patterns are outside the fixed vocabulary" =
      change(function(s) { s$variables[[1]]$patterns <- c("CONSTANT_OFFSET", "+3653"); s }),
    "a variable name or kind is not a short label" = change(function(s) { s$variables[[1]]$name <- c("day", "3653"); s }),
    "unexpected field in a type-difference record (example)" = change(function(s) {
      s$type_differences <- list(list(name = "day", reference_type = "numeric", generated_type = "date", example = "3653")); s }),
    "a type-difference name or type is not a short label" = change(function(s) {
      s$type_differences <- list(list(name = "day", reference_type = 1, generated_type = "date")); s }),
    "a reference path is present" = change(function(s) { s$alignment <- reference; s }),
    "a reference path is present" = change(function(s) { s$variables[[1]]$name <- paste0("x", reference); s })
  )
  for (i in seq_along(cases)) {
    problems <- reviewer_summary_violations(cases[[i]], reference)
    expect_true(names(cases)[i] %in% problems, info = paste(names(cases)[i], "<-", paste(problems, collapse = " | ")))
    expect_false(any(grepl(sentinel, problems)), info = names(cases)[i])
  }
  expect_identical(reviewer_summary_violations("3653"), "the summary is not a list")
  # Every allowed field, removed in turn, is missed: none is optional.
  for (field in REVIEWER_SUMMARY_FIELDS) {
    summary <- base; summary[[field]] <- NULL
    expect_true("missing field in the summary" %in% reviewer_summary_violations(summary), info = field)
  }
})

# A constructor defect of the kind the check exists for: a magnitude and a
# reference path find their way into the model-facing summary.
local_leaking_summary <- function(envir = parent.frame()) {
  real <- dataset_discrepancy_summary
  local_mocked_bindings(dataset_discrepancy_summary = function(target, key = target$target_key) {
    summary <- real(target, key)
    summary$variables[[1L]]$patterns <- c(summary$variables[[1L]]$patterns, "OFFSET +3653")
    summary$alignment <- target$reference_path
    summary
  }, .env = envir)
}

test_that("under test a summary outside the format stops at once", {
  local_leaking_summary()
  expect_error(reviewer_discrepancy_summary(discrepancy_fixture()),
    class = "sas2r_discrepancy_summary_violation")
})

test_that("in a run the summary is withheld, the omission is recorded and nothing stops", {
  withr::local_options(sas2r.strict_discrepancy_summary = FALSE)
  local_leaking_summary()
  withheld <- new.env(parent = emptyenv())
  summary <- "not yet"
  warning <- expect_warning(
    summary <- reviewer_discrepancy_summary(discrepancy_fixture(), omitted = withheld),
    class = "sas2r_discrepancy_summary_omitted")
  expect_null(summary)
  expect_length(withheld$items, 1L)
  expect_identical(withheld$items[[1L]]$type, "discrepancy_summary_omitted")
  expect_identical(withheld$items[[1L]]$target, "work.result")
  expect_setequal(withheld$items[[1L]]$reasons,
    c("a variable's patterns are outside the fixed vocabulary", "a reference path is present"))
  expect_false(grepl(sentinel, conditionMessage(warning)))
  # No collector supplied: still withheld, still no error.
  expect_null(suppressWarnings(reviewer_discrepancy_summary(discrepancy_fixture())))
})

test_that("report diagnosis sends no withheld summary and says what it left out", {
  withr::local_options(sas2r.strict_discrepancy_summary = FALSE)
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()),
    lineage_by_target = list(work.result = list(upstream_components = "p01")))
  requests <- list()
  state$reviewer_llm <- diagnosis_mock(list(explanations = list()),
    callback = function(request) requests[[length(requests) + 1L]] <<- request)
  local_leaking_summary()
  advice <- suppressWarnings(migration_report_diagnosis(state))
  expect_length(requests, 1L)
  text <- paste(vapply(requests[[1L]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_false(grepl(sentinel, text))
  expect_false(grepl("CONSTANT_OFFSET", text, fixed = TRUE))
  # The component's code is still offered for the unresolved target.
  expect_match(text, "Component: p01", fixed = TRUE)
  expect_identical(advice$discrepancy_summary_omitted[[1L]]$target, "work.result")
})

test_that("bug triage evidence carries no withheld summary and records the omission", {
  withr::local_options(sas2r.strict_discrepancy_summary = FALSE)
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()),
    lineage_by_target = list(work.result = list(upstream_components = "p01")))
  local_leaking_summary()
  withheld <- new.env(parent = emptyenv())
  evidence <- suppressWarnings(package_bug_evidence(state, NULL, NULL, omitted = withheld))
  item <- Filter(function(x) identical(x$facts$target, "work.result"), evidence)[[1L]]
  expect_null(item$facts$discrepancy)
  expect_identical(item$facts$status, "failed")
  expect_length(withheld$items, 1L)
  expect_false(grepl(sentinel, paste(unlist(item$facts), collapse = " ")))
})
