completed_record <- function(id, role, total = NULL, cached = NULL, output = NULL) {
  record <- list(record_type = "request_completed", request_id = id, role = role)
  record$total_input_tokens <- total
  record$cached_input_tokens <- cached
  record$total_output_tokens <- output
  record
}

usage_budget_from <- function(records) {
  starts <- lapply(records, function(r) list(record_type = "request_started", request_id = r$request_id))
  budget <- new_usage_budget()
  reconstruct_usage_budget(budget, c(starts, records))
  budget
}

test_that("the cache read rate counts only requests that reported cached input", {
  budget <- usage_budget_from(list(
    completed_record("a", "translator", total = 1000, cached = 800, output = 50),
    completed_record("b", "translator", total = 1000, cached = 0, output = 50),
    # No cached figure: this request must not drag the rate towards zero.
    completed_record("c", "reviewer", total = 4000, output = 20)
  ))
  usage <- migration_usage_summary(budget)
  expect_equal(usage$cache_read_rate, 800 / 2000)

  none <- migration_usage_summary(usage_budget_from(list(
    completed_record("a", "translator", total = 1000, output = 5)
  )))
  expect_true(is.na(none$cache_read_rate))
  expect_true(is.na(migration_usage_summary(list())$cache_read_rate))

  # A cached count above the input total is not a rate; the per-request rule
  # in usage_cache_read_rate() rejects it and so does the run-level figure.
  impossible <- migration_usage_summary(usage_budget_from(list(
    completed_record("a", "translator", total = 10, cached = 50)
  )))
  expect_true(is.na(impossible$cache_read_rate))
})

test_that("usage is broken down by the role that made each completed request", {
  budget <- usage_budget_from(list(
    completed_record("a", "translator", total = 1000, cached = 800, output = 50),
    completed_record("b", "translator", total = 1000, cached = 0, output = 70),
    completed_record("c", "reviewer", total = 4000, output = 20),
    completed_record("d", "fixer"),
    completed_record("e", NULL, total = 5, cached = 5, output = 1)
  ))
  by_role <- migration_usage_summary(budget)$by_role
  expect_identical(names(by_role), c("agent", "fixer", "reviewer", "translator"))
  expect_identical(by_role$translator$calls, 2L)
  expect_equal(by_role$translator$input_tokens, 2000)
  expect_equal(by_role$translator$output_tokens, 120)
  expect_equal(by_role$translator$cached_input_tokens, 800)
  expect_equal(by_role$translator$cache_read_rate, 0.4)
  expect_equal(by_role$reviewer$input_tokens, 4000)
  expect_true(is.na(by_role$reviewer$cached_input_tokens))
  expect_true(is.na(by_role$reviewer$cache_read_rate))
  expect_identical(by_role$fixer$calls, 1L)
  expect_true(is.na(by_role$fixer$input_tokens))
  expect_equal(by_role$agent$cache_read_rate, 1)
  expect_identical(sum(vapply(by_role, `[[`, 0L, "calls")), 5L)
  expect_identical(migration_usage_summary(list())$by_role, list())
})

test_that("early answers are counted from the run's request log", {
  dir <- withr::local_tempdir()
  expect_null(gathered_answer_summary(dir))
  expect_null(gathered_answer_summary(NULL))
  entry <- function(value) as.character(jsonlite::toJSON(
    list(agent = "translator", phase = "gathering", type = "final", gathered_answer = value),
    auto_unbox = TRUE))
  writeLines(c(
    entry("schema_valid"), entry("schema_valid"), entry("schema_valid"),
    entry("schema_valid_not_accepted"), entry("not_schema_valid"), entry("tools_closed"),
    '{"agent":"translator","phase":"finalization","type":"final"}',
    'not json but mentions "gathered_answer"',
    '{"gathered_answer":["schema_valid","schema_valid"]}'
  ), file.path(dir, "llm_log.jsonl"))
  summary <- gathered_answer_summary(dir)
  expect_identical(summary$accepted, 3L)
  expect_identical(summary$valid_not_accepted, 1L)
  expect_identical(summary$not_valid, 1L)
  expect_identical(summary$tools_closed, 1L)
  # tools_closed is not an answer the model gave, so it is not in the rate.
  expect_equal(summary$reuse_rate, 3 / 5)

  writeLines(entry("tools_closed"), file.path(dir, "llm_log.jsonl"))
  expect_true(is.na(gathered_answer_summary(dir)$reuse_rate))
})

test_that("usage lines add the new figures and leave older summaries unchanged", {
  budget <- usage_budget_from(list(
    completed_record("a", "translator", total = 1000, cached = 800, output = 50),
    completed_record("b", "reviewer", total = 3000000, output = 20)
  ))
  usage <- migration_usage_summary(budget)
  usage$gathered_answers <- list(accepted = 3L, valid_not_accepted = 1L, not_valid = 1L,
    tools_closed = 2L, reuse_rate = 0.6)
  lines <- migration_usage_lines(usage)
  expect_true(any(grepl("prompt cache: 80.0%.", lines, fixed = TRUE)))
  role_line <- grep("^Completed requests by role", lines, value = TRUE)
  expect_match(role_line, "translator 1 (input 1000, output 50, cache read 80.0%)", fixed = TRUE)
  # Large totals are printed in full, and an unreported cache is said so.
  expect_match(role_line, "reviewer 1 (input 3000000, output 20, cache read not reported)", fixed = TRUE)
  expect_true(any(grepl("3 of 5 accepted as final", lines, fixed = TRUE)))
  expect_match(lines[length(lines)], "^Effective limits")

  # A summary without the new fields prints exactly the three original lines.
  old <- usage[setdiff(names(usage), c("cache_read_rate", "by_role", "gathered_answers"))]
  expect_length(migration_usage_lines(old), 3L)
  expect_identical(migration_usage_lines(old), lines[c(1L, 2L, length(lines))])

  # No early answers recorded: no line about them.
  usage$gathered_answers <- list(accepted = 0L, valid_not_accepted = 0L, not_valid = 0L,
    tools_closed = 1L, reuse_rate = NA_real_)
  expect_false(any(grepl("accepted as final", migration_usage_lines(usage), fixed = TRUE)))
})

test_that("the run reports carry the usage breakdown and early-answer counts", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$usage_budget <- usage_budget_from(list(
    completed_record("a", "translator", total = 1000, cached = 250, output = 50)
  ))
  dir.create(state$paths$logs, recursive = TRUE, showWarnings = FALSE)
  writeLines('{"agent":"translator","phase":"gathering","type":"final","gathered_answer":"schema_valid"}',
    file.path(state$paths$logs, "llm_log.jsonl"))
  write_migration_report(state)
  report <- jsonlite::read_json(state$paths$report_json, simplifyVector = FALSE)
  expect_equal(report$usage$cache_read_rate, 0.25)
  expect_identical(report$usage$by_role$translator$calls, 1L)
  expect_identical(report$usage$gathered_answers$accepted, 1L)
  for (path in c(state$paths$report_md, state$paths$start_here)) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(text, "prompt cache: 25.0%", fixed = TRUE)
    expect_match(text, "Completed requests by role: translator 1", fixed = TRUE)
    expect_match(text, "1 of 1 accepted as final", fixed = TRUE)
  }
})
