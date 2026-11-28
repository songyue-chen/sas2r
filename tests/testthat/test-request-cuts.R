completed_request <- function(id, seconds, status = "completed",
                              error_class = NULL, timeout_seconds = 900) {
  list(
    record_type = "request_completed", request_id = id, status = status,
    error_class = error_class, duration_ms = seconds * 1000,
    timeout_seconds = timeout_seconds
  )
}

cut_request <- function(id, seconds, ...) completed_request(
  id, seconds, status = "failed", error_class = "sas2r_llm_transport_error", ...
)

test_that("failures that end at the same elapsed time are reported as one cut", {
  # Break caught: ten requests closed at 181 seconds were recorded as ten
  # unrelated transport failures, so nothing told the user that a time limit
  # outside the package was ending every long request.
  cuts <- usage_request_cuts(list(
    completed_request("r1", 172.5),
    cut_request("r2", 180.6),
    cut_request("r3", 181.3),
    cut_request("r4", 181.0),
    cut_request("r5", 47.2),
    completed_request("r6", 12.0)
  ))

  expect_identical(cuts$requests, 3L)
  expect_identical(cuts$shortest_seconds, 180.6)
  expect_identical(cuts$longest_seconds, 181.3)
  expect_identical(cuts$timeout_seconds, 900)
})

test_that("failures that do not share an elapsed time are not reported as a cut", {
  # Break caught: calling scattered network failures a time limit would send
  # the user looking for a proxy that is not there.
  cases <- list(
    `one long failure` = list(cut_request("a", 181.0), completed_request("b", 30)),
    `two failures at different times` = list(cut_request("a", 61.0), cut_request("b", 181.0)),
    `failures before any wait` = list(
      cut_request("a", 0.4), cut_request("b", 0.5), cut_request("c", 0.6)
    ),
    `failures at the configured limit` = list(
      cut_request("a", 899.6), cut_request("b", 900.2)
    ),
    `same time but not connection failures` = list(
      completed_request("a", 181.0, "failed", "sas2r_llm_rate_limit"),
      completed_request("b", 181.2, "failed", "sas2r_llm_invalid_schema")
    ),
    `no failures` = list(completed_request("a", 181.0), completed_request("b", 181.1)),
    `no requests` = list()
  )
  for (name in names(cases)) {
    expect_null(usage_request_cuts(cases[[name]]), info = name)
  }
})

test_that("a cut without a recorded limit is still reported", {
  # Break caught: adapters that record no timeout would hide the cut.
  cuts <- usage_request_cuts(list(
    cut_request("a", 60.1, timeout_seconds = NULL),
    cut_request("b", 60.4, timeout_seconds = NULL)
  ))

  expect_identical(cuts$requests, 2L)
  expect_true(is.na(cuts$timeout_seconds))
})

test_that("the run summary states how many requests were cut, when, and against which limit", {
  # Break caught: a summary that omits the count, the elapsed time or the
  # configured limit leaves the reader unable to tell a cut from a timeout.
  budget <- list(records = list(
    cut_request("r1", 180.6), cut_request("r2", 181.3), completed_request("r3", 20)
  ))
  summary <- migration_usage_summary(budget)

  lines <- migration_request_cut_lines(summary)

  expect_length(lines, 2L)
  expect_match(lines[[1]], "2 model requests", fixed = TRUE)
  expect_match(lines[[1]], "180.6", fixed = TRUE)
  expect_match(lines[[1]], "181.3", fixed = TRUE)
  expect_match(lines[[1]], "900", fixed = TRUE)
})

test_that("a run without a cut adds nothing to the summary", {
  budget <- list(records = list(completed_request("r1", 20)))

  expect_identical(migration_request_cut_lines(migration_usage_summary(budget)), character())
})

test_that("the cut survives the report's JSON round trip", {
  # Break caught: the start-here page is built from report.json, so a cut that
  # only prints from the in-memory summary would be missing from that page.
  budget <- list(records = list(cut_request("r1", 180.6), cut_request("r2", 181.3)))
  summary <- migration_usage_summary(budget)
  path <- withr::local_tempfile(fileext = ".json")
  atomic_write_json(list(usage = summary), path)

  reread <- jsonlite::read_json(path)$usage

  expect_identical(migration_request_cut_lines(reread), migration_request_cut_lines(summary))
})

test_that("a cut is written to the report, the start-here page and the printed result", {
  # Break caught: a diagnosis that is computed but never shown. Each of these
  # is a place a user reads after a run that produced no outputs.
  root <- withr::local_tempdir()
  writeLines("data work.one; value=1; run;", file.path(root, "one.sas"))
  state <- new_migration_state(sas_project(root), withr::local_tempdir())
  state$usage_budget <- list(records = list(
    cut_request("r1", 180.6), cut_request("r2", 181.3), completed_request("r3", 20)
  ))

  write_migration_report(state)

  report <- jsonlite::read_json(state$paths$report_json)
  expect_identical(report$usage$request_cuts$requests, 2L)
  for (path in c(state$paths$report_md, state$paths$start_here)) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(text, "Requests cut short: 2 model requests", fixed = TRUE, info = path)
    expect_match(text, "180.6 to 181.3 seconds", fixed = TRUE, info = path)
  }
  printed <- paste(utils::capture.output(print(structure(
    list(status = "blocked", usage = state$usage_budget), class = "sas2r_translation"
  ))), collapse = "\n")
  expect_match(printed, "Requests cut short: 2 model requests", fixed = TRUE)
})

test_that("a run without a cut leaves the report free of the cut note", {
  root <- withr::local_tempdir()
  writeLines("data work.one; value=1; run;", file.path(root, "one.sas"))
  state <- new_migration_state(sas_project(root), withr::local_tempdir())
  state$usage_budget <- list(records = list(completed_request("r1", 20)))

  write_migration_report(state)

  for (path in c(state$paths$report_md, state$paths$start_here)) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_no_match(text, "Requests cut short", fixed = TRUE)
  }
})
