test_that("signalling progress with no handler is a silent no-op", {
  # The emission points sit inside the agent loop, so an unhandled signal must
  # cost nothing and must never reach the console on its own.
  expect_silent(signal_sas2r_progress("translate", 1L, 10L))
  expect_null(signal_sas2r_progress("translate", 1L, 10L))
})

test_that("a calling handler receives the progress fields", {
  seen <- list()

  withCallingHandlers(
    {
      signal_sas2r_progress("translate", 3L, 10L, unit_id = 42L, status = "ok")
      signal_sas2r_progress("review", 1L, 2L, unit_id = 7L, status = "failed")
    },
    sas2r_progress = function(p) seen[[length(seen) + 1L]] <<- p
  )

  expect_length(seen, 2L)
  expect_identical(seen[[1]]$phase, "translate")
  expect_identical(seen[[1]]$index, 3L)
  expect_identical(seen[[1]]$total, 10L)
  expect_identical(seen[[1]]$unit_id, 42L)
  expect_identical(seen[[1]]$status, "ok")
  expect_identical(seen[[2]]$phase, "review")
  expect_identical(seen[[2]]$status, "failed")
})

test_that("progress conditions do not interrupt the computation", {
  result <- withCallingHandlers(
    {
      total <- 0
      for (i in 1:5) {
        signal_sas2r_progress("translate", i, 5L)
        total <- total + i
      }
      total
    },
    sas2r_progress = function(p) invisible(NULL)
  )

  expect_identical(result, 15)
})

test_that("a handler that errors does not escape as a progress failure", {
  # A broken renderer must not take the run down with it: the agent loop is the
  # payload, progress is decoration.
  expect_identical(
    with_sas2r_progress(
      {
        signal_sas2r_progress("translate", 1L, 1L)
        "finished"
      },
      handler = function(p) stop("renderer exploded")
    ),
    "finished"
  )
})

test_that("with_sas2r_progress returns the value of its expression", {
  expect_identical(with_sas2r_progress(41L + 1L, handler = function(p) NULL), 42L)
})

test_that("with_sas2r_progress renders nothing when progress is disabled", {
  withr::local_options(sas2r.progress = FALSE)

  expect_silent(
    with_sas2r_progress(signal_sas2r_progress("translate", 1L, 3L))
  )
})

test_that("the default renderer is driven by the sas2r.progress option", {
  withr::local_options(sas2r.progress = FALSE)
  expect_false(sas2r_progress_enabled())

  withr::local_options(sas2r.progress = TRUE)
  expect_true(sas2r_progress_enabled())
})

test_that("a progress tally counts outcomes as they are signalled", {
  tally <- new_sas2r_progress_tally()

  tally$record(list(phase = "translate", status = "ok"))
  tally$record(list(phase = "translate", status = "ok"))
  tally$record(list(phase = "translate", status = "failed"))
  tally$record(list(phase = "translate", status = "skipped"))

  expect_identical(tally$counts()[["ok"]], 2L)
  expect_identical(tally$counts()[["failed"]], 1L)
  expect_identical(tally$counts()[["skipped"]], 1L)
  expect_identical(tally$format(), "ok 2 | failed 1 | skipped 1")
})

test_that("a tally reports nothing before anything is recorded", {
  expect_identical(new_sas2r_progress_tally()$format(), "")
})

test_that("the tally resets when the phase changes", {
  # translate's outcome counts must not leak into review's line: a reviewer
  # that has judged one unit should not report the translator's four.
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))

  handler(list(phase = "translate", index = 1L, total = 2L, status = "ok"))
  handler(list(phase = "translate", index = 2L, total = 2L, status = NA_character_))
  handler(list(phase = "translate", index = 2L, total = 2L, status = "ok"))
  handler(list(phase = "review", index = 1L, total = 2L, status = "ok"))
  handler(list(phase = "review", index = 2L, total = 2L, status = NA_character_))

  expect_match(lines[[1]], "translate 2/2\\s+ok 1")
  # review starts its own tally rather than inheriting translate's two.
  expect_match(lines[[2]], "review 2/2\\s+ok 1")
})

test_that("a line is drawn when a unit starts, so a slow unit is still visible", {
  # Drawing only on outcomes meant a unit taking minutes produced minutes of
  # silence -- exactly when the caller most needs to know the run is alive.
  # The entry line carries the tally accumulated so far, so one line per unit
  # still covers both liveness and results.
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))

  handler(list(phase = "translate", index = 1L, total = 3L, status = NA_character_))
  handler(list(phase = "translate", index = 1L, total = 3L, status = "ok"))
  handler(list(phase = "translate", index = 2L, total = 3L, status = NA_character_))

  expect_length(lines, 2L)
  expect_match(lines[[1]], "translate 1/3")
  # The second unit's entry reports the first unit's result.
  expect_match(lines[[2]], "translate 2/3\\s+ok 1")
})

test_that("an outcome updates the tally without drawing its own line", {
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))

  handler(list(phase = "translate", index = 1L, total = 2L, status = "failed"))
  handler(list(phase = "translate", index = 2L, total = 2L, status = NA_character_))

  expect_length(lines, 1L)
  expect_match(lines[[1]], "translate 2/2\\s+failed 1")
})

test_that("progress goes to stderr so it appears while the run is still going", {
  # cli::cat_line() writes to stdout, which is block-buffered when redirected:
  # a long run showed nothing until it exited, which is precisely when progress
  # stops being useful. Status belongs on stderr by convention anyway, leaving
  # stdout clean for whatever a caller pipes.
  withr::local_options(sas2r.progress = TRUE)

  on_stderr <- capture.output(
    with_sas2r_progress(signal_sas2r_progress("translate", 1L, 3L)),
    type = "message"
  )
  on_stdout <- capture.output(
    with_sas2r_progress(signal_sas2r_progress("translate", 1L, 3L)),
    type = "output"
  )

  expect_match(paste(on_stderr, collapse = " "), "translate 1/3")
  expect_identical(on_stdout, character())
})

test_that("progress defaults on everywhere except under testthat", {
  # A user should never need options(sas2r.progress = TRUE): interactive
  # sessions and Rscript both show progress by default. Tests and R CMD check
  # stay silent because testthat marks itself in the environment, and the
  # option still overrides in both directions.
  withr::local_options(sas2r.progress = NULL)

  withr::local_envvar(TESTTHAT = NA)
  expect_true(sas2r_progress_enabled())

  withr::local_envvar(TESTTHAT = "true")
  expect_false(sas2r_progress_enabled())

  withr::local_options(sas2r.progress = TRUE)
  expect_true(sas2r_progress_enabled())
})
