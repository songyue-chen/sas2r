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

# ---- event lines: who is working, on what ---------------------------------

test_that("an agent event names the agent, its target, and how it ended", {
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))
  ctx <- list(purpose = "program_review", component_id = "demo",
              revision_id = "r1", round = 1L)
  withCallingHandlers({
    signal_agent_event("agent_started", "reviewer", ctx)
    signal_agent_event("agent_finished", "reviewer", ctx, status = "ok", tool_calls = 3L)
    signal_agent_event("agent_started", "translator", list(purpose = "translation", unit_id = 12L, round = 0L))
    signal_agent_event("agent_finished", "translator", list(purpose = "translation", unit_id = 12L),
                       status = "tool_calling_unavailable", tool_calls = 0L)
    signal_agent_event("agent_started", "fixer",
                       list(purpose = "program_fix", mode = "bundle", component_id = "demo",
                            revision_id = "r2", round = 2L))
  }, sas2r_progress = handler)

  expect_identical(lines, c(
    "  reviewer  demo (r1, round 1): reviewing",
    "  reviewer  demo (r1, round 1): ok, 3 tool calls",
    "  translator  unit 12: translating",
    "  translator  unit 12: tool_calling_unavailable",
    "  fixer  demo (r2, round 2): repairing the bundle"
  ))
})

test_that("coordinator events say what happened instead of counting to 1/1", {
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))
  withCallingHandlers({
    signal_immediate_coordinator_event("program_generated", "demo", "r1")
    signal_immediate_coordinator_event("mechanical_pass", "demo", "r1")
    signal_immediate_coordinator_event("program_reviewed", "demo", "r1")
    signal_immediate_coordinator_event("agent_degraded", "demo", "r1",
                                       reason = "tool_calling_unavailable")
    signal_immediate_coordinator_event("program_fixed", "demo", "r2")
    signal_immediate_coordinator_event("component_revisited", "demo")
  }, sas2r_progress = handler)

  expect_identical(lines, c(
    "  coordinator  demo (r1): program generated",
    "  coordinator  demo (r1): mechanical checks passed",
    "  coordinator  demo (r1): reviewed",
    "  coordinator  demo (r1): agent degraded -- tool_calling_unavailable",
    "  coordinator  demo (r2): repaired",
    "  coordinator  demo: revisited"
  ))
  expect_false(any(grepl("1/1", lines, fixed = TRUE)))
})

test_that("smoke and bundle events carry the attempt, the outcome, and a one-line reason", {
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))
  long_reason <- paste0("Error in lib_read(): Dataset not found: work.stg1\n",
                        "Calls: <Anonymous> ... more lines of traceback")
  withCallingHandlers({
    signal_program_smoke_event("program_smoke_started", "demo", attempt_id = "smoke_attempt_001")
    signal_program_smoke_event("program_smoke_failed", "demo", attempt_id = "smoke_attempt_001",
                               reason = long_reason)
    signal_bundle_event("bundle_round_started", attempt_id = "bundle_attempt_001",
                        round = 0L, status = "blocked")
    signal_bundle_event("bundle_attempt_started", attempt_id = "bundle_attempt_001", round = 0L)
    signal_bundle_event("bundle_attempt_completed", attempt_id = "bundle_attempt_001",
                        round = 0L, passed = FALSE)
    signal_bundle_event("bundle_gate_evaluated", attempt_id = "bundle_attempt_001",
                        round = 0L, status = "blocked")
    signal_bundle_event("bundle_fixer_invoked", attempt_id = "bundle_attempt_001",
                        round = 1L, component_id = "demo")
    signal_bundle_event("bundle_early_stop", attempt_id = "bundle_attempt_001",
                        round = 1L, reason = "no_fixer_llm")
  }, sas2r_progress = handler)

  expect_identical(lines, c(
    "  smoke  demo: started [smoke_attempt_001]",
    "  smoke  demo: failed [smoke_attempt_001] -- Error in lib_read(): Dataset not found: work.stg1",
    "  bundle  first pass started",
    "  bundle  first pass: running bundle_attempt_001",
    "  bundle  first pass: bundle_attempt_001 failed",
    "  bundle  first pass: bundle_attempt_001 assessed -- blocked",
    "  bundle  repair round 1: fixer invoked for demo",
    "  bundle  repair round 1: stopping early -- no_fixer_llm"
  ))
})

test_that("an identical consecutive event line is drawn once", {
  lines <- character()
  handler <- sas2r_progress_cli_handler(emit = function(line) lines <<- c(lines, line))
  withCallingHandlers({
    signal_immediate_coordinator_event("program_generated", "demo", "r1")
    signal_immediate_coordinator_event("program_generated", "demo", "r1")
    signal_immediate_coordinator_event("mechanical_pass", "demo", "r1")
  }, sas2r_progress = handler)
  expect_length(lines, 2L)
})

test_that("a very long reason is folded to its first line and cut", {
  reason <- paste(rep("x", 200), collapse = "")
  folded <- progress_reason(reason)
  expect_lte(nchar(folded), 4L + 100L)
  expect_match(folded, "\\.\\.\\.$")
  expect_identical(progress_reason(NULL), "")
  expect_identical(progress_reason("  first line  \nsecond"), " -- first line")
})
