#' Signal one unit of agent progress
#'
#' The agent loop runs for minutes to hours against a metered API, and until
#' now it printed nothing between start and finish -- a working run and a wedged
#' one looked identical from the console. Emission uses the condition system
#' rather than a callback threaded through the call stack, because the loops
#' sit several frames below the public entry points and a condition costs
#' nothing when nobody is listening.
#'
#' Consumers subscribe with `withCallingHandlers(expr, sas2r_progress = ...)`;
#' the console renderer in [with_sas2r_progress()] is one such subscriber, and a
#' Shiny front end can be another without the package knowing about it.
#' @noRd
signal_sas2r_progress <- function(phase, index, total, unit_id = NA_integer_,
                                  status = NA_character_) {
  condition <- structure(
    class = c("sas2r_progress", "condition"),
    list(
      message = "", call = NULL,
      phase = phase,
      index = as.integer(index),
      total = as.integer(total),
      unit_id = as.integer(unit_id),
      status = as.character(status)
    )
  )
  signalCondition(condition)
  invisible(NULL)
}

#' Signal a structured program smoke progress event
#'
#' Emits events with specific event types: `program_smoke_started`,
#' `program_smoke_passed`, `program_smoke_failed`, and `program_smoke_deferred`
#' carrying component IDs, attempt IDs, execution IDs, and relevant log/artifact paths.
#'
#' @param event Event type string.
#' @param component_id Component identifier.
#' @param attempt_id Optional attempt identifier.
#' @param execution_id Optional execution identifier.
#' @param path Optional file path to logs or attempt directory.
#' @param reason Optional explanation or deferred reason.
#' @param ... Additional metadata.
#' @noRd
signal_program_smoke_event <- function(
  event,
  component_id,
  attempt_id = NULL,
  execution_id = NULL,
  path = NULL,
  reason = NULL,
  ...
) {
  status_val <- sub("^program_smoke_", "", as.character(event))
  condition <- structure(
    class = c(as.character(event), "sas2r_program_smoke_event", "sas2r_progress", "condition"),
    list(
      message = "", call = NULL,
      event = as.character(event),
      phase = "smoke",
      component_id = as.character(component_id),
      attempt_id = attempt_id,
      execution_id = execution_id,
      path = path,
      reason = reason,
      status = status_val,
      ...
    )
  )
  signalCondition(condition)
  invisible(NULL)
}

#' Signal a structured coordinator lifecycle event
#'
#' Emits coordinator events such as `program_generated`, `mechanical_pass`, `program_reviewed`,
#' `program_fixed`, and `component_revisited`.
#'
#' @param event Event type string.
#' @param component_id Component identifier.
#' @param revision_id Revision identifier.
#' @param ... Additional metadata.
#' @noRd
signal_immediate_coordinator_event <- function(
  event,
  component_id,
  revision_id = NULL,
  ...
) {
  condition <- structure(
    class = c(as.character(event), "sas2r_coordinator_event", "sas2r_progress", "condition"),
    list(
      message = "", call = NULL,
      event = as.character(event),
      phase = "coordinator",
      component_id = as.character(component_id),
      revision_id = revision_id,
      ...
    )
  )
  signalCondition(condition)
  invisible(NULL)
}

#' Signal a structured bundle execution or repair event
#'
#' Emits bundle events such as `bundle_round_started`, `bundle_attempt_started`,
#' `bundle_attempt_completed`, `bundle_gate_evaluated`, `bundle_attempt_selected`,
#' `bundle_fixer_invoked`, `bundle_fixer_completed`, and `bundle_early_stop`.
#'
#' @param event Event type string.
#' @param attempt_id Optional attempt identifier.
#' @param round Optional integer repair round.
#' @param status Optional bundle status.
#' @param path Optional file path to logs or attempt directory.
#' @param reason Optional explanation.
#' @param cost Optional numeric cost or spend.
#' @param ... Additional metadata.
#' @noRd
signal_bundle_event <- function(
  event,
  attempt_id = NULL,
  round = NULL,
  status = NULL,
  path = NULL,
  reason = NULL,
  cost = NULL,
  ...
) {
  condition <- structure(
    class = c(as.character(event), "sas2r_bundle_event", "sas2r_progress", "condition"),
    list(
      message = "", call = NULL,
      event = as.character(event),
      phase = "bundle",
      attempt_id = attempt_id,
      round = if (!is.null(round)) as.integer(round) else NULL,
      status = if (!is.null(status)) as.character(status) else NULL,
      path = path,
      reason = reason,
      cost = cost,
      ...
    )
  )
  signalCondition(condition)
  invisible(NULL)
}

#' Whether the console renderer should draw
#'
#' On by default everywhere -- interactive sessions and Rscript alike -- so a
#' long migration never runs silently without the user asking for it. Tests
#' and `R CMD check` stay quiet because testthat marks itself in the
#' environment. `options(sas2r.progress = ...)` still overrides in both
#' directions.
#' @noRd
sas2r_progress_enabled <- function() {
  default <- !identical(Sys.getenv("TESTTHAT"), "true")
  isTRUE(getOption("sas2r.progress", default))
}

#' Running tally of unit outcomes for the progress line
#' @noRd
new_sas2r_progress_tally <- function() {
  counts <- integer()
  list(
    record = function(progress) {
      status <- progress$status
      if (!length(status) || is.na(status) || !nzchar(status)) return(invisible(NULL))
      # `[[` on an atomic vector aborts for an absent name rather than
      # returning NULL, so the first sighting of a status must be handled.
      previous <- if (status %in% names(counts)) counts[[status]] else 0L
      counts[[status]] <<- previous + 1L
      invisible(NULL)
    },
    counts = function() counts,
    format = function() {
      if (!length(counts)) return("")
      # Keep the common outcomes in a stable order so the line does not
      # reshuffle as a run progresses.
      preferred <- c("ok", "passed", "failed", "deferred", "skipped")
      ordered <- c(intersect(preferred, names(counts)),
                   setdiff(names(counts), preferred))
      paste(vapply(ordered, function(name) paste(name, counts[[name]]),
                   character(1)), collapse = " | ")
    }
  )
}

#' Run `expr` with a progress renderer attached
#'
#' A handler that fails is contained: progress is decoration, and a broken
#' renderer must not take down a run that is otherwise succeeding.
#' @noRd
with_sas2r_progress <- function(expr, handler = NULL) {
  if (is.null(handler)) {
    if (!sas2r_progress_enabled()) return(expr)
    handler <- sas2r_progress_cli_handler()
  }
  withCallingHandlers(
    expr,
    sas2r_progress = function(progress) {
      tryCatch(handler(progress), error = function(error) invisible(NULL))
    }
  )
}

#' Console renderer: one line per completed unit, tallied within a phase
#'
#' Position signals advance nothing on screen -- drawing both the entry and the
#' outcome of every unit would double the output of a 300-unit run for no added
#' information. Each phase keeps its own tally, so the reviewer's line reports
#' what the reviewer has judged rather than inheriting the translator's counts.
#' @noRd
sas2r_progress_cli_handler <- function(emit = NULL) {
  # stderr, not stdout: stdout is block-buffered when redirected to a file or a
  # pipe, so a long run emits nothing until it exits. stderr is unbuffered and
  # is where status belongs, leaving stdout clean for piped output.
  emit <- emit %||% function(line) {
    cli::cat_line(line, file = stderr())
    flush(stderr())
  }
  tally <- new_sas2r_progress_tally()
  phase <- NULL
  function(progress) {
    if (!identical(progress$phase, phase)) {
      phase <<- progress$phase
      tally <<- new_sas2r_progress_tally()
    }
    # Draw when a unit starts, not when it finishes: a unit that takes minutes
    # would otherwise produce minutes of silence, and liveness is most of what
    # a progress line is for. The outcome only updates the tally, which the
    # next entry line reports.
    status <- progress$status
    if (length(status) && !is.na(status) && nzchar(status)) {
      tally$record(progress)
      return(invisible(NULL))
    }
    detail <- tally$format()
    emit(sprintf(
      "  %s %d/%d%s", progress$phase, progress$index %||% 1L, progress$total %||% 1L,
      if (nzchar(detail)) paste0("  ", detail) else ""
    ))
    invisible(NULL)
  }
}
