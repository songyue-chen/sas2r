# What SAS would write to its log as a note, kept for the report.
#
# A SAS DATA step that reads a variable no input supplies does not stop: it
# reads the variable as missing and logs "Variable X is uninitialized". The
# generated R does the same through sas_uninitialized() (R/runtime-data.R),
# which records each case where the program runs. This file carries those
# records into the run report and holds the one check on how the helper is
# called. Nothing here reads a dataset or changes a run's status.

SAS_NAME_PATTERN <- "^[A-Za-z_][A-Za-z0-9_]{0,31}$"

#' The notes generated programs recorded, one entry per program and variable
#'
#' The records come from the process that ran generated code, so only a plain
#' SAS name, a known type and a row count are kept.
#'
#' @param notes A list, component id to the entries sas_uninitialized() wrote.
#' @return The same shape, one entry per variable with `times`, the number of
#'   places that read it; components without a valid entry are left out.
#' @noRd
run_log_notes <- function(notes) {
  out <- list()
  if (!is.list(notes)) return(out)
  # One value as text; "" when it is absent.
  text <- function(x) {
    x <- as.character(unlist(x, use.names = FALSE))
    if (length(x) && !is.na(x[1L])) x[1L] else ""
  }
  count <- function(x) suppressWarnings(as.integer(text(x)))
  # A saved record read back with simplification holds a data frame per program.
  rows_of <- function(x) if (is.data.frame(x)) lapply(seq_len(nrow(x)), function(i) lapply(as.list(x), `[[`, i)) else
    if (is.list(x)) x else list()
  for (cid in names(notes)) {
    entries <- Filter(function(x) is.list(x) && identical(text(x$note), "uninitialized_variable") &&
      grepl(SAS_NAME_PATTERN, text(x$variable)), rows_of(notes[[cid]]))
    if (!nzchar(cid) || !length(entries)) next
    key <- tolower(vapply(entries, function(x) text(x$variable), ""))
    out[[cid]] <- lapply(unique(key), function(name) {
      same <- entries[key == name]
      rows <- vapply(same, function(x) count(x$rows), 0L)
      times <- vapply(same, function(x) count(x$times), 0L)
      list(note = "uninitialized_variable", variable = text(same[[1L]]$variable),
        type = if (identical(text(same[[1L]]$type), "character")) "character" else "numeric",
        rows = if (all(is.na(rows))) NA_integer_ else max(rows, na.rm = TRUE),
        times = sum(ifelse(is.na(times), 1L, times)))
    })
  }
  out
}

#' The notes a report shows
#'
#' Those of the selected bundle attempt, or of the last attempt that ran when
#' none is selected. With no bundle attempt at all, each program's own check
#' run is used. Each entry says whether the SAS source names the variable.
#' @noRd
report_log_notes <- function(state, attempts = NULL) {
  bundles <- Filter(function(x) identical(x$kind, "bundle") && !isTRUE(x$deferred),
    attempts$completed_attempts %||% list())
  notes <- if (!is.null(state$selected_attempt)) state$selected_attempt$log_notes else
    if (length(bundles)) utils::tail(bundles, 1L)[[1L]]$log_notes else {
      smoke <- lapply(state$selected_revisions %||% list(), function(rev) rev$smoke$log_notes)
      own <- lapply(names(smoke), function(cid) smoke[[cid]][[cid]])
      stats::setNames(own, names(smoke))
    }
  notes <- run_log_notes(notes)
  known <- source_identifiers(state$project)
  lapply(notes, function(entries) lapply(entries, function(entry) {
    entry$named_in_source <- if (is.null(known)) NA else tolower(entry$variable) %in% known
    entry
  }))
}

#' One sentence per note, for the Markdown and the HTML report
#' @noRd
log_note_sentences <- function(notes) {
  unlist(lapply(names(notes), function(cid) vapply(notes[[cid]], function(entry) {
    rows <- suppressWarnings(as.integer(entry$rows %||% NA_integer_))
    paste0(cid, ": Variable ", entry$variable, " is uninitialized. No input supplied it, so the program read it as missing",
      if (length(rows) == 1L && !is.na(rows)) sprintf(" in %d row%s", rows, if (rows == 1L) "" else "s"), ".",
      if (identical(entry$named_in_source, FALSE))
        " The SAS source never names this variable: check the generated program.")
  }, "")), use.names = FALSE)
}

LOG_NOTES_INTRODUCTION <- paste(
  "A SAS DATA step that reads a variable no input supplies does not stop.",
  "It reads the variable as missing and writes a note to its log.",
  "The generated programs do the same; SAS would log each note below.",
  "Check whether the input was expected to carry the variable.")

log_note_report_lines <- function(notes) {
  sentences <- log_note_sentences(notes)
  if (!length(sentences)) return(character())
  c("## SAS log notes", "", LOG_NOTES_INTRODUCTION, "", paste0("- ", sentences), "")
}

#' Every name the project's SAS code uses, in lower case
#' @return A character vector, or `NULL` when the source is not available.
#' @noRd
source_identifiers <- function(project) {
  statements <- project$statements
  if (!is.data.frame(statements) || !nrow(statements)) return(NULL)
  text <- statements$text[statements$type == "code"]
  unique(tolower(unlist(regmatches(text, gregexpr("[A-Za-z_][A-Za-z0-9_]*", text, perl = TRUE)), use.names = FALSE)))
}

#' Names written in sas_uninitialized() that the SAS source never uses
#'
#' The helper stands for a variable a SAS step reads, so a name written out
#' in the call must be one the project's SAS code uses. A name computed at
#' run time, as inside a translated macro, cannot be read here; the report
#' marks it when the run recorded it.
#'
#' @param code Generated R code.
#' @param project The scanned project.
#' @return Messages, one per unknown name; empty when all are known.
#' @noRd
check_uninitialized_names <- function(code, project) {
  known <- source_identifiers(project)
  if (is.null(known)) return(character())
  code <- paste(code %||% "", collapse = "\n")
  exprs <- if (grepl("sas_uninitialized", code, fixed = TRUE))
    tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character())
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  written <- function(value) {
    if (string(value)) return(value)
    if (is.call(value) && identical(value[[1L]], quote(c))) {
      entries <- as.list(value)[-1L]
      if (length(entries) && all(vapply(entries, string, logical(1)))) return(vapply(entries, identity, ""))
    }
    NULL
  }
  seen <- new.env(parent = emptyenv())
  seen$names <- character()
  visit <- function(e) {
    if (is.call(e)) {
      head <- e[[1L]]
      name <- if (is.name(head)) as.character(head) else if (is.call(head) && length(head) == 3L &&
        is.name(head[[3L]])) as.character(head[[3L]]) else ""
      if (identical(name, "sas_uninitialized")) {
        arguments <- as.list(e)[-1L]
        labels <- names(arguments) %||% rep("", length(arguments))
        positional <- arguments[!nzchar(labels)]
        # Piped, the frame is not among the arguments and the names come first.
        value <- if ("vars" %in% labels) arguments[[match("vars", labels)]] else
          if (length(positional) >= 2L) positional[[2L]] else if (length(positional) == 1L) positional[[1L]]
        seen$names <- c(seen$names, written(value))
      }
    }
    if (is.call(e) || is.expression(e) || is.pairlist(e)) for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) visit(e[[i]])
    }
  }
  visit(exprs)
  unknown <- unique(seen$names[!tolower(seen$names) %in% known])
  vapply(unknown, function(name) paste0(
    "unbacked_variable_name: sas_uninitialized() names ", name,
    ", which the SAS source never uses. Use it only for a variable a SAS step reads"), "", USE.NAMES = FALSE)
}
