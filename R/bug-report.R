#' Write an issue draft for a possible sas2r bug (beta)
#'
#' Enable `migration.bug_reports: true` before running [sas_translate()].
#' The draft is shown on the run's `START_HERE.html` page. This function saves
#' the same draft as a Markdown file. Use the returned run or its saved machine
#' report, including a blocked run's report if translation raised an error.
#' Read the draft and decide what to share before you send it to the
#' maintainer or post it to the public issue tracker.
#'
#' @param run A `sas2r_translation` object or the path to its `report.json`.
#' @param file Explicit path for a new Markdown file. An existing file is
#'   never replaced.
#' @return The destination path, invisibly.
#' @details
#' The draft has two kinds of content, each under its own heading.
#'
#' What sas2r recorded during the run: the package and R versions, the
#' recorded symptom and, for a finding about an output, the output's name, the
#' failed check, the comparison keys you configured and the keys rows were
#' paired on, the pairing method, and the row counts of the reference and the
#' generated dataset. "Records paired" counts the rows matched between the
#' two. "Row matching ran to its end" says whether the matching step finished
#' its work; it can finish and still leave rows unpaired. The output is found
#' by the name saved with the finding. When a finding was saved without that
#' name, or the saved assessment belongs to another bundle attempt, the draft
#' says that the comparison details could not be linked.
#'
#' What the AI suggested: the package step it suspects, its suggested cause,
#' the uncertainty, a next step and, when it drafted one, a synthetic R
#' example. None of this is verified. Read the example before you run it in a
#' fresh R session using the reported build, and record the result in the
#' draft. A reproducible failure needs an observed result from that separate
#' test.
#'
#' AI triage occurs during translation using the configured reviewer. This
#' function renders the saved report locally. It neither calls the model nor
#' runs examples, and it sends nothing anywhere. The draft holds your
#' project's dataset and variable names, and the model's text and code may
#' reflect the project evidence it was given. Read it and decide what to
#' include.
#' @seealso [sas_translate()], [sas_config()]
#' @export
sas_bug_report <- function(run, file) {
  if (inherits(run, "sas2r_translation")) run <- run$report_json_path
  if (!is_scalar_character(run)) {
    cli::cli_abort("{.arg run} must be a translation result or a path to its report.json",
      class = "sas2r_invalid_argument")
  }
  if (missing(file) || !is_scalar_character(file) || !endsWith(tolower(file), ".md")) {
    cli::cli_abort("{.arg file} must be an explicit destination ending in .md",
      class = "sas2r_invalid_argument")
  }
  if (file.exists(file)) {
    cli::cli_abort("Choose a new destination. {.file {file}} already exists.",
      class = "sas2r_record_exists")
  }

  report <- read_json_record(run, simplifyVector = FALSE)
  diagnosis <- report$package_bug_diagnosis
  if (is.null(diagnosis) || identical(diagnosis$status, "disabled")) {
    cli::cli_abort("This run has no beta bug diagnosis. Set migration.bug_reports: true and rerun.",
      class = "sas2r_bug_diagnosis_disabled")
  }
  if (!isTRUE(diagnosis$schema_version == 2L)) {
    cli::cli_abort("This report predates the current AI beta. Rerun translation to create a draft.",
      class = "sas2r_bug_diagnosis_unavailable")
  }
  if (diagnosis$status %in% c("completed", "not_needed") && !length(diagnosis$findings)) {
    cli::cli_abort("{diagnosis$reason}", class = "sas2r_no_package_bug_finding")
  }
  if (!identical(diagnosis$status, "completed")) {
    cli::cli_abort("{diagnosis$reason}", class = "sas2r_bug_diagnosis_unavailable")
  }

  lines <- c("# Possible sas2r bug", "", bug_report_markdown(bug_report_content(report)))
  atomic_write_file(function(path) writeLines(lines, path, useBytes = TRUE), file,
    require_atomic = TRUE)
  invisible(file)
}

BUG_REPORT_NOTICE <- paste("This draft was written on your computer and has not been sent anywhere.",
  "Read it and decide what to share.")
BUG_REPORT_SUBMIT <- paste("To report this, post what you decide to share at",
  "https://github.com/songyue-chen/sas2r/issues/new or send it to the maintainer.")
BUG_REPORT_COUNT_LABELS <- paste("\"Records paired\" counts the rows of the reference and of the",
  "generated dataset that were matched to each other. \"Row matching ran to its end\" says",
  "whether the matching step finished its work. It can finish and still leave rows unpaired.")

# The draft is built once, as a list of blocks, from a saved report, and
# rendered twice: as Markdown by sas_bug_report() and as HTML on the start-here
# page. Building it reads the report only. It opens no dataset, sends no model
# request and runs no example. A block is a heading, a paragraph, a labelled
# sentence, a list or R code. A list item is a sequence of segments, each plain
# words or a name shown as code.
draft_heading <- function(level, text) list(type = "heading", level = level, text = text)
draft_text <- function(text, strong = FALSE) list(type = "text", text = text, strong = strong)
draft_statement <- function(label, text) {
  if (is_scalar_character(text)) list(type = "statement", label = label, text = text)
}
draft_list <- function(...) {
  items <- Filter(Negate(is.null), list(...))
  if (length(items)) list(type = "list", items = items)
}
draft_names <- function(names) {
  segments <- list()
  for (i in seq_along(names)) {
    segments <- c(segments, if (i > 1L) list(list(text = ", ")), list(list(code = names[[i]])))
  }
  segments
}
draft_item <- function(label, ...) {
  segments <- list()
  for (part in list(...)) {
    segments <- c(segments, if (is.character(part)) list(list(text = part)) else part)
  }
  c(list(list(text = paste0(label, ":", if (length(segments)) " "))), segments)
}
# Names from the report are shown as code only when each is one short line.
draft_plain_names <- function(x) {
  is.character(x) && length(x) > 0L && !anyNA(x) && all(nzchar(x)) &&
    all(nchar(x) <= 200L) && !any(grepl("[[:cntrl:]]", x))
}
draft_checked_item <- function(label, value, pattern) {
  if (is.numeric(value)) value <- as.character(value)
  if (!is_scalar_character(value) || !grepl(pattern, value) || grepl("[\r\n]", value)) return(NULL)
  draft_item(label, draft_names(value))
}

bug_report_content <- function(report) {
  diagnosis <- report$package_bug_diagnosis
  findings <- diagnosis$findings
  env <- diagnosis$environment
  example <- any(vapply(findings, function(finding) is_scalar_character(finding$repro_r), logical(1)))
  described <- unlist(lapply(seq_along(findings), function(i)
    bug_report_finding_blocks(report, findings[[i]], i)), recursive = FALSE)
  measured <- any(vapply(described, function(block) identical(block$text, BUG_REPORT_COUNT_LABELS), logical(1)))
  # The reader is told what the findings hold before reading them.
  holds <- c("your project's dataset and variable names",
    if (measured) "what the comparison with the reference measured",
    "the AI's suggested cause", if (example) "its proposed example")
  Filter(Negate(is.null), c(list(
    draft_text(BUG_REPORT_NOTICE),
    draft_heading(1L, "Run environment"),
    draft_list(
      draft_checked_item("sas2r", env$sas2r, "^[0-9]+([.-][0-9]+)+$"),
      draft_checked_item("sas2r commit", env$revision, "^[a-f0-9]{40}$"),
      draft_checked_item("R", env$R, "^[0-9]+([.-][0-9]+)+$"),
      draft_checked_item("Platform", env$platform, "^[A-Za-z0-9_]+(-[A-Za-z0-9_.]+)+$"),
      draft_checked_item("Translation workers", env$workers, "^[0-9]+$"),
      draft_checked_item("Run ID", report$run_id, "^run_[0-9]{8}T[0-9]{6}Z_[a-f0-9]{8}$")),
    draft_text(paste0("The part below holds ", paste(utils::head(holds, -1L), collapse = ", "), " and ",
      utils::tail(holds, 1L), ". Read it and decide what to include."))),
    described, list(draft_text(BUG_REPORT_SUBMIT))))
}

bug_report_finding_blocks <- function(report, finding, position) {
  binding <- finding$binding
  code <- finding$repro_r
  id <- finding$evidence_id
  step <- package_bug_step_label(finding$responsible_step)
  c(list(
    draft_heading(1L, paste0("Finding ", position,
      if (is_scalar_character(id) && grepl("^E[0-9]+$", id)) paste0(" [", id, "]"))),
    draft_heading(2L, "Recorded by sas2r"),
    draft_statement("Recorded symptom", finding$observed)),
    bug_report_recorded_blocks(report, binding),
    list(draft_list(
      draft_checked_item("Bundle attempt", binding$attempt_id, "^bundle_attempt_[0-9]+$"),
      draft_checked_item("Code revision", binding$revision_id, "^(r[0-9]+|rev_[a-f0-9]+(_[0-9]+)?)$"),
      draft_checked_item("Bundled runtime SHA-256", binding$helper_hash, "^[a-f0-9]{64}$")),
    draft_heading(2L, "Suggested by the AI (not verified)"),
    draft_statement("Suspected package step", step),
    draft_statement("Suggested cause", finding$explanation),
    draft_statement("Expected behavior", finding$expected),
    draft_statement("Uncertainty", finding$uncertainty),
    draft_statement("Next step", finding$next_step),
    draft_text(package_bug_reproduction_label(finding), strong = TRUE)),
    if (is_scalar_character(code)) list(
      draft_heading(3L, "Proposed synthetic reproduction"),
      draft_text(paste("Read the draft code before you run it. To test it, use the reported",
        "package build or the matching bundled runtime.")),
      list(type = "code", text = code)),
    list(draft_heading(2L, "Result on your machine"),
    draft_list(
      draft_item("Outcome", "unverified (update to reproduced, not reproduced, or inconclusive after testing)."),
      draft_item("Build and runtime tested"),
      draft_item("Observed result"))))
}

# What the run recorded about the thing a finding refers to. An output is
# found by the target saved with the finding, never by the position its
# evidence ID suggests, and its comparison is shown only when the saved
# assessment and the finding name the same bundle attempt.
bug_report_recorded_blocks <- function(report, binding) {
  scope <- binding$scope
  target <- binding$target
  if (!is_scalar_character(target)) {
    if (is_scalar_character(scope) && !identical(scope, "Output assessment")) {
      return(list(draft_list(
        if (draft_plain_names(scope)) draft_item("Failure stage", scope),
        if (is_scalar_character(binding$component) && draft_plain_names(binding$component))
          draft_item("Program", draft_names(binding$component)))))
    }
    return(list(draft_text(paste("Comparison details could not be linked to this finding.",
      "It was saved without the name of the output or the failure stage it refers to."))))
  }
  output <- if (draft_plain_names(target)) draft_item("Output", draft_names(target))
  unlinked <- function(reason) list(draft_list(output), draft_text(paste(
    "The comparison details of this output could not be linked to this finding:", reason)))
  assessment <- report$output_assessments[[target]]
  if (!is.list(assessment)) return(unlinked("the saved report holds no assessment for this output."))
  attempt <- binding$attempt_id
  selected <- report$selected_attempt_id
  if (!is_scalar_character(attempt) || !is_scalar_character(selected)) {
    return(unlinked("the bundle attempt of the finding or of the saved assessment was not recorded."))
  }
  if (!identical(attempt, selected)) {
    return(unlinked("the finding and the saved assessment belong to different bundle attempts."))
  }
  bug_report_output_blocks(assessment, target, output)
}

bug_report_output_blocks <- function(assessment, target, output) {
  failed <- names(Filter(function(check) isFALSE(check$passed), assessment$checks))
  facts <- list(output, if (length(failed) && draft_plain_names(failed))
    draft_item(if (length(failed) > 1L) "Failed checks" else "Failed check", draft_names(failed)))
  metrics <- comparison_records(assessment$checks$reference_comparison$summary)
  pairing <- assessment$differences$structure
  if (!identical(assessment$kind %||% "dataset", "dataset")) return(list(do.call(draft_list, facts)))
  if (!length(metrics) && !is_scalar_character(pairing$alignment_method)) {
    return(list(do.call(draft_list, facts),
      draft_text("The saved assessment holds no row comparison for this output.")))
  }

  metric <- function(name) {
    row <- Filter(function(x) identical(x$metric, name), metrics)
    if (length(row)) suppressWarnings(as.numeric(row[[1L]]$value)) else NA_real_
  }
  # A count that was measured is shown as it is, zero included.
  count <- function(x) {
    if (length(x) == 1L && is.finite(x)) format(x, scientific = FALSE, trim = TRUE) else "not recorded"
  }
  keys <- function(label, x) {
    x <- as.character(unlist(x))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) return(draft_item(label, "none"))
    if (draft_plain_names(x)) draft_item(label, draft_names(x)) else draft_item(label, "not shown")
  }
  method <- pairing$alignment_method
  method_note <- c(unique_key = "each key value names one row on each side",
    duplicate_key_multiset = "key values repeat, so rows inside a key group were matched by content",
    keyless_multiset = "no key was used, so rows were matched by content")
  finished <- pairing$alignment_resource_state
  summary <- dataset_discrepancy_summary(assessment, target)
  facts <- c(facts, list(
    keys("Keys configured", pairing$configured_keys),
    keys("Keys rows were paired on", pairing$selected_keys),
    if (is_scalar_character(method) && draft_plain_names(method)) draft_item("Pairing method",
      draft_names(method), if (method %in% names(method_note)) paste0(" (", method_note[[method]], ")")) else
      draft_item("Pairing method", "not recorded"),
    draft_item("Reference rows", count(summary$reference$rows)),
    draft_item("Generated rows", count(summary$generated$rows)),
    draft_item("Records paired", count(summary$rows_aligned)),
    draft_item("Reference rows left unpaired", count(metric("rows_only_base"))),
    draft_item("Generated rows left unpaired", count(metric("rows_only_comp"))),
    if (!is_scalar_character(finished)) draft_item("Row matching ran to its end", "not recorded") else
      if (identical(finished, "complete")) draft_item("Row matching ran to its end", "yes") else
        draft_item("Row matching ran to its end", "no, it stopped at a size limit")),
    bug_report_unpaired_key_items(pairing$unpaired_keys, count))
  # The sentence the run report gives for this output, from the same summary.
  difference <- if (length(metrics)) tryCatch(discrepancy_description(summary), error = function(error) NULL)
  list(do.call(draft_list, facts), draft_statement("Observed difference", difference),
    draft_text(BUG_REPORT_COUNT_LABELS))
}

# For each key column, the counts the comparison recorded about the rows left
# unpaired, shown as recorded. The record is optional: a report saved without
# it shows nothing. What the counts mean for the output is left to the
# sentence the comparison summary gives.
bug_report_unpaired_key_items <- function(records, count) {
  number <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x[[1L]]))
  Filter(Negate(is.null), lapply(comparison_records(records), function(record) {
    key <- if (is.list(record)) record$key
    if (!is_scalar_character(key) || !draft_plain_names(key)) return(NULL)
    counts <- c(`unpaired rows that pair without it` = number(record$pair_without),
      `unpaired reference rows where it is missing` = number(record$missing_reference),
      `unpaired generated rows where it is missing` = number(record$missing_generated))
    counts <- counts[is.finite(counts)]
    if (!length(counts)) return(NULL)
    list(list(text = "Key column "), list(code = key), list(text = paste0(": ",
      paste(names(counts), vapply(counts, count, ""), collapse = "; "))))
  }))
}

bug_report_markdown <- function(blocks) {
  span <- function(name) {
    ticks <- gregexpr("`+", name)[[1L]]
    fence <- strrep("`", 1L + max(0L, attr(ticks, "match.length")))
    pad <- if (grepl("^`|`$", name)) " " else ""
    paste0(fence, pad, name, pad, fence)
  }
  segment <- function(part) if (!is.null(part$code)) span(part$code) else part$text
  unlist(lapply(Filter(Negate(is.null), blocks), function(block) c(switch(block$type,
    heading = paste(strrep("#", block$level + 1L), block$text),
    text = if (isTRUE(block$strong)) paste0("**", block$text, "**") else block$text,
    statement = paste0(block$label, ": ", block$text),
    list = vapply(block$items, function(item)
      paste0("- ", paste(vapply(item, segment, ""), collapse = "")), ""),
    code = {
      # Use a fence longer than every backtick sequence in the draft R.
      ticks <- gregexpr("`+", block$text)[[1L]]
      fence <- strrep("`", 1L + max(2L, attr(ticks, "match.length")))
      c(paste0(fence, "r"), block$text, fence)
    }), "")), use.names = FALSE)
}

bug_report_html <- function(blocks) {
  segment <- function(part) {
    if (!is.null(part$code)) paste0("<code>", run_html_escape(part$code), "</code>") else
      run_html_escape(part$text)
  }
  unlist(lapply(Filter(Negate(is.null), blocks), function(block) switch(block$type,
    heading = sprintf("<h%d>%s</h%d>", block$level + 2L, run_html_escape(block$text), block$level + 2L),
    text = paste0("<p>", if (isTRUE(block$strong)) "<strong>", run_html_escape(block$text),
      if (isTRUE(block$strong)) "</strong>", "</p>"),
    statement = paste0("<p>", run_html_escape(block$label), ": ", run_html_escape(block$text), "</p>"),
    list = c("<ul>", vapply(block$items, function(item)
      paste0("<li>", paste(vapply(item, segment, ""), collapse = ""), "</li>"), ""), "</ul>"),
    code = paste0("<pre><code>", run_html_escape(block$text), "</code></pre>"))), use.names = FALSE)
}
