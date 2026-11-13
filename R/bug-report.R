#' Write an issue draft for a possible sas2r bug (beta)
#'
#' Enable `migration.bug_reports: true` before running [sas_translate()].
#' Use the returned run or its saved machine report, including a blocked
#' run's report if translation raised an error. Review the draft before
#' sending it to the maintainer or posting it to the public issue tracker.
#'
#' @param run A `sas2r_translation` object or the path to its `report.json`.
#' @param file Explicit path for a new Markdown file.
#' @return The destination path, invisibly.
#' @details
#' The draft includes the package-level suspicion and selected run metadata.
#' For supported findings it includes synthetic R code and an expected result.
#' Run that code in a fresh R session using the reported sas2r build, then
#' record the result. If the run used a modified or older bundled runtime,
#' use the recorded runtime hash to identify it. Other findings include
#' instructions for preparing a minimal example.
#'
#' This function works offline. It reads the recorded findings and writes a
#' draft locally. It never runs the example or submits the draft.
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
  if (!isTRUE(diagnosis$schema_version == 1L) ||
      !identical(diagnosis$status, "completed")) {
    cli::cli_abort("Beta bug diagnosis is unavailable for this report. See the local machine report.",
      class = "sas2r_bug_diagnosis_unavailable")
  }
  if (!length(diagnosis$findings)) {
    cli::cli_abort("This run has no recorded package-bug suspicions. See the local run diagnostics.",
      class = "sas2r_no_package_bug_finding")
  }

  # Format only fields whose shape is controlled by sas2r or R. Local component
  # names, source paths, condition text and argument values stay in the report.
  env <- diagnosis$environment
  lines <- c("# Possible sas2r bug", "",
    "This is a draft for review. Confirm the reproduction result before submitting.",
    "", "## Run environment", "",
    bug_report_field("sas2r", env$sas2r, "^[0-9]+([.-][0-9]+)+$"),
    bug_report_field("sas2r commit", env$revision, "^[a-f0-9]{40}$"),
    bug_report_field("R", env$R, "^[0-9]+([.-][0-9]+)+$"),
    bug_report_field("Platform", env$platform, "^[A-Za-z0-9_]+(-[A-Za-z0-9_.]+)+$"),
    bug_report_field("Translation workers", env$workers, "^[0-9]+$"),
    bug_report_field("Run ID", report$run_id, "^run_[0-9]{8}T[0-9]{6}Z_[a-f0-9]{8}$"))
  for (finding in diagnosis$findings) {
    lines <- c(lines, "", bug_report_finding_lines(finding))
  }
  lines <- c(lines, "", "## Result on your machine", "",
    "Record the build you tested and the observed result. Review any additions for confidential information.",
    "", "Submit the reviewed draft at https://github.com/songyue-chen/sas2r/issues/new or send it to the maintainer.")
  atomic_write_file(function(path) writeLines(lines, path, useBytes = TRUE), file,
    require_atomic = TRUE)
  invisible(file)
}

bug_report_field <- function(label, value, pattern) {
  if (is.numeric(value)) value <- as.character(value)
  if (!is_scalar_character(value) || !grepl(pattern, value) || grepl("[\r\n]", value)) return(NULL)
  paste0("- ", label, ": `", value, "`")
}

bug_report_finding_lines <- function(finding) {
  switch(finding$code,
    pdf_written_to_non_pdf_destination = c(
      "## Bundled PDF writer accepted a non-PDF destination", "",
      paste0(
        "A bundled `sas_print_table()` call matched a `.", finding$format,
        "` output containing PDF bytes."),
      "Expected: the PDF-only helper raises an error before creating the destination.",
      "", bug_report_field("Bundle attempt", finding$attempt_id, "^bundle_attempt_[0-9]+$"),
      bug_report_field("Code revision", finding$revision_id, "^(r[0-9]+|rev_[a-f0-9]+(_[0-9]+)?)$"),
      bug_report_field("Bundled runtime SHA-256", finding$helper_hash, "^[a-f0-9]{64}$"),
      "", "### Synthetic reproduction", "",
      "Run this with the reported package build. If you have the matching local bundle,",
      "you can replace the renderer assignment with the function loaded from that bundle.",
      "The script should finish without an assertion failure when the guard works.",
      "", "```r", pdf_destination_example(finding$format), "```"),
    scheduler_unaccounted_components = c(
      "## Translation scheduler stopped with pending components", "",
      "The parallel translation scheduler stopped with pending components and no active worker.",
      "Expected: schedule the remaining components or report the specific dependency that prevents scheduling.",
      "", bug_report_field("Pending components", finding$pending_count, "^[0-9]+$"),
      bug_report_field("Execution order", finding$order, "^(inferred|configured)$"),
      if (isTRUE(finding$resume_requested)) "- Resume requested: yes" else "- Resume requested: no",
      "- Condition: `sas2r_scheduler_unaccounted_components`",
      "", "### Reproduction pending", "",
      "Create synthetic SAS programs with the same dependency shape and preserve the configured order,",
      "worker count and whether the run resumed. Rerun with the same sas2r build and record",
      "whether the scheduler raises the same condition."),
    cli::cli_abort("This report contains a finding from an unsupported beta version.",
      class = "sas2r_bug_diagnosis_unavailable"))
}

pdf_destination_example <- function(format) {
  if (!is_pdf_destination_finding_format(format)) {
    cli::cli_abort("The reported output format is unavailable.",
      class = "sas2r_bug_diagnosis_unavailable")
  }
  c("local({",
    "  render <- sas2r::sas_print_table",
    paste0('  dest <- tempfile(fileext = ".', format, '")'),
    "  on.exit(unlink(dest))",
    "  result <- tryCatch(render(data.frame(value = c(1, 2)), dest), error = identity)",
    '  if (file.exists(dest) && identical(readBin(dest, "raw", n = 5L), charToRaw("%PDF-")))',
    '    stop("Reproduced: the helper wrote PDF bytes to a non-PDF destination.")',
    '  if (!inherits(result, "error") || file.exists(dest))',
    '    stop("Unexpected result: expected a format error and no output file.")',
    '  if (!grepl("file must end in .pdf", conditionMessage(result), fixed = TRUE)) stop(result)',
    '  message("The helper rejected the destination before writing a file.")',
    "})")
}
