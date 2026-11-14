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
#' It includes the reviewer's explanation, uncertainty and suggested next step.
#' When the reviewer drafts synthetic R code, review it before running it in a
#' fresh R session using the reported build. Record the result in the draft.
#' A reproducible failure needs an observed result from that separate test.
#'
#' AI triage occurs during translation using the configured reviewer. This
#' function renders its saved findings locally. It neither calls the model
#' nor runs examples. Model text and code may reflect the supplied project
#' evidence, so review and remove confidential details before sharing.
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

  env <- diagnosis$environment
  lines <- c("# Possible sas2r bug", "",
    "AI-generated triage. Review the diagnosis, code and disclosure before sharing.",
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
  lines <- c(lines, "",
    "Review the proposed R code before running it. Remove confidential details before sharing.",
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
  code <- finding$repro_r
  # Use a fence longer than every backtick sequence in the draft R.
  ticks <- if (is_scalar_character(code)) gregexpr("`+", code)[[1L]] else integer()
  fence <- strrep("`", 1L + max(2L, attr(ticks, "match.length")))
  c(
    paste0("## AI suspicion [", finding$evidence_id, "]"), "",
    paste("Recorded symptom:", finding$observed), "",
    paste("Possible package cause:", finding$explanation), "",
    paste("Expected behavior:", finding$expected), "",
    paste("Uncertainty:", finding$uncertainty), "",
    paste("Next step:", finding$next_step), "",
    bug_report_field("Bundle attempt", finding$binding$attempt_id, "^bundle_attempt_[0-9]+$"),
    bug_report_field("Code revision", finding$binding$revision_id, "^(r[0-9]+|rev_[a-f0-9]+(_[0-9]+)?)$"),
    bug_report_field("Bundled runtime SHA-256", finding$binding$helper_hash, "^[a-f0-9]{64}$"),
    "", paste0("**", package_bug_reproduction_label(finding), "**"),
    if (is_scalar_character(code)) c("", "### Proposed synthetic reproduction", "",
      "Review the draft code, then test it with the reported package build or matching bundled runtime.",
      "", paste0(fence, "r"), code, fence),
    "", "### Result on your machine", "",
    "- Outcome: unverified (update to reproduced, not reproduced, or inconclusive after testing).",
    "- Build and runtime tested:",
    "- Observed result:")
}
