test_that("the opt-in report diagnoses a bundled PDF writer regression and exports a runnable example", {
  root <- withr::local_tempdir()
  src <- file.path(root, "private-study.sas")
  writeLines(c("data work.sample; value = 1; run;",
    'ods rtf file="private-listing.rtf";', "proc print data=work.sample; run;",
    "ods rtf close;"), src)

  transpile <- sas_transpile
  write_runtime <- write_helpers
  testthat::local_mocked_bindings(
    sas_transpile = function(project, out_dir, ...) {
      result <- transpile(project, out_dir, ...)
      staged <- unique(stats::na.omit(result$manifest$staged_file))
      writeLines('sas_print_table(data.frame(value = 1), sas_output_path("private-listing.rtf"))',
        file.path(out_dir, staged))
      result
    },
    write_helpers = function(out_dir) {
      path <- write_runtime(out_dir)
      # Restore the earlier writer behavior in this synthetic bundle only.
      lines <- readLines(path, warn = FALSE)
      guard <- grep('if (!endsWith(tolower(file), ".pdf")) {', lines, fixed = TRUE)
      end <- guard + match("  }", lines[seq.int(guard + 1L, length(lines))])
      writeLines(lines[-seq.int(guard, end)], path)
      invisible(path)
    }
  )

  run <- function(enabled) suppressMessages(sas_translate(src, out_dir = tempfile(tmpdir = root),
    config = list(migration = list(bug_reports = enabled, report_diagnosis = "off")),
    usage_limits = list(max_calls = 0), max_bundle_repair_rounds = 0))
  disabled <- run(FALSE)
  enabled <- run(TRUE)
  expect_identical(enabled$status, disabled$status)
  expect_identical(enabled$output_assessments[["private-listing.rtf"]]$status, "failed")
  report <- read_json_record(enabled$report_json_path, simplifyVector = FALSE)
  expect_identical(report$package_bug_diagnosis$status, "completed")
  finding <- report$package_bug_diagnosis$findings[[1L]]
  expect_identical(finding$code, "pdf_written_to_non_pdf_destination")
  expect_identical(finding$attempt_id, report$selected_attempt_id)
  observation <- report$bundle_execution[[finding$component_id]]$selected_attempt
  expect_identical(finding$revision_id, observation$revision$revision_id)
  expect_identical(finding$helper_hash, unname(cli::hash_file_sha256(
    file.path(enabled$bundle_dir, "runtime", "sas2r-helpers.R"))))
  html <- file.path(dirname(dirname(enabled$report_json_path)), "START_HERE.html")
  expect_match(paste(readLines(html), collapse = "\n"), "Possible sas2r bugs (beta)", fixed = TRUE)

  file <- file.path(root, "draft.md")
  expect_error(sas_bug_report(disabled, file), class = "sas2r_bug_diagnosis_disabled")
  expect_false(file.exists(file))
  expect_identical(sas_bug_report(enabled, file), file)
  lines <- readLines(file)
  draft <- paste(lines, collapse = "\n")
  expect_match(draft, "Synthetic reproduction", fixed = TRUE)
  expect_false(grepl("private-", draft, fixed = TRUE))
  expect_false(grepl(root, draft, fixed = TRUE))
  expect_error(sas_bug_report(enabled, file), class = "sas2r_record_exists")
  expect_identical(readLines(file), lines)
  code <- lines[seq.int(match("```r", lines) + 1L, match("```", lines) - 1L)]
  expect_message(eval(parse(text = code)), "rejected the destination")
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(enabled$bundle_dir, "runtime", "sas2r-helpers.R"), runtime)
  code[grepl("render <-", code, fixed = TRUE)] <- "  render <- legacy_render"
  expect_error(eval(parse(text = code), list(legacy_render = runtime$sas_print_table)),
    "Reproduced: the helper wrote PDF bytes")
})

test_that("a scheduler failure preserves the error and writes an evidence-only draft", {
  root <- withr::local_tempdir()
  writeLines("data work.sample; value = 1; run;", file.path(root, "sample.sas"))
  # Simulate the scheduler receiving a component it cannot make ready.
  testthat::local_mocked_bindings(translation_providers = function(graph, schedule, ids) {
    stats::setNames(rep(list("unreachable"), length(ids)), ids)
  })
  out <- withr::local_tempdir()
  expect_error(suppressMessages(sas_translate(root, out_dir = out,
    config = list(migration = list(bug_reports = TRUE, report_diagnosis = "off")),
    max_parallel_translations = 2L, usage_limits = list(max_calls = 0))),
    class = "sas2r_pipeline_coverage_error")
  report <- list.files(out, pattern = "^report[.]json$", recursive = TRUE, full.names = TRUE)
  expect_length(report, 1L)
  data <- read_json_record(report, simplifyVector = FALSE)
  expect_identical(data$status, "blocked")
  expect_identical(data$package_bug_diagnosis$findings[[1L]]$code, "scheduler_unaccounted_components")
  file <- file.path(out, "draft.md")
  expect_identical(sas_bug_report(report, file), file)
  draft <- paste(readLines(file), collapse = "\n")
  expect_match(draft, "Reproduction pending", fixed = TRUE)
  expect_match(draft, "Pending components: `1`", fixed = TRUE)
  expect_match(draft, "Translation workers: `2`", fixed = TRUE)
  expect_false(grepl("sample", draft, fixed = TRUE))
  expect_false(grepl("```r", draft, fixed = TRUE))
})

test_that("ordinary run failures leave package-bug attribution unresolved", {
  root <- withr::local_tempdir()
  writeLines("data work.sample; set private.missing; run;", file.path(root, "sample.sas"))
  cfg <- list(migration = list(bug_reports = TRUE, report_diagnosis = "off"))
  result <- suppressMessages(sas_translate(root, config = cfg, out_dir = withr::local_tempdir(),
    usage_limits = list(max_calls = 0)))
  report <- read_json_record(result$report_json_path, simplifyVector = FALSE)
  expect_false(result$status %in% c("migration_ready", "validated"))
  expect_length(report$package_bug_diagnosis$findings, 0L)
  expect_error(sas_bug_report(result, file.path(root, "draft.md")),
    class = "sas2r_no_package_bug_finding")
  expect_false(normalize_migration_config(NULL)$bug_reports)
  expect_error(normalize_migration_config(list(bug_reports = "true")),
    class = "sas2r_config_error")
})
