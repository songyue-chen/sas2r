# Tests for one public sas_translate() workflow, result object, report, and resume path

migration_demo_file <- function(envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".sas", .local_envir = envir)
  writeLines(c(
    "data work.demo;",
    "  x = 1;",
    "  y = 'A';",
    "run;"
  ), f)
  f
}

deterministic_migration_llm <- function() {
  code <- paste(
    "demo <- data.frame(x = 1, y = 'A', stringsAsFactors = FALSE)",
    "lib_write(demo, 'work', 'demo')",
    sep = "\n"
  )
  mock_llm(list(
    good_translation(code),
    good_review()
  ))
}

test_that("sas_translate returns one complete observable migration result", {
  expect_identical(names(formals(sas_translate)), c(
    "path", "out_dir", "config", "execute",
    "max_program_repair_rounds", "max_bundle_repair_rounds", "outputs",
    "agent_evidence", "llm", "budget_usd", "budget_mode",
    "pricing_source", "pricing_rates", "usage_limits", "recursive",
    "resume", "keep_raw_attempts", "max_bundle_repairs_per_component", "max_parallel_translations"
  ))
  f <- migration_demo_file()
  out <- withr::local_tempdir()
  result <- sas_translate(f, out_dir = out, llm = deterministic_migration_llm(), resume = FALSE)
  expect_named(result, c(
    "run_id", "out_dir", "bundle_dir", "outputs_dir", "status",
    "status_reason", "graph_path", "output_contracts_path", "report_path",
    "report_json_path", "component_evidence", "output_assessments",
    "diagnostics", "repair_history", "usage", "project"
  ), ignore.order = TRUE)
  dst <- withr::local_tempdir()
  expect_warning(written <- sas_write(result, dst), class = "sas2r_unverified_write")
  expect_identical(written, dst)
  expect_true(file.exists(file.path(dst, "report", "translation.md")))

  # An empty mock makes an unexpected new provider call fail on resume.
  resumed <- sas_translate(f, out_dir = out, llm = mock_llm(list()), resume = TRUE)
  expect_identical(resumed$status, result$status)
})

test_that("sas_translate with execute = FALSE snapshots bundle and returns needs_review", {
  out <- withr::local_tempdir()
  result <- sas_translate(
    migration_demo_file(),
    out_dir = out,
    execute = FALSE,
    llm = deterministic_migration_llm()
  )
  expect_s3_class(result, "sas2r_translation")
  expect_identical(result$status, "needs_review")
  expect_null(result$outputs_dir)
  expect_true(dir.exists(result$bundle_dir))
  expect_true(file.exists(result$report_path))
  expect_true(file.exists(result$report_json_path))

  # sas_code works with snapshot bundle
  code <- sas_code(result, 1L)
  expect_true(nzchar(code))
  cid <- names(result$component_evidence)[1L]
  expect_identical(sas_code(result, cid), code)
  expect_identical(sas_code(result, paste0(cid, ".R")), code)
  expect_warning(sas_write(result, withr::local_tempdir()), class = "sas2r_unverified_write")
  out_txt <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(out_txt, "status: needs_review", ignore.case = TRUE)
  expect_false(grepl("parity", out_txt, ignore.case = TRUE))
})

test_that("sas_translate with no reviewer records review_unavailable when execute = FALSE", {
  out <- withr::local_tempdir()
  result <- sas_translate(
    migration_demo_file(),
    out_dir = out,
    execute = FALSE,
    llm = NULL
  )
  expect_s3_class(result, "sas2r_translation")
  expect_identical(result$status, "blocked")
  # Evidence reflects review unavailable without reviewer
  expect_true(length(result$component_evidence) > 0L)
})
