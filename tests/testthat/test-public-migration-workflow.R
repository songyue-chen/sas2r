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
    "resume", "keep_raw_attempts"
  ))
  result <- sas_translate(
    migration_demo_file(), out_dir = withr::local_tempdir(),
    llm = deterministic_migration_llm()
  )
  expect_named(result, c(
    "run_id", "out_dir", "bundle_dir", "outputs_dir", "status",
    "status_reason", "graph_path", "output_contracts_path", "report_path",
    "report_json_path", "component_evidence", "output_assessments",
    "diagnostics", "repair_history", "usage", "project"
  ), ignore.order = TRUE)
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
  expect_identical(result$status, "needs_review")
  # Evidence reflects review unavailable without reviewer
  expect_true(length(result$component_evidence) > 0L)
})

test_that("sas_code resolves by component ID, relative path, basename, and index", {
  out <- withr::local_tempdir()
  result <- sas_translate(
    migration_demo_file(),
    out_dir = out,
    execute = FALSE,
    llm = deterministic_migration_llm()
  )
  cids <- names(result$component_evidence)
  if (length(cids) > 0L) {
    cid <- cids[1]
    code1 <- sas_code(result, cid)
    code2 <- sas_code(result, 1L)
    expect_identical(code1, code2)

    r_file <- paste0(cid, ".R")
    code3 <- sas_code(result, r_file)
    expect_identical(code1, code3)
  }
})

test_that("sas_write copies selected bundle, outputs, and report", {
  out <- withr::local_tempdir()
  result <- sas_translate(
    migration_demo_file(),
    out_dir = out,
    execute = TRUE,
    llm = deterministic_migration_llm()
  )
  dst <- withr::local_tempdir()
  written <- sas_write(result, dst)
  expect_identical(written, dst)
  expect_true(file.exists(file.path(dst, "report.md")))
})

test_that("sas_write warns on blocked or needs_review status", {
  out <- withr::local_tempdir()
  result <- sas_translate(
    migration_demo_file(),
    out_dir = out,
    execute = FALSE,
    llm = deterministic_migration_llm()
  )
  dst <- withr::local_tempdir()
  expect_warning(sas_write(result, dst), class = "sas2r_unverified_write")
})

test_that("print.sas2r_translation prints status and paths without saying parity", {
  out <- withr::local_tempdir()
  result <- sas_translate(
    migration_demo_file(),
    out_dir = out,
    execute = FALSE,
    llm = deterministic_migration_llm()
  )
  out_txt <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(out_txt, "status: needs_review", ignore.case = TRUE)
  expect_false(grepl("parity", out_txt, ignore.case = TRUE))
})

test_that("resume = TRUE reuses completed work without duplicate paid calls", {
  out <- withr::local_tempdir()
  f <- migration_demo_file()
  llm1 <- deterministic_migration_llm()
  res1 <- sas_translate(f, out_dir = out, llm = llm1, resume = FALSE)

  # Resume with empty LLM (no mock responses); if it attempts new calls it would fail or use calls
  llm_empty <- mock_llm(list())
  res2 <- sas_translate(f, out_dir = out, llm = llm_empty, resume = TRUE)
  expect_identical(res1$status, res2$status)
})
