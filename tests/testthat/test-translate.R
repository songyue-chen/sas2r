# Tests for sas_translate() public workflow and methods

t1_file <- function(envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".sas", .local_envir = envir)
  writeLines(c(
    "data work.w1;",
    "  x = 10;",
    "run;"
  ), f)
  f
}

test_that("sas_translate executes clean dependency-aware workflow on a single SAS file", {
  f <- t1_file()
  out <- withr::local_tempdir()
  res <- sas_translate(f, out_dir = out, execute = TRUE)

  expect_s3_class(res, "sas2r_translation")
  expect_true(res$status %in% c("blocked", "needs_review", "migration_ready", "validated"))
  expect_true(dir.exists(res$bundle_dir))
  expect_true(file.exists(res$graph_path))
  expect_true(file.exists(res$report_path))
  expect_true(file.exists(res$report_json_path))
})

test_that("sas_translate with execute = FALSE leaves outputs_dir NULL", {
  f <- t1_file()
  out <- withr::local_tempdir()
  res <- sas_translate(f, out_dir = out, execute = FALSE)

  expect_s3_class(res, "sas2r_translation")
  expect_identical(res$status, "needs_review")
  expect_null(res$outputs_dir)
  expect_true(dir.exists(res$bundle_dir))
})

test_that("sas_code retrieves generated code from bundle directory", {
  f <- t1_file()
  out <- withr::local_tempdir()
  res <- sas_translate(f, out_dir = out, execute = FALSE)

  code <- sas_code(res, 1L)
  expect_true(is.character(code))
  expect_true(nzchar(code))

  expect_error(sas_code(res, "nonexistent.R"), class = "sas2r_file_not_found")
  expect_error(sas_code(res, 999L), class = "sas2r_file_not_found")
})

test_that("sas_write copies selected bundle and reports to destination", {
  f <- t1_file()
  out <- withr::local_tempdir()
  res <- sas_translate(f, out_dir = out, execute = FALSE)

  dst <- withr::local_tempdir()
  expect_warning(sas_write(res, dst), class = "sas2r_unverified_write")

  expect_true(file.exists(file.path(dst, "report.md")))
  expect_true(file.exists(file.path(dst, "sas2r-helpers.R")))
})

test_that("print.sas2r_translation formats status and paths", {
  f <- t1_file()
  out <- withr::local_tempdir()
  res <- sas_translate(f, out_dir = out, execute = FALSE)

  txt <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(txt, "status: needs_review", ignore.case = TRUE)
  expect_match(txt, "bundle:")
  expect_false(grepl("parity", txt, ignore.case = TRUE))
})

test_that("sas_translate rejects a mistyped budget_mode instead of silently downgrading enforcement", {
  f <- t1_file()
  out <- withr::local_tempdir()
  expect_error(
    sas_translate(f, out_dir = out, budget_mode = "strictt", execute = FALSE),
    class = "sas2r_invalid_argument"
  )
  expect_error(
    sas_translate(f, out_dir = out, pricing_source = "catalogg", execute = FALSE),
    class = "sas2r_invalid_argument"
  )
})

test_that("sas_translate renders progress events when enabled", {
  f <- t1_file()
  out <- withr::local_tempdir()
  withr::local_options(sas2r.progress = TRUE)
  msgs <- capture.output(
    res <- sas_translate(f, out_dir = out, execute = TRUE),
    type = "message"
  )
  expect_true(any(grepl("\\d+/\\d+", msgs)))
})

test_that("sas_translate surfaces components whose agent path was unavailable", {
  withr::local_options(sas2r.agent_backoff_base = 0)
  f <- withr::local_tempfile(fileext = ".sas")
  writeLines(c(
    "data work.w1;",
    "  set work.w0;",
    "  retain c 0;",
    "run;"
  ), f)
  out <- withr::local_tempdir()
  llm <- new_llm(function(request) {
    stop(structure(list(message = "rate limited", status_code = 429L),
                   class = c("sas2r_llm_rate_limit", "error", "condition")))
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))

  res <- sas_translate(f, out_dir = out, execute = FALSE, llm = llm)
  expect_true(length(res$diagnostics$agent_degraded) > 0L)
  expect_match(res$status_reason, "rate_limited")
})
