test_that("report contains verdict, tables, and differing values", {
  b <- tibble::tibble(id = c("1", "2"), aval = c(1, 2))
  cm <- tibble::tibble(id = c("1", "2"), aval = c(1, 99))
  r <- compare_datasets(b, cm, keys = "id")
  out <- withr::local_tempfile(fileext = ".md")
  res <- write_comparison_report(r, out)
  expect_identical(res, out)
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "# Dataset comparison")
  expect_match(txt, "FAILED")
  expect_match(txt, "\\| aval \\|")
  expect_match(txt, "99")   # full fidelity is the point of the local report
})

test_that("passing report says so", {
  b <- tibble::tibble(id = "1", x = 1)
  r <- compare_datasets(b, b, keys = "id")
  out <- withr::local_tempfile(fileext = ".md")
  write_comparison_report(r, out)
  expect_match(paste(readLines(out), collapse = "\n"), "PASSED within tolerance")
})

test_that("custom labels and profile version are rendered", {
  b <- tibble::tibble(id = "1", x = 1)
  r <- compare_datasets(b, b, keys = "id")
  out <- withr::local_tempfile(fileext = ".md")
  write_comparison_report(r, out, base_label = "Production SAS", comp_label = "Rewritten R")
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "- base: Production SAS")
  expect_match(txt, "- compare: Rewritten R")
  expect_match(txt, "- profile version: 1.1")
})

test_that("empty details and cosmetic sections render (none)", {
  b <- tibble::tibble(id = "1", x = 1)
  r <- compare_datasets(b, b, keys = "id")
  out <- withr::local_tempfile(fileext = ".md")
  write_comparison_report(r, out)
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "## Cosmetic differences \\(tolerated and reported\\)\n\n\\(none\\)")
  expect_match(txt, "## Differing cells \\(first 1000\\)\n\n\\(none\\)")
})

test_that("truncation note appears when details exceed 1000 rows", {
  b <- tibble::tibble(id = as.character(seq_len(1005)), val = rep(1, 1005))
  cm <- tibble::tibble(id = as.character(seq_len(1005)), val = rep(2, 1005))
  r <- compare_datasets(b, cm, keys = "id")
  expect_true(isTRUE(attr(r$details, "details_truncated")))
  out <- withr::local_tempfile(fileext = ".md")
  write_comparison_report(r, out)
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "_details truncated at 1000 rows_")
})

test_that("non-sas2r_comparison input errors", {
  out <- withr::local_tempfile(fileext = ".md")
  expect_error(write_comparison_report(list(a = 1), out))
})

test_that("pipe characters in cell values are escaped in markdown table", {
  b <- tibble::tibble(id = "1", txt = "val | with | pipes")
  cm <- tibble::tibble(id = "1", txt = "other | value")
  r <- compare_datasets(b, cm, keys = "id")
  out <- withr::local_tempfile(fileext = ".md")
  write_comparison_report(r, out)
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "val \\| with \\| pipes", fixed = TRUE)
})

test_that("R11: report renders Structure section with kind mismatches and one-sided columns", {
  b <- tibble::tibble(id = "1", aval = 10, extra_b = "only in base")
  cm <- tibble::tibble(id = "1", aval = "10", extra_c = "only in comp")
  r <- compare_datasets(b, cm, keys = "id")
  out <- withr::local_tempfile(fileext = ".md")
  write_comparison_report(r, out)
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "## Structure")
  expect_match(txt, "### Columns only in base\n\n- extra_b")
  expect_match(txt, "### Columns only in compare\n\n- extra_c")
  expect_match(txt, "### Column type mismatches")
  expect_match(txt, "\\| aval \\| numeric \\| character \\|")
})

test_that("write_migration_report writes report.json and report.md and redacts secrets", {
  out_dir <- withr::local_tempdir()
  paths <- init_migration_paths(out_dir)

  state <- list(
    paths = paths,
    out_dir = out_dir,
    run_id = "test_run_123",
    status = "migration_ready",
    status_reason = NULL,
    bundle_dir = file.path(out_dir, "bundle"),
    outputs_dir = file.path(out_dir, "outputs"),
    graph = list(
      nodes = tibble::tibble(node_id = "n1", component_id = "c1", type = "source_unit"),
      edges = tibble::tibble(edge_id = "e1", from = "n1", to = "n2", type = "reads_dataset", resolution = "resolved")
    ),
    schedule = tibble::tibble(component_id = "c1"),
    output_contracts = empty_output_contracts(),
    assessment = list(status = "migration_ready", targets = list()),
    histories = list(
      c1 = new_component_evidence_history("c1")
    ),
    repairs = list(),
    diagnostics = list(stop_reason = "Bearer sk-12345678901234567890secretkey")
  )

  rep_md <- write_migration_report(state)
  expect_true(file.exists(paths$report_md))
  expect_true(file.exists(paths$report_json))

  # Read JSON
  rep_json <- read_migration_report(paths$report_json)
  expect_identical(rep_json$status, "migration_ready")
  expect_identical(rep_json$run_id, "test_run_123")
  # Verify secret redaction
  expect_match(rep_json$diagnostics$stop_reason, "\\[REDACTED")
  expect_false(grepl("secretkey", rep_json$diagnostics$stop_reason))

  # Read MD
  txt <- paste(readLines(paths$report_md), collapse = "\n")
  expect_match(txt, "# SAS to R Migration Report")
  expect_match(txt, "migration_ready")
})




