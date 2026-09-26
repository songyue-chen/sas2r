# Test the migration workflow with a deterministic translator and reviewer.
# This covers synthetic data and PDF content, not independent SAS equivalence.

test_that("demo input generation requires a destination outside the copied project", {
  script <- system.file("examples", "migration-demo", "make-input.R", package = "sas2r")
  expect_true(file.exists(script))
  elsewhere <- withr::local_tempdir()
  withr::local_dir(elsewhere)
  expect_error(source(script, local = new.env()), "pass its data directory", fixed = TRUE)
  expect_false(dir.exists("data"))

  # The command-line destination works even when the caller is elsewhere.
  destination <- file.path(elsewhere, "copied demo", "data")
  output <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(script), shQuote(destination)),
                    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"))
  expect_equal(nrow(readRDS(file.path(destination, "input_ds.rds"))), 5L)
  expect_false(dir.exists("data"))
})

test_that("migration demo executes from local RDS input and produces dataset and TLF outputs", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("callr")

  demo_root <- test_path("..", "..", "inst", "examples", "migration-demo")
  if (!dir.exists(demo_root)) {
    demo_root <- system.file("examples", "migration-demo", package = "sas2r")
  }
  expect_true(dir.exists(demo_root))

  # Create a clean working copy of demo_root
  temp_dir <- withr::local_tempdir()
  temp_demo <- file.path(temp_dir, "migration-demo")
  dir.create(temp_demo, recursive = TRUE)
  file.copy(list.files(demo_root, full.names = TRUE), temp_demo, recursive = TRUE)

  # Run make-input.R in temp_demo
  make_input_script <- file.path(temp_demo, "make-input.R")
  expect_true(file.exists(make_input_script))
  withr::local_dir(temp_demo)
  source(make_input_script, local = new.env())

  input_rds <- file.path(temp_demo, "data", "input_ds.rds")
  expect_true(file.exists(input_rds))

  # Deterministic test worker
  demo_r_code <- paste(
    "# Generated R code for migration demo",
    "plotds_name <- 'input_ds'",
    "stg1 <- lib_read('raw', plotds_name)",
    "stg1$AVAL_FLAG <- ifelse(is.na(stg1$AVAL), 'MISSING', 'RECORDED')",
    "lib_write(stg1, 'work', 'stg1')",
    "",
    "stg_sort <- sas_sort(lib_read('work', 'stg1'), by = c('TRTP', 'AVISITN'))",
    "lib_write(stg_sort, 'work', 'stg_sort')",
    "",
    "final_ds <- lib_read('work', 'stg_sort')",
    "final_ds$HIGH_FLAG <- ifelse(chr_cmp(final_ds$AVAL_FLAG, 'RECORDED', '==') & (!is.na(final_ds$AVAL) & final_ds$AVAL > 10), 1, 0)",
    "lib_write(final_ds, 'adam', 'final_ds')",
    "",
    "dir.create('outputs', recursive = TRUE, showWarnings = FALSE)",
    "pdf('outputs/table1.pdf')",
    "plot.new()",
    "text(0, 1, paste(capture.output(print(final_ds)), collapse = '\\n'),",
    "     adj = c(0, 1), family = 'mono', cex = 0.7)",
    "dev.off()",
    sep = "\n"
  )

  make_worker <- function() {
    schema_routed_llm(
      translation = good_translation(demo_r_code),
      review = good_review()
    )$llm
  }

  demo_file <- file.path(temp_demo, "demo.sas")
  out_single <- file.path(temp_dir, "out_single")
  out_dir <- file.path(temp_dir, "out_dir")

  res_single <- sas_translate(
    path = demo_file,
    out_dir = out_single,
    config = file.path(temp_demo, "_sas2r.yml"),
    llm = make_worker(),
    execute = TRUE
  )

  res_dir <- sas_translate(
    path = temp_demo,
    out_dir = out_dir,
    config = file.path(temp_demo, "_sas2r.yml"),
    llm = make_worker(),
    execute = TRUE
  )

  expect_s3_class(res_single, "sas2r_translation")
  expect_s3_class(res_dir, "sas2r_translation")

  # Both produce migration_ready
  expect_identical(res_single$status, "migration_ready")
  expect_identical(res_dir$status, "migration_ready")

  # Check final dataset exists in candidates/outputs for both
  expect_true(!is.null(res_single$bundle_dir) && dir.exists(res_single$bundle_dir))
  expect_true(!is.null(res_dir$bundle_dir) && dir.exists(res_dir$bundle_dir))

  for (result in list(res_single, res_dir)) {
    expect_true(file.exists(file.path(result$outputs_dir, "datasets", "adam", "final_ds.rds")))
    expect_true(file.exists(file.path(result$outputs_dir, "tlf", "outputs", "table1.pdf")))
  }

  # Source-derived values for the synthetic input, including missingness and order.
  expected <- data.frame(
    USUBJID = c("01", "05", "03", "02", "04"),
    AVISITN = c(1L, 1L, 2L, 1L, 2L),
    AVAL = c(12.5, 14.2, 15.0, NA_real_, 9.8),
    TRTP = c("TRT A", "TRT A", "TRT A", "TRT B", "TRT B"),
    AVAL_FLAG = c("RECORDED", "RECORDED", "RECORDED", "MISSING", "RECORDED"),
    HIGH_FLAG = c(1, 1, 1, 0, 0),
    stringsAsFactors = FALSE
  )
  for (result in list(res_single, res_dir)) {
    actual <- readRDS(file.path(result$outputs_dir, "datasets", "adam", "final_ds.rds"))
    for (column in names(expected)) expect_equal(actual[[column]], expected[[column]])
    expect_identical(names(actual), names(expected))
    if (requireNamespace("pdftools", quietly = TRUE)) {
      pages <- pdftools::pdf_text(file.path(result$outputs_dir, "tlf", "outputs", "table1.pdf"))
      expect_length(pages, 1L)
      for (label in c(names(expected), "MISSING", "RECORDED", "12.5", "14.2", "9.8")) {
        expect_match(pages[[1L]], label, fixed = TRUE)
      }
    }
  }

  # Both entry paths use the same selected code; this is not SAS equivalence.
  code_single <- sas_code(res_single, 1L)
  code_dir <- sas_code(res_dir, 1L)
  expect_identical(trimws(code_single), trimws(code_dir))
})
