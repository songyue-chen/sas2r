# Test executable migration demo on single file and directory

test_that("migration demo executes from local RDS input and produces dataset and TLF outputs", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("callr")

  demo_root <- test_path("..", "..", "inst", "examples", "migration-demo")
  if (!dir.exists(demo_root)) {
    demo_root <- system.file("examples", "migration-demo", package = "sas2r")
  }
  skip_if(!dir.exists(demo_root), "migration-demo directory not found")

  # Create a clean working copy of demo_root
  temp_dir <- withr::local_tempdir()
  temp_demo <- file.path(temp_dir, "migration-demo")
  dir.create(temp_demo, recursive = TRUE)
  file.copy(list.files(demo_root, full.names = TRUE), temp_demo, recursive = TRUE)

  # Run make-input.R in temp_demo
  make_input_script <- file.path(temp_demo, "make-input.R")
  expect_true(file.exists(make_input_script))
  source(make_input_script, local = new.env())

  input_rds <- file.path(temp_demo, "data", "input_ds.rds")
  expect_true(file.exists(input_rds))

  # Deterministic test worker
  demo_r_code <- paste(
    "# Generated R code for migration demo",
    "plotds_name <- 'input_ds'",
    "stg1 <- lib_read('adam', plotds_name)",
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
    "pdf('outputs/figure1.pdf')",
    "plot(final_ds$AVAL, main = 'Demo Plot')",
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

  single_attempt <- dirname(res_single$bundle_dir)
  dir_attempt <- dirname(res_dir$bundle_dir)

  expect_true(
    file.exists(file.path(single_attempt, "adam", "final_ds.rds")) ||
    file.exists(file.path(single_attempt, "candidates", "adam", "final_ds.rds")) ||
    file.exists(file.path(single_attempt, "work", "final_ds.rds")) ||
    (!is.null(res_single$outputs_dir) && file.exists(file.path(res_single$outputs_dir, "final_ds.rds")))
  )
  expect_true(
    file.exists(file.path(dir_attempt, "adam", "final_ds.rds")) ||
    file.exists(file.path(dir_attempt, "candidates", "adam", "final_ds.rds")) ||
    file.exists(file.path(dir_attempt, "work", "final_ds.rds")) ||
    (!is.null(res_dir$outputs_dir) && file.exists(file.path(res_dir$outputs_dir, "final_ds.rds")))
  )

  # Check TLF exists in both
  expect_true(file.exists(file.path(single_attempt, "outputs", "figure1.pdf")))
  expect_true(file.exists(file.path(dir_attempt, "outputs", "figure1.pdf")))

  # Normalized content equivalence
  code_single <- sas_code(res_single, 1L)
  code_dir <- sas_code(res_dir, 1L)
  expect_identical(trimws(code_single), trimws(code_dir))
})
