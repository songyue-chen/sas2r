# Tests for single-file vs directory equivalence in migration workflow

test_that("single SAS file and single-file directory produce equivalent migration contracts and outputs", {
  temp_root <- withr::local_tempdir()

  # Create a self-contained SAS program that reads and writes a dataset
  input_data_dir <- file.path(temp_root, "data")
  dir.create(input_data_dir, recursive = TRUE)
  saveRDS(data.frame(ID = 1:3, VAL = c("a", "b", "c"), stringsAsFactors = FALSE), file.path(input_data_dir, "input.rds"))

  sas_content <- c(
    "data adam.output;",
    "  set adam.input;",
    "  NEWVAR = 1;",
    "run;"
  )

  # Setup 1: direct single file
  single_file <- file.path(temp_root, "single_study.sas")
  writeLines(sas_content, single_file)

  # Setup 2: directory containing only that single file
  dir_study <- file.path(temp_root, "dir_study")
  dir.create(dir_study, recursive = TRUE)
  dir_file <- file.path(dir_study, "single_study.sas")
  writeLines(sas_content, dir_file)

  cfg <- list(
    libraries = list(
      adam = list(path = input_data_dir, engine = "rds", write = "rds")
    )
  )

  out_single <- file.path(temp_root, "out_single")
  out_dir <- file.path(temp_root, "out_dir")

  res_single <- sas_translate(
    path = single_file,
    out_dir = out_single,
    config = cfg,
    execute = TRUE
  )

  res_dir <- sas_translate(
    path = dir_study,
    out_dir = out_dir,
    config = cfg,
    execute = TRUE
  )

  # Check statuses
  expect_identical(res_single$status, res_dir$status)

  # Graph nodes and edges comparison (ignoring physical file paths which naturally differ)
  g_single <- read_json_record(res_single$graph_path)
  g_dir <- read_json_record(res_dir$graph_path)

  expect_identical(g_single$nodes$type, g_dir$nodes$type)
  expect_identical(g_single$edges$type, g_dir$edges$type)
  expect_identical(g_single$edges$resolution, g_dir$edges$resolution)

  # Output contracts
  oc_single <- read_json_record(res_single$output_contracts_path)
  oc_dir <- read_json_record(res_dir$output_contracts_path)
  expect_identical(oc_single$target_key, oc_dir$target_key)
  expect_identical(oc_single$kind, oc_dir$kind)
  expect_identical(oc_single$required, oc_dir$required)

  # Report JSON fields
  r_single <- read_json_record(res_single$report_json_path)
  r_dir <- read_json_record(res_dir$report_json_path)
  expect_identical(r_single$status, r_dir$status)
  expect_identical(r_single$status_reason, r_dir$status_reason)

  # Generated R code in bundle
  code_single <- sas_code(res_single, 1L)
  code_dir <- sas_code(res_dir, 1L)
  expect_identical(trimws(code_single), trimws(code_dir))
})
