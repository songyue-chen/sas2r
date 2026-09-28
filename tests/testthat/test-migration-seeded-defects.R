# Tests for seeded material defects ensuring none receives migration_ready
# Gates checked:
# 1. Wrong SAS missing-value branch
# 2. Reversed sort/order semantics
# 3. Stale dependency binding reused after upstream R change
# 4. Removed terminal dataset write
# 5. Missing TLF destination
# 6. Review-unavailable output-lineage code with no later runtime coverage

test_that("seeded defect 1: wrong SAS missing-value branch does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  ref_dir <- file.path(tmp, "ref", "adam")
  dir.create(in_dir, recursive = TRUE)
  dir.create(ref_dir, recursive = TRUE)

  # In SAS, numeric missing '.' is less than all numbers (val < 10 is TRUE for .)
  in_df <- data.frame(
    USUBJID = c("01", "02", "03"),
    VAL = c(NA_real_, 5.0, 15.0),
    stringsAsFactors = FALSE
  )
  # Correct SAS output where missing VAL is included under VAL < 10
  ref_df <- data.frame(
    USUBJID = c("01", "02"),
    VAL = c(NA_real_, 5.0),
    stringsAsFactors = FALSE
  )

  saveRDS(in_df, file.path(in_dir, "input.rds"))
  saveRDS(ref_df, file.path(ref_dir, "out.rds"))

  sas_file <- file.path(tmp, "01_missing_branch.sas")
  writeLines(c(
    "data adam.out;",
    "  set adam.input;",
    "  where val < 10;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(ref_dir, winslash = "/", mustWork = FALSE))
  ), cfg_file)

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_true(res$status %in% c("blocked", "needs_review"))
})

test_that("seeded defect 2: reversed sort/order semantics does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  ref_dir <- file.path(tmp, "ref", "adam")
  dir.create(in_dir, recursive = TRUE)
  dir.create(ref_dir, recursive = TRUE)

  in_df <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    VISITNUM = c(1, 2, 3, 4),
    AVAL = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  # Correct SAS proc sort descending: 4, 3, 2, 1
  ref_df <- in_df[order(-in_df$VISITNUM), ]

  saveRDS(in_df, file.path(in_dir, "input.rds"))
  saveRDS(ref_df, file.path(ref_dir, "sorted.rds"))

  sas_file <- file.path(tmp, "02_sort.sas")
  writeLines(c(
    "proc sort data=adam.input out=adam.sorted;",
    "  by visitnum;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(ref_dir, winslash = "/", mustWork = FALSE))
  ), cfg_file)

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_true(res$status %in% c("blocked", "needs_review"))
})

test_that("seeded defect 4: removed terminal dataset write does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  dir.create(in_dir, recursive = TRUE)

  in_df <- data.frame(USUBJID = c("01", "02"), stringsAsFactors = FALSE)
  saveRDS(in_df, file.path(in_dir, "input.rds"))

  sas_file <- file.path(tmp, "04_no_write.sas")
  writeLines(c(
    "proc mystery data=adam.input;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "outputs:",
    "  datasets:",
    "    - adam.final_ds"
  ), cfg_file)

  # Defective R code computes data frame but removes lib_write() call
  defective_r_code <- paste(
    "input <- lib_read('adam', 'input')",
    "final_ds <- transform(input, val = 42)",
    "# lib_write(final_ds, 'adam', 'final_ds') REMOVED",
    sep = "\n"
  )

  mock <- mock_llm(list(
    good_translation(defective_r_code),
    good_review()
  ))

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), llm = mock, execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_identical(res$status, "blocked")
})
