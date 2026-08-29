test_that("locate_unit_block identifies deterministic, stub, and llm blocks accurately", {
  lines <- c(
    "# --- sas2r:unit 1 ---",
    "x <- 1",
    "# --- sas2r:unit 2 ---",
    "# sas2r:untranslated unit=2 reason=proc_unsupported",
    "# --- original SAS ---",
    "# proc glm; run;",
    "# --------------------",
    "# --- sas2r:unit 3 ---",
    "# sas2r:llm_authored unit=3 -- REVIEW REQUIRED",
    "y <- 2",
    "# sas2r:end unit=3"
  )
  loc1 <- locate_unit_block(lines, 1)
  expect_identical(unname(loc1), c(1L, 2L))
  loc2 <- locate_unit_block(lines, 2)
  expect_identical(unname(loc2), c(3L, 7L))
  loc3 <- locate_unit_block(lines, 3)
  expect_identical(unname(loc3), c(8L, 11L))
  loc_none <- locate_unit_block(lines, 99)
  expect_identical(length(loc_none), 0L)
})

test_that("splice_unit_code and extract_unit_block roundtrip safely", {
  dir <- withr::local_tempdir()
  r_file <- file.path(dir, "out.R")
  writeLines(c(
    "# --- sas2r:unit 1 ---",
    "# sas2r:untranslated unit=1 reason=stub",
    "# --- original SAS ---",
    "# proc transpose; run;",
    "# --------------------"
  ), r_file)
  splice_unit_code(r_file, 1L, "df <- t(df)", kind = "llm")
  extracted <- extract_unit_block(r_file, 1L)
  expect_true(any(grepl("# --- sas2r:unit 1 ---", extracted)))
  expect_true(any(grepl("# sas2r:llm_authored unit=1", extracted)))
  expect_true(any(grepl("df <- t\\(df\\)", extracted)))
  expect_true(any(grepl("# sas2r:end unit=1", extracted)))
})

test_that("renumber_unit_block updates unit IDs and adds anchors to legacy archives", {
  legacy_block <- c(
    "# sas2r:llm_authored unit=2 -- REVIEW REQUIRED",
    "y <- 2 * x",
    "# sas2r:end unit=2"
  )
  renumbered <- renumber_unit_block(legacy_block, 5L)
  expect_identical(renumbered[1], "# --- sas2r:unit 5 ---")
  expect_identical(renumbered[2], "# sas2r:llm_authored unit=5 -- REVIEW REQUIRED")
  expect_identical(renumbered[4], "# sas2r:end unit=5")
})
