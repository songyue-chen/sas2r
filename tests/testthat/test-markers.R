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
