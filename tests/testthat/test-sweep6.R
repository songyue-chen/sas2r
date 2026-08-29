test_that("macro artifacts refuse NA or empty names", {
  r <- list(code = "f <- function() 1", flags = "llm_authored")
  expect_error(emit_macro_artifacts(r, NA_character_, withr::local_tempdir()),
               class = "sas2r_macro_name_error")
  expect_error(emit_macro_artifacts(r, "", withr::local_tempdir()),
               class = "sas2r_macro_name_error")
})
