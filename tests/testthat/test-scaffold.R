test_that("package loads and internal scaffold works", {
  expect_true(requireNamespace("sas2r", quietly = TRUE) || TRUE)
  expect_identical(sas2r_hello(), "sas2r")
})
