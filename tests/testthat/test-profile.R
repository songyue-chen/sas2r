test_that("defaults encode the accepted tolerance rules", {
  p <- compare_profile()
  expect_s3_class(p, "sas2r_profile")
  expect_identical(p$numeric, list(abs = 1e-8, rel = 1e-8))
  expect_true(p$sas_null_equals_na)
  expect_identical(p$na_tags, "report")
  expect_identical(p$padding, "cosmetic")
  expect_identical(p$attrs_cosmetic, c("label", "format.sas"))
  expect_identical(p$version, "1.1")
})

test_that("per-variable overrides and keys are stored, names lowercased", {
  p <- compare_profile(keys = c("USUBJID", "PARAMCD"),
                       overrides = list(AVAL = list(abs = 1e-4)))
  expect_identical(p$keys, c("usubjid", "paramcd"))
  expect_identical(p$overrides$aval$abs, 1e-4)
})

test_that("R2: invalid override parameters and values are rejected", {
  expect_error(compare_profile(overrides = list(aval = list(abs = NULL))), "non-negative")
  expect_error(compare_profile(overrides = list(aval = list(abs = -1))), "non-negative")
  expect_error(compare_profile(overrides = list(aval = list(abs = "big"))), "non-negative")
  expect_error(compare_profile(overrides = list(aval = list(tol = 1e-4))), "Invalid override")
  expect_error(compare_profile(overrides = list(aval = "not a list")), "must be a list")
})

test_that("invalid enum values are rejected", {
  expect_error(compare_profile(na_tags = "maybe"), "na_tags")
  expect_error(compare_profile(padding = "sometimes"), "padding")
})

test_that("print.sas2r_profile prints cleanly and returns profile invisibly", {
  p <- compare_profile(keys = "usubjid", overrides = list(aval = list(abs = 1e-4)))
  out <- capture.output(res <- expect_invisible(print(p)), type = "message")
  expect_true(any(grepl("sas2r comparison profile", out)))
  expect_true(any(grepl("overrides: aval", out)))
  expect_true(any(grepl("keys: usubjid", out)))
  expect_identical(res, p)
})

test_that("invalid numeric tolerance values and flags are rejected", {
  expect_error(compare_profile(abs = -1), "non-negative")
  expect_error(compare_profile(rel = -1e-5), "non-negative")
  expect_error(compare_profile(abs = "abc"), "non-negative")
  expect_error(compare_profile(sas_null_equals_na = "yes"), "TRUE or FALSE")
})

test_that("effective_tol merges overrides cleanly", {
  p <- compare_profile(abs = 1e-8, rel = 1e-8,
                       overrides = list(aval = list(abs = 1e-3)))
  expect_identical(effective_tol(p, "AVAL"), list(abs = 1e-3, rel = 1e-8))
  expect_identical(effective_tol(p, "other"), list(abs = 1e-8, rel = 1e-8))
})

test_that("string representations of numeric tolerances are parsed cleanly", {
  p <- compare_profile(abs = "1e-6", rel = "1e-5")
  expect_identical(p$numeric$abs, 1e-6)
  expect_identical(p$numeric$rel, 1e-5)
})


