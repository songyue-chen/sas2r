test_that("numeric equality: NA semantics and combined tolerance", {
  expect_true(num_equal(NA_real_, NA_real_, 1e-8, 1e-8))
  expect_false(num_equal(NA_real_, 1, 1e-8, 1e-8))
  expect_true(num_equal(1.00000000001, 1, 1e-8, 1e-8))
  expect_true(num_equal(1e10 + 50, 1e10, 1e-8, 1e-8))  # rel term scales
  expect_false(num_equal(1.001, 1, 1e-8, 1e-8))
})

test_that("tagged special missings compare by tag on their own axis", {
  a1 <- haven::tagged_na("a"); a2 <- haven::tagged_na("a")
  b1 <- haven::tagged_na("b")
  expect_true(na_tags_match(a1, a2))
  expect_false(na_tags_match(a1, b1))
  expect_false(na_tags_match(a1, NA_real_))
  expect_true(na_tags_match(NA_real_, NA_real_))
  expect_true(num_equal(a1, b1, 1e-8, 1e-8))  # value axis: both NA == equal
})

test_that("character classification covers padding, case, na_diff", {
  cls <- chr_classify(c("Y", "Y  ", "yes", "x", NA, "a"),
                      c("Y", "Y",   "YES", "z", "q", NA))
  expect_identical(cls, c("equal", "padding", "case", "diff", "na_diff", "na_diff"))
})

test_that("infinity handling in num_equal rejects finite vs Inf and sign mismatches", {
  expect_false(num_equal(5, Inf, 1e-8, 1e-8))
  expect_false(num_equal(Inf, 5, 1e-8, 1e-8))
  expect_false(num_equal(0, Inf, 1e-8, 1e-8))
  expect_false(num_equal(-Inf, Inf, 1e-8, 1e-8))
  expect_true(num_equal(Inf, Inf, 1e-8, 1e-8))
  expect_true(num_equal(-Inf, -Inf, 1e-8, 1e-8))
})

test_that("chr_classify distinguishes space padding from tab padding", {
  expect_identical(chr_classify("AB  ", "AB"), "padding")
  expect_identical(chr_classify("AB\t", "AB"), "diff")
})

test_that("chr_classify honors sas_null_equals_na flag", {
  expect_identical(chr_classify("", NA_character_, sas_null_equals_na = TRUE), "equal")
  expect_identical(chr_classify(NA_character_, "", sas_null_equals_na = TRUE), "equal")
  expect_identical(chr_classify("", "", sas_null_equals_na = TRUE), "equal")
  expect_identical(chr_classify("", NA_character_, sas_null_equals_na = FALSE), "na_diff")
  expect_identical(chr_classify(NA_character_, "", sas_null_equals_na = FALSE), "na_diff")
})

test_that("R3: na_tags_match compares integer (untagged) vs double tagged NAs", {
  expect_false(na_tags_match(haven::tagged_na("a"), NA_integer_))
  expect_false(na_tags_match(NA_integer_, haven::tagged_na("a")))
  expect_true(na_tags_match(NA_integer_, NA_real_))
})

test_that("R4: chr_classify treats all-space blanks as equal when sas_null_equals_na = TRUE", {
  expect_identical(chr_classify(" ", "", sas_null_equals_na = TRUE), "equal")
  expect_identical(chr_classify("   ", NA_character_, sas_null_equals_na = TRUE), "equal")
  expect_identical(chr_classify(" ", " ", sas_null_equals_na = TRUE), "equal")
  # Leading spaces with non-blank characters are real differences
  expect_identical(chr_classify(" abc", "abc", sas_null_equals_na = TRUE), "diff")
  expect_identical(chr_classify(" abc", " abc", sas_null_equals_na = TRUE), "equal")
})


test_that("na_equal performs NA-safe equality", {
  expect_true(na_equal(TRUE, TRUE))
  expect_true(na_equal(NA, NA))
  expect_false(na_equal(TRUE, FALSE))
  expect_false(na_equal(TRUE, NA))
})


