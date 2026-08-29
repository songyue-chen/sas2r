test_that("col_kind sees through labelled and dates", {
  expect_identical(col_kind(haven::labelled(c(1, 2), c(lo = 1))), "numeric")
  expect_identical(col_kind(as.Date("2020-01-01")), "date")
  expect_identical(col_kind(as.POSIXct("2020-01-01 00:00:01", tz = "UTC")), "datetime")
  expect_identical(col_kind(letters), "character")
})

test_that("normalize_col strips labels and factors, recording notes", {
  n1 <- normalize_col(haven::labelled(c(1, 2), c(lo = 1)))
  expect_identical(n1$x, c(1, 2))
  expect_identical(n1$notes, "zap_labels")
  n2 <- normalize_col(factor(c("a", "b")))
  expect_identical(n2$x, c("a", "b"))
  expect_identical(n2$notes, "factor_to_character")
})

test_that("align_columns is case-insensitive and reports one-sided vars", {
  base <- tibble::tibble(USUBJID = "x", AVAL = 1, extra_b = 1)
  comp <- tibble::tibble(usubjid = "x", aval = "1", extra_c = 2)
  al <- align_columns(base, comp)
  expect_setequal(al$common, c("usubjid", "aval"))
  expect_identical(al$only_base, "extra_b")
  expect_identical(al$only_comp, "extra_c")
  expect_identical(al$kind_mismatch$var, "aval")
  expect_identical(al$kind_mismatch$base_kind, "numeric")
  expect_identical(al$kind_mismatch$comp_kind, "character")
})

test_that("col_kind handles time (hms/difftime) and list columns", {
  expect_identical(col_kind(as.difftime(3600, units = "secs")), "time")
  expect_identical(col_kind(list(1:2, 3:4)), "list")
})

test_that("align_columns aborts on case-folded duplicate column names", {
  b <- tibble::tibble(AVAL = 1, aval = 2)
  expect_error(align_columns(b, tibble::tibble(aval = 1)), "duplicated column names")
  expect_error(align_columns(tibble::tibble(aval = 1), b), "duplicated column names")
})

test_that("N3: normalize_col converts character strings to UTF-8", {
  # Create a latin1 string
  txt_latin1 <- iconv("caf\u00e9", to = "latin1")
  n_enc <- normalize_col(txt_latin1)
  expect_identical(Encoding(n_enc$x), "UTF-8")
  expect_identical(n_enc$x, "caf\u00e9")
  expect_identical(n_enc$notes, "enc2utf8")

  # UTF-8 string needs no conversion note
  n_utf8 <- normalize_col("caf\u00e9")
  expect_identical(n_utf8$notes, character())

  # Q2: bytes-encoded strings do not crash and remain untranslated
  raw_str <- "raw\xff"
  Encoding(raw_str) <- "bytes"
  n_bytes <- normalize_col(raw_str)
  expect_identical(Encoding(n_bytes$x), "bytes")
})


