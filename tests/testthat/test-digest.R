test_that("digest carries counts, names, hints -- and passes/fails", {
  b <- tibble::tibble(id = c("1", "2", "3", "4"), aval = c(1, 2, 3, 4))
  cm <- tibble::tibble(id = c("1", "2", "3", "4"), aval = c(2, 3, 4, 5))
  d <- diff_digest(compare_datasets(b, cm, keys = "id"), label = "adsl")
  expect_s3_class(d, "sas2r_diff_digest")
  expect_identical(d$digest_version, "1.1")
  expect_false(d$passed)
  expect_identical(as.character(d$key_names), "id")
  expect_true(any(d$pattern_hints$hint == "CONSTANT_OFFSET"))
})


test_that("case-only and row-delta hints fire", {
  b <- tibble::tibble(id = c("1", "2"), sex = c("M", "F"))
  cm <- tibble::tibble(id = "1", sex = "m")
  d <- diff_digest(compare_datasets(b, cm, keys = "id"))
  expect_true(any(d$pattern_hints$hint == "CASE_ONLY_DIFF"))
  expect_true(any(d$pattern_hints$hint == "ROW_COUNT_DELTA"))
})

test_that("RED TEAM: sentinel values never appear in the digest JSON", {
  b <- tibble::tibble(usubjid = c("SUBJ-SENTINEL-9999", "SUBJ-0002"),
                      cmt = c("PHI_SENTINEL_TEXT", "ok"),
                      aval = c(42.424242, 1))
  cm <- tibble::tibble(usubjid = c("SUBJ-SENTINEL-9999", "SUBJ-0002"),
                       cmt = c("different", "ok"),
                       aval = c(43.434343, 1))
  r <- compare_datasets(b, cm, keys = "usubjid")
  json <- as_digest_json(diff_digest(r, label = "advs"))
  expect_false(grepl("SENTINEL", json, fixed = TRUE))
  expect_false(grepl("PHI_SENTINEL_TEXT", json, fixed = TRUE))
  expect_false(grepl("42.424242", json, fixed = TRUE))
  expect_false(grepl("different", json, fixed = TRUE))
  # names and counts ARE allowed:
  expect_match(json, "usubjid")
  expect_match(json, "cmt")
  # and the local comparison object still has full fidelity:
  expect_true(any(grepl("PHI_SENTINEL_TEXT", r$details$base_value)))
})

test_that("attr-only difference yields ATTR_ONLY and still passes", {
  b <- tibble::tibble(id = "1", aval = 1)
  attr(b$aval, "label") <- "A"
  cm <- tibble::tibble(id = "1", aval = 1)
  attr(cm$aval, "label") <- "B"
  d <- diff_digest(compare_datasets(b, cm, keys = "id"))
  expect_true(d$passed)
  expect_true(any(d$pattern_hints$hint == "ATTR_ONLY"))
})

test_that("na-pattern and padding-only hints fire", {
  # NA_PATTERN_DIFF: all mismatches are na_diff
  b_na <- tibble::tibble(id = c("1", "2"), val = c(1, NA))
  c_na <- tibble::tibble(id = c("1", "2"), val = c(1, 2))
  d_na <- diff_digest(compare_datasets(b_na, c_na, keys = "id"))
  expect_true(any(d_na$pattern_hints$hint == "NA_PATTERN_DIFF"))

  # PADDING_ONLY: 0 value mismatches, cosmetic padding present
  b_pad <- tibble::tibble(id = "1", txt = "abc  ")
  c_pad <- tibble::tibble(id = "1", txt = "abc")
  d_pad <- diff_digest(compare_datasets(b_pad, c_pad, keys = "id"))
  expect_true(d_pad$passed)
  expect_true(any(d_pad$pattern_hints$hint == "PADDING_ONLY"))
})

test_that("duplicate-key hint fires", {
  b_dup <- tibble::tibble(id = c("1", "1"), val = c(10, 20))
  c_dup <- tibble::tibble(id = c("1", "1"), val = c(10, 20))
  d_dup <- diff_digest(compare_datasets(b_dup, c_dup, keys = "id"))
  expect_true(any(d_dup$pattern_hints$hint == "DUPLICATE_KEYS"))
})

test_that("as_digest_json serializes correctly and enforces class", {
  b <- tibble::tibble(id = "1", x = 1)
  cm <- tibble::tibble(id = "1", x = 1)
  d <- diff_digest(compare_datasets(b, cm, keys = "id"))
  json <- as_digest_json(d)
  expect_type(json, "character")
  parsed <- jsonlite::fromJSON(json)
  expect_identical(parsed$digest_version, "1.1")
  expect_identical(parsed$passed, TRUE)
  expect_identical(parsed$key_names, "id")

  expect_error(as_digest_json(list(a = 1)), "inherits\\(digest, \"sas2r_diff_digest\"\\)")
  expect_error(diff_digest(list(a = 1)), "inherits\\(x, \"sas2r_comparison\"\\)")
})

test_that("R12: as_digest_json preserves vector JSON arrays even for single elements", {
  b <- tibble::tibble(id = "1", x = 1)
  d <- diff_digest(compare_datasets(b, b, keys = "id"))
  json <- as_digest_json(d)
  expect_match(json, '"key_names":\\["id"\\]')
})

test_that("R6: UNSUPPORTED_KIND hint fires for unsupported kinds", {
  b <- tibble::tibble(id = 1, lst = list(1:2))
  d <- diff_digest(compare_datasets(b, b, keys = "id"))
  expect_true(any(d$pattern_hints$hint == "UNSUPPORTED_KIND"))
})

test_that("diff_digest works when keys is NULL", {
  b <- tibble::tibble(x = c(1, 2))
  cm <- tibble::tibble(x = c(1, 2))
  d <- diff_digest(compare_datasets(b, cm, keys = NULL))
  expect_identical(as.character(d$key_names), character())
  expect_true(d$passed)
  expect_identical(nrow(d$pattern_hints), 0L)
})


test_that("F6: CONSTANT_OFFSET is computed from full data and is immune to details truncation", {
  # 1200 rows with a constant +1 offset gets CONSTANT_OFFSET
  n <- 1200
  b_const <- tibble::tibble(id = seq_len(n), aval = seq_len(n))
  cm_const <- tibble::tibble(id = seq_len(n), aval = seq_len(n) + 1)
  r_const <- compare_datasets(b_const, cm_const, keys = "id")
  expect_true(attr(r_const$details, "details_truncated"))
  d_const <- diff_digest(r_const)
  expect_true(any(d_const$pattern_hints$hint == "CONSTANT_OFFSET"))

  # 1200 rows: first 1000 constant +1, last 200 random -> NOT CONSTANT_OFFSET
  cm_mixed <- cm_const
  cm_mixed$aval[1001:1200] <- cm_mixed$aval[1001:1200] + seq(5, 1000, length.out = 200)
  r_mixed <- compare_datasets(b_const, cm_mixed, keys = "id")
  d_mixed <- diff_digest(r_mixed)
  expect_false(any(d_mixed$pattern_hints$hint == "CONSTANT_OFFSET"))
})

test_that("F13: CONSTANT_OFFSET respects per-variable overrides", {
  b <- tibble::tibble(id = 1:5, aval = 1:5)
  # offset is 0.5 with slight jitter (sd ~ 1e-4)
  cm <- tibble::tibble(id = 1:5, aval = 1:5 + 0.5 + c(1e-4, -1e-4, 1e-4, -1e-4, 0))
  p_ov <- compare_profile(overrides = list(aval = list(abs = 1e-3)))
  d <- diff_digest(compare_datasets(b, cm, profile = p_ov, keys = "id"))
  expect_true(any(d$pattern_hints$hint == "CONSTANT_OFFSET"))
})

test_that("F15: digest vars contains only allowlisted columns", {
  b <- tibble::tibble(id = 1, x = 1)
  d <- diff_digest(compare_datasets(b, b, keys = "id"))
  expect_false("diff_sd" %in% names(d$vars))
  expect_identical(names(d$vars), DIGEST_VAR_COLS)
})


