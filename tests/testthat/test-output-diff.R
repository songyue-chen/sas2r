test_that("diff_aligned_cells detects numeric differences under absolute and relative tolerances", {
  ref <- data.frame(id = 1:5, val = c(10.0, 20.0, 30.0, 40.0, 50.0))
  cand <- data.frame(id = 1:5, val = c(10.0000001, 20.5, 30.0, 40.0, 55.0))
  pairs <- tibble::tibble(reference_row = 1:5, candidate_row = 1:5)

  prof <- compare_profile(abs = 1e-5, rel = 0)
  diff_res <- sas2r:::diff_aligned_cells(ref, cand, pairs, profile = prof)

  expect_equal(diff_res$total_mismatches, 2L)
  expect_equal(nrow(diff_res$examples), 2L)
  expect_identical(diff_res$examples$reference_row, c(2L, 5L))
})

test_that("diff_aligned_cells classifies character differences into padding, case, na_diff, and value", {
  ref <- data.frame(
    id = 1:4,
    x = c("abc ", "ABC", NA, "hello"),
    stringsAsFactors = FALSE
  )
  cand <- data.frame(
    id = 1:4,
    x = c("abc", "abc", "world", "HELLO"),
    stringsAsFactors = FALSE
  )
  pairs <- tibble::tibble(reference_row = 1:4, candidate_row = 1:4)

  # Default profile: padding is cosmetic, case is mismatch
  prof <- compare_profile()
  diff_res <- sas2r:::diff_aligned_cells(ref, cand, pairs, profile = prof)

  # id 1: padding (cosmetic)
  # id 2: case (mismatch)
  # id 3: na_diff (mismatch)
  # id 4: case (mismatch)
  expect_equal(diff_res$total_mismatches, 3L)
  expect_equal(nrow(diff_res$cosmetic), 1L)
})

test_that("summarize_column_differences identifies common, missing, and mismatched column kinds", {
  ref <- data.frame(a = 1:3, b = c("x", "y", "z"), c = as.Date("2020-01-01") + 0:2)
  cand <- data.frame(a = 1:3, b = 10:12, d = c("u", "v", "w"))

  summary <- sas2r:::summarize_column_differences(ref, cand)
  expect_identical(summary$columns_common, c("a", "b"))
  expect_identical(summary$columns_only_reference, "c")
  expect_identical(summary$columns_only_candidate, "d")
  expect_equal(nrow(summary$column_kind_mismatches), 1L)
  expect_identical(summary$column_kind_mismatches$var, "b")
})

test_that("sas2r_comparison_report print and as.data.frame methods format cleanly", {
  ref <- data.frame(id = 1:5, x = 1:5)
  cand <- data.frame(id = 1:5, x = c(1L, 2L, 99L, 4L, 5L))
  target <- output_target_fixture()
  report <- sas2r:::compare_aligned_outputs(ref, cand, target, context = alignment_context("id"))

  expect_output(print(report), "sas2r output comparison report")
  df <- as.data.frame(report)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_identical(df$variable, "x")
})

test_that("column matching case-folds exactly as align_columns does", {
  # One dataset spells the key upper case, the other lower. The two comparison
  # paths must not disagree about whether that is one column or two.
  ref <- data.frame(USUBJID = c("01", "02"), AVAL = c(1, 2), stringsAsFactors = FALSE)
  cand <- data.frame(usubjid = c("01", "02"), aval = c(1, 2), stringsAsFactors = FALSE)

  old_path <- sas2r:::align_columns(ref, cand)
  new_path <- sas2r:::summarize_column_differences(ref, cand)

  expect_identical(new_path$columns_common, old_path$common)
  expect_identical(new_path$columns_only_reference, old_path$only_base)
  expect_identical(new_path$columns_only_candidate, old_path$only_comp)

  expect_identical(new_path$columns_common, c("usubjid", "aval"))
  expect_length(new_path$columns_only_reference, 0L)
  expect_length(new_path$columns_only_candidate, 0L)
})

test_that("cell differences are found across a case-folded column pair", {
  ref <- data.frame(USUBJID = c("01", "02"), AVAL = c(1, 2), stringsAsFactors = FALSE)
  cand <- data.frame(usubjid = c("01", "02"), aval = c(1, 99), stringsAsFactors = FALSE)
  pairs <- tibble::tibble(reference_row = 1:2, candidate_row = 1:2)

  diff_res <- sas2r:::diff_aligned_cells(ref, cand, pairs)

  expect_equal(diff_res$total_mismatches, 1L)
  expect_identical(diff_res$examples$variable, "aval")
  expect_identical(sort(diff_res$vars$var), c("aval", "usubjid"))
})

test_that("comparison normalizes each frame exactly once", {
  ref <- data.frame(usubjid = sprintf("%03d", 1:6), aval = as.numeric(1:6),
                    stringsAsFactors = FALSE)
  cand <- ref
  cand$aval[3] <- 99
  target <- output_target_fixture()

  real_normalize <- sas2r:::normalize_output_frame
  calls <- 0L
  testthat::local_mocked_bindings(
    normalize_output_frame = function(...) {
      calls <<- calls + 1L
      real_normalize(...)
    },
    .package = "sas2r"
  )

  report <- compare_aligned_outputs(ref, cand, target)

  # A key was selected, so validate_alignment_key() really did run.
  expect_identical(report$alignment$selected_keys, "usubjid")
  # One pass per frame for the whole comparison -- not one more in
  # align_output_rows() and two more per candidate key underneath it.
  expect_equal(calls, 2L)
})

test_that("bounded report caps every model-visible field and flags truncation", {
  n_common <- 600L
  n_side <- 150L
  long <- paste(rep("Z", 500L), collapse = "")

  mk <- function(prefix, n, value) {
    stats::setNames(
      lapply(seq_len(n), function(i) rep(value, 3L)),
      sprintf("%s%04d", prefix, seq_len(n))
    )
  }

  common <- mk("v", n_common, "same")
  ref <- tibble::as_tibble(c(
    list(usubjid = paste0(long, c("01", "02", "03"))),
    common,
    mk("ronly", n_side, "r")
  ))
  cand_common <- common
  # Exactly one differing column, so total_mismatches stays under the example
  # cap and `truncated` can only come from the new content caps.
  cand_common[["v0001"]] <- paste0(long, c("a", "b", "c"))
  cand <- tibble::as_tibble(c(
    list(usubjid = paste0(long, c("01", "02", "03"))),
    cand_common,
    mk("conly", n_side, "c")
  ))

  # Thirty declared identifiers push the evaluated candidate-key list past its
  # own cap without touching any other field.
  ctx <- sas2r:::alignment_context(
    by = "usubjid",
    known_identifiers = c("usubjid", sprintf("v%04d", 1:30))
  )
  target <- output_target_fixture()

  report <- compare_aligned_outputs(ref, cand, target, context = ctx)

  expect_true(isTRUE(report$truncated))
  expect_lte(length(report$structure$columns_common),
             sas2r:::OUTPUT_REPORT_MAX_COLUMN_NAMES)
  expect_lte(length(report$structure$columns_only_reference),
             sas2r:::OUTPUT_REPORT_MAX_COLUMN_NAMES)
  expect_lte(length(report$structure$columns_only_candidate),
             sas2r:::OUTPUT_REPORT_MAX_COLUMN_NAMES)
  expect_lte(length(report$alignment$candidate_keys),
             sas2r:::OUTPUT_REPORT_MAX_CANDIDATE_KEYS)
  expect_lte(nrow(report$mismatches$by_variable),
             sas2r:::OUTPUT_REPORT_MAX_VARIABLE_ROWS)
  expect_true(all(nchar(report$examples$reference_value) <=
                    sas2r:::OUTPUT_REPORT_MAX_VALUE_CHARS))
  expect_true(all(nchar(report$examples$candidate_value) <=
                    sas2r:::OUTPUT_REPORT_MAX_VALUE_CHARS))
  expect_true(all(nchar(report$examples$key_values) <=
                    sas2r:::OUTPUT_REPORT_MAX_KEY_CHARS))

  # The whole object, as it is serialized across the model boundary.
  expect_lte(sas2r:::comparison_report_serialized_bytes(report),
             sas2r:::OUTPUT_REPORT_MAX_SERIALIZED_BYTES)
})

test_that("a report that fits every cap is not flagged truncated", {
  ref <- data.frame(usubjid = c("01", "02", "03"), aval = c(1, 2, 3),
                    stringsAsFactors = FALSE)
  cand <- data.frame(usubjid = c("01", "02", "03"), aval = c(1, 2, 4),
                     stringsAsFactors = FALSE)
  report <- compare_aligned_outputs(ref, cand, output_target_fixture())

  expect_false(isTRUE(report$truncated))
  expect_identical(report$structure$columns_common, c("usubjid", "aval"))
})

test_that("a clipped cell value alone flags the report truncated", {
  long <- paste(rep("Q", 400L), collapse = "")
  ref <- data.frame(usubjid = c("01", "02"), note = c(long, "short"),
                    stringsAsFactors = FALSE)
  cand <- data.frame(usubjid = c("01", "02"), note = c(paste0(long, "X"), "short"),
                     stringsAsFactors = FALSE)

  report <- compare_aligned_outputs(ref, cand, output_target_fixture())

  expect_equal(report$mismatches$total_mismatch_cells, 1L)
  expect_true(all(nchar(report$examples$reference_value) <=
                    sas2r:::OUTPUT_REPORT_MAX_VALUE_CHARS))
  # One cap fired, so the report must not describe itself as complete.
  expect_true(isTRUE(report$truncated))
})
