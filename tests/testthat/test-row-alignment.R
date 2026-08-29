test_that("a code-derived unique composite key aligns reordered rows", {
  ref <- data.frame(usubjid = c("01", "02"), avisitn = c(1, 1), aval = c(5, 8))
  cand <- ref[c(2, 1), ]
  ctx <- list(by = c("usubjid", "avisitn"), sort = list(), merge = list(),
              known_identifiers = "usubjid", order_contract = NULL)
  al <- align_output_rows(ref, cand, context = ctx)
  expect_identical(al$method, "unique_key")
  expect_identical(al$selected_keys, c("usubjid", "avisitn"))
  expect_equal(al$pairs$reference_row, c(1L, 2L))
  expect_equal(al$pairs$candidate_row, c(2L, 1L))
  expect_true(al$key_unique_reference)
  expect_true(al$key_unique_candidate)
  expect_length(al$only_reference, 0L)
  expect_length(al$only_candidate, 0L)
})

test_that("a key must exist and be unique on both sides for unique_key method", {
  ref <- data.frame(id = c(1, 1), value = c("a", "b"))
  cand <- ref[c(2, 1), ]
  al <- align_output_rows(
    ref, cand,
    context = list(by = "id", sort = list(), merge = list(),
                   known_identifiers = "id", order_contract = NULL)
  )
  expect_false(al$key_unique_reference)
  expect_false(al$key_unique_candidate)
  expect_false(al$method == "unique_key")
})

test_that("duplicate-key reordering is removed as an exact multiset", {
  ref <- data.frame(id = c(1, 1, 2), value = c("a", "b", "c"))
  cand <- ref[c(2, 1, 3), ]
  al <- sas2r:::align_output_rows(
    ref, cand,
    context = list(by = "id", sort = list(), merge = list(),
                   known_identifiers = "id", order_contract = NULL)
  )
  expect_identical(al$method, "duplicate_key_multiset")
  expect_length(al$only_reference, 0L)
  expect_length(al$only_candidate, 0L)
  expect_equal(nrow(al$ambiguous_groups), 0L)
  expect_equal(nrow(al$pairs), 3L)
})

test_that("keyless reordered equal rows compare by whole-frame multiset", {
  ref <- data.frame(x = c(1, 2), y = c("a", "b"))
  cand <- ref[c(2, 1), ]
  al <- sas2r:::align_output_rows(ref, cand, context = empty_alignment_context())
  expect_identical(al$method, "keyless_multiset")
  expect_equal(nrow(al$pairs), 2L)
})

test_that("duplicate-key with one changed duplicate matches least-cost residual pair", {
  ref <- data.frame(id = c(1, 1), code = c("x", "y"), val = c(10, 20))
  cand <- data.frame(id = c(1, 1), code = c("x", "y"), val = c(10, 99))
  ctx <- list(by = "id", sort = list(), merge = list(),
              known_identifiers = "id", order_contract = NULL)
  al <- align_output_rows(ref, cand, context = ctx)
  expect_identical(al$method, "duplicate_key_multiset")
  expect_equal(nrow(al$pairs), 2L)
  # Row 1 (x, 10) matched exact; row 2 (y, 20) matched residual (y, 99)
  expect_equal(al$pairs$reference_row, c(1L, 2L))
  expect_equal(al$pairs$candidate_row, c(1L, 2L))
  expect_length(al$only_reference, 0L)
  expect_length(al$only_candidate, 0L)
})

test_that("duplicate-key with ambiguous equal-cost alternatives records ambiguous_groups", {
  ref <- data.frame(id = c(1, 1), code = c("a", "b"), val = c(10, 10))
  cand <- data.frame(id = c(1, 1), code = c("x", "x"), val = c(10, 10))
  ctx <- list(by = "id", sort = list(), merge = list(),
              known_identifiers = "id", order_contract = NULL)
  al <- align_output_rows(ref, cand, context = ctx)
  expect_identical(al$method, "duplicate_key_multiset")
  expect_true(nrow(al$ambiguous_groups) > 0L)
})

test_that("large duplicate groups exceeding cap return resource_state = 'residual_group_limit'", {
  n <- 120
  ref <- data.frame(id = rep(1, n), val = paste0("ref_", seq_len(n)))
  cand <- data.frame(id = rep(1, n), val = paste0("cand_", seq_len(n)))
  ctx <- list(by = "id", sort = list(), merge = list(),
              known_identifiers = "id", order_contract = NULL)
  al <- align_output_rows(ref, cand, context = ctx)
  expect_identical(al$resource_state, "residual_group_limit")
})

test_that("unmatched rows on reference and candidate are preserved in only_reference and only_candidate", {
  ref <- data.frame(id = c("01", "02", "03"), val = c(10, 20, 30))
  cand <- data.frame(id = c("02", "04"), val = c(20, 40))
  ctx <- list(by = "id", sort = list(), merge = list(),
              known_identifiers = "id", order_contract = NULL)
  al <- align_output_rows(ref, cand, context = ctx)
  expect_identical(al$method, "unique_key")
  expect_identical(al$selected_keys, "id")
  expect_equal(al$pairs$reference_row, 2L)
  expect_equal(al$pairs$candidate_row, 1L)
  expect_equal(al$only_reference, c(1L, 3L))
  expect_equal(al$only_candidate, 2L)
})

test_that("source contracts take precedence over heuristics", {
  ref <- data.frame(usubjid = c("01", "02"), avisitn = c(1, 2), aval = c(10, 20))
  cand <- ref[c(2, 1), ]
  ctx <- list(by = c("usubjid", "avisitn"), sort = list(), merge = list(),
              known_identifiers = "usubjid", order_contract = NULL)
  al <- align_output_rows(ref, cand, context = ctx)
  expect_identical(al$selected_keys, c("usubjid", "avisitn"))
  expect_identical(al$candidate_keys[[1]]$provenance, "by")
})

test_that("infer_alignment_keys creates ordered candidate list from sort, merge, lineage, and heuristics", {
  ref <- data.frame(studyid = "STUDY1", usubjid = c("01", "02"), aeseq = c(1, 2), val = c(1, 2))
  cand <- ref
  ctx <- list(
    by = character(),
    sort = list(vars = c("usubjid", "aeseq"), descending = c(FALSE, FALSE)),
    merge = list(by = "usubjid"),
    lineage = list(keys = "studyid"),
    known_identifiers = c("usubjid", "studyid")
  )
  candidates <- infer_alignment_keys(ref, cand, context = ctx)
  expect_true(length(candidates) >= 3)
  provenances <- vapply(candidates, `[[`, character(1), "provenance")
  expect_true("sort" %in% provenances)
  expect_true("merge" %in% provenances)
  expect_true("lineage" %in% provenances)
  expect_identical(candidates[[1]]$provenance, "sort")
  expect_identical(candidates[[1]]$keys, c("usubjid", "aeseq"))
})

test_that("validate_alignment_key verifies columns, type compatibility, and uniqueness", {
  ref <- data.frame(id = c("1", "2"), val = 1:2)
  cand <- data.frame(id = c("1", "2", "2"), val = 1:3)

  v <- validate_alignment_key(ref, cand, "id")
  expect_true(v$unique_reference)
  expect_false(v$unique_candidate)
  expect_false(v$valid)

  v_missing <- validate_alignment_key(ref, data.frame(other = 1:2), "id")
  expect_false(v_missing$valid)
  expect_false(v_missing$unique_candidate)
})

test_that("normalize_output_frame case-folds, trims padding, and preserves tagged NAs", {
  tag_a <- haven::tagged_na("a")
  ref <- data.frame(
    USUBJID = c("01  ", "02"),
    AVAL = c(10, tag_a),
    STR = factor(c("high", "low")),
    stringsAsFactors = FALSE
  )
  norm <- normalize_output_frame(ref)
  expect_identical(names(norm), c("usubjid", "aval", "str"))
  expect_identical(norm$usubjid, c("01", "02"))
  expect_identical(norm$str, c("high", "low"))
  expect_true(haven::is_tagged_na(norm$aval[2]))
  expect_identical(haven::na_tag(norm$aval[2]), "a")
})

test_that("normalize_output_frame aborts on duplicate case-folded column names", {
  df <- data.frame(A = 1, a = 2)
  expect_error(normalize_output_frame(df), "duplicated column names")
})

test_that("print.sas2r_row_alignment reports structure without printing data cell values", {
  ref <- data.frame(usubjid = c("01", "02"), aval = c(5, 8))
  cand <- ref[c(2, 1), ]
  al <- align_output_rows(ref, cand, context = list(by = "usubjid"))
  out <- capture.output(print(al), type = "message")
  out_txt <- paste(out, collapse = "\n")
  expect_true(grepl("unique_key", out_txt))
  expect_true(grepl("usubjid", out_txt))
  expect_false(grepl("01", out_txt))
  expect_false(grepl("02", out_txt))
})

test_that("empty frames align safely", {
  ref <- data.frame(id = character(), val = numeric())
  cand <- data.frame(id = character(), val = numeric())
  al <- align_output_rows(ref, cand, context = list(by = "id"))
  expect_s3_class(al, "sas2r_row_alignment")
  expect_identical(al$method, "unique_key")
  expect_equal(nrow(al$pairs), 0L)
  expect_length(al$only_reference, 0L)
  expect_length(al$only_candidate, 0L)
})
