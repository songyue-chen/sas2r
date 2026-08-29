test_that("sas_order_key orders ascending numeric values: ._, ., .A-.Z, then nonmissing numbers", {
  x <- c(10, haven::tagged_na("b"), -100, haven::tagged_na("_"), NA_real_, haven::tagged_na("a"), 0)
  ord <- sas_order_key(x, descending = FALSE)
  # Expected order:
  # 1. haven::tagged_na("_") (idx 4)
  # 2. NA_real_ (ordinary .) (idx 5)
  # 3. haven::tagged_na("a") (idx 6)
  # 4. haven::tagged_na("b") (idx 2)
  # 5. -100 (idx 3)
  # 6. 0 (idx 7)
  # 7. 10 (idx 1)
  expect_identical(ord, c(4L, 5L, 6L, 2L, 3L, 7L, 1L))
})

test_that("sas_order_key orders descending numeric values: numbers desc, then .Z-.A, ., ._", {
  x <- c(10, haven::tagged_na("b"), -100, haven::tagged_na("_"), NA_real_, haven::tagged_na("a"), 0)
  ord <- sas_order_key(x, descending = TRUE)
  # Expected descending order:
  # 1. 10 (idx 1)
  # 2. 0 (idx 7)
  # 3. -100 (idx 3)
  # 4. haven::tagged_na("b") (idx 2)
  # 5. haven::tagged_na("a") (idx 6)
  # 6. NA_real_ (idx 5)
  # 7. haven::tagged_na("_") (idx 4)
  expect_identical(ord, c(1L, 7L, 3L, 2L, 6L, 5L, 4L))
})

test_that("sas_order_key orders blank/empty character values before nonblank text ascending", {
  ch <- c("beta", "", "alpha", "   ", NA_character_, "gamma")
  ord <- sas_order_key(ch, descending = FALSE)
  # Blanks are idx 2, 4, 5. Nonblanks are alpha (3), beta (1), gamma (6).
  expect_identical(ord[1:3], c(2L, 4L, 5L))
  expect_identical(ord[4:6], c(3L, 1L, 6L))
})

test_that("sas_order_key orders nonblank text before blanks descending", {
  ch <- c("beta", "", "alpha", "   ", NA_character_, "gamma")
  ord <- sas_order_key(ch, descending = TRUE)
  # Nonblanks descending: gamma (6), beta (1), alpha (3). Blanks: 2, 4, 5.
  expect_identical(ord[1:3], c(6L, 1L, 3L))
  expect_identical(ord[4:6], c(2L, 4L, 5L))
})

test_that("sas_order_key trims trailing padding for character comparisons", {
  ch <- c("apple  ", "apple", "banana")
  ord <- sas_order_key(ch, descending = FALSE)
  expect_identical(ord[1:2], c(1L, 2L))
  expect_identical(ord[3], 3L)
})

test_that("sas_order_key returns unknown_collation when collation cannot be established", {
  ch <- c("alpha", "Beta")
  ord <- sas_order_key(ch, collation = "unknown")
  expect_identical(ord, "unknown_collation")
})

test_that("sas_order_key handles composite multi-column sorting with mixed directions", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    val = c(10, 20, 5, 15)
  )
  ord <- sas_order_key(df, descending = c(FALSE, TRUE))
  # id=1: val=20 (idx 2) then val=10 (idx 1)
  # id=2: val=15 (idx 4) then val=5 (idx 3)
  expect_identical(ord, c(2L, 1L, 4L, 3L))
})

test_that("SAS missing placement is order evidence, not content mismatch", {
  ref <- data.frame(id = c(NA, 1, 2), value = c("m", "a", "b"))
  cand <- ref[c(2, 3, 1), ]
  ctx <- list(by = "id", sort = list(vars = "id", descending = FALSE),
              merge = list(), known_identifiers = "id",
              order_contract = list(vars = "id", descending = FALSE))
  al <- sas2r:::align_output_rows(ref, cand, context = ctx)
  expect_true(al$order$meaningful)
  expect_identical(al$order$reason, "sas_missing_placement")
  expect_true(al$order$content_equivalent)
  expect_false(al$order$order_equivalent)
})

test_that("analyze_output_order classifies same_order, unstable_ties, and genuine_order_difference", {
  # 1. same_order
  ref1 <- data.frame(id = 1:3, v = c("a", "b", "c"))
  cand1 <- ref1
  ctx1 <- list(order_contract = list(vars = "id", descending = FALSE))
  pairs1 <- tibble::tibble(reference_row = 1:3, candidate_row = 1:3)
  res1 <- analyze_output_order(ref1, cand1, pairs1, context = ctx1)
  expect_true(res1$order_equivalent)
  expect_identical(res1$reason, "same_order")

  # 2. unstable_ties
  ref2 <- data.frame(id = c(1, 1, 2), v = c("a", "b", "c"))
  cand2 <- data.frame(id = c(1, 1, 2), v = c("b", "a", "c"))
  ctx2 <- list(order_contract = list(vars = "id", descending = FALSE))
  pairs2 <- tibble::tibble(reference_row = c(1L, 2L, 3L), candidate_row = c(2L, 1L, 3L))
  res2 <- analyze_output_order(ref2, cand2, pairs2, context = ctx2)
  expect_false(res2$order_equivalent)
  expect_true(res2$content_equivalent)
  expect_identical(res2$reason, "unstable_ties")

  # 3. genuine_order_difference
  ref3 <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"))
  cand3 <- data.frame(id = c(3, 1, 2), v = c("c", "a", "b"))
  ctx3 <- list(order_contract = list(vars = "id", descending = FALSE))
  pairs3 <- tibble::tibble(reference_row = c(1L, 2L, 3L), candidate_row = c(2L, 3L, 1L))
  res3 <- analyze_output_order(ref3, cand3, pairs3, context = ctx3)
  expect_false(res3$order_equivalent)
  expect_true(res3$content_equivalent)
  expect_identical(res3$reason, "genuine_order_difference")

  # 4. unknown_collation
  ctx4 <- list(order_contract = list(vars = "v", descending = FALSE), collation = "unknown")
  res4 <- analyze_output_order(ref3, cand3, pairs3, context = ctx4)
  expect_identical(res4$reason, "unknown_collation")
})
