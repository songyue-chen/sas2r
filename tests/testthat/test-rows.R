test_that("key matching pairs rows and reports one-sided rows", {
  mk <- match_rows(tibble::tibble(id = c("a", "b")),
                   tibble::tibble(id = c("b", "c")), keys = "id")
  expect_identical(mk$base_idx, 2L)
  expect_identical(mk$comp_idx, 1L)
  expect_identical(mk$only_base, 1L)
  expect_identical(mk$only_comp, 2L)
  expect_false(mk$dup_fanout)
})

test_that("duplicate keys use occurrence-based 1-to-1 pairing and report fanout", {
  base <- tibble::tibble(id = c("01", "02", "02"), x = 1:3)
  comp <- tibble::tibble(id = c("02", "01"), x = c(9L, 1L))
  mk <- match_rows(base, comp, keys = "id")
  expect_true(mk$dup_fanout)
  # base row 1 (id="01", occ 1) matches comp row 2 (id="01", occ 1)
  # base row 2 (id="02", occ 1) matches comp row 1 (id="02", occ 1)
  # base row 3 (id="02", occ 2) has no comp match -> only_base
  expect_identical(mk$base_idx, c(1L, 2L))
  expect_identical(mk$comp_idx, c(2L, 1L))
  expect_identical(mk$only_base, 3L)
  expect_identical(mk$only_comp, integer())
})

test_that("typed key matching handles integer vs double keys and NA keys safely", {
  b <- tibble::tibble(id = c(1e8, 2e8), x = 1:2)
  cm <- tibble::tibble(id = c(100000000L, 200000000L), x = 1:2)
  mk <- match_rows(b, cm, keys = "id")
  expect_identical(mk$base_idx, 1:2)
  expect_identical(mk$comp_idx, 1:2)
  expect_identical(mk$only_base, integer())
  expect_identical(mk$only_comp, integer())

  # NA in key does not collide with literal string "NA"
  b_na <- tibble::tibble(id = c("NA", NA_character_))
  cm_na <- tibble::tibble(id = c(NA_character_, "NA"))
  mk_na <- match_rows(b_na, cm_na, keys = "id")
  expect_identical(mk_na$base_idx, c(1L, 2L))
  expect_identical(mk_na$comp_idx, c(2L, 1L))
})

test_that("missing key columns abort with informative error", {
  expect_error(match_rows(tibble::tibble(a = 1), tibble::tibble(b = 1), keys = "a"),
               "missing in comparison dataset")
  expect_error(match_rows(tibble::tibble(b = 1), tibble::tibble(b = 1), keys = "a"),
               "missing in base dataset")
})

test_that("compound keys use all columns", {
  mk <- match_rows(tibble::tibble(a = "x", b = 1),
                   tibble::tibble(a = "x", b = 2), keys = c("a", "b"))
  expect_identical(length(mk$base_idx), 0L)
})

test_that("NULL keys means row order with overhang", {
  mk <- match_rows(tibble::tibble(x = 1:3), tibble::tibble(x = 1:2), keys = NULL)
  expect_identical(mk$base_idx, 1:2)
  expect_identical(mk$comp_idx, 1:2)
  expect_identical(mk$only_base, 3L)
  expect_identical(mk$only_comp, integer())
})

test_that("NULL keys with comp overhang and equal rows", {
  mk_comp <- match_rows(tibble::tibble(x = 1:2), tibble::tibble(x = 1:4), keys = NULL)
  expect_identical(mk_comp$base_idx, 1:2)
  expect_identical(mk_comp$comp_idx, 1:2)
  expect_identical(mk_comp$only_base, integer())
  expect_identical(mk_comp$only_comp, 3:4)
  expect_false(mk_comp$dup_fanout)

  mk_eq <- match_rows(tibble::tibble(x = 1:2), tibble::tibble(x = 1:2), keys = NULL)
  expect_identical(mk_eq$base_idx, 1:2)
  expect_identical(mk_eq$comp_idx, 1:2)
  expect_identical(mk_eq$only_base, integer())
  expect_identical(mk_eq$only_comp, integer())
  expect_false(mk_eq$dup_fanout)
})

test_that("R5: key column type mismatch aborts with informative error", {
  b <- tibble::tibble(id = c("1", "2"), val = 1:2)
  cm <- tibble::tibble(id = c(1, 2), val = 1:2)
  expect_error(match_rows(b, cm, keys = "id"), "Key column .* has mismatched types")
})



