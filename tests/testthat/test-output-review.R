
test_that("only the positions that can survive the ordering are selected", {
  # Examples are chosen with order(severity, variable, reference_row,
  # candidate_row) truncated to max_examples, so within a severity only the
  # lowest-numbered rows can ever be reported.
  severities <- rep(2L, 10L)
  positions <- example_candidate_positions(
    severities, reference_rows = 10:1, candidate_rows = 10:1, max_examples = 3L
  )

  expect_length(positions, 3L)
  expect_setequal(positions, c(8L, 9L, 10L))   # the rows numbered 1, 2, 3
})

test_that("each severity keeps its own allowance", {
  # A later severity can still be reported when an earlier one is scarce, so
  # the cap applies per severity rather than across the whole column.
  severities <- c(1L, 1L, 2L, 2L, 2L)
  positions <- example_candidate_positions(
    severities, reference_rows = c(5L, 6L, 1L, 2L, 3L),
    candidate_rows = c(5L, 6L, 1L, 2L, 3L), max_examples = 2L
  )

  expect_setequal(positions, c(1L, 2L, 3L, 4L))
})

test_that("nothing is dropped when the mismatches fit within the cap", {
  expect_setequal(
    example_candidate_positions(rep(2L, 3L), 1:3, 1:3, max_examples = 20L),
    1:3
  )
})

test_that("cells are formatted for reported examples only, not every mismatch", {
  # This is the defect itself: the comparator used to format both values of
  # every mismatching cell and then keep twenty, so its cost scaled with how
  # wrong the output was rather than with what it reports.
  formatted <- 0L
  testthat::local_mocked_bindings(
    format_cell_value = function(...) {
      formatted <<- formatted + 1L
      "v"
    },
    .package = "sas2r"
  )

  n <- 500L
  reference <- data.frame(id = seq_len(n), value = as.numeric(seq_len(n)),
                          label = paste0("row", seq_len(n)),
                          stringsAsFactors = FALSE)
  candidate <- reference
  candidate$value <- candidate$value + 1
  candidate$label <- paste0(candidate$label, "_x")

  report <- compare_aligned_outputs(
    reference = reference, candidate = candidate,
    target = list(target_id = "t", logical_name = "x", contributing_unit_ids = 1L)
  )

  expect_identical(report$mismatches$total_mismatch_cells, 2L * n)
  # Formatting every mismatching cell would be 2000 calls (1000 cells, two
  # values each). The bound is a few per retained example and per severity
  # class, so anything in the low hundreds is bounded and anything approaching
  # the cell count is not.
  expect_lt(formatted, 500L)
})


