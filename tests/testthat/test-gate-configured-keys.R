# Configured comparison keys are the row identity the gate pairs on. All data
# here is made up.

configured_key_frames <- function() {
  ref <- data.frame(
    rowkey = c("a", "b", "c", "d"),
    id = c(1, 2, 3, 4),
    val = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  gen <- data.frame(
    rowkey = c("a", "b", "c", "d"),
    id = c(4, 3, 2, 1),
    val = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  list(ref = ref, gen = gen)
}

# A project whose source states no BY, SORT or MERGE for the target.
silent_source_project <- function() {
  list(statements = data.frame(
    unit_id = c(1L, 1L),
    text = c("data out", "set a"),
    first_token = c("data", "set"),
    type = "code",
    stringsAsFactors = FALSE
  ))
}

assess_with_keys <- function(ref, gen, rules = list(), assertions = NULL,
                             project = silent_source_project()) {
  ref_path <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  saveRDS(ref, ref_path)
  contract <- list(
    target_id = "t1", target_key = "out", logical_name = "out",
    kind = "dataset", required = TRUE, resolution = "static"
  )
  if (!is.null(assertions)) contract$assertions <- assertions
  rules$references <- list(out = ref_path)
  assess_dataset_target(contract, gen, comparison_rules = rules, project = project)
}

test_that("the gate pairs rows on configured keys when a project is present", {
  fr <- configured_key_frames()

  # Without a context the configured key was already honoured.
  direct <- compare_datasets_aligned(fr$ref, fr$gen, keys = "rowkey")
  expect_identical(direct$structure$selected_keys, "rowkey")

  target <- assess_with_keys(fr$ref, fr$gen, rules = list(keys = "rowkey"))
  st <- target$differences$structure
  expect_identical(st$selected_keys, "rowkey")
  expect_identical(st$alignment_method, "unique_key")
  expect_identical(st$configured_keys, "rowkey")
  # Paired on rowkey, the differing column is id, never rowkey.
  expect_identical(unique(target$differences$mismatches$var), "id")
  expect_identical(as.character(target$differences$digest$key_names), "rowkey")
})

test_that("keys configured for one target steer that target", {
  fr <- configured_key_frames()
  target <- assess_with_keys(fr$ref, fr$gen, assertions = list(keys = "rowkey"))
  expect_identical(target$differences$structure$selected_keys, "rowkey")
})

test_that("with no configured keys the gate still infers them", {
  fr <- configured_key_frames()
  target <- assess_with_keys(fr$ref, fr$gen)
  expect_identical(target$differences$structure$selected_keys, "id")
  expect_identical(target$differences$structure$configured_keys, character())
})

test_that("configured keys that repeat pair rows inside each key group", {
  # `grp` repeats; `id` is unique and would win key inference, pairing rows
  # across different `grp` values.
  ref <- data.frame(grp = c("x", "x", "y", "y"), id = c(1, 2, 3, 4),
                    val = c(10, 20, 30, 40), stringsAsFactors = FALSE)
  gen <- data.frame(grp = c("x", "x", "y", "y"), id = c(3, 4, 1, 2),
                    val = c(10, 20, 30, 40), stringsAsFactors = FALSE)

  for (project in list(silent_source_project(), NULL)) {
    target <- assess_with_keys(ref, gen, rules = list(keys = "grp"), project = project)
    st <- target$differences$structure
    expect_identical(st$selected_keys, "grp")
    expect_identical(st$alignment_method, "duplicate_key_multiset")
    expect_true(st$dup_fanout)
    expect_false(target$reference_passed)
    expect_identical(unique(target$differences$mismatches$var), "id")
  }

  # A content-equal reorder inside the groups still passes.
  target <- assess_with_keys(ref, ref[c(2, 1, 4, 3), ], rules = list(keys = "grp"))
  expect_true(target$reference_passed)
  expect_identical(target$differences$structure$selected_keys, "grp")
})

test_that("configured keys do not replace the source order contract", {
  fr <- configured_key_frames()
  gen <- fr$ref[c(4, 3, 2, 1), ]
  ctx <- empty_alignment_context()
  ctx$sort <- list(vars = "id", descending = FALSE)
  ctx$known_identifiers <- "id"

  with_keys <- compare_datasets_aligned(fr$ref, gen, keys = "rowkey", context = ctx)
  without <- compare_datasets_aligned(fr$ref, gen, context = ctx)
  expect_identical(with_keys$structure$selected_keys, "rowkey")
  expect_identical(without$structure$selected_keys, "id")
  expect_identical(with_keys$structure$order, without$structure$order)
  expect_identical(with_keys$passed, without$passed)
})

test_that("the record names the keys used when configured keys cannot pair rows", {
  # The key column has a different type on each side, so it cannot be a key.
  ref <- data.frame(rowkey = c("1", "2", "3"), id = c(1, 2, 3), stringsAsFactors = FALSE)
  gen <- data.frame(rowkey = c(1, 2, 3), id = c(1, 2, 3))
  cmp <- compare_datasets_aligned(ref, gen, keys = "rowkey")
  expect_identical(cmp$structure$configured_keys, "rowkey")
  expect_identical(cmp$structure$selected_keys, "id")
  expect_identical(cmp$keys, "id")
  expect_false(cmp$passed)
})

test_that("compare_datasets() keeps its own key handling and record", {
  fr <- configured_key_frames()
  cmp <- compare_datasets(fr$ref, fr$gen, keys = "rowkey")
  expect_identical(cmp$keys, "rowkey")
  expect_null(cmp$structure$selected_keys)
  expect_null(cmp$structure$configured_keys)
  expect_identical(unique(cmp$details$var), "id")
})
