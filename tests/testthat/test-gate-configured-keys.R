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

  # A content-equal reorder inside the groups still passes. `id` tells the
  # rows of each group apart on both sides, so it refines the configured key.
  target <- assess_with_keys(ref, ref[c(2, 1, 4, 3), ], rules = list(keys = "grp"))
  expect_true(target$reference_passed)
  expect_identical(target$differences$structure$selected_keys, c("grp", "id"))
  expect_identical(target$differences$structure$configured_keys, "grp")

  # With no other key to refine it, the groups are matched by content.
  plain <- ref[c("grp", "val")]
  target <- assess_with_keys(plain, plain[c(2, 1, 4, 3), ], rules = list(keys = "grp"))
  expect_true(target$reference_passed)
  expect_identical(target$differences$structure$selected_keys, "grp")
  expect_identical(target$differences$structure$alignment_method, "duplicate_key_multiset")
})

# Several rows per subject and one sequence number across the dataset.
repeating_key_frame <- function(subjects = 4L, per = 6L) {
  n <- subjects * per
  data.frame(
    usubjid = rep(sprintf("S%02d", seq_len(subjects)), each = per),
    aseq = seq_len(n),
    val = seq_len(n) * 10,
    stringsAsFactors = FALSE
  )
}

comparison_count <- function(cmp, metric) {
  as.integer(cmp$summary$value[cmp$summary$metric == metric])
}

test_that("a repeating configured key is refined by a key that tells a group's rows apart", {
  ref <- repeating_key_frame()
  gen <- transform(ref, val = val + 1)

  cmp <- compare_datasets_aligned(ref, gen, keys = "usubjid")
  st <- cmp$structure
  expect_identical(st$alignment_method, "unique_key")
  expect_identical(st$selected_keys, c("usubjid", "aseq"))
  expect_identical(st$configured_keys, "usubjid")
  expect_identical(cmp$keys, c("usubjid", "aseq"))
  expect_false(st$dup_fanout)
  expect_identical(comparison_count(cmp, "rows_matched"), nrow(ref))
  expect_identical(unique(cmp$details$var), "val")
  expect_identical(nrow(cmp$details), nrow(ref))
  expect_false(cmp$passed)

  # The gate reaches the same pairing with a project present.
  target <- assess_with_keys(ref, gen, rules = list(keys = "usubjid"))
  expect_identical(target$differences$structure$selected_keys, c("usubjid", "aseq"))
  expect_identical(target$differences$structure$alignment_method, "unique_key")
})

test_that("a refined key pairs a group larger than the content-matching limit", {
  ref <- repeating_key_frame(subjects = 2L, per = RESIDUAL_GROUP_LIMIT + 20L)
  gen <- transform(ref, val = val + 1)

  cmp <- compare_datasets_aligned(ref, gen, keys = "usubjid")
  expect_identical(cmp$structure$alignment_resource_state, "complete")
  expect_identical(comparison_count(cmp, "rows_matched"), nrow(ref))
  expect_identical(cmp$structure$rows_only_base, 0L)
  expect_identical(cmp$structure$rows_only_comp, 0L)
  expect_identical(unique(cmp$details$var), "val")
})

test_that("a refined key reports missing and extra rows without content matching", {
  ref <- repeating_key_frame()
  gen <- transform(ref, val = val + 1)
  # One row missing from the first subject, one extra row in the third.
  gen <- rbind(gen[-2L, ], data.frame(usubjid = "S03", aseq = 99, val = 5, stringsAsFactors = FALSE))

  cmp <- compare_datasets_aligned(ref, gen, keys = "usubjid")
  st <- cmp$structure
  expect_identical(st$alignment_method, "unique_key")
  expect_identical(st$selected_keys, c("usubjid", "aseq"))
  expect_identical(comparison_count(cmp, "rows_matched"), nrow(ref) - 1L)
  expect_identical(st$rows_only_base, 1L)
  expect_identical(st$rows_only_comp, 1L)
  expect_identical(unique(cmp$details$var), "val")
})

test_that("a group the two sides disagree on for the refining key is matched by content", {
  ref <- repeating_key_frame()
  second <- ref$usubjid == "S02"
  # Every other subject has a wrong value; the second has its rows renumbered.
  gen <- transform(ref, val = ifelse(second, val, val + 1), aseq = ifelse(second, aseq + 100, aseq))

  context <- empty_alignment_context()
  context$configured_keys <- "usubjid"
  alignment <- align_output_rows(ref, gen, context = context)
  refined <- Filter(function(key) identical(key$provenance, "configured_refined"), alignment$candidate_keys)
  expect_identical(refined[[1L]]$keys, c("usubjid", "aseq"))
  # No pair crosses a configured key value, and no row is left unpaired.
  expect_identical(ref$usubjid[alignment$pairs$reference_row], gen$usubjid[alignment$pairs$candidate_row])
  expect_identical(nrow(alignment$pairs), nrow(ref))
  expect_identical(alignment$method, "duplicate_key_multiset")
  expect_identical(alignment$selected_keys, "usubjid")

  # The renumbered rows are paired with their own content, so the refining
  # column is compared and is the only difference in that subject.
  cmp <- compare_datasets_aligned(ref, gen, keys = "usubjid")
  expect_identical(cmp$keys, "usubjid")
  differing <- table(cmp$details$var)
  expect_setequal(names(differing), c("aseq", "val"))
  expect_identical(as.integer(differing[["aseq"]]), sum(second))
  expect_identical(as.integer(differing[["val"]]), sum(!second))
  expect_identical(comparison_count(cmp, "rows_matched"), nrow(ref))
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
