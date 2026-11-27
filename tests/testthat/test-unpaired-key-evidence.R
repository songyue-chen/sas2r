# Rows left unpaired on both sides of a key: the key column that, taken alone,
# keeps them apart. All data here is made up.

key_records <- function(cmp) cmp$structure$unpaired_keys
metric <- function(cmp, name) cmp$summary$value[cmp$summary$metric == name]
record <- function(key, pairs, reference = 0L, generated = 0L) {
  list(key = key, pair_without = as.integer(pairs), missing_reference = as.integer(reference),
    missing_generated = as.integer(generated))
}
# Six rows keyed on three columns, every key value present.
six <- function() data.frame(id = 1:6, param = "PAIN", visit = 1, value = 1, stringsAsFactors = FALSE)
three_keys <- c("id", "param", "visit")

test_that("a key that is missing on one side and a value on the other is named with its counts", {
  reference <- data.frame(id = seq_len(177L), visit = c(rep(NA_real_, 60L), rep(1, 117L)))
  generated <- reference
  generated$visit[is.na(generated$visit)] <- 0
  cmp <- compare_datasets_aligned(reference, generated, keys = c("id", "visit"))

  expect_identical(key_records(cmp), list(record("visit", 60L, reference = 60L, generated = 0L)))
  # The comparison itself is as it was: the keys, the pairs and the verdict.
  expect_identical(cmp$structure$selected_keys, c("id", "visit"))
  expect_identical(cmp$structure$alignment_method, "unique_key")
  expect_identical(metric(cmp, "rows_matched"), 117L)
  expect_identical(c(cmp$structure$rows_only_base, cmp$structure$rows_only_comp), c(60L, 60L))
  expect_false(cmp$passed)

  # Missing on the generated side instead.
  swapped <- compare_datasets_aligned(generated, reference, keys = c("id", "visit"))
  expect_identical(key_records(swapped), list(record("visit", 60L, reference = 0L, generated = 60L)))
})

test_that("a key that holds different values is named, with nothing missing", {
  reference <- six()
  number <- transform(reference, visit = ifelse(id <= 4L, 9, visit))
  expect_identical(key_records(compare_datasets_aligned(reference, number, keys = three_keys)),
    list(record("visit", 4L)))
  letters_only <- reference
  letters_only$param[1:2] <- "Pain"
  expect_identical(key_records(compare_datasets_aligned(reference, letters_only, keys = three_keys)),
    list(record("param", 2L)))
})

test_that("missing means what it means in the comparison", {
  reference <- data.frame(id = 1:4, grp = c("", "", "x", "y"), value = 1, stringsAsFactors = FALSE)
  generated <- transform(reference, grp = c("z", "z", "x", "y"))
  # A blank character value is a SAS missing value under the default profile.
  expect_identical(key_records(compare_datasets_aligned(reference, generated, keys = c("id", "grp"))),
    list(record("grp", 2L, reference = 2L)))
  # Under a profile that keeps a blank apart from a missing value, it is a value.
  strict <- compare_profile(sas_null_equals_na = FALSE)
  expect_identical(key_records(compare_datasets_aligned(reference, generated, profile = strict,
    keys = c("id", "grp"))), list(record("grp", 2L)))
})

test_that("each column is judged alone and the counts are not added up", {
  reference <- six()
  # Two rows differ on param and three other rows on visit.
  generated <- reference
  generated$param[1:2] <- "ITCH"
  generated$visit[4:6] <- 9
  cmp <- compare_datasets_aligned(reference, generated, keys = three_keys)
  expect_identical(c(cmp$structure$rows_only_base, cmp$structure$rows_only_comp), c(5L, 5L))
  expect_identical(key_records(cmp), list(record("param", 2L), record("visit", 3L)))
})

test_that("no column is named where one column alone does not explain a pair", {
  # Two key columns differ together: leaving one out leaves the other apart.
  reference <- six()
  generated <- reference
  generated$param[1:3] <- "ITCH"
  generated$visit[1:3] <- 9
  together <- compare_datasets_aligned(reference, generated, keys = three_keys)
  expect_identical(together$structure$rows_only_base, 3L)
  expect_null(key_records(together))

  # Leaving the column out leaves a repeated key: two unpaired rows per
  # subject on each side, and nothing says which belongs to which.
  visits <- data.frame(id = rep(1:3, each = 2L), visit = rep(c(1, 2), 3L), value = 1)
  repeated <- compare_datasets_aligned(visits, transform(visits, visit = visit + 10), keys = c("id", "visit"))
  expect_identical(repeated$structure$rows_only_base, 6L)
  expect_null(key_records(repeated))

  # One repeat is enough to leave the column unnamed, although five other
  # pairs are clear. An accepted limit of judging the column as a whole.
  reference <- data.frame(id = c(1:5, 5L), visit = c(rep(NA, 5L), 7), value = 1)
  generated <- data.frame(id = c(1:5, 5L), visit = c(rep(0, 5L), 8), value = 1)
  stray <- compare_datasets_aligned(reference, generated, keys = c("id", "visit"))
  expect_identical(stray$structure$alignment_method, "unique_key")
  expect_identical(stray$structure$rows_only_base, 6L)
  expect_null(key_records(stray))
})

test_that("nothing is recorded where the question does not arise", {
  reference <- six()
  # Every row paired.
  expect_null(key_records(compare_datasets_aligned(reference, reference, keys = three_keys)))
  # Rows left over on one side only are missing or extra rows.
  expect_null(key_records(compare_datasets_aligned(reference, reference[-(1:2), ], keys = three_keys)))
  expect_null(key_records(compare_datasets_aligned(reference[-(1:2), ], reference, keys = three_keys)))
  # One key column: leaving it out leaves nothing to pair on.
  expect_null(key_records(compare_datasets_aligned(reference, transform(reference, id = id + 100L), keys = "id")))
})

test_that("a key that repeats is judged the same way once matching ran to its end", {
  # Subject 1 has two rows under one key, so rows are matched inside key groups.
  reference <- data.frame(id = c(1L, 1L, 2L, 3L), grp = c("a", "a", "a", "b"), value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE)
  generated <- transform(reference, grp = c("a", "a", "a", "c"))
  cmp <- compare_datasets_aligned(reference, generated, keys = c("id", "grp"))
  expect_identical(cmp$structure$alignment_method, "duplicate_key_multiset")
  expect_identical(cmp$structure$alignment_resource_state, "complete")
  expect_identical(c(cmp$structure$rows_only_base, cmp$structure$rows_only_comp), c(1L, 1L))
  expect_identical(key_records(cmp), list(record("grp", 1L)))

  # A pair counts only when it differs on the column left out. Here row 3 is
  # left over on both sides with the same key, and row 4 differs on `grp`.
  ref_norm <- normalize_output_frame(reference)
  cand_norm <- normalize_output_frame(generated)
  left_over <- list(method = "duplicate_key_multiset", selected_keys = c("id", "grp"),
    only_reference = 3:4, only_candidate = 3:4, resource_state = "complete")
  expect_identical(unpaired_key_evidence(ref_norm, cand_norm, left_over), list(record("grp", 1L)))
  # Matching that stopped at a size limit can leave rows of one key unpaired
  # on both sides, so nothing is concluded from what it left over. The same
  # holds when the record does not say how matching ended.
  left_over$resource_state <- "residual_group_limit"
  expect_null(unpaired_key_evidence(ref_norm, cand_norm, left_over))
  left_over$resource_state <- NULL
  expect_null(unpaired_key_evidence(ref_norm, cand_norm, left_over))
})

test_that("a failure inside the check leaves the comparison as it is", {
  reference <- six()
  generated <- transform(reference, visit = ifelse(id <= 4L, 9, visit))
  ref_norm <- normalize_output_frame(reference)
  cand_norm <- normalize_output_frame(generated)
  alignment <- align_output_rows(ref_norm, cand_norm, context = list(configured_keys = three_keys), normalized = TRUE)
  expect_identical(unpaired_key_evidence(ref_norm, cand_norm, alignment), list(record("visit", 4L)))
  local_mocked_bindings(validate_alignment_key = function(...) stop("made-up failure"))
  expect_null(unpaired_key_evidence(ref_norm, cand_norm, alignment))
})

test_that("every counted pair agrees on the other key columns and differs on the named one", {
  withr::local_seed(20261127)
  reference <- data.frame(study = "S1", id = rep(1:40, each = 3L), param = rep(c("A", "B", "C"), 40L),
    visit = rep(c(1, 2, NA), 40L), value = 1, stringsAsFactors = FALSE)
  generated <- reference
  rows <- sample(nrow(reference), 30L)
  generated$visit[rows] <- ifelse(is.na(generated$visit[rows]), 0, generated$visit[rows] + 50)
  keys <- c("study", "id", "param", "visit")
  ref_norm <- normalize_output_frame(reference)
  cand_norm <- normalize_output_frame(generated)
  alignment <- align_output_rows(ref_norm, cand_norm, context = list(configured_keys = keys), normalized = TRUE)
  expect_identical(alignment$method, "unique_key")
  expect_identical(unpaired_key_evidence(ref_norm, cand_norm, alignment), list(record("visit", 30L,
    reference = sum(is.na(reference$visit[rows])), generated = 0L)))

  # The same count, taken by joining the left-over rows by hand.
  left <- as.data.frame(ref_norm[alignment$only_reference, keys])
  right <- as.data.frame(cand_norm[alignment$only_candidate, keys])
  joined <- merge(left, right, by = c("study", "id", "param"), suffixes = c(".reference", ".generated"))
  expect_identical(nrow(joined), 30L)
  same <- (is.na(joined$visit.reference) & is.na(joined$visit.generated)) |
    (!is.na(joined$visit.reference) & !is.na(joined$visit.generated) &
      joined$visit.reference == joined$visit.generated)
  expect_false(any(same))
})

test_that("the pairing and the verdict are the same with and without the record", {
  reference <- data.frame(id = seq_len(20L), visit = c(rep(NA_real_, 8L), rep(1, 12L)), value = seq_len(20L))
  generated <- transform(reference, visit = ifelse(is.na(visit), 0, visit), value = value + (id == 20L))
  with <- compare_datasets_aligned(reference, generated, keys = c("id", "visit"))
  expect_identical(key_records(with), list(record("visit", 8L, reference = 8L)))
  local_mocked_bindings(unpaired_key_evidence = function(...) NULL)
  without <- compare_datasets_aligned(reference, generated, keys = c("id", "visit"))
  expect_null(key_records(without))
  expect_false("unpaired_keys" %in% names(without$structure))
  with$structure$unpaired_keys <- NULL
  expect_identical(with, without)
  expect_false(without$passed)
})

test_that("the record holds column names and counts, never a row's value", {
  reference <- data.frame(id = sprintf("ROWVALUE_SENTINEL_%02d", 1:12), visit = c(rep(424242, 5L), rep(1, 7L)),
    stringsAsFactors = FALSE)
  generated <- transform(reference, visit = ifelse(visit == 424242, 737373, visit))
  evidence <- key_records(compare_datasets_aligned(reference, generated, keys = c("id", "visit")))
  expect_identical(evidence, list(record("visit", 5L)))
  saved <- as.character(jsonlite::toJSON(evidence, auto_unbox = TRUE))
  expect_false(grepl("SENTINEL|424242|737373", saved))
  expect_identical(saved, '[{"key":"visit","pair_without":5,"missing_reference":0,"missing_generated":0}]')
})

test_that("the gate saves the record with the output's assessment and words its check as before", {
  reference <- data.frame(id = seq_len(177L), visit = c(rep(NA_real_, 60L), rep(1, 117L)))
  generated <- transform(reference, visit = ifelse(is.na(visit), 0, visit))
  ref_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(reference, ref_path)
  contract <- list(target_id = "t1", target_key = "out", logical_name = "out",
    kind = "dataset", required = TRUE, resolution = "static")
  target <- assess_dataset_target(contract, generated,
    comparison_rules = list(keys = c("id", "visit"), references = list(out = ref_path)), project = NULL)
  expect_identical(target$differences$structure$unpaired_keys,
    list(record("visit", 60L, reference = 60L, generated = 0L)))
  expect_identical(target$status, "failed")
  expect_identical(target$checks$reference_comparison$details, "Differences observed; alignment: complete")

  # The summaries built from the assessment do not carry it.
  summary <- dataset_discrepancy_summary(target)
  expect_false(grepl("unpaired_keys|pair_without", paste(names(unlist(summary)), collapse = " ")))
  expect_identical(discrepancy_description(summary), "Some rows could not be paired")
  expect_false(grepl("unpaired_keys|pair_without|missing_reference",
    paste(c(names(unlist(reviewer_discrepancy_summary(target))), unlist(reviewer_discrepancy_summary(target))),
      collapse = " ")))
})
