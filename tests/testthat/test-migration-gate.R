test_that("assess_dataset_target evaluates keys, tolerances, missing values, and explicit checks", {
  tmp <- withr::local_tempdir()

  # Create candidate and reference datasets
  ref_df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AVAL = c(10.0000001, 20.0, NA),
    PARAMCD = c("A", "B", ""),
    stringsAsFactors = FALSE
  )
  cand_df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AVAL = c(10.0000002, 20.0, NA), # within 1e-6 tol
    PARAMCD = c("A  ", "B", ""),    # trailing whitespace, blank for NA
    stringsAsFactors = FALSE
  )

  ref_path <- file.path(tmp, "ref.rds")
  cand_path <- file.path(tmp, "adsl.rds")
  saveRDS(ref_df, ref_path)
  saveRDS(cand_df, cand_path)

  contract <- list(
    target_id = "target_001",
    target_key = "adam.adsl",
    kind = "dataset",
    logical_name = "adam.adsl",
    required = TRUE,
    reference_path = ref_path,
    assertions = list(
      required_columns = c("USUBJID", "AVAL", "PARAMCD"),
      keys = c("USUBJID"),
      numeric_tolerance = 1e-5
    )
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)

  res <- assess_dataset_target(contract, attempt)
  expect_true(res$passed)
  expect_identical(res$status, "passed")
  expect_true(res$has_reference)
  expect_true(res$checks$candidate_exists$passed)
  expect_true(res$checks$candidate_readable$passed)
  expect_true(res$checks$required_columns$passed)
  expect_true(res$checks$reference_comparison$passed)
  expect_true(length(res$differences$cosmetic) >= 0L)
})

test_that("assess_dataset_target detects missing required columns and tolerance failures", {
  tmp <- withr::local_tempdir()

  cand_df <- data.frame(
    USUBJID = c("01", "02"),
    AVAL = c(10.0, 50.0), # 50.0 != 20.0
    stringsAsFactors = FALSE
  )
  cand_path <- file.path(tmp, "adsl.rds")
  saveRDS(cand_df, cand_path)

  ref_df <- data.frame(
    USUBJID = c("01", "02"),
    AVAL = c(10.0, 20.0),
    PARAMCD = c("A", "B"),
    stringsAsFactors = FALSE
  )
  ref_path <- file.path(tmp, "ref.rds")
  saveRDS(ref_df, ref_path)

  # Contract requiring PARAMCD
  contract <- list(
    target_id = "target_002",
    target_key = "adam.adsl",
    kind = "dataset",
    logical_name = "adam.adsl",
    required = TRUE,
    reference_path = ref_path,
    assertions = list(
      required_columns = c("USUBJID", "AVAL", "PARAMCD"),
      numeric_tolerance = 1e-6
    )
  )
  attempt <- list(attempt_dir = tmp)

  res <- assess_dataset_target(contract, attempt)
  expect_false(res$passed)
  expect_false(res$checks$required_columns$passed)
  expect_true("paramcd" %in% tolower(res$checks$required_columns$missing_columns))
})

test_that("assess_final_outputs promotes covered components on passing output lineage", {
  tmp <- withr::local_tempdir()

  # Create candidate files
  cand_adsl <- data.frame(USUBJID = c("01", "02"), AVAL = c(1, 2), stringsAsFactors = FALSE)
  saveRDS(cand_adsl, file.path(tmp, "adsl.rds"))

  ref_adsl <- data.frame(USUBJID = c("01", "02"), AVAL = c(1, 2), stringsAsFactors = FALSE)
  ref_path <- file.path(tmp, "ref_adsl.rds")
  saveRDS(ref_adsl, ref_path)

  contracts <- tibble::tibble(
    target_id = c("t1"),
    target_key = c("adam.adsl"),
    kind = c("dataset"),
    logical_name = c("adam.adsl"),
    path_expression = c(NA_character_),
    resolution = c("resolved"),
    producer_node_id = c("node_prog_a"),
    required = c(TRUE),
    reference_path = c(ref_path),
    assertions = list(list(required_columns = c("USUBJID", "AVAL"))),
    source_file = c("prog_a.sas"),
    line = c(1L),
    reason = c("terminal_lineage")
  )

  attempt <- list(
    attempt_id = "bundle_attempt_001",
    attempt_dir = tmp,
    outputs_dir = tmp,
    completed = TRUE,
    passed = TRUE,
    exit_status = 0L
  )

  # Graph with prog_a creating adam.adsl, and prog_unrelated creating nothing
  graph <- list(
    nodes = tibble::tibble(
      node_id = c("node_prog_a", "node_target_adsl", "node_prog_unrelated"),
      component_id = c("prog_a", "adam.adsl", "prog_unrelated"),
      type = c("source_unit", "final_output", "source_unit")
    ),
    edges = tibble::tibble(
      from = c("node_prog_a"),
      to = c("node_target_adsl"),
      type = c("creates_dataset"),
      detail = c("adam.adsl"),
      resolution = c("resolved")
    )
  )

  # Evidence histories
  binding_a <- new_component_binding("src_a", "r_a", "h", "ps", "dc")
  hist_a <- new_component_evidence_history("prog_a", binding = binding_a)
  hist_a <- record_completed_review(hist_a, verdict = "reviewed_no_material_finding")
  hist_a <- promote_component_evidence(hist_a, "runtime_verified", coverage = "run:prog_a")

  binding_u <- new_component_binding("src_u", "r_u", "h", "ps", "dc")
  hist_u <- new_component_evidence_history("prog_unrelated", binding = binding_u)
  hist_u <- record_completed_review(hist_u, verdict = "reviewed_no_material_finding")
  hist_u <- promote_component_evidence(hist_u, "runtime_verified", coverage = "run:prog_u")

  histories <- list(prog_a = hist_a, prog_unrelated = hist_u)

  assessment <- assess_final_outputs(contracts, attempt, graph, histories)

  expect_identical(assessment$status, "validated")
  expect_true(assessment$all_required_passed)

  # prog_a was promoted to reference_validated because reference comparison passed
  updated_a <- current_component_evidence(assessment$evidence_histories$prog_a)
  expect_identical(updated_a$level, "reference_validated")

  # prog_unrelated was off-lineage and remains runtime_verified
  updated_u <- current_component_evidence(assessment$evidence_histories$prog_unrelated)
  expect_identical(updated_u$level, "runtime_verified")
})

test_that("gate without configured tolerance uses compare_profile defaults, not abs-only 1e-6", {
  tmp <- withr::local_tempdir()
  # Values at AUC scale: double resolution near 1e7 makes an absolute-only
  # 1e-6 unmeetable, while the default combined tolerance (abs 1e-8 +
  # rel 1e-8 * 1e7 = 0.1) accepts a 0.05 representation-level difference.
  ref_df <- data.frame(USUBJID = c("01", "02"), AUC = c(1e7, 2e7))
  cand_df <- data.frame(USUBJID = c("01", "02"), AUC = c(1e7 + 0.05, 2e7))
  ref_path <- file.path(tmp, "ref.rds")
  saveRDS(ref_df, ref_path)
  saveRDS(cand_df, file.path(tmp, "adpc.rds"))

  contract <- list(
    target_id = "t1", target_key = "adam.adpc", kind = "dataset",
    logical_name = "adam.adpc", required = TRUE,
    reference_path = ref_path,
    assertions = list(keys = "USUBJID")
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)
  res <- assess_dataset_target(contract, attempt)
  expect_true(res$checks$reference_comparison$passed)

  # An explicitly configured tolerance stays authoritative and absolute.
  contract$assertions$numeric_tolerance <- 1e-6
  res_strict <- assess_dataset_target(contract, attempt)
  expect_false(res_strict$checks$reference_comparison$passed)
})

test_that("a TLF reference that merely exists is never reported as compared", {
  tmp <- withr::local_tempdir()
  writeLines("{\\rtf1 candidate}", file.path(tmp, "t_demo.rtf"))
  ref_path <- file.path(tmp, "reference.rtf")
  writeLines("{\\rtf1 reference}", ref_path)

  contract <- list(
    target_id = "t1", target_key = "t_demo.rtf", kind = "tlf",
    logical_name = "t_demo.rtf", required = TRUE,
    reference_path = ref_path, assertions = list()
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)
  res <- assess_tlf_target(contract, attempt)

  # No TLF content comparator exists, so reference evidence must not be claimed.
  expect_false(isTRUE(res$reference_passed))
  expect_false(isTRUE(res$checks$reference_comparison$passed))
})

test_that("gate alignment: a content-equal reorder without keys passes instead of smearing diffs", {
  tmp <- withr::local_tempdir()
  ref_df <- data.frame(USUBJID = c("01", "02", "03"), AVAL = c(1, 2, 3),
                       stringsAsFactors = FALSE)
  cand_df <- ref_df[c(3, 1, 2), ]
  ref_path <- file.path(tmp, "ref.rds")
  saveRDS(ref_df, ref_path)
  saveRDS(cand_df, file.path(tmp, "adsl.rds"))

  contract <- list(
    target_id = "t1", target_key = "adam.adsl", kind = "dataset",
    logical_name = "adam.adsl", required = TRUE,
    reference_path = ref_path, assertions = list()
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)
  res <- assess_dataset_target(contract, attempt)
  expect_true(res$checks$reference_comparison$passed)
  expect_true(res$passed)
})

test_that("gate alignment: duplicate-key rows match as multisets, not by position", {
  tmp <- withr::local_tempdir()
  ref_df <- data.frame(USUBJID = c("01", "01"), AVAL = c(10, 20),
                       stringsAsFactors = FALSE)
  cand_df <- data.frame(USUBJID = c("01", "01"), AVAL = c(20, 10),
                        stringsAsFactors = FALSE)
  ref_path <- file.path(tmp, "ref.rds")
  saveRDS(ref_df, ref_path)
  saveRDS(cand_df, file.path(tmp, "adsl.rds"))

  contract <- list(
    target_id = "t1", target_key = "adam.adsl", kind = "dataset",
    logical_name = "adam.adsl", required = TRUE,
    reference_path = ref_path, assertions = list(keys = "USUBJID")
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)
  res <- assess_dataset_target(contract, attempt)
  expect_true(res$checks$reference_comparison$passed)
})

test_that("gate alignment: a real defect in a reordered unkeyed candidate is localized to one cell", {
  tmp <- withr::local_tempdir()
  ref_df <- data.frame(USUBJID = c("01", "02", "03", "04"),
                       AVAL = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  cand_df <- ref_df
  cand_df$AVAL[3] <- 99
  cand_df <- cand_df[c(4, 3, 1, 2), ]
  ref_path <- file.path(tmp, "ref.rds")
  saveRDS(ref_df, ref_path)
  saveRDS(cand_df, file.path(tmp, "adsl.rds"))

  contract <- list(
    target_id = "t1", target_key = "adam.adsl", kind = "dataset",
    logical_name = "adam.adsl", required = TRUE,
    reference_path = ref_path, assertions = list()
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)
  res <- assess_dataset_target(contract, attempt)
  expect_false(res$checks$reference_comparison$passed)
  s <- res$checks$reference_comparison$summary
  expect_identical(s$value[s$metric == "value_mismatch_cells"], 1L)
})
