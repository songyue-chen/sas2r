# Tests for seeded material defects ensuring none receives migration_ready
# Gates checked:
# 1. Wrong SAS missing-value branch
# 2. Reversed sort/order semantics
# 3. Stale dependency binding reused after upstream R change
# 4. Removed terminal dataset write
# 5. Missing TLF destination
# 6. Review-unavailable output-lineage code with no later runtime coverage

test_that("seeded defect 1: wrong SAS missing-value branch does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  ref_dir <- file.path(tmp, "ref", "adam")
  dir.create(in_dir, recursive = TRUE)
  dir.create(ref_dir, recursive = TRUE)

  # In SAS, numeric missing '.' is less than all numbers (val < 10 is TRUE for .)
  in_df <- data.frame(
    USUBJID = c("01", "02", "03"),
    VAL = c(NA_real_, 5.0, 15.0),
    stringsAsFactors = FALSE
  )
  # Correct SAS output where missing VAL is included under VAL < 10
  ref_df <- data.frame(
    USUBJID = c("01", "02"),
    VAL = c(NA_real_, 5.0),
    stringsAsFactors = FALSE
  )

  saveRDS(in_df, file.path(in_dir, "input.rds"))
  saveRDS(ref_df, file.path(ref_dir, "out.rds"))

  sas_file <- file.path(tmp, "01_missing_branch.sas")
  writeLines(c(
    "data adam.out;",
    "  set adam.input;",
    "  where val < 10;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(ref_dir, winslash = "/", mustWork = FALSE))
  ), cfg_file)

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_true(res$status %in% c("blocked", "needs_review"))
})

test_that("seeded defect 2: reversed sort/order semantics does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  ref_dir <- file.path(tmp, "ref", "adam")
  dir.create(in_dir, recursive = TRUE)
  dir.create(ref_dir, recursive = TRUE)

  in_df <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    VISITNUM = c(1, 2, 3, 4),
    AVAL = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  # Correct SAS proc sort descending: 4, 3, 2, 1
  ref_df <- in_df[order(-in_df$VISITNUM), ]

  saveRDS(in_df, file.path(in_dir, "input.rds"))
  saveRDS(ref_df, file.path(ref_dir, "sorted.rds"))

  sas_file <- file.path(tmp, "02_sort.sas")
  writeLines(c(
    "proc sort data=adam.input out=adam.sorted;",
    "  by visitnum;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(ref_dir, winslash = "/", mustWork = FALSE))
  ), cfg_file)

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_true(res$status %in% c("blocked", "needs_review"))
})

test_that("seeded defect 3: stale dependency binding reused after upstream R change is invalidated", {
  b_a1 <- new_component_binding("src_a_v1", "r_a_v1", "h", "ps", "dc")
  h_a <- new_component_evidence_history("comp_a", binding = b_a1)
  h_a <- record_completed_review(h_a, verdict = "reviewed_no_material_finding")
  h_a <- promote_component_evidence(h_a, "runtime_verified", coverage = "call:comp_a")
  h_a <- promote_component_evidence(h_a, "output_verified", coverage = "output:work.a")

  b_b1 <- new_component_binding("src_b_v1", "r_b_v1", "h", "ps", "dc")
  h_b <- new_component_evidence_history("comp_b", binding = b_b1)
  h_b <- record_completed_review(h_b, verdict = "reviewed_no_material_finding")
  h_b <- promote_component_evidence(h_b, "runtime_verified", coverage = "call:comp_b")
  h_b <- promote_component_evidence(h_b, "output_verified", coverage = "output:work.b")

  # When upstream provider changes, it receives a new binding without evidence promotions
  b_a2 <- new_component_binding("src_a_v2_changed", "r_a_v2", "h", "ps", "dc")
  h_a_new <- new_component_evidence_history("comp_a", binding = b_a2)

  histories <- list(comp_a = h_a_new, comp_b = h_b)

  # Graph with tibble nodes and edges
  nodes <- tibble::tibble(
    node_id = c("n_a", "n_b", "n_out_b"),
    component_id = c("comp_a", "comp_b", "work.b"),
    type = c("source_unit", "source_unit", "final_output"),
    source_file = c("a.sas", "b.sas", "b.sas"),
    line = c(1L, 1L, 10L),
    original_index = c(1L, 2L, 3L),
    content_hash = c("h1", "h2", "h3")
  )
  edges <- tibble::tibble(
    edge_id = c("e1", "e2"),
    from = c("n_a", "n_b"),
    to = c("n_b", "n_out_b"),
    type = c("reads_dataset", "writes_output"),
    resolution = c("resolved", "resolved"),
    source_file = c("b.sas", "b.sas"),
    line = c(1L, 10L),
    detail = c("work.a", "work.b")
  )
  graph <- list(
    schema_version = "1",
    nodes = nodes,
    edges = edges,
    components = list(
      comp_a = list(component_id = "comp_a", revision_id = "r2", code_hash = b_a2$binding_hash),
      comp_b = list(component_id = "comp_b", revision_id = "r1", code_hash = b_b1$binding_hash)
    )
  )

  lin_b <- evidence_for_output_lineage(graph, histories, "work.b")
  # When upstream provider binding is changed/stale, output lineage evidence does not grant output_verified
  expect_false(isTRUE(lin_b$is_ready))
  expect_true(is.null(lin_b$min_level) || !identical(lin_b$min_level, "output_verified"))
})

test_that("seeded defect 4: removed terminal dataset write does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  dir.create(in_dir, recursive = TRUE)

  in_df <- data.frame(USUBJID = c("01", "02"), stringsAsFactors = FALSE)
  saveRDS(in_df, file.path(in_dir, "input.rds"))

  sas_file <- file.path(tmp, "04_no_write.sas")
  writeLines(c(
    "proc mystery data=adam.input;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "outputs:",
    "  datasets:",
    "    - adam.final_ds"
  ), cfg_file)

  # Defective R code computes data frame but removes lib_write() call
  defective_r_code <- paste(
    "input <- lib_read('adam', 'input')",
    "final_ds <- transform(input, val = 42)",
    "# lib_write(final_ds, 'adam', 'final_ds') REMOVED",
    sep = "\n"
  )

  mock <- mock_llm(list(
    good_translation(defective_r_code),
    good_review()
  ))

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), llm = mock, execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_identical(res$status, "blocked")
})

test_that("seeded defect 5: missing TLF destination does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  dir.create(in_dir, recursive = TRUE)

  in_df <- data.frame(USUBJID = c("01", "02"), AVAL = c(10, 20), stringsAsFactors = FALSE)
  saveRDS(in_df, file.path(in_dir, "input.rds"))

  sas_file <- file.path(tmp, "05_missing_tlf.sas")
  writeLines(c(
    "proc mystery data=adam.input;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "outputs:",
    "  tlfs:",
    "    - outputs/figure1.pdf"
  ), cfg_file)

  # Defective R code produces no PDF
  defective_r_code <- paste(
    "input <- lib_read('adam', 'input')",
    "cat('plot omitted\n')",
    sep = "\n"
  )

  mock <- mock_llm(list(
    good_translation(defective_r_code),
    good_review()
  ))

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), llm = mock, execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_identical(res$status, "blocked")
})

test_that("seeded defect 6: review-unavailable output-lineage code with no later runtime coverage does not receive migration_ready", {
  tmp <- withr::local_tempdir()
  sas_file <- file.path(tmp, "06_review_unavail.sas")
  writeLines(c(
    "proc mystery data=work.target;",
    "run;"
  ), sas_file)

  unavail_review <- list(
    status = "completed",
    action = "final",
    data = list(
      verdict = "review_unavailable",
      static_runnability = "unspecified",
      findings = list(list(
        severity = "uncertain",
        sas_evidence = "cannot verify macro expansion",
        r_evidence = "uncertain translation",
        affected_outputs = list("work.target")
      )),
      unresolved_dependencies = list()
    )
  )

  mock <- mock_llm(list(
    good_translation("target <- data.frame(x = 100); lib_write(target, 'work', 'target')"),
    unavail_review
  ))

  # execute = FALSE ensures no later runtime coverage
  res <- sas_translate(sas_file, out_dir = file.path(tmp, "out"), llm = mock, execute = FALSE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_identical(res$status, "needs_review")
})
