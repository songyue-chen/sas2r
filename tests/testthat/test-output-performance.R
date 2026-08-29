# Bounded Performance, Capacity Limits, and Algorithmic Guardrails

test_that("inventory breadth stops strictly at maximum files cap", {
  dir <- withr::local_tempdir()
  ref_dir <- file.path(dir, "ref")
  dir.create(ref_dir, recursive = TRUE)

  # Create 550 dummy RDS files
  for (i in 1:550) {
    saveRDS(data.frame(x = i), file.path(ref_dir, sprintf("ds_%04d.rds", i)))
  }

  cfg <- structure(list(
    libraries = list(adam = list(path = ref_dir, engine = "rds", write = "rds")),
    output_review = list(
      enabled = TRUE,
      r_libraries = list(adam = ref_dir)
    )
  ), class = "sas2r_config")

  limits <- output_evidence_limits(max_files_per_root = 500L)
  inv <- sas2r:::build_output_inventory(NULL, cfg, limits = limits)

  # Available inventoried candidates must not exceed max_files_per_root
  expect_equal(sum(inv$inventory$status == "available"), 500L)

  # The cap is a cap: the 50 files past it produce one truncation marker
  # between them, not 50 inventory rows and 50 resolver bindings. Both roots
  # point at the same directory, so each contributes one marker.
  markers <- inv$inventory$reason == "max_files_per_root_exceeded"
  expect_equal(sum(markers, na.rm = TRUE), length(unique(inv$inventory$root_id)))
  expect_equal(nrow(inv$inventory), 500L + sum(markers, na.rm = TRUE))

  resolver <- attr(inv, "resolver", exact = TRUE)
  expect_equal(length(ls(resolver)), nrow(inv$inventory))
})

test_that("residual Hungarian LSAP assignment respects group cap and avoids solve_LSAP above cap", {
  ref_df <- data.frame(k = "dup_key", val = paste0("ref_", 1:55), stringsAsFactors = FALSE)
  cand_df <- data.frame(k = "dup_key", val = paste0("cand_", 1:55), stringsAsFactors = FALSE)

  # The cap is a guarantee about work that must NOT happen, so the returned
  # resource_state cannot witness it on its own: count solver entries directly.
  real_solve_LSAP <- clue::solve_LSAP
  lsap_calls <- 0L
  testthat::local_mocked_bindings(
    solve_LSAP = function(x, ...) {
      lsap_calls <<- lsap_calls + 1L
      real_solve_LSAP(x, ...)
    },
    .package = "clue"
  )

  # Exactly at or below cap (e.g. 50): the solver does run.
  res_below <- minimum_cost_residual_pairs(
    ref_df, cand_df,
    ref_residuals = 1:50,
    cand_residuals = 1:50,
    cols = "val",
    cap = 50L
  )
  expect_identical(res_below$resource_state, "complete")
  expect_equal(nrow(res_below$pairs), 50L)
  expect_gt(lsap_calls, 0L)

  # Above cap (e.g. 51): returns residual_group_limit without running LSAP
  calls_before_above <- lsap_calls
  res_above <- minimum_cost_residual_pairs(
    ref_df, cand_df,
    ref_residuals = 1:51,
    cand_residuals = 1:51,
    cols = "val",
    cap = 50L
  )
  expect_identical(res_above$resource_state, "residual_group_limit")
  expect_identical(lsap_calls, calls_before_above)
  expect_equal(nrow(res_above$pairs), 0L)
  expect_identical(res_above$only_reference, 1:51)
  expect_identical(res_above$only_candidate, 1:51)
})

test_that("unique-key and keyless multiset alignments execute in bounded deterministic time", {
  n <- 2000L
  df_ref <- data.frame(
    id = seq_len(n),
    str = sprintf("val_%05d", seq_len(n)),
    num = as.double(seq_len(n)),
    stringsAsFactors = FALSE
  )
  # Permuted candidate with small difference
  df_cand <- df_ref[sample(n), ]
  df_cand$num[10] <- 999999.0

  ctx_key <- alignment_context(by = "id")
  t0 <- Sys.time()
  align_res <- sas2r:::align_output_rows(df_ref, df_cand, context = ctx_key)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_true(elapsed < 2.0)
  expect_identical(align_res$method, "unique_key")
  expect_equal(nrow(align_res$pairs), n)
  expect_identical(align_res$resource_state, "complete")

  # Keyless multiset alignment. No column name is an inferable key and the
  # context supplies none, so the run must take the keyless branch; the two
  # frames differ on three rows so the residual matcher is genuinely entered.
  kl_ref <- data.frame(
    label = sprintf("val_%05d", seq_len(n)),
    amount = as.double(seq_len(n)),
    stringsAsFactors = FALSE
  )
  kl_cand <- kl_ref[sample(n), , drop = FALSE]
  rownames(kl_cand) <- NULL
  residual_rows <- c(1L, 2L, 3L)
  kl_cand$amount[residual_rows] <- kl_cand$amount[residual_rows] + 1e6

  ctx_keyless <- alignment_context(by = character())
  expect_length(sas2r:::infer_alignment_keys(kl_ref, kl_cand, context = ctx_keyless), 0L)

  t0_kl <- Sys.time()
  align_kl <- sas2r:::align_output_rows(kl_ref, kl_cand, context = ctx_keyless)
  elapsed_kl <- as.numeric(difftime(Sys.time(), t0_kl, units = "secs"))

  expect_true(elapsed_kl < 2.0)
  expect_identical(align_kl$method, "keyless_multiset")
  expect_identical(align_kl$selected_keys, character())
  expect_equal(nrow(align_kl$pairs), n)
  expect_identical(align_kl$resource_state, "complete")
  # Residual assignment recovered the three altered rows rather than dropping them.
  expect_length(align_kl$only_reference, 0L)
  expect_length(align_kl$only_candidate, 0L)
  expect_identical(
    kl_ref$label[align_kl$pairs$reference_row],
    kl_cand$label[align_kl$pairs$candidate_row]
  )
  expect_equal(
    sum(kl_ref$amount[align_kl$pairs$reference_row] !=
          kl_cand$amount[align_kl$pairs$candidate_row]),
    length(residual_rows)
  )
})

test_that("comparison report serialization stays strictly within bounded byte limits", {
  ref <- data.frame(id = 1:1000, x = 1:1000)
  cand <- data.frame(id = 1:1000, x = c(1:900, 2001:2100))
  target <- output_target_fixture()
  report <- sas2r:::compare_aligned_outputs(ref, cand, target, context = alignment_context("id"))

  # Report examples are bounded
  expect_true(nrow(report$examples) <= 20L)

  tmp_file <- withr::local_tempfile(fileext = ".json")
  write_comparison_report(report, file = tmp_file)

  info <- file.info(tmp_file)
  # Report file size should be well under 100 KB
  expect_true(info$size < 100 * 1024)
})
