test_that("terminal lineage discovers a final output without a mapping table", {
  p <- output_lineage_fixture(c(
    "data work.stage; set sdtm.dm; run;",
    "data adam.adsl; set work.stage; run;"
  ))
  targets <- sas2r:::discover_output_targets(p)
  expect_true(any(targets$logical_dataset == "adam.adsl" &
                  targets$role == "final_dataset"))
})

test_that("same logical name pairs deterministically without an LLM", {
  inventory <- output_inventory_fixture(
    reference = "adam/adsl.sas7bdat", candidate = "adam/adsl.rds")
  targets <- tibble::tibble(
    target_id = "target-1", logical_dataset = "adam.adsl",
    role = "final_dataset", program = "adsl.sas",
    consumer_proc = NA_character_, contributing_unit_ids = list(1L)
  )
  plan <- sas2r:::pair_output_candidates(targets, inventory)
  expect_identical(plan$status, "paired")
  expect_identical(plan$pairing_method, "exact_logical_name")
})

test_that("TLF discovery stops at saved preparation data", {
  p <- output_lineage_fixture(c(
    "data tlfdata.t_14_1; set adam.adsl; run;",
    "proc report data=tlfdata.t_14_1; columns trt; run;"
  ))
  targets <- sas2r:::discover_output_targets(p)
  expect_true(any(targets$logical_dataset == "tlfdata.t_14_1" &
                  targets$role == "tlf_preparation_data"))
  expect_false(any(grepl("pdf|rtf|html", targets$logical_dataset)))
})

test_that("disabled output review returns sentinel row without scanning inventory", {
  p <- output_lineage_fixture(c("data adam.adsl; set sdtm.dm; run;"))
  cfg <- sas_config(start = withr::local_tempdir())
  plan_obj <- sas2r:::build_output_target_plan(p, cfg)
  expect_s3_class(plan_obj, "sas2r_output_target_plan")
  expect_identical(plan_obj$plan$status, "disabled")
  expect_identical(plan_obj$plan$pairing_method, "disabled")
})

test_that("same physical file cannot be paired as both reference and candidate", {
  dir <- withr::local_tempdir()
  shared_file <- file.path(dir, "adsl.xpt")
  haven::write_xpt(data.frame(id = 1), shared_file)

  cfg <- structure(list(
    libraries = list(adam = list(path = normalizePath(dir, winslash = "/", mustWork = FALSE),
                                 engine = "xpt", write = "xpt")),
    output_review = list(
      enabled = TRUE,
      r_libraries = list(adam = normalizePath(dir, winslash = "/", mustWork = FALSE))
    )
  ), class = "sas2r_config")

  # Inventory sees both from same directory
  inv <- sas2r:::build_output_inventory(NULL, cfg)
  targets <- tibble::tibble(
    target_id = "target-1", logical_dataset = "adam.adsl",
    role = "final_dataset", program = "adsl.sas",
    consumer_proc = NA_character_, contributing_unit_ids = list(1L)
  )
  plan <- sas2r:::pair_output_candidates(targets, inv)
  expect_identical(plan$status, "unsupported")
  expect_identical(plan$reason, "same_physical_file")
})

test_that("ambiguous candidate options are returned as option IDs without paths", {
  dir <- withr::local_tempdir()
  ref_dir <- file.path(dir, "ref")
  cand_dir1 <- file.path(dir, "cand1")
  cand_dir2 <- file.path(dir, "cand2")
  dir.create(ref_dir, recursive = TRUE)
  dir.create(cand_dir1, recursive = TRUE)
  dir.create(cand_dir2, recursive = TRUE)

  haven::write_xpt(data.frame(id = 1), file.path(ref_dir, "adsl.xpt"))
  saveRDS(data.frame(id = 1), file.path(cand_dir1, "adsl.rds"))
  saveRDS(data.frame(id = 1), file.path(cand_dir2, "adsl.rds"))

  cfg <- structure(list(
    libraries = list(adam = list(path = normalizePath(ref_dir, winslash = "/"), engine = "xpt", write = "rds")),
    output_review = list(
      enabled = TRUE,
      r_libraries = list(
        adam1 = normalizePath(cand_dir1, winslash = "/"),
        adam2 = normalizePath(cand_dir2, winslash = "/")
      )
    )
  ), class = "sas2r_config")

  inv <- sas2r:::build_output_inventory(NULL, cfg)
  targets <- tibble::tibble(
    target_id = "target-1", logical_dataset = "adam.adsl",
    role = "final_dataset", program = "adsl.sas",
    consumer_proc = NA_character_, contributing_unit_ids = list(1L)
  )
  plan <- sas2r:::pair_output_candidates(targets, inv)
  expect_identical(plan$status, "ambiguous")
  expect_identical(plan$reason, "multiple_candidate_options")
  expect_equal(length(plan$r_options[[1]]), 2L)
  expect_true(is.na(plan$r_candidate_id))
  # Options contain only opaque candidate IDs, no paths
  for (opt in plan$r_options[[1]]) {
    expect_true(grepl("^candidate_\\d{6}$", opt))
  }
})

test_that("missing reference and missing candidate statuses are recorded accurately", {
  # Reference exists, candidate missing
  inv1 <- output_inventory_fixture(reference = "adam/adsl.sas7bdat", candidate = character())
  targets1 <- tibble::tibble(
    target_id = "target-1", logical_dataset = "adam.adsl",
    role = "final_dataset", program = "adsl.sas",
    consumer_proc = NA_character_, contributing_unit_ids = list(1L)
  )
  plan1 <- sas2r:::pair_output_candidates(targets1, inv1)
  expect_identical(plan1$status, "missing_candidate")
  expect_identical(plan1$reason, "r_candidate_not_found")

  # Reference missing, candidate exists
  inv2 <- output_inventory_fixture(reference = character(), candidate = "adam/adsl.rds")
  targets2 <- tibble::tibble(
    target_id = "target-1", logical_dataset = "adam.adsl",
    role = "final_dataset", program = "adsl.sas",
    consumer_proc = NA_character_, contributing_unit_ids = list(1L)
  )
  plan2 <- sas2r:::pair_output_candidates(targets2, inv2)
  expect_identical(plan2$status, "missing_reference")
  expect_identical(plan2$reason, "reference_dataset_not_found")
})

test_that("reused upstream input to TLF PROC is marked input_only_nonprobative", {
  p <- output_lineage_fixture(c(
    "proc report data=adam.adsl; columns trt; run;"
  ))
  targets <- sas2r:::discover_output_targets(p)
  expect_true(any(targets$logical_dataset == "adam.adsl" &
                  targets$role == "input_only_nonprobative"))
  expect_identical(targets$consumer_proc[targets$logical_dataset == "adam.adsl"], "report")
})

test_that("empty or NULL inputs produce well-formed empty outputs", {
  empty_t <- sas2r:::discover_output_targets(NULL)
  expect_equal(nrow(empty_t), 0L)
  expect_identical(names(empty_t), c("target_id", "logical_dataset", "role", "program", "consumer_proc", "contributing_unit_ids"))

  empty_p <- sas2r:::pair_output_candidates(empty_t, NULL)
  expect_equal(nrow(empty_p), 0L)
  expect_identical(names(empty_p), sas2r:::OUTPUT_TARGET_PLAN_COLUMNS)
})
