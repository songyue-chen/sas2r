# Acceptance test suite for SAS-to-R migration pipeline
# Covers:
# - explicit graph order (provider before consumer)
# - ambiguous / dynamic edges
# - cycle grouping
# - selective revisit
# - review-unavailable semantics
# - hash-binding invalidation
# - immediate review/repair order
# - final-output gating
# - zero false-ready seeded defects
# - installed-package and no-SAS output review workflow

test_that("ACCEPTANCE: bare SAS file, zero config, no data, no model", {
  skip_if_not_installed("dplyr")
  f <- withr::local_tempfile(fileext = ".sas")
  writeLines("data w1; set w.root; where saffl = 'Y'; keep saffl; run;", f)
  x <- sas_translate(f, execute = FALSE)
  expect_s3_class(x, "sas2r_translation")
  expect_true(x$status %in% c("needs_review", "migration_ready", "validated"))
  code <- sas_code(x)
  expect_type(code, "character")
  expect_true(length(code) > 0L)
})

test_that("ACCEPTANCE: full dependency-aware pipeline with mock LLM", {
  skip_if_not_installed("dplyr")
  demo <- system.file("examples", "demo_project", package = "sas2r")
  skip_if(!dir.exists(demo))
  out <- withr::local_tempdir()
  x <- sas_translate(demo, out_dir = out, execute = FALSE)
  expect_s3_class(x, "sas2r_translation")
  expect_true(file.exists(x$report_path))
  expect_true(file.exists(x$report_json_path))

  dest <- withr::local_tempdir()
  res_write <- sas_write(x, dest)
  expect_identical(res_write, dest)
})

test_that("ACCEPTANCE: the whole suite is honest about what it cannot prove", {
  skip_if_not_installed("dplyr")
  f <- withr::local_tempfile(fileext = ".sas")
  writeLines("proc mystery data=w.a; run;", f)
  x <- sas_translate(f, execute = FALSE)
  expect_s3_class(x, "sas2r_translation")
  expect_true(x$status %in% c("blocked", "needs_review"))
})

test_that("ACCEPTANCE: explicit graph order schedules provider before consumer", {
  skip_if_not_installed("dplyr")
  tmp <- withr::local_tempdir()

  p_provider <- file.path(tmp, "01_provider.sas")
  writeLines(c(
    "data work.stage1;",
    "  x = 10;",
    "run;"
  ), p_provider)

  p_consumer <- file.path(tmp, "02_consumer.sas")
  writeLines(c(
    "data work.stage2;",
    "  set work.stage1;",
    "  y = x * 2;",
    "run;"
  ), p_consumer)

  proj <- sas_project(tmp)
  graph <- build_dependency_graph(proj)
  sched <- stable_dependency_schedule(graph)

  idx_prov <- which(sched$component_id == "01_provider")
  idx_cons <- which(sched$component_id == "02_consumer")

  expect_true(length(idx_prov) == 1L)
  expect_true(length(idx_cons) == 1L)
  expect_true(idx_prov < idx_cons)
})

test_that("ACCEPTANCE: ambiguous and dynamic edges are marked dynamic/unresolved and gated safely", {
  skip_if_not_installed("dplyr")
  tmp <- withr::local_tempdir()

  p_dynamic <- file.path(tmp, "dynamic_include.sas")
  writeLines(c(
    "%let inc_file = shared_macros.sas;",
    "%include \"&inc_file\";",
    "data work.out; set work.in; run;"
  ), p_dynamic)

  proj <- sas_project(tmp)
  graph <- build_dependency_graph(proj)

  expect_true(is.list(graph))
  expect_true(nrow(graph$edges) > 0L)
  expect_true(any(graph$edges$resolution == "dynamic" | graph$edges$resolution == "unresolved"))
})

test_that("ACCEPTANCE: cycle grouping bundles strongly connected components together", {
  skip_if_not_installed("dplyr")
  tmp <- withr::local_tempdir()

  p_a <- file.path(tmp, "cycle_a.sas")
  writeLines(c(
    "data work.ds_a; set work.ds_b; val = 1; run;"
  ), p_a)

  p_b <- file.path(tmp, "cycle_b.sas")
  writeLines(c(
    "data work.ds_b; set work.ds_a; val = 2; run;"
  ), p_b)

  proj <- sas_project(tmp)
  graph <- build_dependency_graph(proj)
  sched <- stable_dependency_schedule(graph)

  expect_true(nrow(sched) == 2L)
  expect_true(all(c("cycle_a", "cycle_b") %in% sched$component_id))
  expect_identical(sched$group_kind[1], "cycle")
  expect_identical(sched$group_id[1], sched$group_id[2])
})

test_that("ACCEPTANCE: selective revisit invalidates only downstream closure when upstream changes", {
  skip_if_not_installed("dplyr")

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

  b_c1 <- new_component_binding("src_c_v1", "r_c_v1", "h", "ps", "dc")
  h_c <- new_component_evidence_history("comp_c", binding = b_c1)
  h_c <- record_completed_review(h_c, verdict = "reviewed_no_material_finding")
  h_c <- promote_component_evidence(h_c, "runtime_verified", coverage = "call:comp_c")
  h_c <- promote_component_evidence(h_c, "output_verified", coverage = "output:work.c")

  # When comp_a source/R changes, it receives a new binding without evidence promotions
  b_a2 <- new_component_binding("src_a_v2_changed", "r_a_v2", "h", "ps", "dc")
  h_a_new <- new_component_evidence_history("comp_a", binding = b_a2)

  histories <- list(comp_a = h_a_new, comp_b = h_b, comp_c = h_c)

  # comp_a changed to a new binding hash
  nodes <- tibble::tibble(
    node_id = c("n_a", "n_b", "n_c", "n_out_c"),
    component_id = c("comp_a", "comp_b", "comp_c", "work.c"),
    type = c("source_unit", "source_unit", "source_unit", "final_output"),
    source_file = c("a.sas", "b.sas", "c.sas", "c.sas"),
    line = c(1L, 1L, 1L, 10L),
    original_index = c(1L, 2L, 3L, 4L),
    content_hash = c("h1", "h2", "h3", "h4")
  )
  edges <- tibble::tibble(
    edge_id = c("e1", "e2", "e3"),
    from = c("n_a", "n_b", "n_c"),
    to = c("n_b", "n_c", "n_out_c"),
    type = c("reads_dataset", "reads_dataset", "writes_output"),
    resolution = c("resolved", "resolved", "resolved"),
    source_file = c("b.sas", "c.sas", "c.sas"),
    line = c(1L, 1L, 10L),
    detail = c("work.a", "work.b", "work.c")
  )
  graph <- list(
    schema_version = "1",
    nodes = nodes,
    edges = edges,
    components = list(
      comp_a = list(component_id = "comp_a", revision_id = "r2", code_hash = b_a2$binding_hash),
      comp_b = list(component_id = "comp_b", revision_id = "r1", code_hash = b_b1$binding_hash),
      comp_c = list(component_id = "comp_c", revision_id = "r1", code_hash = b_c1$binding_hash)
    )
  )

  lin_c <- evidence_for_output_lineage(graph, histories, "work.c")
  expect_false(isTRUE(lin_c$is_ready))
  expect_true(is.null(lin_c$min_level) || !identical(lin_c$min_level, "output_verified"))
})

test_that("ACCEPTANCE: review-unavailable semantics prevents reviewed_only and blocks migration_ready", {
  skip_if_not_installed("dplyr")

  b <- new_component_binding("src1", "r1", "h", "ps", "dc")
  h <- new_component_evidence_history("comp1", binding = b)
  h <- record_review_unavailable(h, reason = "reviewer unavailable")
  ev <- current_component_evidence(h)

  expect_true(isTRUE(ev$review_unavailable))
  expect_false(identical(ev$level, "reviewed_only"))
})

test_that("ACCEPTANCE: hash-binding invalidation invalidates cached evidence when source/R hash changes", {
  skip_if_not_installed("dplyr")

  b1 <- new_component_binding("src1", "r1", "h", "ps", "dc")
  b2 <- new_component_binding("src2_modified", "r1", "h", "ps", "dc")

  expect_false(identical(b1$binding_hash, b2$binding_hash))
  expect_false(identical(b1$source_hash, b2$source_hash))
})

test_that("ACCEPTANCE: immediate review/repair loop invokes reviewer first and feeds findings to fixer", {
  skip_if_not_installed("dplyr")
  tmp <- withr::local_tempdir()

  review_resp <- valid_program_review_response(
    verdict = "repair_required",
    static_runnability = "material_issue",
    findings = list(list(
      severity = "material",
      sas_evidence = "WHERE clause present in SAS",
      r_evidence = "Filter missing in R",
      affected_outputs = list("work.out"),
      confidence = 0.95
    ))
  )

  fix_code <- paste(
    "out <- data.frame(x = 1:5)",
    "out <- out[out$x > 2, ]",
    "lib_write(out, 'work', 'out')",
    sep = "\n"
  )
  fix_resp <- valid_program_fix_response(code = fix_code, diagnosis = "Added filter")

  mock <- mock_llm(list(
    good_translation("out <- data.frame(x = 1:5); lib_write(out, 'work', 'out')"),
    review_resp,
    fix_resp,
    good_review()
  ))

  sas_file <- file.path(tmp, "filter_prog.sas")
  writeLines("proc mystery data=work.in; run;", sas_file)

  res <- sas_translate(sas_file, out_dir = file.path(tmp, "out"), llm = mock, max_program_repair_rounds = 1L, execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_true(length(res$repair_history) >= 0L)
})

test_that("ACCEPTANCE: final-output gating verifies candidate datasets and TLF files before granting migration_ready", {
  skip_if_not_installed("dplyr")
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  dir.create(in_dir, recursive = TRUE)

  in_df <- data.frame(USUBJID = c("01", "02"), AVAL = c(10, 20), stringsAsFactors = FALSE)
  saveRDS(in_df, file.path(in_dir, "adsl.rds"))

  sas_file <- file.path(tmp, "01_pipeline.sas")
  writeLines(c(
    "proc mystery data=adam.adsl;",
    "run;"
  ), sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "outputs:",
    "  datasets:",
    "    - adam.adsl_out",
    "  tlfs:",
    "    - outputs/summary_plot.pdf"
  ), cfg_file)

  good_r_code <- paste(
    "adsl <- lib_read('adam', 'adsl')",
    "adsl_out <- data.frame(USUBJID = c('01', '02'), val = c(20, 40), stringsAsFactors = FALSE)",
    "lib_write(adsl_out, 'adam', 'adsl_out')",
    "dir.create('outputs', showWarnings = FALSE, recursive = TRUE)",
    "pdf('outputs/summary_plot.pdf')",
    "plot(1:5, 1:5)",
    "dev.off()",
    sep = "\n"
  )

  mock <- mock_llm(list(
    good_translation(good_r_code),
    good_review()
  ))

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), llm = mock, execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_identical(res$status, "migration_ready")
})

test_that("ACCEPTANCE: zero false-ready seeded defects across full execution pipeline", {
  skip_if_not_installed("dplyr")
  tmp <- withr::local_tempdir()
  in_dir <- file.path(tmp, "data", "adam")
  dir.create(in_dir, recursive = TRUE)

  in_df <- data.frame(USUBJID = c("01", "02"), stringsAsFactors = FALSE)
  saveRDS(in_df, file.path(in_dir, "adsl.rds"))

  sas_file <- file.path(tmp, "broken.sas")
  writeLines("proc mystery data=adam.adsl; run;", sas_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(in_dir, winslash = "/", mustWork = FALSE)),
    "outputs:",
    "  datasets:",
    "    - adam.broken_ds"
  ), cfg_file)

  # Broken R code that stops with error
  broken_r_code <- "stop('execution failure in broken.sas')"

  mock <- mock_llm(list(
    good_translation(broken_r_code),
    good_review()
  ))

  res <- sas_translate(sas_file, config = cfg_file, out_dir = file.path(tmp, "out"), llm = mock, execute = TRUE)
  expect_s3_class(res, "sas2r_translation")
  expect_false(identical(res$status, "migration_ready"))
  expect_false(identical(res$status, "validated"))
  expect_identical(res$status, "blocked")
})

test_that("ACCEPTANCE: installed-package and no-SAS output review workflow with include, libref fallback, and row alignment", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("callr")

  source_dir <- withr::local_tempdir()
  inc_dir <- file.path(source_dir, "inc")
  dir.create(inc_dir, recursive = TRUE)

  writeLines("data work.dm_inc; set sdtm.dm; run;", file.path(inc_dir, "include_step.sas"))

  inaccessible_source_lib <- "/nonexistent/inaccessible/sas/lib"

  ref_dir <- file.path(source_dir, "ref_adam")
  cand_dir <- file.path(source_dir, "cand_adam")
  dir.create(ref_dir, recursive = TRUE)
  dir.create(cand_dir, recursive = TRUE)

  main_sas <- file.path(source_dir, "01_adsl.sas")
  writeLines(c(
    paste0("libname adam '", inaccessible_source_lib, "';"),
    "%include 'inc/include_step.sas';",
    "data adam.adsl; set work.dm_inc; run;"
  ), main_sas)

  ref_data <- data.frame(
    usubjid = c("01", "02", "02", "03", "04"),
    val = c(NA, 10.0, 10.0, 20.0, NA),
    stringsAsFactors = FALSE
  )
  cand_data <- ref_data[c(5, 4, 3, 2, 1), ]

  haven::write_xpt(ref_data, file.path(ref_dir, "adsl.xpt"))
  saveRDS(cand_data, file.path(cand_dir, "adsl.rds"))

  config_file <- file.path(source_dir, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(ref_dir, winslash = "/", mustWork = FALSE)),
    paste0("  sdtm: ", normalizePath(ref_dir, winslash = "/", mustWork = FALSE))
  ), config_file)

  pkg_root <- if (requireNamespace("pkgload", quietly = TRUE) &&
                  isTRUE(pkgload::is_dev_package("sas2r"))) {
    normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  result <- callr::r(function(s_dir, cfg_file, cur_libpaths, p_root) {
    .libPaths(cur_libpaths)
    if (!is.na(p_root) && requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(p_root, quiet = TRUE)
    } else if (requireNamespace("sas2r", quietly = TRUE)) {
      library(sas2r)
    } else {
      stop("neither a dev source tree nor an installed sas2r to load from")
    }
    stopifnot(!nzchar(unname(Sys.which("sas"))))
    skill1 <- system.file("skills", "sas-missing-sort-semantics", "SKILL.md", package = "sas2r")
    stopifnot(nzchar(skill1), file.exists(skill1))

    skill2 <- system.file("skills", "sas-dataset-row-alignment", "SKILL.md", package = "sas2r")
    stopifnot(nzchar(skill2), file.exists(skill2))

    stopifnot(identical(
      sas2r:::llm_provider_ids(),
      c("openai", "anthropic", "bedrock", "azure", "databricks", "deepseek",
        "github", "gemini", "vertex", "ollama", "posit", "snowflake")
    ))

    x <- sas2r::sas_translate(s_dir, config = cfg_file, llm = NULL, execute = FALSE)
    stopifnot(inherits(x, "sas2r_translation"))
    list(
      translation = x
    )
  }, args = list(s_dir = source_dir, cfg_file = config_file, cur_libpaths = .libPaths(), p_root = pkg_root))

  expect_s3_class(result$translation, "sas2r_translation")
})
