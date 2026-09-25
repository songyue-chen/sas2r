test_that("dataset evidence never borrows another library's member", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "work"))
  dir.create(file.path(dir, "adam"))
  saveRDS(data.frame(id = 1), file.path(dir, "work", "adsl.rds"))
  contract <- list(kind = "dataset", logical_name = "adam.adsl", target_key = "adam.adsl", required = TRUE)
  attempt <- list(attempt_dir = dir, work_dir = file.path(dir, "work"))
  expect_true(is.na(find_attempt_candidate_file(contract, attempt)))
  expect_false(assess_dataset_target(contract, attempt)$passed)
  saveRDS(data.frame(id = 1), file.path(dir, "adam", "adsl.rds"))
  expect_equal(find_attempt_candidate_file(contract, attempt), normalizePath(file.path(dir, "adam", "adsl.rds")))
  file.create(file.path(dir, "adam", "adsl.xpt"))
  expect_true(is.na(find_attempt_candidate_file(contract, attempt)))
})

test_that("empty-target runs and off-lineage blockers cannot claim readiness", {
  h <- new_component_evidence_history("unrelated", new_component_binding("s", "r", "h", "p", "d"))
  attempt <- list(completed = TRUE, passed = TRUE, execution_order = "unrelated")
  assessment <- assess_final_outputs(empty_output_contracts(), attempt, evidence_histories = list(unrelated = h))
  expect_false(derive_bundle_status(assessment) %in% c("migration_ready", "validated"))
})

test_that("remaining untranslated transformation blocks mechanical approval", {
  p <- tempfile(fileext = ".R")
  writeLines(c("# sas2r:untranslated unit=1 reason=else_deferred", "x <- 1"), p)
  expect_false(check_program_revision(p)$pass)
  unlink(p)
})

test_that("unsupported PROC options and variable lists are deferred intact", {
  cases <- list(
    list(emit_proc_sort, 'proc sort data=a out=b; by id; where flag="Y"; run;'),
    list(emit_proc_sort, 'proc sort data=a out=b noduprecs; by id; run;'),
    list(emit_proc_freq, 'proc freq data=a(where=(flag="Y")) noprint; tables id / out=b; run;'),
    list(emit_proc_freq, 'proc freq data=a noprint; tables id / out=b(where=(count>1)); run;'),
    list(emit_proc_means, 'proc means data=a vardef=n noprint; var x; output out=b mean=y; run;'),
    list(emit_proc_means, 'proc means data=a noprint; var x; output out=b q1=q; run;'))
  for (case in cases) expect_true(is.na(case[[1]](sas_units(sas_statements(case[[2]])))$code), info = case[[2]])
  for (source in c('data b; set a; keep x1-x5; run;', 'data b; set a; rename x1-x3=y1-y3; run;',
                   'data b; set a; keep a,file.create("/tmp/unwanted"); run;'))
    expect_gt(nrow(parse_data_step(sas_units(sas_statements(source)))$blockers), 0L)
})

test_that("numeric and character missing conditions follow SAS truth rules", {
  x <- c(NA_real_, 0, 1, 2)
  y <- c(0, 1, NA_real_, 1)
  evaluate <- function(text) eval(parse(text = sas_cond_to_r(text)))
  expect_equal(evaluate("x"), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(evaluate("not x"), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(evaluate("x and y"), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(evaluate("x or y"), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(sas_missing(c(NA, "", "   ", "A", "\t")), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(eval(parse(text = translate_expr('missing(" ")'))), TRUE)
})

test_that("SAS string contents and macro default text survive conversion", {
  expect_identical(eval(parse(text = translate_expr('"C:\\new\\temp"'))), "C:\\new\\temp")
  expect_identical(eval(parse(text = translate_expr("'it''s {{subject}}'"))), "it's {{subject}}")
})

test_that("rounding and collation do not inherit the host locale", {
  expect_equal(sas_round(c(.15, .35, -.15, -.35), .1), c(.2, .4, -.2, -.4), tolerance = 1e-15)
  expect_error(sas_round(1, 0), "unit")
  expect_identical(sas_sort(data.frame(x = c("a", "B", "", NA)), "x")$x, c("", NA, "B", "a"))
  expect_true(chr_cmp("B", "a", "<"))
  expect_false(chr_cmp("A\t", "A", "=="))
})

test_that("dataset replacement and writes preserve one ordinary data frame", {
  x <- structure(data.frame(AVAL = c(1, 2)), class = c("sas2r_dataset", "data.frame"))
  x$aval <- c(3, 4)
  x[["aVaL"]] <- c(5, 6)
  expect_identical(names(x), "AVAL")
  expect_equal(x$AVAL, c(5, 6))
  skip_if_not_installed("dplyr")
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  dir <- withr::local_tempdir()
  e$.sas2r_registry <- list(work = list(path = dir, write = "rds"))
  e$lib_write(dplyr::group_by(data.frame(id = c(1, 1), x = 1:2), id), "work", "out")
  expect_identical(class(readRDS(file.path(dir, "out.rds"))), "data.frame")
})

test_that("merge keeps unmatched rows and compares composite keys exactly", {
  a <- data.frame(id = c(1, 2), x = c(10, 20))
  b <- data.frame(id = c(2, 3), y = c(30, 40))
  expect_equal(sas_merge(a, b, "id")$id, c(1, 2, 3))
  expect_equal(nrow(sas_merge(a[FALSE, ], b, "id")), 2L)
  a <- data.frame(x = 1, y = 1 + 2e-15, a = 1)
  b <- data.frame(x = 1, y = 1, b = 1)
  expect_equal(nrow(sas_merge(a, b, c("x", "y"))), 2L)
})

test_that("comparison rejects invalid profiles and magnitude-dependent default matches", {
  for (p in list(list(), list(numeric = list(abs = Inf, rel = Inf))))
    expect_error(compare_datasets(data.frame(x = 1), data.frame(x = 999), p))
  expect_error(compare_profile(abs = Inf))
  expect_error(compare_profile(overrides = list(x = list(rel = Inf))))
  expect_false(compare_datasets(data.frame(id = 100100101), data.frame(id = 100100102))$passed)
  expect_false(compare_datasets(data.frame(t = 1900000000), data.frame(t = 1900000015))$passed)
  expect_false(compare_datasets(data.frame(d = as.Date("2026-01-01")), data.frame(d = as.Date("2026-01-02")), compare_profile(abs = 1))$passed)
})

test_that("prompt substitution treats source braces as literal task data", {
  path <- tempfile(fileext = ".md")
  writeLines("Source: {{source}} Context: {{context}}", path)
  expect_identical(render_prompt(path, list(source = "{{subject}} {{context}}", context = "facts")),
                   "Source: {{subject}} {{context}} Context: facts")
  unlink(path)
})

test_that("code-only diagnostics exclude dataset-bearing stderr", {
  path <- tempfile()
  writeLines("PRIVATE_SUBJECT_123", path)
  x <- bounded_agent_diagnostics(list(stderr_path = path, condition = list(class = "error")))
  expect_false(grepl("PRIVATE_SUBJECT_123", jsonlite::toJSON(x, auto_unbox = TRUE), fixed = TRUE))
  expect_length(x$log_excerpt, 0L)
  unlink(path)
})

test_that("source encoding errors identify the file and line", {
  path <- tempfile(fileext = ".sas")
  writeBin(as.raw(c(0x64, 0x61, 0x74, 0x61, 0x20, 0xe9)), path)
  expect_error(read_sas_source(path), "line 1", class = "sas2r_source_encoding_error")
  writeLines("\ufeffdata a; run;", path, useBytes = TRUE)
  expect_identical(read_sas_source(path), "data a; run;")
  unlink(path)
})


test_that("passing outputs cannot hide an unrelated component's repair finding", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "adam"))
  saveRDS(data.frame(id = 1), file.path(dir, "adam", "out.rds"))
  contracts <- data.frame(target_id = "adam.out", target_key = "adam.out",
    logical_name = "adam.out", kind = "dataset", required = TRUE)
  graph <- list(nodes = tibble::tibble(node_id = c("producer", "other", "adam.out"),
    component_id = c("producer", "other", "adam.out"), type = c("source_unit", "source_unit", "final_output")),
    edges = tibble::tibble(from = "producer", to = "adam.out", type = "creates_dataset",
      resolution = "resolved", detail = "adam.out"))
  history <- function(id, verdict) {
    h <- new_component_evidence_history(id, new_component_binding(id, "r", "h", "p", "d"))
    h <- record_completed_review(h, verdict = verdict)
    if (identical(verdict, "repair_required")) h else
      promote_component_evidence(h, "runtime_verified", coverage = paste0("run:", id))
  }
  histories <- list(producer = history("producer", "reviewed_no_material_finding"),
    other = history("other", "repair_required"))
  attempt <- list(attempt_dir = dir, completed = TRUE, passed = TRUE, execution_order = c("producer", "other"))
  assessed <- assess_final_outputs(contracts, attempt, graph, histories)
  expect_true(assessed$all_required_passed)
  expect_false(assessed$status %in% c("migration_ready", "validated"))
  expect_true("repair_required" %in% assessed$lineage_evidence$blockers)
})

test_that("recorded runtime bindings survive selection and override stale locations", {
  dir <- withr::local_tempdir()
  paths <- init_migration_paths(dir, "binding-run")
  attempt <- init_attempt(paths, kind = "bundle")
  dynamic <- file.path(attempt$attempt_dir, "dynamic-adam")
  dir.create(dynamic)
  saveRDS(data.frame(id = 2), file.path(dynamic, "adsl.rds"))
  attempt <- complete_attempt(attempt, passed = TRUE, exit_status = 0L, output_dirs = list(adam = dynamic))
  selected <- select_attempt(paths, attempt, list(status = "needs_review"))
  expect_identical(selected$output_dirs$adam, dynamic)
  contract <- list(kind = "dataset", logical_name = "adam.adsl", target_key = "adam.adsl")
  expect_equal(find_attempt_candidate_file(contract, selected), normalizePath(file.path(dynamic, "adsl.rds")))
})

test_that("YAML budgets and execution timeouts reach the effective public configuration", {
  dir <- withr::local_tempdir()
  writeLines("proc mystery; run;", file.path(dir, "p.sas"))
  yaml::write_yaml(list(budget = list(max_calls = 0, max_request_chars = 1000),
    migration = list(smoke_timeout = 300, bundle_timeout = 900)), file.path(dir, "_sas2r.yml"))
  check <- sas_preflight(dir)
  expect_equal(check$budget$max_calls, 0)
  expect_equal(check$budget$max_request_chars, 1000)
  explicit <- sas_preflight(dir, usage_limits = list(max_calls = 2))
  expect_equal(explicit$budget$max_calls, 2)
  expect_equal(explicit$budget$max_request_chars, 1000)
  cfg <- sas_config(start = dir)
  expect_equal(cfg$migration$smoke_timeout, 300)
  expect_equal(cfg$migration$bundle_timeout, 900)
})

test_that("dependency caching invalidates for a changed graph", {
  ids <- sprintf("p%03d", seq_len(200))
  graph <- list(nodes = tibble::tibble(node_id = ids, component_id = ids,
    type = "source_unit", original_index = seq_along(ids)),
    edges = tibble::tibble(from = head(ids, -1), to = tail(ids, -1),
      type = "reads_dataset", resolution = "resolved", detail = "work.x"))
  expect_identical(dependency_closure(graph, ids[200]), ids[1:199])
  graph$edges <- graph$edges[-199, ]
  expect_length(dependency_closure(graph, ids[200]), 0L)
})

test_that("large tolerable residuals receive complete alignment", {
  ref <- data.frame(value = seq_len(150) / 100)
  cand <- ref[150:1, , drop = FALSE]
  cand$value <- cand$value + 1e-12
  result <- compare_datasets_aligned(ref, cand, profile = compare_profile(abs = 1e-8))
  expect_true(result$passed)
  expect_identical(result$structure$alignment_resource_state, "complete")
})

test_that("decimal rounding covers the review's SAS examples", {
  expect_equal(sas_round(c(1.005, 0.285, 1.15), c(.01, .01, .1)), c(1.01, .29, 1.2))
})


test_that("execution children omit provider secrets and report input mutation", {
  withr::local_envvar(OPENAI_API_KEY = "synthetic-review-key")
  child_key <- callr::r(function() Sys.getenv("OPENAI_API_KEY"),
    env = execution_process_env(), user_profile = FALSE, system_profile = FALSE)
  expect_identical(child_key, "")
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  input <- file.path(fx$root, "inputs", "input.rds")
  fx$state$selected_revisions$p01$r_code <- paste(fx$fixed$p01,
    sprintf("saveRDS(data.frame(id = 1, value = 99), %s)", deparse(input)), sep = "\n")
  result <- run_bundle_attempt(fx$state)
  expect_false(result$passed)
  expect_match(result$condition$message, "input files changed", fixed = TRUE)
})
