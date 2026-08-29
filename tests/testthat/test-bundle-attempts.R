bundle_attempt_fixture <- function(envir = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = envir)
  input_dir <- file.path(base, "inputs", "adam")
  dir.create(input_dir, recursive = TRUE)
  input_file <- file.path(input_dir, "adsl.rds")
  saveRDS(data.frame(USUBJID = c("01", "02"), TRT = c("A", "B"), stringsAsFactors = FALSE), input_file)

  # Two programs: prog_a creates work.t1 from adam.adsl, prog_b creates adam.out from work.t1
  prog_a <- file.path(base, "prog_a.sas")
  writeLines("data work.t1; set adam.adsl; run;", prog_a)
  prog_b <- file.path(base, "prog_b.sas")
  writeLines("data adam.out; set work.t1; run;", prog_b)

  config <- list(libraries = list(adam = list(path = input_dir, engine = "rds", write = "rds")))
  project <- sas_project(base, config = config)

  out_dir <- file.path(base, "migration_out")
  state <- new_migration_state(project, out_dir = out_dir, config = config, execute = TRUE)

  # Setup selected revisions with generated R code
  r_code_a <- paste(
    "t1 <- lib_read('adam', 'adsl')",
    "lib_write(t1, 'work', 't1')",
    sep = "\n"
  )
  r_code_b <- paste(
    "t1 <- lib_read('work', 't1')",
    "out <- transform(t1, DERIVED = 1)",
    "lib_write(out, 'adam', 'out')",
    sep = "\n"
  )

  state$selected_revisions <- list(
    prog_a = list(
      component_id = "prog_a",
      revision_id = "r1",
      r_code = r_code_a,
      staged_file = "prog_a.R",
      contract = list(component_id = "prog_a", staged_file = "prog_a.R", sas_text = "data work.t1; set adam.adsl; run;")
    ),
    prog_b = list(
      component_id = "prog_b",
      revision_id = "r1",
      r_code = r_code_b,
      staged_file = "prog_b.R",
      contract = list(component_id = "prog_b", staged_file = "prog_b.R", sas_text = "data adam.out; set work.t1; run;")
    )
  )

  list(
    base = base,
    input_file = input_file,
    input_dir = input_dir,
    project = project,
    state = state,
    paths = state$paths,
    expected_roots = c("prog_a", "prog_b"),
    binding = list(source_hash = "src_123", r_hash = "r_123")
  )
}

write_incomplete_attempt_fixture <- function(paths, sequence = 2L) {
  init_attempt(paths, kind = "bundle", sequence = sequence)
}

test_that("bundle attempts are fresh immutable and resume safely", {
  fx <- bundle_attempt_fixture()
  before <- input_hash_manifest(fx$project)
  a1 <- run_bundle_attempt(fx$state, sequence = 1L)

  expect_true(a1$completed)
  expect_identical(a1$execution_order, fx$expected_roots)
  expect_identical(input_hash_manifest(fx$project), before)

  broken <- write_incomplete_attempt_fixture(fx$paths, sequence = 2L)
  resumed <- resume_migration_attempts(fx$paths, fx$binding)
  expect_identical(resumed$latest_completed$attempt_id, a1$attempt_id)
  expect_false(broken$attempt_id %in% resumed$reusable_attempt_ids)
})

test_that("snapshot_selected_bundle copies programs, helpers, formats and emits attempt registry", {
  fx <- bundle_attempt_fixture()
  attempt <- init_attempt(fx$paths, kind = "bundle", sequence = 1L)

  snapshot <- snapshot_selected_bundle(fx$state, attempt)

  expect_true(dir.exists(attempt$bundle_dir))
  expect_true(file.exists(file.path(attempt$bundle_dir, "sas2r-helpers.R")))
  expect_true(file.exists(file.path(attempt$bundle_dir, "_sas2r_registry.R")))
  expect_true(file.exists(file.path(attempt$bundle_dir, "_sas2r_formats.R")))
  expect_true(file.exists(file.path(attempt$bundle_dir, "prog_a.R")))
  expect_true(file.exists(file.path(attempt$bundle_dir, "prog_b.R")))
})

test_that("build_bundle_execution_plan orders root entry points and avoids double running included modules", {
  fx <- bundle_attempt_fixture()
  plan <- build_bundle_execution_plan(fx$state$graph)

  expect_identical(plan$execution_order, fx$expected_roots)
  expect_true("prog_a" %in% plan$root_programs)
  expect_true("prog_b" %in% plan$root_programs)
})

test_that("select_attempt updates selected.json atomically for non-regressive candidate", {
  fx <- bundle_attempt_fixture()
  a1 <- run_bundle_attempt(fx$state, sequence = 1L)

  assessment_1 <- list(
    status = "migration_ready",
    assessed_targets = list(list(target_id = "adam.out", passed = TRUE)),
    passing_targets = c("adam.out")
  )

  sel1 <- select_attempt(fx$paths, candidate = a1, assessment = assessment_1)
  expect_true(file.exists(fx$paths$selected))
  expect_identical(sel1$attempt_id, a1$attempt_id)
  expect_identical(sel1$status, "migration_ready")

  # Regressive candidate with blocked status is rejected
  a2 <- init_attempt(fx$paths, kind = "bundle", sequence = 2L)
  a2_comp <- complete_attempt(a2, passed = FALSE, exit_status = 1L)
  assessment_regressive <- list(
    status = "blocked",
    assessed_targets = list(),
    passing_targets = character()
  )

  expect_error(
    select_attempt(fx$paths, candidate = a2_comp, assessment = assessment_regressive, previous = sel1),
    class = "sas2r_regressive_selection"
  )

  # selected.json still holds a1
  selected_on_disk <- jsonlite::fromJSON(fx$paths$selected)
  expect_identical(selected_on_disk$attempt_id, a1$attempt_id)
})

test_that("prune_rejected_attempt_outputs removes only rejected work and outputs", {
  fx <- bundle_attempt_fixture()
  a1 <- run_bundle_attempt(fx$state, sequence = 1L)
  a2 <- run_bundle_attempt(fx$state, sequence = 2L)

  assessment <- list(status = "migration_ready", passing_targets = c("adam.out"))
  select_attempt(fx$paths, candidate = a2, assessment = assessment)

  # Before pruning, both have work/outputs
  expect_true(dir.exists(a1$work_dir))
  expect_true(dir.exists(a2$work_dir))

  pruned <- prune_rejected_attempt_outputs(fx$paths)
  expect_identical(pruned$selected_attempt_id, a2$attempt_id)
  expect_true(a1$attempt_id %in% pruned$pruned_attempts)

  # a1 work/outputs pruned, but bundle/logs/record preserved
  expect_false(dir.exists(a1$work_dir))
  expect_true(dir.exists(a1$bundle_dir))
  expect_true(dir.exists(a1$logs_dir))
  expect_true(file.exists(file.path(a1$attempt_dir, "record.json")))

  # a2 (selected) intact
  expect_true(dir.exists(a2$work_dir))
})
