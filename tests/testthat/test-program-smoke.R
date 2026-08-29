callable_macro_fixture <- function(envir = parent.frame()) {
  base_dir <- withr::local_tempdir(.local_envir = envir)
  paths <- migration_paths(base_dir)
  init_migration_paths(base_dir)

  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)

  nodes <- tibble::tibble(
    node_id = c("node_macro_def", "node_caller_prog"),
    component_id = c("macro_def", "caller_prog"),
    type = c("source_unit", "source_unit"),
    source_file = c("macro_def.sas", "caller_prog.sas"),
    line = c(1L, 1L),
    original_index = c(1L, 2L),
    content_hash = c("hash_def", "hash_caller")
  )

  edges <- tibble::tibble(
    edge_id = c("edge_1"),
    from = c("node_macro_def"),
    to = c("node_caller_prog"),
    type = c("calls_macro"),
    resolution = c("resolved"),
    source_file = c("caller_prog.sas"),
    line = c(1L),
    detail = c("calc_total")
  )

  graph <- list(schema_version = "1", nodes = nodes, edges = edges)

  selected <- list(
    macro_def = "calc_total <- function(a, b) {\n  stop('first defect')\n}",
    caller_prog = "calc_total(1, 2)",
    unused_macro = "unused_macro <- function() {\n  stop('unused')\n}"
  )

  staged_dir <- file.path(attempt$attempt_dir, "staged")
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)
  write_helpers(staged_dir)

  reg_lines <- c(
    ".sas2r_registry <- list(",
    sprintf("  work = list(path = %s, engine = 'rds', write = 'rds')",
            deparse(attempt$work_dir)),
    ")"
  )
  writeLines(reg_lines, file.path(staged_dir, "_sas2r_registry.R"))

  runtime <- list(
    registry = file.path(staged_dir, "_sas2r_registry.R"),
    helpers = file.path(staged_dir, "sas2r-helpers.R")
  )

  list(
    base_dir = base_dir,
    paths = paths,
    attempt = attempt,
    attempt_dir = attempt$attempt_dir,
    graph = graph,
    selected = selected,
    runtime = runtime
  )
}

no_call_site_graph <- function() {
  nodes <- tibble::tibble(
    node_id = c("node_unused_macro"),
    component_id = c("unused_macro"),
    type = c("source_unit"),
    source_file = c("unused_macro.sas"),
    line = c(1L),
    original_index = c(1L),
    content_hash = c("hash_unused")
  )
  edges <- tibble::tibble(
    edge_id = character(),
    from = character(),
    to = character(),
    type = character(),
    resolution = character(),
    source_file = character(),
    line = integer(),
    detail = character()
  )
  list(schema_version = "1", nodes = nodes, edges = edges)
}

test_that("program smoke executes a real dependency prefix or defers honestly", {
  fx <- callable_macro_fixture()
  plan <- build_program_smoke_plan(fx$graph, "macro_def", fx$selected)
  expect_identical(plan$status, "runnable")
  expect_true(all(c("dependency_prefix", "call_site") %in% names(plan)))

  smoke <- run_program_smoke(plan, fx$runtime, fx$attempt_dir)
  expect_false(smoke$passed)
  expect_match(readLines(smoke$stderr_path), "first defect")

  deferred <- build_program_smoke_plan(
    no_call_site_graph(), "unused_macro", fx$selected
  )
  expect_identical(deferred$status, "deferred")
  expect_identical(deferred$reason, "no_callable_path")
})

test_that("program smoke defers when dependency is missing or execute is disabled", {
  fx <- callable_macro_fixture()

  # Missing dependency
  selected_incomplete <- list(caller_prog = "calc_total(1, 2)")
  plan_missing <- build_program_smoke_plan(fx$graph, "caller_prog", selected_incomplete)
  expect_identical(plan_missing$status, "deferred")
  expect_identical(plan_missing$reason, "missing_dependency")

  # Execute disabled
  plan_disabled <- build_program_smoke_plan(fx$graph, "macro_def", fx$selected, execute = FALSE)
  expect_identical(plan_disabled$status, "deferred")
  expect_identical(plan_disabled$reason, "execute_disabled")

  # Dynamic call unresolved
  nodes_dyn <- tibble::tibble(
    node_id = c("node_dyn_macro", "node_unres"),
    component_id = c("dyn_comp", "unresolved_comp"),
    type = c("source_unit", "unresolved_dependency"),
    source_file = c("dyn.sas", "unres.sas"),
    line = c(1L, 1L),
    original_index = c(1L, 2L),
    content_hash = c("h1", "h2")
  )
  edges_dyn <- tibble::tibble(
    edge_id = c("e_dyn"),
    from = c("node_unres"),
    to = c("node_dyn_macro"),
    type = c("calls_macro"),
    resolution = c("dynamic"),
    source_file = c("dyn.sas"),
    line = c(1L),
    detail = c("dyn_macro")
  )
  graph_dyn <- list(schema_version = "1", nodes = nodes_dyn, edges = edges_dyn)
  plan_dyn <- build_program_smoke_plan(graph_dyn, "dyn_comp", list(dyn_comp = "x <- 1"))
  expect_identical(plan_dyn$status, "deferred")
  expect_identical(plan_dyn$reason, "dynamic_call_unresolved")
})

test_that("program smoke stops at first defect in two-error program without claiming downstream", {
  fx <- callable_macro_fixture()
  fx$selected$macro_def <- paste(
    "calc_total <- function(a, b) {",
    "  x <- a + b",
    "  stop('first defect at step 1')",
    "  stop('second defect at step 2')",
    "}",
    sep = "\n"
  )

  plan <- build_program_smoke_plan(fx$graph, "macro_def", fx$selected)
  smoke <- run_program_smoke(plan, fx$runtime, fx$attempt_dir)

  expect_false(smoke$passed)
  expect_match(smoke$condition$message %||% paste(readLines(smoke$stderr_path), collapse = "\n"), "first defect at step 1")
  expect_no_match(paste(readLines(smoke$stderr_path), collapse = "\n"), "second defect at step 2")
})

test_that("program smoke passes when code executes successfully", {
  fx <- callable_macro_fixture()
  fx$selected$macro_def <- "calc_total <- function(a, b) { a + b }"

  plan <- build_program_smoke_plan(fx$graph, "macro_def", fx$selected)
  smoke <- run_program_smoke(plan, fx$runtime, fx$attempt_dir)

  expect_true(smoke$passed)
  expect_identical(smoke$exit_status, 0L)
  expect_true(file.exists(smoke$stdout_path))
  expect_true(file.exists(smoke$stderr_path))
})

test_that("bounded_agent_diagnostics respects code_only, bounded, and full policies", {
  fx <- callable_macro_fixture()
  plan <- build_program_smoke_plan(fx$graph, "macro_def", fx$selected)
  smoke <- run_program_smoke(plan, fx$runtime, fx$attempt_dir)

  # code_only policy
  diag_code <- bounded_agent_diagnostics(smoke, policy = "code_only")
  expect_identical(diag_code$policy, "code_only")
  expect_true(!is.null(diag_code$condition_message))
  expect_match(diag_code$condition_message, "first defect")
  expect_true(!is.null(diag_code$log_excerpt))
  expect_null(diag_code$dataset_rows)
  expect_null(diag_code$output_previews)

  # bounded policy
  diag_bounded <- bounded_agent_diagnostics(smoke, policy = "bounded")
  expect_identical(diag_bounded$policy, "bounded")
  expect_true(!is.null(diag_bounded$condition_message))
  expect_true(!is.null(diag_bounded$output_metadata))

  # full policy
  diag_full <- bounded_agent_diagnostics(smoke, policy = "full")
  expect_identical(diag_full$policy, "full")
  expect_true(!is.null(diag_full$stdout_path))
  expect_true(!is.null(diag_full$stderr_path))
})

test_that("program smoke emits progress events", {
  fx <- callable_macro_fixture()
  plan <- build_program_smoke_plan(fx$graph, "macro_def", fx$selected)

  events <- list()
  withCallingHandlers(
    smoke <- run_program_smoke(plan, fx$runtime, fx$attempt_dir),
    sas2r_progress = function(p) {
      events[[length(events) + 1L]] <<- p
    }
  )

  event_phases <- vapply(events, function(e) e$phase, character(1))
  event_statuses <- vapply(events, function(e) e$status %||% e$event %||% "", character(1))

  expect_true("smoke" %in% event_phases)
  expect_true(any(grepl("started", event_statuses)))
  expect_true(any(grepl("failed", event_statuses)))
})

test_that("program smoke handles multi-step dependency prefix and work outputs", {
  base_dir <- withr::local_tempdir()
  paths <- migration_paths(base_dir)
  init_migration_paths(base_dir)
  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)

  nodes <- tibble::tibble(
    node_id = c("n_setup", "n_mid", "n_target"),
    component_id = c("comp_setup", "comp_mid", "comp_target"),
    type = c("source_unit", "source_unit", "source_unit"),
    source_file = c("s.sas", "m.sas", "t.sas"),
    line = c(1L, 1L, 1L),
    original_index = c(1L, 2L, 3L),
    content_hash = c("h1", "h2", "h3")
  )

  edges <- tibble::tibble(
    edge_id = c("e1", "e2"),
    from = c("n_setup", "n_mid"),
    to = c("n_mid", "n_target"),
    type = c("reads_dataset", "reads_dataset"),
    resolution = c("resolved", "resolved"),
    source_file = c("m.sas", "t.sas"),
    line = c(1L, 1L),
    detail = c("ds_setup", "ds_mid")
  )
  graph <- list(schema_version = "1", nodes = nodes, edges = edges)

  staged_dir <- file.path(attempt$attempt_dir, "staged")
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)
  write_helpers(staged_dir)

  reg_lines <- c(
    ".sas2r_registry <- list(",
    sprintf("  work = list(path = %s, engine = 'rds', write = 'rds')",
            deparse(attempt$work_dir)),
    ")"
  )
  writeLines(reg_lines, file.path(staged_dir, "_sas2r_registry.R"))

  selected <- list(
    comp_setup = "val_setup <- 10",
    comp_mid = "val_mid <- val_setup * 2",
    comp_target = paste(
      "final_val <- val_mid + 5",
      "df <- data.frame(id = 1:3, result = rep(final_val, 3))",
      "lib_write(df, 'work', 'my_output')",
      sep = "\n"
    )
  )

  plan <- build_program_smoke_plan(graph, "comp_target", selected)
  expect_identical(plan$status, "runnable")
  expect_identical(plan$dependency_prefix, c("comp_setup", "comp_mid"))

  runtime <- list(
    registry = file.path(staged_dir, "_sas2r_registry.R"),
    helpers = file.path(staged_dir, "sas2r-helpers.R")
  )

  smoke <- run_program_smoke(plan, runtime, attempt$attempt_dir)
  expect_true(smoke$passed)
  expect_identical(smoke$exit_status, 0L)
  expect_true("my_output.rds" %in% names(smoke$output_hashes))
  expect_identical(smoke$executed_component_ids, c("comp_setup", "comp_mid", "comp_target"))

  # Test bounded diagnostics preview of generated dataset
  diag_bounded <- bounded_agent_diagnostics(smoke, policy = "bounded")
  expect_true("my_output" %in% names(diag_bounded$output_metadata))
  expect_identical(diag_bounded$output_metadata$my_output$row_count, 3L)
  expect_identical(diag_bounded$output_metadata$my_output$columns, c("id", "result"))
})

test_that("build_program_smoke_plan and run_program_smoke validate arguments", {
  expect_error(build_program_smoke_plan(list(), "", list()), class = "sas2r_invalid_argument")
  expect_error(run_program_smoke(list(), list(), "attempt_dir"), class = "sas2r_invalid_argument")
})
