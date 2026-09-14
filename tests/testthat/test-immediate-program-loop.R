# Unit tests for immediate program review, smoke, and repair loop

immediate_loop_fixture <- function(
  review_issue = TRUE,
  smoke_failure = TRUE,
  two_error = FALSE,
  envir = parent.frame()
) {
  base_dir <- withr::local_tempdir(.local_envir = envir)
  paths <- migration_paths(base_dir)
  init_migration_paths(base_dir)

  writeLines(
    c("data work.prog; set work.in; x = x + 1; run;"),
    file.path(base_dir, "prog.sas")
  )
  project <- sas_project(base_dir)
  graph <- build_dependency_graph(project)
  schedule <- stable_dependency_schedule(graph)

  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)

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

  # Initial r1 code
  r1_code <- if (isTRUE(smoke_failure)) {
    if (isTRUE(two_error)) {
      paste(
        "prog <- data.frame(x = 1:5)",
        "stop('stopping defect 1')",
        "stop('downstream defect 2')",
        "lib_write(prog, 'work', 'prog')",
        sep = "\n"
      )
    } else {
      paste(
        "prog <- data.frame(x = 1:5)",
        "stop('smoke runtime failure in r1')",
        "lib_write(prog, 'work', 'prog')",
        sep = "\n"
      )
    }
  } else {
    paste(
      "prog <- data.frame(x = 1:5)",
      "prog$x <- prog$x + 1",
      "lib_write(prog, 'work', 'prog')",
      sep = "\n"
    )
  }

  # Reviewer Mock
  review_call_count <- 0L
  reviewer_fn <- function(request) {
    review_call_count <<- review_call_count + 1L
    if (isTRUE(review_issue) && review_call_count == 1L) {
      material_review_response(
        sas_evidence = "data step adds 1",
        r_evidence = "R code may drop missing values",
        affected_outputs = "work.prog"
      )
    } else {
      valid_program_review_response(
        verdict = "reviewed_no_material_finding",
        static_runnability = "looks_runnable"
      )
    }
  }
  reviewer_llm <- recording_reviewer(reviewer_fn)

  # Fixer Mock
  fix_call_count <- 0L
  fixer_fn <- function(request) {
    fix_call_count <<- fix_call_count + 1L
    if (isTRUE(two_error) && fix_call_count == 1L) {
      # Round 1 fixes defect 1 but still has defect 2
      r2_code <- paste(
        "prog <- data.frame(x = 1:5)",
        "stop('downstream defect 2')",
        "lib_write(prog, 'work', 'prog')",
        sep = "\n"
      )
      valid_program_fix_response(
        code = r2_code,
        diagnosis = "fixed stopping defect 1",
        summary = "removed first stop call",
        evidence_ids = request$evidence_ids %||% character()
      )
    } else {
      # Fixes all defects
      good_code <- paste(
        "prog <- data.frame(x = 1:5)",
        "prog$x <- prog$x + 1",
        "lib_write(prog, 'work', 'prog')",
        sep = "\n"
      )
      valid_program_fix_response(
        code = good_code,
        diagnosis = "fixed all defects and handled missing values",
        summary = "clean working implementation",
        evidence_ids = request$evidence_ids %||% character()
      )
    }
  }
  fixer_llm <- recording_fixer(fixer_fn)

  # Initial pre-generated revision r1
  rev_dir <- file.path(paths$component_revisions, "prog", "revisions", "r1")
  dir.create(rev_dir, recursive = TRUE, showWarnings = FALSE)
  r1_path <- file.path(rev_dir, "program.R")
  contract1_path <- file.path(rev_dir, "contract.json")
  writeLines(r1_code, r1_path)

  binding1 <- new_component_binding(
    source_hash = migration_hash("data work.prog; set work.in; x = x + 1; run;"),
    r_hash = migration_hash(r1_code),
    helper_hash = migration_hash(""),
    prompt_skill_hash = migration_hash("translator"),
    dependency_closure_hash = migration_hash("closure")
  )
  contract1 <- new_behavioral_contract(
    component_id = "prog",
    writes = c("work.prog"),
    binding = binding1
  )
  atomic_write_json(contract1, contract1_path)

  history1 <- new_component_evidence_history("prog", binding = binding1)

  state <- list(
    project = project,
    graph = graph,
    schedule = schedule,
    paths = paths,
    runtime = runtime,
    reviewer_llm = reviewer_llm,
    fixer_llm = fixer_llm,
    translator_llm = NULL,
    usage_budget = new_usage_budget(),
    config = list(),
    events = character(),
    selected_revisions = list(
      prog = list(
        component_id = "prog",
        revision_id = "r1",
        r_code = r1_code,
        r_path = r1_path,
        contract = contract1,
        contract_path = contract1_path,
        binding = binding1,
        status = "ok"
      )
    ),
    histories = list(prog = history1),
    active_revision = "r1"
  )

  list(
    base_dir = base_dir,
    paths = paths,
    attempt = attempt,
    project = project,
    graph = graph,
    runtime = runtime,
    state = state
  )
}

test_that("each program is reviewed and repaired immediately", {
  fx <- immediate_loop_fixture(review_issue = TRUE, smoke_failure = TRUE)
  result <- run_program_pipeline(
    fx$state, max_program_repair_rounds = 1L, execute = TRUE
  )
  expect_identical(result$events, c(
    "generated:r1", "mechanical_pass:r1", "reviewed:r1", "smoke_failed:r1",
    "fixed:r2", "mechanical_pass:r2", "reviewed:r2", "smoke_passed:r2"
  ))
  expect_identical(result$active_revision, "r2")
})

test_that("two-error program fixes first defect in round 1 and second defect in round 2", {
  # With 1 round configured: round 1 fixes bug 1, exposes bug 2, leaving usable non-ready code
  fx1 <- immediate_loop_fixture(review_issue = FALSE, smoke_failure = TRUE, two_error = TRUE)
  res1 <- run_program_pipeline(
    fx1$state, max_program_repair_rounds = 1L, execute = TRUE
  )
  expect_identical(res1$active_revision, "r2")
  expect_identical(res1$events, c(
    "generated:r1", "mechanical_pass:r1", "reviewed:r1", "smoke_failed:r1",
    "fixed:r2", "mechanical_pass:r2", "reviewed:r2", "smoke_failed:r2"
  ))
  # With 1 round exhausted, evidence has not reached runtime_verified
  ev1 <- current_component_evidence(res1$histories$prog)
  expect_false(identical(ev1$level, "runtime_verified"))

  # With 2 rounds configured: round 2 fixes bug 2 and reaches runtime evidence
  fx2 <- immediate_loop_fixture(review_issue = FALSE, smoke_failure = TRUE, two_error = TRUE)
  res2 <- run_program_pipeline(
    fx2$state, max_program_repair_rounds = 2L, execute = TRUE
  )
  expect_identical(res2$active_revision, "r3")
  expect_identical(res2$events, c(
    "generated:r1", "mechanical_pass:r1", "reviewed:r1", "smoke_failed:r1",
    "fixed:r2", "mechanical_pass:r2", "reviewed:r2", "smoke_failed:r2",
    "fixed:r3", "mechanical_pass:r3", "reviewed:r3", "smoke_passed:r3"
  ))
  ev2 <- current_component_evidence(res2$histories$prog)
  expect_identical(ev2$level, "runtime_verified")
})

test_that("clean program without review or smoke issue skips fixer and reaches runtime evidence", {
  fx <- immediate_loop_fixture(review_issue = FALSE, smoke_failure = FALSE)
  result <- run_program_pipeline(
    fx$state, max_program_repair_rounds = 1L, execute = TRUE
  )
  expect_identical(result$events, c(
    "generated:r1", "mechanical_pass:r1", "reviewed:r1", "smoke_passed:r1"
  ))
  expect_identical(result$active_revision, "r1")
  ev <- current_component_evidence(result$histories$prog)
  expect_identical(ev$level, "runtime_verified")
})

test_that("execute = FALSE performs review, skips runtime smoke and defers with execute_disabled", {
  fx <- immediate_loop_fixture(review_issue = FALSE, smoke_failure = TRUE)
  result <- run_program_pipeline(
    fx$state, max_program_repair_rounds = 1L, execute = FALSE
  )
  expect_identical(result$events, c(
    "generated:r1", "mechanical_pass:r1", "reviewed:r1", "smoke_deferred:r1"
  ))
  expect_identical(result$active_revision, "r1")
  ev <- current_component_evidence(result$histories$prog)
  expect_identical(ev$level, "reviewed_only")
  expect_identical(ev$runtime_deferred, "execute_disabled")
})

test_that("a crashing repair cannot replace executable code or erase its finding", {
  fx <- immediate_loop_fixture(review_issue = TRUE, smoke_failure = FALSE)
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
    code = "stop('new defect introduced by repair')", evidence_ids = req$evidence_ids))
  result <- run_program_pipeline(fx$state, max_program_repair_rounds = 1L, execute = TRUE)
  expect_identical(result$active_revision, "r1")
  expect_true("repair_rejected:r2" %in% result$events)
  expect_identical(component_review_verdict(result$histories$prog), "repair_required")
  expect_true("repair_required" %in% current_component_evidence(result$histories$prog)$blockers)
  expect_length(result$histories$prog$revisions, 2L)
  candidate <- result$histories$prog$revisions[[2L]]
  smoke_events <- Filter(function(ev) identical(ev$type, "program_smoke"), candidate$events)
  expect_identical(smoke_events[[1]]$status, "failed")
  expect_match(smoke_events[[1]]$condition$message, "new defect introduced by repair")
  expect_true(file.exists(result$selected_revisions$prog$smoke$stderr_path))
})

test_that("upstream execution failure blocks the consumer without invoking its fixer", {
  fx <- immediate_loop_fixture(review_issue = FALSE, smoke_failure = FALSE)
  plan <- list(status = "runnable", component_id = "prog", dependency_prefix = "upstream",
    selected_revisions = list(upstream = "stop('upstream calculation failed')", prog = "out <- 1"))
  testthat::local_mocked_bindings(build_program_smoke_plan = function(...) plan)
  called <- FALSE
  fx$state$fixer_llm <- recording_fixer(function(req) { called <<- TRUE; valid_program_fix_response() })
  result <- process_program_component(fx$state, "prog")
  expect_false(called)
  expect_identical(result$selected_revisions$prog$smoke$blocked_by, "upstream")
  expect_identical(result$selected_revisions$prog$smoke$failed_component_id, "upstream")
  expect_identical(result$selected_revisions$prog$smoke$executed_component_ids, character())
  events <- current_component_evidence(result$histories$prog)$events
  smoke <- Filter(function(e) identical(e$type, "program_smoke"), events)[[1]]
  expect_identical(smoke$status, "blocked")
  expect_true("smoke_blocked:r1" %in% result$events)
  expect_false("smoke_failed:r1" %in% result$events)
  blockers <- current_component_evidence(result$histories$prog)$blockers
  expect_true("blocked_by:upstream" %in% blockers)
  expect_false("smoke_failed" %in% blockers)
})

test_that("mechanical helper failures reach the fixer before smoke execution", {
  fx <- immediate_loop_fixture(review_issue = FALSE, smoke_failure = FALSE)
  code <- 'stop("must not execute"); x <- sas_merge(a, b, by = "id", all.x = TRUE)'
  fx$state$selected_revisions$prog$r_code <- code
  writeLines(code, fx$state$selected_revisions$prog$r_path)
  result <- process_program_component(fx$state, "prog", max_program_repair_rounds = 1L)
  expect_true("smoke_deferred:r1" %in% result$events)
  expect_false("smoke_failed:r1" %in% result$events)
  expect_identical(result$active_revision, "r2")
  request <- fx$state$fixer_llm$requests()[[1]]
  prompt <- paste(vapply(request$messages, `[[`, character(1), "content"), collapse = "\n")
  expect_match(prompt, "Mechanical Check Failure", fixed = TRUE)
  expect_match(prompt, "unused argument (all.x = TRUE)", fixed = TRUE)
})
