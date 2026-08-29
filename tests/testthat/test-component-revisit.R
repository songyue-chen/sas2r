# Unit tests for graph-driven targeted component revisit

component_revisit_fixture <- function(envir = parent.frame()) {
  base_dir <- withr::local_tempdir(.local_envir = envir)
  paths <- migration_paths(base_dir)
  init_migration_paths(base_dir)

  # Program A produces data work.ds_a
  # Program B reads data work.ds_a and produces work.ds_b
  # Program C is independent (produces work.ds_c)
  writeLines(
    c("data work.ds_a; x = 1; run;"),
    file.path(base_dir, "comp_a.sas")
  )
  writeLines(
    c("data work.ds_b; set work.ds_a; y = x * 2; run;"),
    file.path(base_dir, "comp_b.sas")
  )
  writeLines(
    c("data work.ds_c; z = 99; run;"),
    file.path(base_dir, "comp_c.sas")
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

  # Selected code
  code_a_r1 <- "ds_a <- data.frame(x = 1:3)\nstop('bug in comp_a r1')\nlib_write(ds_a, 'work', 'ds_a')"
  code_a_r2 <- "ds_a <- data.frame(x = 1:3)\nlib_write(ds_a, 'work', 'ds_a')"
  code_b_r1 <- "ds_a <- lib_read('work', 'ds_a')\nds_b <- ds_a\nds_b$y <- ds_b$x * 2\nlib_write(ds_b, 'work', 'ds_b')"
  code_c_r1 <- "ds_c <- data.frame(z = 99)\nlib_write(ds_c, 'work', 'ds_c')"

  # Reviewer Mock
  reviewer_calls <- list()
  reviewer_fn <- function(request) {
    cid <- request$audit_context$component_id %||% "unknown"
    reviewer_calls[[length(reviewer_calls) + 1L]] <<- cid
    valid_program_review_response(
      verdict = "reviewed_no_material_finding",
      static_runnability = "looks_runnable"
    )
  }
  reviewer_llm <- recording_reviewer(reviewer_fn)

  # Fixer Mock
  fixer_fn <- function(request) {
    cid <- request$audit_context$component_id %||% "comp_a"
    if (cid == "comp_a") {
      valid_program_fix_response(
        code = code_a_r2,
        diagnosis = "fixed bug in comp_a",
        summary = "removed stop call",
        evidence_ids = request$evidence_ids %||% character()
      )
    } else {
      valid_program_fix_response(
        code = code_b_r1,
        diagnosis = "ok",
        summary = "ok",
        evidence_ids = request$evidence_ids %||% character()
      )
    }
  }
  fixer_llm <- recording_fixer(fixer_fn)

  # Setup initial revisions on disk
  setup_comp <- function(cid, code, sas_text) {
    r_dir <- file.path(paths$programs, cid, "revisions", "r1")
    dir.create(r_dir, recursive = TRUE, showWarnings = FALSE)
    r_path <- file.path(r_dir, "program.R")
    c_path <- file.path(r_dir, "contract.json")
    writeLines(code, r_path)

    b <- new_component_binding(
      source_hash = migration_hash(sas_text),
      r_hash = migration_hash(code),
      helper_hash = migration_hash(""),
      prompt_skill_hash = migration_hash("translator"),
      dependency_closure_hash = migration_hash("closure")
    )
    cntr <- new_behavioral_contract(component_id = cid, binding = b)
    atomic_write_json(cntr, c_path)
    hist <- new_component_evidence_history(cid, binding = b)
    list(
      selected = list(
        component_id = cid,
        revision_id = "r1",
        r_code = code,
        r_path = r_path,
        contract = cntr,
        contract_path = c_path,
        binding = b,
        status = "ok"
      ),
      history = hist
    )
  }

  ca <- setup_comp("comp_a", code_a_r1, "data work.ds_a; x = 1; run;")
  cb <- setup_comp("comp_b", code_b_r1, "data work.ds_b; set work.ds_a; y = x * 2; run;")
  cc <- setup_comp("comp_c", code_c_r1, "data work.ds_c; z = 99; run;")

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
      comp_a = ca$selected,
      comp_b = cb$selected,
      comp_c = cc$selected
    ),
    histories = list(
      comp_a = ca$history,
      comp_b = cb$history,
      comp_c = cc$history
    )
  )

  list(
    base_dir = base_dir,
    paths = paths,
    attempt = attempt,
    project = project,
    graph = graph,
    schedule = schedule,
    runtime = runtime,
    state = state,
    get_reviewer_calls = function() reviewer_calls
  )
}

test_that("downstream components are requeued when upstream revision changes", {
  fx <- component_revisit_fixture()

  result <- run_program_pipeline(
    fx$state, max_program_repair_rounds = 1L, execute = TRUE
  )

  # comp_a was fixed to r2
  expect_identical(result$selected_revisions$comp_a$revision_id, "r2")
  # comp_b (dependent on comp_a) was requeued and evaluated with comp_a r2
  ev_b <- current_component_evidence(result$histories$comp_b)
  expect_identical(ev_b$level, "runtime_verified")
  # comp_c (independent of comp_a) reached runtime_verified
  ev_c <- current_component_evidence(result$histories$comp_c)
  expect_identical(ev_c$level, "runtime_verified")
})

test_that("requeue_components correctly filters to affected downstream or deferred components", {
  fx <- component_revisit_fixture()

  old_hashes <- c(
    comp_a = "hash_a_old",
    comp_b = "hash_b_old",
    comp_c = "hash_c_old"
  )

  # If only comp_a changed
  new_hashes <- c(
    comp_a = "hash_a_new",
    comp_b = "hash_b_old",
    comp_c = "hash_c_old"
  )

  requeued <- requeue_components(fx$graph, old_hashes, new_hashes)
  # comp_a changed, comp_b depends on comp_a -> both requeued; comp_c not requeued
  expect_true("comp_a" %in% requeued)
  expect_true("comp_b" %in% requeued)
  expect_false("comp_c" %in% requeued)
})

test_that("revisit does not trigger unnecessary fixer calls when component is clean", {
  fx <- component_revisit_fixture()
  # Set comp_a initial code to be clean already
  good_a <- "ds_a <- data.frame(x = 1:3)\nlib_write(ds_a, 'work', 'ds_a')"
  fx$state$selected_revisions$comp_a$r_code <- good_a
  writeLines(good_a, fx$state$selected_revisions$comp_a$r_path)

  result <- run_program_pipeline(
    fx$state, max_program_repair_rounds = 1L, execute = TRUE
  )

  # All 3 components reach runtime_verified in single pass without repairs
  expect_identical(result$selected_revisions$comp_a$revision_id, "r1")
  expect_identical(result$selected_revisions$comp_b$revision_id, "r1")
  expect_identical(result$selected_revisions$comp_c$revision_id, "r1")
  expect_identical(current_component_evidence(result$histories$comp_a)$level, "runtime_verified")
  expect_identical(current_component_evidence(result$histories$comp_b)$level, "runtime_verified")
  expect_identical(current_component_evidence(result$histories$comp_c)$level, "runtime_verified")
})
