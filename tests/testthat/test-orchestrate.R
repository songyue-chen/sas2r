# Tests for dependency-aware migration orchestration

mixed_project <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(c("data w1; set w.root; where sex = 'M'; c = x * 2; run;",
               "data w2; set work.w1; retain csum 0; csum = csum + c; run;"),
             file.path(dir, "run.sas"))
  sas_project(dir)
}

test_that("new_migration_state initializes canonical directories and records", {
  dir <- withr::local_tempdir()
  writeLines(
    c("data work.a; x = 1; run;", "data work.b; set work.a; y = x * 2; run;"),
    file.path(dir, "run.sas")
  )
  project <- sas_project(dir)
  out <- withr::local_tempdir()

  state <- new_migration_state(project, out)
  expect_s3_class(state, "sas2r_migration_state")
  expect_true(dir.exists(state$paths$state))
  expect_true(dir.exists(state$paths$programs))
  expect_true(dir.exists(state$paths$attempts))
  expect_true(dir.exists(state$paths$outputs))
  expect_true(!is.null(state$graph$nodes))
  expect_true(nrow(state$schedule) >= 1L)
  expect_true(!is.null(state$attempt))
  expect_identical(state$attempt$sequence, 1L)
})

test_that("process_program_component generates, checks, reviews, and smokes program", {
  dir <- withr::local_tempdir()
  writeLines("data work.out; x = 10; run;", file.path(dir, "calc.sas"))
  project <- sas_project(dir)
  out <- withr::local_tempdir()

  state <- new_migration_state(project, out)

  # Pre-populate selected revision with working code
  r_dir <- file.path(state$paths$programs, "calc", "revisions", "r1")
  dir.create(r_dir, recursive = TRUE, showWarnings = FALSE)
  r_path <- file.path(r_dir, "program.R")
  c_path <- file.path(r_dir, "contract.json")
  code <- "out <- data.frame(x = 10)\nlib_write(out, 'work', 'out')"
  writeLines(code, r_path)

  b <- new_component_binding(
    source_hash = migration_hash("data work.out; x = 10; run;"),
    r_hash = migration_hash(code),
    helper_hash = migration_hash(""),
    prompt_skill_hash = migration_hash("translator"),
    dependency_closure_hash = migration_hash("closure")
  )
  cntr <- new_behavioral_contract(component_id = "calc", writes = "work.out", binding = b)
  atomic_write_json(cntr, c_path)
  hist <- new_component_evidence_history("calc", binding = b)

  state$selected_revisions$calc <- list(
    component_id = "calc",
    revision_id = "r1",
    r_code = code,
    r_path = r_path,
    contract = cntr,
    contract_path = c_path,
    binding = b,
    status = "ok"
  )
  state$histories$calc <- hist

  # Reviewer returning clean review
  state$reviewer_llm <- recording_reviewer(function(req) {
    valid_program_review_response(verdict = "reviewed_no_material_finding")
  })

  updated_state <- process_program_component(state, "calc", execute = TRUE, max_program_repair_rounds = 1L)
  expect_identical(updated_state$active_revision, "r1")
  expect_true("mechanical_pass:r1" %in% updated_state$events)
  expect_true("reviewed:r1" %in% updated_state$events)
  expect_true("smoke_passed:r1" %in% updated_state$events)

  ev <- current_component_evidence(updated_state$histories$calc)
  expect_identical(ev$level, "runtime_verified")
})

