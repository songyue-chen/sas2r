test_that("an inconsistent reference cannot authorize a code change", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  ref <- file.path(fx$root, "reference.rds")
  saveRDS(data.frame(id = 1:2, value = c(91, 92)), ref)
  fx$state$comparison_rules <- list(references = list(work.out1 = ref))
  result <- run_bundle_pipeline(fx$state)
  expect_length(fx$state$fixer_llm$requests(), 0L)
  expect_length(fx$state$reviewer_llm$requests(), 1L)
  expect_identical(result$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_identical(result$attempts$sequence, 1L)
  expect_identical(result$status, "blocked")
  expect_identical(result$assessment$targets$work.out1$reference_issue,
                   "reference mismatch; cause unresolved")
})

test_that("bundle candidate review rejects a harmful repair before another run", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
    code = sub("+ 1", "+ 99", fx$fixed$p01, fixed = TRUE)))
  fx$state$reviewer_llm <- recording_reviewer(function(req) material_review_response(
    sas_evidence = "value = value + 1", r_evidence = "value + 99 contradicts source"))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$selected_revisions$p01$r_code,
                   fx$state$selected_revisions$p01$r_code)
  expect_identical(component_review_verdict(result$histories$p01),
                   "reviewed_no_material_finding")
  expect_identical(result$attempts$sequence, 1L)
  expect_true(length(result$diagnostics$rejected_repairs) > 0L)
})

test_that("a source correction may lose agreement with an incorrect reference", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer(), value_errors = 1L)
  fx$state$histories$p01 <- record_completed_review(fx$state$histories$p01,
    verdict = "repair_required", basis_id = "wrong-addition", findings = list(list(
      severity = "material", sas_evidence = "value = value + 1",
      r_evidence = "value + 9", affected_outputs = "work.out1")))
  ref <- file.path(fx$root, "reference.rds")
  saveRDS(data.frame(id = 1:3, value = 19:21), ref)
  fx$state$comparison_rules <- list(references = list(work.out1 = ref))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_identical(result$selected_attempt$attempt_id, "bundle_attempt_002")
  expect_identical(result$status, "blocked")
  out <- readRDS(file.path(result$selected_attempt$attempt_dir, "work", "out1.rds"))
  expect_equal(out$value, 11:13)
})

test_that("selection rejects source regressions at equal and improved reference status", {
  for (match_reference in c(FALSE, TRUE)) {
    fx <- repair_workflow_fixture(n = 1L, failures = integer())
    ref <- file.path(fx$root, "reference.rds")
    saveRDS(data.frame(id = 1:3, value = if (match_reference) 109:111 else 209:211), ref)
    rules <- list(references = list(work.out1 = ref))
    first <- run_bundle_attempt(fx$state, sequence = 1L)
    before <- assess_final_outputs(fx$state$output_contracts, first, fx$state$graph,
      fx$state$histories, rules)
    selected <- select_attempt(fx$state$paths, first, before)
    candidate <- stage_workflow_revision(fx$state, "p01",
      sub("+ 1", "+ 99", fx$fixed$p01, fixed = TRUE), "repair_required")
    second <- run_bundle_attempt(candidate, sequence = 2L)
    after <- assess_final_outputs(candidate$output_contracts, second, candidate$graph,
      candidate$histories, rules)
    expect_identical(before$status, "blocked")
    expect_identical(after$status, if (match_reference) "needs_review" else "blocked")
    expect_error(select_attempt(candidate$paths, second, after, selected),
                 "p01 review regressed", fixed = TRUE)
  }
})

test_that("an already blocked lineage cannot hide another component regression", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  fx$state$histories$p01 <- record_completed_review(fx$state$histories$p01, "repair_required")
  first <- run_bundle_attempt(fx$state, sequence = 1L)
  before <- assess_final_outputs(fx$state$output_contracts, first, fx$state$graph, fx$state$histories)
  selected <- select_attempt(fx$state$paths, first, before)
  candidate <- stage_workflow_revision(fx$state, "p02",
    sub("+ 1", "+ 99", fx$fixed$p02, fixed = TRUE), "repair_required")
  second <- run_bundle_attempt(candidate, sequence = 2L)
  after <- assess_final_outputs(candidate$output_contracts, second, candidate$graph, candidate$histories)
  expect_true(before$lineage_evidence$is_blocked)
  expect_true(after$lineage_evidence$is_blocked)
  expect_error(select_attempt(candidate$paths, second, after, selected), "p02 review regressed", fixed = TRUE)
})

test_that("helper rejection retains files, all consumers and inactive review evidence", {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L)
  old_path <- fx$state$runtime$helpers
  old_text <- readLines(old_path, warn = FALSE)
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
    code = fx$fixed$p01, bundle_helper_patch = list(path = "sas2r-helpers.R",
      content = paste(c(old_text, "# wrong shared behavior"), collapse = "\n"), reason = "repair helper")))
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    if (req$component_id == "p02") material_review_response(
      sas_evidence = "value = value + 1", r_evidence = "shared helper changes source behavior")
    else valid_program_review_response()
  })
  result <- run_bundle_pipeline(fx$state)
  expect_identical(readLines(old_path, warn = FALSE), old_text)
  expect_identical(result$runtime$helpers, old_path)
  expect_identical(result$attempts$sequence, 1L)
  for (cid in fx$ids) {
    expect_identical(result$selected_revisions[[cid]]$binding, fx$state$selected_revisions[[cid]]$binding)
    expect_identical(component_review_verdict(result$histories[[cid]]), "reviewed_no_material_finding")
    expect_gt(length(result$histories[[cid]]$revisions), 1L)
  }
  expect_true(file.exists(result$diagnostics$rejected_repairs[[1L]]$helper_path))
  expect_length(fx$state$reviewer_llm$requests(), 2L)
})

test_that("comparison tool overrides do not reopen an authoring route", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, ".sas2r", "agents"), recursive = TRUE)
  for (role in c("translator", "fixer", "reviewer")) {
    yaml::write_yaml(list(name = "custom-display-name", tools = list(read_comparison_report = list())),
      file.path(root, ".sas2r", "agents", paste0(role, ".yml")))
    spec <- load_agent_specs(root)[[role]]
    tools <- build_tools(spec, list(agent_role = role))
    expect_false("read_comparison_report" %in% names(tools))
  }
  reference <- data.frame(id = 1:2, value = c(1, 987.65))
  report <- compare_aligned_outputs(reference, transform(reference, value = value + 1),
    target = list(target_id = "work.out1", logical_dataset = "work.out1", role = "output"))
  expect_true(length(report$examples) > 0L)
})

test_that("a review error after a helper write leaves the retained runtime intact", {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L)
  old_path <- fx$state$runtime$helpers
  old_text <- readLines(old_path, warn = FALSE)
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
    code = fx$fixed$p01, bundle_helper_patch = list(path = "sas2r-helpers.R",
      content = paste(c(old_text, "# proposed helper change"), collapse = "\n"), reason = "repair helper")))
  local_mocked_bindings(review_program_revision = function(...) stop("review service unavailable"))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(readLines(old_path, warn = FALSE), old_text)
  expect_identical(result$runtime$helpers, old_path)
  expect_identical(result$selected_revisions, fx$state$selected_revisions)
  expect_length(fx$state$fixer_llm$requests(), 1L)
  expect_match(result$diagnostics$rejected_repairs[[1L]]$errors,
               "review service unavailable", fixed = TRUE)
  expect_true(file.exists(result$diagnostics$rejected_repairs[[1L]]$helper_path))
})

test_that("a legitimate source ID filter survives an inconsistent reference", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  sas <- "data work.out1; set raw.input; if id in (1, 3); value=value+1; run;"
  writeLines(sas, file.path(fx$root, "p01.sas"))
  project <- sas_project(fx$root, config = fx$state$config)
  state <- new_migration_state(project, file.path(fx$root, "filtered"), config = fx$state$config)
  state$selected_revisions <- fx$state$selected_revisions
  code <- sub("x$value <-", "x <- x[x$id %in% c(1, 3), ]; x$value <-", fx$fixed$p01, fixed = TRUE)
  state$selected_revisions$p01$r_code <- code
  state$selected_revisions$p01$contract$sas_text <- sas
  state$fixer_llm <- fx$state$fixer_llm
  state$reviewer_llm <- fx$state$reviewer_llm
  state$output_contracts <- infer_output_contracts(project, overrides = list(datasets = "work.out1"))
  ref <- file.path(fx$root, "ref.rds")
  saveRDS(data.frame(id = 1:3, value = 11:13), ref)
  state$comparison_rules <- list(references = list(work.out1 = ref))
  result <- run_bundle_pipeline(state)
  expect_identical(result$selected_revisions$p01$r_code, code)
  expect_length(state$fixer_llm$requests(), 0L)
  out <- readRDS(file.path(result$selected_attempt$attempt_dir, "work", "out1.rds"))
  expect_equal(out$id, c(1L, 3L))
  expect_equal(out$value, c(11L, 13L))
  expect_identical(result$status, "blocked")
})

test_that("focused review survives reference path changes and checkpoint resume", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  reference <- file.path(fx$root, "reference.rds")
  saveRDS(data.frame(id = 1:2, value = 91:92), reference)
  fx$state$config$comparison_rules <- list(references = list(work.out1 = reference))
  fx$state$config$outputs <- list(datasets = "work.out1", references = list(work.out1 = reference))
  fx$state$comparison_rules <- fx$state$config$comparison_rules
  fx$state$output_contracts$reference_path <- reference
  fingerprint <- migration_resume_fingerprint(fx$state)
  result <- run_bundle_pipeline(fx$state)
  result$bundle_dir <- file.path(fx$root, "editable")
  materialize_user_bundle(file.path(result$selected_attempt$attempt_dir, "bundle"),
                          result$bundle_dir, result$project)
  write_migration_checkpoint(result, fingerprint)
  other <- file.path(fx$root, "other-reference.rds")
  saveRDS(data.frame(id = 1:3, value = 500:502), other)
  config <- fx$state$config
  config$comparison_rules$references$work.out1 <- other
  config$outputs$references$work.out1 <- other
  next_state <- new_migration_state(fx$state$project, fx$state$paths$root, config = config)
  next_state$output_contracts <- fx$state$output_contracts
  next_state$output_contracts$reference_path <- other
  next_state$fixer_llm <- fx$state$fixer_llm
  next_state$reviewer_llm <- fx$state$reviewer_llm
  expect_identical(migration_resume_fingerprint(next_state), fingerprint)
  next_state <- restore_migration_checkpoint(next_state, fingerprint)
  resumed <- run_bundle_pipeline(next_state)
  expect_length(fx$state$reviewer_llm$requests(), 1L)
  expect_length(fx$state$fixer_llm$requests(), 0L)
  expect_identical(resumed$status, "blocked")
  expect_identical(resumed$assessment$targets$work.out1$reference_path, other)
})

test_that("unavailable focused review consumes its single opportunity", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  reference <- file.path(fx$root, "reference.rds")
  saveRDS(data.frame(id = 1:2, value = 91:92), reference)
  fx$state$comparison_rules <- list(references = list(work.out1 = reference))
  fx$state$reviewer_llm <- recording_reviewer(function(req) valid_program_review_response(verdict = "review_unavailable"))
  attempt <- run_bundle_attempt(fx$state, sequence = 1L)
  assessment <- assess_final_outputs(fx$state$output_contracts, attempt, fx$state$graph,
    fx$state$histories, fx$state$comparison_rules)
  once <- review_bundle_mismatches(fx$state, attempt, assessment, 0L)
  twice <- review_bundle_mismatches(once, attempt, assessment, 0L)
  expect_length(fx$state$reviewer_llm$requests(), 1L)
  expect_identical(component_review_verdict(twice$histories$p01), "review_unavailable")
})

test_that("pending smoke evidence does not reject a valid pre-execution candidate", {
  previous <- list(revision = list(checks = list(pass = TRUE), smoke = list(passed = TRUE,
    population_checks = list(p = list(list(unit_id = 1L, outputs = "work.out", status = "passed"))))),
    verdict = "reviewed_no_material_finding")
  candidate <- list(checks = list(pass = TRUE))
  expect_identical(program_repair_regressions(previous, candidate,
    list(verdict = "reviewed_no_material_finding"), execution = FALSE), character())
  expect_setequal(program_repair_regressions(previous, candidate,
    list(verdict = "reviewed_no_material_finding")),
    c("execution regressed", "source population coverage regressed"))
})

test_that("swapping passed outputs is a regression even when the total is equal", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  before_state <- stage_workflow_revision(fx$state, "p01",
    paste("not_called <- function() {", fx$fixed$p01, "}"), "reviewed_no_material_finding")
  first <- run_bundle_attempt(before_state, sequence = 1L)
  before <- assess_final_outputs(before_state$output_contracts, first, before_state$graph, before_state$histories)
  selected <- select_attempt(before_state$paths, first, before)
  after_state <- stage_workflow_revision(fx$state, "p02",
    paste("not_called <- function() {", fx$fixed$p02, "}"), "reviewed_no_material_finding")
  second <- run_bundle_attempt(after_state, sequence = 2L)
  after <- assess_final_outputs(after_state$output_contracts, second, after_state$graph, after_state$histories)
  expect_length(before$passing_targets, 1L)
  expect_length(after$passing_targets, 1L)
  expect_error(select_attempt(after_state$paths, second, after, selected), "work.out2:candidate_exists", fixed = TRUE)
})

test_that("a missing source input remains visible without changing the translation", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  unlink(file.path(fx$root, "inputs", "input.rds"))
  result <- run_bundle_pipeline(fx$state)
  expect_length(fx$state$fixer_llm$requests(), 0L)
  expect_identical(result$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_match(result$attempt$condition$message, "Dataset not found: raw.input", fixed = TRUE)
  expect_true(length(result$diagnostics$bundle_repair$attempts[[1L]]$non_translation_failures) > 0L)
})

test_that("multiple mismatching outputs from one script receive one focused review", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  sas <- "data work.out1 work.out2; set raw.input; value=value+1; run;"
  writeLines(sas, file.path(fx$root, "p01.sas"))
  project <- sas_project(fx$root, config = fx$state$config)
  state <- new_migration_state(project, file.path(fx$root, "grouped"), config = fx$state$config)
  state$selected_revisions <- fx$state$selected_revisions
  state$selected_revisions$p01$r_code <- paste(fx$fixed$p01, "lib_write(x, 'work', 'out2')", sep = "\n")
  state$selected_revisions$p01$contract$sas_text <- sas
  state$fixer_llm <- fx$state$fixer_llm
  state$reviewer_llm <- fx$state$reviewer_llm
  state$output_contracts <- infer_output_contracts(project, overrides = list(datasets = c("work.out1", "work.out2")))
  ref <- file.path(fx$root, "ref.rds")
  saveRDS(data.frame(id = 1:2, value = 90:91), ref)
  state$comparison_rules <- list(references = list(work.out1 = ref, work.out2 = ref))
  result <- run_bundle_pipeline(state)
  expect_length(state$reviewer_llm$requests(), 1L)
  expect_length(state$fixer_llm$requests(), 0L)
  text <- paste(vapply(state$reviewer_llm$requests()[[1L]]$messages, function(m) as.character(m$content), ""), collapse = "\n")
  expect_match(text, "work.out1, work.out2", fixed = TRUE)
  expect_no_match(text, "rows_base|ROW_COUNT_DELTA|n_mismatch")
})
