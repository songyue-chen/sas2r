test_that("a repair batch distinguishes a repaired consumer from its executed revision", {
  fx <- repair_workflow_fixture(n = 2L, failures = 2L, value_errors = 1L, chain = TRUE)
  for (cid in fx$ids) {
    fx$state$histories[[cid]] <- record_completed_review(fx$state$histories[[cid]],
      verdict = "repair_required", basis_id = paste0("source-", cid), findings = list(list(
        category = "translation_defect", severity = "material",
        sas_evidence = "value = value + 1", r_evidence = fx$state$selected_revisions[[cid]]$r_code,
        affected_outputs = paste0("work.out", match(cid, fx$ids)), repair_disposition = "unverified")))
  }
  order <- character()
  fx$state$fixer_llm <- recording_fixer(function(ctx) {
    order <<- c(order, ctx$component_id)
    valid_program_fix_response(fx$fixed[[ctx$component_id]])
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_identical(order, c("p02", "p01"))
  request <- fx$state$fixer_llm$requests()[[2L]]
  text <- request_task_text(request)
  expect_match(text, '"failed_component_id": "p02"', fixed = TRUE)
  expect_match(text, "component p02 snapshot revision r1", fixed = TRUE)
  expect_match(text, result$repairs[[1L]]$revision_id, fixed = TRUE)
  expect_match(text, "differs from the attempt (not the executed revision)", fixed = TRUE)
  page <- request$tools$read_dependency_context$call(list(component_id = "p02", language = "r"))
  expect_identical(page$code, fx$fixed$p02)
  expect_match(page$revision_note, "snapshot revision r1", fixed = TRUE)
  expect_match(page$revision_note, "differs from the attempt", fixed = TRUE)
})

test_that("fixer requests and tool turns expose only eligible execution and history fields", {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L, chain = TRUE)
  attempt <- run_bundle_attempt(fx$state)
  attempt$condition$message <- "PRIVATE_CONDITION_VALUE"
  attempt$population_checks <- list(list(expected_rows = "PRIVATE_EXPECTED",
    actual_rows = "PRIVATE_ACTUAL", mismatched_by_groups = "PRIVATE_GROUPS", detail = "PRIVATE_DETAIL"))
  attempt$candidate_input_observations <- list(p01 = list(input = list(nrow = "PRIVATE_NROW")))
  attempt$input_metadata <- list(value = "PRIVATE_METADATA")
  attempt$deferred_reasons <- "PRIVATE_DEFERRED_REASON"
  attempt$execution_context$environment <- "PRIVATE_ENVIRONMENT"
  attempt$revision_manifest$p01$extra <- "PRIVATE_MANIFEST_EXTRA"
  attempt$failed_targets <- list(differences = "PRIVATE_DIFFERENCE",
    checks = list(reference_comparison = "PRIVATE_REFERENCE"), reference_path = "PRIVATE_REFERENCE_PATH")
  fx$state$selected_revisions$p01$contract$diagnosis <- "PRIVATE_CONTRACT"
  prior <- list(run_id = fx$state$paths$run_id, component_id = "p01", attempt_id = "earlier",
    revision_id = "prior-candidate", outcome = "applied", reference_triggered_review = FALSE,
    diagnosis = "ELIGIBLE_PRIOR_CLAIM", remaining_uncertainty = "ELIGIBLE_UNCERTAINTY",
    extra = "PRIVATE_OUTCOME_EXTRA")
  excluded <- lapply(c("reference", "unknown", "other_component", "other_run"), function(kind) {
    x <- prior
    x$diagnosis <- paste0("PRIVATE_", kind)
    if (kind == "reference") x$reference_triggered_review <- TRUE
    if (kind == "unknown") x$reference_triggered_review <- NULL
    if (kind == "other_component") x$component_id <- "p02"
    if (kind == "other_run") x$run_id <- "previous-run"
    x
  })
  calls <- 0L
  fixer <- recording_fixer(function(ctx) {
    calls <<- calls + 1L
    if (calls < 3L) return(list(status = "completed", action = "tool_call",
      tool_name = if (calls == 1L) "query_project_graph" else "read_dependency_context",
      tool_arguments = if (calls == 1L) list(dataset = "work.out1") else list(component_id = "p02", language = "r"),
      tool_call_id = paste0("context-", calls)))
    valid_program_fix_response(fx$fixed$p01)
  })
  fix_program_revision(fx$state$selected_revisions$p01, bundle = attempt, mode = "bundle",
    llm = fixer, paths = fx$state$paths, project = fx$state$project, config = fx$state$config,
    selected_revisions = fx$state$selected_revisions, helper_code = runtime_helper_code(fx$state$runtime),
    prior_outcomes = c(list(prior), excluded), evidence_ids = attempt$attempt_id)
  requests <- fixer$requests()
  expect_length(requests, 3L)
  text <- paste(vapply(requests, function(req)
    as.character(jsonlite::toJSON(req$messages, auto_unbox = TRUE)), ""), collapse = "\n")
  expect_false(grepl("PRIVATE_", text, fixed = TRUE))
  expect_match(text, "ELIGIBLE_PRIOR_CLAIM", fixed = TRUE)
  expect_match(text, "unverified claims", fixed = TRUE)
  expect_match(text, ".sas2r_registry", fixed = TRUE)
  expect_match(text, attempt$bundle_dir, fixed = TRUE)
  expect_true(any(vapply(requests[[3L]]$messages, function(m) identical(m$role, "tool"), logical(1))))
  lineage <- requests[[1L]]$tools$query_project_graph$call(list(dataset = "work.out1"))
  expect_true("creates" %in% lineage$role)
})

test_that("startup context is bounded and compares helper text on the same basis", {
  dir <- withr::local_tempdir()
  autoexec <- file.path(dir, "autoexec.R")
  writeLines(strrep("#", 12001L), autoexec)
  writeLines("helper <- function() 1", file.path(dir, "sas2r-helpers.R"))
  bundle <- list(attempt_id = "bundle_attempt_001", bundle_dir = dir)
  context <- repair_startup_context(bundle, "helper <- function() 2")
  expect_match(context, "Startup characters 1-12000 of 12001; truncated", fixed = TRUE)
  expect_false(grepl(strrep("#", 12001L), context, fixed = TRUE))
  expect_match(context, "Shown current helpers differ from", fixed = TRUE)
  expect_match(repair_startup_context(bundle, "helper <- function() 1"),
    "Shown current helpers match", fixed = TRUE)
  unlink(autoexec)
  expect_match(repair_startup_context(bundle, ""), "startup snapshot unavailable", fixed = TRUE)
  expect_match(repair_revision_note(bundle, "p01", "r1"), "binding is unestablished", fixed = TRUE)
})

test_that("independent branch diagnostics retain their own smoke snapshot identity", {
  fx <- repair_workflow_fixture(n = 2L, failures = c(1L, 2L))
  attempt <- run_bundle_attempt(fx$state)
  diagnostics <- collect_bundle_diagnostics(fx$state, attempt)
  diagnostics$executions$p02$condition$message <- "PRIVATE_SMOKE_CONDITION"
  diagnostics$executions$p02$population_checks <- list(list(detail = "PRIVATE_SMOKE_POPULATION"))
  diagnostics$executions$p02$output_files <- "PRIVATE_SMOKE_OUTPUT_FILE"
  assessment <- assess_final_outputs(fx$state$output_contracts, attempt,
    fx$state$graph, fx$state$histories, project = fx$state$project)
  queue <- bundle_repair_queue(fx$state, attempt, assessment, diagnostics)
  repair_bundle_component(fx$state, queue$p02, attempt, 0L)
  text <- request_task_text(fx$state$fixer_llm$requests()[[1L]])
  smoke <- diagnostics$executions$p02
  expect_match(text, paste("Smoke execution", smoke$execution_id), fixed = TRUE)
  expect_match(text, smoke$code_hashes$p02, fixed = TRUE)
  expect_match(text, file.path(smoke$attempt_dir, "autoexec.R"), fixed = TRUE)
  expect_false(grepl(paste("Attempt", attempt$attempt_id, "component p02 snapshot revision"), text, fixed = TRUE))
  messages <- paste(vapply(fx$state$fixer_llm$requests(), function(req)
    as.character(jsonlite::toJSON(req$messages, auto_unbox = TRUE)), ""), collapse = "\n")
  expect_false(grepl("PRIVATE_SMOKE_", messages, fixed = TRUE))
})

test_that("an unchanged repair preserves uncertainty and evidence without requeueing or promotion", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  fx$state$fixer_llm <- recording_fixer(function(ctx) valid_program_fix_response(
    fx$state$selected_revisions$p01$r_code, diagnosis = "Inspect the runtime binding",
    remaining_uncertainty = "Input availability remains unresolved", evidence_ids = ctx$evidence_ids))
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 3L)
  expect_length(fx$state$fixer_llm$requests(), 1L)
  expect_length(result$repairs, 0L)
  expect_identical(result$diagnostics$bundle_repair$deferred$p01, "identical_patch")
  expect_identical(result$status, "blocked")
  outcome <- result$diagnostics$bundle_repair$outcomes[[1L]]
  expect_identical(outcome$diagnosis, "Inspect the runtime binding")
  expect_identical(outcome$remaining_uncertainty, "Input availability remains unresolved")
  expect_identical(outcome$run_id, fx$state$paths$run_id)
  expect_true(result$attempt$attempt_id %in% outcome$evidence_ids)
  write_migration_report(result)
  report <- jsonlite::read_json(result$paths$report_json, simplifyVector = FALSE)
  expect_identical(report$diagnostics$bundle_repair$outcomes[[1L]]$diagnosis, outcome$diagnosis)
  for (path in c(result$paths$report_md, result$paths$start_here)) {
    text <- paste(readLines(path), collapse = "\n")
    expect_match(text, outcome$diagnosis, fixed = TRUE)
    expect_match(text, outcome$remaining_uncertainty, fixed = TRUE)
  }

  fingerprint <- migration_resume_fingerprint(fx$state)
  write_migration_checkpoint(result, fingerprint)
  next_state <- new_migration_state(fx$state$project, fx$state$paths$root, config = fx$state$config)
  next_state$output_contracts <- fx$state$output_contracts
  next_state$fixer_llm <- fx$state$fixer_llm
  next_state$reviewer_llm <- fx$state$reviewer_llm
  expect_identical(migration_resume_fingerprint(next_state), fingerprint)
  next_state <- restore_migration_checkpoint(next_state, fingerprint)
  expect_identical(repair_history_context(next_state$diagnostics$bundle_repair$outcomes,
    next_state$paths$run_id, "p01"), "")
  # Resume starts a new invocation: one remaining repair is allowed by the
  # cumulative limit, while the prior run's claim remains report-only history.
  resumed <- run_bundle_pipeline(next_state)
  expect_false(identical(resumed$paths$run_id, outcome$run_id))
  expect_length(fx$state$fixer_llm$requests(), 2L)
  expect_identical(resumed$diagnostics$bundle_repair$deferred$p01, "identical_patch")
  expect_identical(resumed$status, "blocked")
  expect_equal(resumed$diagnostics$bundle_repair$repair_counts$p01, 2L)
  write_migration_report(resumed)
  html <- paste(readLines(resumed$paths$start_here), collapse = "\n")
  expect_match(html, paste("Outcome run:", outcome$run_id), fixed = TRUE)
  expect_match(html, paste("Outcome run:", resumed$paths$run_id), fixed = TRUE)
})

test_that("a second repair sees same-run prior claims while acceptance remains independent", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  calls <- 0L
  fx$state$fixer_llm <- recording_fixer(function(ctx) {
    calls <<- calls + 1L
    valid_program_fix_response(if (calls == 1L) "stop('second fault')" else fx$fixed$p01,
      diagnosis = "EARLIER_REPAIR_CLAIM", remaining_uncertainty = "EARLIER_REPAIR_UNCERTAINTY")
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_length(fx$state$fixer_llm$requests(), 2L)
  second <- request_task_text(fx$state$fixer_llm$requests()[[2L]])
  expect_match(second, "EARLIER_REPAIR_CLAIM", fixed = TRUE)
  expect_match(second, "EARLIER_REPAIR_UNCERTAINTY", fixed = TRUE)
  for (req in fx$state$reviewer_llm$requests())
    expect_false(grepl("EARLIER_REPAIR_", request_task_text(req), fixed = TRUE))
  expect_length(result$repairs, 2L)
  prior <- result$repairs[[1L]]
  expect_identical(repair_history_context(list(prior), "resumed-run", "p01"), "")
  prior$reference_triggered_review <- TRUE
  expect_identical(repair_history_context(list(prior), fx$state$paths$run_id, "p01"), "")
})
