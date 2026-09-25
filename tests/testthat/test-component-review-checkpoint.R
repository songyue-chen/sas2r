test_that("repeated helper edits across many components coalesce earlier reviews", {
  # Keep all three invalidation waves on CRAN; CI also exercises the larger graph.
  n <- if (identical(Sys.getenv("NOT_CRAN"), "true")) 20L else 8L
  repairs <- if (n == 20L) c(6L, 12L, 18L) else c(2L, 4L, 6L)
  fx <- repair_workflow_fixture(n = n, failures = repairs)
  revisions <- fx$state$selected_revisions
  fx$state$selected_revisions <- fx$state$histories <- list()
  # Exercise the agent translation path, including its real binding creation,
  # rather than letting the deterministic translator handle these small sources.
  fx$state$baseline$manifest$tier <- "stub"
  fx$state$baseline$manifest$reason <- "agent_translation_required"
  fx$state$translator_llm <- recording_reviewer(function(req)
    valid_program_translation_response(code = revisions[[req$component_id]]$r_code))
  stage <- "component"
  calls <- list()
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    calls[[length(calls) + 1L]] <<- list(component = req$component_id, stage = stage)
    prompt <- paste(vapply(req$messages, `[[`, "", "content"), collapse = "\n")
    if (grepl("translation fault", prompt, fixed = TRUE)) {
      material_review_response(sas_evidence = "source writes a dataset", r_evidence = "R stops before writing")
    } else valid_program_review_response()
  })
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
    code = fx$fixed[[req$component_id]], bundle_helper_patch = list(path = "sas2r-helpers.R",
      content = sprintf("checkpoint_marker <- function() '%s'", req$component_id), reason = "synthetic helper edit")))
  result <- withCallingHandlers(run_program_pipeline(fx$state, execute = FALSE),
    sas2r_progress = function(e) {
      if (identical(e$event, "component_review_checkpoint_started")) stage <<- "checkpoint"
    })
  immediate <- table(vapply(Filter(function(x) x$stage == "component", calls), `[[`, "", "component"))
  expected <- stats::setNames(rep(1L, n), fx$ids)
  expected[repairs] <- 2L
  expect_equal(as.integer(immediate[fx$ids]), unname(expected))
  final <- table(vapply(Filter(function(x) x$stage == "checkpoint", calls), `[[`, "", "component"))
  expect_true(length(final) > 0L)
  expect_true(all(final == 1L))
  expect_length(fx$state$fixer_llm$requests(), 3L)
  expect_null(result$pending_reviews)
  before <- length(calls)
  finalize_component_reviews(result)
  expect_length(calls, before)
  result$resumed_components <- names(result$selected_revisions)
  resumed <- run_program_pipeline(result, execute = FALSE)
  expect_length(calls, before)
  expect_identical(resumed$repair_counts, result$repair_counts)
})

test_that("helper edits that crash an earlier passing consumer restore the whole candidate", {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L)
  fx$state <- stage_workflow_revision(fx$state, "p02",
    sub("x$value + 1", "shift(x$value)", fx$fixed$p02, fixed = TRUE), "reviewed_no_material_finding")
  writeLines(assemble_helper_overlay(runtime_helper_code(fx$state$runtime),
    "shift <- function(x) x + 1"), fx$state$runtime$helpers)
  retained <- process_program_component(fx$state, "p02")
  retained$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(
    code = fx$fixed$p01, bundle_helper_patch = list(path = "sas2r-helpers.R",
      content = "shift <- function(x) stop('candidate helper crash')", reason = "broken helper candidate")))
  result <- process_program_component(retained, "p01")
  expect_identical(result$runtime, retained$runtime)
  expect_identical(result$selected_revisions$p02, retained$selected_revisions$p02)
  expect_identical(result$selected_revisions$p01$r_code, retained$selected_revisions$p01$r_code)
  expect_match(paste(result$diagnostics$rejected_repairs[[1]]$errors, collapse = " "), "p02 execution regressed")
  expect_length(retained$reviewer_llm$requests(), 3L) # p02 once, p01 before/after repair
})

test_that("context-only revisits preserve repair counts and skip agents even after repairs", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  state <- process_program_component(fx$state, "p02")
  state$repair_counts$p02 <- 1L
  state$selected_revisions$p01$r_code <- sub("+ 1", "+ 2", fx$fixed$p01, fixed = TRUE)
  state <- revisit_component_runtime(state, "p02")
  expect_length(fx$state$reviewer_llm$requests(), 1L)
  expect_length(fx$state$fixer_llm$requests(), 0L)
  expect_identical(state$repair_counts$p02, 1L)
  expect_null(current_component_evidence(state$histories$p02)$level)
  expect_true(state$selected_revisions$p02$smoke$passed)
  execution <- state$selected_revisions$p02$smoke$execution_id
  again <- revisit_component_runtime(state, "p02")
  expect_identical(again$selected_revisions$p02$smoke$execution_id, execution)
  saveRDS(data.frame(id = 1:3, value = 20:22), file.path(fx$root, "inputs", "input.rds"))
  again <- revisit_component_runtime(again, "p02")
  expect_false(identical(again$selected_revisions$p02$smoke$execution_id, execution))
  state <- finalize_component_reviews(again)
  expect_identical(component_review_verdict(state$histories$p02), "reviewed_no_material_finding")
  expect_identical(current_component_evidence(state$histories$p02)$level, "runtime_verified")
})

test_that("local helper checks preserve source coverage without blaming changed inputs", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- stage_workflow_revision(fx$state, "p01",
    sub("lib_write(x, 'work', 'out1')", "persist(x)", fx$fixed$p01, fixed = TRUE),
    "reviewed_no_material_finding")
  writeLines(assemble_helper_overlay(runtime_helper_code(state$runtime),
    "persist <- function(x) lib_write(x, 'work', 'out1')"), state$runtime$helpers)
  retained <- process_program_component(state, "p01")
  expect_length(passed_population_checks(retained$selected_revisions$p01$smoke), 1L)
  candidate <- retained
  candidate$runtime$helpers <- file.path(fx$root, "candidate-helpers.R")
  writeLines(assemble_helper_overlay(runtime_helper_code(retained$runtime),
    "persist <- function(x) invisible(x)"), candidate$runtime$helpers)
  checked <- check_helper_consumers(candidate, retained, "p01")
  expect_true(checked$state$selected_revisions$p01$smoke$passed)
  expect_match(paste(checked$reasons, collapse = " "), "source population coverage regressed")
  expect_length(state$reviewer_llm$requests(), 1L)
  unlink(file.path(fx$root, "inputs", "input.rds"))
  checked <- check_helper_consumers(candidate, retained, "p01")
  expect_false(checked$state$selected_revisions$p01$smoke$passed)
  expect_length(checked$reasons, 0L)
})

test_that("resumed bundle repairs honor previously spent per-component and overall limits", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  fx$state$diagnostics$bundle_repair$repair_counts <- list(p01 = 1L)
  for (limits in list(list(max_bundle_repair_rounds = 1L),
                     list(max_bundle_repairs_per_component = 1L))) {
    result <- do.call(run_bundle_pipeline, c(list(state = fx$state), limits))
    expect_false(result$attempt$passed)
    expect_identical(result$diagnostics$bundle_repair$repair_counts, list(p01 = 1L))
    expect_length(fx$state$fixer_llm$requests(), 0L)
  }
})

test_that("checkpoint uses the normal latest full-review identity and never invokes a fixer", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- process_program_component(fx$state, "p01", execute = FALSE)
  expect_length(fx$state$reviewer_llm$requests(), 1L)
  state <- finalize_component_reviews(state)
  expect_length(fx$state$reviewer_llm$requests(), 1L)
  state$histories$p01 <- record_review_unavailable(state$histories$p01, "later unavailable review")
  state <- finalize_component_reviews(state)
  expect_length(fx$state$reviewer_llm$requests(), 2L)
  state$reviewer_llm <- recording_reviewer(function(req) material_review_response(
    sas_evidence = "source adds one", r_evidence = "wrong calculation"))
  # A different reviewer setting changes the request identity.
  state$reviewer_llm$model <- "checkpoint-reviewer"
  state <- finalize_component_reviews(state)
  expect_identical(component_review_verdict(state$histories$p01), "repair_required")
  state <- finalize_component_reviews(state)
  expect_length(state$reviewer_llm$requests(), 1L)
  expect_length(state$fixer_llm$requests(), 0L)
})

test_that("checkpoint budget exhaustion leaves unavailable review evidence", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- check_component_revision(fx$state, "p01")
  state$usage_budget <- new_usage_budget(max_calls = 0, mode = "strict")
  result <- finalize_component_reviews(state)
  expect_length(state$reviewer_llm$requests(), 0L)
  expect_identical(component_review_verdict(result$histories$p01), "review_unavailable")
  expect_true("budget_exhausted" %in% current_component_evidence(result$histories$p01)$blockers)
})

test_that("runtime revisits keep macros deferred until a supported caller is available", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- fx$state
  # Ordinary resolved macro-call graph shape; the smoke planner finds the call
  # from the generated caller, without inventing arguments for the definition.
  nodes <- state$graph$nodes
  from <- nodes$node_id[nodes$component_id == "p01"][[1]]
  to <- nodes$node_id[nodes$component_id == "p02"][[1]]
  state$graph$edges <- tibble::tibble(edge_id = "call", from = from, to = to,
    type = "calls_macro", resolution = "resolved", source_file = "p02.sas", line = 1L, detail = "calc_total")
  state <- stage_workflow_revision(state, "p01", "calc_total <- function(a, b) a + b", "reviewed_no_material_finding")
  caller <- state$selected_revisions$p02
  caller$r_code <- "calc_total(1, 2)"
  state$selected_revisions$p02 <- NULL
  state <- revisit_component_runtime(state, "p01")
  expect_true(state$selected_revisions$p01$smoke$deferred)
  expect_identical(state$selected_revisions$p01$smoke$reason, "caller_not_generated")
  state <- revisit_component_runtime(state, "p01")
  expect_identical(current_component_evidence(state$histories$p01)$runtime_deferred, "caller_not_generated")
  state$selected_revisions$p02 <- caller
  state <- revisit_component_runtime(state, "p01")
  expect_true(state$selected_revisions$p01$smoke$passed)
  expect_identical(state$selected_revisions$p01$smoke$executed_call_ids, "call_site_1")
  expect_length(state$reviewer_llm$requests(), 0L)
})

test_that("checkpoint requests exclude local runtime and reference answers", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- check_component_revision(fx$state, "p01")
  state$selected_revisions$p01$smoke <- list(passed = FALSE,
    condition = list(message = "PRIVATE_RUNTIME_RECORD_123"))
  state$config$outputs$references <- list(out1 = "PRIVATE_REFERENCE_PATH")
  state$config$comparison_rules <- list(answer = "PRIVATE_REFERENCE_VALUE")
  state <- finalize_component_reviews(state)
  request <- state$reviewer_llm$requests()[[1]]
  prompt <- paste(vapply(request$messages, `[[`, "", "content"), collapse = "\n")
  expect_false(grepl("PRIVATE_RUNTIME|PRIVATE_REFERENCE", prompt))
  expect_false(any(c("read_comparison_report", "read_dataset_preview") %in% names(request$tools)))
  expect_match(prompt, fx$fixed$p01, fixed = TRUE)
})

test_that("interrupted checkpoint resumes completed reviews without resetting counters", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  state <- fx$state
  state$resume_fingerprint <- migration_resume_fingerprint(state)
  state$repair_counts <- list(p01 = 1L)
  state$diagnostics$bundle_repair$repair_counts <- list(p01 = 2L)
  for (cid in fx$ids) state <- check_component_revision(state, cid)
  calls <- character()
  state$reviewer_llm <- recording_reviewer(function(req) {
    calls <<- c(calls, req$component_id)
    valid_program_review_response()
  })
  interrupted <- tryCatch(withCallingHandlers(finalize_component_reviews(state),
    sas2r_progress = function(e) {
      if (identical(e$event, "agent_started") && identical(e$component_id, "p02")) {
        stop(structure(list(message = "checkpoint interruption", call = NULL),
          class = c("interrupt", "condition")))
      }
    }), interrupt = function(e) e)
  expect_s3_class(interrupted, "interrupt")
  restored <- restore_migration_checkpoint(fx$state, state$resume_fingerprint)
  restored$reviewer_llm <- state$reviewer_llm
  expect_identical(restored$repair_counts, list(p01 = 1L))
  expect_identical(restored$diagnostics$bundle_repair$repair_counts, list(p01 = 2L))
  result <- finalize_component_reviews(restored)
  expect_identical(calls, fx$ids)
  expect_true(all(vapply(result$histories, component_review_verdict, "") == "reviewed_no_material_finding"))
})
