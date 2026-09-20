test_that("helper overlays preserve siblings, sequential edits and nested scope without evaluation", {
  base <- 'outer <- function(x) { offset <- 3; sibling <- function(z) z * 2; inner <- function(y) { deep <- function(z) z + offset; sibling(deep(y)) }; inner(x) }\nuntouched <- function() 11'
  patch <- sub('offset <- 3', 'offset <- 5', strsplit(base, '\n')[[1]][1], fixed = TRUE)
  merged <- assemble_helper_overlay(base, patch)
  second <- assemble_helper_overlay(merged, '`quoted helper` = function(x) x + 7')
  e <- new.env(parent = baseenv())
  eval(parse(text = second), e)
  expect_equal(e$outer(2), 14)
  expect_equal(e$untouched(), 11)
  expect_equal(e[['quoted helper']](1), 8)
  expect_equal(assemble_helper_overlay(second, second), second)
  stock <- runtime_helper_code()
  expect_identical(assemble_helper_overlay(stock, paste(stock, '# comment-only overlay')), stock)
  marker <- withr::local_tempfile()
  expect_error(assemble_helper_overlay(base, sprintf('writeLines("ran", %s)', deparse(marker))), "only named")
  expect_false(file.exists(marker))
  for (bad in c('x <- 1', 'x <- function() 1; x <- function() 2', 'x <<- function() 1', 'a <- b <- function() 1')) {
    expect_error(assemble_helper_overlay(base, bad))
  }
})

test_that("the actual fixer receives complete retained parent bodies and materializes one snapshot", {
  fx <- review_fix_fixture()
  parent <- 'outer <- function(x) { offset <- 3; sibling <- function(z) z * 2; inner <- function(y) { deep <- function(z) z + offset; sibling(deep(y)) }; inner(x) }'
  retained <- assemble_helper_overlay(runtime_helper_code(), parent)
  edited <- sub('offset <- 3', 'offset <- 5', parent, fixed = TRUE)
  fixer <- recording_fixer(function(req) valid_program_fix_response(code = fx$revision$r_code,
    bundle_helper_patch = list(path = 'sas2r-helpers.R', content = edited, reason = 'source correction')))
  rev <- fix_program_revision(fx$revision, smoke = fx$failed_smoke, llm = fixer,
    paths = fx$paths, helper_code = retained)
  expect_true(rev$checks$pass)
  expect_true(rev$helper_changed)
  prompt <- paste(vapply(fixer$requests()[[1]]$messages, `[[`, '', 'content'), collapse = '\n')
  expect_match(prompt, retained, fixed = TRUE)
  expect_identical(paste(readLines(rev$helper_path), collapse = '\n'), rev$helper_code)
  expect_identical(rev$contract$binding$helper_hash, migration_hash(rev$helper_code))
  e <- new.env(parent = baseenv()); sys.source(rev$helper_path, e)
  expect_equal(e$outer(2), 14)
  expect_true(is.function(e$lib_read))
  expect_true(is.function(e$lib_members))
})

test_that("immediate helper-only repair executes and exports the reviewed snapshot", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  code <- sub('x$value + 1', 'shift(x$value)', fx$fixed$p01, fixed = TRUE)
  fx$state <- stage_workflow_revision(fx$state, 'p01', code, 'repair_required')
  writeLines(assemble_helper_overlay(runtime_helper_code(fx$state$runtime),
    'shift <- function(x) x + 99'), fx$state$runtime$helpers)
  old_path <- fx$state$runtime$helpers
  old <- readLines(old_path)
  reviews <- 0L
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    reviews <<- reviews + 1L
    if (reviews == 1L) material_review_response(sas_evidence = 'value=value+1', r_evidence = 'shift adds 99')
    else valid_program_review_response()
  })
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(code = code,
    bundle_helper_patch = list(path = 'sas2r-helpers.R', content = 'shift <- function(x) x + 1', reason = 'source says +1')))
  result <- process_program_component(fx$state, 'p01', max_program_repair_rounds = 1L)
  rev <- result$selected_revisions$p01
  expect_true(rev$smoke$passed)
  expect_identical(readLines(old_path), old)
  helper <- runtime_helper_code(result$runtime)
  expect_identical(rev$helper_code, helper)
  requests <- fx$state$reviewer_llm$requests()
  prompt <- paste(vapply(tail(requests, 1)[[1]]$messages, `[[`, '', 'content'), collapse = '\n')
  expect_match(prompt, helper, fixed = TRUE)
  plan <- build_program_smoke_plan(result$graph, 'p01', result$selected_revisions)
  staged <- prepare_program_smoke(result, plan, withr::local_tempdir())
  expect_identical(runtime_helper_code(staged$runtime), helper)
  attempt <- run_bundle_attempt(result, sequence = 1L)
  expect_true(attempt$passed)
  expect_identical(paste(readLines(file.path(attempt$bundle_dir, 'sas2r-helpers.R')), collapse = '\n'), helper)
  expect_equal(readRDS(file.path(attempt$work_dir, 'out1.rds'))$value, 11:13)
  materialize_user_bundle(attempt$bundle_dir, result$paths$bundle, result$project)
  result$bundle_dir <- result$paths$bundle
  expect_identical(paste(readLines(file.path(result$bundle_dir, 'runtime', 'sas2r-helpers.R')), collapse = '\n'), helper)
  fingerprint <- migration_resume_fingerprint(fx$state)
  write_migration_checkpoint(result, fingerprint)
  resumed <- restore_migration_checkpoint(fx$state, fingerprint)
  expect_identical(runtime_helper_code(resumed$runtime), helper)
  expect_identical(resumed$selected_revisions$p01$contract$binding$helper_hash, migration_hash(helper))
  expect_identical(resumed$resumed_components, 'p01')
})

test_that("member listing follows resolution without reading rows or granting deletion", {
  input <- withr::local_tempdir(); output <- withr::local_tempdir()
  .sas2r_registry <- list(raw = list(read_path = input, write_path = output),
    empty = list(path = file.path(output, 'not_created')))
  expect_identical(lib_members('empty'), character())
  saveRDS(data.frame(id = 1), file.path(input, 'alpha.rds'))
  saveRDS(data.frame(id = 2), file.path(output, 'alpha.rds'))
  file.create(file.path(output, 'alpha.xpt'), file.path(input, 'broken.rds'),
    file.path(input, 'Upper.rds'), file.path(output, 'bad name.rds'))
  dir.create(file.path(output, 'directory.rds'))
  expect_equal(lib_members('raw'), c('Upper', 'alpha', 'broken'))
  expect_true(all(vapply(lib_members('raw'), function(x) lib_exists('raw', x), logical(1))))
  expect_equal(lib_read('raw', 'alpha')$id, 2)
  expect_error(lib_delete('raw', lib_members('raw')), 'separate input')
  expect_true(file.exists(file.path(input, 'alpha.rds')))
  expect_error(lib_members('unknown'), 'Unknown libref')
  expect_error(lib_members('../raw'), 'registered library')
  .sas2r_registry$raw$read_path <- file.path(input, 'missing')
  expect_error(lib_members('raw'), 'unavailable')
})

test_that("merge guidance preserves repeated events and genuine overlap refusals", {
  events <- data.frame(id = c(1, 1, 2), event = c('a', 'b', 'c'))
  subjects <- data.frame(id = 1:2, base = c(10, 20))
  joined <- sas_merge(events, subjects, by = 'id', keep = 'left')
  expect_equal(joined$event, events$event)
  expect_equal(joined$base, c(10, 10, 20))
  events$base <- rep(NA_real_, nrow(events))
  expect_error(sas_merge(events, subjects, by = 'id', keep = 'left'), 'overlap|shared|non-key')
  events$base <- c(3, 4, 5)
  expect_error(sas_merge(events, subjects, by = 'id', keep = 'left'), 'overlap|shared|non-key')
  empty <- events[FALSE, ]; empty$score <- rep(NA_real_, nrow(empty))
  expect_equal(nrow(empty), 0)
})

test_that("nonlocal scope notices are advisory and distinguish normal locals", {
  for (code in c('x <<- 1', 'assign("x", 1, envir = globalenv())',
    'base::assign("x", 1, envir = .GlobalEnv)', '.GlobalEnv$x <- 1', '.GlobalEnv[["x"]] <- 1')) {
    lint <- lint_r_code(code)
    expect_true(any(lint$kind == 'nonlocal_assignment'))
    expect_false(any(lint$level == 'error'))
    file <- withr::local_tempfile(fileext = '.R'); writeLines(code, file)
    expect_true(check_program_revision(file)$pass)
  }
  expect_false(any(lint_r_code('f <- function() { x <- 1; x }')$kind == 'nonlocal_assignment'))
  outer <- function() { x <- 1; inner <- function() x <<- 2; inner(); x }
  expect_equal(outer(), 2)
  shadow <- function() { x <- 1; inner <- function() { x <- 3; x }; inner(); x }
  expect_equal(shadow(), 1)
})

empty_repair_fixture <- function(envir = parent.frame()) {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE, envir = envir)
  writeLines('data work.out1; set raw.input; if id < 0; value=value+1; run;', file.path(fx$root, 'p01.sas'))
  project <- sas_project(fx$root, config = fx$state$config)
  state <- new_migration_state(project, file.path(fx$root, 'empty-run'), config = fx$state$config)
  state$selected_revisions <- fx$state$selected_revisions
  state$histories <- fx$state$histories
  state$reviewer_llm <- fx$state$reviewer_llm
  state$fixer_llm <- fx$state$fixer_llm
  state$output_contracts <- infer_output_contracts(project, overrides = list(datasets = 'work.out2'))
  state <- stage_workflow_revision(state, 'p01',
    sub('x$value <-', 'x <- x[x$id < 0, ]; x$value <-', fx$fixed$p01, fixed = TRUE), 'reviewed_no_material_finding')
  state <- stage_workflow_revision(state, 'p02', "x <- lib_read('work', 'out1'); invisible(NULL)", 'reviewed_no_material_finding')
  fx$state <- state
  fx
}

test_that("ambiguous empty-input artifact failures require source review before a fixer", {
  for (verdict in c('reviewed_no_material_finding', 'review_unavailable')) {
    fx <- empty_repair_fixture()
    fx$state$reviewer_llm <- recording_reviewer(function(req) valid_program_review_response(verdict = verdict))
    result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
    expect_identical(result$status, 'blocked')
    expect_length(fx$state$fixer_llm$requests(), 0)
    expect_length(fx$state$reviewer_llm$requests(), 1)
    observations <- result$diagnostics$candidate_input_observations[[1]]
    expect_identical(observations$p02$work.out1$status, 'observed_empty_candidate_input')
    expect_false(result$assessment$targets$work.out2$checks$candidate_exists$passed)
    expect_identical(result$selected_revisions$p01$r_code, fx$state$selected_revisions$p01$r_code)
    prompt <- paste(vapply(fx$state$reviewer_llm$requests()[[1]]$messages, `[[`, '', 'content'), collapse = '\n')
    expect_false(grepl('observed_empty_candidate_input|candidate_input_observations|rows_candidate', prompt))
  }
})

test_that("a source-required empty output is repairable in the same cycle without changing its rows", {
  fx <- empty_repair_fixture()
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    text <- paste(vapply(req$messages, `[[`, '', 'content'), collapse = '\n')
    if (grepl('Focused source review', text, fixed = TRUE)) material_review_response(
      sas_evidence = 'data work.out2; set work.out1; value=value+1; run; creates a dataset even with zero rows',
      r_evidence = 'invisible(NULL) omits the required lib_write', affected_outputs = 'work.out2')
    else valid_program_review_response()
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_length(result$repairs, 1)
  expect_identical(result$repairs[[1]]$component_id, 'p02')
  expect_equal(nrow(readRDS(file.path(result$selected_attempt$attempt_dir, 'work', 'out2.rds'))), 0)
  expect_identical(result$selected_revisions$p01$r_code, fx$state$selected_revisions$p01$r_code)
  prompt <- paste(vapply(fx$state$fixer_llm$requests()[[1]]$messages, `[[`, '', 'content'), collapse = '\n')
  expect_match(prompt, 'omits the required lib_write', fixed = TRUE)
  expect_false(grepl('observed_empty_candidate_input|Candidate dataset file not found', prompt))
})

test_that("an independent defect stays eligible while its artifact investigation is unresolved", {
  fx <- empty_repair_fixture()
  fx$state$histories$p02 <- record_completed_review(fx$state$histories$p02, verdict = 'repair_required',
    findings = list(list(severity = 'material', sas_evidence = 'value=value+1', r_evidence = 'value + 9')))
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    text <- paste(vapply(req$messages, `[[`, '', 'content'), collapse = '\n')
    if (grepl('Focused source review', text, fixed = TRUE)) valid_program_review_response(verdict = 'review_unavailable')
    else valid_program_review_response()
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_length(fx$state$fixer_llm$requests(), 1)
  prompt <- paste(vapply(fx$state$fixer_llm$requests()[[1]]$messages, `[[`, '', 'content'), collapse = '\n')
  expect_match(prompt, 'value + 9', fixed = TRUE)
  expect_false(grepl('observed_empty_candidate_input|Candidate dataset file not found', prompt))
})

test_that("an empty optional input does not hide a consumer's own filtering defect", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  sas <- paste('data work.lookup; set work.out1; if 0; run;',
    'data work.stage; set work.out1; value=value+1; run;',
    'data work.out2; set work.stage work.lookup; run;')
  writeLines(sas, file.path(fx$root, 'p02.sas'))
  project <- sas_project(fx$root, config = fx$state$config)
  state <- new_migration_state(project, file.path(fx$root, 'own-intermediate-run'), config = fx$state$config)
  state$selected_revisions <- fx$state$selected_revisions
  state$histories <- fx$state$histories
  state$selected_revisions$p02$contract$sas_text <- sas
  state$selected_revisions$p02$binding$source_hash <- migration_hash(sas)
  state$output_contracts <- infer_output_contracts(project, overrides = list(datasets = 'work.out2'))
  code <- paste("x <- lib_read('work', 'out1')",
    "lib_write(x[FALSE, ], 'work', 'lookup')",
    "x$value <- x$value + 1",
    "lib_write(x, 'work', 'stage')",
    "out <- rbind(lib_read('work', 'stage'), lib_read('work', 'lookup'))",
    "lib_write(out, 'work', 'out2')", sep = '\n')
  bad <- sub('x$value <-', 'x <- x[x$id < 0, ]; x$value <-', code, fixed = TRUE)
  bad <- sub("lib_write(out, 'work', 'out2')", "if (nrow(out)) lib_write(out, 'work', 'out2')", bad, fixed = TRUE)
  state <- stage_workflow_revision(state, 'p02', bad, 'reviewed_no_material_finding')
  state$reviewer_llm <- recording_reviewer(function(req) {
    if (identical(req$purpose, 'source_mismatch_review')) material_review_response(
      sas_evidence = 'work.stage preserves work.out1 rows; work.out2 is always written',
      r_evidence = 'id < 0 removes every row and nrow(out) skips lib_write', affected_outputs = 'work.out2')
    else valid_program_review_response()
  })
  state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(code = code))
  result <- run_bundle_pipeline(state, max_bundle_repair_rounds = 2L)
  expect_length(state$fixer_llm$requests(), 1)
  expect_identical(result$repairs[[1]]$component_id, 'p02')
  first <- result$diagnostics$candidate_input_observations[[1]]$p02
  expect_identical(first$work.lookup$status, 'observed_empty_candidate_input')
  # The independent source-population gate stops this invalid write. Its
  # absent candidate stays unknown rather than being guessed from the error.
  expect_identical(first$work.stage$status, 'unknown')
  attempt <- read_attempt_record(file.path(result$paths$bundle_attempts, 'bundle_attempt_001'))
  expect_true('sas2r_population_mismatch' %in% attempt$condition$class)
  expect_identical(first$work.out1$status, 'observed_nonempty_candidate_input')
  folder <- result$selected_attempt$attempt_dir
  expect_equal(readRDS(file.path(folder, 'work', 'out2.rds'))$value, 12:14)
  expect_equal(nrow(readRDS(file.path(folder, 'work', 'lookup.rds'))), 0)
  expect_identical(result$selected_revisions$p01$r_code, state$selected_revisions$p01$r_code)
})

test_that("candidate observations use inventoried source intermediates and preserve unknowns", {
  fx <- empty_repair_fixture()
  attempt <- run_bundle_attempt(fx$state, sequence = 1L)
  observed <- observe_candidate_inputs(fx$state, attempt)
  expect_equal(observed$p02$work.out1$nrow, 0)
  limited <- observe_candidate_inputs(fx$state, attempt, output_evidence_limits(max_file_bytes = 1))
  expect_identical(limited$p02$work.out1$status, 'unknown')
  attempt$output_hashes <- list()
  expect_identical(observe_candidate_inputs(fx$state, attempt)$p02$work.out1$status, 'unknown')
  expect_null(observed$p01$raw.input) # original input never a candidate fallback
})

test_that("execution blockers rank before unrelated review findings within the same cap", {
  fx <- repair_workflow_fixture(n = 3L, failures = 3L, value_errors = 1L)
  fx$state$histories$p01 <- record_completed_review(fx$state$histories$p01, verdict = 'repair_required',
    findings = list(list(severity = 'material', sas_evidence = 'value=value+1', r_evidence = 'value + 9')))
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_equal(vapply(result$repairs, `[[`, '', 'component_id'), c('p03', 'p01'))
  expect_length(result$repairs, 2)
})

test_that("saved static blockers outrank other source defects on required output paths", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), value_errors = 1L)
  fx$state <- stage_workflow_revision(fx$state, "p02",
    "render <- function() stop('required output not implemented')", "reviewed_no_material_finding")
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    text <- paste(vapply(req$messages, `[[`, "", "content"), collapse = "\n")
    if (req$component_id == "p02" && grepl("required output not implemented", text, fixed = TRUE)) {
      response <- material_review_response(sas_evidence = "data work.out2; set raw.input; value=value+1; run;",
        r_evidence = "render <- function() stop('required output not implemented')")
      response$data$static_runnability <- "known_blocker"
      response$data$findings[[1]]$affected_outputs <- list("work.out2")
      return(response)
    }
    if (req$component_id == "p01" && grepl("value + 9", text, fixed = TRUE)) {
      response <- material_review_response(sas_evidence = "value=value+1", r_evidence = "value + 9")
      response$data$findings[[1]]$affected_outputs <- list("work.out1")
      return(response)
    }
    valid_program_review_response()
  })
  # Use the actual review API and its persisted event, not fabricated history.
  for (cid in fx$ids) {
    fx$state <- check_component_revision(fx$state, cid)
    fx$state <- review_component_revision(fx$state, cid)$state
  }
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_identical(vapply(result$repairs, `[[`, "", "component_id"), c("p02", "p01"))
})

test_that("paired dependency packets retain dataset producers even without translated calls", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  cat(paste0('\n/* ', paste(rep('source context ', 1000), collapse = ''), ' */'),
    file = file.path(fx$root, 'p01.sas'), append = TRUE)
  fx$state$selected_revisions$p01$r_code <- paste('r_dependency_marker <- 1', paste(rep('x <- 1', 1500), collapse = '\n'))
  p <- build_agent_guidance(fx$state$project, 'p02', selected_revisions = fx$state$selected_revisions,
    packet_limit = 4500L)
  expect_match(p$text, 'r_dependency_marker', fixed = TRUE)
  expect_match(p$text, 'data work.out1', fixed = TRUE)
  expect_match(p$text, 'truncated')
  expect_true('p01' %in% p$selected_dependencies)
  expect_lte(nchar(p$text), 4500L)
  expect_identical(p$allocation_policy, 'paired-v1')
})

test_that("a warranted follow-up can recover an unavailable full review with explicit scope", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  fx$state$histories$p01 <- record_review_unavailable(fx$state$histories$p01, 'dependency context incomplete')
  reference <- file.path(fx$root, 'reference.rds'); saveRDS(data.frame(id = 1, value = 999), reference)
  fx$state$comparison_rules <- list(references = list(work.out1 = reference))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(component_review_verdict(result$histories$p01), 'reviewed_no_material_finding')
  expect_length(fx$state$fixer_llm$requests(), 0)
  expect_length(fx$state$reviewer_llm$requests(), 1)
  prompt <- paste(vapply(fx$state$reviewer_llm$requests()[[1]]$messages, `[[`, '', 'content'), collapse = '\n')
  expect_match(prompt, 'Full component review with additional focus', fixed = TRUE)
  expect_match(prompt, 'entire component', fixed = TRUE)
  expect_false(grepl('999|reference.rds', prompt))
  event <- Filter(function(e) identical(e$type, 'source_mismatch_review'), current_component_evidence(result$histories$p01)$events)[[1]]
  expect_identical(event$review_scope, 'full')
  expect_true(event$adopted)
  again <- review_bundle_mismatches(result, result$attempt, result$assessment, 1L)
  expect_length(fx$state$reviewer_llm$requests(), 1)
})

test_that("totals and categories stay distinct across ledger reconstruction and retries", {
  records <- list(
    list(record_type = 'request_started', request_id = 'one'),
    list(record_type = 'request_completed', request_id = 'one', input_tokens = 9,
      total_input_tokens = 44529, output_tokens = 3372, total_output_tokens = 13306,
      reasoning_tokens = 9934, cached_input_tokens = 44520),
    list(record_type = 'request_started', request_id = 'retry', retry_of = 'one'),
    list(record_type = 'request_completed', request_id = 'retry'))
  records <- c(records, records[2])
  budget <- new_usage_budget(); reconstruct_usage_budget(budget, records)
  usage <- migration_usage_summary(budget)
  expect_equal(usage$input_tokens, 44529)
  expect_equal(usage$output_tokens, 13306)
  expect_equal(usage$input_token_category, 9)
  expect_equal(usage$output_token_category, 3372)
  expect_equal(usage$reasoning_tokens, 9934)
  expect_equal(usage$cached_input_tokens, 44520)
  expect_equal(usage$unknown_input_token_calls, 1)
  expect_equal(usage$unknown_output_token_calls, 1)
  expect_true(is.na(migration_usage_summary(list())$input_tokens))
})

test_that("no-op and unchanged repair responses do not become implemented effects", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  fx$state <- stage_workflow_revision(fx$state, 'p01', 'invisible(NULL)', 'repair_required')
  fx$state$reviewer_llm <- recording_reviewer(function(req) material_review_response(
    sas_evidence = 'data work.out1 creates the required dataset', r_evidence = 'invisible(NULL) has no required effect'))
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(code = 'invisible(NULL)',
    diagnosis = 'Faithful repair unavailable', remaining_uncertainty = 'Required output remains unimplemented'))
  active <- fx$state$histories$p01$active_revision_id
  result <- process_program_component(fx$state, 'p01', max_program_repair_rounds = 2L)
  expect_identical(result$histories$p01$active_revision_id, active)
  expect_identical(component_review_verdict(result$histories$p01), 'repair_required')
  expect_length(fx$state$fixer_llm$requests(), 1)
  expect_length(fx$state$reviewer_llm$requests(), 1)
})

test_that("unavailable full review cannot recover from focused-only or stale evidence", {
  for (kind in c('focused', 'stale')) {
    fx <- repair_workflow_fixture(n = 1L, failures = integer())
    fx$state$histories$p01 <- record_review_unavailable(fx$state$histories$p01, 'context incomplete')
    attempt <- run_bundle_attempt(fx$state, sequence = 1L)
    reference <- file.path(fx$root, 'reference.rds'); saveRDS(data.frame(id = 1, value = 999), reference)
    assessment <- assess_final_outputs(fx$state$output_contracts, attempt, fx$state$graph,
      fx$state$histories, list(references = list(work.out1 = reference)))
    original_review <- review_program_revision
    local_mocked_bindings(review_program_revision = function(...) {
      review <- original_review(...)
      if (kind == 'focused') review$review_scope <- 'focused' else review$binding_hash <- 'earlier-binding'
      review
    })
    result <- review_bundle_mismatches(fx$state, attempt, assessment, 0L)
    expect_identical(component_review_verdict(result$histories$p01), 'review_unavailable')
    event <- tail(current_component_evidence(result$histories$p01)$events, 1)[[1]]
    expect_false(event$adopted)
  }
})

test_that("silent shared-helper regressions are reviewed at the final checkpoint", {
  fx <- repair_workflow_fixture(n = 2L, failures = 1L)
  fx$state <- stage_workflow_revision(fx$state, 'p02',
    sub('x$value + 1', 'shift(x$value)', fx$fixed$p02, fixed = TRUE), 'reviewed_no_material_finding')
  old_path <- fx$state$runtime$helpers
  writeLines(assemble_helper_overlay(runtime_helper_code(fx$state$runtime), 'shift <- function(x) x + 1'), old_path)
  old <- readLines(old_path)
  fx$state <- process_program_component(fx$state, 'p02')
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    if (req$component_id == 'p02') material_review_response(
      sas_evidence = 'value=value+1', r_evidence = 'shift(x) now adds 99',
      affected_outputs = 'work.out2') else valid_program_review_response()
  })
  fx$state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(code = fx$fixed$p01,
    bundle_helper_patch = list(path = 'sas2r-helpers.R', content = 'shift <- function(x) x + 99', reason = 'incorrect candidate')))
  result <- process_program_component(fx$state, 'p01')
  expect_identical(readLines(old_path), old)
  expect_true(result$selected_revisions$p02$smoke$passed)
  expect_false(identical(component_review_verdict(result$histories$p02), 'reviewed_no_material_finding'))
  expect_length(fx$state$reviewer_llm$requests(), 2L) # current component only
  result <- finalize_component_reviews(result)
  expect_identical(component_review_verdict(result$histories$p02), 'repair_required')
  expect_length(result$diagnostics$rejected_repairs, 0L)
  expect_length(fx$state$fixer_llm$requests(), 1L) # checkpoint never repairs
  bundle <- run_bundle_pipeline(result, max_bundle_repair_rounds = 0L)
  expect_true(bundle$attempt$passed)
  expect_false(bundle$status %in% c('migration_ready', 'validated'))
  queue <- bundle_repair_queue(result, bundle$attempt, bundle$assessment, list(failures = list()))
  expect_true('p02' %in% names(queue))
  expect_true(queue$p02$code_local)
})

test_that("live totals equal resumed totals without adding reasoning twice", {
  budget <- new_usage_budget()
  req <- llm_request(messages = list(list(role = 'user', content = 'synthetic request')), model = 'mock-model')
  reservation <- reserve_usage_request(budget, req, audit_context = list(provider = 'mock', resolved_model = 'mock-model'))
  response <- new_llm_response(status = 'completed', action = 'final', data = list(result = 'ok'),
    request = req, resolved_model = 'mock-model', provider = 'mock',
    usage = list(input_tokens = 9, output_tokens = 3372, cached_input_tokens = 44520,
      cache_write_tokens = 0, reasoning_tokens = 9934, total_input_tokens = 44529,
      total_output_tokens = 13306, total_tokens = 57835))
  reconcile_usage_request(budget, reservation, response)
  live <- migration_usage_summary(budget)
  restored <- new_usage_budget(); reconstruct_usage_budget(restored, budget$records)
  resumed <- migration_usage_summary(restored)
  for (field in c('input_tokens', 'output_tokens', 'reasoning_tokens', 'cached_input_tokens')) {
    expect_identical(live[[field]], resumed[[field]])
  }
  expect_equal(live$input_tokens, 44529)
  expect_equal(live$output_tokens, 13306)
  expect_equal(live$unknown_output_token_calls, 0)
})
