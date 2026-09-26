# The installed-tests CI job runs this full process integration matrix.
skip_on_cran()

test_that("thread configuration is explicit, validated, and not semantic review evidence", {
  expect_identical(normalize_migration_config(NULL)$max_parallel_translations, 1L)
  for (bad in list(0, -1, 1.5, Inf, NA_real_, TRUE, "2", c(1, 2)))
    expect_error(normalize_max_parallel_translations(bad), class = "sas2r_config_error")
  root <- withr::local_tempdir()
  writeLines("data out; x=1; run;", file.path(root, "p.sas"))
  writeLines(c("migration:", "  max_parallel_translations: 4"), file.path(root, "_sas2r.yml"))
  expect_identical(sas_config(start = root)$migration$max_parallel_translations, 4L)
  expect_identical(sas_preflight(root)$max_parallel_translations, 4L)
  expect_identical(sas_preflight(root, max_parallel_translations = 2)$max_parallel_translations, 2L)
  a <- sas_config(start = root); b <- a
  b$migration$max_parallel_translations <- b$raw$migration$max_parallel_translations <- 2L
  expect_identical(source_review_config(a), source_review_config(b))
})

test_that("unsupported adapters visibly fall back before any worker calls", {
  state <- list(translator_llm = mock_llm(list()), reviewer_llm = NULL, fixer_llm = NULL)
  expect_message(result <- resolve_parallel_execution(state, 4L), "cannot be reconstructed")
  expect_identical(result$effective, 1L)
})

test_that("available test slots preserve source-defined outputs and shared accounting", {
  full_suite <- identical(Sys.getenv("NOT_CRAN"), "true")
  slots <- if (full_suite) 1:4 else 1:2
  for (threads in slots) {
    # Two independent programs exercise overlap on CRAN; full CI also covers
    # queue turnover and every supported worker count with four programs.
    fx <- repair_workflow_fixture(n = if (full_suite) 4L else 2L, failures = integer())
    expected_calls <- 2L * length(fx$ids) # One translation and one review per program.
    markers <- file.path(fx$root, "requests"); dir.create(markers)
    responses <- stats::setNames(lapply(fx$fixed, valid_program_translation_response), paste0("translator:", fx$ids))
    responses$reviewer <- valid_program_review_response()
    barrier <- NULL
    if (threads > 1L) {
      arrival_dir <- file.path(fx$root, "translator-arrivals")
      dir.create(arrival_dir)
      # Worker startup is slower than a mock call on some Windows runners.
      # Synchronize the first wave instead of depending on startup timing.
      barrier <- list(dir = arrival_dir, count = threads)
    }
    llm <- parallel_test_llm(responses, delay = 0.3, marker_dir = markers,
      translator_barrier = barrier)
    state <- fx$state
    state$selected_revisions <- state$histories <- list()
    state$baseline$manifest$tier <- "stub"
    state$baseline$manifest$reason <- "agent_translation_required"
    state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
    state$parallel <- resolve_parallel_execution(state, threads)
    result <- run_program_pipeline(state, execute = TRUE)
    expect_true(all(vapply(result$selected_revisions, function(r) isTRUE(r$smoke$passed), logical(1))))
    expect_true(all(vapply(result$histories, component_review_verdict, "") == "reviewed_no_material_finding"))
    expect_identical(result$usage_budget$request_count, expected_calls)
    expect_equal(result$usage_budget$known_amount, 0.01 * expected_calls)
    write_migration_report(result)
    manifest <- read_json_record(result$paths$manifest)
    for (cid in fx$ids) {
      expect_identical(manifest$components[[cid]]$revision_path, result$selected_revisions[[cid]]$r_path)
      expect_true(file.exists(manifest$components[[cid]]$revision_path))
    }
    calls <- lapply(list.files(markers, full.names = TRUE), readRDS)
    expect_length(calls, expected_calls)
    overlaps <- vapply(calls, function(call) sum(vapply(calls, function(other)
      other$start <= call$start && other$end > call$start, logical(1))), integer(1))
    expect_lte(max(overlaps), threads)
    if (threads > 1L) expect_gt(max(overlaps), 1L)
    for (i in seq_along(fx$ids)) {
      smoke <- result$selected_revisions[[fx$ids[i]]]$smoke
      expect_length(passed_population_checks(smoke), 1L)
      expect_equal(lapply(readRDS(smoke$output_files[[paste0("out", i, ".rds")]]), identity),
        list(id = 1:3, value = 11:13))
    }
  }
})

test_that("dependency chain settles its producer before dispatching a consumer", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer(), chain = TRUE)
  state <- fx$state
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()))
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  events <- list()
  result <- withCallingHandlers(run_program_pipeline(state), sas2r_progress = function(event) {
    events[[length(events) + 1L]] <<- list(event = event$event, cid = event$component_id, classes = class(event))
  })
  smoke <- which(vapply(events, function(e) identical(e$event, "program_smoke_passed"), logical(1)))
  generated <- which(vapply(events, function(e) identical(e$event, "program_generated"), logical(1)))
  expect_true(all(vapply(result$selected_revisions, function(r) isTRUE(r$smoke$passed), logical(1))))
  first_generated <- vapply(fx$ids, function(cid) min(which(vapply(events, function(e)
    identical(e$event, "program_generated") && identical(e$cid, cid), logical(1)))), integer(1))
  for (i in seq_along(fx$ids)) expect_equal(lapply(readRDS(result$selected_revisions[[fx$ids[i]]]$smoke$output_files[[paste0("out", i, ".rds")]]), identity),
    list(id = 1:3, value = (10:12) + i))
  expect_true(all(vapply(events[smoke], function(event) "sas2r_program_smoke_event" %in% event$classes, logical(1))))
  expect_lt(smoke[1], first_generated[2])
  expect_lt(smoke[2], first_generated[3])
})

test_that("concurrent workers share one request ceiling and attribute each invocation's spend", {
  fx <- repair_workflow_fixture(n = 4L, failures = integer())
  markers <- file.path(fx$root, "requests"); dir.create(markers)
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()), marker_dir = markers)
  state <- fx$state
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$usage_budget <- new_usage_budget(max_calls = 2L)
  state$parallel <- resolve_parallel_execution(state, 2L)
  for (cid in fx$ids) state <- check_component_revision(state, cid)
  result <- finalize_parallel_component_reviews(state)
  expect_identical(result$usage_budget$request_count, 2L)
  expect_equal(result$usage_budget$known_amount, 0.02)
  expect_length(list.files(markers), 2L)
  log <- lapply(readLines(file.path(state$paths$logs, "llm_log.jsonl")), jsonlite::fromJSON)
  completed <- Filter(function(record) identical(record$status, "completed"), log)
  expect_length(completed, 2L)
  expect_true(all(vapply(completed, function(record) identical(record$cost_usd, 0.01), logical(1))))
})

test_that("temporary dollar holds wait, while actual exhaustion refuses", {
  rates <- list(list(provider = "mock", resolved_model = "m", region = "*", service_tier = "frontier",
    currency = "USD", source = "fixture", source_version = "v1", effective_date = "2000-01-01",
    input_per_million = 1, output_per_million = 1, cached_input_per_million = 0,
    cache_write_per_million = 0, reasoning_per_million = 0, tool_rates = list()))
  budget <- new_usage_budget(mode = "strict", max_usd = 0.002, rates = rates)
  request <- llm_request(messages = list(list(role = "user", content = "ping")), model = "m", max_output_tokens = 1000L)
  context <- list(provider = "mock", resolved_model = "m", tier = "frontier")
  held <- reserve_usage_request(budget, request, context)
  expect_true(parallel_reservation_wait(budget, request, context))
  expect_gt(budget$reserved_amount, 0)
  reconcile_usage_request(budget, held, list(status = "completed", cost = list(
    amount_usd = 0, status = "billed_amount", currency = "USD")))
  expect_false(parallel_reservation_wait(budget, request, context))
  budget$known_amount <- budget$max_usd
  update_usage_remaining(budget)
  expect_false(parallel_reservation_wait(budget, request, context))
  expect_false(usage_budget_allows_future(budget))
})

test_that("checkpoint import preserves known allowances and unknown legacy revisit history", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$repair_counts <- list(p01 = 1L)
  state$revisit_counts <- c(p01 = 2L)
  state$resume_fingerprint <- migration_resume_fingerprint(state)
  write_migration_checkpoint(state, state$resume_fingerprint)
  restored <- restore_migration_checkpoint(fx$state, state$resume_fingerprint)
  expect_identical(restored$repair_counts, state$repair_counts)
  expect_identical(restored$revisit_counts, state$revisit_counts)
  path <- file.path(state$paths$state, "resume.rds")
  checkpoint <- readRDS(path)
  checkpoint$version <- 8L
  checkpoint$fingerprint <- migration_resume_fingerprint(state, version = 8L)
  checkpoint$revisit_counts <- NULL
  saveRDS(checkpoint, path)
  imported <- restore_migration_checkpoint(fx$state, state$resume_fingerprint)
  expect_identical(imported$repair_counts, state$repair_counts)
  expect_true(is.na(imported$revisit_counts[["p01"]]))
  expect_match(imported$diagnostics$resume_import, "unknown")
})

test_that("an interrupted worker retains admitted usage and useful failure diagnostics", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- check_component_revision(fx$state, "p01")
  # Leave a bounded interruption window after admission. Inf is not portable:
  # Windows R converts the sleep duration to an integer number of milliseconds.
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 300)
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  pool <- parallel_new_pool(state)
  on.exit(parallel_stop_pool(pool), add = TRUE)
  job <- parallel_start_job(pool, state, "p01", "review", FALSE, 0L)
  deadline <- Sys.time() + 120
  while (state$usage_budget$request_count == 0L && Sys.time() < deadline) {
    parallel_poll(pool)
    Sys.sleep(0.02)
  }
  expect_identical(state$usage_budget$request_count, 1L)
  if (state$usage_budget$request_count != 1L) {
    stop("Worker did not reach request admission within 120 seconds")
  }
  job$process$kill_tree()
  job$process$wait(2000)
  expect_length(parallel_poll(pool), 0L)
  expect_error(parallel_abort_failure(pool), class = "sas2r_parallel_worker_error")
  expect_identical(state$usage_budget$request_count, 1L)
  expect_identical(state$usage_budget$unknown_count, 1L)
  expect_length(state$usage_budget$reservations, 1L)
  expect_true(state$usage_budget$reservations[[1L]]$abandoned)
  expect_true(file.exists(file.path(job$dir, "stderr.log")))
})

test_that("a crashed reviewer preserves accounting and continues remaining reviews", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  state <- fx$state
  for (cid in fx$ids) state <- check_component_revision(state, cid)
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()),
    delay = c(p01 = 0.8, p02 = 1.6, p03 = 0), crash_component = "p01")
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  state$resume_fingerprint <- migration_resume_fingerprint(state)
  events <- list()
  result <- withCallingHandlers(finalize_parallel_component_reviews(state),
    sas2r_progress = function(e) events[[length(events) + 1L]] <<- e)
  failures <- Filter(function(e) identical(e$event, "component_failed"), events)
  expect_length(failures, 1L)
  expect_identical(failures[[1L]]$component_id, "p01")
  expect_identical(failures[[1L]]$severity, "warning")
  expect_match(format_sas2r_progress(failures[[1L]]), "other programs will continue", fixed = TRUE)
  saved <- readRDS(file.path(state$paths$state, "resume.rds"))
  expect_identical(component_review_verdict(saved$histories$p02), "reviewed_no_material_finding")
  expect_identical(component_review_verdict(saved$histories$p03), "reviewed_no_material_finding")
  expect_identical(state$usage_budget$request_count, 3L)
  expect_identical(state$usage_budget$unknown_count, 1L)
  expect_identical(saved$component_stage$p01, "failed")
  expect_identical(saved$diagnostics$worker_failures[[1L]]$component_id, "p01")
})

test_that("a crashed translator preserves completed sibling drafts for resume", {
  skip_on_cran() # Extended integration scenario; both installed-package CI jobs run it.
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  state <- fx$state
  state$selected_revisions <- state$histories <- list()
  state$baseline$manifest$tier <- "stub"
  state$baseline$manifest$reason <- "agent_translation_required"
  responses <- stats::setNames(lapply(fx$fixed, valid_program_translation_response), paste0("translator:", fx$ids))
  responses$reviewer <- valid_program_review_response()
  llm <- parallel_test_llm(responses,
    delay = c(p01 = 0.8, p02 = 1.6, p03 = 0), crash_component = "p01")
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  state$resume_fingerprint <- migration_resume_fingerprint(state)
  initial <- run_program_pipeline(state, execute = FALSE)
  saved <- readRDS(file.path(state$paths$state, "resume.rds"))
  expect_identical(saved$component_stage$p02, "settled")
  expect_true(file.exists(saved$selected_revisions$p02$r_path))
  expect_true(file.exists(saved$selected_revisions$p03$r_path))
  expect_identical(state$usage_budget$request_count, 5L) # crashed call plus both siblings translation/review
  expect_identical(state$usage_budget$unknown_count, 1L)
  # Successful sibling work is reusable; the failed component is retried.
  llm <- parallel_test_llm(responses, delay = 0)
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  resumed <- restore_migration_checkpoint(state, state$resume_fingerprint)
  before <- resumed$usage_budget$request_count
  result <- run_program_pipeline(resumed, execute = FALSE)
  expect_identical(result$component_stage$p02, "settled")
  expect_true(all(vapply(result$histories, component_review_verdict, "") == "reviewed_no_material_finding"))
  expect_identical(result$usage_budget$request_count - before, 2L) # failed component translation/review
})

test_that("helper repair rollback preserves other selected programs with parallel drafts", {
  skip_on_cran() # Extended integration scenario; both installed-package CI jobs run it.
  fx <- repair_workflow_fixture(n = 2L, failures = 1L)
  state <- stage_workflow_revision(fx$state, "p02",
    sub("x$value + 1", "shift(x$value)", fx$fixed$p02, fixed = TRUE), "reviewed_no_material_finding")
  writeLines(assemble_helper_overlay(runtime_helper_code(state$runtime), "shift <- function(x) x + 1"),
    state$runtime$helpers)
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response(),
    fixer = valid_program_fix_response(code = fx$fixed$p01, bundle_helper_patch = list(
      path = "sas2r-helpers.R", content = "shift <- function(x) stop('broken candidate')", reason = "bad fixture"))))
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state <- process_program_component(state, "p02")
  state$resumed_components <- "p02"
  state$parallel <- resolve_parallel_execution(state, 2L)
  retained_helper <- runtime_helper_code(state$runtime)
  result <- run_program_pipeline(state)
  expect_identical(runtime_helper_code(result$runtime), retained_helper)
  expect_true(result$selected_revisions$p02$smoke$passed)
  expect_match(paste(result$diagnostics$rejected_repairs[[1L]]$errors, collapse = " "), "p02 execution regressed")
  expect_identical(result$repair_counts$p01, 1L)
})

test_that("dependency uncertainty permits drafts while deferring execution", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  state <- fx$state
  state$selected_revisions$p01$contract$suspected_dependencies <- "work.missing"
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()))
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  result <- run_program_pipeline(state, execute = FALSE)
  expect_identical(result$component_stage$p01, "settled")
  expect_match(component_execution_reasons(result, "p01"), "work.missing", fixed = TRUE)
  expect_identical(result$component_stage$p02, "settled")
  expect_identical(result$component_stage$p03, "settled")
  expect_identical(result$diagnostics$dependency_findings$p01$findings, "work.missing")
})

test_that("abandoned strict reservations remain unknown and held after resume", {
  path <- file.path(withr::local_tempdir(), "usage.jsonl")
  rates <- list(list(provider = "mock", resolved_model = "m", region = "*", service_tier = "frontier",
    currency = "USD", source = "fixture", source_version = "v1", effective_date = "2000-01-01",
    input_per_million = 1, output_per_million = 1, cached_input_per_million = 0,
    cache_write_per_million = 0, reasoning_per_million = 0, tool_rates = list()))
  budget <- new_usage_budget(ledger_path = path, mode = "strict", max_usd = 0.002, rates = rates)
  request <- llm_request(messages = list(list(role = "user", content = "ping")), model = "m", max_output_tokens = 1000L)
  reservation <- reserve_usage_request(budget, request, list(provider = "mock", resolved_model = "m", tier = "frontier"))
  abandon_usage_request(budget, reservation$request_id)
  abandon_usage_request(budget, reservation$request_id)
  expect_identical(budget$unknown_count, 1L)
  restored <- load_usage_budget(path, mode = "strict", max_usd = 0.002, rates = rates)
  expect_gt(restored$reserved_amount, 0)
  expect_equal(restored$reserved_amount, budget$reserved_amount)
  expect_identical(restored$known_amount, 0)
  expect_identical(restored$unknown_count, 1L)
  expect_true(restored$reservations[[reservation$request_id]]$abandoned)
  expect_false(parallel_reservation_wait(restored, request))
})

test_that("source-resolved dataset and component names are confirmations, not new findings", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  state <- fx$state
  state$selected_revisions$p01$contract$discovered_dependencies <- "RAW.INPUT"
  state$selected_revisions$p02$contract$discovered_dependencies <- c("work.out1", "p01")
  expect_length(parallel_dependency_findings(state, "p01"), 0L)
  expect_length(parallel_dependency_findings(state, "p02"), 0L)
})

test_that("supplied SAS macro findings use scanner rules without hiding unknown dependencies", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$selected_revisions$p01$contract$suspected_dependencies <- c("qleft", "%QTRIM", "kleft")
  state$selected_revisions$p01$contract$discovered_dependencies <-
    c("work.qtrim", "missing_program", "%missing_macro", "%qklength")
  expect_identical(parallel_dependency_findings(state, "p01"),
    c("work.qtrim", "missing_program", "%missing_macro", "%qklength"))
})

test_that("dependency prose remains an observation and does not defer independent work", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  observations <- c(
    "sdtm.dm must carry studyid, usubjid, subjid, siteid, age, sex, race, ethnic, arm",
    "sdtm.ex must carry studyid, usubjid, exstdtc, exendtc.",
    "sdtm.ds must carry studyid, usubjid, dscat, dsdecod, dsstdtc.",
    "sdtm.vs column set (STUDYID/USUBJID/VSTESTCD/VSTEST/VSSTRESN/VSSTRESU/VISIT/VISITNUM)",
    "adam.adsl produced by the upstream derive_adsl component")
  state <- fx$state
  state$selected_revisions$p01$contract$suspected_dependencies <- observations
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()))
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$parallel <- resolve_parallel_execution(state, 2L)
  result <- run_program_pipeline(state, execute = FALSE)
  expect_identical(result$component_stage$p01, "settled")
  expect_identical(result$component_stage$p02, "settled")
  expect_identical(result$selected_revisions$p01$contract$suspected_dependencies, observations)
  state$selected_revisions$p01$contract$discovered_dependencies <- c("missing_program", "%missing_macro")
  expect_identical(parallel_dependency_findings(state, "p01"), c("missing_program", "%missing_macro"))
})

test_that("a join waits for both producers and retains complete source-defined values", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  writeLines(paste("data work.out3; merge work.out1(rename=(value=v1))",
    "work.out2(rename=(value=v2)); by id; total=v1+v2; keep id total; run;"), file.path(fx$root, "p03.sas"))
  fixed <- fx$fixed
  fixed$p03 <- paste("a <- lib_read('work', 'out1')", "b <- lib_read('work', 'out2')",
    "x <- data.frame(id=a$id, total=a$value+b$value)", "lib_write(x, 'work', 'out3')", sep = "\n")
  project <- sas_project(fx$root, config = fx$state$config)
  state <- new_migration_state(project, file.path(fx$root, "join"), config = fx$state$config)
  responses <- stats::setNames(lapply(fixed, valid_program_translation_response), paste0("translator:", fx$ids))
  responses$reviewer <- valid_program_review_response()
  llm <- parallel_test_llm(responses)
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  state$baseline$manifest$tier <- "stub"
  state$baseline$manifest$reason <- "agent_translation_required"
  state$parallel <- resolve_parallel_execution(state, 2L)
  settled <- character()
  result <- withCallingHandlers(run_program_pipeline(state), sas2r_progress = function(event) {
    if (identical(event$event, "program_smoke_passed")) settled <<- union(settled, event$component_id)
    if (identical(event$event, "agent_started") && identical(event$agent, "translator") &&
        identical(event$component_id, "p03")) expect_setequal(settled, c("p01", "p02"))
  })
  attempt <- run_bundle_attempt(result, sequence = 1L)
  expect_true(attempt$passed)
  expect_equal(lapply(readRDS(file.path(attempt$work_dir, "out3.rds")), identity),
    list(id = 1:3, total = c(22, 24, 26)))
})

test_that("a worker receives equivalent review messages, settings and tool contracts on the same snapshot", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  markers <- file.path(fx$root, "requests"); dir.create(markers)
  state <- check_component_revision(fx$state, "p01")
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()), marker_dir = markers)
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  direct <- review_component_revision(state, "p01")
  state$parallel <- resolve_parallel_execution(state, 2L)
  parallel <- finalize_parallel_component_reviews(state)
  requests <- lapply(list.files(markers, full.names = TRUE), function(path) readRDS(path)$request)
  expect_length(requests, 2L)
  # Requests intentionally have unique task-block delimiters. Compare all
  # policy, source, settings and tool contracts after normalizing only those labels.
  comparable <- lapply(requests, function(request) {
    request$messages <- lapply(request$messages, function(message) {
      message$content <- gsub("(?m)^(BEGIN|END) SAS2R_TASK_[^ ]+ ",
        "\\1 SAS2R_TASK_NONCE ", message$content, perl = TRUE)
      message
    })
    request
  })
  expect_identical(comparable[[1L]], comparable[[2L]])
  expect_match(paste(vapply(requests[[1L]]$messages, `[[`, "", "content"), collapse = "\n"),
    "data work.out1; set raw.input; value = value + 1; run;", fixed = TRUE)
  expect_identical(component_review_verdict(direct$state$histories$p01), component_review_verdict(parallel$histories$p01))
})
