test_that("a continuing parallel run names warnings and reports skipped bundle work", {
  fx <- repair_workflow_fixture(n = 3L, failures = integer())
  # RETAIN requires agent translation, so the public API actually receives the
  # worker's dependency finding instead of keeping a complete baseline program.
  for (cid in c("p01", "p03")) {
    source <- file.path(fx$root, paste0(cid, ".sas"))
    writeLines(sub("value =", "retain marker 1; value =", readLines(source), fixed = TRUE), source)
    fx$fixed[[cid]] <- sub("x$value <-", "x$marker <- 1\nx$value <-", fx$fixed[[cid]], fixed = TRUE)
  }
  writeLines("data work.out2; set work.out1; value=value+1; run;", file.path(fx$root, "p02.sas"))
  responses <- list(
    "translator:p01" = valid_program_translation_response(fx$fixed$p01,
      suspected_dependencies = c("qtrim", "work.missing")),
    "translator:p03" = valid_program_translation_response("stop('translation fault p03')"),
    reviewer = valid_program_review_response(),
    fixer = valid_program_fix_response(fx$fixed$p03))
  events <- list()
  withr::local_options(sas2r.progress = TRUE)
  output <- capture.output(result <- withCallingHandlers(sas_translate(
    fx$root, out_dir = file.path(fx$root, "public-run"), config = fx$state$config,
    llm = parallel_test_llm(responses), max_parallel_translations = 2L,
    max_program_repair_rounds = 1L, max_bundle_repair_rounds = 1L,
    outputs = c("work.out2", "work.out3")),
    sas2r_progress = function(event) events[[length(events) + 1L]] <<- event), type = "message")
  expect_identical(result$status, "needs_review")
  expect_match(result$status_reason, "p01 (work.missing)", fixed = TRUE)
  expect_setequal(result$diagnostics$dependency_findings$p01$affected, c("p01", "p02"))
  expect_setequal(names(result$component_evidence), c("p01", "p02", "p03"))
  blocked <- which(vapply(events, function(e) identical(e$event, "dependency_warning"), logical(1)))
  expect_length(blocked, 1L)
  expect_identical(events[[blocked]]$severity, "warning")
  expect_true(any(vapply(events, function(e)
    identical(e$event, "program_smoke_passed") && identical(e$component_id, "p03"), logical(1))))
  log <- paste(output, collapse = "\n")
  expect_match(log, "WARNING: coordinator  p01", fixed = TRUE)
  expect_match(log, "Translation continues", fixed = TRUE)
  expect_match(log, "WARNING: Run requires review", fixed = TRUE)
  # p03 has no unresolved findings, so the bundle runs it; p01 and p02 wait.
  expect_match(log, "Bundle execution: EXECUTED (1 attempt; 1 ran to completion, 0 failed; 0 deferred, 0 incomplete); not executed: p01, p02", fixed = TRUE)
  expect_match(log, "Bundle-level fixes: NOT INVOKED", fixed = TRUE)
  expect_true(file.exists(file.path(result$outputs_dir, "datasets", "work", "out3.rds")))
  expect_false(any(grepl("out2", list.files(result$outputs_dir, recursive = TRUE), fixed = TRUE)))
  expect_false(result$status %in% c("migration_ready", "validated"))
  expect_match(log, "Component fixes: INVOKED (1 fixer invocation;", fixed = TRUE)

  report <- read_json_record(result$report_json_path)
  paths <- migration_paths(result$out_dir, result$run_id)
  saved <- paste(readLines(file.path(paths$logs, "run-outcome.log")), collapse = "\n")
  html <- paste(readLines(paths$start_here), collapse = "\n")
  expect_identical(report$outcome$severity, "warning")
  expect_match(html, '<section class="outcome warning"', fixed = TRUE)
  for (text in c("Run requires review", "p01 (work.missing)",
                 "not executed: p01, p02", "Required validation: REVIEW REQUIRED")) {
    expect_match(saved, text, fixed = TRUE)
    expect_match(html, text, fixed = TRUE)
  }
  expect_lt(regexpr("Run requires review", html, fixed = TRUE)[1L],
    regexpr('id="components"', html, fixed = TRUE)[1L])
})

test_that("a bundle that ran and invoked a fixer is not reported as skipped", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  state <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  write_migration_report(state)
  outcome <- read_json_record(state$paths$report_json)$outcome
  expect_match(outcome$stages[["Bundle execution"]], "EXECUTED (2 attempts", fixed = TRUE)
  expect_match(outcome$stages[["Bundle-level fixes"]], "INVOKED (1 fixer invocation;", fixed = TRUE)
  expect_identical(outcome$stages[["Component fixes"]], "NOT INVOKED")
})

test_that("failed bundles expose their actual exception and log links above advisories", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  state <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 0L)
  write_migration_report(state)
  report <- read_json_record(state$paths$report_json)
  html <- paste(readLines(state$paths$start_here), collapse = "\n")
  log <- paste(readLines(file.path(state$paths$logs, "run-outcome.log")), collapse = "\n")
  expect_true("p01" %in% report$outcome$affected_components)
  for (text in c("Bundle execution stopped in p01", "translation fault p01", "bundle_attempt_001")) {
    expect_match(html, text, fixed = TRUE)
    expect_match(log, text, fixed = TRUE)
  }
  expect_match(html, 'href="diagnostics/bundle_attempts/bundle_attempt_001/logs/bundle_stderr.log"', fixed = TRUE)
  expect_lt(regexpr("Bundle execution stopped in p01", html, fixed = TRUE)[1L],
    regexpr('id="components"', html, fixed = TRUE)[1L])
})

test_that("disabled execution and interrupted attempts are distinguished from executed bundles", {
  root <- withr::local_tempdir()
  writeLines("data work.out; x=1; run;", file.path(root, "p.sas"))
  result <- sas_translate(root, out_dir = file.path(root, "out"), execute = FALSE)
  report <- read_json_record(result$report_json_path)
  # This unsupported DATA step still contains an untranslated marker.
  expect_identical(report$outcome$severity, "error")
  expect_match(report$outcome$stages[["Bundle execution"]], "NOT RUN", fixed = TRUE)
  expect_match(report$outcome$stages[["Bundle execution"]], "execution disabled", fixed = TRUE)

  state <- new_migration_state(sas_project(root), file.path(root, "interrupted"))
  init_attempt(state$paths, kind = "bundle")
  state$status <- "blocked"
  state$diagnostics$failure <- list(stage = "bundle execution and repair", message = "connection lost")
  write_migration_report(state)
  outcome <- read_json_record(state$paths$report_json)$outcome
  expect_identical(outcome$title, "Run incomplete - failed")
  expect_match(outcome$stages[["Bundle execution"]], "INCOMPLETE", fixed = TRUE)
  expect_match(outcome$reason, "connection lost", fixed = TRUE)
})

test_that("histories without reviews retain the correct outstanding component names", {
  fx <- repair_workflow_fixture(n = 4L, failures = integer())
  state <- fx$state
  for (cid in c("p01", "p03")) {
    state$histories[[cid]] <- record_completed_review(state$histories[[cid]], verdict = "repair_required")
  }
  # A history before its first review is normalized by the authoritative reader.
  state$histories$p02 <- new_component_evidence_history("p02", state$selected_revisions$p02$binding)
  expect_identical(component_review_verdict(state$histories$p02), "review_unavailable")
  write_migration_report(state)
  report <- read_json_record(state$paths$report_json)
  expect_identical(report$component_evidence$p02$review_status, "review_unavailable")
  expect_identical(report$component_evidence$p03$review_status, "repair_required")
  expected <- "Separate outstanding static reviews (not proof these paths executed): p01, p02, p03"
  pending <- report$outcome$details[startsWith(report$outcome$details, "Separate outstanding static reviews")]
  expect_identical(unname(unlist(pending)), expected)
  for (path in c(state$paths$start_here, file.path(state$paths$logs, "run-outcome.log"))) {
    expect_match(paste(readLines(path), collapse = "\n"), expected, fixed = TRUE)
  }
})

test_that("retained selections and prior-run repairs do not imply current-run success", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  # Ordinary resume loads prior request records, but this run has made no calls.
  state$usage_budget$records <- list(list(record_type = "request_started", run_id = "prior-run",
    agent = "fixer", invocation_id = "prior-fix", request_id = "prior-request"))
  state$status <- "validated"
  state$current_run_status <- "blocked"
  state$status_reason <- "Current attempt failed; prior selection retained"
  write_migration_report(state)
  outcome <- read_json_record(state$paths$report_json)$outcome
  expect_identical(outcome$severity, "error")
  expect_identical(outcome$stages[["Component fixes"]], "NOT INVOKED")
  expect_match(outcome$stages[["Bundle execution"]], "NOT RUN", fixed = TRUE)
})

test_that("source-level claims that no automated repair addresses are named in the outcome", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- fx$state
  claim <- list(category = "source_syntax_claim", severity = "material",
    repair_disposition = "source_syntax_claim_only",
    sas_evidence = "The producer keeps only ID and VALUE, so the consumer's WHERE on FLAG cannot run.",
    r_evidence = "x$flag", affected_outputs = list("work.out2"), confidence = 0.9,
    unresolved_dependencies = list())
  context <- list(category = "missing_context", severity = "high", repair_disposition = "context_available",
    sas_evidence = "The caller invokes %helper.", r_evidence = "helper('x')",
    affected_outputs = list("work.out1"), confidence = 0.9, unresolved_dependencies = list("helper"))
  defect <- list(category = "translation_defect", severity = "material", repair_disposition = "unverified",
    sas_evidence = "SAS keeps the last record.", r_evidence = "x[1, ]",
    affected_outputs = list("work.out1"), confidence = 0.9, unresolved_dependencies = list())
  state$histories$p01 <- record_completed_review(state$histories$p01, verdict = "repair_required",
    findings = list(claim, context))
  state$histories$p02 <- record_completed_review(state$histories$p02, verdict = "repair_required",
    findings = list(defect))
  write_migration_report(state)
  report <- read_json_record(state$paths$report_json)
  details <- unlist(report$outcome$details)
  judgment <- details[startsWith(details, "Findings for human judgment")]
  expect_length(judgment, 1L)
  expect_match(judgment, "p01: The producer keeps only ID and VALUE", fixed = TRUE)
  expect_match(judgment, "The caller invokes %helper.", fixed = TRUE)
  expect_false(grepl("p02", judgment, fixed = TRUE))
  expect_false(grepl("last record", judgment, fixed = TRUE))
  for (path in c(state$paths$start_here, file.path(state$paths$logs, "run-outcome.log"))) {
    expect_match(paste(readLines(path), collapse = "\n"), "Findings for human judgment", fixed = TRUE)
  }
})

test_that("configured targets are counted before any bundle attempt assesses them", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  coverage <- migration_coverage(list(), fx$state$histories, contracts = fx$state$output_contracts)
  expect_identical(coverage$outputs_total, 2L)
  expect_identical(coverage$outputs_produced, 0L)
  expect_setequal(names(coverage$targets), c("work.out1", "work.out2"))
  expect_false(coverage$targets$work.out1$produced)
  write_migration_report(fx$state)
  report <- read_json_record(fx$state$paths$report_json)
  expect_identical(report$coverage$outputs_total, 2L)
  expect_match(paste(readLines(fx$state$paths$report_md), collapse = "\n"),
    "0 produced / 2 targets", fixed = TRUE)
})
