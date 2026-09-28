

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
