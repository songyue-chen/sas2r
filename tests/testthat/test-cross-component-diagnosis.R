test_that("one canonical summary separates human magnitudes from investigation patterns", {
  target <- discrepancy_fixture()
  summary <- dataset_discrepancy_summary(target)
  expect_equal(summary$generated, list(rows = 3, columns = 2))
  expect_equal(summary$reference, list(rows = 3, columns = 2))
  expect_equal(summary$rows_aligned, 3)
  expect_match(discrepancy_description(summary), "absolute 3653", fixed = TRUE)
  # A numerical offset alone never proves a date representation or its cause.
  expect_false(grepl("epoch|date|Date", discrepancy_description(summary)))
  packet <- as.character(jsonlite::toJSON(reviewer_discrepancy_summary(target), auto_unbox = TRUE))
  expect_match(packet, "CONSTANT_OFFSET", fixed = TRUE)
  expect_false(grepl("3653|RECORD_SENTINEL|REFERENCE_PATH_SENTINEL|101|102|103", packet))
  expect_null(reviewer_discrepancy_summary(target)$variables[[1]]$absolute_offset)
  expect_identical(names(reviewer_discrepancy_summary(target)$variables[[1]]),
    c("name", "kind", "differences", "patterns"))
  table <- discrepancy_table(list(summary))
  expect_identical(table$`Rows aligned`, "100.0% (3/3)")
  expect_identical(table$`Columns present in both`, "100.0% (2/2)")
  summary$generated$rows <- 0; summary$reference$rows <- 0; summary$rows_aligned <- 0
  expect_identical(discrepancy_table(list(summary))$`Rows aligned`, "0/0 (both empty)")
  expect_identical(discrepancy_table(list(dataset_discrepancy_summary(list(target_key = "missing"))))$`Rows aligned`, "Unavailable")
})

test_that("execution diagnostics never read files or forward data-bearing messages", {
  expect_identical(bounded_agent_diagnostics(list(passed = TRUE))$condition_kind, "none")
  execution <- list(component_id = "main", passed = FALSE,
    condition = list(message = "Record PRIVATE_RECORD_123 failed with value 998877", class = c("simpleError", "PRIVATE_RECORD_123", "error", "condition"),
      call = 'stop("PRIVATE_RECORD_123")'), stdout_path = "/not-opened/stdout.log", stderr_path = "/not-opened/stderr.log",
    output_metadata = list(preview = "PRIVATE_RECORD_123"))
  for (policy in c("code_only", "bounded", "full")) {
    packet <- bounded_agent_diagnostics(execution, policy, source_code = 'x <- lib_read("work", "stage")')
    text <- as.character(jsonlite::toJSON(packet, auto_unbox = TRUE))
    expect_false(grepl("PRIVATE_RECORD_123|998877", text))
    expect_identical(packet$condition_kind, "unclassified_error")
    expect_identical(packet$source_location, NA_character_)
    expect_null(packet$output_metadata)
    expect_null(packet$output_previews)
    expect_length(packet$log_excerpt, 0)
  }
  execution$condition <- list(message = "Dataset not found: work.stage", class = c("simpleError", "error"), call = 'lib_read("work", "stage")')
  facts <- bounded_agent_diagnostics(execution, source_code = 'x <- lib_read("work", "stage")')
  expect_identical(facts$condition_kind, "dataset_not_found")
  expect_identical(facts$condition_identifiers, "work.stage")
  expect_identical(facts$source_location, 'lib_read("work", "stage")')
  expect_identical(agent_error_facts(list(message = "object 'PRIVATE_RECORD_123' not found"), "x <- stage")$kind, "unclassified_error")
  expect_identical(agent_error_facts(list(message = "object 'stage' not found"), "x <- stage")$identifiers, "stage")
})

test_that("transitive code is available to every role and invalidates selected context", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  dir.create(file.path(root, "programs"))
  writeLines(c("macros:", "  search_path: [macros]", "migration:",
    "  execution_order: [programs/unrelated.sas, programs/main.sas]"), file.path(root, "_sas2r.yml"))
  writeLines("%macro inside(); %let note=1; %mend;", file.path(root, "macros", "inside.sas"))
  writeLines("%macro outside(); %inside(); %mend;", file.path(root, "macros", "outside.sas"))
  writeLines("%outside();", file.path(root, "programs", "main.sas"))
  writeLines("data work.unrelated; x=1; run;", file.path(root, "programs", "unrelated.sas"))
  project <- sas_project(file.path(root, "programs"))
  state <- new_migration_state(project, withr::local_tempdir())
  selected <- list(macro__inside = list(revision_id = "inside-r1", r_code = "inside <- function() 1L"),
    macro__outside = list(revision_id = "outside-r1", r_code = "outside <- function() inside()"))
  llm <- recording_reviewer(function(req) valid_program_translation_response("outside()"))
  rev <- generate_program_revision("main", project, state$baseline, state$graph,
    state$schedule, state$output_contracts, llm = llm, paths = state$paths,
    selected_revisions = selected, config = state$config)
  reviewer <- recording_reviewer(function(req) valid_program_review_response())
  review_program_revision(rev, list(project = project, selected_revisions = selected, config = state$config), reviewer, paths = state$paths)
  fixer <- recording_fixer(function(req) valid_program_fix_response("outside()"))
  fix_program_revision(rev, checks = list(check_id = "synthetic", errors = "example"),
    llm = fixer, paths = state$paths, project = project, config = state$config, selected_revisions = selected)
  for (role in list(llm, reviewer, fixer)) {
    tool <- role$requests()[[1]]$tools$read_dependency_context$call
    expect_identical(tool(list(component_id = "macro__inside", language = "r"))$code, "inside <- function() 1L")
    expect_identical(tool(list(component_id = "unrelated", language = "r"))$error, "not_a_related_dependency_or_consumer")
    expect_identical(tool(list(component_id = "unrelated", language = "r"))$error, "unchanged_context_unavailable")
  }
  before <- build_agent_guidance(project, "main", selected_revisions = selected)
  selected$macro__inside$r_code <- "inside <- function() 2L"
  after <- build_agent_guidance(project, "main", selected_revisions = selected)
  expect_false(identical(before$identity, after$identity))
  expect_false("unrelated" %in% names(agent_dependency_bodies(project, "main", selected)))
})

test_that("ordered WORK writer facts distinguish a preceding writer from no producer", {
  root <- withr::local_tempdir()
  files <- c("first", "reader", "last", "unknown")
  source <- c("data work.stage; x=1; run;", "data work.answer; set work.stage; run;",
    "data work.stage; x=2; run;", "data work.answer2; set work.dynamic; run;")
  for (i in seq_along(files)) writeLines(source[i], file.path(root, paste0(files[i], ".sas")))
  project <- sas_project(root, config = list(migration = list(execution_order = paste0(files, ".sas"))))
  read <- component_read_context(project$graph, "reader")$reads[[1]]
  expect_identical(read$writer, "first")
  expect_identical(read$writer_status, "selected_by_source_order")
  unknown <- component_read_context(project$graph, "unknown")
  expect_identical(unknown$reads[[1]]$writer_status, "no_producer")
  expect_length(unknown$possible, 0)
})

test_that("report diagnosis is a one-way bounded request with explicit off and budget states", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()),
    lineage_by_target = list(work.result = list(upstream_components = "p01")))
  requests <- list()
  state$reviewer_llm <- diagnosis_mock(list(explanations = list(list(target = "work.result", component_id = "p01",
    sas_evidence = "value=value+1", r_evidence = "value + 1", possible_cause = "Cause unresolved; code appears faithful.",
    next_action = "Check which source and input versions produced the reference."))),
    callback = function(request) requests[[length(requests) + 1L]] <<- request)
  before <- state
  advice <- migration_report_diagnosis(state)
  expect_identical(advice$status, "completed")
  expect_length(requests, 1)
  text <- paste(vapply(requests[[1]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_match(text, "CONSTANT_OFFSET", fixed = TRUE)
  expect_false(grepl("RECORD_SENTINEL|REFERENCE_PATH_SENTINEL|3653", text))
  expect_length(requests[[1]]$tools, 0)
  expect_identical(state$assessment, before$assessment)
  expect_identical(state$selected_revisions, before$selected_revisions)
  expect_identical(state$status, before$status)
  expect_true(any(vapply(state$usage_budget$records, function(x) identical(x$agent, "report_diagnosis"), logical(1))))
  state$config$migration$report_diagnosis <- "off"
  expect_identical(migration_report_diagnosis(state)$status, "disabled")
  state$config$migration$report_diagnosis <- "auto"
  state$usage_budget$max_calls <- 0
  expect_identical(migration_report_diagnosis(state)$status, "budget_unavailable")
  state$reviewer_llm <- NULL
  expect_identical(migration_report_diagnosis(state)$status, "not_configured")
  expect_length(requests, 1)
})

test_that("human start page shows measurements, uncertainty and repairs before warnings", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()))
  state$report_diagnosis <- list(status = "not_configured", reason = "No reviewer LLM configured.")
  state$diagnostics$rejected_repairs <- list(list(component_id = "p01", revision_id = "r2", errors = "Source contradiction <unsafe>"))
  write_migration_report(state)
  html <- paste(readLines(state$paths$start_here), collapse = "\n")
  report <- read_json_record(state$paths$report_json)
  expect_identical(report$output_assessments$work.result$status, "failed")
  for (text in c("100.0% (3/3)", "absolute 3653", "Cause unresolved", "No reviewer LLM", "Rejected; previous code retained", "Source contradiction &lt;unsafe&gt;"))
    expect_match(html, text, fixed = TRUE)
  expect_lt(regexpr("Dataset discrepancies", html)[1], regexpr("Warnings and diagnostics", html)[1])
  expect_match(html, "Rows aligned means paired records, not equal values", fixed = TRUE)
})

test_that("mismatch summaries investigate the upstream defect while fixers and acceptance stay blind", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE, value_errors = 1L)
  refs <- list()
  for (i in 1:2) {
    path <- file.path(fx$root, paste0("REFERENCE_PATH_SENTINEL_", i, ".rds"))
    saveRDS(data.frame(id = 1:3, value = 70001:70003 + i), path)
    refs[[paste0("work.out", i)]] <- path
  }
  fx$state$comparison_rules <- list(references = refs)
  repaired <- character()
  fx$state$fixer_llm <- recording_fixer(function(req) {
    repaired <<- c(repaired, req$component_id)
    valid_program_fix_response(fx$fixed[[req$component_id]])
  })
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    text <- request_task_text(req)
    if (req$component_id == "p01" && grepl("x$value <- x$value + 9", text, fixed = TRUE))
      material_review_response(sas_evidence = "value = value + 1", r_evidence = "x$value <- x$value + 9")
    else valid_program_review_response()
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_identical(result$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_identical(result$selected_revisions$p02$r_code, fx$fixed$p02)
  expect_identical(result$status, "blocked") # source correction cannot satisfy these references
  expect_length(fx$state$fixer_llm$requests(), 1L)
  expect_identical(repaired, "p01")
  expect_true(result$repairs[[1]]$reference_triggered_review)
  expect_false(anyDuplicated(names(result$repairs[[1]])) > 0L)
  expect_match(paste(report_repair_table(result)$Reason, collapse = " "), "prompted by a reference discrepancy")
  reviewer_text <- vapply(fx$state$reviewer_llm$requests(), request_task_text, "")
  expect_true(any(grepl("CONSTANT_OFFSET", reviewer_text, fixed = TRUE)))
  focused <- reviewer_text[grepl("Investigation-only comparison patterns", reviewer_text, fixed = TRUE)]
  expect_false(any(grepl("value_mismatches|rows_aligned|missing_differences|\"compared\"|\"mismatches\"", focused)))
  for (request in fx$state$fixer_llm$requests()) {
    text <- request_task_text(request)
    expect_false(grepl("CONSTANT_OFFSET|REFERENCE_PATH_SENTINEL|7000[1-5]|value_mismatches|rows_aligned", text))
  }
  # Full candidate acceptance requests are separate from focused investigation.
  acceptance <- reviewer_text[!grepl("Investigation-only comparison patterns", reviewer_text, fixed = TRUE)]
  expect_gt(length(acceptance), 0)
  expect_false(any(grepl("CONSTANT_OFFSET|REFERENCE_PATH_SENTINEL|7000[1-5]|value_mismatches|rows_aligned", acceptance)))
})

test_that("report advice reuses current findings and failed extra diagnosis never retries", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.result = discrepancy_fixture()))
  state$histories$p01 <- record_completed_review(state$histories$p01,
    verdict = "repair_required", findings = list(list(severity = "material", affected_outputs = "work.result",
      sas_evidence = "value = value + 1", r_evidence = "value + 9")))
  result <- migration_report_diagnosis(state)
  expect_identical(result$origin, "existing_source_review")
  expect_length(result$explanations, 1)
  expect_length(state$reviewer_llm$requests(), 0)
  state$histories <- list()
  calls <- 0L
  state$reviewer_llm <- new_llm(function(request, audit_context = list()) {
    calls <<- calls + 1L
    stop("synthetic provider unavailable")
  }, provider = "mock")
  before <- state$status
  result <- migration_report_diagnosis(state)
  expect_identical(result$status, "unavailable")
  expect_identical(calls, 1L)
  expect_identical(state$status, before)
  expect_error(normalize_migration_config(list(report_diagnosis = "always")), class = "sas2r_config_error")
  expect_identical(normalize_migration_config(yaml::yaml.load("report_diagnosis: off"))$report_diagnosis, "off")
})

test_that("readable comparison summaries survive serialization and retain type differences", {
  target <- discrepancy_fixture()
  roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(target, auto_unbox = TRUE, null = "null"), simplifyVector = FALSE)
  expect_identical(migration_hash(reviewer_discrepancy_summary(target)),
    migration_hash(reviewer_discrepancy_summary(roundtrip)))
  target$differences$structure$kind_mismatch <- data.frame(var = "eventdt", base_kind = "numeric", comp_kind = "date")
  expect_match(discrepancy_description(dataset_discrepancy_summary(target)), "eventdt (date vs numeric)", fixed = TRUE)
  target <- assess_dataset_target(list(target_id = "synthetic", target_key = "work.no_ref"),
    data.frame(x = numeric(), flag = character()))
  summary <- dataset_discrepancy_summary(target)
  expect_equal(summary$generated, list(rows = 0, columns = 2))
  expect_identical(discrepancy_table(list(summary))$`Reference dimension`, "Unavailable")
})


test_that("report-only controls do not invalidate source translations or reviews", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  before <- migration_resume_fingerprint(fx$state)
  fx$state$config$migration$report_diagnosis <- "off"
  fx$state$config$raw$migration$report_diagnosis <- "off"
  expect_identical(migration_resume_fingerprint(fx$state), before)
})
