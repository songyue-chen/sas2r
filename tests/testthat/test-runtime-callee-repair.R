runtime_callee_fixture <- function(caller_fault = FALSE, nested = FALSE, source_abort = FALSE,
                                   envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  dir.create(file.path(root, "macros"))
  writeLines(if (nested) '%wrapper(label=Sensor reading);' else '%empty_frame(label=Sensor reading);',
    file.path(root, "main.sas"))
  if (nested) writeLines("%macro wrapper(label=); %empty_frame(label=&label); %mend;",
    file.path(root, "macros", "wrapper.sas"))
  writeLines(c("%macro empty_frame(label=);", "data work.empty_frame;",
    'attrib measure label="&label" length=8;', "if 0;", "run;", "%mend;"),
    file.path(root, "macros", "empty_frame.sas"))
  if (source_abort) writeLines("%macro empty_frame(label=); %abort cancel; %mend;",
    file.path(root, "macros", "empty_frame.sas"))
  project <- sas_project(file.path(root, "main.sas"),
    config = list(macro_search_path = file.path(root, "macros")))
  state <- new_migration_state(project, file.path(root, "migration"))
  correct <- paste(c('empty_frame <- function(label = "") {',
    "  x <- data.frame(measure = numeric())",
    '  attr(x$measure, "label") <- label',
    '  lib_write(x, "work", "empty_frame")', "}"), collapse = "\n")
  faulty <- sub("  x <-", paste0(
    '  if (grepl("[\\\\r\\\\n]", label)) stop(paste0("record", "_value_", 77123))\n',
    "  x <-"), correct, fixed = TRUE)
  fixed <- list(macro__empty_frame = correct, main = 'empty_frame(label = "Sensor reading")')
  if (source_abort) fixed$macro__empty_frame <- 'empty_frame <- function(label = "") stop("Source-required abort")'
  if (nested) {
    fixed$macro__wrapper <- 'wrapper <- function(label = "") empty_frame(label = label)'
    fixed$main <- 'wrapper(label = "Sensor reading")'
  }
  code <- fixed
  if (caller_fault) code$main <- 'empty_frame(label = "Sensor reading", extra = 1)' else
    if (!source_abort) code$macro__empty_frame <- faulty
  for (cid in names(code)) {
    path <- file.path(root, paste0(cid, ".R"))
    writeLines(code[[cid]], path)
    sas <- component_source_text(state$graph, cid)
    binding <- new_component_binding(migration_hash(sas), migration_hash(code[[cid]]),
      migration_hash("helpers"), migration_hash("review"), migration_hash("closure"))
    macro <- component_macro_contract(project, state$graph, cid)
    staged <- if (isTRUE(macro$standalone)) paste0("R/macros/", macro$name, ".R") else paste0(cid, ".R")
    state$selected_revisions[[cid]] <- list(component_id = cid, revision_id = "r1",
      r_code = code[[cid]], r_path = path, staged_file = staged, binding = binding,
      contract = list(component_id = cid, staged_file = staged,
        sas_text = sas, binding = binding, macro_contract = macro))
    state$histories[[cid]] <- record_completed_review(new_component_evidence_history(cid, binding))
  }
  state$output_contracts <- infer_output_contracts(project, overrides = list(datasets = "work.empty_frame"))
  state$reviewer_llm <- recording_reviewer(function(req) {
    text <- request_task_text(req)
    if (req$component_id == "macro__empty_frame" && grepl('stop(paste0("record", "_value_", 77123))', text, fixed = TRUE))
      return(material_review_response(sas_evidence = 'attrib measure label="&label"; if 0;',
        r_evidence = 'grepl rejects ordinary labels with r or n before creating the empty dataset',
        affected_outputs = "work.empty_frame"))
    valid_program_review_response()
  })
  state$fixer_llm <- recording_fixer(function(req) valid_program_fix_response(fixed[[req$component_id]]))
  list(state = state, fixed = fixed, root = root)
}

test_that("a real callee failure repairs the source-defective macro and preserves its caller", {
  fx <- runtime_callee_fixture()
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_identical(vapply(result$repairs, "[[", "", "component_id"), "macro__empty_frame")
  expect_identical(result$selected_revisions$main$r_code, fx$fixed$main)
  expect_identical(result$selected_revisions$macro__empty_frame$r_code, fx$fixed$macro__empty_frame)
  expect_true(result$attempt$passed)
  expect_equal(nrow(result$attempts), 2L)
  first <- result$diagnostics$bundle_repair$attempts$bundle_attempt_001
  expect_identical(first$failures$main$message, "record_value_77123")
  expect_identical(first$failures$main$component_id, "main")
  expect_identical(first$callee_reviews$macro__empty_frame$caller, "main")
  expect_identical(first$callee_reviews$macro__empty_frame$verdict, "repair_required")
  expect_length(fx$state$fixer_llm$requests(), 1L)
  requests <- c(fx$state$reviewer_llm$requests(), fx$state$fixer_llm$requests())
  expect_false(any(grepl("record_value_77123", vapply(requests, request_task_text, ""), fixed = TRUE)))
  output <- readRDS(file.path(result$attempt$attempt_dir, "work", "empty_frame.rds"))
  expect_equal(nrow(output), 0L)
  expect_identical(attr(output$measure, "label"), "Sensor reading")
})

test_that("nested runtime calls can identify the inner macro without rewriting its callers", {
  fx <- runtime_callee_fixture(nested = TRUE)
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_identical(vapply(result$repairs, "[[", "", "component_id"), "macro__empty_frame")
  expect_identical(result$selected_revisions$macro__wrapper$r_code, fx$fixed$macro__wrapper)
  expect_identical(result$selected_revisions$main$r_code, fx$fixed$main)
  expect_true(result$attempt$passed)
})

test_that("source-required macro failure remains blocked rather than being removed", {
  fx <- runtime_callee_fixture(source_abort = TRUE)
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_length(result$repairs, 0L)
  expect_false(result$attempt$passed)
  expect_identical(result$selected_revisions$macro__empty_frame$r_code, fx$fixed$macro__empty_frame)
  expect_identical(result$status, "blocked")
  expect_identical(result$diagnostics$bundle_repair$attempts$bundle_attempt_001$
    callee_reviews$macro__empty_frame$verdict, "reviewed_no_material_finding")
})

test_that("a function call does not blame a faithful macro for an incorrect caller", {
  fx <- runtime_callee_fixture(caller_fault = TRUE)
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_identical(vapply(result$repairs, "[[", "", "component_id"), "main")
  expect_identical(result$selected_revisions$macro__empty_frame$r_code, fx$fixed$macro__empty_frame)
  expect_true(result$attempt$passed)
  first <- result$diagnostics$bundle_repair$attempts$bundle_attempt_001
  expect_identical(first$callee_reviews$macro__empty_frame$verdict, "reviewed_no_material_finding")
  expect_length(fx$state$fixer_llm$requests(), 1L)
})

test_that("callee investigation reuses source review and respects budget and unavailable calls", {
  fx <- runtime_callee_fixture()
  state <- fx$state
  attempt <- run_bundle_attempt(state, sequence = 1L)
  diagnostic <- collect_bundle_diagnostics(state, attempt)
  first <- review_bundle_callees(state, attempt, diagnostic, 0L)
  second <- review_bundle_callees(first$state, attempt, diagnostic, 0L)
  expect_true(second$diagnostic$callee_reviews$macro__empty_frame$reused)
  expect_length(state$reviewer_llm$requests(), 1L)
  first$state$usage_budget$max_calls <- 0L
  expect_length(review_bundle_callees(first$state, attempt, diagnostic, 0L)$diagnostic$callee_reviews, 0L)
  expect_null(runtime_callee_component(state, "main", list()))
  # Missing and dynamic call forms have no uniquely established callee.
  for (code in c("stop('failure')", "missing()", "get('empty_frame')(label)", "base::stop('failure')")) {
    condition <- execution_condition(tryCatch(eval(parse(text = code)), error = identity))
    expect_null(runtime_callee_component(state, "main", condition))
  }
})

test_that("regex bracket notices are advisory and respect the selected engine", {
  pattern <- "[\\r\\n]"
  expect_true(grepl(pattern, "Sensor reading"))
  expect_false(grepl("[\r\n]", "Sensor reading"))
  expect_false(grepl(pattern, "Sensor reading", perl = TRUE))
  code <- function(fun, pattern, args = "") paste0(fun, "(", deparse(pattern), ', "text"', args, ")")
  for (fun in c("grep", "grepl", "base::grepl", "regexpr", "gregexpr", "regexec", "gregexec")) {
    lint <- lint_r_code(code(fun, pattern))
    expect_true("regex_bracket_escape" %in% lint$kind)
    expect_identical(lint$level[lint$kind == "regex_bracket_escape"], "warn")
  }
  for (text in c(code("grepl", pattern, ", perl = TRUE"), code("grepl", pattern, ", fixed = TRUE"),
      code("grepl", "[\r\n]"), code("grepl", "[rn]"), code("grepl", "\\r\\n"),
      code("other::grepl", pattern), 'grepl(pattern, "text")', code("grepl", pattern, ", perl = mode"))) {
    expect_false("regex_bracket_escape" %in% lint_r_code(text)$kind, info = text)
  }
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(code("grepl", pattern), path)
  checked <- check_program_revision(path)
  expect_true(checked$pass)
  expect_true(any(grepl("regex_bracket_escape", checked$warnings, fixed = TRUE)))
})
