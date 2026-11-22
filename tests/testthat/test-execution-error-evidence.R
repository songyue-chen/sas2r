test_that("invalid argument diagnostics retain the failing parameter without values", {
  snippets <- c(
    width = 'print(data.frame(x = 1), width = Inf)',
    digits = 'print(data.frame(x = 1), digits = NA_integer_)',
    max = 'print(data.frame(x = 1), max = NA_integer_)',
    times = 'rep(1, times = -1)'
  )
  for (argument in names(snippets)) {
    code <- snippets[[argument]]
    error <- tryCatch(suppressWarnings(eval(parse(text = code))), error = identity)
    expect_s3_class(error, "error")
    for (condition in list(error, execution_condition(error))) {
      facts <- agent_error_facts(condition, code)
      expect_identical(facts$kind, "invalid_argument", info = code)
      expect_identical(facts$identifiers, argument, info = code)
      expect_match(facts$condition_message, argument, fixed = TRUE)
      if (argument == "width") expect_identical(facts$condition_message, "invalid printing width")
    }
  }
  # Argument values can originate in data; only the code-supported name is sent.
  facts <- agent_error_facts(list(message = "invalid value PRIVATE_RECORD for 'digits' argument"),
    'print(x, digits = setting)')
  expect_identical(facts$kind, "invalid_argument")
  expect_false(grepl("PRIVATE_RECORD", facts$condition_message, fixed = TRUE))
})

test_that("missing-variable messages require a matching source call", {
  message <- "date_parts: variable STARTDTC is not in the data"
  condition <- execution_condition(simpleError(message))
  code <- "out <- date_parts('startdtc', prefix = 'START')(data)"
  facts <- agent_error_facts(condition, code)
  expect_identical(facts$kind, "column_not_found")
  expect_identical(facts$identifiers, "STARTDTC")
  expect_identical(facts$called_function, "date_parts")
  expect_identical(facts$condition_message, message)

  for (other in c("date_parts('another'); startdtc <- NULL",
    "another('startdtc'); date_parts <- NULL", "# date_parts('startdtc')")) {
    facts <- agent_error_facts(condition, other)
    expect_identical(facts$kind, "unclassified_error")
    expect_null(facts$called_function)
  }
  facts <- agent_error_facts(simpleError(paste(message, "PRIVATE_RECORD")), code)
  expect_identical(facts$kind, "unclassified_error")
  expect_false(grepl("PRIVATE_RECORD", facts$condition_message, fixed = TRUE))
})

test_that("a real printing failure reaches smoke and bundle fixer prompts", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  code <- paste(fx$fixed$p01, 'print(x, width = Inf)', sep = "\n")
  state <- stage_workflow_revision(state, "p01", code, "reviewed_no_material_finding")
  revision <- state$selected_revisions$p01
  plan <- build_program_smoke_plan(state$graph, "p01", state$selected_revisions, execute = TRUE)
  prep <- prepare_program_smoke(state, plan, withr::local_tempdir())
  smoke <- run_program_smoke(prep$plan, prep$runtime, prep$attempt_dir)
  bundle <- run_bundle_attempt(state)
  bundle$bundle_id <- bundle$attempt_id
  for (mode in c("program", "bundle")) {
    execution <- if (mode == "program") smoke else bundle
    expect_false(execution$passed)
    expect_match(execution$condition$message, "invalid printing width", fixed = TRUE)
    # Exercise persisted records, as used when resuming a saved run.
    execution <- jsonlite::fromJSON(jsonlite::toJSON(unclass(execution), auto_unbox = TRUE), simplifyVector = FALSE)
    fixer <- recording_fixer(function(req) valid_program_fix_response(
      code = fx$fixed$p01, evidence_ids = req$evidence_ids))
    args <- list(revision = revision, mode = mode, llm = fixer,
      project = state$project, config = state$config, paths = state$paths)
    args[[if (mode == "program") "smoke" else "bundle"]] <- execution
    fixed <- do.call(fix_program_revision, args)
    expect_identical(fixed$status, "ok")
    prompt <- request_task_text(fixer$requests()[[1L]])
    expect_match(prompt, '"condition_message": "invalid printing width"', fixed = TRUE)
    expect_match(prompt, '"condition_kind": "invalid_argument"', fixed = TRUE)
    expect_false(grepl("Execution failed; inspect the local diagnostics", prompt, fixed = TRUE))
    expect_length(bounded_agent_diagnostics(execution, source_code = code)$log_excerpt, 0L)
  }
})

test_that("row-size failures are recognized however the assignment is written", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  df <- data.frame(id = c("01", "02"))
  cases <- list(
    list('df$flag <- numeric(0)', "zero_length_value", character()),
    list('df$flag <- c(1, 2, 3)', "differing_row_counts", character()),
    list('data.frame(a = 1:2, flag = numeric(0))', "differing_row_counts", character()),
    list('dplyr::mutate(df, flag = numeric(0))', "zero_length_value", "flag"),
    list('dplyr::mutate(df[1, , drop = FALSE], flag = numeric(0))', "zero_length_value", "flag"),
    list('dplyr::mutate(df, flag = c(1, 2, 3))', "differing_row_counts", "flag"),
    # An empty frame is the data's size, not the value's: no zero-length claim.
    list('dplyr::mutate(df[0, , drop = FALSE], flag = c(1, 2))', "differing_row_counts", "flag"),
    list('tibble::add_column(df, flag = numeric(0))', "zero_length_value", character()),
    list('tibble::tibble(a = 1:2, flag = numeric(0))', "zero_length_value", "flag"),
    list('vctrs::vec_recycle(numeric(3), 2)', "differing_row_counts", character()),
    list('dplyr::bind_cols(df, data.frame(flag = numeric(3)))', "differing_row_counts", character()))
  for (case in cases) {
    code <- case[[1L]]
    error <- tryCatch(eval(parse(text = code)), error = identity)
    expect_s3_class(error, "error")
    saved <- jsonlite::fromJSON(jsonlite::toJSON(execution_condition(error), auto_unbox = TRUE,
                                                 null = "null"), simplifyVector = FALSE)
    for (condition in list(error, execution_condition(error), saved)) {
      facts <- agent_error_facts(condition, code)
      expect_identical(facts$kind, case[[2L]], info = code)
      expect_identical(facts$identifiers, case[[3L]], info = code)
      # Row sizes can come from data, so no number leaves the classifier.
      expect_false(grepl("[0-9]", facts$condition_message), info = code)
    }
  }
})

test_that("a styled saved message with a generic class is classified like a plain one", {
  styled <- paste0("\033[1m\033[22m\033[36mi\033[39m In argument: `flag = to_flag(NULL)`.\n",
                   "\033[1mCaused by error:\033[22m\n\033[33m!\033[39m `flag` must be size 2 or 1, not 0.")
  saved <- list(message = styled, class = c("rlang_error", "error", "condition"))
  facts <- agent_error_facts(saved, "out <- dplyr::mutate(out, flag = to_flag(NULL))")
  expect_identical(facts$kind, "zero_length_value")
  expect_identical(facts$identifiers, "flag")
  expect_false(grepl("[0-9]|must be size", facts$condition_message))

  # The name is sent only when the selected code assigns it. A mention
  # elsewhere in the code, or no code at all, leaves the kind without a name.
  for (other in c("out <- dplyr::mutate(out, other = flag)", "flag <- 1", "")) {
    facts <- agent_error_facts(saved, other)
    expect_identical(facts$kind, "zero_length_value", info = other)
    expect_length(facts$identifiers, 0L)
  }

  # Styling is removed before every pattern, not only this one.
  plain <- agent_error_facts(list(message = "\033[33m!\033[39m object 'zz' not found",
                                  class = c("rlang_error", "error", "condition")), "zz + 1")
  expect_identical(plain$kind, "object_not_found")
  expect_identical(plain$identifiers, "zz")

  unrelated <- agent_error_facts(list(message = "\033[33m!\033[39m something else went wrong",
                                      class = c("rlang_error", "error", "condition")), "x <- 1")
  expect_identical(unrelated$kind, "unclassified_error")
})

test_that("a zero-length assignment reaches fixer prompts and the triage request by kind and name", {
  skip_if_not_installed("dplyr")
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  code <- paste(fx$fixed$p01, "x <- dplyr::mutate(x, flag = numeric(0))", sep = "\n")
  state <- stage_workflow_revision(state, "p01", code, "reviewed_no_material_finding")
  revision <- state$selected_revisions$p01
  plan <- build_program_smoke_plan(state$graph, "p01", state$selected_revisions, execute = TRUE)
  prep <- prepare_program_smoke(state, plan, withr::local_tempdir())
  smoke <- run_program_smoke(prep$plan, prep$runtime, prep$attempt_dir)
  bundle <- run_bundle_attempt(state)
  bundle$bundle_id <- bundle$attempt_id
  leaks <- function(text) grepl("must be size|size [0-9]|not [0-9]", text)
  for (mode in c("program", "bundle")) {
    execution <- if (mode == "program") smoke else bundle
    expect_false(execution$passed)
    expect_match(execution$condition$message, "must be size", fixed = TRUE)
    # Exercise persisted records, as used when resuming a saved run.
    execution <- jsonlite::fromJSON(jsonlite::toJSON(unclass(execution), auto_unbox = TRUE), simplifyVector = FALSE)
    fixer <- recording_fixer(function(req) valid_program_fix_response(
      code = fx$fixed$p01, evidence_ids = req$evidence_ids))
    args <- list(revision = revision, mode = mode, llm = fixer,
      project = state$project, config = state$config, paths = state$paths)
    args[[if (mode == "program") "smoke" else "bundle"]] <- execution
    fixed <- do.call(fix_program_revision, args)
    expect_identical(fixed$status, "ok")
    prompt <- request_task_text(fixer$requests()[[1L]])
    expect_match(prompt, '"condition_kind": "zero_length_value"', fixed = TRUE, info = mode)
    expect_match(prompt, ": flag\"", fixed = TRUE, info = mode)
    expect_false(leaks(prompt), info = mode)
  }

  state$config$migration$bug_reports <- TRUE
  state$attempt <- list(kind = "bundle", completed = TRUE, attempt_id = "attempt-1",
    condition = c(list(component_id = "p01"), bundle$condition),
    revision_manifest = list(p01 = list(revision_id = revision$revision_id,
      r_hash = migration_hash(code))))
  request <- NULL
  llm <- diagnosis_mock(list(findings = list()), function(req) request <<- req)
  diagnosis <- suppressMessages(migration_package_bug_diagnosis(state, llm))
  expect_identical(diagnosis$status, "completed")
  task <- request_task_text(request)
  expect_match(task, '"kind":"zero_length_value"', fixed = TRUE)
  expect_match(task, '"identifiers":"flag"', fixed = TRUE)
  expect_false(leaks(task))
})
