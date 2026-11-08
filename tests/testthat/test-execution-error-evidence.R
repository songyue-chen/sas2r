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
