test_that("translation and macro indexing leave source projects unchanged", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  writeLines("%macro helper(); %mend;", file.path(root, "macros", "helper.sas"))
  writeLines("data work.out; x = 1; run;", file.path(root, "p.sas"))
  config <- list(macro_search_path = "macros")
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  withr::local_options(sas2r.progress = FALSE)
  result <- suppressMessages(sas_translate(root, config = config, execute = FALSE,
    usage_limits = list(max_calls = 0)))
  on.exit(unlink(result$out_dir, recursive = TRUE), add = TRUE)
  project <- result$project
  first <- project_macro_index(project, project$config)
  second <- project_macro_index(project, project$config)
  expect_identical(first, second)
  expect_true("helper" %in% first$name)
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE), before)
  expect_false(dir.exists(file.path(root, ".sas2r")))
  expect_true(startsWith(normalizePath(result$out_dir), normalizePath(tempdir())))
  expect_identical(result$usage$request_count, 0L)
})

test_that("quiet translation keeps status and reports without console output", {
  root <- withr::local_tempdir()
  writeLines("data work.out; x = 1; run;", file.path(root, "p.sas"))
  withr::local_options(sas2r.progress = TRUE)
  workspace_names <- ls(envir = globalenv(), all.names = TRUE)
  messages <- capture.output(
    output <- capture.output(result <- suppressMessages(sas_translate(root,
      out_dir = withr::local_tempdir(), execute = FALSE))), type = "message")
  expect_length(output, 0L)
  expect_length(messages, 0L)
  expect_identical(result$status, "blocked")
  expect_true(file.exists(result$report_json_path))
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages[["Bundle execution"]], "NOT RUN", fixed = TRUE)
  expect_identical(ls(envir = globalenv(), all.names = TRUE), workspace_names)
})


test_that("standalone audit logging does not write into the working directory", {
  working <- withr::local_tempdir()
  withr::local_dir(working)
  entry <- list(event = "cran_default_log_location")
  llm_log(entry)
  log <- file.path(tempdir(), "llm_log.jsonl")
  expect_true(file.exists(log))
  expect_identical(jsonlite::fromJSON(tail(readLines(log), 1L))$event, entry$event)
  expect_length(list.files(working, all.files = TRUE, no.. = TRUE), 0L)

  prompts <- withr::local_tempdir()
  expect_error(write_llm_lock(list(), prompts), "argument.*path.*missing")
  expect_false(file.exists(file.path(working, "_sas2r.lock")))
})

test_that("runtime library changes stay in the local registry", {
  workspace_names <- ls(envir = globalenv(), all.names = TRUE)
  .sas2r_registry <- list()
  sas2r_libname_assign("work", withr::local_tempdir(), engine = "rds")
  expect_identical(names(.sas2r_registry), "work")
  lib_write(data.frame(value = 1), "work", "result")
  expect_equal(lib_read("work", "result")$value, 1)
  sas2r_libname_clear("work")
  expect_length(.sas2r_registry, 0L)
  expect_identical(ls(envir = globalenv(), all.names = TRUE), workspace_names)
})

test_that("quiet execution preserves failure logs and leaves the workspace unchanged", {
  root <- withr::local_tempdir()
  writeLines("data work.result; value = 1; run;", file.path(root, "main.sas"))
  state <- new_migration_state(sas_project(root), withr::local_tempdir())
  code <- 'fail_local <- function() stop("execution diagnostic"); fail_local()'
  state$selected_revisions <- list(main = list(component_id = "main",
    r_code = code, staged_file = "main.R",
    contract = list(component_id = "main", staged_file = "main.R")))
  plan <- build_program_smoke_plan(state$graph, "main", state$selected_revisions)
  prepared <- prepare_program_smoke(state, plan, state$attempt$attempt_dir)
  workspace_names <- ls(envir = globalenv(), all.names = TRUE)
  withr::local_options(sas2r.progress = TRUE)
  expect_silent(smoke <- suppressMessages(with_sas2r_progress(
    run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir))))
  expect_false(smoke$passed)
  expect_identical(smoke$condition$message, "execution diagnostic")
  expect_true("fail_local" %in% smoke$condition$call_names)
  expect_match(paste(readLines(smoke$stderr_path), collapse = "\n"),
               "Error: execution diagnostic", fixed = TRUE)
  expect_silent(bundle <- suppressMessages(with_sas2r_progress(run_bundle_attempt(state))))
  expect_false(bundle$passed)
  expect_true("fail_local" %in% bundle$condition$call_names)
  expect_match(paste(readLines(file.path(bundle$logs_dir, "bundle_stderr.log")), collapse = "\n"),
               "execution diagnostic", fixed = TRUE)
  old_dir <- getwd()
  expect_error(source(prepared$plan$replay_script, local = new.env()), "execution diagnostic")
  expect_identical(getwd(), old_dir)
  expect_identical(ls(envir = globalenv(), all.names = TRUE), workspace_names)
})
