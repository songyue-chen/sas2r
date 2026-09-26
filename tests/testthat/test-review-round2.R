test_that("agent policy stays in system instructions and source stays readable", {
  captured <- NULL
  llm <- new_llm(function(request, audit_context = list()) {
    captured <<- request$messages
    response <- valid_program_review_response()
    attr(response, "cost_usd") <- 0
    normalize_provider_response(response, request, "mock")
  }, provider = "mock", capabilities = llm_capabilities(structured_output = "native", tool_calling = "native"))
  spec <- as.list(load_agent_specs()$reviewer)
  spec$prompt <- withr::local_tempfile(fileext = ".md")
  writeLines("{{phase}} {{style}} {{skills}} {{unit}} {{context}}", spec$prompt)
  run_agent(spec, llm, list(), "Review the source", log_dir = withr::local_tempdir(),
    prompt_vars = list(style = "PACKAGE_STYLE", phase = "PACKAGE_PHASE", skills = "PACKAGE_SKILL", unit = "data a;\nx='{{context}}';\nrun;", staged_r = "x <- 1", context = "SOURCE_CONTEXT"))
  expect_match(captured[[1]]$content, "PACKAGE_STYLE", fixed = TRUE)
  expect_match(captured[[1]]$content, "PACKAGE_PHASE", fixed = TRUE)
  expect_match(captured[[1]]$content, "PACKAGE_SKILL", fixed = TRUE)
  expect_false(grepl("SOURCE_CONTEXT", captured[[1]]$content, fixed = TRUE))
  expect_match(captured[[2]]$content, "data a;\nx='{{context}}';\nrun;", fixed = TRUE)
  expect_match(captured[[2]]$content, "BEGIN SAS2R_TASK_", fixed = TRUE)
})

test_that("configured libraries return between programs and in exported runs", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  fx$state$selected_revisions$p01$r_code <- paste(fx$fixed$p01, 'sas2r_libname_clear("_all_")', sep = "\n")
  attempt <- run_bundle_attempt(fx$state)
  expect_true(attempt$passed, info = attempt$condition$message)
  expect_equal(readRDS(file.path(attempt$attempt_dir, "work", "out2.rds"))$value, 12:14)
  dest <- withr::local_tempdir()
  materialize_user_bundle(file.path(attempt$attempt_dir, "bundle"), dest, fx$state$project)
  child <- callr::r(function() { source("run.R"); TRUE }, wd = dest)
  expect_true(child)
  expect_equal(readRDS(file.path(dest, "output", "work", "out2.rds"))$value, 12:14)
})

test_that("existing flat files require review while absent required files block", {
  root <- withr::local_tempdir()
  source <- file.path(root, "export.sas")
  writeLines('proc export data=raw.input outfile="class.csv" dbms=csv replace; run;', source)
  p <- sas_project(source)
  contracts <- p$output_contracts
  expect_identical(contracts$kind, "file")
  attempt <- list(attempt_dir = root, completed = TRUE, passed = TRUE, exit_status = 0L)
  missing <- assess_final_outputs(contracts, attempt)
  expect_identical(missing$status, "blocked")
  expect_identical(missing$targets[[1]]$status, "missing_candidate")
  writeLines("id\n1", file.path(root, "class.csv"))
  present <- assess_final_outputs(contracts, attempt)
  expect_identical(present$status, "needs_review")
  expect_identical(present$targets[[1]]$status, "unassessed_file")
  expect_false(present$targets[[1]]$passed)
  optional <- infer_output_contracts(p, list(optional = "class.csv"))
  expect_false(optional$required)
  expect_error(validate_output_overrides(list(optional = FALSE)), "optional")
})

test_that("execution startup ignores local credentials and profiles on all platforms", {
  root <- withr::local_tempdir()
  startup <- withr::local_tempfile()
  file.create(startup)
  writeLines('OPENAI_API_KEY=synthetic-round2-key', file.path(root, ".Renviron"))
  writeLines('options(sas2r.round2.profile = TRUE)', file.path(root, ".Rprofile"))
  withr::local_envvar(OPENAI_API_KEY = NA_character_, R_USER = root)
  inspect <- function() list(key = Sys.getenv("OPENAI_API_KEY"), profile = getOption("sas2r.round2.profile", FALSE))
  # The positive control proves the fixture would load without our isolation.
  control <- callr::r(inspect, wd = root, user_profile = TRUE,
    env = c(R_ENVIRON_USER = file.path(root, ".Renviron"), R_PROFILE_USER = file.path(root, ".Rprofile")))
  expect_identical(control$key, "synthetic-round2-key")
  expect_true(control$profile)
  isolated <- callr::r(inspect, wd = root, env = execution_process_env(startup), user_profile = FALSE, system_profile = FALSE)
  expect_identical(isolated$key, "")
  expect_false(isolated$profile)
})

test_that("open-code macro branches are deferred by the public translation path", {
  root <- withr::local_tempdir()
  writeLines(c('%if 1 %then %do;', 'data work.out; x=1; run;', '%end;',
    '%else %do;', 'data work.out; x=2; run;', '%end;'), file.path(root, "p.sas"))
  result <- suppressWarnings(sas_translate(root, execute = FALSE, usage_limits = list(max_calls = 0)))
  expect_true(all(result$project$statements$macro_control))
  code <- sas_code(result)
  expect_match(code, "open_code_macro_control", fixed = TRUE)
  expect_false(grepl("lib_write\\(", code))
  expect_identical(result$status, "blocked")
})

test_that("generated files nested under an input root are not source mutations", {
  skip_if_not_installed("dplyr")
  root <- withr::local_tempdir()
  saveRDS(data.frame(id = 1, value = 2), file.path(root, "input.rds"))
  writeLines('data work.out; set raw.input; value=value+1; run;', file.path(root, "p.sas"))
  cfg <- list(libraries = list(raw = list(path = root, engine = "rds")))
  result <- sas_translate(root, out_dir = file.path(root, "out"), config = cfg, outputs = "work.out", usage_limits = list(max_calls = 0))
  expect_false(identical(result$status, "blocked"), info = result$status_reason)
  expect_false(any(grepl("out/", names(input_hash_manifest(result$project)), fixed = TRUE)))
  expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "out.rds"))$value, 3)
})

test_that("character truth converts numeric text and rejects nonnumeric values", {
  values <- c("Y", "N", "0", "1", "ABC", " -2 ", "", NA_character_)
  expect_identical(sas_true(values), c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  helper <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), helper)
  expect_identical(helper$sas_true(values), sas_true(values))
})

test_that("wrapped network failures retry without retrying schema defects", {
  connection <- structure(list(message = "Failed to connect: Could not connect to server", call = NULL),
    class = c("httr2_failure", "error", "condition"))
  expect_identical(failure_class(connection), "sas2r_llm_transport_error")
  expect_identical(failure_class(simpleError("Unexpected schema")), "sas2r_llm_error")
})

test_that("re-export preserves unrelated files and refuses new collisions", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  x <- structure(list(bundle_dir = fx$state$paths$staging, project = fx$state$project,
    status = "migration_ready"), class = "sas2r_translation")
  dir <- withr::local_tempdir()
  sas_write(x, dir)
  dir.create(file.path(dir, ".git"))
  writeLines("ref: refs/heads/main", file.path(dir, ".git", "HEAD"))
  writeLines("retain me", file.path(dir, "NOTES.md"))
  sas_write(x, dir, overwrite = TRUE)
  expect_identical(readLines(file.path(dir, "NOTES.md")), "retain me")
  expect_true(file.exists(file.path(dir, ".git", "HEAD")))
  writeLines("new generated file", file.path(x$bundle_dir, "NOTES.md"))
  # In a flat bundle, add a colliding script instead of a support document.
  writeLines("manual <- TRUE", file.path(dir, "programs", "new.R"))
  writeLines("generated <- TRUE", file.path(x$bundle_dir, "new.R"))
  expect_error(sas_write(x, dir, overwrite = TRUE), "conflicts", class = "sas2r_export_exists")
  expect_identical(readLines(file.path(dir, "programs", "new.R")), "manual <- TRUE")
})

test_that("source content identity still detects equal-metadata changes", {
  root <- withr::local_tempdir()
  path <- file.path(root, "input.txt")
  writeLines("AAAA", path)
  # Use a timestamp Windows can restore exactly, without subsecond rounding.
  mtime <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(path, mtime)
  p <- list(libraries = list(raw = root))
  full <- input_hash_manifest(p)
  metadata <- input_hash_manifest(p, metadata_only = TRUE)
  writeLines("BBBB", path)
  Sys.setFileTime(path, mtime)
  expect_identical(input_hash_manifest(p, metadata_only = TRUE), metadata)
  expect_false(identical(input_hash_manifest(p), full))
})

test_that("missing and boolean adapters no longer claim known divergence", {
  registry <- load_semantic_registry()
  for (id in c("functions.missing", "operators.and", "operators.or", "operators.not")) {
    expect_identical(registry$rules[[id]]$classification, "adapter_required")
    expect_identical(registry$rules[[id]]$risk, "low")
  }
  expect_length(semantic_coverage(load_rulebook())$strategy_mismatches, 0L)
})

test_that("abbreviated include is resolved through the public scanner", {
  root <- withr::local_tempdir()
  writeLines('data work.a; x=1; run;', file.path(root, "included.sas"))
  source <- file.path(root, "main.sas")
  writeLines('%inc "included.sas";', source)
  p <- sas_project(source)
  expect_true(any(basename(p$statements$file) == "included.sas"))
  expect_identical(extract_includes(sas_statements('%inc "included.sas";'))$target, "included.sas")
})

test_that("documented dollar caps opt into catalog cost estimates explicitly", {
  root <- withr::local_tempdir()
  writeLines('data work.a; x=1; run;', file.path(root, "p.sas"))
  expect_identical(sas_preflight(root, budget_usd = 10, budget_mode = "soft")$budget$mode, "soft")
  expect_error(sas_preflight(root, budget_usd = 10), class = "sas2r_budget_config_error")
})

test_that("existing flat-file review never creates a content repair request", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  saveRDS(data.frame(id = 1:2), file.path(root, "data", "cls.rds"))
  writeLines('proc export data=raw.cls outfile="out/class.csv" dbms=csv replace; run;', file.path(root, "p.sas"))
  calls <- character()
  llm <- new_llm(function(request, audit_context = list()) {
    role <- audit_context$agent %||% audit_context$purpose
    calls <<- c(calls, role)
    response <- if (identical(role, "translator")) valid_program_translation_response(
      'x <- lib_read("raw", "cls")\ndir.create("out", showWarnings = FALSE)\nutils::write.csv(x, "out/class.csv", row.names = FALSE)') else valid_program_review_response()
    attr(response, "cost_usd") <- 0
    normalize_provider_response(response, request, "mock")
  }, provider = "mock", capabilities = llm_capabilities(structured_output = "native", tool_calling = "native", tools_with_structured_output = "supported"))
  result <- sas_translate(root, llm = llm, config = list(libraries = list(raw = list(path = file.path(root, "data"), engine = "rds"))))
  expect_identical(result$status, "needs_review")
  expect_true(startsWith(result$status_reason, "unassessed_file: out/class.csv"))
  expect_identical(result$diagnostics$stop_reason, "no_causal_evidence")
  expect_false(any(calls == "fixer"))
  report <- jsonlite::fromJSON(result$report_json_path, simplifyVector = FALSE)
  expect_match(report$outcome$reason, "Review file contents", fixed = TRUE)
})

test_that("bundle timeout records the effective setting without claiming a defect", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  fx$state$selected_revisions$p01$r_code <- "Sys.sleep(5)"
  attempt <- run_bundle_attempt(fx$state, timeout = 0.2)
  expect_false(attempt$passed)
  expect_match(attempt$condition$message, "migration.bundle_timeout = 0.2 seconds", fixed = TRUE)
  expect_match(non_translation_runtime_reason(fx$state, "p01", attempt$condition), "no translation defect established", fixed = TRUE)
})

test_that("legacy ellmer request limits fail before any provider request", {
  local_mocked_bindings(ellmer_has_request_callbacks = function() FALSE)
  calls <- 0L
  llm <- new_llm(function(...) { calls <<- calls + 1L; stop("provider must not be called") }, provider = "mock")
  attr(llm, "is_ellmer") <- TRUE
  root <- withr::local_tempdir()
  writeLines('data work.a; x=1; run;', file.path(root, "p.sas"))
  error <- tryCatch(sas_translate(root, llm = llm, usage_limits = list(max_calls = 2)), error = identity)
  expect_s3_class(error, "sas2r_budget_unmeterable")
  expect_identical(calls, 0L)
})

test_that("RPC publication never exposes a partially copied response", {
  root <- withr::local_tempdir()
  target <- file.path(root, "message.reply")
  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  expect_error(atomic_write_file(function(path) saveRDS(list(ok = TRUE), path), target, require_atomic = TRUE),
    class = "sas2r_write_failed")
  expect_false(file.exists(target))
})

test_that("runtime I/O helpers cannot be replaced by generated programs", {
  for (helper in c("lib_write", "lib_read", "sas2r_lib_member_file", "sas2r_registry_env")) {
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(paste0(helper, " <- function(...) NULL"), path)
    expect_false(check_program_revision(path)$pass, info = helper)
  }
})


test_that("an output root cannot hide the entire source library from input checks", {
  root <- withr::local_tempdir()
  saveRDS(data.frame(id = 1), file.path(root, "input.rds"))
  project <- list(libraries = list(raw = root), input_manifest_exclude = normalizePath(root, winslash = "/"))
  expect_error(input_hash_manifest(project), "choose a separate output directory", class = "sas2r_config_error")
})
