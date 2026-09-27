test_that("unknown effects do not hide absent or available external inputs", {
  effects <- c("%unknown;", "proc datasets library=work kill; quit;",
    "proc sql; create table a as select * from work.seed; select * from a; quit;",
    "data _null_; call execute('data tmp; x=1; run;'); run;",
    "%if 0 %then %do; data tmp; x=1; run; %end;")
  for (effect in effects) {
    fx <- ordered_fixture(list("a.sas" = c(effect, "data out; set raw.dm; run;")))
    fx$config$libraries <- list(raw = fx$root)
    p <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(p$inputs$status[p$inputs$dataset == "raw.dm"], "missing")
    expect_true(any(vapply(p$readiness$warnings, function(w)
      w$kind == "input_missing" && w$blocks_execution, logical(1))))
    saveRDS(data.frame(x = 1), file.path(fx$root, "dm.rds"))
    p <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(p$inputs$status[p$inputs$dataset == "raw.dm"], "available")
  }
})

test_that("simple setup macros preserve known data versions but dynamic macros do not", {
  fx <- ordered_fixture(list(
    "z.sas" = c("%setup_study(study=XYZ);", "data tmp; set raw.dm; run;"),
    "a.sas" = c("%setup_study(study=ABC);", "data out; set tmp; run;")))
  dir.create(file.path(fx$root, "macros"))
  macro <- file.path(fx$root, "macros", "setup_study.sas")
  writeLines(c("%macro setup_study(study=);", "%global studyid;",
    "%let studyid=&study;", "%mend setup_study;"), macro)
  fx$config$macro_search_path <- dirname(macro)
  fx$config$libraries <- list(raw = fx$root)
  saveRDS(data.frame(x = 1), file.path(fx$root, "dm.rds"))
  p <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_identical(p$inputs$status, c("available", "generated"))
  expect_false(any(dataset_producers(p$project)$deferred))
  # Resolved definitions are insufficient when the body can emit runtime text.
  for (body in c("&study", "%let code=&outside; &code", "%sysfunc(dosubl(&study))",
                 "data tmp; x=2; run;", "* Note: %other;")) {
    writeLines(c("%macro setup_study(study=);", body, "%mend;"), macro)
    p <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(p$inputs$status[p$inputs$dataset == "work.tmp"], "deferred")
    expect_identical(p$inputs$status[p$inputs$dataset == "raw.dm"], "available")
  }
})

test_that("ordered replays retain session context with a timeout allowance per root", {
  fx <- ordered_fixture(stats::setNames(rep(list("data out; set raw.dm; run;"), 8L),
    paste0("p", seq_len(8L), ".sas")))
  fx$config$libraries <- list(raw = fx$root)
  saveRDS(data.frame(x = 1), file.path(fx$root, "dm.rds"))
  p <- sas_preflight(fx$root, config = fx$config, diagnose = "off")$project
  revisions <- stats::setNames(rep(list("invisible(NULL)"), 8L), paste0("p", seq_len(8L)))
  plan <- build_program_smoke_plan(p$graph, "p8", revisions)
  expect_identical(plan$dependency_prefix, paste0("p", seq_len(7L)))
  expect_equal(plan$replay_program_count, 8L)
  captured <- NULL
  testthat::local_mocked_bindings(r = function(func, args, timeout, ...) {
    captured <<- timeout
    list(success = TRUE, executed_component_ids = names(revisions))
  }, .package = "callr")
  result <- run_program_smoke(plan, list(), withr::local_tempdir(), timeout = 7)
  expect_equal(captured, 56)
  expect_true(result$passed)
})

test_that("order path errors are concise and retain complete structured paths", {
  fx <- ordered_fixture(stats::setNames(rep(list("data out; x=1; run;"), 8L),
    paste0("p", seq_len(8L), ".sas")), order = paste0("wrong/p", seq_len(8L), ".sas"))
  error <- tryCatch(suppressMessages(sas_preflight(fx$root, config = fx$config,
    diagnose = "off")), error = identity)
  expect_s3_class(error, "sas2r_config_error")
  expect_length(error$missing_roots, 8L)
  expect_length(error$unknown_roots, 8L)
  expect_match(conditionMessage(error), "and 3 more")
  expect_match(conditionMessage(error), "relative to the YAML")
  expect_false(grepl("p8.sas", conditionMessage(error), fixed = TRUE))
})

test_that("diagnosis announces source use and preserves configured generation allowance", {
  llm <- diagnosis_mock()
  llm$model_parameters <- list(reasoning_effort = "high", max_output_tokens = 65536L)
  request <- NULL
  llm$request <- function(payload, ...) {
    request <<- payload
    new_llm_response(status = "incomplete", action = "none", request = payload,
      provider = "mock", finish_reason = "max_output_tokens")
  }
  expect_message(check <- sas_preflight(diagnosis_fixture(), llm = llm),
    "bounded SAS statements, source paths and findings")
  expect_equal(request$parameters$max_output_tokens, 65536L)
  expect_identical(request$parameters$reasoning_effort, "high")
  expect_equal(check$diagnosis$max_output_tokens, 65536L)
  expect_identical(check$diagnosis$finish_reason, "max_output_tokens")
  expect_identical(check$diagnosis$response_status, "incomplete")
  expect_identical(check$diagnosis$status, "unavailable")
  expect_equal(check$model_calls, 1L)
  bug <- sas_preflight(diagnosis_fixture(), llm = diagnosis_mock(diagnosis_answer("suspected_sas2r_bug")))
  expect_match(paste(preflight_diagnosis_lines(bug$diagnosis), collapse = "\n"), "tracker is public")
  expect_match(paste(preflight_diagnosis_lines(bug$diagnosis), collapse = "\n"), "synthetic example")
})
