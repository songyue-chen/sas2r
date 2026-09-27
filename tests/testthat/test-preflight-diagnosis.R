test_that("preflight reports missing LLM setup and explicit offline mode makes no calls", {
  root <- diagnosis_fixture()
  testthat::local_mocked_bindings(sas_llm = function(...) stop("must stay offline"))
  absent <- sas_preflight(root)
  expect_identical(absent$diagnosis$status, "not_configured")
  expect_message(print(absent), "no LLM configured")
  offline <- sas_preflight(root, diagnose = "off", config = list(
    llm = list(provider = "openai", model = "unused")))
  expect_identical(offline$diagnosis$status, "disabled")
  expect_equal(offline$model_calls, 0L)
  expect_identical(offline$inputs, absent$inputs)
  ready_root <- diagnosis_fixture(code = "data out; set raw.input; run;")
  saveRDS(data.frame(x = 1), file.path(ready_root, "input.rds"))
  ready <- sas_preflight(ready_root, config = list(libraries = list(raw = ready_root)),
    llm = diagnosis_mock(callback = function(request) stop("no actionable finding")))
  expect_identical(ready$diagnosis$status, "not_needed")
  expect_equal(ready$model_calls, 0L)
})

test_that("one advisory request preserves static results and offers a bug report draft", {
  root <- diagnosis_fixture()
  offline <- sas_preflight(root, diagnose = "off")
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  requests <- list()
  llm <- diagnosis_mock(diagnosis_answer("suspected_sas2r_bug"), function(request) {
    requests[[length(requests) + 1L]] <<- request
  })
  check <- sas_preflight(root, llm = llm)
  expect_identical(check$diagnosis$status, "completed")
  expect_equal(check$model_calls, 1L)
  expect_length(requests, 1L)
  expect_lte(requests[[1L]]$parameters$max_output_tokens, 4096L)
  expect_match(requests[[1L]]$messages[[2L]]$content, "main.sas")
  expect_identical(check$inputs, offline$inputs)
  expect_identical(check$findings, offline$findings)
  expect_identical(check$status, offline$status)
  expect_identical(check$diagnosis$model, "diagnosis-test")
  expect_match(check$diagnosis$issue_url, "sas2r/issues/new$")
  expect_match(check$diagnosis$advisory$issue_body, "Reproduce")
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE), before)
  expect_message(print(check), "1 model calls")
})

test_that("diagnosis respects budget limits and retains invalid or failed advice separately", {
  root <- diagnosis_fixture()
  calls <- 0L
  llm <- diagnosis_mock(callback = function(request) calls <<- calls + 1L)
  blocked <- sas_preflight(root, llm = llm, usage_limits = list(max_calls = 0))
  expect_identical(blocked$diagnosis$status, "unavailable")
  expect_equal(blocked$model_calls, 0L)
  expect_equal(calls, 0L)
  too_large <- sas_preflight(root, llm = llm, usage_limits = list(max_request_bytes = 1))
  expect_equal(too_large$model_calls, 0L)
  expect_equal(calls, 0L)
  invalid <- sas_preflight(root, llm = diagnosis_mock(list(summary = "not a complete answer")))
  expect_identical(invalid$diagnosis$status, "unavailable")
  expect_match(invalid$diagnosis$reason, "Invalid diagnosis")
  expect_null(invalid$diagnosis$advisory)
  failed <- sas_preflight(root, llm = new_llm(function(...) stop("service unavailable"),
    provider = "mock", model = "diagnosis-test"))
  expect_identical(failed$status, "needs_attention")
  expect_identical(failed$diagnosis$status, "unavailable")
  expect_equal(failed$model_calls, 1L)
  expect_match(failed$diagnosis$reason, "service unavailable")
})

test_that("configuration diagnosis uses one transport attempt and preserves original errors", {
  root <- diagnosis_fixture()
  settings <- NULL
  testthat::local_mocked_bindings(sas_llm = function(config) {
    settings <<- config
    diagnosis_mock()
  })
  cfg <- list(llm = list(provider = "openai", model = "unused", max_tries = 5,
    timeout_seconds = 300), migration = list(execution_order = "missing.sas"))
  error <- tryCatch(suppressMessages(sas_preflight(root, config = cfg)), error = identity)
  expect_s3_class(error, "sas2r_config_error")
  expect_match(conditionMessage(error), "execution_order")
  expect_identical(error$diagnosis$status, "completed")
  expect_equal(settings$max_tries, 1L)
  expect_lte(settings$timeout_seconds, 120)
  expect_equal(error$diagnosis$model_calls, 1L)
  invalid <- tryCatch(suppressMessages(sas_preflight(root,
    config = list(llm = list(provider = "invalid-provider")))), error = identity)
  expect_s3_class(invalid, "sas2r_llm_config_error")
  expect_identical(invalid$diagnosis$status, "unavailable")
  expect_match(invalid$diagnosis$reason, "configuration could be resolved")
})

test_that("diagnosis context is bounded and actual configured secrets are redacted", {
  root <- diagnosis_fixture(code = c("%let note=actual-test-secret;",
    "data out; set work.absent; run;", rep("%let long=abcdefghij;", 2500)))
  sent <- NULL
  llm <- new_llm(function(request, ...) {
    sent <<- request$messages[[2L]]$content
    answer <- diagnosis_answer()
    answer$summary <- "Never show actual-test-secret."
    new_llm_response(status = "completed", action = "final", data = answer,
      request = request, provider = "mock")
  }, provider = "mock", model = "diagnosis-test", redaction_secrets = "actual-test-secret")
  check <- sas_preflight(root, llm = llm)
  expect_identical(check$diagnosis$status, "completed")
  expect_true(check$diagnosis$context_truncated)
  expect_lt(nchar(sent), 31000L)
  expect_false(grepl("actual-test-secret", sent, fixed = TRUE))
  expect_false(grepl("actual-test-secret", check$diagnosis$advisory$summary, fixed = TRUE))
  expect_match(sent, "main.sas")
})
