settings_test_adapter <- function(reply = NULL, capabilities = NULL) {
  seen <- new.env(parent = emptyenv())
  seen$requests <- list()
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(request, llm_request_capabilities(llm, request),
      function(request, params) {
        seen$requests[[length(seen$requests) + 1L]] <- list(request = request, params = params)
        if (is.function(reply)) return(reply(request, params))
        if (identical(params$reasoning_effort, "sas2r-invalid-effort")) {
          stop(structure(list(message = "Invalid value for reasoning_effort", call = NULL,
                              status_code = 400L), class = c("error", "condition")))
        }
        list(type = "final", data = if (request$phase == "probe") list(ok = TRUE) else list(
          r_code = "x <- 1", summary = "test", parameters = list(), defaults = list(),
          reads = character(), writes = character(), side_effects = character(),
          helper_use = character(), discovered_dependencies = character(),
          suspected_dependencies = character(), affected_outputs = character(), uncertainty = list()
        ))
      })
  }, provider = "mock", model = "settings-test", endpoint = "http://local.test",
  capabilities = capabilities %||% llm_capabilities(
    structured_output = "native", tool_calling = "native"
  ), model_parameters = list(reasoning_effort = "high", max_output_tokens = 32768L))
  llm <- with_usage_managed_request(llm)
  list(llm = llm, seen = seen)
}

test_that("unknown reasoning is verified and forwarded to every agent phase", {
  test <- settings_test_adapter()
  llm <- test$llm
  budget <- new_usage_budget(max_calls = 8, max_output_tokens = 32768)
  dir <- withr::local_tempdir()
  spec <- list(name = "translator", prompt = "translator.md", temperature = 0,
               output_schema = "program_translation_v1", tool_call_limit = 3L,
               retry_limit = 0L, tier = "frontier")
  for (role in c("translator", "reviewer", "fixer")) {
    spec$name <- role
    result <- run_agent(spec, llm, list(), "unit", log_dir = dir, usage_budget = budget)
    expect_identical(result$status, "ok")
  }
  requests <- test$seen$requests
  expect_length(requests, 5L) # invalid control, positive ping, three agent requests
  expect_identical(budget$request_count, 5L)
  expect_identical(requests[[1]]$params$reasoning_effort, "sas2r-invalid-effort")
  for (item in requests[-1L]) {
    expect_identical(item$params$reasoning_effort, "high")
    expect_identical(item$params$max_output_tokens, 32768L)
    expect_null(item$params$temperature) # shipped optional default stays optional
  }
  expect_identical(llm$capabilities$reasoning_effort, "unknown")
  expect_length(ls(llm$verified_settings), 1L)
})

test_that("a settings check is scoped to exact requested values and deployment", {
  test <- settings_test_adapter()
  llm <- test$llm
  budget <- new_usage_budget()
  dir <- withr::local_tempdir()
  high <- llm$model_parameters
  ensure_llm_settings(llm, high, "frontier", dir, budget)
  ensure_llm_settings(llm, high, "frontier", dir, budget)
  expect_length(test$seen$requests, 2L)
  low <- high
  low$reasoning_effort <- "low"
  ensure_llm_settings(llm, low, "frontier", dir, budget)
  expect_length(test$seen$requests, 4L)
  caps <- llm$capabilities
  key <- llm_settings_key(llm, caps, high)
  expect_false(identical(
    llm_settings_key(llm, caps, list(top_p = 0.123456)),
    llm_settings_key(llm, caps, list(top_p = 0.123457))
  ))
  for (field in c("endpoint", "api_version", "model")) {
    other <- llm
    other[[field]] <- "different"
    expect_false(identical(key, llm_settings_key(other, caps, high)))
  }
  caps$model <- "another-tier-model"
  expect_false(identical(key, llm_settings_key(llm, caps, high)))
  caps <- llm$capabilities
  caps$reasoning_effort <- "unsupported"
  caps <- rehash_capabilities(caps)
  expect_false(identical(key, llm_settings_key(llm, caps, high)))
})

test_that("an endpoint ignoring reasoning stops translation before any agent call", {
  test <- settings_test_adapter(function(request, params) list(type = "final", data = list(ok = TRUE)))
  path <- withr::local_tempfile(fileext = ".sas")
  writeLines("data work.example; x=1; run;", path)
  out <- withr::local_tempdir()
  expect_error(sas_translate(path, out_dir = out, llm = test$llm, execute = FALSE),
               "support remains unknown", class = "sas2r_llm_settings_error")
  expect_length(test$seen$requests, 1L)
  expect_identical(test$seen$requests[[1]]$request$phase, "probe")
  expect_length(ls(test$llm$verified_settings), 0L)
  ledger <- read_usage_records(file.path(migration_paths(out)$state, "usage.jsonl"))
  expect_true(any(vapply(ledger, function(row) {
    identical(row$record_type, "run_summary") && identical(row$terminal_status, "failed")
  }, logical(1))))
})

test_that("declared unsupported settings stop without transport", {
  test <- settings_test_adapter(capabilities = llm_capabilities(
    structured_output = "native", reasoning_effort = "unsupported"
  ))
  expect_error(ensure_llm_settings(test$llm, test$llm$model_parameters, "frontier",
    withr::local_tempdir(), new_usage_budget()), "declared unsupported",
    class = "sas2r_llm_settings_error")
  expect_length(test$seen$requests, 0L)
})

test_that("required settings are never removed on a provider rejection", {
  calls <- 0L
  request <- llm_request(list(list(role = "user", content = "unit")), reasoning_effort = "high")
  request$required_parameters <- "reasoning_effort"
  result <- invoke_with_capability_retry(request, llm_capabilities(reasoning_effort = "supported"),
    function(request, params) {
      calls <<- calls + 1L
      stop(llm_optional_parameter_error("reasoning_effort"))
    })
  expect_identical(calls, 1L)
  expect_identical(result$retry_count, 0L)
  expect_identical(result$error$class, "sas2r_llm_settings_error")
  expect_error(assert_required_settings(request, result), class = "sas2r_llm_settings_error")
})

test_that("a rejected real level is not cached after the negative control", {
  test <- settings_test_adapter(function(request, params) {
    stop(structure(list(message = "Invalid value for reasoning_effort", call = NULL,
                        status_code = 422L), class = c("error", "condition")))
  })
  expect_error(ensure_llm_settings(test$llm, test$llm$model_parameters, "frontier",
    withr::local_tempdir(), new_usage_budget()), "Invalid value", class = "sas2r_llm_settings_error")
  expect_length(test$seen$requests, 2L)
  expect_identical(test$seen$requests[[2]]$params$reasoning_effort, "high")
  expect_length(ls(test$llm$verified_settings), 0L)
})

test_that("a successful custom transport must report forwarding required settings", {
  llm <- new_llm(function(request) {
    normalize_provider_response(list(type = "final", data = list(ok = TRUE)), request)
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native", reasoning_effort = "supported"
  ), model_parameters = list(reasoning_effort = "high"))
  expect_error(ensure_llm_settings(llm, llm$model_parameters, "frontier",
    withr::local_tempdir(), new_usage_budget()), "not forwarded",
    class = "sas2r_llm_settings_error")
  expect_length(ls(llm$verified_settings), 0L)
})

test_that("authentication failure leaves support unknown without repeated calls", {
  test <- settings_test_adapter(function(request, params) {
    stop(structure(list(message = "authentication rejected", call = NULL),
      class = c("sas2r_llm_authentication_error", "error", "condition")))
  })
  expect_error(ensure_llm_settings(test$llm, test$llm$model_parameters, "frontier",
    withr::local_tempdir(), new_usage_budget()), class = "sas2r_llm_authentication_error")
  expect_length(test$seen$requests, 1L)
  expect_length(ls(test$llm$verified_settings), 0L)
})

test_that("connector warnings about required settings cannot pass a probe", {
  request <- llm_request(list(list(role = "user", content = "ping")), reasoning_effort = "high")
  request$required_parameters <- "reasoning_effort"
  expect_error(with_required_ellmer_settings(request, {
    warning("Ignoring unsupported parameters: reasoning_effort")
    TRUE
  }), "cannot forward", class = "sas2r_llm_settings_error")
  request$required_parameters <- "max_output_tokens"
  expect_error(with_required_ellmer_settings(request, warning("Ignoring unsupported parameters: max_tokens")),
               class = "sas2r_llm_settings_error")
})

test_that("timeouts, truncation and exhausted budgets never populate the cache", {
  timeout <- settings_test_adapter(function(request, params) {
    stop(structure(list(message = "timed out", call = NULL),
                   class = c("sas2r_llm_timeout", "error", "condition")))
  })
  budget <- new_usage_budget(max_calls = 2)
  expect_error(ensure_llm_settings(timeout$llm, timeout$llm$model_parameters, "frontier",
    withr::local_tempdir(), budget), class = "sas2r_llm_timeout")
  expect_identical(budget$request_count, 2L)
  expect_length(ls(timeout$llm$verified_settings), 0L)
  truncated <- settings_test_adapter(function(request, params) {
    list(type = "final", data = list(ok = TRUE), finish_reason = "length")
  }, llm_capabilities(structured_output = "native", reasoning_effort = "supported"))
  expect_error(ensure_llm_settings(truncated$llm, truncated$llm$model_parameters, "frontier",
    withr::local_tempdir(), new_usage_budget()), "could not be verified")
  expect_length(ls(truncated$llm$verified_settings), 0L)
  limited <- settings_test_adapter()
  expect_error(ensure_llm_settings(limited$llm, limited$llm$model_parameters, "frontier",
    withr::local_tempdir(), new_usage_budget(max_calls = 1)), "could not be verified")
  expect_length(limited$seen$requests, 1L)
  expect_length(ls(limited$llm$verified_settings), 0L)
  zero <- settings_test_adapter()
  ensure_llm_settings(zero$llm, zero$llm$model_parameters, "frontier",
                       withr::local_tempdir(), new_usage_budget(max_calls = 0))
  expect_length(zero$seen$requests, 0L)
})
