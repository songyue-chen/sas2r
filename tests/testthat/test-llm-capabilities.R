test_that("unknown and unsupported optional parameters are omitted", {
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0,
    reasoning_effort = "high",
    max_output_tokens = 512L
  )

  unknown <- llm_capabilities(
    temperature = "unknown",
    reasoning_effort = "unsupported",
    max_output_tokens = "supported"
  )
  effective <- effective_model_params(request, unknown)

  expect_null(effective$temperature)
  expect_null(effective$reasoning_effort)
  expect_identical(effective$max_output_tokens, 512L)
  expect_null(effective$top_p)
})

test_that("supported parameters are still omitted unless explicitly requested", {
  capabilities <- llm_capabilities(
    temperature = "supported",
    reasoning_effort = "supported",
    max_output_tokens = "supported",
    top_p = "supported"
  )

  omitted <- effective_model_params(
    llm_request(messages = list(list(role = "user", content = "ping"))),
    capabilities
  )
  expect_length(omitted, 0L)

  explicit <- effective_model_params(
    llm_request(
      messages = list(list(role = "user", content = "ping")),
      temperature = 0,
      top_p = 0.9
    ),
    capabilities
  )
  expect_identical(explicit$temperature, 0)
  expect_identical(explicit$top_p, 0.9)
})

test_that("capability resolution caches exact deployment identity without leaking overrides", {
  base_a <- resolve_model_capabilities(
    provider = "azure", endpoint = "https://a.example", model = "deployment",
    api_version = "2026-01-01"
  )
  overridden <- resolve_model_capabilities(
    provider = "azure", endpoint = "https://a.example", model = "deployment",
    api_version = "2026-01-01",
    overrides = list(temperature = "supported")
  )
  base_b <- resolve_model_capabilities(
    provider = "azure", endpoint = "https://b.example", model = "deployment",
    api_version = "2026-01-01"
  )

  expect_identical(base_a$temperature, "unknown")
  expect_identical(overridden$temperature, "supported")
  expect_identical(
    resolve_model_capabilities(
      provider = "azure", endpoint = "https://a.example", model = "deployment",
      api_version = "2026-01-01"
    )$temperature,
    "unknown"
  )
  expect_false(identical(base_a$cache_key, base_b$cache_key))
  expect_false(identical(base_a$record_hash, base_b$record_hash))
})

test_that("untested real-provider model capabilities remain unknown", {
  model <- paste0("untested-", basename(tempfile()))
  capabilities <- resolve_model_capabilities(
    provider = "openai", endpoint = "https://api.example.invalid/v1",
    model = model, api_version = "2026-08-20"
  )

  expect_identical(capabilities$structured_output, "unknown")
  expect_identical(capabilities$tool_calling, "unknown")
  expect_identical(capabilities$tools_with_structured_output, "unknown")
})

test_that("only DeepSeek carries a provider-wide structured-output default", {
  model <- paste0("untested-", basename(tempfile()))
  for (provider in setdiff(llm_provider_ids(), "deepseek")) {
    capabilities <- resolve_model_capabilities(
      provider = provider, endpoint = NULL, model = model
    )
    expect_identical(capabilities$structured_output, "unknown", info = provider)
    expect_identical(capabilities$tool_calling, "unknown", info = provider)
  }
  deepseek <- resolve_model_capabilities(
    provider = "deepseek", endpoint = NULL, model = model
  )
  expect_identical(deepseek$structured_output, "fallback")
  expect_identical(deepseek$source, "adapter_metadata")
  expect_identical(deepseek$tool_calling, "unknown")
  expect_identical(deepseek$tools_with_structured_output, "unknown")
  for (parameter in LLM_OPTIONAL_PARAMETERS) {
    expect_identical(deepseek[[parameter]], "unknown", info = parameter)
  }
  # An explicit override still wins over the adapter default.
  overridden <- resolve_model_capabilities(
    provider = "deepseek", endpoint = NULL, model = model,
    overrides = list(structured_output = "native")
  )
  expect_identical(overridden$structured_output, "native")
  expect_identical(overridden$source, "explicit_override")
})

test_that("a classified optional-parameter failure downgrades and retries once", {
  calls <- list()
  transport <- function(request, params) {
    calls[[length(calls) + 1L]] <<- params
    if (length(calls) == 1L) {
      stop(llm_optional_parameter_error("temperature"))
    }
    list(type = "final", data = list(ok = TRUE))
  }
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0
  )
  capabilities <- llm_capabilities(temperature = "supported")

  response <- invoke_with_capability_retry(request, capabilities, transport)

  expect_length(calls, 2L)
  expect_identical(calls[[1]]$temperature, 0)
  expect_null(calls[[2]]$temperature)
  expect_identical(response$status, "completed")
  expect_identical(response$retry_count, 1L)
  expect_identical(response$downgraded_parameters, "temperature")
  expect_null(response$effective_parameters$temperature)
})

test_that("authentication, schema, and arbitrary request failures never downgrade", {
  cases <- list(
    structure(list(message = "unauthorized", status_code = 401L),
              class = c("sas2r_llm_authentication_error", "error", "condition")),
    structure(list(message = "invalid response schema", status_code = 400L),
              class = c("sas2r_llm_invalid_schema", "error", "condition")),
    structure(list(message = "bad request", status_code = 400L),
              class = c("sas2r_llm_transport_error", "error", "condition"))
  )

  for (error in cases) {
    calls <- 0L
    response <- invoke_with_capability_retry(
      llm_request(
        messages = list(list(role = "user", content = "ping")),
        temperature = 0
      ),
      llm_capabilities(temperature = "supported"),
      function(request, params) {
        calls <<- calls + 1L
        stop(error)
      }
    )
    expect_identical(calls, 1L)
    expect_identical(response$status, "failed")
    expect_identical(response$retry_count, 0L)
  }
})

test_that("real-style optional parameter rejection is sticky and hashes the downgrade", {
  model <- paste0("retry-", basename(tempfile()))
  endpoint <- "https://retry.example.invalid"
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0
  )
  capabilities <- resolve_model_capabilities(
    "openai", endpoint, model, "v1",
    overrides = list(temperature = "supported")
  )
  calls <- list()

  response <- invoke_with_capability_retry(
    request, capabilities,
    function(request, params) {
      calls[[length(calls) + 1L]] <<- params
      if (length(calls) == 1L) {
        error <- structure(
          list(
            message = "Unsupported parameter: temperature",
            call = NULL,
            response = structure(list(status_code = 400L),
                                 class = "httr2_response")
          ),
          class = c("httr2_http_400", "error", "condition")
        )
        stop(error)
      }
      list(type = "final", data = list(ok = TRUE))
    }
  )

  expect_length(calls, 2L)
  expect_identical(calls[[1]]$temperature, 0)
  expect_null(calls[[2]]$temperature)
  expect_identical(response$retry_count, 1L)
  expect_false(identical(response$capability_hash, capabilities$record_hash))
  expect_identical(
    resolve_model_capabilities(
      "openai", endpoint, model, "v1",
      overrides = list(temperature = "supported")
    )$temperature,
    "unsupported"
  )
})

test_that("ellmer max_tokens rejection downgrades max_output_tokens", {
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    max_output_tokens = 512L
  )
  capabilities <- llm_capabilities(max_output_tokens = "supported")
  calls <- list()

  response <- invoke_with_capability_retry(
    request, capabilities,
    function(request, params) {
      calls[[length(calls) + 1L]] <<- params
      if (length(calls) == 1L) {
        error <- structure(
          list(
            message = "Unsupported parameter: max_tokens",
            call = NULL, status_code = 400L
          ),
          class = c("httr2_http_400", "error", "condition")
        )
        stop(error)
      }
      list(type = "final", data = list(ok = TRUE))
    }
  )

  expect_length(calls, 2L)
  expect_identical(
    lapply(calls, function(params) params$max_output_tokens),
    list(512L, NULL)
  )
  expect_identical(response$downgraded_parameters, "max_output_tokens")
})

test_that("parameters withheld by the capability gate are reported, not dropped silently", {
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0,
    reasoning_effort = "high",
    max_output_tokens = 512L
  )
  capabilities <- llm_capabilities(
    temperature = "unknown",
    reasoning_effort = "unsupported",
    max_output_tokens = "supported"
  )

  expect_setequal(
    withheld_model_params(request, capabilities),
    c("temperature", "reasoning_effort")
  )
})

test_that("a parameter that was never requested is not reported as withheld", {
  request <- llm_request(messages = list(list(role = "user", content = "ping")))

  expect_identical(
    withheld_model_params(request, llm_capabilities(temperature = "unknown")),
    character()
  )
})

test_that("withheld parameters travel on the response, distinct from downgrades", {
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0
  )
  capabilities <- llm_capabilities(temperature = "unknown")
  transport <- function(request, params, ...) {
    list(text = "ok", usage = list(input_tokens = 1L, output_tokens = 1L))
  }

  response <- invoke_with_capability_retry(request, capabilities, transport)

  # `downgraded_parameters` means a runtime rejection forced a retry; a
  # capability-gated omission is a different event and must not borrow it.
  expect_identical(response$withheld_parameters, "temperature")
  expect_identical(response$downgraded_parameters, character())
})

test_that("configured model parameters are carried onto the llm object", {
  # `llm: reasoning_effort:` was an accepted config key that nothing read: the
  # value was validated, then silently discarded before the runner saw it.
  llm <- ellmer_llm(list(
    provider = "deepseek", model = "deepseek-v4-flash", auth_mode = "api_key",
    api_key = "test-key-not-used", reasoning_effort = "high", temperature = 0.5
  ))

  expect_identical(llm$model_parameters$reasoning_effort, "high")
  expect_identical(llm$model_parameters$temperature, 0.5)
})

test_that("an agent spec value wins over the configured default", {
  # The shipped translator sets temperature 0 for determinism; a project-level
  # default must not silently undo a deliberate per-agent choice.
  spec <- list(temperature = 0, reasoning_effort = "low")
  llm <- list(model_parameters = list(temperature = 0.9, reasoning_effort = "high"))

  expect_identical(resolve_model_parameter(spec, llm, "temperature"), 0)
  expect_identical(resolve_model_parameter(spec, llm, "reasoning_effort"), "low")
})

test_that("the configured default applies when the spec sets nothing", {
  spec <- list(temperature = 0)
  llm <- list(model_parameters = list(reasoning_effort = "high"))

  expect_identical(resolve_model_parameter(spec, llm, "reasoning_effort"), "high")
})

test_that("a parameter set in neither place stays absent", {
  expect_null(resolve_model_parameter(list(), list(), "reasoning_effort"))
  expect_null(resolve_model_parameter(list(), list(model_parameters = list()),
                                      "reasoning_effort"))
})

test_that("a configured reasoning_effort is still withheld while the capability is unknown", {
  # Carrying the value through must not bypass the capability gate: declaring
  # `capabilities: reasoning_effort: supported` stays the thing that sends it.
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    reasoning_effort = "high"
  )

  unknown <- llm_capabilities(reasoning_effort = "unknown")
  expect_null(effective_model_params(request, unknown)$reasoning_effort)
  expect_identical(withheld_model_params(request, unknown), "reasoning_effort")

  supported <- llm_capabilities(reasoning_effort = "supported")
  expect_identical(effective_model_params(request, supported)$reasoning_effort, "high")
  expect_identical(withheld_model_params(request, supported), character())
})

test_that("transport constraints only downgrade known capabilities", {
  configured <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported",
    source = "explicit_override", record_hash = "configured"
  )

  effective <- apply_transport_constraints(configured, list(
    tools_with_structured_output = "unsupported"
  ))

  expect_identical(effective$structured_output, "native")
  expect_identical(effective$tool_calling, "native")
  expect_identical(effective$tools_with_structured_output, "unsupported")
  expect_match(effective$source, "explicit_override", fixed = TRUE)
  expect_match(effective$source, "transport_constraint", fixed = TRUE)
  expect_false(identical(effective$record_hash, configured$record_hash))
  expect_identical(
    apply_transport_constraints(effective, list(
      tools_with_structured_output = "unsupported"
    )),
    effective
  )
})

test_that("transport constraints reject promotions and unknown fields", {
  expect_error(
    normalize_transport_constraints(list(
      tools_with_structured_output = "supported"
    )),
    class = "sas2r_llm_capability_error"
  )
  expect_error(
    normalize_transport_constraints(list(provider_feature = "unsupported")),
    class = "sas2r_llm_capability_error"
  )
  expect_error(
    normalize_transport_constraints(stats::setNames(
      list("unsupported"), ""
    )),
    class = "sas2r_llm_capability_error"
  )
})

test_that("transport constraints apply after per-tier capability resolution", {
  supported <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", source = "explicit_override"
  )
  llm <- new_llm(
    identity, provider = "custom", capabilities = supported,
    capabilities_for = function(tier, model = NULL) supported,
    transport_constraints = list(
      tools_with_structured_output = "unsupported"
    )
  )

  expect_identical(llm$capabilities$tools_with_structured_output, "unsupported")
  expect_identical(
    llm_capabilities_for(llm, tier = "cheap")$tools_with_structured_output,
    "unsupported"
  )
})

test_that("ellmer responses report the effective transport-constrained hash", {
  model <- paste0("capability-provenance-", basename(tempfile()))
  cfg <- list(
    provider = "openai", model = model, auth_mode = "api_key",
    api_key = "offline-test-key", base_url = "https://example.invalid/v1",
    capabilities = list(
      temperature = "supported", structured_output = "native",
      tool_calling = "native", tools_with_structured_output = "supported"
    )
  )
  testthat::local_mocked_bindings(
    ellmer_transport_request = function(cfg, request, model, params) {
      list(type = "final", data = list(ok = TRUE))
    },
    .package = "sas2r"
  )

  llm <- ellmer_llm(cfg)
  raw <- llm$capabilities_for(tier = "frontier", model = NULL)
  effective <- llm_capabilities_for(llm, tier = "frontier", model = NULL)
  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0
  ))

  expect_false(identical(raw$record_hash, effective$record_hash))
  expect_identical(response$capability_hash, effective$record_hash)
})

test_that("ellmer retry hashes preserve transport and runtime provenance", {
  model <- paste0("capability-retry-provenance-", basename(tempfile()))
  cfg <- list(
    provider = "openai", model = model, auth_mode = "api_key",
    api_key = "offline-test-key", base_url = "https://example.invalid/v1",
    capabilities = list(
      temperature = "supported", structured_output = "native",
      tool_calling = "native", tools_with_structured_output = "supported"
    )
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    ellmer_transport_request = function(cfg, request, model, params) {
      calls <<- calls + 1L
      if (identical(calls, 1L)) stop(llm_optional_parameter_error("temperature"))
      list(type = "final", data = list(ok = TRUE))
    },
    .package = "sas2r"
  )

  llm <- ellmer_llm(cfg)
  effective <- llm_capabilities_for(llm, tier = "frontier", model = NULL)
  expected_retry <- effective
  expected_retry$temperature <- "unsupported"
  expected_retry$source <- paste(
    c(effective$source, "runtime_rejection"), collapse = "+"
  )
  retry_payload <- unclass(expected_retry)
  retry_payload$record_hash <- NULL
  expected_retry_hash <- as.character(cli::hash_sha256(jsonlite::toJSON(
    retry_payload, auto_unbox = TRUE, null = "null", na = "null"
  )))
  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "ping")),
    temperature = 0
  ))

  expect_identical(calls, 2L)
  expect_match(expected_retry$source, "transport_constraint", fixed = TRUE)
  expect_match(expected_retry$source, "runtime_rejection", fixed = TRUE)
  expect_identical(response$capability_hash, expected_retry_hash)
})
