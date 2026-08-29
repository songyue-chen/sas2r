role_fixture <- function() {
  list(
    list(role = "system", content = "policy"),
    list(role = "user", content = "question"),
    list(role = "assistant", content = "calling a tool"),
    list(role = "tool", tool_call_id = "call_1", content = "result")
  )
}

test_that("llm requests preserve system, user, assistant, and tool roles", {
  request <- llm_request(messages = role_fixture(), tier = "frontier")

  expect_s3_class(request, "sas2r_llm_request")
  expect_identical(
    vapply(request$messages, `[[`, "", "role"),
    c("system", "user", "assistant", "tool")
  )
  expect_identical(request$messages[[4]]$tool_call_id, "call_1")
})

test_that("reasoning items before an OpenAI final message normalize correctly", {
  raw <- list(
    id = "resp_123",
    status = "completed",
    model = "resolved-model",
    output = list(
      list(type = "reasoning", summary = list()),
      list(
        type = "message", role = "assistant", status = "completed",
        content = list(list(type = "output_json", json = list(ok = TRUE)))
      )
    ),
    usage = list(
      input_tokens = 100L, output_tokens = 25L, total_tokens = 125L,
      input_tokens_details = list(
        cached_tokens = 20L, cache_write_tokens = 30L
      ),
      output_tokens_details = list(reasoning_tokens = 10L)
    )
  )

  response <- normalize_provider_response(
    raw,
    request = llm_request(
      messages = list(list(role = "user", content = "ping")),
      model = "requested-model", request_id = "req_123"
    ),
    provider = "openai"
  )

  expect_s3_class(response, "sas2r_llm_response")
  expect_identical(response$status, "completed")
  expect_identical(response$action, "final")
  expect_identical(response$data, list(ok = TRUE))
  expect_identical(response$response_id, "resp_123")
  expect_identical(response$request_id, "req_123")
  expect_identical(response$requested_model, "requested-model")
  expect_identical(response$resolved_model, "resolved-model")
  expect_identical(response$usage$total_input_tokens, 100)
  expect_identical(response$usage$input_tokens, 50)
  expect_identical(response$usage$cached_input_tokens, 20)
  expect_identical(response$usage$cache_write_tokens, 30)
  expect_identical(response$usage$total_output_tokens, 25)
  expect_identical(response$usage$output_tokens, 15)
  expect_identical(response$usage$reasoning_tokens, 10)
})

test_that("signed native accounting deltas survive response normalization", {
  # Break caught: a provider reporting hit + miss above its prompt total yields
  # a negative diagnostic delta; treating that diagnostic as a token count
  # silently converts it to NA before the ledger can flag the mismatch.
  usage <- list(
    input_tokens = 70,
    output_tokens = 10,
    total_input_tokens = 100,
    total_output_tokens = 10,
    total_tokens = 110,
    cached_input_tokens = 40,
    cache_write_tokens = NA_real_,
    reasoning_tokens = 0,
    tool_charges = NA_real_
  )
  attr(usage, "input_accounting_status") <- "mismatch"
  attr(usage, "input_accounting_delta_tokens") <- -10
  attr(usage, "total_accounting_status") <- "mismatch"
  attr(usage, "total_accounting_delta_tokens") <- -20

  normalized <- normalized_usage(list(usage = usage))

  expect_identical(attr(normalized, "input_accounting_status"), "mismatch")
  expect_identical(attr(normalized, "input_accounting_delta_tokens"), -10)
  expect_identical(attr(normalized, "total_accounting_status"), "mismatch")
  expect_identical(attr(normalized, "total_accounting_delta_tokens"), -20)
})

test_that("primary usage takes precedence over auxiliary usage metadata", {
  # Break caught: detecting an unused auxiliary envelope changed the meaning
  # of fields already selected from the primary usage object.
  normalized <- normalized_usage(list(
    usage = list(
      input_tokens = 10L,
      output_tokens = 2L,
      cached_input_tokens = 2L,
      total_tokens = 14L
    ),
    usageMetadata = list(
      promptTokenCount = 1000L,
      candidatesTokenCount = 100L,
      totalTokenCount = 1100L
    )
  ))

  expect_identical(normalized$input_tokens, 10)
  expect_identical(normalized$cached_input_tokens, 2)
  expect_identical(normalized$total_input_tokens, 12)
  expect_identical(normalized$output_tokens, 2)
  expect_true(is.na(normalized$reasoning_tokens))
  expect_identical(normalized$total_output_tokens, 2)
  expect_identical(normalized$total_tokens, 14)
})

test_that("refusal and incomplete responses remain terminal non-schema outcomes", {
  refusal <- normalize_provider_response(list(
    id = "resp_refused", status = "completed",
    output = list(list(
      type = "message", role = "assistant",
      content = list(list(type = "refusal", refusal = "cannot comply"))
    ))
  ), provider = "openai")
  incomplete <- normalize_provider_response(list(
    id = "resp_incomplete", status = "incomplete",
    incomplete_details = list(reason = "max_output_tokens"), output = list()
  ), provider = "openai")

  expect_identical(refusal$status, "refused")
  expect_identical(refusal$action, "none")
  expect_s3_class(refusal, "sas2r_llm_refused")
  expect_false(is_schema_retryable_response(refusal))
  expect_identical(incomplete$status, "incomplete")
  expect_identical(incomplete$finish_reason, "max_output_tokens")
  expect_s3_class(incomplete, "sas2r_llm_incomplete")
  expect_false(is_schema_retryable_response(incomplete))
})

test_that("terminal finish reasons override completed final payloads", {
  incomplete_reasons <- c("max_tokens", "context_window")
  for (reason in incomplete_reasons) {
    response <- normalize_provider_response(list(
      type = "final", data = list(ok = TRUE), finish_reason = reason
    ))
    expect_identical(response$status, "incomplete")
    expect_identical(response$action, "none")
    expect_null(response$data)
    expect_s3_class(response, "sas2r_llm_incomplete")
    expect_false(is_schema_retryable_response(response))
  }

  filtered <- normalize_provider_response(list(
    type = "final", data = list(ok = TRUE), finish_reason = "content_filter"
  ))
  expect_identical(filtered$status, "refused")
  expect_identical(filtered$action, "none")
  expect_null(filtered$data)
  expect_s3_class(filtered, "sas2r_llm_refused")
  expect_false(is_schema_retryable_response(filtered))
})

test_that("terminal finish reasons survive normalized response wrappers", {
  wrapped <- structure(
    list(
      status = "completed", action = "final", data = list(ok = TRUE),
      finish_reason = "max_tokens", request_id = NULL
    ),
    class = c("sas2r_llm_response", "list")
  )

  response <- normalize_provider_response(wrapped)

  expect_identical(response$status, "incomplete")
  expect_identical(response$action, "none")
  expect_null(response$data)
  expect_s3_class(response, "sas2r_llm_incomplete")
})

test_that("OpenAI completed messages honor content filter finishes", {
  response <- normalize_provider_response(list(
    id = "resp_filter", status = "completed", finish_reason = "content_filter",
    output = list(list(
      type = "message", role = "assistant", status = "completed",
      content = list(list(type = "output_json", json = list(ok = TRUE)))
    ))
  ), provider = "openai")

  expect_identical(response$status, "refused")
  expect_identical(response$action, "none")
  expect_null(response$data)
})

test_that("Bedrock safety stops reject parseable provider data", {
  for (reason in c("guardrail_intervened", "content_filtered")) {
    response <- normalize_provider_response(list(
      stopReason = reason,
      output = list(message = list(content = list(
        list(json = list(ok = TRUE))
      )))
    ), provider = "bedrock")

    expect_identical(response$status, "refused", info = reason)
    expect_identical(response$action, "none", info = reason)
    expect_null(response$data, info = reason)
  }
})

test_that("Bedrock context-window exhaustion discards parseable provider data", {
  fixtures <- list(
    data = list(json = list(ok = TRUE)),
    tool = list(toolUse = list(
      toolUseId = "partial_tool", name = "lookup", input = list(name = "round")
    ))
  )

  for (fixture in fixtures) {
    response <- normalize_provider_response(list(
      stopReason = "model_context_window_exceeded",
      output = list(message = list(content = list(fixture)))
    ), provider = "bedrock")

    expect_identical(response$status, "incomplete")
    expect_identical(response$action, "none")
    expect_null(response$data)
    expect_null(response$tool_name)
    expect_null(response$tool_arguments)
    expect_s3_class(response, "sas2r_llm_incomplete")
    expect_false(is_schema_retryable_response(response))
  }
})

test_that("Vertex safety stops reject parseable candidate data", {
  reasons <- c(
    "SAFETY", "RECITATION", "BLOCKLIST", "PROHIBITED_CONTENT", "SPII",
    "MODEL_ARMOR", "IMAGE_SAFETY", "IMAGE_PROHIBITED_CONTENT",
    "IMAGE_RECITATION"
  )
  for (reason in reasons) {
    response <- normalize_provider_response(list(
      candidates = list(list(
        finishReason = reason,
        content = list(parts = list(list(
          structured_output = list(ok = TRUE)
        )))
      ))
    ), provider = "vertex")

    expect_identical(response$status, "refused", info = reason)
    expect_identical(response$action, "none", info = reason)
    expect_null(response$data, info = reason)
  }
})

test_that("Vertex escalation discards parseable candidate data and tools", {
  fixtures <- list(
    data = list(structured_output = list(ok = TRUE)),
    tool = list(functionCall = list(
      id = "partial_tool", name = "lookup", args = list(name = "round")
    ))
  )

  for (fixture in fixtures) {
    response <- normalize_provider_response(list(
      candidates = list(list(
        finishReason = "ESCALATION",
        content = list(parts = list(fixture))
      ))
    ), provider = "vertex")

    expect_identical(response$status, "refused")
    expect_identical(response$action, "none")
    expect_null(response$data)
    expect_null(response$tool_name)
    expect_null(response$tool_arguments)
    expect_s3_class(response, "sas2r_llm_refused")
    expect_false(is_schema_retryable_response(response))
  }
})

test_that("provider truncation stops remain terminal incomplete", {
  fixtures <- list(
    bedrock = list(
      stopReason = "max_tokens",
      output = list(message = list(content = list(
        list(json = list(ok = TRUE))
      )))
    ),
    vertex = list(candidates = list(list(
      finishReason = "MAX_TOKENS",
      content = list(parts = list(list(
        structured_output = list(ok = TRUE)
      )))
    )))
  )

  for (provider in names(fixtures)) {
    response <- normalize_provider_response(fixtures[[provider]], provider = provider)
    expect_identical(response$status, "incomplete", info = provider)
    expect_identical(response$action, "none", info = provider)
    expect_null(response$data, info = provider)
  }
})

test_that("provider final and tool variants normalize to one contract", {
  fixtures <- list(
    openai_final = list(
      provider = "openai",
      raw = list(status = "completed", output = list(list(
        type = "message", content = list(list(
          type = "output_text", text = '{"ok":true}'
        ))
      ))),
      action = "final"
    ),
    azure_tool = list(
      provider = "azure",
      raw = list(status = "completed", output = list(list(
        type = "function_call", call_id = "call_azure", name = "lookup",
        arguments = '{"name":"round"}'
      ))),
      action = "tool_call"
    ),
    bedrock_final = list(
      provider = "bedrock",
      raw = list(stopReason = "end_turn", output = list(message = list(
        content = list(list(json = list(ok = TRUE)))
      ))),
      action = "final"
    ),
    bedrock_tool = list(
      provider = "bedrock",
      raw = list(stopReason = "tool_use", output = list(message = list(
        content = list(list(toolUse = list(
          toolUseId = "call_bedrock", name = "lookup", input = list(name = "round")
        )))
      ))),
      action = "tool_call"
    ),
    vertex_final = list(
      provider = "vertex",
      raw = list(candidates = list(list(
        finishReason = "STOP",
        content = list(parts = list(list(text = '{"ok":true}')))
      ))),
      action = "final"
    ),
    vertex_tool = list(
      provider = "vertex",
      raw = list(candidates = list(list(content = list(parts = list(list(
        functionCall = list(name = "lookup", args = list(name = "round"))
      )))))),
      action = "tool_call"
    ),
    ollama_final = list(
      provider = "ollama",
      raw = list(model = "local", done = TRUE, done_reason = "stop",
                 message = list(role = "assistant", content = '{"ok":true}')),
      action = "final"
    ),
    ollama_tool = list(
      provider = "ollama",
      raw = list(model = "local", done = TRUE, message = list(
        role = "assistant", content = "", tool_calls = list(list(
          "function" = list(name = "lookup", arguments = list(name = "round"))
        ))
      )),
      action = "tool_call"
    )
  )

  for (fixture in fixtures) {
    response <- normalize_provider_response(fixture$raw, provider = fixture$provider)
    expect_identical(response$status, "completed")
    expect_identical(response$action, fixture$action)
    if (identical(fixture$action, "final")) {
      expect_identical(response$data, list(ok = TRUE))
    } else {
      expect_identical(response$tool_name, "lookup")
      expect_identical(response$tool_arguments$name, "round")
    }
  }
})

test_that("transport failures map to distinct classed normalized outcomes", {
  fixtures <- list(
    auth = structure(list(message = "invalid credentials", status_code = 401L),
                     class = c("sas2r_llm_authentication_error", "error", "condition")),
    rate = structure(list(message = "rate limited", status_code = 429L),
                     class = c("sas2r_llm_rate_limit", "error", "condition")),
    timeout = structure(list(message = "timed out"),
                        class = c("sas2r_llm_timeout", "error", "condition")),
    schema = structure(list(message = "invalid schema", status_code = 400L),
                       class = c("sas2r_llm_invalid_schema", "error", "condition")),
    transport = structure(list(message = "connection failed"),
                          class = c("sas2r_llm_transport_error", "error", "condition"))
  )
  expected <- c(
    auth = "sas2r_llm_authentication_error",
    rate = "sas2r_llm_rate_limit",
    timeout = "sas2r_llm_timeout",
    schema = "sas2r_llm_invalid_schema",
    transport = "sas2r_llm_transport_error"
  )

  for (name in names(fixtures)) {
    response <- normalize_provider_response(fixtures[[name]])
    expect_identical(response$status, "failed")
    expect_identical(response$action, "none")
    expect_s3_class(response, expected[[name]])
  }
})

test_that("raw provider error payloads retain actionable failure classes", {
  fixtures <- list(
    auth = list(error = list(message = "invalid credentials", status_code = 401L)),
    rate = list(error = list(message = "rate limited", status_code = 429L)),
    timeout = list(error = list(message = "request timed out")),
    schema = list(error = list(message = "invalid response schema", status_code = 400L)),
    transport = list(error = "connection failed")
  )
  expected <- c(
    auth = "sas2r_llm_authentication_error",
    rate = "sas2r_llm_rate_limit",
    timeout = "sas2r_llm_timeout",
    schema = "sas2r_llm_invalid_schema",
    transport = "sas2r_llm_transport_error"
  )

  for (name in names(fixtures)) {
    response <- normalize_provider_response(fixtures[[name]])
    expect_identical(response$error$class, expected[[name]])
    expect_s3_class(response, expected[[name]])
  }
})

test_that("httr2-style response conditions retain HTTP failure classes", {
  make_http_error <- function(status, message) {
    structure(
      list(
        message = message,
        call = NULL,
        response = structure(list(status_code = status),
                             class = "httr2_response")
      ),
      class = c(paste0("httr2_http_", status), "error", "condition")
    )
  }

  auth <- normalize_provider_response(make_http_error(401L, "unauthorized"))
  rate <- normalize_provider_response(make_http_error(429L, "too many requests"))

  expect_s3_class(auth, "sas2r_llm_authentication_error")
  expect_s3_class(rate, "sas2r_llm_rate_limit")
})

test_that("classified timeouts receive a stable normalized reason", {
  request <- llm_request(
    messages = list(list(role = "user", content = "ping"))
  )
  response <- normalize_provider_response(
    simpleError("operation timed out after 50 milliseconds"),
    request = request, provider = "mock"
  )

  expect_identical(response$status, "failed")
  expect_identical(response$error$class, "sas2r_llm_timeout")
  expect_identical(response$error$reason, "timed_out")
})
