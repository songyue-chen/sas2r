spec_min <- function(schema = "program_translation_v1", tool_limit = 3, retries = 1)
  list(name = "t", prompt = "translator.md", tools = list(),
       tool_call_limit = tool_limit, retry_limit = retries,
       temperature = 0, output_schema = schema, on_budget_exhausted = "downgrade")

good <- list(
  type = "final",
  data = list(
    r_code = "x <- 1",
    summary = "test translation",
    parameters = list(),
    defaults = list(),
    reads = character(),
    writes = character(),
    side_effects = character(),
    helper_use = character(),
    discovered_dependencies = character(),
    suspected_dependencies = character(),
    affected_outputs = character(),
    uncertainty = list()
  )
)

test_that("happy path returns validated data", {
  r <- run_agent(spec_min(), mock_llm(list(good)), tools = list(),
                 user_content = "unit", log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
  expect_identical(r$data$r_code, "x <- 1")
})

test_that("invalid output retries with feedback, then downgrades", {
  bad <- list(type = "final", data = list(assumptions = list()))
  r <- run_agent(spec_min(retries = 1), mock_llm(list(bad, bad)), list(),
                 "unit", log_dir = withr::local_tempdir())
  expect_identical(r$status, "invalid_output")
  r2 <- run_agent(spec_min(retries = 1), mock_llm(list(bad, good)), list(),
                  "unit", log_dir = withr::local_tempdir())
  expect_identical(r2$status, "ok")
})

test_that("schema repair is submitted as a current user turn", {
  bad <- list(type = "final", data = list(assumptions = list()))
  responses <- list(bad, good)
  requests <- list()
  llm <- new_llm(function(request) {
    requests[[length(requests) + 1L]] <<- request
    normalize_provider_response(
      responses[[length(requests)]], request = request, provider = "mock"
    )
  }, provider = "mock")

  result <- run_agent(
    spec_min(retries = 1L), llm, list(), "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_identical(
    vapply(requests[[2]]$messages, `[[`, "", "role"),
    c("system", "user", "assistant", "user")
  )
  expect_match(requests[[2]]$messages[[3]]$content, "assumptions")
})

test_that("malformed repair retains an assistant turn before user feedback", {
  requests <- list()
  responses <- list(
    new_llm_response(status = "completed", action = "none"),
    normalize_provider_response(good, provider = "mock")
  )
  llm <- new_llm(function(request) {
    requests[[length(requests) + 1L]] <<- request
    responses[[length(requests)]]
  }, provider = "mock")

  result <- run_agent(
    spec_min(retries = 1L), llm, list(), "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_identical(
    vapply(requests[[2]]$messages, `[[`, "", "role"),
    c("system", "user", "assistant", "user")
  )
})

test_that("tool calls route through budgets and exhaustion downgrades", {
  tool_resp <- list(type = "tool", tool = "echo", args = list(x = 1))
  tools <- list(echo = make_tool(
    "echo", function(a) list(got = a$x), 5,
    schema = list(
      type = "object", properties = list(x = list(type = "number")),
      required = "x", additionalProperties = FALSE
    )
  ))
  r <- run_agent(spec_min(tool_limit = 1),
                 mock_llm(list(tool_resp, good)), tools, "unit",
                 log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
  expect_identical(r$tool_calls, 1L)
  # Exhausting the tool allowance no longer discards the unit: the model is
  # asked to answer with what it gathered, and that answer is used.
  r2 <- run_agent(spec_min(tool_limit = 1),
                  mock_llm(list(tool_resp, tool_resp, good)), tools, "unit",
                  log_dir = withr::local_tempdir())
  expect_identical(r2$status, "ok")
  expect_identical(r2$tool_calls, 1L)
})

test_that("the per-agent ceiling records a direct policy refusal", {
  state <- new_agent_tool_state(
    0L,
    new_usage_budget(mode = "observe", max_tool_calls = 10L)
  )
  state$audit_context <- list(request_id = "req_direct", unit_id = 29L)

  expect_error(
    reserve_agent_tool_call(state, "get_macro_source", list(name = "helper")),
    class = "sas2r_agent_tool_limit"
  )

  expect_identical(state$usage_budget$tool_request_count, 1L)
  expect_identical(state$usage_budget$tool_count, 0L)
  refused <- Filter(
    function(record) identical(record$record_type, "tool_refused"),
    state$usage_budget$records
  )
  expect_length(refused, 1L)
  expect_identical(refused[[1L]]$result_status, "agent_tool_limit")
  expect_false(refused[[1L]]$admitted)
})

test_that("transport-bound tools record agent and global ceiling refusals", {
  capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported"
  )
  bound_llm <- function() new_llm(
    function(request) {
      request$tools[[1L]]$call(list(x = 1))
      normalize_provider_response(good, request = request, provider = "mock")
    },
    provider = "mock", model = "m", capabilities = capabilities
  )
  tools <- list(echo = make_tool(
    "echo", function(args) list(got = args$x), 1L,
    schema = closed_tool_schema(
      list(x = list(type = "number")), required = "x"
    )
  ))

  agent_budget <- new_usage_budget(max_tool_calls = 10L)
  agent_result <- run_agent(
    spec_min(tool_limit = 0L), bound_llm(), tools, "unit",
    log_dir = withr::local_tempdir(), usage_budget = agent_budget
  )
  expect_identical(agent_result$status, "agent_tool_limit_reached")
  expect_identical(agent_budget$tool_request_count, 1L)
  expect_identical(agent_budget$tool_count, 0L)
  expect_identical(
    Filter(function(x) identical(x$record_type, "tool_refused"),
           agent_budget$records)[[1L]]$result_status,
    "agent_tool_limit"
  )

  global_budget <- new_usage_budget(max_tool_calls = 0L)
  global_result <- run_agent(
    spec_min(tool_limit = 1L), bound_llm(), tools, "unit",
    log_dir = withr::local_tempdir(), usage_budget = global_budget
  )
  expect_identical(global_result$status, "transport_failed")
  expect_identical(global_budget$tool_request_count, 1L)
  expect_identical(global_budget$tool_count, 0L)
  expect_identical(
    Filter(function(x) identical(x$record_type, "tool_refused"),
           global_budget$records)[[1L]]$result_status,
    "global_tool_limit"
  )
})

test_that("a managed retry links its tool to the executing subrequest", {
  capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", temperature = "supported"
  )
  attempts <- 0L
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(
      request, capabilities,
      function(attempted_request, params) {
        attempts <<- attempts + 1L
        if (attempts == 1L) {
          stop(llm_optional_parameter_error("temperature"))
        }
        attempted_request$tools[[1L]]$call(list(x = 7))
        normalize_provider_response(
          good, request = attempted_request, provider = "mock"
        )
      }
    )
  }, provider = "mock", model = "m", capabilities = capabilities)
  llm <- with_usage_managed_request(llm)
  budget <- new_usage_budget()
  tools <- list(echo = make_tool(
    "echo", function(args) list(got = args$x), 1L,
    schema = closed_tool_schema(
      list(x = list(type = "number")), required = "x"
    )
  ))

  result <- run_agent(
    spec_min(tool_limit = 1L), llm, tools, "unit",
    log_dir = withr::local_tempdir(), usage_budget = budget,
    audit_context = list(unit_id = 62L, purpose = "translation")
  )

  expect_identical(result$status, "ok")
  started <- Filter(
    function(record) identical(record$record_type, "request_started"),
    budget$records
  )
  terminal <- Filter(
    function(record) identical(record$record_type, "tool_executed"),
    budget$records
  )[[1L]]
  expect_length(started, 2L)
  expect_identical(terminal$request_id, started[[2L]]$request_id)
  expect_false(identical(terminal$request_id, started[[1L]]$request_id))
  expect_identical(terminal$parent_request_id, started[[2L]]$parent_request_id)
})

test_that("direct tool-call lifecycle records unit, phase, subject, and outcome", {
  # Break caught: the non-ellmer tool-call branch reserved an anonymous event
  # before resolving the tool name, then never wrote whether execution ended.
  tool_resp <- list(type = "tool", tool = "echo", args = list(x = 7))
  tools <- list(echo = make_tool(
    "echo", function(args) list(got = args$x), 2L,
    schema = closed_tool_schema(
      list(x = list(type = "number")), required = "x"
    )
  ))
  budget <- new_usage_budget(mode = "observe")

  result <- run_agent(
    spec_min(tool_limit = 2L), mock_llm(list(tool_resp, good)), tools,
    "unit", log_dir = withr::local_tempdir(), usage_budget = budget,
    audit_context = list(purpose = "translation", unit_id = 29L)
  )

  expect_identical(result$status, "ok")
  completed <- Filter(
    function(record) identical(record$record_type, "tool_executed"),
    budget$records
  )
  expect_length(completed, 1L)
  expect_match(completed[[1L]]$tool_name, "^tool_sha256_")
  expect_match(completed[[1L]]$tool_arguments, "sha256", fixed = TRUE)
  expect_identical(completed[[1L]]$unit_id, 29L)
  expect_identical(completed[[1L]]$agent, "t")
  expect_identical(completed[[1L]]$purpose, "translation")
  expect_identical(completed[[1L]]$phase, "finalization")
  expect_identical(completed[[1L]]$outcome, "completed")
})

test_that("tool lifecycle instrumentation accepts scalar tool results", {
  # A tool result is not required to be a named list. Instrumentation must not
  # dereference cache fields on an otherwise valid atomic result.
  tool_resp <- list(type = "tool", tool = "scalar", args = list())
  scalar_tool <- make_tool(
    "scalar", function(args) "ok", 1L, schema = closed_tool_schema()
  )

  result <- run_agent(
    spec_min(tool_limit = 1L), mock_llm(list(tool_resp, good)),
    list(scalar = scalar_tool), "unit", log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
})

test_that("a tool error gets a terminal failed record before it propagates", {
  # Break caught: if local tool code raises, a lone attempted record leaves
  # the audit trail unable to distinguish a crash from an in-flight process.
  tool_resp <- list(type = "tool", tool = "broken", args = list())
  tools <- list(broken = make_tool(
    "broken", function(args) stop("tool exploded"), 1L,
    schema = closed_tool_schema()
  ))
  budget <- new_usage_budget(mode = "observe")

  expect_error(
    run_agent(
      spec_min(tool_limit = 1L), mock_llm(list(tool_resp)), tools,
      "unit", log_dir = withr::local_tempdir(), usage_budget = budget,
      audit_context = list(purpose = "translation", unit_id = 62L)
    ),
    "tool exploded"
  )

  failed <- Filter(
    function(record) identical(record$record_type, "tool_failed"),
    budget$records
  )
  expect_length(failed, 1L)
  expect_identical(failed[[1L]]$unit_id, 62L)
  expect_identical(failed[[1L]]$error_class, "simpleError")
})

test_that("unknown tools are reported to the model, not crashed on", {
  resp <- list(type = "tool", tool = "ghost", args = list())
  r <- run_agent(spec_min(tool_limit = 2), mock_llm(list(resp, good)),
                 list(), "unit", log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
})

test_that("prompts render placeholders and exchanges are logged", {
  dir <- withr::local_tempdir()
  p <- render_prompt("translator.md", list(dialect = "tidyverse"))
  expect_match(p, "tidyverse")
  expect_false(grepl("\\{\\{", p))
  run_agent(spec_min(), mock_llm(list(good)), list(), "unit", log_dir = dir)
  expect_true(file.exists(file.path(dir, "llm_log.jsonl")))
})

test_that("runner logs the post-downgrade response capability hash", {
  dir <- withr::local_tempdir()
  capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", record_hash = "before"
  )
  llm <- new_llm(
    function(request) new_llm_response(
      status = "completed", action = "final", data = good$data,
      request = request, capability_hash = "after"
    ),
    provider = "mock", capabilities = capabilities
  )

  result <- run_agent(spec_min(), llm, list(), "unit", log_dir = dir)

  expect_identical(result$status, "ok")
  row <- jsonlite::fromJSON(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)[[1]]
  )
  expect_identical(row$capability_hash, "after")
})

test_that("runner redacts an exact configured transport key echoed by a provider", {
  dir <- withr::local_tempdir()
  secret <- "configured-azure-transport-key-7c39"
  nested_secret <- "nested-runner-credential-82e1"
  callback_secret <- "callback-runner-credential-4a62"
  cache_path <- "/Users/reviewer/.azure/msal_token_cache.json"
  patient <- "patient-request-content-must-not-enter-failure-audit"
  clear_llm_registered_secrets()
  withr::defer(clear_llm_registered_secrets())
  cfg <- normalize_llm_config(list(
    provider = "azure", auth_mode = "api_key",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    model = "deployment-a", api_key = secret
  ))
  expect_identical(
    ellmer_constructor_args(cfg, model = cfg$model)$api_key,
    secret
  )
  register_llm_secret_values(callback_secret)
  llm <- new_llm(
    function(request) new_llm_response(
      status = "failed", action = "none", request = request,
      error = list(
        class = "sas2r_llm_authentication_error",
        reason = "authentication_rejected",
        message = paste("provider rejected", secret, callback_secret, cache_path),
        credential_bundle = list(access_token = nested_secret),
        content = patient,
        nested = list(
          message = paste("nested echo", nested_secret),
          request = list(content = patient)
        )
      ),
      extra_class = "sas2r_llm_authentication_error"
    ),
    provider = "azure",
    capabilities = llm_capabilities(structured_output = "native", source = "test"),
    redaction_secrets = llm_secret_values(cfg)
  )

  result <- run_agent(
    spec_min(), llm, tools = list(), user_content = "unit", log_dir = dir
  )
  text <- paste(readLines(
    file.path(dir, "llm_log.jsonl"), warn = FALSE
  ), collapse = "\n")

  expect_identical(result$status, "authentication_failed")
  expect_identical(result$error$class, "sas2r_llm_authentication_error")
  expect_identical(result$error$reason, "authentication_rejected")
  public_text <- jsonlite::toJSON(result$error, auto_unbox = TRUE)
  for (leak in c(secret, nested_secret, callback_secret, cache_path)) {
    expect_false(grepl(leak, public_text, fixed = TRUE), info = leak)
  }
  expect_false(grepl(secret, text, fixed = TRUE))
  expect_false(grepl(patient, text, fixed = TRUE))
  expect_match(text, "[REDACTED]", fixed = TRUE)
  audit <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  expect_false(any(c("messages", "response") %in% names(audit)))
  expect_named(audit$error, c("class", "reason", "message"),
               ignore.order = FALSE)

  protected <- llm_audit_redactor(llm)(list(
    credential_bundle = list(access_token = nested_secret),
    error = list(message = paste(nested_secret, callback_secret, cache_path))
  ))
  protected_text <- jsonlite::toJSON(protected, auto_unbox = TRUE)
  for (leak in c(nested_secret, callback_secret, cache_path)) {
    expect_false(grepl(leak, protected_text, fixed = TRUE), info = leak)
  }
})

test_that("extract_response_cost accepts only explicitly classified cost metadata", {
  # 1. Direct cost attribute
  resp1 <- good
  attr(resp1, "cost_usd") <- 0.05
  expect_equal(extract_response_cost(resp1), 0.05)

  # 2. Transport usage without a provider price remains explicitly unknown.
  resp2 <- good
  attr(resp2, "usage") <- list(input_tokens = 1000, output_tokens = 500)
  expect_true(is.na(extract_response_cost(resp2, tier = "cheap")))

  # 3. Model payload resp$usage is IGNORED
  resp3 <- good
  resp3$usage <- list(input_tokens = 999999)
  expect_true(is.na(extract_response_cost(resp3)))

  # 4. Malformed/non-numeric tokens do not crash
  resp4 <- good
  attr(resp4, "usage") <- list(input_tokens = "garbage", output_tokens = NA)
  expect_no_error(cost <- extract_response_cost(resp4))
  expect_true(is.na(cost))
})

test_that("run_agent flags usage-only transport responses as cost_unknown", {
  usage_only <- good
  attr(usage_only, "usage") <- list(input_tokens = 100, output_tokens = 25)
  charged <- numeric()
  r <- run_agent(
    spec_min(), mock_llm(list(usage_only)), list(), "unit",
    log_dir = withr::local_tempdir(),
    on_charge = function(amount) charged <<- c(charged, amount)
  )
  expect_identical(r$status, "ok")
  expect_true(isTRUE(r$cost_unknown))
  expect_equal(r$spend_usd, 0)
  expect_true(is.na(charged[1]))
})

test_that("run_agent tracks cumulative spend across retries and on errors", {
  bad <- list(type = "final", data = list(assumptions = list()))
  attr(bad, "cost_usd") <- 0.01
  good_cost <- good
  attr(good_cost, "cost_usd") <- 0.02

  r <- run_agent(spec_min(retries = 1), mock_llm(list(bad, good_cost)), list(),
                 "unit", log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
  expect_equal(r$known_cost_usd, 0.03)
  expect_equal(r$estimated_cost_usd, 0.03)
  expect_equal(r$spend_usd, 0)
})

test_that("agents with no tools go directly to native structured finalization", {
  requests <- list()
  llm <- new_llm(
    function(request) {
      requests[[length(requests) + 1L]] <<- request
      normalize_provider_response(good, request = request, provider = "mock")
    },
    provider = "mock",
    capabilities = llm_capabilities(
      structured_output = "native", tool_calling = "native",
      tools_with_structured_output = "unsupported"
    )
  )

  result <- run_agent(
    spec_min(), llm, tools = list(), user_content = "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_length(requests, 1L)
  expect_s3_class(requests[[1]], "sas2r_llm_request")
  expect_identical(requests[[1]]$phase, "finalization")
  expect_identical(requests[[1]]$schema_mode, "native")
  expect_length(requests[[1]]$tools, 0L)
  expect_false(requests[[1]]$output_schema$additionalProperties)
  expect_identical(
    vapply(requests[[1]]$messages, `[[`, "", "role"),
    c("system", "user")
  )
})

test_that("tools and structured output use a ledger-visible two-phase state machine", {
  tool_executions <- 0L
  responses <- list(
    list(type = "tool", tool = "echo", args = list(x = 1), tool_call_id = "call_1"),
    list(type = "final", data = list(notes = "gathering complete")),
    good
  )
  requests <- list()
  i <- 0L
  llm <- new_llm(
    function(request) {
      requests[[length(requests) + 1L]] <<- request
      i <<- i + 1L
      normalize_provider_response(responses[[i]], request = request, provider = "mock")
    },
    provider = "mock",
    capabilities = llm_capabilities(
      structured_output = "native", tool_calling = "native",
      tools_with_structured_output = "supported"
    ),
    transport_constraints = list(
      tools_with_structured_output = "unsupported"
    )
  )
  echo <- make_tool(
    "echo", function(args) {
      tool_executions <<- tool_executions + 1L
      list(got = args$x)
    }, max_calls = 2L,
    schema = list(
      type = "object", properties = list(x = list(type = "number")),
      required = "x", additionalProperties = FALSE
    )
  )

  result <- run_agent(
    spec_min(tool_limit = 2), llm, tools = list(echo = echo),
    user_content = "unit", log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_identical(tool_executions, 1L)
  expect_identical(
    llm_capabilities_for(llm)$tools_with_structured_output, "unsupported"
  )
  expect_identical(result$tool_calls, 1L)
  expect_length(requests, 3L)
  expect_identical(
    vapply(requests, `[[`, "", "phase"),
    c("gathering", "gathering", "finalization")
  )
  expect_true(length(requests[[1]]$tools) == 1L)
  expect_null(requests[[1]]$output_schema)
  expect_length(requests[[3]]$tools, 0L)
  expect_false(is.null(requests[[3]]$output_schema))
  expect_true("tool" %in% vapply(requests[[2]]$messages, `[[`, "", "role"))
})

test_that("fallback schema mode is explicit and audited", {
  requests <- list()
  llm <- new_llm(
    function(request) {
      requests[[length(requests) + 1L]] <<- request
      normalize_provider_response(good, request = request, provider = "mock")
    },
    provider = "mock",
    capabilities = llm_capabilities(
      structured_output = "fallback", tool_calling = "unsupported",
      tools_with_structured_output = "unsupported"
    )
  )

  result <- run_agent(
    spec_min(), llm, tools = list(), user_content = "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_identical(requests[[1]]$schema_mode, "fallback")
  expect_true(isTRUE(result$fallback_json))
})

test_that("runner routes by the selected tier model capability record", {
  requests <- list()
  frontier <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", model = "frontier-model"
  )
  cheap <- llm_capabilities(
    structured_output = "fallback", tool_calling = "unsupported",
    tools_with_structured_output = "unsupported", model = "cheap-model"
  )
  llm <- new_llm(function(request) {
    requests[[length(requests) + 1L]] <<- request
    normalize_provider_response(good, request = request, provider = "mock")
  }, provider = "mock", capabilities = frontier)
  llm$capabilities_for <- function(tier, model = NULL) {
    if (identical(tier, "cheap")) cheap else frontier
  }
  spec <- spec_min()
  spec$tier <- "cheap"

  result <- run_agent(
    spec, llm, tools = list(), user_content = "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_identical(requests[[1]]$schema_mode, "fallback")
  expect_true(isTRUE(result$fallback_json))
})

test_that("selected tier tool capability fails closed before transport", {
  calls <- 0L
  frontier <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", model = "frontier-model"
  )
  cheap <- llm_capabilities(
    structured_output = "native", tool_calling = "unknown",
    tools_with_structured_output = "unknown", model = "cheap-model"
  )
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(good, request = request, provider = "mock")
  }, provider = "mock", capabilities = frontier)
  llm$capabilities_for <- function(tier, model = NULL) {
    if (identical(tier, "cheap")) cheap else frontier
  }
  spec <- spec_min()
  spec$tier <- "cheap"
  echo <- make_tool(
    "echo", function(args) args, max_calls = 1L,
    schema = list(
      type = "object", properties = list(), additionalProperties = FALSE
    )
  )

  result <- run_agent(
    spec, llm, tools = list(echo = echo), user_content = "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "tool_calling_unavailable")
  expect_identical(calls, 0L)
})

test_that("unknown structured-output capability does not enable fallback", {
  calls <- 0L
  llm <- new_llm(
    function(request) {
      calls <<- calls + 1L
      normalize_provider_response(good, request = request, provider = "custom")
    },
    provider = "custom",
    capabilities = llm_capabilities()
  )

  result <- run_agent(
    spec_min(), llm, tools = list(), user_content = "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "structured_output_unavailable")
  expect_identical(calls, 0L)
})

test_that("refusal and incomplete responses do not enter schema repair", {
  fixtures <- list(
    refused = structure(
      list(status = "refused", action = "none", data = NULL),
      class = c("sas2r_llm_refused", "sas2r_llm_response", "list")
    ),
    incomplete = structure(
      list(status = "incomplete", action = "none", data = NULL),
      class = c("sas2r_llm_incomplete", "sas2r_llm_response", "list")
    )
  )

  for (name in names(fixtures)) {
    calls <- 0L
    llm <- new_llm(function(request) {
      calls <<- calls + 1L
      fixtures[[name]]
    }, provider = "mock")
    result <- run_agent(
      spec_min(retries = 2L), llm, list(), "unit",
      log_dir = withr::local_tempdir()
    )
    expect_identical(result$status, name)
    expect_identical(calls, 1L)
  }
})

test_that("versioned schemas reject out-of-contract fields after native success", {
  schema <- agent_output_schema("program_translation_v1")
  expect_identical(schema$`x-sas2r-schema-version`, "1")
  expect_false(schema$additionalProperties)

  extra <- c(good$data, list(unexpected = TRUE))
  result <- validate_output(extra, "program_translation_v1")
  expect_false(result$ok)
  expect_match(result$errors, "unexpected field")
})

test_that("runner logs skill_provenance in audit entry when provided in audit_context", {
  dir <- withr::local_tempdir()
  prov <- list(
    list("sas-missing-sort-semantics", 1L, paste(rep("a", 64), collapse = ""), "procs.sort,sort")
  )
  result <- run_agent(
    spec_min(), mock_llm(list(good)), tools = list(), user_content = "unit",
    log_dir = dir,
    audit_context = list(skill_provenance = prov)
  )

  expect_identical(result$status, "ok")
  log_lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
  expect_true(length(log_lines) >= 1L)
  entry <- jsonlite::fromJSON(log_lines[[1]], simplifyVector = FALSE)
  expect_false(is.null(entry$skill_provenance))
  expect_identical(entry$skill_provenance[[1]][[1]], "sas-missing-sort-semantics")
})

test_that("validate_output reports a bad enum instead of aborting on an untyped node", {
  testthat::local_mocked_bindings(
    agent_output_schema = function(schema_name) list(
      type = "object",
      required = list("mode"),
      properties = list(mode = list(enum = list("keep", "repair")))
    ),
    .package = "sas2r"
  )
  ok <- validate_output(list(mode = "keep"), "untyped_enum")
  expect_true(ok$ok)
  expect_identical(ok$errors, character())

  bad <- validate_output(list(mode = "nope"), "untyped_enum")
  expect_false(bad$ok)
  expect_match(bad$errors, "not in enum", all = FALSE)
})

test_that("validate_output tolerates an untyped const node", {
  testthat::local_mocked_bindings(
    agent_output_schema = function(schema_name) list(
      type = "object",
      properties = list(kind = list(const = "final"))
    ),
    .package = "sas2r"
  )
  expect_true(validate_output(list(kind = "final"), "untyped_const")$ok)
})

test_that("the tool-call limit message names the scope it actually has", {
  # tool_state is created inside run_agent(), so the limit is per agent
  # invocation -- one unit's allowance, reset for the next. Calling it "global"
  # sends a caller to the project-level budget: max_tool_calls setting when the
  # one to change is the agent spec's tool_call_limit.
  state <- new_agent_tool_state(1L)
  reserve_agent_tool_call(state, "read_unit_context")

  err <- tryCatch(reserve_agent_tool_call(state, "read_unit_context"),
                  sas2r_agent_tool_limit = function(e) e)

  expect_s3_class(err, "sas2r_agent_tool_limit")
  expect_no_match(conditionMessage(err), "global")
  expect_match(conditionMessage(err), "tool_call_limit")
})

test_that("each agent invocation gets its own tool allowance", {
  first <- new_agent_tool_state(2L)
  reserve_agent_tool_call(first); reserve_agent_tool_call(first)

  second <- new_agent_tool_state(2L)

  expect_identical(second$count, 0L)
  expect_no_error(reserve_agent_tool_call(second))
})

test_that("a spent tool allowance asks for an answer instead of discarding the unit", {
  # Four macro units in the validation project each burned 15 tool calls and
  # ~40k output tokens, then were abandoned the moment they asked for a 16th --
  # 170,864 output tokens paid for and thrown away, with the manifest blaming a
  # budget that was not set.
  tools <- list(t = list(
    name = "t", description = "d", call = function(args) list(ok = TRUE),
    schema = closed_tool_schema()
  ))
  tool_resp <- list(type = "tool", tool = "t", args = list())

  result <- run_agent(spec_min(tool_limit = 2),
                      mock_llm(list(tool_resp, tool_resp, tool_resp, good)),
                      tools, "unit", log_dir = withr::local_tempdir())

  expect_identical(result$status, "ok")
  expect_identical(result$tool_calls, 2L)
})

test_that("a model that will not answer after exhaustion reports the tool limit", {
  tools <- list(t = list(
    name = "t", description = "d", call = function(args) list(ok = TRUE),
    schema = closed_tool_schema()
  ))
  tool_resp <- list(type = "tool", tool = "t", args = list())

  result <- run_agent(spec_min(tool_limit = 1),
                      mock_llm(list(tool_resp, tool_resp, tool_resp)),
                      tools, "unit", log_dir = withr::local_tempdir())

  # Named for what happened: the allowance ran out, no budget was involved.
  expect_identical(result$status, "agent_tool_limit_reached")
})

test_that("a completed answer survives a tool allowance spent inside the transport", {
  # The transport resolves tool calls within a single request, so the allowance
  # is spent by the bound tool wrappers and never reaches the tool_call branch.
  # A live macro unit gathered context, used all 15 calls, answered with 53,244
  # tokens -- and the answer was discarded because the tools had run out.
  state <- new_agent_tool_state(1L)
  bound <- bind_transport_tool_limits(
    list(t = list(name = "t", description = "d",
                  call = function(args) list(ok = TRUE),
                  schema = closed_tool_schema())),
    state
  )

  bound$t$call(list())
  expect_error(bound$t$call(list()), class = "sas2r_agent_tool_limit")
  expect_true(state$exhausted)

  # Emulate the transport: it invokes the bound tools itself, inside one
  # request, and answers afterwards. mock_llm alone cannot reach this path,
  # which is why the defect survived -- the mocks only exercise the tool_call
  # branch that a real transport never takes.
  transport <- new_llm(
    function(request) {
      for (tool in request$tools %||% list()) {
        for (k in 1:2) try(tool$call(list()), silent = TRUE)
      }
      normalize_provider_response(good, request = request, provider = "mock")
    },
    provider = "mock"
  )

  result <- run_agent(
    spec_min(tool_limit = 1), transport,
    tools = list(t = list(name = "t", description = "d",
                          call = function(args) list(ok = TRUE),
                          schema = closed_tool_schema())),
    user_content = "unit", log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
})

test_that("runner loads and validates new migration worker schemas", {
  for (s in c("program_translation_v1", "program_review_v1", "program_fix_v1")) {
    schema <- agent_output_schema(s)
    expect_identical(schema$`x-sas2r-schema-version`, "1")
    expect_false(schema$additionalProperties)
  }
})


test_that("transient transport failures retry with bounded backoff; terminal ones do not", {
  withr::local_options(sas2r.agent_backoff_base = 0)

  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    if (calls <= 2L) {
      stop(structure(list(message = "rate limited", status_code = 429L),
                     class = c("sas2r_llm_rate_limit", "error", "condition")))
    }
    normalize_provider_response(good, request = request, provider = "mock")
  }, provider = "mock")
  r <- run_agent(spec_min(), llm, tools = list(), user_content = "unit",
                 log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
  expect_identical(calls, 3L)

  # A persistent transient failure still surfaces after bounded retries.
  calls2 <- 0L
  llm_fail <- new_llm(function(request) {
    calls2 <<- calls2 + 1L
    stop(structure(list(message = "rate limited", status_code = 429L),
                   class = c("sas2r_llm_rate_limit", "error", "condition")))
  }, provider = "mock")
  r2 <- run_agent(spec_min(), llm_fail, tools = list(), user_content = "unit",
                  log_dir = withr::local_tempdir())
  expect_identical(r2$status, "rate_limited")
  expect_identical(calls2, 3L)

  # Authentication failures are terminal and are never retried.
  calls3 <- 0L
  llm_auth <- new_llm(function(request) {
    calls3 <<- calls3 + 1L
    stop(structure(list(message = "bad key", status_code = 401L),
                   class = c("sas2r_llm_authentication_error", "error", "condition")))
  }, provider = "mock")
  r3 <- run_agent(spec_min(), llm_auth, tools = list(), user_content = "unit",
                  log_dir = withr::local_tempdir())
  expect_identical(r3$status, "authentication_failed")
  expect_identical(calls3, 1L)
})
