fixed_rate_fixture <- function() {
  list(list(
    provider = "mock", resolved_model = "m", region = "*",
    service_tier = "frontier", currency = "USD",
    source = "organization-contract", source_version = "2026-08-20",
    effective_date = "2026-08-20",
    input_per_million = 1,
    output_per_million = 10,
    cached_input_per_million = 0,
    cache_write_per_million = 0,
    reasoning_per_million = 10,
    tool_rates = list()
  ))
}

partitioned_rate_fixture <- function() {
  list(list(
    provider = "provider-a", resolved_model = "reasoning-model", region = "*",
    service_tier = "frontier", currency = "USD",
    source = "synthetic-contract", source_version = "test-v1",
    effective_date = "2000-01-01",
    input_per_million = 0.20,
    cached_input_per_million = 0.02,
    cache_write_per_million = 0.25,
    output_per_million = 1.20,
    reasoning_per_million = 1.20,
    tool_rates = list()
  ))
}

usage_good_response <- function() {
  list(type = "final", data = list(
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
  ))
}

usage_spec_min <- function(retries = 1L, tool_limit = 3L) {
  list(
    name = "translator", prompt = "translator.md", tools = list(),
    tool_call_limit = tool_limit, retry_limit = retries,
    temperature = 0, output_schema = "program_translation_v1",
    on_budget_exhausted = "downgrade"
  )
}

budget_request_fixture <- function(text = "ping", max_output_tokens = 1000L,
                                   request_id = NULL) {
  llm_request(
    messages = list(list(role = "user", content = text)),
    tier = "frontier", model = "m", request_id = request_id,
    max_output_tokens = max_output_tokens
  )
}

test_that("ellmer cost is catalog_estimate, never provider-reported", {
  row <- usage_from_ellmer(
    cost = 0.05, input = 100, output = 20,
    ellmer_version = "0.4.2"
  )

  expect_identical(row$cost_status, "catalog_estimate")
  expect_match(row$rate_source, "ellmer|LiteLLM")
  expect_identical(row$source_version, "0.4.2")
})

test_that("strict mode fails closed before an unpriced call", {
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(
      usage_good_response(), request = request, provider = "mock"
    )
  }, provider = "mock")
  budget <- new_usage_budget(mode = "strict", max_usd = 1)

  result <- attempt_llm_request(
    budget_request_fixture(), llm, usage_budget = budget,
    audit_context = list(purpose = "translation")
  )

  expect_identical(result$status, "budget_exhausted")
  expect_s3_class(attr(result, "budget_error"), "sas2r_budget_unmeterable")
  expect_identical(calls, 0L)
})

test_that("strict mode requires a provider-enforced output ceiling", {
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(
      usage_good_response(), request = request, provider = "mock"
    )
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native", max_output_tokens = "unknown",
    source = "test"
  ), model = "m")
  budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = fixed_rate_fixture(),
    max_output_tokens = 1000L
  )

  result <- attempt_llm_request(
    budget_request_fixture(max_output_tokens = 1000L), llm, budget,
    list(provider = "mock", resolved_model = "m")
  )

  expect_identical(result$status, "budget_exhausted")
  expect_s3_class(attr(result, "budget_error"), "sas2r_budget_unmeterable")
  expect_identical(calls, 0L)
})

test_that("finite output ceilings fail closed when the adapter cannot enforce them", {
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(
      usage_good_response(), request = request, provider = "mock"
    )
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native", max_output_tokens = "unknown",
    source = "test"
  ), model = "m")
  budget <- new_usage_budget(
    mode = "observe", max_output_tokens = 1000L
  )

  result <- attempt_llm_request(
    budget_request_fixture(max_output_tokens = NULL), llm, budget
  )

  expect_identical(result$status, "budget_exhausted")
  expect_s3_class(attr(result, "budget_error"), "sas2r_budget_unmeterable")
  expect_identical(calls, 0L)
})

test_that("resume reconstructs completed costs and in-flight reservations", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  first <- new_usage_budget(
    mode = "strict", max_usd = 0.50, rates = fixed_rate_fixture(),
    ledger_path = path, run_id = "run_first"
  )
  request <- budget_request_fixture("x", max_output_tokens = 1000L,
                                    request_id = "req_first")
  reservation <- reserve_usage_request(
    first, request,
    audit_context = list(provider = "mock", resolved_model = "m")
  )
  response <- new_llm_response(
    status = "completed", action = "final", data = usage_good_response()$data,
    request = request, resolved_model = "m",
    usage = list(
      input_tokens = 100, output_tokens = 20, total_tokens = 120,
      cached_input_tokens = 0, cache_write_tokens = 0,
      reasoning_tokens = 0
    ),
    cost = list(
      amount_usd = 0.40, status = "billed_amount", currency = "USD",
      source = "provider-invoice"
    ), provider = "mock"
  )
  reconcile_usage_request(first, reservation, response)

  inflight <- budget_request_fixture("y", max_output_tokens = 1000L,
                                     request_id = "req_inflight")
  reserve_usage_request(
    first, inflight,
    audit_context = list(provider = "mock", resolved_model = "m")
  )

  resumed <- load_usage_budget(
    path, mode = "strict", max_usd = 0.50,
    rates = fixed_rate_fixture(), run_id = "run_resumed"
  )

  expect_equal(resumed$known_amount, 0.40)
  expect_true(resumed$reserved_amount > 0)
  expect_equal(resumed$remaining_usd,
               0.50 - resumed$known_amount - resumed$reserved_amount)
})

test_that("reservation prevents a one-call strict overshoot", {
  budget <- new_usage_budget(
    mode = "strict", max_usd = 0.00001,
    rates = fixed_rate_fixture(), max_output_tokens = 1000L
  )

  expect_false(can_reserve_request(
    budget,
    budget_request_fixture(strrep("x", 1000), max_output_tokens = 1000L),
    audit_context = list(provider = "mock", resolved_model = "m")
  ))

  foreign_rates <- fixed_rate_fixture()
  foreign_rates[[1]]$currency <- "EUR"
  foreign_budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = foreign_rates,
    max_output_tokens = 1000L
  )
  expect_false(can_reserve_request(
    foreign_budget, budget_request_fixture(max_output_tokens = 1000L),
    audit_context = list(provider = "mock", resolved_model = "m")
  ))
})

test_that("strict reconciliation uses locked rates instead of catalog estimates", {
  budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = fixed_rate_fixture(),
    pricing_source = "organization", max_output_tokens = 1000L
  )
  request <- budget_request_fixture(max_output_tokens = 1000L)
  reservation <- reserve_usage_request(
    budget, request,
    audit_context = list(provider = "mock", resolved_model = "m")
  )
  response <- usage_good_response()
  attr(response, "usage") <- list(
    input_tokens = 100, output_tokens = 20, cached_input_tokens = 0,
    cache_write_tokens = 0, reasoning_tokens = 0
  )
  attr(response, "cost_usd") <- 0.000001
  attr(response, "cost_status") <- "catalog_estimate"
  normalized <- normalize_provider_response(
    response, request = request, provider = "mock"
  )

  row <- reconcile_usage_request(budget, reservation, normalized)

  expect_identical(row$cost_status, "contract_estimate")
  expect_identical(row$rate_source, "organization-contract")
  expect_equal(row$per_call_amount, 0.0003)
  expect_equal(budget$estimated_amount, 0.0003)
})

test_that("cross-turn grand-total mismatches remain auditable and incomplete", {
  # Two opposite deltas cancel numerically, but each raw turn remains
  # inconsistent and the aggregate must not be relabeled as consistent.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  budget <- new_usage_budget(
    mode = "observe", rates = fixed_rate_fixture(),
    pricing_source = "organization"
  )
  request <- budget_request_fixture(max_output_tokens = 1000L)
  reservation <- reserve_usage_request(
    budget, request,
    audit_context = list(provider = "mock", resolved_model = "m")
  )
  turn <- function(input, total) ellmer::AssistantTurn(
    "answer",
    json = list(usage = list(
      inputTokens = input,
      cacheReadInputTokens = 0L,
      cacheWriteInputTokens = 0L,
      outputTokens = 10L,
      output_tokens_details = list(reasoning_tokens = 0L),
      totalTokens = total
    )),
    finish_reason = "stop"
  )
  turns <- list(turn(50L, 55L), turn(40L, 55L))
  usage <- ellmer_usage(list(
    get_turns = function() turns,
    last_turn = function() turns[[2L]]
  ))
  response <- new_llm_response(
    status = "completed", action = "final", data = list(ok = TRUE),
    request = request, resolved_model = "m", usage = usage,
    provider = "mock"
  )

  row <- reconcile_usage_request(budget, reservation, response)

  expect_identical(row$total_accounting_status, "mismatch")
  expect_identical(row$total_accounting_delta_tokens, 0)
  expect_identical(row$cost_status, "incomplete_estimate")
})

test_that("locked rates price disjoint provider cache and reasoning buckets", {
  # Break caught: treating provider totals as ordinary tokens double-prices
  # cached reads, cache writes, and reasoning while also hiding cache hit rate.
  budget <- new_usage_budget(
    mode = "observe", rates = partitioned_rate_fixture(),
    pricing_source = "external"
  )
  request <- llm_request(
    messages = list(list(role = "user", content = "ping")),
    tier = "frontier", model = "reasoning-model"
  )
  reservation <- reserve_usage_request(
    budget, request,
    audit_context = list(
      provider = "provider-a", resolved_model = "reasoning-model"
    )
  )
  usage <- list(
    input_tokens = 500,
    output_tokens = 60,
    total_input_tokens = 1000,
    total_output_tokens = 100,
    total_tokens = 1100,
    cached_input_tokens = 200,
    cache_write_tokens = 300,
    reasoning_tokens = 40,
    tool_charges = 0
  )
  attr(usage, "token_provenance") <-
    "ellmer public AssistantTurn@json usage"
  attr(usage, "input_accounting_status") <- "consistent"
  attr(usage, "input_accounting_delta_tokens") <- 0
  response <- new_llm_response(
    status = "completed", action = "final", data = list(ok = TRUE),
    request = request, resolved_model = "reasoning-model",
    usage = usage, provider = "provider-a"
  )

  row <- reconcile_usage_request(budget, reservation, response)
  priced_response <- apply_reconciled_usage_cost(response, row)
  finalize_usage_run(budget, "completed")
  summary <- Filter(function(record) {
    identical(record$record_type, "run_summary")
  }, budget$records)[[1]]

  expect_equal(row$per_call_amount, 0.000299)
  expect_identical(row$total_input_tokens, 1000)
  expect_identical(row$total_output_tokens, 100)
  expect_identical(row$total_tokens, 1100)
  expect_identical(row$cache_read_rate, 0.2)
  expect_identical(row$input_accounting_status, "consistent")
  expect_identical(row$input_accounting_delta_tokens, 0)
  expect_identical(
    row$raw_usage_provenance,
    "ellmer public AssistantTurn@json usage"
  )
  expect_identical(row$cost_provenance, "organization pricing table")
  expect_identical(
    priced_response$cost$provenance,
    "organization pricing table"
  )
  expect_identical(summary$total_input_tokens, 1000)
  expect_identical(summary$total_output_tokens, 100)
  expect_identical(summary$total_tokens, 1100)
  expect_identical(summary$cache_read_rate, 0.2)
})

test_that("strict tool reservations reconcile to the tool actually requested", {
  rates <- fixed_rate_fixture()
  rates[[1]]$tool_rates <- list(echo = 0.25)
  budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = rates,
    pricing_source = "organization", max_output_tokens = 1000L
  )
  request <- budget_request_fixture(max_output_tokens = 1000L)
  request$tools <- list(list(name = "echo"))
  context <- list(
    provider = "mock", resolved_model = "m", max_tool_calls = 2L
  )
  reservation <- reserve_usage_request(budget, request, context)
  expect_true(reservation$amount > 0.5)
  response <- usage_good_response()
  attr(response, "usage") <- list(
    input_tokens = 100, output_tokens = 20, cached_input_tokens = 0,
    cache_write_tokens = 0, reasoning_tokens = 0
  )
  normalized <- normalize_provider_response(
    response, request = request, provider = "mock"
  )

  row <- reconcile_usage_request(budget, reservation, normalized)

  expect_equal(row$per_call_amount, 0.0003)
  expect_equal(budget$reserved_amount, 0)

  bad_rates <- fixed_rate_fixture()
  bad_rates[[1]]$tool_rates <- list(echo = -1)
  bad_budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = bad_rates,
    pricing_source = "organization", max_output_tokens = 1000L
  )
  expect_false(can_reserve_request(bad_budget, request, context))
})

test_that("strict rate matching uses the tier-resolved request model", {
  seen_model <- NULL
  tier_capabilities <- function(tier = "frontier", model = NULL) {
    llm_capabilities(
      structured_output = "native", max_output_tokens = "supported",
      provider = "mock", model = "cheap-model", source = "test"
    )
  }
  llm <- new_llm(function(request) {
    seen_model <<- request$model
    response <- usage_good_response()
    attr(response, "usage") <- list(
      input_tokens = 10, output_tokens = 5, cached_input_tokens = 0,
      cache_write_tokens = 0, reasoning_tokens = 0
    )
    normalize_provider_response(
      response, request = request, provider = "mock"
    )
  }, provider = "mock", model = "base-model",
  capabilities_for = tier_capabilities)
  rates <- fixed_rate_fixture()
  rates[[1]]$resolved_model <- "cheap-model"
  budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = rates,
    pricing_source = "organization", max_output_tokens = 100L
  )

  result <- run_agent(
    usage_spec_min(), llm, list(), "translate",
    log_dir = withr::local_tempdir(), usage_budget = budget
  )

  expect_identical(result$status, "ok")
  expect_identical(seen_model, "cheap-model")
})

test_that("strict rate matching inherits the adapter region identity", {
  calls <- 0L
  capabilities <- llm_capabilities(
    structured_output = "native", max_output_tokens = "supported",
    provider = "mock", model = "m", source = "test"
  )
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    response <- usage_good_response()
    attr(response, "usage") <- list(
      input_tokens = 10, output_tokens = 5, cached_input_tokens = 0,
      cache_write_tokens = 0, reasoning_tokens = 0
    )
    normalize_provider_response(
      response, request = request, provider = "mock"
    )
  }, provider = "mock", model = "m", capabilities = capabilities)
  attr(llm, "auth_context") <- list(region = "us-west-2")
  rates <- fixed_rate_fixture()
  rates[[1]]$region <- "us-west-2"
  budget <- new_usage_budget(
    mode = "strict", max_usd = 1, rates = rates,
    pricing_source = "organization", max_output_tokens = 100L
  )

  result <- attempt_llm_request(
    budget_request_fixture(max_output_tokens = 100L), llm, budget
  )

  expect_identical(result$status, "completed")
  expect_identical(calls, 1L)
})

test_that("request ceilings include tool and output-schema payloads", {
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(
      usage_good_response(), request = request, provider = "mock"
    )
  }, provider = "mock")
  request <- budget_request_fixture("x")
  request$tools <- list(list(
    name = "large_tool", description = strrep("tool contract ", 20),
    schema = list(type = "object", properties = list())
  ))
  request$output_schema <- list(
    type = "object", description = strrep("output contract ", 20)
  )
  budget <- new_usage_budget(mode = "observe", max_request_chars = 100L)

  result <- attempt_llm_request(request, llm, budget)

  expect_identical(result$status, "budget_exhausted")
  expect_identical(calls, 0L)
})

test_that("known soft-budget exhaustion prevents a schema retry", {
  bad <- list(type = "final", data = list(assumptions = list()))
  attr(bad, "cost_usd") <- 0.01
  attr(bad, "cost_status") <- "billed_amount"
  good_cost <- usage_good_response()
  attr(good_cost, "cost_usd") <- 0.01
  attr(good_cost, "cost_status") <- "billed_amount"
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(
      list(bad, good_cost)[[calls]], request = request, provider = "mock"
    )
  }, provider = "mock")
  budget <- new_usage_budget(mode = "soft", max_usd = 0.01)

  result <- run_agent(
    usage_spec_min(retries = 1L), llm, list(), "translate",
    log_dir = withr::local_tempdir(), usage_budget = budget
  )

  expect_identical(result$status, "budget_exhausted")
  expect_identical(calls, 1L)
  expect_equal(result$known_cost_usd, 0.01)
})

test_that("tool continuation also checks the shared request gate", {
  tool_response <- list(type = "tool", tool = "echo", args = list(x = 1))
  attr(tool_response, "cost_usd") <- 0.01
  attr(tool_response, "cost_status") <- "billed_amount"
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    normalize_provider_response(
      list(tool_response, usage_good_response())[[calls]],
      request = request, provider = "mock"
    )
  }, provider = "mock")
  tools <- list(echo = make_tool(
    "echo", function(args) args, 2L,
    schema = list(
      type = "object", properties = list(x = list(type = "number")),
      required = "x", additionalProperties = FALSE
    )
  ))
  budget <- new_usage_budget(mode = "soft", max_usd = 0.01)

  result <- run_agent(
    usage_spec_min(), llm, tools, "translate",
    log_dir = withr::local_tempdir(), usage_budget = budget
  )

  expect_identical(result$status, "budget_exhausted")
  expect_identical(calls, 1L)
})

test_that("capability downgrade cannot bypass the per-transport request gate", {
  calls <- 0L
  capabilities <- llm_capabilities(
    structured_output = "native", temperature = "supported",
    source = "test"
  )
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(
      request, capabilities,
      function(request, params) {
        calls <<- calls + 1L
        if (calls == 1L) {
          stop(llm_optional_parameter_error("temperature"))
        }
        normalize_provider_response(
          usage_good_response(), request = request, provider = "mock"
        )
      }
    )
  }, provider = "mock", capabilities = capabilities)
  llm <- with_usage_managed_request(llm)
  budget <- new_usage_budget(mode = "observe", max_calls = 1L)
  request <- budget_request_fixture()
  request$parameters$temperature <- 0

  result <- attempt_llm_request(
    request, llm, budget,
    list(agent = "translator", purpose = "parameter_negotiation")
  )

  expect_identical(result$status, "budget_exhausted")
  expect_identical(calls, 1L)
  expect_identical(budget$request_count, 1L)
})

test_that("explicit billed cost provenance survives normalization", {
  raw <- usage_good_response()
  attr(raw, "cost_usd") <- 0.25
  attr(raw, "cost_status") <- "billed_amount"
  attr(raw, "cost_currency") <- "USD"
  attr(raw, "cost_source") <- "provider invoice export"

  response <- normalize_provider_response(raw, provider = "mock")

  expect_identical(response$cost$status, "billed_amount")
  expect_identical(response$cost$currency, "USD")
  expect_identical(response$cost$source, "provider invoice export")

  attr(raw, "cost_currency") <- "EUR"
  incompatible <- normalize_provider_response(raw, provider = "mock")
  expect_identical(incompatible$cost$status, "unknown")
  expect_true(is.na(incompatible$cost$amount_usd))

  attr(raw, "cost_currency") <- "USD"
  attr(raw, "cost_usd") <- -0.25
  negative <- normalize_provider_response(raw, provider = "mock")
  expect_identical(negative$cost$status, "unknown")
  expect_true(is.na(negative$cost$amount_usd))
})

test_that("probe retries use the same gate and usage row schema", {
  calls <- 0L
  llm <- new_llm(function(request) {
    calls <<- calls + 1L
    if (calls == 1L) {
      stop(structure(
        list(message = "temporary network failure", call = NULL),
        class = c("sas2r_llm_transport_error", "error", "condition")
      ))
    }
    normalize_provider_response(
      list(type = "final", data = list(ok = TRUE)),
      request = request, provider = "mock"
    )
  }, provider = "mock")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  budget <- new_usage_budget(
    mode = "observe", max_calls = 1L,
    ledger_path = path, run_id = "run_probe"
  )

  result <- sas_llm_probe(
    llm, max_retries = 2L, log_dir = dir, usage_budget = budget
  )

  expect_false(result)
  expect_identical(calls, 1L)
  rows <- read_usage_ledger(path)
  completed <- rows[rows$record_type == "request_completed", , drop = FALSE]
  expect_equal(nrow(completed), 1L)
  expect_identical(completed$agent, "probe")
  expect_identical(completed$purpose, "probe")
  expect_equal(completed$attempt, 1L)
})

test_that("cache-write reasoning and tool dimensions survive normalization", {
  raw <- usage_good_response()
  attr(raw, "usage") <- list(
    input_tokens = 10, output_tokens = 5, cached_input_tokens = 3,
    cache_write_tokens = 2, reasoning_tokens = 1, tool_charges = 0.004
  )

  response <- normalize_provider_response(raw, provider = "mock")

  expect_equal(response$usage$cached_input_tokens, 3)
  expect_equal(response$usage$cache_write_tokens, 2)
  expect_equal(response$usage$reasoning_tokens, 1)
  expect_equal(response$usage$tool_charges, 0.004)

  attr(raw, "usage")$input_tokens <- -1
  invalid <- normalize_provider_response(raw, provider = "mock")
  expect_true(is.na(invalid$usage$input_tokens))
})

test_that("request ledger rows share identity fields and contain no content", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  budget <- new_usage_budget(
    mode = "observe", ledger_path = path, run_id = "run_identity"
  )
  llm <- new_llm(function(request) {
    response <- usage_good_response()
    attr(response, "usage") <- list(input_tokens = 2, output_tokens = 1)
    normalize_provider_response(response, request = request, provider = "mock")
  }, provider = "mock")

  attempt_llm_request(
    budget_request_fixture("patient content", request_id = "req_identity"),
    llm, budget,
    list(agent = "translator", purpose = "translation", tier = "frontier")
  )

  rows <- read_usage_ledger(path)
  completed <- rows[rows$record_type == "request_completed", , drop = FALSE]
  required <- c(
    "record_type", "run_id", "request_id", "provider",
    "requested_model", "resolved_model", "agent", "tier", "purpose",
    "attempt", "status", "cost_status", "cumulative_amount"
  )
  expect_true(all(required %in% names(completed)))
  expect_false(anyNA(completed$request_id))
  expect_false(any(grepl(
    "patient content", readLines(path, warn = FALSE), fixed = TRUE
  )))
  expect_false(any(c("messages", "response", "content") %in% names(completed)))
})

test_that("run summary is idempotent and reconstructs metadata-only totals", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  budget <- new_usage_budget(
    mode = "observe", ledger_path = path, run_id = "run_summary"
  )
  llm <- new_llm(function(request) {
    stop(structure(
      list(message = "network down", call = NULL),
      class = c("sas2r_llm_transport_error", "error", "condition")
    ))
  }, provider = "mock")
  attempt_llm_request(
    budget_request_fixture(request_id = "req_failed"), llm, budget,
    list(agent = "translator", purpose = "translation")
  )

  finalize_usage_run(budget, "failed")
  finalize_usage_run(budget, "failed")
  rows <- read_usage_ledger(path)
  summary <- rows[
    rows$record_type == "run_summary" & rows$run_id == "run_summary",
    , drop = FALSE
  ]

  expect_equal(nrow(summary), 1L)
  expect_identical(summary$terminal_status, "failed")
  expect_true(summary$request_count >= 1L)
  expect_true(summary$cost_unknown)
})

test_that("strict capability downgrade cannot remove the effective output cap", {
  calls <- 0L
  seen_params <- list()
  capabilities <- llm_capabilities(
    structured_output = "native", max_output_tokens = "supported",
    provider = "mock", model = "m", source = "test"
  )
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(
      request, capabilities,
      function(request, params) {
        calls <<- calls + 1L
        seen_params[[calls]] <<- params
        if (calls == 1L) {
          stop(llm_optional_parameter_error("max_output_tokens"))
        }
        normalize_provider_response(
          usage_good_response(), request = request, provider = "mock"
        )
      }
    )
  }, provider = "mock", model = "m", capabilities = capabilities)
  llm <- with_usage_managed_request(llm)
  budget <- new_usage_budget(
    mode = "strict", max_usd = 10, rates = fixed_rate_fixture(),
    pricing_source = "organization", max_output_tokens = 100L
  )

  result <- attempt_llm_request(
    budget_request_fixture(max_output_tokens = 100L), llm, budget
  )

  expect_identical(result$status, "budget_exhausted")
  expect_identical(calls, 1L)
  expect_equal(seen_params[[1]]$max_output_tokens, 100L)
  expect_identical(budget$request_count, 1L)
})

test_that("external pricing keeps adapter dollar metadata unknown without rates", {
  for (status in c("catalog_estimate", "billed_amount")) {
    response <- usage_good_response()
    attr(response, "cost_usd") <- 0.25
    attr(response, "cost_status") <- status
    attr(response, "cost_source") <- "adapter dollar metadata"
    llm <- new_llm(function(request) {
      normalize_provider_response(
        response, request = request, provider = "mock"
      )
    }, provider = "mock")
    budget <- new_usage_budget(
      mode = "observe", pricing_source = "external"
    )

    attempt_llm_request(budget_request_fixture(), llm, budget)

    completed <- Filter(function(record) {
      identical(record$record_type, "request_completed")
    }, budget$records)[[1]]
    expect_equal(budget$known_amount, 0, info = status)
    expect_equal(budget$billed_amount, 0, info = status)
    expect_equal(budget$estimated_amount, 0, info = status)
    expect_identical(completed$cost_status, "unknown", info = status)
    expect_true(is.na(completed$per_call_amount), info = status)
  }
})

test_that("usage persistence canonicalizes endpoints and provenance metadata", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  endpoint_secret <- "endpoint-secret-7e9a"
  provenance_secret <- "provenance-secret-2b4c"
  version_secret <- "version-secret-6d1f"
  llm <- new_llm(function(request) {
    response <- usage_good_response()
    attr(response, "cost_usd") <- 0.25
    attr(response, "cost_status") <- "catalog_estimate"
    attr(response, "cost_source") <- paste0(
      "https://billing-user:billing-pass@billing.example/rates?token=",
      provenance_secret
    )
    attr(response, "cost_source_version") <- paste0(
      "api_key=", version_secret
    )
    normalize_provider_response(
      response, request = request, provider = "custom"
    )
  }, provider = "custom", model = "m", endpoint = paste0(
    "https://api-user:api-pass@example.com/v1?api_key=", endpoint_secret,
    "#fragment"
  ))
  attr(llm, "auth_context") <- list(endpoint = llm$endpoint)
  budget <- new_usage_budget(
    mode = "observe", ledger_path = path, run_id = "run_redaction"
  )

  attempt_llm_request(budget_request_fixture(), llm, budget)

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl(endpoint_secret, text, fixed = TRUE))
  expect_false(grepl(provenance_secret, text, fixed = TRUE))
  expect_false(grepl(version_secret, text, fixed = TRUE))
  expect_false(grepl("api-user|api-pass|billing-user|billing-pass", text))
  expect_false(grepl("[?]api_key=|[?]token=|#fragment", text))
  rows <- read_usage_ledger(path)
  expect_true(all(rows$endpoint[!is.na(rows$endpoint)] ==
                    "https://example.com/v1"))
})

test_that("strict reconciliation prices tools executed inside a final transport", {
  ledger_path <- file.path(withr::local_tempdir(), "usage.jsonl")
  rates <- fixed_rate_fixture()
  rates[[1]]$tool_rates <- list(internal_lookup = 0.25)
  capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported",
    max_output_tokens = "supported", provider = "mock", model = "m",
    source = "test"
  )
  llm <- new_llm(function(request) {
    request$tools[[1]]$call(list())
    response <- usage_good_response()
    attr(response, "usage") <- list(
      input_tokens = 0, output_tokens = 0, cached_input_tokens = 0,
      cache_write_tokens = 0, reasoning_tokens = 0, tool_charges = 0
    )
    normalize_provider_response(
      response, request = request, provider = "mock"
    )
  }, provider = "mock", model = "m", capabilities = capabilities)
  tools <- list(internal_lookup = make_tool(
    "internal_lookup", function(args) list(found = TRUE), 1L,
    schema = list(
      type = "object", properties = list(), additionalProperties = FALSE
    )
  ))
  budget <- new_usage_budget(
    mode = "strict", max_usd = 2, rates = rates,
    pricing_source = "organization", max_output_tokens = 100L,
    max_tool_calls = 1L, ledger_path = ledger_path
  )

  result <- run_agent(
    usage_spec_min(tool_limit = 1L), llm, tools, "translate",
    log_dir = withr::local_tempdir(), usage_budget = budget
  )

  expect_identical(result$status, "ok")
  expect_equal(budget$tool_count, 1L)
  expect_equal(budget$estimated_amount, 0.25)
  completed <- Filter(function(record) {
    identical(record$record_type, "request_completed")
  }, budget$records)[[1]]
  expect_equal(completed$per_call_amount, 0.25)
  ledger_text <- paste(readLines(ledger_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("internal_lookup", ledger_text, fixed = TRUE))
  expect_match(ledger_text, "tool_sha256_", fixed = TRUE)
})

test_that("resume reconstructs the durable tool-call ceiling", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  first <- new_usage_budget(
    ledger_path = path, run_id = "run_tools_first", max_tool_calls = 1L
  )
  reserve_usage_tool_call(first, "lookup_rulebook")
  finalize_usage_run(first, "completed")

  resumed <- load_usage_budget(
    path, run_id = "run_tools_resumed", max_tool_calls = 1L
  )

  expect_equal(resumed$tool_count, 1L)
  expect_error(
    reserve_usage_tool_call(resumed, "lookup_rulebook"),
    class = "sas2r_budget_exhausted"
  )
})

test_that("resume counts an attempted tool call even if the process died mid-call", {
  # Break caught: lifecycle instrumentation writes the attempt before running
  # local code; resume must not forget that durable reservation merely because
  # no terminal tool record or run summary was written before the crash.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  first <- new_usage_budget(
    ledger_path = path, run_id = "run_tool_interrupted", max_tool_calls = 1L
  )
  reserve_usage_tool_call(first, "get_macro_source", list(name = "helper"))

  resumed <- load_usage_budget(
    path, run_id = "run_tool_after_restart", max_tool_calls = 1L
  )

  expect_identical(resumed$tool_count, 1L)
  expect_length(resumed$tool_events, 1L)
  expect_identical(resumed$tool_events[[1L]]$record_type, "tool_attempted")
})

test_that("a global tool ceiling records demand without admitting the call", {
  # A denied model request is still demand. It needs a durable attempted and
  # refused pair, but it must not consume the admitted-call ceiling on resume.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  budget <- new_usage_budget(
    ledger_path = path, run_id = "run_global_refusal", max_tool_calls = 0L
  )

  expect_error(
    reserve_usage_tool_call(
      budget, "get_macro_source", list(name = "helper"),
      audit_context = list(request_id = "req_denied", unit_id = 29L)
    ),
    class = "sas2r_budget_exhausted"
  )

  expect_identical(budget$tool_request_count, 1L)
  expect_identical(budget$tool_count, 0L)
  expect_identical(budget$tool_refused_count, 1L)
  attempted <- Filter(
    function(record) identical(record$record_type, "tool_attempted"),
    budget$records
  )[[1L]]
  refused <- Filter(
    function(record) identical(record$record_type, "tool_refused"),
    budget$records
  )[[1L]]
  expect_false(attempted$admitted)
  expect_identical(refused$tool_event_id, attempted$tool_event_id)
  expect_identical(refused$result_status, "global_tool_limit")
  summary <- finalize_usage_run(budget, "completed")
  expect_identical(summary$tool_request_count, 1L)
  expect_identical(summary$tool_count, 0L)
  expect_identical(summary$tool_refused_count, 1L)

  resumed <- load_usage_budget(
    path, run_id = "run_after_refusal", max_tool_calls = 1L
  )
  expect_identical(resumed$tool_request_count, 1L)
  expect_identical(resumed$tool_count, 0L)
  expect_no_error(reserve_usage_tool_call(resumed, "get_macro_source"))
  expect_identical(resumed$tool_request_count, 2L)
  expect_identical(resumed$tool_count, 1L)
})

test_that("failed atomic summary write is retryable and never memory-only", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  budget <- new_usage_budget(
    ledger_path = path, run_id = "run_summary_retry"
  )
  original_atomic_write <- atomic_write_file
  calls <- 0L
  testthat::local_mocked_bindings(
    atomic_write_file = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) stop("injected atomic summary failure")
      original_atomic_write(...)
    },
    .package = "sas2r"
  )

  expect_error(
    finalize_usage_run(budget, "failed"),
    "injected atomic summary failure"
  )
  expect_false(budget$summary_written)
  expect_length(budget$records, 0L)
  expect_false(file.exists(path))

  finalize_usage_run(budget, "failed")
  finalize_usage_run(budget, "failed")
  rows <- read_usage_ledger(path)
  summary <- rows[rows$record_type == "run_summary", , drop = FALSE]
  expect_equal(nrow(summary), 1L)
  expect_identical(summary$terminal_status, "failed")
})

test_that("external unknown reconciliation reaches runner callback and audit", {
  raw <- usage_good_response()
  attr(raw, "cost_usd") <- 0.25
  attr(raw, "cost_status") <- "billed_amount"
  attr(raw, "cost_currency") <- "USD"
  capabilities <- llm_capabilities(
    structured_output = "native", source = "test"
  )
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(
      request, capabilities, function(request, params) {
        normalize_provider_response(raw, request = request, provider = "mock")
      }
    )
  }, provider = "mock", capabilities = capabilities)
  llm <- with_usage_managed_request(llm)
  dir <- withr::local_tempdir()
  charged <- numeric()
  budget <- new_usage_budget(
    mode = "observe", pricing_source = "external",
    ledger_path = file.path(dir, "usage.jsonl")
  )

  result <- run_agent(
    usage_spec_min(), llm, list(), "translate", log_dir = dir,
    on_charge = function(amount) charged <<- c(charged, amount),
    usage_budget = budget
  )

  expect_identical(result$status, "ok")
  expect_true(result$cost_unknown)
  expect_equal(result$known_cost_usd, 0)
  expect_equal(result$spend_usd, 0)
  expect_length(charged, 1L)
  expect_true(is.na(charged[[1]]))
  audit <- jsonlite::fromJSON(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)[[1]],
    simplifyVector = FALSE
  )
  expect_identical(audit$cost_status, "unknown")
  expect_null(audit$cost_usd)
})

test_that("external unknown reconciliation reaches probe callback and audit", {
  raw <- list(type = "final", data = list(ok = TRUE))
  attr(raw, "cost_usd") <- 0.50
  attr(raw, "cost_status") <- "catalog_estimate"
  attr(raw, "cost_currency") <- "USD"
  capabilities <- llm_capabilities(
    structured_output = "native", source = "test"
  )
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(
      request, capabilities, function(request, params) {
        normalize_provider_response(raw, request = request, provider = "mock")
      }
    )
  }, provider = "mock", capabilities = capabilities)
  llm <- with_usage_managed_request(llm)
  dir <- withr::local_tempdir()
  charged <- numeric()
  budget <- new_usage_budget(
    mode = "observe", pricing_source = "external",
    ledger_path = file.path(dir, "usage.jsonl")
  )

  result <- sas_llm_probe(
    llm, max_retries = 1L, log_dir = dir,
    on_charge = function(amount) charged <<- c(charged, amount),
    usage_budget = budget
  )

  expect_true(result)
  expect_length(charged, 1L)
  expect_true(is.na(charged[[1]]))
  audit <- jsonlite::fromJSON(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)[[1]],
    simplifyVector = FALSE
  )
  expect_identical(audit$cost_status, "unknown")
  expect_null(audit$cost_usd)
  expect_equal(audit$cumulative_spend_usd, 0)
})

test_that("usage JSONL redacts colon and equals provenance credentials", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  password_secret <- "colon-password-secret-83bc"
  token_secret <- "colon-token-secret-42ad"
  key_secret <- "equals-key-secret-11fe"
  raw <- usage_good_response()
  attr(raw, "cost_usd") <- 0.25
  attr(raw, "cost_status") <- "catalog_estimate"
  attr(raw, "cost_source") <- paste0(
    "provider catalog; password: ", password_secret,
    "; access_token: ", token_secret
  )
  attr(raw, "cost_provenance") <- paste0("api_key=", key_secret)
  llm <- new_llm(function(request) {
    normalize_provider_response(raw, request = request, provider = "mock")
  }, provider = "mock")
  budget <- new_usage_budget(
    mode = "observe", pricing_source = "adapter", ledger_path = path
  )

  attempt_llm_request(budget_request_fixture(), llm, budget)

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  for (secret in c(password_secret, token_secret, key_secret)) {
    expect_false(grepl(secret, text, fixed = TRUE), info = secret)
  }
  expect_match(text, "password: [REDACTED]", fixed = TRUE)
  expect_match(text, "access_token: [REDACTED]", fixed = TRUE)
  expect_match(text, "api_key=[REDACTED]", fixed = TRUE)
})

test_that("usage provenance redacts complete quoted values without trailing loss", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  json_secret <- "json-quoted-secret-5c31"
  spaced_secret <- "double quoted secret value 7a42"
  single_secret <- "single quoted secret value 9d63"
  equals_secret <- "equals-secret-b814"
  raw <- usage_good_response()
  attr(raw, "cost_usd") <- 0.25
  attr(raw, "cost_status") <- "catalog_estimate"
  attr(raw, "cost_source") <- paste0(
    "prefix {\"password\":\"", json_secret,
    "\",\"note\":\"keep-json-tail\"}; password: \"", spaced_secret,
    "\" keep-double-tail; access_token: '", single_secret,
    "' keep-single-tail; api_key=", equals_secret, "; keep-equals-tail"
  )
  attr(raw, "cost_provenance") <- attr(raw, "cost_source")
  llm <- new_llm(function(request) {
    normalize_provider_response(raw, request = request, provider = "mock")
  }, provider = "mock")
  budget <- new_usage_budget(
    mode = "observe", pricing_source = "adapter", ledger_path = path
  )

  response <- attempt_llm_request(budget_request_fixture(), llm, budget)

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  projected <- paste(
    response$cost$source %||% "", response$cost$provenance %||% ""
  )
  for (secret in c(
    json_secret, spaced_secret, single_secret, equals_secret
  )) {
    expect_false(grepl(secret, text, fixed = TRUE), info = secret)
    expect_false(grepl(secret, projected, fixed = TRUE), info = secret)
  }
  for (tail in c(
    "keep-json-tail", "keep-double-tail", "keep-single-tail",
    "keep-equals-tail"
  )) {
    expect_match(text, tail, fixed = TRUE)
    expect_match(projected, tail, fixed = TRUE)
  }
  expect_match(projected, "password\":\"[REDACTED]\"", fixed = TRUE)
  expect_match(projected, "password: \"[REDACTED]\"", fixed = TRUE)
  expect_match(projected, "access_token: '[REDACTED]'", fixed = TRUE)
  expect_match(projected, "api_key=[REDACTED]", fixed = TRUE)
})

test_that("the outgoing request carries no audit context and no redactor closure", {
  # Built the way ellmer_llm() builds it: the shipped adapter always ends in
  # with_usage_managed_request(), so the managed branch -- not the plain one --
  # is the path every production request takes. A plain new_llm() here let the
  # managed branch staple an auth-bearing closure onto the outgoing request
  # while this test stayed green.
  capabilities <- llm_capabilities(
    structured_output = "native", temperature = "supported", source = "test")
  seen_adapter <- NULL
  seen_transport <- NULL
  llm <- new_llm(function(request) {
    seen_adapter <<- request
    invoke_with_capability_retry(
      request, capabilities,
      function(request, params) {
        seen_transport <<- request
        normalize_provider_response(
          usage_good_response(), request = request, provider = "mock")
      }
    )
  }, provider = "mock", model = "m", capabilities = capabilities,
  redaction_secrets = "fixture-key-9f21")
  attr(llm, "is_ellmer") <- TRUE
  llm <- with_usage_managed_request(llm)
  attr(llm, "auth_context") <- list(endpoint = "https://example.invalid")
  expect_true(isTRUE(attr(llm, "usage_managed_request", exact = TRUE)))

  budget <- new_usage_budget()
  response <- attempt_llm_request(
    llm_request(messages = list(list(role = "user", content = "ping"))),
    llm, budget, list(purpose = "translation")
  )
  for (seen in list(seen_adapter, seen_transport, response$request)) {
    expect_null(seen$audit_context)
    expect_null(seen$purpose)
    expect_null(seen$.usage_attempt_callback)
    expect_false(any(vapply(seen, is.function, logical(1))))
  }
  # the ledger still records the purpose it was given
  completed <- Filter(function(r) identical(r$record_type, "request_completed"),
                      budget$records)
  expect_identical(completed[[1]]$purpose, "translation")
})

test_that("the managed adapter still meters every capability-retry attempt", {
  # The callback moved off the request, so prove the metering it carries did
  # not move off with it: a downgraded retry must still reserve and reconcile.
  capabilities <- llm_capabilities(
    structured_output = "native", temperature = "supported", source = "test")
  attempts <- list()
  llm <- new_llm(function(request) {
    invoke_with_capability_retry(
      request, capabilities,
      function(request, params) {
        attempts[[length(attempts) + 1L]] <<- request
        if (length(attempts) == 1L) stop(llm_optional_parameter_error("temperature"))
        normalize_provider_response(
          usage_good_response(), request = request, provider = "mock")
      }
    )
  }, provider = "mock", model = "m", capabilities = capabilities)
  attr(llm, "is_ellmer") <- TRUE
  llm <- with_usage_managed_request(llm)

  budget <- new_usage_budget()
  request <- budget_request_fixture()
  request$parameters$temperature <- 0
  response <- attempt_llm_request(
    request, llm, budget, list(purpose = "translation"))

  expect_identical(response$status, "completed")
  expect_length(attempts, 2L)
  for (attempt in attempts) {
    expect_null(attempt$.usage_attempt_callback)
    expect_false(any(vapply(attempt, is.function, logical(1))))
  }
  started <- Filter(function(r) identical(r$record_type, "request_started"),
                    budget$records)
  expect_length(started, 2L)
})

test_that("an adapter that asks for the audit context is handed it alongside the request", {
  seen <- NULL
  llm <- new_llm(function(request, audit_context = list()) {
    seen <<- audit_context
    list(status = "completed", action = "final", data = list(ok = TRUE))
  }, provider = "mock", model = "m")
  attempt_llm_request(
    llm_request(messages = list(list(role = "user", content = "ping"))),
    llm, new_usage_budget(), list(purpose = "reviewer")
  )
  expect_identical(seen$purpose, "reviewer")
  # the redactor closes over the auth-bearing adapter and never travels
  expect_null(seen$.usage_redactor)
  expect_false(any(vapply(seen, is.function, logical(1))))
})

test_that("the wall-time ceiling denies a request once elapsed time passes it", {
  budget <- new_usage_budget(mode = "soft", max_wall_time = 30)
  # Reach the ceiling by ageing the budget, not by sleeping through it.
  budget$start_time <- Sys.time() - 31

  quote <- usage_reservation_quote(budget, budget_request_fixture())

  expect_false(quote$ok)
  expect_identical(quote$error$reason, "wall_time")
})

test_that("an unreached wall-time ceiling admits the request", {
  budget <- new_usage_budget(mode = "soft", max_wall_time = 30)

  expect_true(usage_reservation_quote(budget, budget_request_fixture())$ok)
})

test_that("the retry ceiling denies a retry but never a first attempt", {
  budget <- new_usage_budget(mode = "soft", max_retries = 2)
  budget$retry_count <- 2L
  request <- budget_request_fixture()

  expect_true(usage_reservation_quote(budget, request)$ok)

  retry <- usage_reservation_quote(
    budget, request, audit_context = list(retry_of = "req_earlier")
  )
  expect_false(retry$ok)
  expect_identical(retry$error$reason, "retries")
})

test_that("the call ceiling denies once the request count reaches it", {
  budget <- new_usage_budget(mode = "soft", max_calls = 5)
  budget$request_count <- 5L

  quote <- usage_reservation_quote(budget, budget_request_fixture())

  expect_false(quote$ok)
  expect_identical(quote$error$reason, "calls")
})

test_that("a ledger record names the unit the request was for", {
  # Without this the audit trail cannot answer "which unit cost 57k tokens?" --
  # the only way to identify it was to reconstruct the agent queue offline.
  budget <- new_usage_budget(mode = "observe")
  request <- budget_request_fixture()

  record <- usage_audit_identity(
    budget, request,
    audit_context = list(purpose = "translation", unit_id = 29L),
    record_type = "request_started", status = "started", attempt = 1L
  )

  expect_identical(record$unit_id, 29L)
})

test_that("a request with no unit carries no unit id rather than a placeholder", {
  budget <- new_usage_budget(mode = "observe")

  record <- usage_audit_identity(
    budget, budget_request_fixture(),
    audit_context = list(purpose = "probe"),
    record_type = "request_started", status = "started", attempt = 1L
  )

  expect_null(record$unit_id)
})

test_that("a tool event records what the tool was asked for", {
  # The ledger recorded that a tool ran, never what it looked up. Across 61
  # units, 41% of calls were cookbook and rulebook lookups whose subject was
  # invisible, so there was no way to tell productive lookups from thrashing.
  budget <- new_usage_budget(mode = "observe")

  reserve_usage_tool_call(budget, tool_name = "get_macro_source",
                          arguments = list(name = "boxplot_each_param_tp"))

  event <- budget$tool_events[[1L]]
  expect_identical(event$tool_name, "get_macro_source")
  expect_match(event$tool_arguments, "boxplot_each_param_tp")
})

test_that("a tool called with no arguments records none", {
  budget <- new_usage_budget(mode = "observe")

  reserve_usage_tool_call(budget, tool_name = "read_unit_context")

  expect_null(budget$tool_events[[1L]]$tool_arguments)
})

test_that("recorded arguments are bounded so one call cannot bloat the ledger", {
  budget <- new_usage_budget(mode = "observe")
  source_body <- paste0("%macro leaked_body;", strrep("x", 5000), "%mend;")

  reserve_usage_tool_call(budget, tool_name = "search_docs",
                          arguments = list(q = source_body))

  recorded <- budget$tool_events[[1L]]$tool_arguments
  expect_lt(nchar(recorded), 400L)
  expect_false(grepl("leaked_body", recorded, fixed = TRUE))
  expect_false(grepl('"q"', recorded, fixed = TRUE))
  expect_match(recorded, "sha256", fixed = TRUE)
})

test_that("invalid model arguments never persist source bodies", {
  # Reservation intentionally precedes schema validation so a refusal is
  # durable. That ordering must not turn arbitrary extra model fields into a
  # source-code side channel in both lifecycle records.
  budget <- new_usage_budget(mode = "observe")
  state <- new_agent_tool_state(2L, budget)
  bound <- bind_transport_tool_limits(list(
    get_macro_source = make_tool(
      "get_macro_source", function(args) list(source = "unused"), 1L,
      schema = tool_argument_schema("get_macro_source")
    )
  ), state)
  leaked <- "%macro leaked_body; %put private; %mend;"

  expect_error(
    bound$get_macro_source$call(list(name = "helper", source = leaked)),
    class = "sas2r_tool_arguments_error"
  )

  serialized <- jsonlite::toJSON(budget$records, auto_unbox = TRUE)
  expect_false(grepl(leaked, serialized, fixed = TRUE))
  expect_false(grepl("leaked_body", serialized, fixed = TRUE))
  expect_match(serialized, "helper", fixed = TRUE)
  expect_match(serialized, "source", fixed = TRUE)
  expect_identical(budget$tool_refused_count, 1L)
})

test_that("unknown tool metadata is fingerprinted instead of persisted", {
  budget <- new_usage_budget(mode = "observe")
  reservation <- reserve_usage_tool_call(
    budget,
    tool_name = "private_tool_9988",
    arguments = list(private_account_id = 77123991, private_flag = TRUE)
  )
  complete_usage_tool_call(
    budget, reservation, outcome = "failed",
    result_status = "private_status_7788",
    error_class = "private_error_7788"
  )

  serialized <- jsonlite::toJSON(budget$records, auto_unbox = TRUE)
  for (private_value in c(
    "private_tool_9988", "private_account_id", "77123991",
    "private_flag", "private_status_7788", "private_error_7788"
  )) {
    expect_false(grepl(private_value, serialized, fixed = TRUE))
  }
  expect_match(serialized, "sha256", fixed = TRUE)
})

test_that("the bound tool wrapper forwards its arguments to the ledger", {
  budget <- new_usage_budget(mode = "observe")
  state <- new_agent_tool_state(5L, budget)
  bound <- bind_transport_tool_limits(
    list(find_macro = list(name = "find_macro", description = "d",
                           call = function(args) list(ok = TRUE),
                           schema = closed_tool_schema())),
    state
  )

  bound$find_macro$call(list(name = "utl_quantile"))

  expect_match(budget$tool_events[[1L]]$tool_arguments, "utl_quantile")
})

test_that("a per-tool refusal is not reported as an execution", {
  # Break caught: moving the ledger reservation ahead of tool$call() and
  # calling every reservation `tool_executed` makes a max_calls refusal look
  # indistinguishable from a macro body that was actually returned.
  budget <- new_usage_budget(mode = "observe")
  state <- new_agent_tool_state(3L, budget)
  bound <- bind_transport_tool_limits(
    list(source = make_tool(
      "source", function(args) list(source = "macro body"), max_calls = 1L,
      schema = closed_tool_schema()
    )),
    state
  )

  expect_identical(bound$source$call(list())$source, "macro body")
  expect_identical(bound$source$call(list())$error, "budget_exhausted")

  types <- vapply(budget$records, `[[`, "", "record_type")
  expect_identical(sum(types == "tool_executed"), 1L)
  expect_identical(sum(types == "tool_refused"), 1L)
})

test_that("request ledger records policy duration and safe terminal failure", {
  path <- file.path(withr::local_tempdir(), "usage.jsonl")
  policy <- list(
    timeout_scope = "http_attempt_absolute",
    timeout_seconds = 0.05,
    transport_max_tries = 1L
  )
  llm <- new_llm(
    function(request) simpleError("secret upstream text timed out"),
    provider = "mock", model = "fixture-model", request_policy = policy
  )
  budget <- new_usage_budget(ledger_path = path, run_id = "run_timeout")

  response <- attempt_llm_request(
    budget_request_fixture(), llm, usage_budget = budget
  )

  expect_identical(response$status, "failed")
  started <- Filter(
    function(record) identical(record$record_type, "request_started"),
    budget$records
  )[[1L]]
  completed <- Filter(
    function(record) identical(record$record_type, "request_completed"),
    budget$records
  )[[1L]]
  expect_identical(started[names(policy)], policy)
  expect_identical(completed[names(policy)], policy)
  expect_true(is.numeric(completed$duration_ms))
  expect_gte(completed$duration_ms, 0)
  expect_identical(completed$error_class, "sas2r_llm_timeout")
  expect_identical(completed$error_reason, "timed_out")
  expect_false(grepl(
    "secret upstream text", paste(readLines(path, warn = FALSE), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("successful request completion leaves failure fields null", {
  path <- file.path(withr::local_tempdir(), "usage.jsonl")
  policy <- list(
    timeout_scope = "http_attempt_absolute",
    timeout_seconds = 300,
    transport_max_tries = 1L
  )
  llm <- new_llm(
    function(request) usage_good_response(),
    provider = "mock", model = "fixture-model", request_policy = policy
  )
  budget <- new_usage_budget(ledger_path = path)

  response <- attempt_llm_request(
    budget_request_fixture(), llm, usage_budget = budget
  )
  completed <- Filter(
    function(record) identical(record$record_type, "request_completed"),
    budget$records
  )[[1L]]

  expect_identical(response$status, "completed")
  expect_true(all(c("error_class", "error_reason") %in% names(completed)))
  expect_null(completed$error_class)
  expect_null(completed$error_reason)
  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json, '"error_class":null', fixed = TRUE)
  expect_match(json, '"error_reason":null', fixed = TRUE)
})

test_that("request ledger rejects mismatched terminal failure pairs", {
  budget <- new_usage_budget()
  reservation <- reserve_usage_request(
    budget, budget_request_fixture(),
    audit_context = list(provider = "mock")
  )
  response <- new_llm_response(
    status = "failed", request = reservation$request, provider = "mock",
    error = list(
      class = "sas2r_llm_timeout",
      reason = "rate_limited",
      message = "untrusted upstream text"
    )
  )

  reconcile_usage_request(budget, reservation, response)
  completed <- Filter(
    function(record) identical(record$record_type, "request_completed"),
    budget$records
  )[[1L]]

  expect_identical(completed$error_class, "sas2r_llm_timeout")
  expect_null(completed$error_reason)
})

test_that("request ledger omits unknown terminal failures from JSONL", {
  path <- file.path(withr::local_tempdir(), "usage.jsonl")
  budget <- new_usage_budget(ledger_path = path)
  reservation <- reserve_usage_request(
    budget, budget_request_fixture(),
    audit_context = list(provider = "mock")
  )
  response <- new_llm_response(
    status = "failed", request = reservation$request, provider = "mock",
    error = list(
      class = "private_failure_class",
      reason = "private_failure_reason",
      message = "private upstream text"
    )
  )

  reconcile_usage_request(budget, reservation, response)
  completed <- Filter(
    function(record) identical(record$record_type, "request_completed"),
    budget$records
  )[[1L]]

  expect_null(completed$error_class)
  expect_null(completed$error_reason)
  expect_true(all(c("error_class", "error_reason") %in% names(completed)))
  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json, '"error_class":null', fixed = TRUE)
  expect_match(json, '"error_reason":null', fixed = TRUE)
  expect_false(grepl("private_failure", json, fixed = TRUE))
})

test_that("probe persists the effective constrained hash to audit and ledger", {
  raw_capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", source = "test"
  )
  llm <- new_llm(
    function(request) list(type = "final", data = list(ok = TRUE)),
    provider = "mock", model = "probe-model",
    capabilities = raw_capabilities,
    capabilities_for = function(tier, model = NULL) raw_capabilities,
    transport_constraints = list(
      tools_with_structured_output = "unsupported"
    )
  )
  effective <- llm_capabilities_for(llm, tier = "cheap")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "usage.jsonl")
  budget <- new_usage_budget(ledger_path = path, run_id = "probe-hash")

  expect_true(sas_llm_probe(
    llm, max_retries = 1L, log_dir = dir, tier = "cheap",
    usage_budget = budget
  ))

  audit <- jsonlite::fromJSON(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)[[1L]],
    simplifyVector = FALSE
  )
  rows <- read_usage_ledger(path)
  requests <- rows[rows$record_type %in% c(
    "request_started", "request_completed"
  ), , drop = FALSE]

  expect_identical(audit$capability_hash, effective$record_hash)
  expect_identical(
    unname(requests$capability_hash),
    rep(effective$record_hash, nrow(requests))
  )
})

test_that("probe retryable failures retain the effective constrained hash", {
  raw_capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", source = "test"
  )
  calls <- 0L
  llm <- new_llm(
    function(request) {
      calls <<- calls + 1L
      if (identical(calls, 1L)) {
        stop(structure(
          list(message = "temporary transport failure", call = NULL),
          class = c("sas2r_llm_transport_error", "error", "condition")
        ))
      }
      list(type = "final", data = list(ok = TRUE))
    },
    provider = "mock", model = "probe-model",
    capabilities = raw_capabilities,
    capabilities_for = function(tier, model = NULL) raw_capabilities,
    transport_constraints = list(
      tools_with_structured_output = "unsupported"
    )
  )
  effective <- llm_capabilities_for(llm, tier = "cheap")
  dir <- withr::local_tempdir()

  expect_true(sas_llm_probe(
    llm, max_retries = 2L, log_dir = dir, tier = "cheap"
  ))

  audit <- lapply(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE),
    jsonlite::fromJSON, simplifyVector = FALSE
  )
  expect_identical(calls, 2L)
  expect_identical(length(audit), 2L)
  expect_true(all(vapply(audit, function(row) {
    identical(row$capability_hash, effective$record_hash)
  }, logical(1))))
})

test_that("probe terminal errors retain the effective constrained hash", {
  raw_capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", source = "test"
  )
  llm <- new_llm(
    function(request) stop(structure(
      list(message = "terminal transport failure", call = NULL),
      class = c("sas2r_llm_transport_error", "error", "condition")
    )),
    provider = "mock", model = "probe-model",
    capabilities = raw_capabilities,
    capabilities_for = function(tier, model = NULL) raw_capabilities,
    transport_constraints = list(
      tools_with_structured_output = "unsupported"
    )
  )
  effective <- llm_capabilities_for(llm, tier = "cheap")
  dir <- withr::local_tempdir()

  error <- tryCatch(
    sas_llm_probe(llm, max_retries = 1L, log_dir = dir, tier = "cheap"),
    error = identity
  )
  audit <- jsonlite::fromJSON(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)[[1L]],
    simplifyVector = FALSE
  )

  expect_s3_class(error, "sas2r_llm_transport_error")
  expect_identical(audit$type, "terminal_error")
  expect_identical(audit$capability_hash, effective$record_hash)
})

test_that("probe budget refusals retain the effective constrained hash", {
  raw_capabilities <- llm_capabilities(
    structured_output = "native", tool_calling = "native",
    tools_with_structured_output = "supported", source = "test"
  )
  llm <- new_llm(
    function(request) stop("probe transport should not run"),
    provider = "mock", model = "probe-model",
    capabilities = raw_capabilities,
    capabilities_for = function(tier, model = NULL) raw_capabilities,
    transport_constraints = list(
      tools_with_structured_output = "unsupported"
    )
  )
  effective <- llm_capabilities_for(llm, tier = "cheap")
  dir <- withr::local_tempdir()

  expect_false(sas_llm_probe(
    llm, max_retries = 1L, log_dir = dir, tier = "cheap",
    can_attempt = function() FALSE
  ))

  audit <- jsonlite::fromJSON(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)[[1L]],
    simplifyVector = FALSE
  )
  expect_identical(audit$type, "budget_exhausted")
  expect_identical(audit$capability_hash, effective$record_hash)
})
