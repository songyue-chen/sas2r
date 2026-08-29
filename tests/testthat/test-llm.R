test_that("mock llm replays and exhausts loudly", {
  m <- mock_llm(list(list(type = "final", data = list(x = 1))))
  request <- llm_request(messages = list(list(role = "user", content = "ping")))
  r <- m$request(request)
  expect_identical(r$data$x, 1)
  expect_error(m$request(request), class = "sas2r_llm_error")
})

test_that("ellmer adapter errors informatively when ellmer is absent", {
  skip_if(requireNamespace("ellmer", quietly = TRUE))
  expect_error(ellmer_llm(list(
    provider = "bedrock", model = "m", region = "us-west-2"
  )),
               class = "sas2r_llm_unavailable")
})

test_that("provider list includes direct OpenAI and production requires an explicit model", {
  expect_true("openai" %in% LLM_PROVIDERS)
  expect_error(
    ellmer_llm(list(provider = "openai")),
    class = "sas2r_llm_model_required"
  )
})

test_that("provider constructor arguments preserve ambient identity selectors", {
  bedrock <- ellmer_constructor_args(list(
    provider = "bedrock", auth_mode = "ambient", profile = "clinical-dev",
    region = "us-west-2", model = "us.anthropic.example", cache = "5m"
  ), model = "us.anthropic.example")
  expect_identical(bedrock$profile, "clinical-dev")
  expect_identical(bedrock$cache, "5m")
  expect_identical(
    bedrock$base_url,
    "https://bedrock-runtime.us-west-2.amazonaws.com"
  )
  expect_false("region" %in% names(bedrock))
  expect_false("api_key" %in% names(bedrock))

  azure <- ellmer_constructor_args(list(
    provider = "azure", auth_mode = "ambient",
    endpoint = "https://example.openai.azure.com", model = "deployment",
    api_version = "2025-04-01-preview", credentials = NULL
  ), model = "deployment")
  expect_identical(azure$endpoint, "https://example.openai.azure.com")
  expect_identical(azure$api_version, "2025-04-01-preview")
  expect_false("api_key" %in% names(azure))

  vertex <- ellmer_constructor_args(list(
    provider = "vertex", auth_mode = "ambient", project_id = "project-a",
    location = "us-central1", model = "gemini-example", credentials = NULL
  ), model = "gemini-example")
  expect_identical(vertex$project_id, "project-a")
  expect_identical(vertex$location, "us-central1")
  expect_false("credentials" %in% names(vertex))
  expect_false("api_key" %in% names(vertex))
})

test_that("explicit OpenAI API-key auth remains a valid constructor alternative", {
  cfg <- normalize_llm_config(list(
    provider = "openai", auth_mode = "api_key", model = "gpt-example",
    base_url = "https://api.example.invalid/v1", api_key = "test-api-key"
  ))
  args <- ellmer_constructor_args(cfg, model = cfg$model)
  expect_identical(args$api_key, "test-api-key")
})

test_that("provider selector identity includes Bedrock region and Vertex project", {
  expect_identical(
    ellmer_endpoint_identity(list(provider = "bedrock", region = "us-west-2")),
    "aws-region:us-west-2"
  )
  expect_identical(
    ellmer_endpoint_identity(list(
      provider = "vertex", project_id = "project-a", location = "us-central1"
    )),
    "vertex:project-a:us-central1"
  )
})

test_that("different Bedrock regions cannot share a capability identity", {
  west <- normalize_llm_config(list(
    provider = "bedrock", region = "us-west-2", model = "model-a"
  ))
  east <- normalize_llm_config(list(
    provider = "bedrock", region = "us-east-1", model = "model-a"
  ))

  expect_false(identical(
    ellmer_endpoint_identity(west), ellmer_endpoint_identity(east)
  ))
  west_record <- resolve_model_capabilities(
    "bedrock", ellmer_endpoint_identity(west), "model-a"
  )
  east_record <- resolve_model_capabilities(
    "bedrock", ellmer_endpoint_identity(east), "model-a"
  )
  expect_false(identical(west_record$record_hash, east_record$record_hash))

  shared_base <- "https://bedrock.example.invalid"
  expect_false(identical(
    ellmer_endpoint_identity(list(
      provider = "bedrock", base_url = shared_base, region = "us-west-2"
    )),
    ellmer_endpoint_identity(list(
      provider = "bedrock", base_url = shared_base, region = "us-east-1"
    ))
  ))
})

test_that("unknown providers are rejected regardless of ellmer", {
  expect_error(ellmer_llm(list(provider = "smoke_signals", model = "m")),
               class = "sas2r_llm_error")
})

test_that("lockfile pins provider, models, and prompt hashes", {
  dir <- withr::local_tempdir()
  pdir <- file.path(dir, "prompts"); dir.create(pdir)
  writeLines("translate {{unit}}", file.path(pdir, "translator.md"))
  lock <- file.path(dir, "_sas2r.lock")
  write_llm_lock(list(provider = "bedrock",
                      tiers = list(frontier = "m1", cheap = "m2")), pdir, lock)
  j <- jsonlite::read_json(lock)
  expect_identical(j$provider, "bedrock")
  expect_identical(j$tiers$frontier, "m1")
  expect_true(nzchar(j$prompts[["translator.md"]]))
})

test_that("lockfile separates requested and effective optional parameters", {
  dir <- withr::local_tempdir()
  pdir <- file.path(dir, "prompts")
  dir.create(pdir)
  writeLines("translate", file.path(pdir, "translator.md"))
  lock <- file.path(dir, "_sas2r.lock")

  write_llm_lock(
    list(provider = "openai", model = "m", temperature = 0),
    pdir, lock,
    requested_parameters = list(temperature = 0),
    effective_parameters = list(temperature = NULL)
  )
  json <- jsonlite::read_json(lock)
  text <- paste(readLines(lock, warn = FALSE), collapse = "\n")

  expect_equal(json$requested_parameters$temperature, 0)
  expect_null(json$effective_parameters$temperature)
  expect_match(text, '"effective_parameters"')
  expect_match(text, '"temperature": null')
})

test_that("lockfile capability identity matches the provider constructor", {
  dir <- withr::local_tempdir()
  model <- paste0("identity-", basename(tempfile()))
  cfg <- list(
    provider = "openai", model = model,
    endpoint = "https://ignored.example.invalid",
    base_url = "https://used.example.invalid/v1",
    api_version = "ignored-for-openai",
    capabilities = list(temperature = "supported"),
    temperature = 0
  )
  lock <- file.path(dir, "_sas2r.lock")

  write_llm_lock(cfg, dir, lock)

  written <- jsonlite::read_json(lock)
  expected <- resolve_model_capabilities(
    "openai", cfg$base_url, model, NULL, overrides = cfg$capabilities
  )
  mismatched <- resolve_model_capabilities(
    "openai", cfg$endpoint, model, cfg$api_version,
    overrides = cfg$capabilities
  )
  expect_identical(written$capability_record_hash, expected$record_hash)
  expect_false(identical(written$capability_record_hash, mismatched$record_hash))
})

test_that("llm_log appends jsonl locally", {
  dir <- withr::local_tempdir()
  llm_log(list(agent = "translator", tokens = 10), dir = dir)
  llm_log(list(agent = "fixer", tokens = 5), dir = dir)
  lines <- readLines(file.path(dir, "llm_log.jsonl"))
  expect_identical(length(lines), 2L)
  expect_match(lines[1], "translator")
})

test_that("sas_llm_probe succeeds on structured pong and fails on malformed response", {
  log_dir <- withr::local_tempdir()
  good_llm <- mock_llm(list(list(type = "final", data = list(ok = TRUE))))
  expect_true(sas_llm_probe(good_llm, log_dir = log_dir))

  bad_llm <- mock_llm(list(list(type = "final", data = list(ok = FALSE))))
  expect_false(sas_llm_probe(bad_llm, log_dir = log_dir))
})

test_that("ellmer schema conversion preserves JSON Schema array keywords", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  schema <- list(
    type = "object",
    properties = list(
      ok = list(type = "boolean"),
      detail = list(
        type = "object",
        properties = list(code = list(type = "string", enum = "only")),
        required = "code",
        additionalProperties = FALSE
      )
    ),
    required = "ok",
    additionalProperties = FALSE
  )

  converted <- S7::prop(ellmer_type_from_schema(schema), "json")

  expect_identical(converted$required, list("ok"))
  expect_identical(converted$properties$detail$required, list("code"))
  expect_identical(converted$properties$detail$properties$code$enum,
                   list("only"))
})

test_that("ellmer output schemas follow the provider registry dialect", {
  # Break caught: selecting strict-schema behavior from a hard-coded provider
  # list instead of the provider registry makes every new compatible adapter
  # require another transport-code change.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  schema <- list(
    `$id` = "https://example.invalid/schema#/$defs/result",
    type = "object",
    properties = list(
      ok = list(type = "boolean"), note = list(type = "string")
    ),
    required = "ok",
    additionalProperties = TRUE
  )
  testthat::local_mocked_bindings(
    llm_provider_spec = function(provider) {
      list(output_schema_dialect = "strict_all_properties")
    },
    .package = "sas2r"
  )

  converted <- S7::prop(
    ellmer_output_type(list(provider = "synthetic"), schema), "json"
  )

  expect_identical(converted$required, list("ok", "note"))
  expect_false(converted$additionalProperties)
  expect_null(converted$`$id`)
})

test_that("OpenAI wire schemas require every object property recursively", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  schema <- list(
    `$id` = "https://sas2r.dev/schema.json#/$defs/translation",
    type = "object",
    properties = list(
      ok = list(type = "boolean"),
      detail = list(
        type = "object",
        properties = list(
          code = list(type = "string"), note = list(type = "string")
        ),
        required = "code",
        additionalProperties = TRUE
      )
    ),
    required = "ok",
    additionalProperties = TRUE
  )

  openai <- S7::prop(ellmer_output_type(list(provider = "openai"), schema),
                     "json")
  deepseek <- S7::prop(ellmer_output_type(list(provider = "deepseek"), schema),
                       "json")

  expect_identical(openai$required, list("ok", "detail"))
  expect_false(openai$additionalProperties)
  expect_identical(openai$properties$detail$required, list("code", "note"))
  expect_false(openai$properties$detail$additionalProperties)
  expect_null(openai$`$id`)
  expect_identical(deepseek$required, list("ok"))
  expect_true(deepseek$additionalProperties)
  expect_identical(
    deepseek$`$id`, "https://sas2r.dev/schema.json#/$defs/translation"
  )
})

test_that("schema normalization preserves keyword-shaped property names", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  schema <- list(
    `$id` = "https://sas2r.dev/schema.json#/$defs/translation",
    type = "object",
    properties = list(
      required = list(type = "string"),
      enum = list(type = "integer"),
      allOf = list(type = "boolean"),
      `$id` = list(type = "string"),
      nested = list(
        type = "object",
        properties = list(oneOf = list(type = "string")),
        required = "oneOf",
        additionalProperties = FALSE
      )
    ),
    `$defs` = list(
      required = list(
        type = "object",
        properties = list(enum = list(type = "string")),
        required = "enum",
        additionalProperties = FALSE
      )
    ),
    required = c("required", "enum", "allOf", "$id", "nested"),
    additionalProperties = FALSE
  )

  deepseek <- S7::prop(ellmer_output_type(list(provider = "deepseek"), schema),
                       "json")
  openai <- S7::prop(ellmer_output_type(list(provider = "openai"), schema),
                     "json")

  expect_identical(deepseek$properties$required$type, "string")
  expect_identical(deepseek$properties$enum$type, "integer")
  expect_identical(deepseek$properties$allOf$type, "boolean")
  expect_identical(deepseek$properties$`$id`$type, "string")
  expect_identical(deepseek$`$defs`$required$properties$enum$type, "string")
  expect_identical(openai$properties$`$id`$type, "string")
  expect_identical(openai$properties$nested$properties$oneOf$type, "string")
  expect_null(openai$`$id`)
})

test_that("sas_llm_probe does not infer fallback from unknown capability", {
  calls <- 0L
  unknown_llm <- new_llm(
    function(request) {
      calls <<- calls + 1L
      list(type = "final", data = list(ok = TRUE))
    },
    provider = "custom",
    capabilities = llm_capabilities()
  )

  expect_false(sas_llm_probe(
    unknown_llm, log_dir = withr::local_tempdir()
  ))
  expect_identical(calls, 0L)
})

test_that("render_prompt does not load arbitrary bare files from cwd", {
  dir <- withr::local_tempdir()
  # create a malicious translator.md in temp working directory
  evil_file <- file.path(dir, "fake_prompt.md")
  writeLines("MALICIOUS INJECTION", evil_file)
  # bare filename not in package prompts errors
  expect_error(render_prompt("fake_prompt.md"), class = "sas2r_prompt_error")
  # explicit path loads correctly
  expect_match(render_prompt(evil_file), "MALICIOUS INJECTION")
})

test_that("strip_fences cleanly extracts json from markdown code fences", {
  json_txt <- '{\n  "type": "final",\n  "data": {"ok": true}\n}'
  fenced1 <- sprintf("```json\n%s\n```", json_txt)
  fenced2 <- sprintf("```\n%s\n```", json_txt)
  expect_identical(strip_fences(fenced1), json_txt)
  expect_identical(strip_fences(fenced2), json_txt)
  expect_identical(strip_fences(json_txt), json_txt)
})

test_that("sas_llm_probe does not retry definitive nonanswers and writes durable audit fields", {
  log_dir <- withr::local_tempdir()
  retry_llm <- mock_llm(list(
    structure(list(type = "final", data = list(ok = FALSE)),
              cost_usd = 0.001,
              usage = list(input_tokens = 100, output_tokens = 25)),
    list(type = "final", data = list(ok = TRUE))
  ))
  charged <- numeric()
  expect_false(sas_llm_probe(
    retry_llm, max_retries = 2L, log_dir = log_dir, tier = "cheap",
    on_charge = function(amount) {
      charged <<- c(charged, amount)
      sum(charged, na.rm = TRUE)
    }
  ))
  expect_identical(length(charged), 1L)
  log_rows <- lapply(readLines(file.path(log_dir, "llm_log.jsonl")), jsonlite::fromJSON)
  expect_identical(length(log_rows), 1L)
  expect_identical(log_rows[[1]]$tier, "cheap")
  expect_equal(log_rows[[1]]$attempt, 1)
  expect_equal(log_rows[[1]]$input_tokens, 100)
  expect_equal(log_rows[[1]]$output_tokens, 25)
  expect_equal(log_rows[[1]]$cost_usd, 0.001)
  expect_identical(log_rows[[1]]$cost_status, "catalog_estimate")
  expect_equal(log_rows[[1]]$cumulative_spend_usd, 0.001)
})

test_that("real S7 adapter fixture runs in an isolated ellmer library", {
  skip_if_not_installed("S7")
  result <- run_isolated_ellmer_s7(
    test_path("..", "fixtures", "ellmer-s7")
  )

  expect_match(result$ellmer_path, "sas2r-ellmer-lib-")
  expect_identical(result$code, "x <- 1")
  expect_equal(result$usage$input_tokens, 100)
  expect_equal(result$usage$output_tokens, 25)
  expect_equal(result$cost, 0.005)
  # The durable provenance names both public APIs that produced a number.
  expect_identical(
    result$direct_response$cost$provenance,
    "ellmer public get_tokens() / ellmer public get_cost(include = 'last')"
  )
  expect_identical(result$finish_reason, "success")
  expect_identical(result$two_phase$status, "ok")
  expect_identical(result$two_phase_executions, 1L)
  # Named for the cause: the tool allowance ran out, no budget was involved.
  expect_identical(result$limited$status, "agent_tool_limit_reached")
  expect_identical(result$limited_executions, 1L)
  expect_identical(result$secret_result$status, "transport_failed")
  expect_false(grepl(
    result$configured_secret,
    jsonlite::toJSON(result$secret_result$error, auto_unbox = TRUE),
    fixed = TRUE
  ))
  expect_false(grepl(result$configured_secret, result$secret_log, fixed = TRUE))
  expect_match(result$secret_log, "[REDACTED]", fixed = TRUE)

  constructors <- result$contract$constructors
  expect_true(length(constructors) >= 6L)
  azure <- Filter(function(x) identical(x$provider, "azure"), constructors)
  expect_true(all(vapply(
    azure, function(x) x$endpoint %||% "", character(1)
  ) == "https://azure.example.invalid"))
  expect_true(all(vapply(
    azure, function(x) x$api_version %||% "", character(1)
  ) == "2026-08-20"))
  openai <- Filter(function(x) identical(x$provider, "openai"), constructors)
  expect_identical(openai[[1]]$base_url, "https://openai.example.invalid/v1")
  ollama <- Filter(function(x) identical(x$provider, "ollama"), constructors)
  expect_identical(ollama[[1]]$base_url, "http://ollama.example.invalid")
  bedrock <- Filter(function(x) identical(x$provider, "bedrock"), constructors)
  expect_identical(
    bedrock[[1]]$base_url,
    "https://bedrock-runtime.us-west-2.amazonaws.com"
  )
  expect_identical(bedrock[[1]]$profile, "research")
  expect_null(bedrock[[1]]$region)
  expect_identical(bedrock[[1]]$cache, "auto")
  expect_false(bedrock[[1]]$has_api_key)
  vertex <- Filter(function(x) identical(x$provider, "vertex"), constructors)
  expect_length(vertex, 1L)
  expect_identical(vertex[[1]]$project_id, "project-a")
  expect_identical(vertex[[1]]$location, "us-central1")
  expect_false(vertex[[1]]$has_credentials)
  anthropic <- Filter(function(x) identical(x$provider, "anthropic"), constructors)
  expect_identical(
    anthropic[[1]]$base_url, "https://anthropic.example.invalid/v1"
  )
  expect_identical(anthropic[[1]]$cache, "1h")
  expect_false(anthropic[[1]]$has_api_key)
  expect_false(anthropic[[1]]$has_credentials)
  databricks <- Filter(function(x) identical(x$provider, "databricks"), constructors)
  expect_identical(
    databricks[[1]]$workspace, "https://example.cloud.databricks.invalid"
  )
  expect_null(databricks[[1]]$base_url)
  expect_false(databricks[[1]]$has_credentials)
  deepseek <- Filter(function(x) identical(x$provider, "deepseek"), constructors)
  expect_identical(deepseek[[1]]$base_url, "https://deepseek.example.invalid")
  github <- Filter(function(x) identical(x$provider, "github"), constructors)
  expect_identical(
    github[[1]]$base_url, "https://models.example.invalid/inference/"
  )
  # An omitted selector must leave the public ellmer default in place.
  gemini <- Filter(function(x) identical(x$provider, "gemini"), constructors)
  expect_identical(
    gemini[[1]]$base_url,
    "https://generativelanguage.googleapis.com/v1beta/"
  )
  expect_true(gemini[[1]]$has_credentials)
  expect_false(gemini[[1]]$has_api_key)
  posit <- Filter(function(x) identical(x$provider, "posit"), constructors)
  expect_identical(posit[[1]]$base_url, "https://gateway.posit.ai")
  expect_identical(posit[[1]]$cache, "none")
  expect_true(posit[[1]]$has_credentials)
  snowflake <- Filter(function(x) identical(x$provider, "snowflake"), constructors)
  expect_identical(snowflake[[1]]$account, "org-acct")
  expect_true(snowflake[[1]]$has_credentials)
  expect_null(snowflake[[1]]$base_url)

  # An omitted `workspace`/`account` passes no argument at all, so ellmer's own
  # ambient default resolves the tenant -- and sas2r still identifies it.
  ambient <- result$ambient_constructors
  expect_identical(vapply(ambient, `[[`, "", "provider"),
                   c("databricks", "snowflake"))
  expect_identical(ambient[[1]]$workspace, "https://ambient.databricks.invalid")
  expect_identical(ambient[[2]]$account, "ambient-acct")
  expect_identical(
    result$ambient_identities,
    list(
      databricks = "https://ambient.databricks.invalid",
      snowflake = "snowflake:ambient-acct"
    )
  )

  expect_identical(result$bedrock_inventory$models$id, "s7-bedrock")
  expect_identical(result$vertex_inventory$models[[1]]$model_id, "s7-vertex")
  # Every declared inventory endpoint must resolve in the fixture, or a typo
  # in `models_export` degrades to "inventory_unavailable" instead of failing.
  expect_length(result$declared_inventories, 9L)
  expect_true(all(result$inventory_exports_resolved),
              info = paste("unresolved models_export:", paste(
                names(result$inventory_exports_resolved)[
                  !result$inventory_exports_resolved], collapse = ", ")))

  agreed <- result$agreed_inventories
  expect_identical(
    vapply(agreed, `[[`, "", "status"),
    c(openai = "available", ollama = "available", anthropic = "available",
      databricks = "inventory_unavailable",
      deepseek = "available", github = "available", gemini = "available",
      posit = "available", snowflake = "inventory_unavailable")
  )
  expect_identical(agreed$openai$models$id, "s7-openai")
  expect_identical(agreed$ollama$models$id, "s7-ollama")
  expect_identical(agreed$anthropic$models$id, "s7-anthropic")
  expect_identical(agreed$deepseek$models$id, "s7-deepseek")
  expect_identical(agreed$github$models$id, "s7-github")
  expect_identical(agreed$gemini$models[[1]]$model_id, "s7-gemini")
  expect_identical(agreed$posit$models$id, "s7-posit")
  expect_null(agreed$databricks$models)
  expect_null(agreed$snowflake$models)
  expect_identical(
    result$github_custom_inventory$status, "inventory_unavailable"
  )
  expect_identical(
    result$deepseek_default_capabilities$structured_output, "fallback"
  )
  expect_identical(result$deepseek_default_capabilities$tool_calling, "unknown")
  expect_setequal(
    names(result$provider_effective_coexistence), llm_provider_ids()
  )
  expect_true(all(
    result$provider_effective_coexistence == "unsupported"
  ))

  inventories <- result$contract$inventories
  expect_identical(inventories[[1]]$profile, "research")
  expect_identical(
    inventories[[1]]$base_url, "https://bedrock.us-west-2.amazonaws.com"
  )
  expect_identical(inventories[[2]]$project_id, "project-a")
  expect_identical(inventories[[2]]$location, "us-central1")
  agreed_calls <- inventories[-(1:2)]
  expect_identical(
    vapply(agreed_calls, `[[`, "", "provider"),
    c("openai", "ollama", "anthropic", "deepseek", "github", "gemini", "posit")
  )
  names(agreed_calls) <- vapply(agreed_calls, `[[`, "", "provider")
  expect_identical(
    agreed_calls$openai$base_url, "https://openai.example.invalid/v1"
  )
  expect_identical(
    agreed_calls$ollama$base_url, "http://ollama.example.invalid"
  )
  expect_identical(
    agreed_calls$anthropic$base_url, "https://anthropic.example.invalid/v1"
  )
  expect_identical(
    agreed_calls$github$base_url, "https://models.example.invalid/"
  )
  expect_identical(
    agreed_calls$gemini$base_url,
    "https://generativelanguage.googleapis.com/v1beta/"
  )
  expect_true(agreed_calls$gemini$has_credentials)
  expect_true(agreed_calls$posit$has_credentials)
  expect_false(any(vapply(
    agreed_calls, function(x) isTRUE(x$has_api_key), logical(1)
  )))
  expect_true(any(vapply(constructors, function(x) {
    identical(x$params$max_tokens %||% NULL, 321L)
  }, logical(1))))
  expect_false(any(vapply(constructors, function(x) {
    !is.null(x$params$max_output_tokens)
  }, logical(1))))

  registered <- result$contract$tools
  lookup <- Filter(function(x) identical(x$name, "lookup"), registered)
  expect_length(lookup, 1L)
  expect_identical(
    lookup[[1]]$required,
    c(name = TRUE, functions = FALSE, operators = FALSE, procs = FALSE)
  )

  calls <- result$contract$calls
  expect_true(any(vapply(calls, `[[`, "", "method") == "chat"))
  expect_true(any(vapply(calls, `[[`, "", "method") == "chat_structured"))
  expect_true(all(vapply(
    Filter(function(call) identical(call$method, "chat_structured"), calls),
    `[[`, "", "type_class"
  ) == "TypeJsonSchema"))
  direct <- calls[[1]]
  expect_identical(
    direct$roles,
    c("system", "user", "assistant_tool", "tool")
  )
  expect_identical(direct$prompt, "final answer")
  expect_identical(direct$request_ids[3:4], c("call_1", "call_1"))
  two_phase_final <- Filter(function(call) {
    identical(call$method, "chat_structured") &&
      identical(call$prompt, "Return the complete final answer in the required schema.")
  }, calls)
  expect_length(two_phase_final, 1L)
  expect_identical(
    two_phase_final[[1]]$roles,
    c("system", "user", "assistant_tool", "tool", "assistant")
  )
  expect_identical(
    two_phase_final[[1]]$request_ids[3:4], c("auto_1", "auto_1")
  )
})

test_that("usage falls back to public per-turn tokens when get_tokens fails", {
  skip_if_not_installed("S7")
  # Real ellmer's public `get_tokens()` tabulates the whole conversation and
  # errors whenever the user and assistant turn counts differ, which is every
  # tool-call exchange. Usage metering must not silently degrade to unknown.
  Turn <- S7::new_class("Turn", properties = list(tokens = S7::class_numeric))
  failing <- list(
    get_tokens = function() stop("Can't recycle input of size 2 to size 1"),
    last_turn = function() Turn(
      tokens = c(input = 7, output = 3, cached_input = 0)
    )
  )
  usage <- ellmer_usage(failing)
  expect_identical(usage$input_tokens, 7)
  expect_identical(usage$output_tokens, 3)

  # A working conversation tabulation still wins over the per-turn record.
  working <- list(
    get_tokens = function() list(input = 11, output = 5),
    last_turn = function() Turn(tokens = c(input = 99, output = 99))
  )
  expect_identical(ellmer_usage(working)$input_tokens, 11)
  expect_identical(ellmer_usage(working)$output_tokens, 5)

  # ellmer's own default `AssistantTurn@tokens` is an unnamed `c(NA, NA, NA)`,
  # so an unnamed vector must never be read positionally.
  unknown <- list(
    get_tokens = function() stop("no tokens"),
    last_turn = function() Turn(tokens = c(1, 2, 3))
  )
  expect_true(is.na(ellmer_usage(unknown)$input_tokens))
  expect_true(is.na(ellmer_usage(list())$input_tokens))
  expect_null(attr(ellmer_usage(unknown), "token_provenance"))

  # The recorded provenance names the API each number actually came from.
  expect_identical(
    attr(ellmer_usage(failing), "token_provenance"),
    "ellmer public Turn@tokens"
  )
  expect_identical(
    attr(ellmer_usage(working), "token_provenance"),
    "ellmer public get_tokens()"
  )
  expect_identical(
    ellmer_usage_provenance(ellmer_usage(failing), 0.5),
    "ellmer public Turn@tokens / ellmer public get_cost(include = 'last')"
  )
  expect_identical(
    ellmer_usage_provenance(ellmer_usage(failing), NA_real_),
    "ellmer public Turn@tokens"
  )
  expect_null(ellmer_usage_provenance(ellmer_usage(list()), NA_real_))
})

test_that("usage sums every complete assistant turn of a real ellmer request", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  # The real trigger is not a balanced tool round trip -- 2 user / 2 assistant
  # tabulates fine. `get_tokens()` builds one row per complete assistant turn
  # and then assigns one user-turn preview per row, so it errors on a replayed
  # history that ends on a user or tool turn, exactly what the real-ellmer
  # contract builds.
  chat <- ellmer::chat_openai(
    model = "offline-usage-model",
    credentials = function() "offline-usage-key", echo = "none"
  )
  chat$set_turns(list(
    ellmer::UserTurn("first question"),
    ellmer::AssistantTurn(
      "tool request", tokens = c(input = 5, output = 3, cached_input = 0)
    ),
    ellmer::UserTurn("tool result"),
    ellmer::AssistantTurn(
      "final answer", tokens = c(input = 12, output = 8, cached_input = 1)
    ),
    ellmer::UserTurn("replayed tail")
  ))
  expect_error(chat$get_tokens(), "recycle")

  # A single request can append both a tool-request turn and a final answer
  # turn. `get_tokens()` would have summed both, so the fallback must too:
  # reading only `last_turn()` reports 12/8 and drops the tool-request turn.
  usage <- ellmer_usage(chat)
  expect_identical(usage$input_tokens, 17)
  expect_identical(usage$output_tokens, 11)
  expect_identical(
    attr(usage, "token_provenance"), "ellmer public Turn@tokens"
  )

  # A balanced history is tabulated by ellmer itself and never reaches the
  # fallback.
  balanced <- ellmer::chat_openai(
    model = "offline-usage-model",
    credentials = function() "offline-usage-key", echo = "none"
  )
  balanced$set_turns(chat$get_turns()[1:4])
  expect_identical(nrow(balanced$get_tokens()), 2L)
  balanced_usage <- ellmer_usage(balanced)
  expect_identical(
    attr(balanced_usage, "token_provenance"), "ellmer public get_tokens()"
  )
  # ellmer's own tabulation and the fallback agree on the same conversation.
  expect_identical(balanced_usage$input_tokens, usage$input_tokens)
  expect_identical(balanced_usage$output_tokens, usage$output_tokens)
})

test_that("OpenAI turn JSON preserves disjoint cache and reasoning usage", {
  # Break caught: falling back to ellmer's reduced token table loses cache
  # writes and reasoning details, and makes a 1,000-token prompt look like
  # three ordinary input tokens in the durable ledger.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  provider_turn <- ellmer::AssistantTurn(
    "answer",
    json = list(
      usage = list(
        input_tokens = 1000L,
        input_tokens_details = list(
          cached_tokens = 200L,
          cache_write_tokens = 300L
        ),
        output_tokens = 100L,
        output_tokens_details = list(reasoning_tokens = 40L),
        total_tokens = 1100L
      )
    ),
    tokens = c(input = 3, output = 100, cached_input = 200),
    finish_reason = "stop"
  )
  replayed_turn <- ellmer::AssistantTurn(
    "history",
    json = list(),
    tokens = c(input = 999, output = 999, cached_input = 0),
    finish_reason = "stop"
  )
  chat <- list(
    get_tokens = function() list(input = 3, output = 100, cached_input = 200),
    get_turns = function() list(replayed_turn, provider_turn),
    last_turn = function() provider_turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$total_input_tokens, 1000)
  expect_identical(usage$input_tokens, 500)
  expect_identical(usage$cached_input_tokens, 200)
  expect_identical(usage$cache_write_tokens, 300)
  expect_identical(usage$total_output_tokens, 100)
  expect_identical(usage$output_tokens, 60)
  expect_identical(usage$reasoning_tokens, 40)
  expect_identical(usage$total_tokens, 1100)
  expect_identical(
    attr(usage, "token_provenance"),
    "ellmer public AssistantTurn@json usage"
  )
  expect_identical(attr(usage, "input_accounting_status"), "consistent")
  expect_identical(attr(usage, "input_accounting_delta_tokens"), 0)
  expect_identical(attr(usage, "total_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_delta_tokens"), 0)
})

test_that("DeepSeek native cache hit and miss fields remain measurable", {
  # Break caught: DeepSeek is OpenAI-compatible but reports cache reads in
  # prompt_cache_hit_tokens, which ellmer 0.4.2 does not copy into Turn@tokens.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  provider_turn <- ellmer::AssistantTurn(
    "answer",
    json = list(
      usage = list(
        prompt_tokens = 1000L,
        prompt_cache_hit_tokens = 700L,
        prompt_cache_miss_tokens = 300L,
        completion_tokens = 50L,
        completion_tokens_details = list(reasoning_tokens = 20L),
        total_tokens = 1050L
      )
    ),
    tokens = c(input = 1000, output = 50, cached_input = 0),
    finish_reason = "stop"
  )
  chat <- list(
    get_tokens = function() list(input = 1000, output = 50, cached_input = 0),
    get_turns = function() list(provider_turn),
    last_turn = function() provider_turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$total_input_tokens, 1000)
  expect_identical(usage$input_tokens, 300)
  expect_identical(usage$cached_input_tokens, 700)
  expect_true(is.na(usage$cache_write_tokens))
  expect_identical(usage$total_output_tokens, 50)
  expect_identical(usage$output_tokens, 30)
  expect_identical(usage$reasoning_tokens, 20)
  expect_identical(usage$total_tokens, 1050)
  expect_identical(attr(usage, "input_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_delta_tokens"), 0)
})

test_that("hit-miss cache usage leaves unreported reasoning unknown", {
  # Break caught: input cache fields do not establish whether output reasoning
  # is absent or merely unreported by an otherwise compatible endpoint.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  turn <- ellmer::AssistantTurn(
    "answer",
    json = list(usage = list(
      prompt_tokens = 1000L,
      prompt_cache_hit_tokens = 700L,
      prompt_cache_miss_tokens = 300L,
      completion_tokens = 50L,
      completion_tokens_details = list(audio_tokens = 4L),
      total_tokens = 1050L
    )),
    tokens = c(input = 1000, output = 50, cached_input = 0),
    finish_reason = "stop"
  )
  chat <- list(
    get_turns = function() list(turn),
    last_turn = function() turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$input_tokens, 300)
  expect_identical(usage$cached_input_tokens, 700)
  expect_identical(usage$output_tokens, 50)
  expect_true(is.na(usage$reasoning_tokens))
  expect_identical(usage$total_output_tokens, 50)
  expect_identical(attr(usage, "total_accounting_status"), "consistent")
})

test_that("Anthropic turn JSON preserves cache writes, reads, and thinking", {
  # Break caught: treating input_tokens as the whole prompt drops Anthropic's
  # separately reported cache partitions, while ignoring thinking_tokens
  # folds hidden reasoning into ordinary output.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  turn <- ellmer::AssistantTurn(
    "answer",
    json = list(usage = list(
      input_tokens = 500L,
      cache_creation_input_tokens = 300L,
      cache_read_input_tokens = 200L,
      output_tokens = 100L,
      output_tokens_details = list(thinking_tokens = 40L)
    )),
    tokens = c(input = 800, output = 100, cached_input = 200),
    finish_reason = "stop"
  )
  chat <- list(
    get_tokens = function() list(input = 800, output = 100, cached_input = 200),
    get_turns = function() list(turn),
    last_turn = function() turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$input_tokens, 500)
  expect_identical(usage$cached_input_tokens, 200)
  expect_identical(usage$cache_write_tokens, 300)
  expect_identical(usage$total_input_tokens, 1000)
  expect_identical(usage$output_tokens, 60)
  expect_identical(usage$reasoning_tokens, 40)
  expect_identical(usage$total_output_tokens, 100)
  expect_identical(usage$total_tokens, 1100)
  expect_identical(attr(usage, "input_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_status"), "unavailable")
  expect_true(is.na(attr(usage, "total_accounting_delta_tokens")))
})

test_that("partitioned cache usage leaves unreported reasoning unknown", {
  # Break caught: cache partition fields say nothing about whether a provider
  # exposes reasoning tokens, so their presence must not invent a zero count.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  turn <- ellmer::AssistantTurn(
    "answer",
    json = list(usage = list(
      input_tokens = 500L,
      cache_creation_input_tokens = 300L,
      cache_read_input_tokens = 200L,
      output_tokens = 100L,
      output_tokens_details = list(audio_tokens = 4L)
    )),
    tokens = c(input = 800, output = 100, cached_input = 200),
    finish_reason = "stop"
  )
  chat <- list(
    get_tokens = function() list(input = 800, output = 100, cached_input = 200),
    get_turns = function() list(turn),
    last_turn = function() turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$input_tokens, 500)
  expect_identical(usage$cached_input_tokens, 200)
  expect_identical(usage$cache_write_tokens, 300)
  expect_identical(usage$total_input_tokens, 1000)
  expect_identical(usage$output_tokens, 100)
  expect_true(is.na(usage$reasoning_tokens))
  expect_identical(usage$total_output_tokens, 100)
  expect_identical(usage$total_tokens, 1100)
  expect_identical(attr(usage, "total_accounting_status"), "unavailable")
  expect_true(is.na(attr(usage, "total_accounting_delta_tokens")))
})

test_that("Bedrock turn JSON preserves camel-case cache partitions", {
  # Break caught: reading only snake-case usage fields falls back to ellmer's
  # reduced three-column token record and loses Bedrock cache writes.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  turn <- ellmer::AssistantTurn(
    "answer",
    json = list(usage = list(
      inputTokens = 500L,
      cacheReadInputTokens = 200L,
      cacheWriteInputTokens = 300L,
      outputTokens = 100L,
      totalTokens = 1100L
    )),
    tokens = c(input = 7, output = 8, cached_input = 9),
    finish_reason = "stop"
  )
  chat <- list(
    get_tokens = function() list(input = 7, output = 8, cached_input = 9),
    get_turns = function() list(turn),
    last_turn = function() turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$input_tokens, 500)
  expect_identical(usage$cached_input_tokens, 200)
  expect_identical(usage$cache_write_tokens, 300)
  expect_identical(usage$total_input_tokens, 1000)
  expect_identical(usage$output_tokens, 100)
  expect_true(is.na(usage$reasoning_tokens))
  expect_identical(usage$total_output_tokens, 100)
  expect_identical(usage$total_tokens, 1100)
  expect_identical(attr(usage, "input_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_delta_tokens"), 0)
})

test_that("Gemini turn JSON separates cached prompts, tools, and thoughts", {
  # Break caught: checking only json$usage misses Google's usageMetadata and
  # misclassifies both tool-result input and thinking tokens as output.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  turn <- ellmer::AssistantTurn(
    "answer",
    json = list(usageMetadata = list(
      promptTokenCount = 1000L,
      cachedContentTokenCount = 200L,
      toolUsePromptTokenCount = 50L,
      candidatesTokenCount = 60L,
      thoughtsTokenCount = 40L,
      totalTokenCount = 1150L
    )),
    tokens = c(input = 7, output = 8, cached_input = 9),
    finish_reason = "stop"
  )
  chat <- list(
    get_tokens = function() list(input = 7, output = 8, cached_input = 9),
    get_turns = function() list(turn),
    last_turn = function() turn
  )

  usage <- ellmer_usage(chat)

  expect_identical(usage$input_tokens, 850)
  expect_identical(usage$cached_input_tokens, 200)
  expect_true(is.na(usage$cache_write_tokens))
  expect_identical(usage$total_input_tokens, 1050)
  expect_identical(usage$output_tokens, 60)
  expect_identical(usage$reasoning_tokens, 40)
  expect_identical(usage$total_output_tokens, 100)
  expect_identical(usage$total_tokens, 1150)
  expect_identical(attr(usage, "input_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_status"), "consistent")
  expect_identical(attr(usage, "total_accounting_delta_tokens"), 0)
})

test_that("reported grand totals are checked against normalized dimensions", {
  # Break caught: individually consistent cache buckets can still disagree
  # with the provider's explicit grand total after response-shape changes.
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  usage_from <- function(json) {
    turn <- ellmer::AssistantTurn(
      "answer", json = json,
      tokens = c(input = 1, output = 1, cached_input = 0),
      finish_reason = "stop"
    )
    ellmer_usage(list(
      get_turns = function() list(turn),
      last_turn = function() turn
    ))
  }

  bedrock <- usage_from(list(usage = list(
    inputTokens = 500L,
    cacheReadInputTokens = 200L,
    cacheWriteInputTokens = 300L,
    outputTokens = 100L,
    totalTokens = 1050L
  )))
  gemini <- usage_from(list(usageMetadata = list(
    promptTokenCount = 1000L,
    cachedContentTokenCount = 200L,
    toolUsePromptTokenCount = 50L,
    candidatesTokenCount = 60L,
    thoughtsTokenCount = 40L,
    totalTokenCount = 1200L
  )))

  expect_identical(attr(bedrock, "total_accounting_status"), "mismatch")
  expect_identical(attr(bedrock, "total_accounting_delta_tokens"), -50)
  expect_identical(attr(gemini, "total_accounting_status"), "mismatch")
  expect_identical(attr(gemini, "total_accounting_delta_tokens"), 50)
})

test_that("native cache accounting mismatches are flagged without aborting", {
  # Break caught: silently forcing inconsistent provider buckets to add up
  # would erase the evidence needed to diagnose an API or adapter change.
  skip_if_not_installed("ellmer")
  turn <- ellmer::AssistantTurn(
    "answer",
    json = list(usage = list(
      prompt_tokens = 100L,
      prompt_cache_hit_tokens = 30L,
      prompt_cache_miss_tokens = 60L,
      completion_tokens = 10L,
      total_tokens = 110L
    )),
    finish_reason = "stop"
  )
  chat <- list(
    get_turns = function() list(turn),
    last_turn = function() turn
  )

  usage <- expect_no_error(ellmer_usage(chat))

  expect_identical(usage$total_input_tokens, 100)
  expect_identical(usage$input_tokens, 60)
  expect_identical(usage$cached_input_tokens, 30)
  expect_identical(attr(usage, "input_accounting_status"), "mismatch")
  expect_identical(attr(usage, "input_accounting_delta_tokens"), 10)
})

test_that("sas_llm_probe retries transport errors only", {
  log_dir <- withr::local_tempdir()
  calls <- 0L
  transient_llm <- new_llm(function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("temporary transport failure")
    list(type = "final", data = list(ok = TRUE))
  }, provider = "mock")
  expect_true(sas_llm_probe(transient_llm, max_retries = 2L, log_dir = log_dir))
  expect_identical(calls, 2L)
  rows <- lapply(readLines(file.path(log_dir, "llm_log.jsonl")), jsonlite::fromJSON)
  expect_identical(length(rows), 2L)
  expect_identical(rows[[1]]$type, "error")
  expect_equal(rows[[2]]$attempt, 2)
})

test_that("render_prompt rejects slash-relative paths from cwd", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  evil_file <- file.path(dir, "translator.md")
  writeLines("MALICIOUS INJECTION", evil_file)
  # Relative paths containing ./ or ../ or single backslash must be rejected
  expect_error(render_prompt("./translator.md"), class = "sas2r_prompt_error")
  expect_error(render_prompt("../translator.md"), class = "sas2r_prompt_error")
  expect_error(render_prompt("\\translator.md"), class = "sas2r_prompt_error")
})

test_that("write_llm_lock hashes custom prompt files from specs and includes macro prompt", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  # Plant a fake translator.md in cwd to verify it is NOT hashed for bare names
  cwd_fake <- file.path(dir, "translator.md")
  writeLines("CWD FAKE PROMPT", cwd_fake)

  custom_prompt <- file.path(dir, "custom_fixer.md")
  writeLines("CUSTOM FIXER CONTENT", custom_prompt)
  specs <- list(
    translator = list(prompt = "translator.md"),
    fixer = list(prompt = custom_prompt)
  )
  lock_file <- file.path(dir, "_sas2r.lock")
  write_llm_lock(list(provider = "bedrock", tiers = list(frontier = "m")),
                 prompts_dir = dir, path = lock_file, specs = specs)
  j <- jsonlite::read_json(lock_file)
  expect_true("translator.md" %in% names(j$prompts))
  # Must match the packaged prompt hash, NOT the stray cwd file hash
  pkg_prompt <- system.file("prompts", "translator.md", package = "sas2r")
  expect_identical(j$prompts[["translator.md"]], unname(tools::md5sum(pkg_prompt)))
  # Must include custom prompt
  expect_true("custom_fixer.md" %in% names(j$prompts))
  expect_identical(j$prompts[["custom_fixer.md"]], unname(tools::md5sum(custom_prompt)))
  # Must include translator-macro.md
  expect_true("translator-macro.md" %in% names(j$prompts))
})

test_that("write_llm_lock warns and falls back on unresolvable prompt paths", {
  dir <- withr::local_tempdir()
  missing_prompt <- file.path(dir, "does_not_exist_translator.md")
  specs <- list(
    translator = list(prompt = missing_prompt)
  )
  lock_file <- file.path(dir, "_sas2r.lock")
  expect_warning(
    write_llm_lock(list(provider = "bedrock", tiers = list(frontier = "m")),
                   prompts_dir = dir, path = lock_file, specs = specs),
    "could not be resolved"
  )
})

test_that("a zero-argument tool is callable", {
  # `names(list())` is NULL, not character(0), and mget(NULL) aborts with
  # "invalid first argument" -- which silently broke every argument-less tool
  # (read_unit_context, read_diff_evidence, list_macro_files) against a live
  # provider while every mock-backed test passed.
  called <- FALSE
  tool <- list(
    name = "read_unit_context",
    description = "no arguments",
    schema = closed_tool_schema(),
    call = function(args) { called <<- TRUE; list(ok = TRUE, args = args) }
  )

  fn <- ellmer_tool_function(tool)

  expect_no_error(result <- fn())
  expect_true(called)
  expect_true(result$ok)
  expect_identical(length(result$args), 0L)
})

test_that("a tool with arguments still receives them", {
  seen <- NULL
  tool <- list(
    name = "find_macro", description = "one argument",
    schema = closed_tool_schema(list(name = tool_string()), "name"),
    call = function(args) { seen <<- args; list(ok = TRUE) }
  )

  fn <- ellmer_tool_function(tool)
  fn(name = "utl_quantile")

  expect_identical(seen$name, "utl_quantile")
})

test_that("every shipped tool schema produces a callable contract", {
  for (nm in names(TOOL_ARGUMENT_SCHEMAS)) {
    tool <- list(
      name = nm, description = TOOL_DESCRIPTIONS[[nm]],
      schema = TOOL_ARGUMENT_SCHEMAS[[nm]],
      call = function(args) list(ok = TRUE)
    )
    expect_no_error(ellmer_tool_function(tool), message = nm)
  }
})

test_that("parallel tool calls are paired with their results, not batched", {
  # ellmer groups an assistant turn's N tool requests together and their N
  # results in the following turn, which flattens to
  #   assistant(tc1), assistant(tc2), tool(r1), tool(r2)
  # Every OpenAI-compatible API rejects that: an assistant message carrying
  # tool_calls must be followed by the tool messages answering them. DeepSeek
  # returns HTTP 400 "insufficient tool messages following tool_calls message".
  flattened <- list(
    list(role = "user", content = "go"),
    list(role = "assistant", content = "", tool_call = list(id = "c1", name = "t", arguments = list())),
    list(role = "assistant", content = "", tool_call = list(id = "c2", name = "t", arguments = list())),
    list(role = "tool", name = "t", tool_call_id = "c1", content = "{}"),
    list(role = "tool", name = "t", tool_call_id = "c2", content = "{}"),
    list(role = "user", content = "finish")
  )

  paired <- interleave_tool_messages(flattened)

  expect_identical(vapply(paired, `[[`, character(1), "role"),
                   c("user", "assistant", "tool", "assistant", "tool", "user"))
  expect_identical(paired[[2]]$tool_call$id, "c1")
  expect_identical(paired[[3]]$tool_call_id, "c1")
  expect_identical(paired[[4]]$tool_call$id, "c2")
  expect_identical(paired[[5]]$tool_call_id, "c2")
})

test_that("a conversation without tool calls is returned unchanged", {
  plain <- list(
    list(role = "system", content = "s"),
    list(role = "user", content = "u"),
    list(role = "assistant", content = "a")
  )

  expect_identical(interleave_tool_messages(plain), plain)
})

test_that("a tool call with no matching result is preserved, not dropped", {
  orphaned <- list(
    list(role = "assistant", content = "", tool_call = list(id = "c1", name = "t", arguments = list())),
    list(role = "user", content = "finish")
  )

  paired <- interleave_tool_messages(orphaned)

  expect_length(paired, 2L)
  expect_identical(paired[[1]]$tool_call$id, "c1")
})

test_that("an already-paired conversation is left alone", {
  paired_in <- list(
    list(role = "assistant", content = "", tool_call = list(id = "c1", name = "t", arguments = list())),
    list(role = "tool", name = "t", tool_call_id = "c1", content = "{}")
  )

  expect_identical(interleave_tool_messages(paired_in), paired_in)
})

test_that("request policy is local adapter metadata, not request payload", {
  seen_request <- NULL
  seen_context <- NULL
  policy <- list(
    timeout_scope = "http_attempt_absolute",
    timeout_seconds = 300,
    transport_max_tries = 1L
  )
  llm <- new_llm(
    function(request, audit_context = list()) {
      seen_request <<- request
      seen_context <<- audit_context
      list(type = "final", data = list(ok = TRUE))
    },
    provider = "mock", model = "fixture-model", request_policy = policy
  )

  response <- attempt_llm_request(
    llm_request(messages = list(list(role = "user", content = "ping"))),
    llm, new_usage_budget()
  )

  expect_identical(response$status, "completed")
  expect_identical(llm$request_policy, policy)
  expect_identical(seen_context[names(policy)], policy)
  expect_false(any(names(policy) %in% names(seen_request)))
})

test_that("request policy rejects unknown fields", {
  expect_error(
    new_llm(identity, provider = "mock", request_policy = list(secret = "x")),
    class = "sas2r_llm_error"
  )
})

test_that("request policy requires an absolute HTTP timeout scope", {
  expect_error(
    new_llm(
      identity, provider = "mock",
      request_policy = list(
        timeout_scope = "idle", timeout_seconds = 300,
        transport_max_tries = 1L
      )
    ),
    class = "sas2r_llm_error"
  )
})

test_that("request policy requires a finite positive timeout", {
  expect_error(
    new_llm(
      identity, provider = "mock",
      request_policy = list(
        timeout_scope = "http_attempt_absolute", timeout_seconds = NULL,
        transport_max_tries = 1L
      )
    ),
    class = "sas2r_llm_error"
  )
  expect_error(
    new_llm(
      identity, provider = "mock",
      request_policy = list(
        timeout_scope = "http_attempt_absolute", timeout_seconds = Inf,
        transport_max_tries = 1L
      )
    ),
    class = "sas2r_llm_error"
  )
})

test_that("request policy requires positive whole transport retries", {
  expect_error(
    new_llm(
      identity, provider = "mock",
      request_policy = list(
        timeout_scope = "http_attempt_absolute", timeout_seconds = 300,
        transport_max_tries = NULL
      )
    ),
    class = "sas2r_llm_error"
  )
  expect_error(
    new_llm(
      identity, provider = "mock",
      request_policy = list(
        timeout_scope = "http_attempt_absolute", timeout_seconds = 300,
        transport_max_tries = 0L
      )
    ),
    class = "sas2r_llm_error"
  )
})

test_that("request policy rejects transport retries beyond R integer capacity", {
  expect_error(
    new_llm(
      identity, provider = "mock",
      request_policy = list(
        timeout_scope = "http_attempt_absolute", timeout_seconds = 300,
        transport_max_tries = .Machine$integer.max + 1
      )
    ),
    class = "sas2r_llm_error"
  )
})

test_that("request policy accepts R's largest integer transport retry count", {
  llm <- new_llm(
    identity, provider = "mock",
    request_policy = list(
      timeout_scope = "http_attempt_absolute", timeout_seconds = 300,
      transport_max_tries = .Machine$integer.max
    )
  )

  expect_identical(
    llm$request_policy$transport_max_tries,
    .Machine$integer.max
  )
})

test_that("caller audit context cannot forge request policy", {
  seen_context <- NULL
  llm <- new_llm(
    function(request, audit_context = list()) {
      seen_context <<- audit_context
      list(type = "final", data = list(ok = TRUE))
    },
    provider = "mock", model = "fixture-model"
  )

  response <- attempt_llm_request(
    llm_request(messages = list(list(role = "user", content = "ping"))),
    llm, new_usage_budget(),
    audit_context = list(
      timeout_scope = "idle", timeout_seconds = Inf,
      transport_max_tries = 99L
    )
  )

  expect_identical(response$status, "completed")
  expect_false(any(
    LLM_REQUEST_POLICY_FIELDS %in% names(seen_context)
  ))
})

test_that("ellmer rejects combined tools and schema before construction", {
  request <- llm_request(
    messages = list(list(role = "user", content = "unit")),
    tools = list(list(name = "lookup")),
    output_schema = list(type = "object")
  )

  expect_error(
    ellmer_transport_request(
      list(provider = "not-resolved"), request,
      model = "not-resolved", params = list()
    ),
    class = "sas2r_llm_capability_error"
  )
})

test_that("the probe honors the configured output ceiling and reasoning headroom", {
  log_dir <- withr::local_tempdir()
  pong <- list(type = "final", data = list(ok = TRUE))

  probe_ceiling <- function(model_parameters) {
    seen <- NULL
    llm <- new_llm(function(request) {
      seen <<- request$parameters$max_output_tokens
      normalize_provider_response(pong, request = request, provider = "mock")
    }, provider = "mock")
    if (length(model_parameters)) llm$model_parameters <- model_parameters
    expect_true(sas_llm_probe(llm, log_dir = log_dir))
    seen
  }

  # A configured ceiling on the adapter's model parameters is what the live
  # request may use; falling to the 32-token floor made a reasoning model
  # spend the whole ping thinking and read as offline despite valid auth.
  expect_identical(probe_ceiling(list(max_output_tokens = 16384,
                                      reasoning_effort = "high")), 16384L)
  # Reasoning without an explicit ceiling still needs thinking headroom.
  expect_identical(probe_ceiling(list(reasoning_effort = "high")), 2048L)
  # No ceiling, no reasoning: the tight floor stands.
  expect_identical(probe_ceiling(list()), 32L)
})
