test_that("CI runs the production adapter against minimum and current ellmer", {
  workflow_path <- test_path("..", "..", ".github", "workflows", "check.yml")
  skip_if_not(file.exists(workflow_path), "source-only workflow contract")

  workflow <- yaml::read_yaml(workflow_path)
  job <- workflow$jobs[["real-ellmer-contract"]]
  expect_type(job, "list")
  if (is.null(job)) return(invisible(NULL))

  matrix <- job$strategy$matrix$include
  versions <- vapply(matrix, `[[`, "", "ellmer-version")
  install_modes <- stats::setNames(
    vapply(matrix, function(row) row[["ellmer-install"]] %||% "", character(1)),
    versions
  )
  expect_setequal(versions, c("0.4.2", "current"))
  expect_identical(unname(install_modes[["0.4.2"]]), "archive")
  expect_identical(unname(install_modes[["current"]]), "current")

  # `chat_github()` requires gitcreds before any credential lookup runs, so
  # the contract job cannot construct a GitHub chat without it.
  extra_packages <- paste(vapply(job$steps, function(step) {
    step$with[["extra-packages"]] %||% ""
  }, character(1)), collapse = "\n")
  expect_match(extra_packages, "any::gitcreds", fixed = TRUE)
  expect_match(extra_packages, "any::processx", fixed = TRUE)

  run_steps <- vapply(job$steps, function(step) step$run %||% "", character(1))
  expect_true(any(grepl("remotes::install_version", run_steps, fixed = TRUE)))
  expect_true(any(grepl('install.packages("ellmer"', run_steps, fixed = TRUE)))
  expect_true(any(grepl(
    "tests/real-ellmer/contract.R", run_steps, fixed = TRUE
  )))
  expect_false(any(grepl("fixtures/ellmer-s7", run_steps, fixed = TRUE)))
  contract_path <- test_path("..", "real-ellmer", "contract.R")
  server_path <- test_path("..", "real-ellmer", "replay_server.py")
  llm_path <- test_path("..", "..", "R", "llm.R")
  expect_true(file.exists(contract_path))
  expect_true(file.exists(server_path))
  expect_true(file.exists(llm_path))

  contract <- paste(readLines(contract_path, warn = FALSE), collapse = "\n")
  server <- paste(readLines(server_path, warn = FALSE), collapse = "\n")
  llm_source <- paste(readLines(llm_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("with_mocked_bindings", contract, fixed = TRUE))
  expect_false(grepl("offline_chat_openai", contract, fixed = TRUE))
  # No agreed provider constructor or inventory export may be replaced by a
  # mock: only credential lookups are ever mocked.
  for (export in c(
    "chat_openai", "chat_anthropic", "chat_aws_bedrock", "chat_azure_openai",
    "chat_databricks", "chat_deepseek", "chat_github", "chat_google_gemini",
    "chat_google_vertex", "chat_ollama", "chat_posit", "chat_snowflake",
    "models_openai", "models_anthropic", "models_aws_bedrock",
    "models_deepseek", "models_github", "models_google_gemini",
    "models_google_vertex", "models_ollama", "models_posit"
  )) {
    expect_false(grepl(paste0(export, " ="), contract, fixed = TRUE),
                 info = export)
  }
  expect_true(grepl("locate_aws_credentials =", contract, fixed = TRUE))
  expect_true(grepl("default_google_credentials =", contract, fixed = TRUE))
  for (credential_mock in c(
    "anthropic_key =", "deepseek_key =", "github_key ="
  )) {
    expect_true(grepl(credential_mock, contract, fixed = TRUE),
                info = credential_mock)
  }
  for (handwritten_method in c(
    "set_turns = function", "register_tool = function", "chat = function",
    "chat_structured = function", "last_turn = function",
    "get_tokens = function", "get_cost = function"
  )) {
    expect_false(grepl(handwritten_method, contract, fixed = TRUE))
  }
  expect_true(grepl("ellmer::chat_openai", contract, fixed = TRUE))
  for (export in c(
    "chat_aws_bedrock", "models_aws_bedrock", "chat_azure_openai",
    "models_azure_openai", "chat_google_vertex", "models_google_vertex",
    "chat_anthropic", "models_anthropic", "chat_databricks",
    "models_databricks", "chat_deepseek", "models_deepseek", "chat_github",
    "models_github", "chat_google_gemini", "models_google_gemini",
    "chat_posit", "models_posit", "chat_snowflake", "models_snowflake"
  )) {
    expect_true(grepl(export, contract, fixed = TRUE), info = export)
  }
  # The registry/ellmer drift and public-formal guards.
  expect_true(grepl("llm_provider_registry()", contract, fixed = TRUE))
  expect_true(grepl("getNamespaceExports(\"ellmer\")", contract, fixed = TRUE))
  expect_true(grepl("published_formals", contract, fixed = TRUE))
  expect_true(grepl(
    "GitHub chat and inventory defaults converged", contract, fixed = TRUE
  ))
  expect_true(grepl("cache_options", contract, fixed = TRUE))
  expect_true(grepl("inventory_unavailable", contract, fixed = TRUE))
  expect_true(grepl("assert_public_formals", contract, fixed = TRUE))
  expect_true(grepl("offline_credentials_boundary", contract, fixed = TRUE))
  expect_true(grepl("cloud constructor returned no chat object", contract, fixed = TRUE))
  expect_true(grepl("replay_server.py", contract, fixed = TRUE))
  expect_true(grepl("127.0.0.1", contract, fixed = TRUE))
  expect_true(grepl("max_completion_tokens", contract, fixed = TRUE))
  expect_true(grepl("tool_parameters", contract, fixed = TRUE))
  expect_true(grepl(
    'identical(direct$finish_reason, "success")', contract, fixed = TRUE
  ))
  expect_false(grepl(
    'identical(direct$finish_reason, "stop")', contract, fixed = TRUE
  ))
  expect_true(grepl(
    "get_turns(include_system_prompt = TRUE)", llm_source, fixed = TRUE
  ))
  expect_true(grepl("finalization_requests", contract, fixed = TRUE))
  expect_true(grepl("wire_history_positions", contract, fixed = TRUE))
  expect_true(grepl("/chat/completions", server, fixed = TRUE))
  expect_true(grepl("/responses", server, fixed = TRUE))
  expect_true(grepl("/bedrock-control/foundation-models", server, fixed = TRUE))
  # The two native, non-OpenAI protocol families the agreed providers add.
  expect_true(grepl("/anthropic/v1/messages", server, fixed = TRUE))
  expect_true(grepl(":generateContent", server, fixed = TRUE))
  expect_true(grepl("stop_reason", server, fixed = TRUE))
  expect_true(grepl("usageMetadata", server, fixed = TRUE))
  expect_true(grepl("anthropic_requests", contract, fixed = TRUE))
  expect_true(grepl("gemini_requests", contract, fixed = TRUE))
  expect_true(grepl("bedrock_control_base_url", contract, fixed = TRUE))
  expect_true(grepl(
    "production Bedrock inventory selector is not the regional control plane",
    contract, fixed = TRUE
  ))
})
