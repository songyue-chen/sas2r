test_that("all agreed ellmer providers are selectable in stable order", {
  expected <- c(
    openai = "chat_openai",
    anthropic = "chat_anthropic",
    bedrock = "chat_aws_bedrock",
    azure = "chat_azure_openai",
    databricks = "chat_databricks",
    deepseek = "chat_deepseek",
    github = "chat_github",
    gemini = "chat_google_gemini",
    vertex = "chat_google_vertex",
    ollama = "chat_ollama",
    posit = "chat_posit",
    snowflake = "chat_snowflake"
  )
  registry <- sas2r:::llm_provider_registry()
  expect_identical(names(registry), names(expected))
  expect_identical(
    unname(vapply(registry, `[[`, "", "chat_export")), unname(expected)
  )
  expect_identical(sas2r:::LLM_PROVIDERS, names(expected))
  expect_false("openai_compatible" %in% names(registry))
  expect_identical(sas2r:::llm_provider_ids(), names(expected))
  expect_true(all(vapply(registry, inherits, logical(1),
                         "sas2r_llm_provider_spec")))
})

test_that("agreed provider configs normalize without embedding API keys", {
  withr::local_envvar(c(
    ANTHROPIC_API_KEY = "offline-anthropic-key",
    DEEPSEEK_API_KEY = "offline-deepseek-key",
    GITHUB_PAT = "offline-github-token",
    GOOGLE_API_KEY = "offline-gemini-key"
  ))
  configs <- list(
    anthropic = list(provider = "anthropic", auth_mode = "api_key",
                     model = "model-a", cache = "none"),
    databricks = list(provider = "databricks", auth_mode = "ambient",
                      model = "model-a",
                      workspace = "https://example.cloud.databricks.com"),
    deepseek = list(provider = "deepseek", auth_mode = "api_key",
                    model = "model-a"),
    github = list(provider = "github", auth_mode = "api_key",
                  model = "model-a"),
    gemini = list(provider = "gemini", auth_mode = "api_key",
                  model = "model-a"),
    posit = list(provider = "posit", auth_mode = "ambient",
                 model = "model-a", cache = "none"),
    snowflake = list(provider = "snowflake", auth_mode = "ambient",
                     model = "model-a", account = "org-account")
  )
  normalized <- lapply(configs, sas2r:::normalize_llm_config)
  expect_identical(
    unname(vapply(normalized, `[[`, "", "provider")), names(configs)
  )
  expect_true(all(vapply(normalized, function(x) is.null(x$api_key), logical(1))))
})

test_that("model inventory functions and unsupported inventories are explicit", {
  exports <- vapply(
    sas2r:::llm_provider_registry()[c(
      "anthropic", "databricks", "deepseek", "github",
      "gemini", "posit", "snowflake"
    )],
    function(x) {
      if (is.null(x$models_export)) NA_character_ else x$models_export
    },
    character(1)
  )
  expect_identical(unname(exports), c(
    "models_anthropic", NA, "models_deepseek", "models_github",
    "models_google_gemini", "models_posit", NA
  ))
})

test_that("DeepSeek uses fallback structured output and unknown model features stay unknown", {
  deepseek <- sas2r:::resolve_model_capabilities(
    "deepseek", "https://api.deepseek.com", "model-a"
  )
  anthropic <- sas2r:::resolve_model_capabilities(
    "anthropic", "https://api.anthropic.com/v1", "model-a"
  )
  expect_identical(deepseek$structured_output, "fallback")
  expect_identical(anthropic$temperature, "unknown")
  expect_identical(anthropic$tool_calling, "unknown")
})

test_that("each new provider declares its exact auth and selector contract", {
  registry <- sas2r:::llm_provider_registry()
  expected <- list(
    anthropic = list(
      auth_modes = "api_key", default_auth_mode = "api_key",
      config_fields = c("base_url", "credentials", "api_key", "cache"),
      credential_envs = "ANTHROPIC_API_KEY"
    ),
    databricks = list(
      auth_modes = "ambient", default_auth_mode = "ambient",
      config_fields = "workspace",
      credential_envs = "DATABRICKS_TOKEN"
    ),
    deepseek = list(
      auth_modes = "api_key", default_auth_mode = "api_key",
      config_fields = c("base_url", "credentials", "api_key"),
      credential_envs = "DEEPSEEK_API_KEY"
    ),
    github = list(
      auth_modes = "api_key", default_auth_mode = "api_key",
      config_fields = c(
        "base_url", "models_base_url", "credentials", "api_key"
      ),
      credential_envs = "GITHUB_PAT"
    ),
    gemini = list(
      auth_modes = c("ambient", "api_key"), default_auth_mode = "ambient",
      config_fields = c("base_url", "credentials", "api_key"),
      credential_envs = c("GOOGLE_API_KEY", "GEMINI_API_KEY")
    ),
    posit = list(
      auth_modes = "ambient", default_auth_mode = "ambient",
      config_fields = c("base_url", "credentials", "cache"),
      credential_envs = character()
    ),
    snowflake = list(
      auth_modes = "ambient", default_auth_mode = "ambient",
      config_fields = c("account", "credentials"),
      credential_envs = c("SNOWFLAKE_TOKEN", "SNOWFLAKE_PRIVATE_KEY")
    )
  )
  for (id in names(expected)) {
    spec <- registry[[id]]
    for (field in names(expected[[id]])) {
      expect_identical(spec[[field]], expected[[id]][[field]],
                       info = paste(id, field))
    }
  }
  # Non-secret tenant selectors must never be labelled as credentials.
  all_envs <- unlist(lapply(registry, `[[`, "credential_envs"), use.names = FALSE)
  expect_false(any(c(
    "DATABRICKS_HOST", "SNOWFLAKE_ACCOUNT", "SNOWFLAKE_USER",
    "DATABRICKS_CONFIG_PROFILE", "GOOGLE_CLOUD_PROJECT"
  ) %in% all_envs))
})

test_that("new providers accept only their declared selectors", {
  withr::local_envvar(c(
    ANTHROPIC_API_KEY = "offline-anthropic-key",
    GOOGLE_API_KEY = "offline-gemini-key"
  ))
  accepted <- list(
    anthropic = list(provider = "anthropic", model = "m",
                     base_url = "https://anthropic.example.invalid/v1",
                     cache = "1h"),
    databricks = list(provider = "databricks", model = "m",
                      workspace = "https://example.cloud.databricks.com"),
    deepseek = list(provider = "deepseek", model = "m",
                    base_url = "https://deepseek.example.invalid"),
    github = list(provider = "github", model = "m",
                  base_url = "https://models.example.invalid/inference/",
                  models_base_url = "https://models.example.invalid/"),
    gemini = list(provider = "gemini", model = "m",
                  base_url = "https://gemini.example.invalid/v1beta/"),
    posit = list(provider = "posit", model = "m",
                 base_url = "https://gateway.example.invalid", cache = "5m"),
    snowflake = list(provider = "snowflake", model = "m", account = "org-acct")
  )
  for (id in names(accepted)) {
    expect_type(sas2r:::normalize_llm_config(accepted[[id]]), "list")
  }

  rejected <- list(
    anthropic_workspace = list(provider = "anthropic", model = "m",
                               workspace = "https://x.example.invalid"),
    databricks_base_url = list(provider = "databricks", model = "m",
                               workspace = "https://w.example.invalid",
                               base_url = "https://x.example.invalid"),
    deepseek_cache = list(provider = "deepseek", model = "m", cache = "5m"),
    github_account = list(provider = "github", model = "m", account = "a"),
    gemini_models_base_url = list(
      provider = "gemini", model = "m",
      models_base_url = "https://x.example.invalid"
    ),
    posit_api_key = list(provider = "posit", model = "m", api_key = "k"),
    snowflake_base_url = list(provider = "snowflake", model = "m",
                              account = "a", base_url = "https://x.invalid")
  )
  for (case in names(rejected)) {
    expect_error(
      sas2r:::normalize_llm_config(rejected[[case]]),
      class = "sas2r_llm_config_error", info = case
    )
  }
})

test_that("the accepted top-level llm config key set is pinned", {
  expect_setequal(sas2r:::LLM_CONFIG_KEYS, c(
    "provider", "auth_mode", "model", "tiers", "capabilities",
    "timeout_seconds", "max_tries",
    "temperature", "top_p", "reasoning_effort", "max_output_tokens",
    "base_url", "credentials", "api_key", "cache", "profile", "region",
    "endpoint", "api_version", "workspace", "models_base_url",
    "project_id", "location", "account"
  ))
  expect_false(anyDuplicated(sas2r:::LLM_CONFIG_KEYS) > 0L)
})

test_that("new provider cache, tenant, and URL selectors are validated", {
  withr::local_envvar(c(ANTHROPIC_API_KEY = "offline-anthropic-key"))
  for (cache in c("5m", "1h", "none")) {
    expect_identical(
      sas2r:::normalize_llm_config(list(
        provider = "anthropic", model = "m", cache = cache
      ))$cache,
      cache
    )
    expect_identical(
      sas2r:::normalize_llm_config(list(
        provider = "posit", model = "m", cache = cache
      ))$cache,
      cache
    )
  }
  # Bedrock keeps its extra `auto` option; Anthropic and Posit must not.
  expect_identical(
    sas2r:::normalize_llm_config(list(
      provider = "bedrock", model = "m", region = "us-west-2", cache = "auto"
    ))$cache,
    "auto"
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "anthropic", model = "m", cache = "auto"
    )),
    "5m, 1h, or none", class = "sas2r_llm_config_error"
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "posit", model = "m", cache = "auto"
    )),
    "5m, 1h, or none", class = "sas2r_llm_config_error"
  )

  # `workspace` and `account` are normalized only when supplied, so an omitted
  # field leaves ellmer's own ambient resolution in charge.
  expect_null(
    sas2r:::normalize_llm_config(
      list(provider = "databricks", model = "m")
    )$workspace
  )
  expect_null(
    sas2r:::normalize_llm_config(
      list(provider = "snowflake", model = "m")
    )$account
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "databricks", model = "m", workspace = "example.databricks.com"
    )),
    "absolute HTTP", class = "sas2r_llm_config_error"
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "databricks", model = "m",
      workspace = "https://user:pw@example.databricks.com?token=abc"
    )),
    class = "sas2r_llm_config_error"
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "snowflake", model = "m", account = "org acct/../x"
    )),
    class = "sas2r_llm_config_error"
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "github", model = "m",
      base_url = "https://models.example.invalid/inference/",
      models_base_url = "models.example.invalid"
    )),
    "absolute HTTP", class = "sas2r_llm_config_error"
  )
})

test_that("gemini api_key mode never silently falls back to ADC or OAuth", {
  withr::local_envvar(c(GOOGLE_API_KEY = NA, GEMINI_API_KEY = NA))
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "gemini", auth_mode = "api_key", model = "m"
    )),
    "GOOGLE_API_KEY", class = "sas2r_llm_config_error"
  )
  expect_identical(
    sas2r:::normalize_llm_config(list(
      provider = "gemini", model = "m"
    ))$auth_mode,
    "ambient"
  )
  withr::local_envvar(c(GEMINI_API_KEY = "offline-gemini-key"))
  present <- sas2r:::normalize_llm_config(list(
    provider = "gemini", auth_mode = "api_key", model = "m"
  ))
  expect_null(present$api_key)
  expect_identical(present$auth_mode, "api_key")
})

test_that("new provider constructor and inventory arguments match ellmer formals", {
  withr::local_envvar(c(
    ANTHROPIC_API_KEY = "offline-anthropic-key",
    DEEPSEEK_API_KEY = "offline-deepseek-key",
    GITHUB_PAT = "offline-github-token"
  ))
  chat_args <- function(config) {
    config <- sas2r:::normalize_llm_config(config)
    names(sas2r:::ellmer_constructor_args(config, config$model))
  }
  expect_identical(
    chat_args(list(provider = "anthropic", model = "m",
                   base_url = "https://anthropic.example.invalid/v1",
                   cache = "none")),
    c("model", "base_url", "cache")
  )
  expect_identical(
    chat_args(list(provider = "databricks", model = "m",
                   workspace = "https://example.cloud.databricks.com")),
    c("model", "workspace")
  )
  expect_identical(
    chat_args(list(provider = "deepseek", model = "m")), "model"
  )
  expect_identical(
    chat_args(list(provider = "github", model = "m")), "model"
  )
  expect_identical(
    chat_args(list(provider = "posit", model = "m", cache = "1h")),
    c("model", "cache")
  )
  expect_identical(
    chat_args(list(provider = "snowflake", model = "m", account = "org-acct")),
    c("model", "account")
  )
  # An explicit key is forwarded, never copied into the normalized config.
  keyed <- sas2r:::normalize_llm_config(list(
    provider = "anthropic", auth_mode = "api_key", model = "m",
    api_key = "explicit-anthropic-key"
  ))
  expect_identical(
    sas2r:::ellmer_constructor_args(keyed, "m")$api_key,
    "explicit-anthropic-key"
  )

  inventory_args <- function(config) {
    names(sas2r:::llm_inventory_args(sas2r:::normalize_llm_config(config)))
  }
  expect_identical(
    inventory_args(list(provider = "anthropic", model = "m",
                        base_url = "https://anthropic.example.invalid/v1")),
    "base_url"
  )
  expect_identical(
    inventory_args(list(provider = "posit", model = "m",
                        base_url = "https://gateway.example.invalid")),
    "base_url"
  )
  expect_identical(inventory_args(list(provider = "gemini", model = "m")),
                   character())
  # GitHub's chat and inventory defaults are different documented paths, so a
  # custom chat base_url must never be reused as an inventory endpoint.
  expect_identical(
    inventory_args(list(
      provider = "github", model = "m",
      base_url = "https://models.example.invalid/inference/",
      models_base_url = "https://models.example.invalid/"
    )),
    "base_url"
  )
  github_custom <- sas2r:::normalize_llm_config(list(
    provider = "github", model = "m",
    base_url = "https://models.example.invalid/inference/"
  ))
  expect_false(sas2r:::llm_inventory_selector_supported(github_custom))
  expect_identical(
    sas2r:::sas_llm_models(github_custom)$status, "inventory_unavailable"
  )
  expect_true(sas2r:::llm_inventory_selector_supported(
    sas2r:::normalize_llm_config(list(provider = "github", model = "m"))
  ))
  for (id in c("databricks", "snowflake")) {
    config <- if (identical(id, "databricks")) {
      list(provider = id, model = "m",
           workspace = "https://example.cloud.databricks.com")
    } else {
      list(provider = id, model = "m", account = "org-acct")
    }
    expect_identical(
      sas2r:::sas_llm_models(config)$status, "inventory_unavailable", info = id
    )
    expect_null(sas2r:::sas_llm_models(config)$models, info = id)
  }
})

test_that("registry credential env values are redacted even when no name regex matches", {
  private_key <- "snowflake-private-key-material-a71f"
  withr::local_envvar(c(SNOWFLAKE_PRIVATE_KEY = private_key))
  # The generic environment-name regex cannot see this variable, so only the
  # registry allowlist can supply it to the redactor.
  expect_false(private_key %in% sas2r:::llm_sensitive_env_values())

  cfg <- sas2r:::normalize_llm_config(list(
    provider = "snowflake", model = "cortex-model", account = "org-acct"
  ))
  expect_false(private_key %in% unlist(cfg, use.names = FALSE))
  expect_true(private_key %in% sas2r:::llm_config_secret_values(cfg))
  expect_false(any(grepl(
    private_key, utils::capture.output(print(cfg)), fixed = TRUE
  )))
  expect_false(any(grepl(
    private_key,
    unlist(sas2r:::llm_selector_identity(cfg), use.names = FALSE),
    fixed = TRUE
  )))

  redactor <- sas2r:::new_llm_audit_redactor(sas2r:::llm_config_secret_values(cfg))
  classified <- sas2r:::classify_llm_access_error(
    simpleError(paste("provider rejected key", private_key)), cfg
  )
  public <- sas2r:::sanitize_llm_public(classified, redactor)
  expect_false(grepl(private_key, conditionMessage(public), fixed = TRUE))

  dir <- withr::local_tempdir()
  sas2r:::llm_log(
    list(error = list(message = paste("rejected", private_key))),
    dir = dir, redactor = redactor
  )
  lock <- file.path(dir, "_sas2r.lock")
  sas2r:::write_llm_lock(
    cfg, prompts_dir = dir, path = lock,
    requested_parameters = list(note = private_key),
    effective_parameters = list(note = private_key)
  )

  ledger <- file.path(dir, "usage.jsonl")
  llm <- sas2r:::new_llm(function(request) {
    stop(paste("provider rejected key", private_key))
  }, provider = "snowflake", model = "cortex-model",
  redaction_secrets = sas2r:::llm_config_secret_values(cfg))
  budget <- sas2r:::new_usage_budget(
    mode = "observe", ledger_path = ledger, run_id = "run_snowflake_secret"
  )
  sas2r:::attempt_llm_request(
    sas2r:::llm_request(
      messages = list(list(role = "user", content = "ping"))
    ), llm, budget
  )
  sas2r:::finalize_usage_run(budget, "failed")

  text <- paste(c(
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE),
    readLines(lock, warn = FALSE),
    readLines(ledger, warn = FALSE)
  ), collapse = "\n")
  expect_false(grepl(private_key, text, fixed = TRUE))
  expect_match(text, "[REDACTED]", fixed = TRUE)
  expect_match(text, "run_summary", fixed = TRUE)
  expect_match(text, "org-acct", fixed = TRUE)
})

test_that("existing provider arguments are unchanged after registry dispatch", {
  openai <- sas2r:::normalize_llm_config(list(
    provider = "openai", auth_mode = "api_key", model = "model-a",
    base_url = "https://api.example.invalid/v1"
  ))
  bedrock <- sas2r:::normalize_llm_config(list(
    provider = "bedrock", auth_mode = "ambient", model = "model-b",
    region = "us-east-1", profile = "clinical-dev", cache = "none"
  ))
  expect_identical(
    names(sas2r:::ellmer_constructor_args(openai, "model-a")),
    c("model", "base_url")
  )
  bargs <- sas2r:::ellmer_constructor_args(bedrock, "model-b")
  expect_identical(bargs$profile, "clinical-dev")
  expect_identical(bargs$cache, "none")
  expect_match(bargs$base_url, "bedrock-runtime.us-east-1")
})

test_that("configuration cannot select an arbitrary ellmer export", {
  expect_error(
    sas2r:::llm_provider_spec("chat_openai_compatible"),
    class = "sas2r_llm_config_error"
  )
  expect_error(
    sas2r:::normalize_llm_config(list(
      provider = "openai_compatible", model = "model-a",
      base_url = "https://gateway.example.invalid/v1"
    )),
    class = "sas2r_llm_config_error"
  )
})

test_that("every spec keeps the closed field contract and package-owned exports", {
  registry <- sas2r:::llm_provider_registry()
  for (id in names(registry)) {
    spec <- registry[[id]]
    expect_identical(names(spec), sas2r:::LLM_PROVIDER_SPEC_FIELDS)
    expect_identical(spec$id, id)
    expect_identical(spec$ellmer_support, "official")
    expect_match(spec$chat_export, "^chat_[a-z0-9_]+$")
    expect_true(spec$default_auth_mode %in% spec$auth_modes)
    expect_true(all(spec$auth_modes %in% c("ambient", "api_key", "none")))
    expect_false(anyDuplicated(spec$config_fields) > 0L)
    expect_false(anyDuplicated(spec$credential_envs) > 0L)
    for (callback in c(
      "build_chat_args", "build_models_args", "normalize_config",
      "inventory_available", "endpoint_identity", "api_version_identity",
      "auth_command", "access_context"
    )) {
      expect_true(is.function(spec[[callback]]), info = paste(id, callback))
    }
    if (!is.null(spec$models_export)) {
      expect_match(spec$models_export, "^models_[a-z0-9_]+$")
    }
  }
})

test_that("registry validation rejects unsafe or malformed provider specs", {
  valid <- sas2r:::new_llm_provider_spec(
    id = "openai", chat_export = "chat_openai",
    auth_modes = c("ambient", "api_key"), default_auth_mode = "api_key"
  )
  expect_silent(sas2r:::validate_llm_provider_registry(list(openai = valid)))

  broken <- list(
    duplicate_ids = list(openai = valid, openai = valid),
    name_mismatch = list(azure = valid),
    reserved_id = list(mock = sas2r:::new_llm_provider_spec(
      id = "mock", chat_export = "chat_openai",
      auth_modes = "none", default_auth_mode = "none"
    )),
    default_outside_modes = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "ambient", default_auth_mode = "api_key"
    )),
    unknown_auth_mode = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "device_code", default_auth_mode = "device_code"
    )),
    forbidden_export = list(gateway = sas2r:::new_llm_provider_spec(
      id = "gateway", chat_export = "chat_openai_compatible",
      auth_modes = "api_key", default_auth_mode = "api_key"
    )),
    community_support = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      ellmer_support = "community"
    )),
    duplicate_config_fields = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      config_fields = c("base_url", "base_url")
    )),
    shadowed_shared_field = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      config_fields = "model"
    )),
    lowercase_credential_env = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      credential_envs = "openai_api_key"
    )),
    non_function_callback = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      auth_command = "az login"
    )),
    invented_capability = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      capability_defaults = list(telepathy = "supported")
    )),
    vector_identity = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      endpoint_identity = function(config) c("a", "b")
    )),
    missing_availability = list(openai = sas2r:::new_llm_provider_spec(
      id = "openai", chat_export = "chat_openai",
      auth_modes = "api_key", default_auth_mode = "api_key",
      inventory_available = function(config) NA
    )),
    # Built raw, because `new_llm_provider_spec()` always emits the canonical
    # 17 fields in order and so can never reach the field-name/order branch.
    swapped_field_order = list(openai = local({
      fields <- unclass(sas2r:::new_llm_provider_spec(
        id = "openai", chat_export = "chat_openai",
        auth_modes = "api_key", default_auth_mode = "api_key"
      ))
      swap <- match(c("auth_modes", "default_auth_mode"), names(fields))
      order <- replace(seq_along(fields), swap, rev(swap))
      structure(fields[order], class = c("sas2r_llm_provider_spec", "list"))
    }))
  )
  swapped <- broken$swapped_field_order$openai
  expect_setequal(names(swapped), sas2r:::LLM_PROVIDER_SPEC_FIELDS)
  expect_false(identical(names(swapped), sas2r:::LLM_PROVIDER_SPEC_FIELDS))
  expect_identical(swapped$auth_modes, "api_key")
  for (case in names(broken)) {
    expect_error(
      sas2r:::validate_llm_provider_registry(broken[[case]]),
      class = "sas2r_llm_config_error", info = case
    )
  }
})

test_that("the mock capability record stays outside the production registry", {
  expect_false("mock" %in% sas2r:::llm_provider_ids())
  expect_false("mock" %in% sas2r:::LLM_PROVIDERS)
  expect_identical(
    sas2r:::adapter_capability_metadata("mock"),
    list(
      structured_output = "native", tool_calling = "native",
      tools_with_structured_output = "supported"
    )
  )
  expect_identical(sas2r:::adapter_capability_metadata("openai"), list())

  mock <- sas2r:::mock_llm(list(list(type = "final", data = list(ok = TRUE))))
  expect_identical(mock$capabilities$structured_output, "native")
  expect_identical(mock$capabilities$tool_calling, "native")
  expect_identical(mock$capabilities$tools_with_structured_output, "supported")
  expect_identical(mock$capabilities$source, "mock")
})

test_that("provider adapter defaults never invent model-specific support", {
  registry <- sas2r:::llm_provider_registry()
  # DeepSeek is the single agreed provider-wide adapter default.
  expect_identical(
    registry$deepseek$capability_defaults, list(structured_output = "fallback")
  )
  for (spec in registry[setdiff(names(registry), "deepseek")]) {
    expect_identical(spec$capability_defaults, list(), info = spec$id)
  }
  for (spec in registry) {
    expect_false(
      any(c("tool_calling", "tools_with_structured_output",
            "temperature", "top_p", "reasoning_effort", "max_output_tokens")
          %in% names(spec$capability_defaults)),
      info = spec$id
    )
  }
  deepseek <- sas2r:::resolve_model_capabilities(
    "deepseek", NULL, paste0("unlisted-", basename(tempfile())), NULL
  )
  expect_identical(deepseek$structured_output, "fallback")
  expect_identical(deepseek$tool_calling, "unknown")
  expect_identical(deepseek$temperature, "unknown")
  resolved <- sas2r:::resolve_model_capabilities(
    "openai", "https://api.example.invalid/v1",
    paste0("unlisted-", basename(tempfile())), NULL
  )
  for (field in c(
    "temperature", "top_p", "reasoning_effort", "max_output_tokens",
    "structured_output", "tool_calling", "tools_with_structured_output"
  )) {
    expect_identical(resolved[[field]], "unknown", info = field)
  }
})

test_that("ambient providers are derived from the registry, not a parallel list", {
  registry <- sas2r:::llm_provider_registry()
  ambient <- names(registry)[vapply(
    registry, function(spec) identical(spec$default_auth_mode, "ambient"),
    logical(1)
  )]
  expect_setequal(ambient, c(
    "bedrock", "azure", "vertex", "databricks", "gemini", "posit", "snowflake"
  ))
  expect_false(exists("LLM_AMBIENT_PROVIDERS", envir = asNamespace("sas2r"),
                      inherits = FALSE))

  expect_identical(
    sas2r:::normalize_llm_config(list(
      provider = "bedrock", model = "m", region = "us-west-2"
    ))$auth_mode,
    "ambient"
  )
  expect_identical(
    sas2r:::normalize_llm_config(list(
      provider = "openai", model = "m"
    ))$auth_mode,
    "api_key"
  )
  expect_identical(
    sas2r:::normalize_llm_config(list(
      provider = "ollama", model = "m", base_url = "http://localhost:11434"
    ))$auth_mode,
    "none"
  )
})

test_that("registry dispatch preserves every provider auth_mode rejection", {
  cases <- list(
    list(
      config = list(provider = "ollama", model = "m",
                    base_url = "http://localhost:11434", auth_mode = "ambient"),
      message = "Ollama llm config requires auth_mode none"
    ),
    list(
      config = list(provider = "openai", model = "m", auth_mode = "none"),
      message = "OpenAI llm config requires ambient or api_key auth_mode"
    ),
    list(
      config = list(provider = "azure", model = "m", auth_mode = "none",
                    endpoint = "https://example.openai.azure.com",
                    api_version = "v1"),
      message = "Cloud LLM provider .*azure.* requires ambient or api_key"
    ),
    list(
      config = list(provider = "bedrock", model = "m", region = "us-west-2",
                    auth_mode = "none"),
      message = "Cloud LLM provider .*bedrock.* requires ambient or api_key"
    ),
    list(
      config = list(provider = "vertex", model = "m", project_id = "p",
                    location = "us-central1", auth_mode = "api_key"),
      message = "supports ambient authentication only"
    ),
    list(
      config = list(provider = "azure", model = "m", auth_mode = "device_code",
                    endpoint = "https://example.openai.azure.com",
                    api_version = "v1"),
      message = "auth_mode must be one of ambient, api_key, or none"
    )
  )
  for (case in cases) {
    expect_error(
      sas2r:::normalize_llm_config(case$config), case$message,
      class = "sas2r_llm_config_error"
    )
  }
})

test_that("a rejected auth_mode names only the provider's declared modes", {
  # An ambient-only provider must never be told to try `api_key`, and a
  # key-only provider must never be told to try ambient authentication.
  ambient_only <- tryCatch(
    sas2r:::normalize_llm_config(list(
      provider = "snowflake", model = "m", account = "org-acct",
      auth_mode = "none"
    )),
    error = identity
  )
  expect_s3_class(ambient_only, "sas2r_llm_config_error")
  message <- conditionMessage(ambient_only)
  expect_match(message, "ambient", fixed = TRUE)
  expect_false(grepl("api_key auth_mode", message, fixed = TRUE))
  expect_false(grepl("requires ambient or api_key", message, fixed = TRUE))

  for (provider in c("databricks", "posit")) {
    config <- list(provider = provider, model = "m", auth_mode = "api_key")
    if (identical(provider, "databricks")) {
      config$workspace <- "https://example.cloud.databricks.com"
    }
    failure <- tryCatch(
      sas2r:::normalize_llm_config(config), error = identity
    )
    expect_s3_class(failure, "sas2r_llm_config_error")
    expect_false(
      grepl("requires ambient or api_key", conditionMessage(failure),
            fixed = TRUE),
      info = provider
    )
  }

  key_only <- tryCatch(
    sas2r:::normalize_llm_config(list(
      provider = "anthropic", model = "m", auth_mode = "ambient"
    )),
    error = identity
  )
  expect_s3_class(key_only, "sas2r_llm_config_error")
  expect_match(conditionMessage(key_only), "api_key", fixed = TRUE)
  expect_false(grepl(
    "supports ambient authentication only", conditionMessage(key_only),
    fixed = TRUE
  ))
})

test_that("inventory dispatch reads export, arguments, and availability from the spec", {
  registry <- sas2r:::llm_provider_registry()
  expect_identical(
    vapply(registry, function(spec) spec$models_export %||% NA_character_, ""),
    c(openai = "models_openai", anthropic = "models_anthropic",
      bedrock = "models_aws_bedrock", azure = NA_character_,
      databricks = NA_character_, deepseek = "models_deepseek",
      github = "models_github", gemini = "models_google_gemini",
      vertex = "models_google_vertex", ollama = "models_ollama",
      posit = "models_posit", snowflake = NA_character_)
  )

  bedrock <- sas2r:::normalize_llm_config(list(
    provider = "bedrock", region = "us-west-2", profile = "clinical-dev",
    model = "anthropic.example"
  ))
  expect_named(sas2r:::llm_inventory_args(bedrock), c("profile", "base_url"))
  expect_true(sas2r:::llm_inventory_selector_supported(bedrock))

  profile_selector <- sas2r:::normalize_llm_config(list(
    provider = "bedrock", region = "us-west-2", profile = "clinical-dev",
    model = "us.anthropic.example"
  ))
  expect_false(sas2r:::llm_inventory_selector_supported(profile_selector))

  ollama <- sas2r:::normalize_llm_config(list(
    provider = "ollama", base_url = "http://localhost:11434", model = "m"
  ))
  expect_identical(
    sas2r:::llm_inventory_args(ollama), list(base_url = "http://localhost:11434")
  )
  expect_identical(
    sas2r:::sas_llm_models(list(
      provider = "azure", endpoint = "https://example.openai.azure.com",
      api_version = "v1", model = "deployment-a"
    ))$status,
    "inventory_unavailable"
  )
})

test_that("credential_envs allowlist names only, and their values stay ephemeral", {
  registry <- sas2r:::llm_provider_registry()
  envs <- lapply(registry, `[[`, "credential_envs")
  expect_identical(envs$bedrock, c(
    "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN"
  ))
  for (id in names(envs)) {
    expect_true(all(grepl("^[A-Z][A-Z0-9_]*$", envs[[id]])), info = id)
    expect_false(any(c(
      "AWS_PROFILE", "AWS_REGION", "GOOGLE_CLOUD_PROJECT"
    ) %in% envs[[id]]), info = id)
  }

  secret <- "registry-env-credential-6f21"
  withr::local_envvar(c(AWS_SESSION_TOKEN = secret))
  cfg <- sas2r:::normalize_llm_config(list(
    provider = "bedrock", region = "us-west-2", model = "anthropic.example"
  ))
  expect_false(secret %in% unlist(cfg, use.names = FALSE))
  expect_true(secret %in% sas2r:::llm_config_secret_values(cfg))

  redactor <- sas2r:::new_llm_audit_redactor(
    sas2r:::llm_config_secret_values(cfg)
  )
  redacted <- redactor(list(message = paste("rejected", secret)))
  expect_false(grepl(secret, redacted$message, fixed = TRUE))

  dir <- withr::local_tempdir()
  lock <- file.path(dir, "_sas2r.lock")
  sas2r:::write_llm_lock(cfg, prompts_dir = dir, path = lock)
  expect_false(grepl(secret, paste(readLines(lock, warn = FALSE),
                                   collapse = "\n"), fixed = TRUE))
})

test_that("missing auth commands yield a hint instead of an empty or NULL command", {
  spec <- sas2r:::llm_provider_spec("openai")
  expect_null(spec$auth_command(list(provider = "openai")))
  expect_identical(
    sas2r:::llm_provider_spec("azure")$auth_command(list(provider = "azure")),
    "az login"
  )

  unauthorized <- simpleError("request failed")
  class(unauthorized) <- c("httr2_http_401", class(unauthorized))
  classified <- sas2r:::classify_llm_access_error(unauthorized, list(
    provider = "openai", auth_mode = "ambient", model = "model-a",
    base_url = "https://api.example.invalid/v1"
  ))
  expect_s3_class(classified, "sas2r_auth_required")
  message <- conditionMessage(classified)
  expect_false(grepl("NULL", message, fixed = TRUE))
  expect_false(grepl("``", message, fixed = TRUE))
  expect_null(classified$command)

  bedrock <- sas2r:::classify_llm_access_error(unauthorized, list(
    provider = "bedrock", auth_mode = "ambient", profile = "clinical-dev",
    region = "us-west-2", model = "anthropic.example"
  ))
  expect_identical(bedrock$command, "aws sso login --profile clinical-dev")
  expect_match(
    conditionMessage(bedrock),
    "Run `aws sso login --profile clinical-dev` yourself, then retry.",
    fixed = TRUE
  )
})

test_that("access-error context is provider specific and never leaks selectors", {
  registry <- sas2r:::llm_provider_registry()
  contexts <- vapply(registry, function(spec) {
    spec$access_context(list(provider = spec$id))
  }, "")
  expect_identical(contexts[["openai"]], "configured identity")
  expect_identical(contexts[["ollama"]], "configured identity")
  expect_identical(contexts[["bedrock"]], "profile <default>, region <unknown>")
  expect_identical(
    contexts[["azure"]], "endpoint <unknown>, deployment <unknown>"
  )
  expect_identical(contexts[["vertex"]], "project <unknown>, location <unknown>")
  expect_identical(
    contexts[["databricks"]], "workspace <unknown>, model <unknown>"
  )
  expect_identical(
    contexts[["snowflake"]],
    paste(
      "account <unknown>, model <unknown>; supported ambient credentials:",
      "SNOWFLAKE_TOKEN, SNOWFLAKE_USER with SNOWFLAKE_PRIVATE_KEY,",
      "or a Posit Connect viewer token"
    )
  )
  expect_identical(
    contexts[["posit"]],
    paste(
      "gateway <default>, model <unknown>; ellmer may complete its Posit",
      "device sign-in on the next interactive request"
    )
  )
  expect_identical(
    contexts[["anthropic"]],
    "model <unknown>, credentials from environment variable ANTHROPIC_API_KEY"
  )
  expect_identical(
    contexts[["deepseek"]],
    "model <unknown>, credentials from environment variable DEEPSEEK_API_KEY"
  )
  expect_identical(
    contexts[["github"]],
    "model <unknown>, credentials from environment variable GITHUB_PAT"
  )
  expect_identical(
    contexts[["gemini"]],
    paste(
      "model <unknown>, credentials from the ellmer Google chain",
      "(GOOGLE_API_KEY, GEMINI_API_KEY, application default credentials,",
      "or an ellmer-owned interactive sign-in)"
    )
  )
  # No diagnostic may echo a key, and only Gemini may suggest ADC, in ambient
  # mode only.
  commands <- lapply(registry, function(spec) {
    spec$auth_command(list(provider = spec$id, auth_mode = spec$default_auth_mode))
  })
  expect_identical(
    commands[["databricks"]],
    "databricks auth login --host <configured-workspace>"
  )
  expect_identical(
    commands[["gemini"]], "gcloud auth application-default login"
  )
  expect_null(sas2r:::llm_provider_spec("gemini")$auth_command(list(
    provider = "gemini", auth_mode = "api_key"
  )))
  for (id in c("anthropic", "deepseek", "github", "posit", "snowflake")) {
    expect_null(commands[[id]], info = id)
  }
})

test_that("prompt cache defaults to 1h when unset; migrations outlive a 5m TTL", {
  anthropic <- normalize_llm_config(list(
    provider = "anthropic", auth_mode = "api_key", api_key = "k", model = "m"))
  expect_identical(anthropic$cache, "1h")

  explicit <- normalize_llm_config(list(
    provider = "anthropic", auth_mode = "api_key", api_key = "k", model = "m",
    cache = "5m"))
  expect_identical(explicit$cache, "5m")

  off <- normalize_llm_config(list(
    provider = "anthropic", auth_mode = "api_key", api_key = "k", model = "m",
    cache = "none"))
  expect_identical(off$cache, "none")

  posit <- normalize_llm_config(list(provider = "posit", model = "m"))
  expect_identical(posit$cache, "1h")

  # Bedrock keeps ellmer's adaptive auto default when unset.
  bedrock <- normalize_llm_config(list(
    provider = "bedrock", model = "m", region = "us-east-1"))
  expect_null(bedrock$cache)
})
