test_that("sas_llm_models returns the inventory under the selected identity", {
  seen <- NULL
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) {
      seen <<- config
      c("model-a", "model-b")
    },
    .package = "sas2r"
  )

  result <- sas_llm_models(list(
    provider = "bedrock", profile = "clinical-dev", region = "us-west-2",
    model = "model-a"
  ))

  expect_identical(result$status, "available")
  expect_identical(result$models, c("model-a", "model-b"))
  expect_identical(result$identity$profile, "clinical-dev")
  expect_identical(seen$auth_mode, "ambient")
})

test_that("inventory selectors match the public ellmer cloud functions", {
  bedrock <- llm_inventory_args(normalize_llm_config(list(
    provider = "bedrock", region = "us-west-2", profile = "clinical-dev",
    model = "model-a"
  )))
  vertex <- llm_inventory_args(normalize_llm_config(list(
    provider = "vertex", project_id = "project-a",
    location = "us-central1", model = "model-b"
  )))

  expect_named(bedrock, c("profile", "base_url"))
  expect_identical(
    bedrock$base_url,
    "https://bedrock.us-west-2.amazonaws.com"
  )
  expect_named(vertex, c("project_id", "location"))
})

test_that("Bedrock custom runtime endpoints do not imply a control-plane inventory", {
  cfg <- normalize_llm_config(list(
    provider = "bedrock", base_url = "https://bedrock-runtime.example.invalid",
    profile = "clinical-dev", model = "anthropic.example"
  ))

  expect_null(llm_bedrock_inventory_base_url(cfg))
  expect_false(llm_inventory_selector_supported(cfg))
})

test_that("sas_llm_models rejects a missing configuration cleanly", {
  expect_error(sas_llm_models(NULL), class = "sas2r_llm_config_error")
})

test_that("provider registry rejects unknown output schema dialects", {
  # Break caught: an unvalidated dialect silently falls back to standard
  # behavior and can send an incompatible native schema to the provider.
  registry <- build_llm_provider_registry()
  registry$openai$output_schema_dialect <- "provider-specific-special-case"

  expect_error(
    validate_llm_provider_registry(registry),
    "supported output schema dialect",
    class = "sas2r_llm_config_error"
  )
})

test_that("new provider inventory selectors match the public ellmer formals", {
  withr::local_envvar(c(
    ANTHROPIC_API_KEY = "offline-anthropic-key",
    DEEPSEEK_API_KEY = "offline-deepseek-key",
    GITHUB_PAT = "offline-github-token"
  ))
  seen <- list()
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) {
      seen[[config$provider]] <<- llm_inventory_args(config)
      c("model-a")
    },
    .package = "sas2r"
  )
  configs <- list(
    anthropic = list(provider = "anthropic", model = "model-a"),
    deepseek = list(provider = "deepseek", model = "model-a",
                    base_url = "https://deepseek.example.invalid"),
    github = list(provider = "github", model = "model-a",
                  models_base_url = "https://models.example.invalid/"),
    gemini = list(provider = "gemini", model = "model-a",
                  credentials = function() "offline-gemini-token"),
    posit = list(provider = "posit", model = "model-a",
                 credentials = function() "offline-posit-token")
  )
  for (id in names(configs)) {
    expect_identical(sas_llm_models(configs[[id]])$status, "available", info = id)
  }
  expect_length(seen$anthropic, 0L)
  expect_identical(
    seen$deepseek, list(base_url = "https://deepseek.example.invalid")
  )
  expect_identical(
    seen$github, list(base_url = "https://models.example.invalid/")
  )
  expect_named(seen$gemini, "credentials")
  expect_named(seen$posit, "credentials")
  expect_true(is.function(seen$posit$credentials))
})

test_that("tenant selectors are visible but never carry credential material", {
  token <- "databricks-callback-token-77c1"
  private_key <- "snowflake-key-material-3e0b"
  withr::local_envvar(c(
    DATABRICKS_TOKEN = token, SNOWFLAKE_PRIVATE_KEY = private_key
  ))
  databricks <- normalize_llm_config(list(
    provider = "databricks", model = "databricks-model",
    workspace = "https://example.cloud.databricks.com"
  ))
  snowflake <- normalize_llm_config(list(
    provider = "snowflake", model = "cortex-model", account = "org-acct"
  ))
  github <- normalize_llm_config(list(
    provider = "github", model = "gh-model",
    base_url = "https://models.example.invalid/inference/",
    models_base_url = "https://models.example.invalid/"
  ))

  expect_identical(
    llm_selector_identity(databricks)$workspace,
    "https://example.cloud.databricks.com"
  )
  expect_identical(llm_selector_identity(snowflake)$account, "org-acct")
  expect_identical(
    llm_selector_identity(github)$models_base_url,
    "https://models.example.invalid/"
  )
  expect_identical(
    ellmer_endpoint_identity(databricks), "https://example.cloud.databricks.com"
  )
  expect_identical(ellmer_endpoint_identity(snowflake), "snowflake:org-acct")

  # A raw (not yet normalized) selector still loses user info and query data.
  raw <- llm_selector_identity(list(
    provider = "databricks", auth_mode = "ambient", model = "m",
    workspace = paste0("https://svc:", token, "@example.databricks.com?t=", token)
  ))
  expect_false(grepl(token, raw$workspace, fixed = TRUE))
  expect_identical(raw$workspace, "https://example.databricks.com")

  # Each configuration redacts exactly the credential variables its own
  # registry entry allowlists.
  secrets <- list(databricks = token, snowflake = private_key)
  for (cfg in list(databricks, snowflake)) {
    secret <- secrets[[cfg$provider]]
    expect_true(secret %in% llm_config_secret_values(cfg), info = cfg$provider)
    text <- paste(utils::capture.output(print(
      redact_llm_secrets(
        list(identity = llm_selector_identity(cfg),
             note = paste("provider said", secret)),
        secret_values = llm_config_secret_values(cfg)
      )
    )), collapse = "\n")
    expect_false(grepl(secret, text, fixed = TRUE), info = cfg$provider)
    expect_match(text, "[REDACTED]", fixed = TRUE, info = cfg$provider)
  }
  expect_false(private_key %in% llm_config_secret_values(databricks))
})

test_that("an omitted workspace or account still identifies its own tenant", {
  # ellmer resolves `workspace`/`account` ambiently when they are omitted, so
  # requiring them would make that documented discovery unusable. The identity
  # reads the same non-secret selector instead, or two tenants would share one
  # capability cache entry.
  withr::local_envvar(c(
    DATABRICKS_HOST = "https://tenant-a.cloud.databricks.com",
    SNOWFLAKE_ACCOUNT = "tenant-a-account"
  ))
  databricks <- normalize_llm_config(list(provider = "databricks", model = "m"))
  snowflake <- normalize_llm_config(list(provider = "snowflake", model = "m"))
  expect_null(databricks$workspace)
  expect_null(snowflake$account)
  expect_identical(
    ellmer_endpoint_identity(databricks),
    "https://tenant-a.cloud.databricks.com"
  )
  expect_identical(
    ellmer_endpoint_identity(snowflake), "snowflake:tenant-a-account"
  )

  # The constructor receives no `workspace`/`account` argument at all, so
  # ellmer's own public default applies.
  expect_identical(
    names(ellmer_constructor_args(databricks, model = "m")), "model"
  )
  expect_identical(
    names(ellmer_constructor_args(snowflake, model = "m")), "model"
  )

  # A second tenant is a different capability cache key.
  keys <- c(
    capability_cache_key(
      "databricks", ellmer_endpoint_identity(databricks), "m", NULL
    ),
    capability_cache_key(
      "snowflake", ellmer_endpoint_identity(snowflake), "m", NULL
    )
  )
  withr::with_envvar(
    c(
      DATABRICKS_HOST = "https://tenant-b.cloud.databricks.com",
      SNOWFLAKE_ACCOUNT = "tenant-b-account"
    ),
    {
      other_databricks <- normalize_llm_config(
        list(provider = "databricks", model = "m")
      )
      other_snowflake <- normalize_llm_config(
        list(provider = "snowflake", model = "m")
      )
      expect_identical(
        ellmer_endpoint_identity(other_snowflake), "snowflake:tenant-b-account"
      )
      expect_false(capability_cache_key(
        "databricks", ellmer_endpoint_identity(other_databricks), "m", NULL
      ) %in% keys)
      expect_false(capability_cache_key(
        "snowflake", ellmer_endpoint_identity(other_snowflake), "m", NULL
      ) %in% keys)
    }
  )

  # An explicit selector always wins over the ambient one.
  explicit <- normalize_llm_config(list(
    provider = "databricks", model = "m",
    workspace = "https://explicit.cloud.databricks.com"
  ))
  expect_identical(
    ellmer_endpoint_identity(explicit), "https://explicit.cloud.databricks.com"
  )
})

test_that("ambient tenant identity carries no credential or URL secret data", {
  token <- "databricks-host-embedded-token-6f21"
  withr::local_envvar(c(
    DATABRICKS_HOST = paste0(
      "svc:", token, "@tenant-c.cloud.databricks.com?token=", token, "#", token
    ),
    SNOWFLAKE_ACCOUNT = "tenant-c-account",
    DATABRICKS_TOKEN = token
  ))
  databricks <- normalize_llm_config(list(provider = "databricks", model = "m"))
  identity <- ellmer_endpoint_identity(databricks)
  expect_identical(identity, "https://tenant-c.cloud.databricks.com")
  expect_false(grepl(token, identity, fixed = TRUE))
  expect_false(grepl(token, capability_cache_key(
    "databricks", identity, "m", NULL
  ), fixed = TRUE))
  expect_false(any(grepl(
    token, unlist(llm_selector_identity(databricks), use.names = FALSE),
    fixed = TRUE
  )))
  # The non-secret selector is read for identity only; it is never captured
  # into the configuration and never becomes an allowlisted credential name.
  expect_null(databricks$workspace)
  all_envs <- unlist(
    lapply(sas2r:::llm_provider_registry(), `[[`, "credential_envs"),
    use.names = FALSE
  )
  expect_false("DATABRICKS_HOST" %in% all_envs)
  expect_false("SNOWFLAKE_ACCOUNT" %in% all_envs)

  # A selector that is not a plain account or a safe URL is no identity at all.
  withr::with_envvar(
    c(SNOWFLAKE_ACCOUNT = "tenant c/../other", DATABRICKS_HOST = "not a url"),
    {
      expect_null(ellmer_endpoint_identity(
        normalize_llm_config(list(provider = "snowflake", model = "m"))
      ))
      expect_null(ellmer_endpoint_identity(
        normalize_llm_config(list(provider = "databricks", model = "m"))
      ))
    }
  )
})

test_that("ambient credential failures for new providers classify without leaking", {
  token <- "databricks-ambient-token-51ab"
  private_key <- "snowflake-ambient-key-90fd"
  withr::local_envvar(c(
    DATABRICKS_TOKEN = token, SNOWFLAKE_PRIVATE_KEY = private_key
  ))
  cases <- list(
    databricks = list(
      config = list(provider = "databricks", model = "databricks-model",
                    workspace = "https://example.cloud.databricks.com"),
      detail = paste("the OAuth session expired for token", token),
      class = "sas2r_auth_required", reason = "expired_login",
      secret = token
    ),
    snowflake = list(
      config = list(provider = "snowflake", model = "cortex-model",
                    account = "org-acct"),
      detail = paste("access denied for private key", private_key),
      class = "sas2r_llm_permission_denied", reason = "permission_denied",
      secret = private_key
    ),
    posit = list(
      config = list(provider = "posit", model = "posit-model"),
      detail = "login required for the Posit gateway",
      class = "sas2r_auth_required", reason = "not_logged_in",
      secret = NULL
    )
  )
  for (id in names(cases)) {
    case <- cases[[id]]
    cfg <- normalize_llm_config(case$config)
    classified <- classify_llm_access_error(simpleError(case$detail), cfg)
    expect_s3_class(classified, case$class)
    expect_identical(classified$reason, case$reason)
    redactor <- new_llm_audit_redactor(llm_config_secret_values(cfg))
    public <- sanitize_llm_public(classified, redactor)
    message <- conditionMessage(public)
    if (!is.null(case$secret)) {
      expect_false(grepl(case$secret, message, fixed = TRUE), info = id)
    }
    expect_false(grepl("NULL", message, fixed = TRUE), info = id)
    expect_false(grepl("``", message, fixed = TRUE), info = id)
  }
  expect_match(
    conditionMessage(classify_llm_access_error(
      simpleError("login required"),
      normalize_llm_config(cases$posit$config)
    )),
    "device sign-in", fixed = TRUE
  )
  expect_match(
    conditionMessage(classify_llm_access_error(
      simpleError("login required"),
      normalize_llm_config(cases$databricks$config)
    )),
    "databricks auth login --host https://example.cloud.databricks.com",
    fixed = TRUE
  )
})

test_that("key-only providers point at their environment variable, never a key", {
  unauthorized <- simpleError("request failed")
  class(unauthorized) <- c("httr2_http_401", class(unauthorized))

  ambient_key <- "anthropic-ambient-key-88b0"
  withr::local_envvar(c(ANTHROPIC_API_KEY = ambient_key))
  env_cfg <- normalize_llm_config(list(
    provider = "anthropic", model = "claude-model"
  ))
  env_classified <- classify_llm_access_error(unauthorized, env_cfg)
  expect_s3_class(env_classified, "sas2r_llm_authentication_error")
  env_message <- conditionMessage(sanitize_llm_public(
    env_classified, new_llm_audit_redactor(llm_config_secret_values(env_cfg))
  ))
  expect_match(env_message, "ANTHROPIC_API_KEY", fixed = TRUE)
  expect_false(grepl(ambient_key, env_message, fixed = TRUE))
  expect_null(env_classified$command)
  expect_false(grepl("gcloud", env_message, fixed = TRUE))

  key <- "anthropic-explicit-key-4d17"
  cfg <- normalize_llm_config(list(
    provider = "anthropic", auth_mode = "api_key", model = "claude-model",
    api_key = key
  ))
  classified <- classify_llm_access_error(unauthorized, cfg)
  redactor <- new_llm_audit_redactor(llm_config_secret_values(cfg))
  message <- conditionMessage(sanitize_llm_public(classified, redactor))
  expect_false(grepl(key, message, fixed = TRUE))
  expect_match(message, "the configured api_key", fixed = TRUE)
  expect_null(classified$command)
})

test_that("Azure inventory absence is not reported as an empty model list", {
  result <- sas_llm_models(list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "2025-04-01-preview", model = "deployment-a"
  ))

  expect_identical(result$status, "inventory_unavailable")
  expect_null(result$models)
  expect_identical(result$configured_model, "deployment-a")
})

test_that("ambient auth failures are classed and only recommend user-run login", {
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) {
      error <- simpleError("The SSO session associated with this profile has expired")
      class(error) <- c("paws_expired_credentials", class(error))
      stop(error)
    },
    .package = "sas2r"
  )

  error <- tryCatch(
    sas_llm_models(list(
      provider = "bedrock", profile = "clinical-dev", region = "us-west-2",
      model = "model-a"
    )),
    error = identity
  )
  expect_s3_class(error, "sas2r_auth_required")
  expect_identical(error$reason, "expired_login")
  expect_match(conditionMessage(error), "aws sso login --profile clinical-dev", fixed = TRUE)
})

test_that("HTTP auth status and missing credential conditions classify safely", {
  cfg <- list(
    provider = "vertex", project_id = "project-a", location = "us-central1",
    model = "gemini-example"
  )
  unauthorized <- simpleError("request failed")
  class(unauthorized) <- c("httr2_http_401", class(unauthorized))
  forbidden <- simpleError("request failed")
  class(forbidden) <- c("httr2_http_403", class(forbidden))
  missing <- simpleError("could not load application default credentials")

  expect_s3_class(
    classify_llm_access_error(unauthorized, cfg), "sas2r_auth_required"
  )
  expect_s3_class(
    classify_llm_access_error(forbidden, cfg), "sas2r_llm_permission_denied"
  )
  expect_s3_class(
    classify_llm_access_error(missing, cfg), "sas2r_auth_required"
  )
})

test_that("auth diagnostics consume status fields and nested response status", {
  cfg <- list(
    provider = "vertex", auth_mode = "ambient", project_id = "project-a",
    location = "us-central1", model = "gemini-example"
  )
  conditions <- list(
    structure(
      list(message = "request rejected", status_code = 401L, call = NULL),
      class = c("google_auth_error", "error", "condition")
    ),
    structure(
      list(message = "access token expired", status = 401L, call = NULL),
      class = c("azure_identity_error", "error", "condition")
    ),
    structure(
      list(
        message = "request rejected", response = list(status_code = 401L),
        call = NULL
      ),
      class = c("paws_http_error", "error", "condition")
    ),
    structure(
      list(message = "request rejected", resp = list(status = 401L), call = NULL),
      class = c("provider_http_error", "error", "condition")
    )
  )

  for (condition in conditions) {
    expect_s3_class(
      classify_llm_access_error(condition, cfg), "sas2r_auth_required"
    )
  }
})

test_that("API-key authentication errors never recommend an ambient login", {
  cfg <- list(
    provider = "azure", auth_mode = "api_key",
    endpoint = "https://example.openai.azure.com", model = "deployment-a",
    api_version = "v1"
  )
  unauthorized <- simpleError("request failed")
  class(unauthorized) <- c("httr2_http_401", class(unauthorized))

  error <- classify_llm_access_error(unauthorized, cfg)
  expect_s3_class(error, "sas2r_llm_authentication_error")
  expect_false(inherits(error, "sas2r_auth_required"))
  expect_false(grepl("az login", conditionMessage(error), fixed = TRUE))
})

test_that("access diagnostics distinguish permission model region endpoint and network failures", {
  cfgs <- list(
    permission = list(provider = "bedrock", profile = "p", region = "us-west-2", model = "m"),
    model = list(provider = "azure", endpoint = "https://valid.example", api_version = "v", model = "missing"),
    region = list(provider = "bedrock", profile = "p", region = "us-west-2", model = "m"),
    endpoint = list(provider = "azure", endpoint = "not-a-url", api_version = "v", model = "m"),
    network = list(provider = "vertex", project_id = "p", location = "us-central1", model = "m")
  )
  errors <- list(
    permission = simpleError("AccessDenied: not authorized"),
    model = simpleError("deployment was not found"),
    region = simpleError("model is not available in this region"),
    endpoint = simpleError("invalid endpoint host"),
    network = simpleError("could not resolve host")
  )
  expected <- c(
    permission = "sas2r_llm_permission_denied",
    model = "sas2r_llm_model_not_found",
    region = "sas2r_llm_region_mismatch",
    endpoint = "sas2r_llm_endpoint_invalid",
    network = "sas2r_llm_network_error"
  )
  for (name in names(errors)) {
    classified <- classify_llm_access_error(errors[[name]], cfgs[[name]])
    expect_s3_class(classified, expected[[name]])
  }
})

test_that("probe preserves classed ambient authentication diagnostics", {
  llm <- new_llm(function(request) {
    error <- simpleError("Azure CLI credential has expired; please authenticate")
    class(error) <- c("azure_identity_error", class(error))
    stop(error)
  }, provider = "azure", capabilities = llm_capabilities(
    structured_output = "native", source = "test"
  ))
  attr(llm, "auth_context") <- list(
    provider = "azure", auth_mode = "ambient",
    endpoint = "https://example.openai.azure.com", model = "deployment-a",
    api_version = "2025-04-01-preview"
  )

  error <- tryCatch(
    sas_llm_probe(llm, max_retries = 1L, log_dir = withr::local_tempdir()),
    error = identity
  )
  expect_s3_class(error, "sas2r_auth_required")
  expect_match(conditionMessage(error), "az login", fixed = TRUE)
})

test_that("probe redacts public 401 diagnostics and writes one terminal audit", {
  dir <- withr::local_tempdir()
  secret <- "configured-probe-key-3be7"
  callback_secret <- "callback-probe-token-91af"
  cache_path <- "/Users/reviewer/.azure/msal_token_cache.json"
  clear_llm_registered_secrets()
  withr::defer(clear_llm_registered_secrets())
  register_llm_secret_values(callback_secret)
  llm <- new_llm(function(request) {
    condition <- structure(
      list(
        message = paste("401 rejected", secret, callback_secret, cache_path),
        status_code = 401L,
        response = list(
          status = 401L,
          error = list(message = paste("nested", secret, callback_secret))
        ),
        call = NULL
      ),
      class = c("httr2_http_401", "error", "condition")
    )
    stop(condition)
  }, provider = "azure", capabilities = llm_capabilities(
    structured_output = "native", source = "test"
  ), redaction_secrets = secret)
  attr(llm, "auth_context") <- list(
    provider = "azure", auth_mode = "api_key",
    endpoint = "https://example.openai.azure.com", model = "deployment-a",
    api_version = "v1"
  )

  error <- tryCatch(
    sas_llm_probe(llm, max_retries = 1L, log_dir = dir),
    error = identity
  )

  expect_s3_class(error, "sas2r_llm_authentication_error")
  expect_identical(error$reason, "authentication_rejected")
  public_text <- paste(c(
    conditionMessage(error), jsonlite::toJSON(unclass(error), auto_unbox = TRUE)
  ), collapse = "\n")
  for (leak in c(secret, callback_secret, cache_path)) {
    expect_false(grepl(leak, public_text, fixed = TRUE), info = leak)
  }
  expect_match(public_text, "[REDACTED]", fixed = TRUE)

  audit_path <- file.path(dir, "llm_log.jsonl")
  expect_true(file.exists(audit_path), info = "audit_exists=TRUE")
  lines <- readLines(audit_path, warn = FALSE)
  expect_length(lines, 1L)
  audit <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
  expect_identical(audit$agent, "probe")
  expect_identical(audit$type, "terminal_error")
  expect_identical(audit$status, "failed")
  expect_identical(audit$terminal_class, "sas2r_llm_authentication_error")
  expect_identical(audit$terminal_reason, "authentication_rejected")
  expect_false("messages" %in% names(audit))
  for (leak in c(secret, callback_secret, cache_path)) {
    expect_false(grepl(leak, lines[[1]], fixed = TRUE), info = leak)
  }
})

test_that("probe promotes normalized 401 and 403 outcomes to access diagnostics", {
  make_llm <- function(status) {
    llm <- new_llm(function(request) {
      condition <- structure(
        list(message = "request rejected", status_code = status, call = NULL),
        class = c(paste0("httr2_http_", status), "error", "condition")
      )
      normalize_provider_response(condition, request = request, provider = "vertex")
    }, provider = "vertex", capabilities = llm_capabilities(
      structured_output = "native", source = "test"
    ))
    attr(llm, "auth_context") <- list(
      provider = "vertex", auth_mode = "ambient", project_id = "project-a",
      location = "us-central1", model = "gemini-example"
    )
    llm
  }

  unauthorized <- tryCatch(
    sas_llm_probe(make_llm(401L), max_retries = 1L,
                  log_dir = withr::local_tempdir()),
    error = identity
  )
  forbidden <- tryCatch(
    sas_llm_probe(make_llm(403L), max_retries = 1L,
                  log_dir = withr::local_tempdir()),
    error = identity
  )
  expect_s3_class(unauthorized, "sas2r_auth_required")
  expect_s3_class(forbidden, "sas2r_llm_permission_denied")
})

test_that("probe requests one bounded structured response", {
  seen <- NULL
  llm <- new_llm(function(request) {
    seen <<- request
    list(type = "final", data = list(ok = TRUE))
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native", max_output_tokens = "supported",
    source = "test"
  ))

  expect_true(sas_llm_probe(
    llm, max_retries = 1L, log_dir = withr::local_tempdir()
  ))
  expect_identical(seen$parameters$max_output_tokens, 32L)
})

test_that("lockfiles and logs retain selectors but never credentials", {
  dir <- withr::local_tempdir()
  secret <- "sas2r-unit-secret-9f4a"
  token <- "sas2r-unit-bearer-2d71"
  withr::local_envvar(SAS2R_TEST_ACCESS_TOKEN = token)
  cfg <- list(
    provider = "azure", auth_mode = "api_key",
    endpoint = paste0(
      "https://", secret,
      "@example.openai.azure.com?access_token=", token
    ), model = "deployment-a",
    api_version = "2025-04-01-preview", api_key = secret,
    credentials = function() token
  )

  lock <- file.path(dir, "_sas2r.lock")
  write_llm_lock(cfg, prompts_dir = dir, path = lock)
  llm_log(list(config = cfg, authorization = paste("Bearer", token)), dir = dir)
  text <- paste(
    c(readLines(lock, warn = FALSE),
      readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)),
    collapse = "\n"
  )

  expect_match(text, "example.openai.azure.com", fixed = TRUE)
  expect_false(grepl(secret, text, fixed = TRUE))
  expect_false(grepl(token, text, fixed = TRUE))
  expect_false(grepl("credentials", text, ignore.case = TRUE))
  expect_false(grepl("access_token", text, ignore.case = TRUE))
})

test_that("persisted LLM artifacts redact configured secrets and cloud cache paths", {
  dir <- withr::local_tempdir()
  secret <- "configured-only-secret-66c2"
  nested_secret <- "nested-credential-secret-5b19"
  aws_cache <- "/Users/reviewer/.aws/sso/cache/0f3d-token.json"
  aws_credentials <- "/Users/reviewer/.aws/credentials"
  azure_cache <- "/home/reviewer/.azure/msal_token_cache.json"
  gcp_cache <- paste0(
    "/Users/reviewer/.config/gcloud/",
    "application_default_credentials.json"
  )
  cfg <- list(
    provider = "azure", auth_mode = "api_key",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    model = "deployment-a", api_key = secret
  )
  cfg_with_nested <- c(cfg, list(
    credential_bundle = list(access_token = nested_secret)
  ))
  detail <- paste(
    secret, nested_secret, aws_cache, aws_credentials, azure_cache, gcp_cache
  )

  lock <- file.path(dir, "_sas2r.lock")
  write_llm_lock(
    cfg_with_nested, prompts_dir = dir, path = lock,
    requested_parameters = list(note = detail),
    effective_parameters = list(note = detail)
  )
  llm_log(list(
    config = cfg_with_nested,
    error = list(message = detail, response = list(detail = detail))
  ), dir = dir)
  text <- paste(c(
    readLines(lock, warn = FALSE),
    readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
  ), collapse = "\n")

  for (leak in c(
    secret, nested_secret, aws_cache, aws_credentials, azure_cache, gcp_cache
  )) {
    expect_false(grepl(leak, text, fixed = TRUE), info = leak)
  }
  expect_match(text, "[REDACTED]", fixed = TRUE)
})

test_that("credential callback results are redacted after provider use", {
  clear_llm_registered_secrets()
  withr::defer(clear_llm_registered_secrets())
  secret <- "callback-only-credential-981d"
  cfg <- normalize_llm_config(list(
    provider = "azure", auth_mode = "ambient",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    model = "deployment-a", credentials = function() secret
  ))
  args <- ellmer_constructor_args(cfg, model = cfg$model)
  expect_length(formals(args$credentials), 0L)
  expect_identical(args$credentials(), secret)

  dir <- withr::local_tempdir()
  llm_log(list(error = list(message = paste("rejected", secret))), dir = dir)
  text <- paste(readLines(
    file.path(dir, "llm_log.jsonl"), warn = FALSE
  ), collapse = "\n")
  expect_false(grepl(secret, text, fixed = TRUE))
})

test_that("api_key and credentials conflict only when both are present", {
  clear_llm_registered_secrets()
  withr::defer(clear_llm_registered_secrets())
  secret <- "callback-only-key-mode-credential-7c15"

  # `anthropic`, `deepseek` and `github` declare `api_key` as their only auth
  # mode, so an exclusion that fired on the mode alone made the `credentials`
  # field each of them declares unreachable.
  for (provider in c("anthropic", "deepseek", "github")) {
    cfg <- normalize_llm_config(list(
      provider = provider, model = "m", credentials = function() secret
    ))
    expect_identical(cfg$auth_mode, "api_key", info = provider)
    expect_null(cfg$api_key, info = provider)
    args <- ellmer_constructor_args(cfg, model = cfg$model)
    expect_true(is.function(args$credentials), info = provider)
    expect_length(formals(args$credentials), 0L)
    expect_identical(args$credentials(), secret, info = provider)
    expect_false("api_key" %in% names(args), info = provider)
  }

  # Supplying both really is the conflict the message describes.
  expect_error(
    normalize_llm_config(list(
      provider = "anthropic", model = "m", api_key = "explicit-anthropic-key",
      credentials = function() secret
    )),
    "not both", class = "sas2r_llm_config_error"
  )

  # Azure pins each deployment to an explicit key, so a credentials-only
  # api_key config falls through to azure's own required-key rule instead of
  # being masked by the exclusion check.
  azure_key_mode <- list(
    provider = "azure", auth_mode = "api_key",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    model = "deployment-a"
  )
  expect_error(
    normalize_llm_config(c(
      azure_key_mode, list(credentials = function() secret)
    )),
    "non-empty string", class = "sas2r_llm_config_error"
  )
  # The double fault still reports the conflict, not the missing key.
  expect_error(
    normalize_llm_config(c(azure_key_mode, list(
      api_key = "explicit-azure-key", credentials = function() secret
    ))),
    "not both", class = "sas2r_llm_config_error"
  )
  # And the single-fault legacy messages are unchanged.
  expect_error(
    normalize_llm_config(azure_key_mode),
    "non-empty string", class = "sas2r_llm_config_error"
  )
  expect_error(
    normalize_llm_config(list(
      provider = "gemini", auth_mode = "ambient", model = "m",
      api_key = "unsupported-in-ambient-mode"
    )),
    "api_key may be set only when auth_mode is api_key",
    class = "sas2r_llm_config_error"
  )
})

test_that("ambient smoke scripts are inert without an explicit opt-in gate", {
  scripts <- c("bedrock-ambient.R", "azure-ambient.R", "vertex-ambient.R")
  gates <- c(
    SAS2R_SMOKE_BEDROCK = "", SAS2R_SMOKE_AZURE = "", SAS2R_SMOKE_VERTEX = ""
  )
  withr::local_envvar(gates)
  for (script in scripts) {
    path <- system.file("smoke", script, package = "sas2r")
    expect_true(nzchar(path))
    expect_error(sys.source(path, envir = new.env(parent = baseenv())), "opt in")
  }
})

test_that("ambient smoke scripts validate selectors before printing", {
  secret <- "azure-query-secret-4f88"
  withr::local_envvar(c(
    SAS2R_SMOKE_AZURE = "true",
    SAS2R_SMOKE_AZURE_ENDPOINT = paste0(
      "https://example.openai.azure.com?api_key=", secret
    ),
    SAS2R_SMOKE_AZURE_API_VERSION = "v1",
    SAS2R_SMOKE_AZURE_DEPLOYMENT = "deployment-a"
  ))
  holder <- new.env(parent = emptyenv())
  output <- capture.output({
    holder$error <- tryCatch(
      sys.source(
        system.file("smoke", "azure-ambient.R", package = "sas2r"),
        envir = new.env(parent = baseenv())
      ),
      error = identity
    )
  }, type = "output")

  expect_s3_class(holder$error, "sas2r_llm_config_error")
  expect_false(grepl(
    secret,
    paste(c(output, conditionMessage(holder$error)), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("probe validates the exact selected tier model against inventory", {
  cfg <- list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "v1",
    tiers = list(frontier = "frontier-model", cheap = "cheap-model")
  )
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) c("cheap-model"),
    ellmer_llm = function(config) mock_llm(
      list(list(type = "final", data = list(ok = TRUE))),
      capabilities = llm_capabilities(structured_output = "native", source = "test")
    ),
    .package = "sas2r"
  )

  expect_true(sas_llm_probe(
    cfg, tier = "cheap", max_retries = 1L,
    log_dir = withr::local_tempdir()
  ))
})

test_that("Bedrock inference-profile selectors are not rejected by foundation inventory", {
  inventory_calls <- 0L
  probe_calls <- 0L
  cfg <- list(
    provider = "bedrock", profile = "clinical-dev", region = "us-west-2",
    tiers = list(cheap = "us.anthropic.claude-example-v1:0")
  )
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) {
      inventory_calls <<- inventory_calls + 1L
      data.frame(modelId = "anthropic.claude-example-v1:0")
    },
    ellmer_llm = function(config) {
      new_llm(function(request) {
        probe_calls <<- probe_calls + 1L
        list(type = "final", data = list(ok = TRUE))
      }, provider = "bedrock", capabilities = llm_capabilities(
        structured_output = "native", source = "test"
      ))
    },
    .package = "sas2r"
  )

  inventory <- sas_llm_models(cfg)
  expect_identical(inventory$status, "inventory_unavailable")
  expect_null(inventory$models)
  expect_identical(inventory_calls, 0L)
  expect_true(sas_llm_probe(
    cfg, tier = "cheap", max_retries = 1L,
    log_dir = withr::local_tempdir()
  ))
  expect_identical(inventory_calls, 0L)
  expect_identical(probe_calls, 1L)
})

test_that("probe understands row inventories and rejects unknown inventory shapes", {
  cfg <- list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "v1",
    tiers = list(frontier = "frontier-model", cheap = "cheap-model")
  )
  adapter <- function(config) mock_llm(
    list(list(type = "final", data = list(ok = TRUE))),
    capabilities = llm_capabilities(structured_output = "native", source = "test")
  )

  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) data.frame(id = "frontier-model"),
    ellmer_llm = adapter,
    .package = "sas2r"
  )
  expect_error(
    sas_llm_probe(
      cfg, tier = "cheap", max_retries = 1L,
      log_dir = withr::local_tempdir()
    ),
    class = "sas2r_llm_model_not_found"
  )

  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) list(metadata = list(total = 1L)),
    ellmer_llm = adapter,
    .package = "sas2r"
  )
  expect_error(
    sas_llm_probe(
      cfg, tier = "cheap", max_retries = 1L,
      log_dir = withr::local_tempdir()
    ),
    class = "sas2r_llm_inventory_shape_error"
  )
})

test_that("probe extracts model identifiers from record-list inventories", {
  cfg <- list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "v1",
    tiers = list(frontier = "frontier-model", cheap = "cheap-model")
  )
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) list(
      list(model_id = "frontier-model", display_name = "Frontier")
    ),
    ellmer_llm = function(config) mock_llm(
      list(list(type = "final", data = list(ok = TRUE))),
      capabilities = llm_capabilities(structured_output = "native", source = "test")
    ),
    .package = "sas2r"
  )

  expect_error(
    sas_llm_probe(
      cfg, tier = "cheap", max_retries = 1L,
      log_dir = withr::local_tempdir()
    ),
    class = "sas2r_llm_model_not_found"
  )
})

read_probe_audit <- function(dir) {
  lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
  lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
}

test_that("probe audits and sanitizes configuration and capability failures", {
  secret <- "probe-boundary-secret-f7c1"
  patient <- "patient-prompt-must-not-enter-audit"

  config_dir <- withr::local_tempdir()
  config_error <- tryCatch(
    sas_llm_probe(list(
      provider = "azure", auth_mode = "api_key",
      endpoint = "https://example.openai.azure.com", model = "deployment-a",
      api_version = "v1", api_key = secret, misspelled_selector = patient
    ), max_retries = 1L, log_dir = config_dir),
    error = identity
  )
  expect_s3_class(config_error, "sas2r_llm_config_error")
  config_rows <- read_probe_audit(config_dir)
  expect_length(config_rows, 1L)
  expect_identical(config_rows[[1]]$type, "terminal_error")
  expect_identical(config_rows[[1]]$terminal_class, "sas2r_llm_config_error")
  expect_false("capability_hash" %in% names(config_rows[[1]]))
  expect_false(grepl(secret, readLines(
    file.path(config_dir, "llm_log.jsonl"), warn = FALSE
  ), fixed = TRUE))

  capability_dir <- withr::local_tempdir()
  llm <- new_llm(
    function(request) stop("transport must not be called"),
    provider = "azure",
    capabilities = llm_capabilities(structured_output = "native", source = "test"),
    capabilities_for = function(tier, model = NULL) {
      stop(structure(
        list(
          message = paste("capability callback failed", secret),
          reason = "capability_resolution_failed",
          content = patient,
          request = list(content = patient),
          call = NULL
        ),
        class = c("sas2r_llm_capability_error", "error", "condition")
      ))
    },
    redaction_secrets = secret
  )
  capability_error <- tryCatch(
    sas_llm_probe(llm, max_retries = 1L, log_dir = capability_dir),
    error = identity
  )
  expect_s3_class(capability_error, "sas2r_llm_capability_error")
  expect_identical(capability_error$reason, "capability_resolution_failed")
  expect_false(any(c("content", "request") %in% names(capability_error)))
  capability_lines <- readLines(
    file.path(capability_dir, "llm_log.jsonl"), warn = FALSE
  )
  expect_length(capability_lines, 1L)
  expect_false(grepl(secret, capability_lines, fixed = TRUE))
  expect_false(grepl(patient, capability_lines, fixed = TRUE))
  capability_audit <- jsonlite::fromJSON(
    capability_lines[[1]], simplifyVector = FALSE
  )
  expect_false("capability_hash" %in% names(capability_audit))
  expect_named(
    capability_audit$error, c("class", "reason", "message"),
    ignore.order = FALSE
  )
})

test_that("probe preserves normalized rate-limit and timeout identity", {
  patient <- "patient-response-content-must-not-enter-audit"
  cases <- list(
    list(class = "sas2r_llm_rate_limit", reason = "rate_limited"),
    list(class = "sas2r_llm_timeout", reason = "timed_out")
  )

  for (case in cases) {
    dir <- withr::local_tempdir()
    response <- new_llm_response(
      status = "failed", action = "none",
      usage = list(input_tokens = 11, output_tokens = 5),
      cost = list(amount_usd = 0.02, status = "billed_amount", source = "provider"),
      error = list(
        class = case$class, reason = case$reason,
        message = "provider request failed", content = patient,
        request = list(content = patient)
      ),
      extra_class = case$class
    )
    llm <- new_llm(
      function(request) response,
      provider = "azure",
      capabilities = llm_capabilities(structured_output = "native", source = "test")
    )
    attr(llm, "auth_context") <- list(
      provider = "azure", auth_mode = "ambient",
      endpoint = "https://example.openai.azure.com", model = "deployment-a"
    )

    error <- tryCatch(
      sas_llm_probe(llm, max_retries = 1L, log_dir = dir),
      error = identity
    )
    expect_s3_class(error, case$class)
    expect_identical(error$reason, case$reason)
    expect_equal(attr(error, "usage")$input_tokens, 11)
    expect_equal(attr(error, "cost_usd"), 0.02)

    lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
    expect_length(lines, 1L)
    expect_false(grepl(patient, lines, fixed = TRUE))
    audit <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
    expect_identical(audit$type, "terminal_error")
    expect_identical(audit$terminal_class, case$class)
    expect_identical(audit$terminal_reason, case$reason)
    expect_equal(audit$input_tokens, 11)
    expect_equal(audit$output_tokens, 5)
    expect_equal(audit$cost_usd, 0.02)
    expect_named(audit$error, c("class", "reason", "message"),
                 ignore.order = FALSE)
  }
})

test_that("probe preserves thrown authentication usage and cost metadata", {
  dir <- withr::local_tempdir()
  secret <- "thrown-auth-secret-2dd4"
  patient <- "patient-auth-content-must-not-enter-audit"
  llm <- new_llm(function(request) {
    error <- structure(
      list(
        message = paste("authentication rejected", secret),
        reason = "authentication_rejected", content = patient,
        request = list(content = patient), call = NULL
      ),
      class = c("sas2r_llm_authentication_error", "error", "condition")
    )
    attr(error, "usage") <- list(input_tokens = 7, output_tokens = 3)
    attr(error, "cost_usd") <- 0.125
    stop(error)
  }, provider = "azure", capabilities = llm_capabilities(
    structured_output = "native", source = "test"
  ), redaction_secrets = secret)
  attr(llm, "auth_context") <- list(
    provider = "azure", auth_mode = "ambient",
    endpoint = "https://example.openai.azure.com", model = "deployment-a"
  )

  error <- tryCatch(
    sas_llm_probe(llm, max_retries = 1L, log_dir = dir),
    error = identity
  )
  expect_s3_class(error, "sas2r_llm_authentication_error")
  expect_identical(error$reason, "authentication_rejected")
  expect_equal(attr(error, "usage")$input_tokens, 7)
  expect_equal(attr(error, "usage")$output_tokens, 3)
  expect_equal(attr(error, "cost_usd"), 0.125)
  expect_false(any(c("content", "request") %in% names(error)))
  expect_false(grepl(secret, conditionMessage(error), fixed = TRUE))

  lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
  expect_length(lines, 1L)
  expect_false(grepl(patient, lines, fixed = TRUE))
  expect_false(grepl(secret, lines, fixed = TRUE))
  audit <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
  expect_identical(audit$terminal_class, "sas2r_llm_authentication_error")
  expect_identical(audit$terminal_reason, "authentication_rejected")
  expect_equal(audit$input_tokens, 7)
  expect_equal(audit$output_tokens, 3)
  expect_equal(audit$cost_usd, 0.125)
})

test_that("probe callback failures use the same terminal boundary", {
  secret <- "probe-callback-secret-91ed"
  patient <- "patient-callback-content-must-not-enter-audit"
  callback_error <- function(stage) structure(
    list(
      message = paste(stage, "callback failed", secret),
      reason = paste0(stage, "_callback_failed"),
      content = patient, request = list(content = patient), call = NULL
    ),
    class = c("sas2r_probe_callback_error", "error", "condition")
  )

  for (stage in c("budget", "charge")) {
    dir <- withr::local_tempdir()
    raw <- structure(
      list(type = "final", data = list(ok = TRUE)),
      usage = list(input_tokens = 2, output_tokens = 1),
      cost_usd = 0.01
    )
    llm <- new_llm(
      function(request) raw,
      provider = "mock",
      capabilities = llm_capabilities(structured_output = "native", source = "test"),
      redaction_secrets = secret
    )
    args <- list(
      llm = llm, max_retries = 1L, log_dir = dir,
      can_attempt = if (identical(stage, "budget")) {
        function() stop(callback_error(stage))
      } else NULL,
      on_charge = if (identical(stage, "charge")) {
        function(cost) stop(callback_error(stage))
      } else NULL
    )

    error <- tryCatch(do.call(sas_llm_probe, args), error = identity)
    expect_s3_class(error, "sas2r_probe_callback_error")
    expect_identical(error$reason, paste0(stage, "_callback_failed"))
    expect_false(any(c("content", "request") %in% names(error)))
    lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
    expect_length(lines, 1L)
    expect_false(grepl(secret, lines, fixed = TRUE))
    expect_false(grepl(patient, lines, fixed = TRUE))
    audit <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
    expect_identical(audit$type, "terminal_error")
    expect_identical(audit$terminal_class, "sas2r_probe_callback_error")
    expect_identical(audit$terminal_reason,
                     paste0(stage, "_callback_failed"))
    if (identical(stage, "charge")) {
      expect_equal(audit$input_tokens, 2)
      expect_equal(audit$output_tokens, 1)
      expect_equal(audit$cost_usd, 0.01)
    }
  }
})

test_that("probe does not relabel local failures as cloud access failures", {
  local_failure <- function(stage) simpleError(paste(stage, "failed locally"))
  cases <- list(
    capabilities = function(llm, dir) {
      llm$capabilities_for <- function(tier, model = NULL) {
        stop(local_failure("capabilities"))
      }
      sas_llm_probe(llm, max_retries = 1L, log_dir = dir)
    },
    can_attempt = function(llm, dir) {
      sas_llm_probe(
        llm, max_retries = 1L, log_dir = dir,
        can_attempt = function() stop(local_failure("can_attempt"))
      )
    },
    on_charge = function(llm, dir) {
      sas_llm_probe(
        llm, max_retries = 1L, log_dir = dir,
        on_charge = function(cost) stop(local_failure("on_charge"))
      )
    }
  )

  for (stage in names(cases)) {
    dir <- withr::local_tempdir()
    raw <- structure(
      list(type = "final", data = list(ok = TRUE)),
      usage = list(input_tokens = 2, output_tokens = 1),
      cost_usd = 0.01
    )
    llm <- new_llm(
      function(request) raw,
      provider = "azure", model = "deployment-frontier",
      capabilities = llm_capabilities(structured_output = "native", source = "test")
    )
    attr(llm, "auth_context") <- list(
      provider = "azure", auth_mode = "ambient",
      endpoint = "https://example.openai.azure.com",
      model = "deployment-frontier"
    )

    error <- tryCatch(cases[[stage]](llm, dir), error = identity)
    expect_s3_class(error, "sas2r_llm_probe_local_error")
    expect_false(inherits(error, c(
      "sas2r_llm_authentication_error", "sas2r_llm_permission_denied",
      "sas2r_llm_network_error", "sas2r_llm_access_failed"
    )))
    expect_identical(error$stage, stage)
    expect_identical(error$reason, paste0(stage, "_failed"))

    rows <- read_probe_audit(dir)
    expect_length(rows, 1L)
    expect_identical(rows[[1]]$terminal_class, "sas2r_llm_probe_local_error")
    expect_identical(rows[[1]]$terminal_reason, paste0(stage, "_failed"))
  }
})

test_that("probe failure metrics are closed scalars for hostile usage shapes", {
  secret <- "usage-payload-secret-68b2"
  patient <- "patient-usage-content-must-not-enter-audit"
  usage_cases <- list(
    nested = list(
      input_tokens = list(value = 11, secret = secret),
      output_tokens = 5,
      request = list(content = patient),
      arbitrary = list(secret = secret)
    ),
    atomic = secret
  )
  metric_names <- c(
    "input_tokens", "output_tokens", "total_input_tokens",
    "total_output_tokens", "total_tokens",
    "cached_input_tokens", "cache_write_tokens", "reasoning_tokens",
    "tool_charges"
  )

  for (usage in usage_cases) {
    dir <- withr::local_tempdir()
    llm <- new_llm(function(request) {
      error <- structure(
        list(
          message = "authentication rejected", call = NULL,
          reason = "authentication_rejected"
        ),
        class = c("sas2r_llm_authentication_error", "error", "condition")
      )
      attr(error, "usage") <- usage
      attr(error, "cost_usd") <- list(amount = 0.5, secret = secret)
      stop(error)
    }, provider = "azure", capabilities = llm_capabilities(
      structured_output = "native", source = "test"
    ), redaction_secrets = secret)
    attr(llm, "auth_context") <- list(
      provider = "azure", auth_mode = "ambient",
      endpoint = "https://example.openai.azure.com", model = "deployment-a"
    )

    error <- tryCatch(
      sas_llm_probe(llm, max_retries = 1L, log_dir = dir),
      error = identity
    )
    expect_s3_class(error, "sas2r_llm_authentication_error")
    expect_identical(error$reason, "authentication_rejected")
    expect_named(attr(error, "usage"), metric_names, ignore.order = FALSE)
    expect_true(all(vapply(attr(error, "usage"), function(value) {
      is.numeric(value) && length(value) == 1L
    }, logical(1))))
    expect_true(is.numeric(attr(error, "cost_usd")))
    expect_length(attr(error, "cost_usd"), 1L)

    lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
    expect_length(lines, 1L)
    expect_false(grepl(secret, lines, fixed = TRUE))
    expect_false(grepl(patient, lines, fixed = TRUE))
    row <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
    expect_identical(row$terminal_class, "sas2r_llm_authentication_error")
    expect_identical(row$terminal_reason, "authentication_rejected")
    expect_named(row, c(
      "timestamp", "agent", "type", "status", "terminal_class",
      "terminal_reason", "provider", "requested_model", "resolved_model",
      "tier", "attempt", "input_tokens", "output_tokens",
      "total_input_tokens", "total_output_tokens", "total_tokens",
      "cached_input_tokens", "cache_write_tokens", "reasoning_tokens",
      "cost_usd", "cost_status",
      "cumulative_spend_usd", "error"
    ), ignore.order = FALSE)
    expect_named(row$error, c("class", "reason", "message"),
                 ignore.order = FALSE)
  }
})

test_that("terminal probe audit preserves the exact tier-selected model", {
  dir <- withr::local_tempdir()
  cfg <- list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "v1",
    tiers = list(frontier = "frontier-model", cheap = "cheap-model")
  )
  testthat::local_mocked_bindings(
    llm_model_inventory = function(config) c("cheap-model"),
    ellmer_llm = function(config) {
      llm <- new_llm(
        function(request) stop(simpleError("provider timed out")),
        provider = "azure", model = "frontier-model",
        capabilities = llm_capabilities(structured_output = "native", source = "test")
      )
      attr(llm, "auth_context") <- llm_selector_identity(config)
      llm
    },
    .package = "sas2r"
  )

  error <- tryCatch(
    sas_llm_probe(cfg, tier = "cheap", max_retries = 1L, log_dir = dir),
    error = identity
  )
  expect_s3_class(error, "sas2r_llm_timeout")
  expect_identical(error$requested_model, "cheap-model")
  expect_identical(error$resolved_model, "cheap-model")

  rows <- read_probe_audit(dir)
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$requested_model, "cheap-model")
  expect_identical(rows[[1]]$resolved_model, "cheap-model")
})

test_that("raw provider reasons cannot bypass ambient auth classification", {
  dir <- withr::local_tempdir()
  cfg <- list(
    provider = "azure", auth_mode = "ambient",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    tiers = list(frontier = "frontier-model", cheap = "cheap-model")
  )
  llm <- new_llm(
    function(request) {
      stop(structure(
        list(
          message = "Unauthorized", reason = "Unauthorized",
          status_code = 401L, call = NULL
        ),
        class = c("azure_provider_error", "error", "condition")
      ))
    },
    provider = "azure", model = "frontier-model",
    capabilities = llm_capabilities(structured_output = "native", source = "test")
  )
  attr(llm, "auth_context") <- llm_selector_identity(normalize_llm_config(cfg))

  error <- tryCatch(
    sas_llm_probe(llm, tier = "cheap", max_retries = 1L, log_dir = dir),
    error = identity
  )

  expect_s3_class(error, "sas2r_auth_required")
  expect_identical(error$reason, "not_logged_in")
  expect_identical(error$command, "az login")
  expect_match(conditionMessage(error), "Run `az login`", fixed = TRUE)

  rows <- read_probe_audit(dir)
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$terminal_class, "sas2r_auth_required")
  expect_identical(rows[[1]]$terminal_reason, "not_logged_in")
  expect_identical(rows[[1]]$error$class, "sas2r_auth_required")
  expect_identical(rows[[1]]$error$reason, "not_logged_in")
})

test_that("all normalized Task 16 and 17 failure semantics round trip", {
  supported <- list(
    c("sas2r_auth_required", "not_logged_in"),
    c("sas2r_auth_required", "expired_login"),
    c("sas2r_llm_authentication_error", "authentication_rejected"),
    c("sas2r_llm_permission_denied", "permission_denied"),
    c("sas2r_llm_rate_limit", "rate_limited"),
    c("sas2r_llm_timeout", "timed_out"),
    c("sas2r_llm_invalid_schema", "invalid_schema"),
    c("sas2r_llm_transport_error", "transport_failure"),
    c("sas2r_llm_config_error", "configuration_error"),
    c("sas2r_llm_capability_error", "capability_resolution_failed"),
    c("sas2r_llm_region_mismatch", "region_mismatch"),
    c("sas2r_llm_model_not_found", "model_not_found"),
    c("sas2r_llm_endpoint_invalid", "endpoint_invalid"),
    c("sas2r_llm_network_error", "network_failure"),
    c("sas2r_llm_access_failed", "access_failed"),
    c("sas2r_llm_inventory_shape_error", "inventory_shape")
  )

  for (pair in supported) {
    condition <- structure(
      list(
        message = paste("normalized", pair[[2]]), call = NULL,
        reason = pair[[2]], provider = "azure", command = "az login",
        identity = list(provider = "azure", model = "deployment-a")
      ),
      class = c(pair[[1]], "sas2r_llm_access_error", "error", "condition")
    )

    response <- normalize_provider_response(condition, provider = "azure")
    expect_s3_class(response, pair[[1]])
    expect_identical(response$error$class, pair[[1]])
    expect_identical(response$error$reason, pair[[2]])
    expect_identical(response$error$provider, "azure")
    expect_identical(response$error$command, "az login")
    expect_identical(
      response$error$identity,
      list(provider = "azure", model = "deployment-a")
    )

    round_trip <- llm_condition_from_failure_response(response)
    expect_s3_class(round_trip, pair[[1]])
    expect_identical(round_trip$reason, pair[[2]])
    expect_identical(round_trip$provider, "azure")
    expect_identical(round_trip$command, "az login")
    expect_identical(
      round_trip$identity,
      list(provider = "azure", model = "deployment-a")
    )
  }
})

test_that("failure normalization trusts only registered pairs and safe fields", {
  secret <- "provider-normalization-secret-a63f"
  cfg <- normalize_llm_config(list(
    provider = "azure", auth_mode = "ambient",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    model = "deployment-a"
  ))
  generic <- structure(
    list(
      message = paste("Unauthorized", secret), call = NULL,
      reason = "Unauthorized", status_code = 401L,
      command = paste("curl", secret),
      identity = list(endpoint = paste0(cfg$endpoint, "?token=", secret)),
      api_key = secret, content = paste("patient", secret)
    ),
    class = c("azure_provider_error", "error", "condition")
  )

  response <- normalize_provider_response(generic, provider = "azure")
  expect_s3_class(response, "sas2r_llm_authentication_error")
  expect_identical(response$error$reason, "authentication_rejected")
  expect_false(any(c("command", "identity", "api_key", "content") %in%
                     names(response$error)))
  expect_false(grepl(secret, response$error$message, fixed = TRUE))

  classified <- llm_condition_from_failure_response(
    response, llm_selector_identity(cfg)
  )
  expect_s3_class(classified, "sas2r_auth_required")
  expect_identical(classified$reason, "not_logged_in")
  expect_identical(classified$command, "az login")
  expect_false(grepl(secret, conditionMessage(classified), fixed = TRUE))
})

test_that("constructed adapter probe selects model from retained tier context", {
  dir <- withr::local_tempdir()
  cfg <- normalize_llm_config(list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "v1",
    tiers = list(frontier = "frontier-model", cheap = "cheap-model")
  ))
  llm <- new_llm(
    function(request) stop(simpleError("provider timed out")),
    provider = "azure", model = "frontier-model",
    capabilities = llm_capabilities(structured_output = "native", source = "test")
  )
  attr(llm, "auth_context") <- llm_selector_identity(cfg)

  error <- tryCatch(
    sas_llm_probe(llm, tier = "cheap", max_retries = 1L, log_dir = dir),
    error = identity
  )

  expect_s3_class(error, "sas2r_llm_timeout")
  expect_identical(error$requested_model, "cheap-model")
  expect_identical(error$resolved_model, "cheap-model")

  rows <- read_probe_audit(dir)
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$requested_model, "cheap-model")
  expect_identical(rows[[1]]$resolved_model, "cheap-model")
})

test_that("timeout_seconds is accepted and preserved", {
  cfg <- normalize_llm_config(list(
    provider = "deepseek", model = "deepseek-v4-flash", timeout_seconds = 900
  ))

  expect_identical(cfg$timeout_seconds, 900)
})

test_that("ellmer request limits have explicit provider-neutral defaults", {
  cfg <- normalize_llm_config(list(
    provider = "ollama", auth_mode = "none", model = "fixture-model",
    base_url = "http://127.0.0.1:11434"
  ))

  expect_identical(cfg$timeout_seconds, 300)
  expect_identical(cfg$max_tries, 1L)
})

test_that("timeout_seconds must be a single positive finite number", {
  bad_values <- list(0, -1, Inf, NA_real_, "900", c(300, 900), list(300))
  for (bad in bad_values) {
    expect_error(
      normalize_llm_config(list(
        provider = "deepseek", model = "deepseek-v4-flash",
        timeout_seconds = bad
      )),
      class = "sas2r_llm_config_error"
    )
  }
})

test_that("with_ellmer_limits scopes both ellmer options and restores them", {
  before <- list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries"))

  seen <- with_ellmer_limits(900, 1L, list(
    getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")
  ))

  expect_identical(seen, list(900, 1L))
  expect_identical(
    list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")), before
  )
})

test_that("with_ellmer_limits scopes both options when limits are unset", {
  withr::local_options(list(ellmer_timeout_s = 123, ellmer_max_tries = 7L))
  before <- list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries"))

  seen <- with_ellmer_limits(NULL, NULL, list(
    getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")
  ))

  expect_identical(seen, list(NULL, NULL))
  expect_identical(
    list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")), before
  )
})

test_that("with_ellmer_limits scopes both options when one limit is unset", {
  withr::local_options(list(ellmer_timeout_s = 123, ellmer_max_tries = 7L))
  before <- list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries"))

  seen <- with_ellmer_limits(900, NULL, list(
    getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")
  ))

  expect_identical(seen, list(900, NULL))
  expect_identical(
    list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")), before
  )
})

test_that("with_ellmer_limits restores both options after an error", {
  before <- list(
    getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")
  )

  expect_error(
    with_ellmer_limits(0.05, 1L, stop("injected transport failure")),
    "injected transport failure"
  )

  expect_identical(
    list(getOption("ellmer_timeout_s"), getOption("ellmer_max_tries")),
    before
  )
})

test_that("max_tries is accepted and preserved", {
  cfg <- normalize_llm_config(list(
    provider = "deepseek", model = "deepseek-v4-flash", max_tries = 2
  ))

  expect_identical(cfg$max_tries, 2L)
})

test_that("max_tries must be a single positive whole number", {
  bad_values <- list(0, -1, 2.5, Inf, NA_integer_, "2", c(1, 2))
  for (bad in bad_values) {
    expect_error(
      normalize_llm_config(list(
        provider = "deepseek", model = "deepseek-v4-flash", max_tries = bad
      )),
      class = "sas2r_llm_config_error"
    )
  }
})

test_that("max_tries rejects values beyond R integer capacity", {
  expect_error(
    normalize_llm_config(list(
      provider = "deepseek", model = "deepseek-v4-flash",
      max_tries = .Machine$integer.max + 1
    )),
    class = "sas2r_llm_config_error"
  )
})

test_that("max_tries accepts R's largest integer", {
  cfg <- normalize_llm_config(list(
    provider = "deepseek", model = "deepseek-v4-flash",
    max_tries = .Machine$integer.max
  ))

  expect_identical(cfg$max_tries, .Machine$integer.max)
})
