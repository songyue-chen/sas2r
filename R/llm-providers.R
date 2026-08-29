# The closed, validated registry of ellmer-backed LLM providers.
#
# ellmer is the only transport this package speaks. Every provider fact that
# used to live in a parallel `switch()` -- constructor export, constructor
# arguments, auth modes, model inventory, endpoint identity, diagnostics, and
# capability defaults -- is declared here once, validated at load time, and
# looked up at runtime. Adding a provider is a data addition to
# `build_llm_provider_registry()`; it is never a new dispatch site.
#
# This file is collated after R/llm-auth.R and R/llm-capabilities.R (whose
# helpers the specs call) but before R/llm.R, and it validates the registry at
# load time, so it cannot rely on anything R/llm.R defines at its top level.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

LLM_PROVIDER_SPEC_FIELDS <- c(
  "id", "chat_export", "models_export", "ellmer_support",
  "output_schema_dialect",
  "auth_modes", "default_auth_mode", "config_fields", "credential_envs",
  "build_chat_args", "build_models_args", "normalize_config",
  "inventory_available", "endpoint_identity", "api_version_identity",
  "auth_command", "access_context", "capability_defaults"
)

LLM_PROVIDER_SPEC_CALLBACKS <- c(
  "build_chat_args", "build_models_args", "normalize_config",
  "inventory_available", "endpoint_identity", "api_version_identity",
  "auth_command", "access_context"
)

LLM_AUTH_MODES <- c("ambient", "api_key", "none")
LLM_OUTPUT_SCHEMA_DIALECTS <- c("standard", "strict_all_properties")

# Configuration keys that mean the same thing for every provider. Everything
# else a config may carry is a provider selector declared by that provider's
# `config_fields`.
LLM_SHARED_CONFIG_KEYS <- c(
  "provider", "auth_mode", "model", "tiers", "capabilities", "timeout_seconds",
  "max_tries",
  "temperature", "top_p", "reasoning_effort", "max_output_tokens"
)

# `mock` is a test-only adapter, not a provider: it must never appear in
# `llm_provider_ids()`, but its capability record stays a single source of
# truth for `new_llm()` and `adapter_capability_metadata()`.
LLM_MOCK_CAPABILITY_DEFAULTS <- list(
  structured_output = "native", tool_calling = "native",
  tools_with_structured_output = "supported"
)

# ellmer exports that configuration must never be able to reach, because they
# accept an arbitrary provider endpoint rather than a known provider protocol.
LLM_FORBIDDEN_ELLMER_EXPORTS <- c("chat_openai_compatible")

new_llm_provider_spec <- function(
    id, chat_export, models_export = NULL,
    auth_modes, default_auth_mode,
    config_fields = character(), credential_envs = character(),
    build_chat_args = function(config) list(),
    build_models_args = function(config) list(),
    normalize_config = identity,
    inventory_available = function(config) TRUE,
    endpoint_identity = function(config) NULL,
    api_version_identity = function(config) NULL,
    auth_command = function(config) NULL,
    access_context = function(config) "configured identity",
    output_schema_dialect = "standard", capability_defaults = list(),
    ellmer_support = "official") {
  spec <- list(
    id = id, chat_export = chat_export, models_export = models_export,
    ellmer_support = ellmer_support,
    output_schema_dialect = output_schema_dialect, auth_modes = auth_modes,
    default_auth_mode = default_auth_mode, config_fields = config_fields,
    credential_envs = credential_envs,
    build_chat_args = build_chat_args, build_models_args = build_models_args,
    normalize_config = normalize_config,
    inventory_available = inventory_available,
    endpoint_identity = endpoint_identity,
    api_version_identity = api_version_identity,
    auth_command = auth_command,
    access_context = access_context,
    capability_defaults = capability_defaults
  )
  structure(spec[LLM_PROVIDER_SPEC_FIELDS],
            class = c("sas2r_llm_provider_spec", "list"))
}

llm_provider_scalar_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

# A config that is safe to hand every diagnostic and identity callback while
# validating the registry: no credentials, no environment reads, no network.
llm_provider_validation_config <- function(spec) {
  list(
    provider = spec$id, auth_mode = spec$default_auth_mode,
    model = "registry-validation-model"
  )
}

validate_llm_provider_registry <- function(registry) {
  if (!is.list(registry) || !length(registry) ||
      is.null(names(registry)) || any(!nzchar(names(registry)))) {
    llm_config_abort("The LLM provider registry must be a named list of specs")
  }
  if (anyDuplicated(names(registry))) {
    llm_config_abort("The LLM provider registry contains duplicate provider ids")
  }
  for (name in names(registry)) {
    spec <- registry[[name]]
    if (!inherits(spec, "sas2r_llm_provider_spec") ||
        !identical(names(spec), LLM_PROVIDER_SPEC_FIELDS)) {
      llm_config_abort(
        "LLM provider {.val {name}} must be a complete sas2r_llm_provider_spec"
      )
    }
    if (!llm_provider_scalar_string(spec$id) ||
        !grepl("^[a-z][a-z0-9_]*$", spec$id) ||
        !identical(spec$id, name)) {
      llm_config_abort(
        "LLM provider id {.val {name}} must be a safe lowercase identifier"
      )
    }
    if (identical(spec$id, "mock")) {
      llm_config_abort(
        "LLM provider id {.val mock} is reserved for the offline test adapter"
      )
    }
    if (!identical(spec$ellmer_support, "official")) {
      llm_config_abort(
        "LLM provider {.val {name}} must use an official ellmer constructor"
      )
    }
    if (!llm_provider_scalar_string(spec$output_schema_dialect) ||
        !spec$output_schema_dialect %in% LLM_OUTPUT_SCHEMA_DIALECTS) {
      llm_config_abort(
        "LLM provider {.val {name}} must declare a supported output schema dialect"
      )
    }
    if (!llm_provider_scalar_string(spec$chat_export) ||
        !grepl("^chat_[a-z0-9_]+$", spec$chat_export) ||
        spec$chat_export %in% LLM_FORBIDDEN_ELLMER_EXPORTS) {
      llm_config_abort(
        "LLM provider {.val {name}} must name a fixed public ellmer chat export"
      )
    }
    if (!is.null(spec$models_export) &&
        (!llm_provider_scalar_string(spec$models_export) ||
         !grepl("^models_[a-z0-9_]+$", spec$models_export) ||
         spec$models_export %in% LLM_FORBIDDEN_ELLMER_EXPORTS)) {
      llm_config_abort(
        "LLM provider {.val {name}} must name a fixed public ellmer models export"
      )
    }
    if (!is.character(spec$auth_modes) || !length(spec$auth_modes) ||
        anyNA(spec$auth_modes) || anyDuplicated(spec$auth_modes) ||
        !all(spec$auth_modes %in% LLM_AUTH_MODES)) {
      llm_config_abort(
        "LLM provider {.val {name}} must declare distinct known auth modes"
      )
    }
    if (!llm_provider_scalar_string(spec$default_auth_mode) ||
        !spec$default_auth_mode %in% spec$auth_modes) {
      llm_config_abort(
        "LLM provider {.val {name}} must default to one of its own auth modes"
      )
    }
    if (!is.character(spec$config_fields) || anyNA(spec$config_fields) ||
        anyDuplicated(spec$config_fields) ||
        !all(grepl("^[a-z][a-z0-9_]*$", spec$config_fields)) ||
        length(intersect(spec$config_fields, LLM_SHARED_CONFIG_KEYS))) {
      llm_config_abort(
        "LLM provider {.val {name}} must declare unique selector fields that do not shadow shared keys"
      )
    }
    if (!is.character(spec$credential_envs) || anyNA(spec$credential_envs) ||
        anyDuplicated(spec$credential_envs) ||
        !all(grepl("^[A-Z][A-Z0-9_]*$", spec$credential_envs))) {
      llm_config_abort(
        "LLM provider {.val {name}} must allowlist credential environment variable names only"
      )
    }
    for (callback in LLM_PROVIDER_SPEC_CALLBACKS) {
      if (!is.function(spec[[callback]]) ||
          length(formals(spec[[callback]])) != 1L) {
        llm_config_abort(
          "LLM provider {.val {name}} field {.field {callback}} must be a one-argument package closure"
        )
      }
    }
    if (!is.list(spec$capability_defaults) ||
        (length(spec$capability_defaults) &&
         (is.null(names(spec$capability_defaults)) ||
          any(!nzchar(names(spec$capability_defaults)))))) {
      llm_config_abort(
        "LLM provider {.val {name}} capability defaults must be a named mapping"
      )
    }
    tryCatch(
      apply_capability_values(llm_capabilities(), spec$capability_defaults),
      error = function(error) llm_config_abort(conditionMessage(error))
    )

    fixture <- llm_provider_validation_config(spec)
    for (callback in c(
      "endpoint_identity", "api_version_identity", "auth_command",
      "access_context"
    )) {
      value <- spec[[callback]](fixture)
      if (!is.null(value) && !llm_provider_scalar_string(value)) {
        llm_config_abort(
          "LLM provider {.val {name}} field {.field {callback}} must return one string or NULL"
        )
      }
    }
    available <- spec$inventory_available(fixture)
    if (!is.logical(available) || length(available) != 1L || is.na(available)) {
      llm_config_abort(
        "LLM provider {.val {name}} field {.field inventory_available} must return one non-missing logical"
      )
    }
  }
  registry
}

build_llm_provider_registry <- function() {
  list(
    openai = new_llm_provider_spec(
      id = "openai",
      chat_export = "chat_openai",
      models_export = "models_openai",
      output_schema_dialect = "strict_all_properties",
      auth_modes = c("ambient", "api_key"),
      default_auth_mode = "api_key",
      config_fields = c("base_url", "credentials", "api_key"),
      credential_envs = "OPENAI_API_KEY",
      build_chat_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode %||% "api_key", "api_key")) {
          config$api_key
        } else NULL
      ),
      build_models_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      normalize_config = function(config) {
        # ellmer resolves OPENAI_API_KEY itself, so an omitted explicit key is
        # a valid api_key configuration here.
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        config
      },
      endpoint_identity = function(config) config$base_url
    ),
    anthropic = new_llm_provider_spec(
      id = "anthropic",
      chat_export = "chat_anthropic",
      models_export = "models_anthropic",
      auth_modes = "api_key",
      default_auth_mode = "api_key",
      config_fields = c("base_url", "credentials", "api_key", "cache"),
      credential_envs = "ANTHROPIC_API_KEY",
      build_chat_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL,
        cache = config$cache
      ),
      build_models_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      normalize_config = function(config) {
        # ellmer resolves ANTHROPIC_API_KEY itself, so an omitted explicit key
        # is a valid api_key configuration; sas2r never reads the variable.
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        config$cache <- llm_provider_cache(config$cache, "Anthropic")
        config
      },
      endpoint_identity = function(config) config$base_url,
      access_context = function(config) {
        llm_key_access_context(config, "ANTHROPIC_API_KEY")
      }
    ),
    bedrock = new_llm_provider_spec(
      id = "bedrock",
      chat_export = "chat_aws_bedrock",
      models_export = "models_aws_bedrock",
      auth_modes = "ambient",
      default_auth_mode = "ambient",
      config_fields = c("profile", "region", "base_url", "cache"),
      credential_envs = c(
        "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN"
      ),
      build_chat_args = function(config) list(
        base_url = llm_bedrock_base_url(config),
        profile = config$profile,
        cache = config$cache
      ),
      build_models_args = function(config) list(
        profile = config$profile,
        base_url = llm_bedrock_inventory_base_url(config)
      ),
      normalize_config = function(config) {
        config$profile <- llm_scalar_string(config$profile, "profile")
        config$region <- llm_scalar_string(config$region, "region")
        if (!is.null(config$region) && !grepl("^[a-z0-9-]+$", config$region)) {
          llm_config_abort("Bedrock llm region must be an AWS region name")
        }
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        if (is.null(config$region) && is.null(config$base_url)) {
          llm_config_abort("Bedrock llm config requires region or base_url")
        }
        if (!is.null(config$region) && !is.null(config$base_url)) {
          llm_config_abort("Bedrock llm config must choose either region or base_url")
        }
        if (!is.null(config$cache) &&
            (!is.character(config$cache) || length(config$cache) != 1L ||
             is.na(config$cache) ||
             !config$cache %in% c("auto", "5m", "1h", "none"))) {
          llm_config_abort("Bedrock llm cache must be one of auto, 5m, 1h, or none")
        }
        config
      },
      # The foundation-model inventory enumerates neither inference profiles
      # nor custom runtime endpoints, so those selectors need probe-based
      # access validation instead.
      inventory_available = function(config) {
        model <- config$model %||% configured_llm_models(config)[[1]]
        !is.null(config$region) && !llm_bedrock_inference_profile(model)
      },
      endpoint_identity = function(config) {
        if (!is.null(config$base_url) && !is.null(config$region)) {
          paste0("aws-base:", config$base_url, "|region:", config$region)
        } else {
          config$base_url %||%
            if (!is.null(config$region)) {
              paste0("aws-region:", config$region)
            } else NULL
        }
      },
      auth_command = function(config) {
        paste("aws sso login --profile", config$profile %||% "<configured-profile>")
      },
      access_context = function(config) {
        paste0(
          "profile ", config$profile %||% "<default>",
          ", region ", config$region %||% config$base_url %||% "<unknown>"
        )
      }
    ),
    azure = new_llm_provider_spec(
      id = "azure",
      chat_export = "chat_azure_openai",
      models_export = NULL,
      output_schema_dialect = "strict_all_properties",
      auth_modes = c("ambient", "api_key"),
      default_auth_mode = "ambient",
      config_fields = c("endpoint", "api_version", "credentials", "api_key"),
      credential_envs = c("AZURE_OPENAI_API_KEY", "AZURE_CLIENT_SECRET"),
      build_chat_args = function(config) list(
        endpoint = config$endpoint,
        api_version = config$api_version,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      normalize_config = function(config) {
        # Azure deployments are pinned per identity, so an api_key mode must
        # name its key explicitly rather than inherit an ambient one.
        if (identical(config$auth_mode, "api_key")) {
          config$api_key <- llm_scalar_string(
            config$api_key, "api_key", required = TRUE
          )
        }
        config$endpoint <- llm_scalar_url(config$endpoint, "endpoint", TRUE)
        config$api_version <- llm_scalar_string(
          config$api_version, "api_version", TRUE
        )
        config
      },
      endpoint_identity = function(config) config$endpoint,
      api_version_identity = function(config) config$api_version,
      auth_command = function(config) "az login",
      access_context = function(config) {
        paste0(
          "endpoint ", config$endpoint %||% "<unknown>",
          ", deployment ", config$model %||% "<unknown>"
        )
      }
    ),
    databricks = new_llm_provider_spec(
      id = "databricks",
      chat_export = "chat_databricks",
      # ellmer publishes no Databricks serving-endpoint inventory, and an empty
      # list must never be presented as an authoritative one.
      models_export = NULL,
      auth_modes = "ambient",
      default_auth_mode = "ambient",
      config_fields = "workspace",
      # The workspace host is a tenant selector, not a credential; only the
      # bearer token ellmer reads is allowlisted for redaction.
      credential_envs = "DATABRICKS_TOKEN",
      # ellmer owns the `token` formal through its unified ambient
      # authentication, so sas2r passes only the workspace and never invents a
      # `credentials` formal that `chat_databricks()` does not publish.
      build_chat_args = function(config) list(workspace = config$workspace),
      normalize_config = function(config) {
        # Normalized only when supplied: `compact_non_null()` then omits the
        # argument entirely so `chat_databricks()` keeps its public
        # `databricks_workspace()` default.
        config$workspace <- llm_scalar_url(config$workspace, "workspace")
        config
      },
      inventory_available = function(config) FALSE,
      # An omitted workspace still has to yield a tenant-distinct identity, or
      # two workspaces would collide in one capability cache entry.
      endpoint_identity = function(config) {
        config$workspace %||% llm_ambient_url_identity("DATABRICKS_HOST")
      },
      auth_command = function(config) {
        paste(
          "databricks auth login --host",
          config$workspace %||% "<configured-workspace>"
        )
      },
      access_context = function(config) {
        paste0(
          "workspace ", config$workspace %||% "<unknown>",
          ", model ", config$model %||% "<unknown>"
        )
      }
    ),
    deepseek = new_llm_provider_spec(
      id = "deepseek",
      chat_export = "chat_deepseek",
      models_export = "models_deepseek",
      auth_modes = "api_key",
      default_auth_mode = "api_key",
      config_fields = c("base_url", "credentials", "api_key"),
      credential_envs = "DEEPSEEK_API_KEY",
      build_chat_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      build_models_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      normalize_config = function(config) {
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        config
      },
      endpoint_identity = function(config) config$base_url,
      # Provider-adapter support only: the DeepSeek chat completions surface
      # has no native structured-output mode, so the adapter carries JSON
      # fallback. Tool calling and optional parameters stay unknown.
      capability_defaults = list(structured_output = "fallback"),
      access_context = function(config) {
        llm_key_access_context(config, "DEEPSEEK_API_KEY")
      }
    ),
    github = new_llm_provider_spec(
      id = "github",
      chat_export = "chat_github",
      models_export = "models_github",
      auth_modes = "api_key",
      default_auth_mode = "api_key",
      config_fields = c(
        "base_url", "models_base_url", "credentials", "api_key"
      ),
      credential_envs = "GITHUB_PAT",
      build_chat_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      # GitHub Models publishes different documented chat and inventory paths,
      # so the inventory endpoint is only ever the explicitly configured one.
      build_models_args = function(config) list(
        base_url = config$models_base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      normalize_config = function(config) {
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        config$models_base_url <- llm_scalar_url(
          config$models_base_url, "models_base_url"
        )
        config
      },
      # A custom chat endpoint without an explicit inventory endpoint is
      # reported as unavailable rather than guessed from string surgery.
      inventory_available = function(config) {
        is.null(config$base_url) || !is.null(config$models_base_url)
      },
      endpoint_identity = function(config) config$base_url,
      access_context = function(config) {
        llm_key_access_context(config, "GITHUB_PAT")
      }
    ),
    gemini = new_llm_provider_spec(
      id = "gemini",
      chat_export = "chat_google_gemini",
      models_export = "models_google_gemini",
      auth_modes = c("ambient", "api_key"),
      default_auth_mode = "ambient",
      config_fields = c("base_url", "credentials", "api_key"),
      credential_envs = c("GOOGLE_API_KEY", "GEMINI_API_KEY"),
      build_chat_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      build_models_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        api_key = if (identical(config$auth_mode, "api_key")) {
          config$api_key
        } else NULL
      ),
      normalize_config = function(config) {
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        # `ambient` deliberately selects ellmer's ordered Google credential
        # chain. `api_key` must not silently degrade into OAuth or application
        # default credentials, so it requires an explicit key or the presence
        # -- never the capture -- of a documented key variable.
        if (identical(config$auth_mode, "api_key") &&
            is.null(config$api_key) && is.null(config$credentials) &&
            !llm_env_name_present(c("GOOGLE_API_KEY", "GEMINI_API_KEY"))) {
          llm_config_abort(paste(
            "Gemini api_key auth_mode requires an explicit api_key or",
            "credentials callback, or GOOGLE_API_KEY or GEMINI_API_KEY in the",
            "environment; use auth_mode ambient for the Google credential chain"
          ))
        }
        config
      },
      endpoint_identity = function(config) config$base_url,
      auth_command = function(config) {
        if (identical(config$auth_mode %||% "ambient", "ambient")) {
          "gcloud auth application-default login"
        } else NULL
      },
      access_context = function(config) {
        if (identical(config$auth_mode, "api_key")) {
          llm_key_access_context(config, c("GOOGLE_API_KEY", "GEMINI_API_KEY"))
        } else {
          paste0(
            "model ", config$model %||% "<unknown>",
            ", credentials from the ellmer Google chain (GOOGLE_API_KEY, ",
            "GEMINI_API_KEY, application default credentials, or an ",
            "ellmer-owned interactive sign-in)"
          )
        }
      }
    ),
    vertex = new_llm_provider_spec(
      id = "vertex",
      chat_export = "chat_google_vertex",
      models_export = "models_google_vertex",
      auth_modes = "ambient",
      default_auth_mode = "ambient",
      config_fields = c("project_id", "location", "credentials"),
      # Names the Application Default Credentials file; project and location
      # are identity selectors, not credentials.
      credential_envs = "GOOGLE_APPLICATION_CREDENTIALS",
      build_chat_args = function(config) list(
        project_id = config$project_id,
        location = config$location
      ),
      build_models_args = function(config) list(
        project_id = config$project_id, location = config$location,
        credentials = tracked_llm_credentials(config$credentials)
      ),
      normalize_config = function(config) {
        if (!is.null(config$credentials)) {
          llm_config_abort(
            "Vertex chat uses Application Default Credentials; a credentials callback is not supported"
          )
        }
        config$project_id <- llm_scalar_string(
          config$project_id, "project_id", TRUE
        )
        config$location <- llm_scalar_string(config$location, "location", TRUE)
        config
      },
      endpoint_identity = function(config) {
        if (!is.null(config$project_id) && !is.null(config$location)) {
          paste0("vertex:", config$project_id, ":", config$location)
        } else NULL
      },
      auth_command = function(config) "gcloud auth application-default login",
      access_context = function(config) {
        paste0(
          "project ", config$project_id %||% "<unknown>",
          ", location ", config$location %||% "<unknown>"
        )
      }
    ),
    ollama = new_llm_provider_spec(
      id = "ollama",
      chat_export = "chat_ollama",
      models_export = "models_ollama",
      auth_modes = "none",
      default_auth_mode = "none",
      config_fields = "base_url",
      credential_envs = "OLLAMA_API_KEY",
      build_chat_args = function(config) list(base_url = config$base_url),
      build_models_args = function(config) list(base_url = config$base_url),
      normalize_config = function(config) {
        config$base_url <- llm_scalar_url(config$base_url, "base_url", TRUE)
        config
      },
      endpoint_identity = function(config) config$base_url
    ),
    posit = new_llm_provider_spec(
      id = "posit",
      chat_export = "chat_posit",
      models_export = "models_posit",
      auth_modes = "ambient",
      default_auth_mode = "ambient",
      config_fields = c("base_url", "credentials", "cache"),
      # Posit sign-in is an ellmer-owned OAuth/device credential cache or an
      # explicit callback; there is no documented credential variable.
      credential_envs = character(),
      build_chat_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials),
        cache = config$cache
      ),
      build_models_args = function(config) list(
        base_url = config$base_url,
        credentials = tracked_llm_credentials(config$credentials)
      ),
      normalize_config = function(config) {
        config$base_url <- llm_scalar_url(config$base_url, "base_url")
        config$cache <- llm_provider_cache(config$cache, "Posit")
        config
      },
      endpoint_identity = function(config) config$base_url,
      access_context = function(config) {
        paste0(
          "gateway ", config$base_url %||% "<default>",
          ", model ", config$model %||% "<unknown>",
          "; ellmer may complete its Posit device sign-in on the next",
          " interactive request"
        )
      }
    ),
    snowflake = new_llm_provider_spec(
      id = "snowflake",
      chat_export = "chat_snowflake",
      # ellmer publishes no Cortex model inventory.
      models_export = NULL,
      auth_modes = "ambient",
      default_auth_mode = "ambient",
      config_fields = c("account", "credentials"),
      # The account is a tenant selector, not a credential. `SNOWFLAKE_USER`
      # names a principal, so only the two secret variables are allowlisted --
      # and `SNOWFLAKE_PRIVATE_KEY` is exactly the name the generic sensitive
      # environment regex cannot see.
      credential_envs = c("SNOWFLAKE_TOKEN", "SNOWFLAKE_PRIVATE_KEY"),
      build_chat_args = function(config) list(
        account = config$account,
        credentials = tracked_llm_credentials(config$credentials)
      ),
      normalize_config = function(config) {
        # Normalized only when supplied: `compact_non_null()` then omits the
        # argument entirely so `chat_snowflake()` keeps its public
        # `snowflake_account()` default.
        config$account <- llm_scalar_string(config$account, "account")
        if (!is.null(config$account) &&
            !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", config$account)) {
          llm_config_abort(
            "Snowflake llm account must be one plain account identifier"
          )
        }
        config
      },
      inventory_available = function(config) FALSE,
      # An omitted account still has to yield a tenant-distinct identity, or
      # two accounts would collide in one capability cache entry.
      endpoint_identity = function(config) {
        account <- config$account %||%
          llm_ambient_account_identity("SNOWFLAKE_ACCOUNT")
        if (is.null(account)) NULL else paste0("snowflake:", account)
      },
      access_context = function(config) {
        paste0(
          "account ", config$account %||% "<unknown>",
          ", model ", config$model %||% "<unknown>",
          "; supported ambient credentials: SNOWFLAKE_TOKEN, SNOWFLAKE_USER",
          " with SNOWFLAKE_PRIVATE_KEY, or a Posit Connect viewer token"
        )
      }
    )
  )
}

# Built and validated once per session. The cache lives in this `local()`
# closure rather than the namespace, so it stays writable under a locked
# installed namespace.
llm_provider_registry <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      value <<- validate_llm_provider_registry(build_llm_provider_registry())
    }
    value
  }
})

llm_provider_ids <- function() names(llm_provider_registry())

llm_provider_spec <- function(provider) {
  provider <- tolower(llm_scalar_string(provider, "provider", TRUE))
  spec <- llm_provider_registry()[[provider]]
  if (is.null(spec)) {
    llm_config_abort("Unknown LLM provider {.val {provider}}")
  }
  spec
}

# Tolerant lookup for diagnostics that also run for adapters outside the
# production registry, such as the offline `mock` adapter.
llm_provider_spec_or_null <- function(provider) {
  if (!is.character(provider) || length(provider) != 1L || is.na(provider)) {
    return(NULL)
  }
  llm_provider_registry()[[tolower(provider)]]
}

# Provider-specific auth-mode rejections, preserved verbatim from the switch
# statements this registry replaced. The wording is not derivable from
# `auth_modes` alone: Bedrock words `none` and `api_key` rejections
# differently, and OpenAI and Azure word the same rejection differently.
#
# The legacy phrasings are keyed on provider id, never on `default_auth_mode`,
# because a mode-shaped key would hand a legacy sentence to a provider whose
# spec does not declare that mode -- for example telling an ambient-only
# provider to try `api_key`. Every provider outside the legacy set gets the
# generic message, which names exactly the modes its own spec declares.
LLM_LEGACY_AMBIENT_PROVIDERS <- c("bedrock", "azure", "vertex")

llm_auth_mode_abort <- function(spec, auth_mode) {
  provider <- spec$id
  legacy_ambient <- provider %in% LLM_LEGACY_AMBIENT_PROVIDERS
  auth_modes <- spec$auth_modes
  message <- if (identical(provider, "openai")) {
    "OpenAI llm config requires ambient or api_key auth_mode"
  } else if (identical(provider, "ollama")) {
    "Ollama llm config requires auth_mode none"
  } else if (legacy_ambient &&
             identical(spec$default_auth_mode, "ambient") &&
             !auth_mode %in% c("ambient", "api_key")) {
    "Cloud LLM provider {.val {provider}} requires ambient or api_key auth_mode"
  } else if (legacy_ambient && identical(auth_modes, "ambient")) {
    "LLM provider {.val {provider}} supports ambient authentication only"
  } else {
    paste(
      "LLM provider {.val {provider}} does not support auth_mode",
      "{.val {auth_mode}}; it declares {.val {auth_modes}}"
    )
  }
  llm_config_abort(message, .envir = environment())
}

# Current values of the selected provider's allowlisted credential variables.
# Read on demand for redaction only: the values never enter the registry, a
# normalized config, the lockfile, or the usage ledger.
llm_provider_credential_env_values <- function(config) {
  if (!is.list(config)) return(character())
  spec <- llm_provider_spec_or_null(config$provider)
  if (is.null(spec) || !length(spec$credential_envs)) return(character())
  values <- unname(Sys.getenv(spec$credential_envs, unset = ""))
  unique(values[!is.na(values) & nchar(values) >= 8L])
}

# Every secret an audit redactor must mask for one LLM configuration: the
# values carried in the config itself plus the provider's ambient credential
# environment variables, which a generic name regex can miss.
llm_config_secret_values <- function(config) {
  unique(c(
    llm_secret_values(config), llm_provider_credential_env_values(config)
  ))
}

LLM_PROVIDERS <- llm_provider_ids()

LLM_CONFIG_KEYS <- unique(c(
  LLM_SHARED_CONFIG_KEYS,
  unlist(
    lapply(llm_provider_registry(), `[[`, "config_fields"), use.names = FALSE
  )
))
