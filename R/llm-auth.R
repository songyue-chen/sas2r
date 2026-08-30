LLM_TIER_NAMES <- c("frontier", "cheap", "fast")

llm_config_abort <- function(message, class = "sas2r_llm_config_error",
                             .envir = parent.frame()) {
  cli::cli_abort(message, class = unique(c(class, "sas2r_llm_config_error")),
                 .envir = .envir)
}

llm_scalar_string <- function(value, field, required = FALSE) {
  if (is.null(value) && !required) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    llm_config_abort("LLM {.field {field}} must be a non-empty string")
  }
  value
}

llm_scalar_url <- function(value, field, required = FALSE) {
  value <- llm_scalar_string(value, field, required)
  if (is.null(value)) return(NULL)
  if (!grepl("^https?://[^/?#[:space:]]+", value)) {
    llm_config_abort("LLM {.field {field}} must be an absolute HTTP(S) URL")
  }
  if (grepl("[?#]", value) || grepl("://[^/]*@", value)) {
    llm_config_abort(
      "LLM {.field {field}} must not contain user info, a query, or a fragment"
    )
  }
  value
}

configured_llm_models <- function(config) {
  values <- c(config$model, unlist(config$tiers, use.names = FALSE))
  values <- as.character(values)
  unique(values[!is.na(values) & nzchar(values)])
}

llm_bedrock_base_url <- function(config) {
  if (!is.null(config$base_url)) return(config$base_url)
  if (is.null(config$region)) return(NULL)
  domain <- if (startsWith(config$region, "cn-")) {
    "amazonaws.com.cn"
  } else {
    "amazonaws.com"
  }
  paste0("https://bedrock-runtime.", config$region, ".", domain)
}

llm_bedrock_inventory_base_url <- function(config) {
  if (is.null(config$region)) return(NULL)
  domain <- if (startsWith(config$region, "cn-")) {
    "amazonaws.com.cn"
  } else {
    "amazonaws.com"
  }
  paste0("https://bedrock.", config$region, ".", domain)
}

llm_bedrock_inference_profile <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(model)) {
    return(FALSE)
  }
  grepl(
    "^(?:us|eu|apac|jp|au|global)\\.|^arn:[^:]+:bedrock:[^:]*:[^:]*:(?:application-)?inference-profile/",
    model, ignore.case = TRUE, perl = TRUE
  )
}

# Every ellmer constructor that publishes a `cache` argument offers exactly
# 5m/1h/none; Bedrock alone adds `auto` and keeps its own validator.
llm_provider_cache <- function(value, provider_label) {
  # A migration outlives a 5-minute prompt-cache TTL by design -- component
  # translation, review, and repair rounds are minutes apart -- so leaving the
  # provider's 5m default in place threw the cache away between turns. Unset
  # means 1h; an explicit value (including "5m" or "none") is always honored.
  if (is.null(value)) return("1h")
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !value %in% c("5m", "1h", "none")) {
    llm_config_abort(paste(
      provider_label, "llm cache must be one of 5m, 1h, or none"
    ))
  }
  value
}

# Presence only. The value transits a local for the `nzchar()` test and is
# never returned, stored, or copied into a configuration.
llm_env_name_present <- function(variables) {
  any(nzchar(Sys.getenv(variables, unset = "")))
}

# Ambient tenant identity. `workspace` and `account` are optional because
# ellmer resolves them itself when they are omitted, so the capability identity
# has to read the same public selector or two tenants would share one
# `capability_cache_key()` entry. `DATABRICKS_HOST` and `SNOWFLAKE_ACCOUNT` are
# non-secret selectors, never credentials: they are read only for identity,
# never captured into a configuration, and never allowlisted in any
# `credential_envs`. Only the environment variable is consulted -- never a
# developer credential file or CLI profile.
llm_ambient_url_identity <- function(variable) {
  value <- trimws(Sys.getenv(variable, unset = ""))
  if (!nzchar(value) || grepl("[[:space:]]", value)) return(NULL)
  # ellmer's own `databricks_workspace()` assumes HTTPS for a bare host.
  if (!grepl("^https?://", value)) value <- paste0("https://", value)
  if (!grepl("^https?://[^/?#[:space:]]+", value)) return(NULL)
  # User info, query, and fragment data can carry a token, so they are dropped
  # before the value can reach a capability key, lockfile, or audit row.
  value <- sub("[?#].*$", "", value)
  value <- sub("^([^:]+://)[^/@]+@", "\\1", value)
  value
}

llm_ambient_account_identity <- function(variable) {
  value <- trimws(Sys.getenv(variable, unset = ""))
  if (!nzchar(value) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", value)) {
    return(NULL)
  }
  value
}

# Access diagnostics for key-based modes: name the documented environment
# variable, or say that a key/callback was configured, but never echo either.
llm_key_access_context <- function(config, env_names) {
  origin <- if (!is.null(config$api_key) || !is.null(config$credentials)) {
    "the configured api_key or credentials callback"
  } else {
    paste0("environment variable ", paste(env_names, collapse = " or "))
  }
  paste0("model ", config$model %||% "<unknown>", ", credentials from ", origin)
}

llm_inventory_selector_supported <- function(config) {
  spec <- llm_provider_spec_or_null(config$provider)
  if (is.null(spec)) return(TRUE)
  isTRUE(spec$inventory_available(config))
}

ELLMER_DEFAULT_TIMEOUT_SECONDS <- 300
ELLMER_DEFAULT_MAX_TRIES <- 1L

normalize_llm_config <- function(config) {
  if (inherits(config, "sas2r_config")) config <- config$llm
  if (is.list(config) && is.null(config$provider) &&
      is.list(config$llm) && length(config$llm)) {
    config <- config$llm
  }
  if (is.null(config) || !length(config)) return(NULL)
  if (!is.list(config) || is.null(names(config)) || any(!nzchar(names(config)))) {
    llm_config_abort("LLM configuration must be a named mapping")
  }
  unknown <- setdiff(names(config), LLM_CONFIG_KEYS)
  if (length(unknown)) {
    llm_config_abort(
      "Unknown key{?s} in {.field llm}: {.field {unknown}}"
    )
  }

  config$provider <- tolower(llm_scalar_string(config$provider, "provider", TRUE))
  spec <- llm_provider_spec(config$provider)
  wrong_provider <- setdiff(
    names(config), c(LLM_SHARED_CONFIG_KEYS, spec$config_fields)
  )
  if (length(wrong_provider)) {
    llm_config_abort(
      "LLM provider {.val {config$provider}} does not accept selector{?s}: {.field {wrong_provider}}"
    )
  }
  if (!is.null(config$tiers) &&
      (!is.list(config$tiers) || is.null(names(config$tiers)) ||
       any(!nzchar(names(config$tiers))))) {
    llm_config_abort("LLM tiers must be a named mapping")
  }
  if (!is.null(config$tiers)) {
    tier_names <- names(config$tiers)
    unknown_tiers <- setdiff(tier_names, LLM_TIER_NAMES)
    if (anyDuplicated(tier_names) || length(unknown_tiers)) {
      llm_config_abort(
        "LLM tiers may contain only frontier, cheap, and fast, without duplicates"
      )
    }
    valid_tiers <- vapply(config$tiers, function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1))
    if (!all(valid_tiers)) {
      llm_config_abort("Each LLM tier must name one non-empty model")
    }
  }
  if (!is.null(config$model)) config$model <- llm_scalar_string(config$model, "model")
  if (!length(configured_llm_models(config))) {
    llm_config_abort(
      "An explicit LLM model or tier mapping is required",
      class = "sas2r_llm_model_required"
    )
  }
  if (is.null(config$auth_mode)) config$auth_mode <- spec$default_auth_mode
  config$auth_mode <- tolower(llm_scalar_string(config$auth_mode, "auth_mode", TRUE))
  if (!config$auth_mode %in% LLM_AUTH_MODES) {
    llm_config_abort("LLM auth_mode must be one of ambient, api_key, or none")
  }
  if (!config$auth_mode %in% spec$auth_modes) {
    llm_auth_mode_abort(spec, config$auth_mode)
  }
  if (identical(config$auth_mode, "api_key")) {
    # Whether an omitted explicit key is valid is a provider decision, because
    # ellmer owns the environment lookup for some providers and not others.
    config$api_key <- llm_scalar_string(config$api_key, "api_key")
  } else if (!is.null(config$api_key)) {
    llm_config_abort("api_key may be set only when auth_mode is api_key")
  }
  if (!is.null(config$credentials) && !is.function(config$credentials)) {
    llm_config_abort("LLM credentials must be a callback function or NULL")
  }
  # Only an actual conflict is one, which is what the message already says.
  # Providers whose sole declared auth mode is `api_key` must still reach their
  # `credentials` field, and a provider that demands an explicit key keeps its
  # own required-key rule in `normalize_config()`.
  if (identical(config$auth_mode, "api_key") && !is.null(config$api_key) &&
      !is.null(config$credentials)) {
    llm_config_abort("Choose either api_key or a credentials callback, not both")
  }
  if (!is.null(config$capabilities)) {
    if (!is.list(config$capabilities) || is.null(names(config$capabilities)) ||
        any(!nzchar(names(config$capabilities)))) {
      llm_config_abort("LLM capabilities must be a named mapping")
    }
    tryCatch(
      apply_capability_values(llm_capabilities(), config$capabilities),
      error = function(error) llm_config_abort(conditionMessage(error))
    )
  }
  timeout <- config$timeout_seconds %||% ELLMER_DEFAULT_TIMEOUT_SECONDS
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0) {
    llm_config_abort(
      "LLM timeout_seconds must be a single positive finite number of seconds"
    )
  }
  config$timeout_seconds <- as.numeric(timeout)

  tries <- config$max_tries %||% ELLMER_DEFAULT_MAX_TRIES
  if (!is.numeric(tries) || length(tries) != 1L || is.na(tries) ||
      !is.finite(tries) || tries < 1 || tries != round(tries) ||
      tries > .Machine$integer.max) {
    llm_config_abort(
      "LLM max_tries must be a single positive whole number of attempts"
    )
  }
  config$max_tries <- as.integer(tries)
  config <- spec$normalize_config(config)
  if (!isTRUE(config$auth_mode %in% spec$auth_modes)) {
    llm_config_abort(
      "LLM provider {.val {config$provider}} does not support auth_mode {.val {config$auth_mode}}"
    )
  }
  config
}

LLM_SELECTOR_URL_FIELDS <- c(
  "base_url", "endpoint", "workspace", "models_base_url"
)

llm_selector_identity <- function(config) {
  fields <- c(
    "provider", "auth_mode", "model", "tiers", "profile", "region",
    "base_url", "endpoint", "api_version", "project_id", "location", "cache",
    "workspace", "models_base_url", "account"
  )
  out <- config[intersect(fields, names(config))]
  for (field in intersect(LLM_SELECTOR_URL_FIELDS, names(out))) {
    value <- out[[field]]
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      value <- sub("[?#].*$", "", value)
      value <- sub("^([^:]+://)[^/@]+@", "\\1", value)
      out[[field]] <- value
    }
  }
  out[!vapply(out, is.null, logical(1))]
}

llm_sensitive_key <- function(name) {
  grepl(
    "api.?key|access.?token|authorization|credential|secret|bearer|password|token.?cache|cache.?path",
    name, ignore.case = TRUE
  )
}

llm_sensitive_env_values <- function() {
  names <- names(Sys.getenv())
  names <- names[grepl(
    "TOKEN|SECRET|PASSWORD|API_?KEY|ACCESS_?KEY|CREDENTIAL|AUTHORIZATION|BEARER",
    names, ignore.case = TRUE
  )]
  values <- unname(Sys.getenv(names, unset = ""))
  unique(values[nchar(values) >= 8L])
}

.llm_secret_state <- new.env(parent = emptyenv())
.llm_secret_state$values <- character()

llm_credential_secret_values <- function(value) {
  if (is.function(value) || is.environment(value) || is.null(value)) {
    return(character())
  }
  if (is.character(value)) {
    values <- value[!is.na(value) & nzchar(value)]
    bare <- sub("(?i)^Bearer[[:space:]]+", "", values, perl = TRUE)
    return(unique(c(values, bare[nzchar(bare)])))
  }
  if (!is.list(value)) return(character())
  unique(unlist(lapply(
    value, llm_credential_secret_values
  ), use.names = FALSE))
}

register_llm_secret_values <- function(value) {
  .llm_secret_state$values <- unique(c(
    .llm_secret_state$values, llm_credential_secret_values(value)
  ))
  invisible(value)
}

llm_registered_secret_values <- function() .llm_secret_state$values

clear_llm_registered_secrets <- function() {
  .llm_secret_state$values <- character()
  invisible(NULL)
}

tracked_llm_credentials <- function(callback) {
  if (is.null(callback)) return(NULL)
  force(callback)
  function() {
    value <- callback()
    register_llm_secret_values(value)
    value
  }
}

llm_secret_values <- function(value, field = NULL) {
  if (is.function(value) || is.environment(value) || is.null(value)) {
    return(character())
  }
  if (!is.null(field) && llm_sensitive_key(field)) {
    if (is.atomic(value)) {
      values <- as.character(value)
      return(unique(values[!is.na(values) & nzchar(values)]))
    }
    if (is.list(value)) {
      values <- unlist(lapply(
        value, llm_secret_values, field = "secret"
      ), use.names = FALSE)
      return(unique(values[nzchar(values)]))
    }
  }
  if (!is.list(value)) return(character())
  value_names <- names(value)
  if (is.null(value_names)) value_names <- rep(NA_character_, length(value))
  values <- unlist(Map(function(item, name) {
    llm_secret_values(item, if (is.na(name)) NULL else name)
  }, value, value_names), use.names = FALSE)
  unique(values[nzchar(values)])
}

redact_cloud_secret_patterns <- function(value) {
  if (!is.character(value) || !length(value)) return(value)
  patterns <- c(
    "(?i)(?:~|[A-Za-z]:|/)[^[:space:]\\\"']*[/\\\\]\\.aws[/\\\\]sso[/\\\\]cache(?:[/\\\\][^[:space:]\\\"']*)?",
    "(?i)(?:~|[A-Za-z]:|/)[^[:space:]\\\"']*[/\\\\]\\.aws[/\\\\]credentials",
    "(?i)(?:~|[A-Za-z]:|/)[^[:space:]\\\"']*[/\\\\]\\.azure(?:[/\\\\][^[:space:]\\\"']*)?",
    "(?i)(?:~|[A-Za-z]:|/)[^[:space:]\\\"']*[/\\\\]\\.config[/\\\\]gcloud(?:[/\\\\][^[:space:]\\\"']*)?",
    "\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b",
    "\\bya29\\.[A-Za-z0-9._~-]+\\b",
    "\\beyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+(?:\\.[A-Za-z0-9_-]+)?\\b",
    "(?i)\\bBearer[[:space:]]+[^[:space:],;\\\"']+"
  )
  replacements <- c(
    rep("[REDACTED_CLOUD_CACHE_PATH]", 4L),
    rep("[REDACTED]", 3L),
    "Bearer [REDACTED]"
  )
  for (i in seq_along(patterns)) {
    value <- gsub(patterns[[i]], replacements[[i]], value, perl = TRUE)
  }
  value
}

redact_llm_secrets <- function(
    value, env_values = llm_sensitive_env_values(),
    secret_values = unique(c(
      llm_secret_values(value), llm_registered_secret_values()
    ))) {
  if (is.function(value) || is.environment(value)) return(NULL)
  if (is.list(value)) {
    value_names <- names(value)
    if (is.null(value_names)) {
      return(lapply(
        value, redact_llm_secrets,
        env_values = env_values, secret_values = secret_values
      ))
    }
    keep <- !vapply(value_names, llm_sensitive_key, logical(1))
    value <- value[keep]
    for (field in intersect(LLM_SELECTOR_URL_FIELDS, names(value))) {
      field_value <- value[[field]]
      if (is.character(field_value) && length(field_value) == 1L &&
          !is.na(field_value)) {
        field_value <- sub("[?#].*$", "", field_value)
        field_value <- sub("^([^:]+://)[^/@]+@", "\\1", field_value)
        value[[field]] <- field_value
      }
    }
    return(lapply(
      value, redact_llm_secrets,
      env_values = env_values, secret_values = secret_values
    ))
  }
  if (is.character(value)) {
    for (secret in unique(c(env_values, secret_values))) {
      value <- gsub(secret, "[REDACTED]", value, fixed = TRUE)
    }
    value <- redact_cloud_secret_patterns(value)
  }
  value
}

llm_auth_command <- function(config) {
  spec <- llm_provider_spec_or_null(config$provider)
  if (is.null(spec)) NULL else spec$auth_command(config)
}

llm_access_context <- function(config) {
  spec <- llm_provider_spec_or_null(config$provider)
  if (is.null(spec)) "configured identity" else spec$access_context(config)
}

# A provider without a user-runnable login command still needs actionable
# guidance; it must never render an empty or literal `NULL` command.
llm_ambient_login_guidance <- function(config, command) {
  if (is.null(command)) {
    paste0(
      "Provide ambient credentials for provider ",
      config$provider %||% "<unknown>", ", then retry."
    )
  } else {
    paste0("Run `", command, "` yourself, then retry.")
  }
}

new_llm_access_condition <- function(class, message, reason, config,
                                     command = NULL) {
  structure(
    list(
      message = message, call = NULL, reason = reason,
      provider = config$provider, command = command,
      identity = llm_selector_identity(config)
    ),
    class = c(class, "sas2r_llm_access_error", "error", "condition")
  )
}

classify_llm_access_error <- function(error, config) {
  if (inherits(error, "sas2r_llm_access_error")) return(error)
  message <- conditionMessage(error)
  lower <- tolower(paste(class(error), message, collapse = " "))
  status <- condition_status_code(error)
  command <- llm_auth_command(config)
  context <- llm_access_context(config)

  if (inherits(error, "sas2r_llm_authentication_error") ||
      identical(status, 401L) ||
      grepl("expired|sso session|login required|not logged|unauthenticated|azure cli credential|application default credentials|adc", lower)) {
    if (!identical(config$auth_mode %||% "ambient", "ambient")) {
      return(new_llm_access_condition(
        "sas2r_llm_authentication_error",
        paste0("The configured authentication was rejected for ", context,
               ". Provider detail: ", message),
        "authentication_rejected", config
      ))
    }
    reason <- if (grepl("expired", lower)) "expired_login" else "not_logged_in"
    return(new_llm_access_condition(
      "sas2r_auth_required",
      paste0("Ambient identity is unavailable for ", context, ". ",
             llm_ambient_login_guidance(config, command),
             " Provider detail: ", message),
      reason, config, command
    ))
  }
  if (inherits(error, "sas2r_llm_permission_denied") ||
      identical(status, 403L) ||
      grepl("access.?denied|not authorized|permission denied|forbidden|insufficient permission", lower)) {
    return(new_llm_access_condition(
      "sas2r_llm_permission_denied",
      paste0("Identity lacks permission for ", context, ". Provider detail: ", message),
      "permission_denied", config
    ))
  }
  if (grepl("region", lower) && grepl("not available|mismatch|unsupported|wrong", lower)) {
    return(new_llm_access_condition(
      "sas2r_llm_region_mismatch",
      paste0("The selected model is unavailable in the configured region (", context,
             "). Provider detail: ", message),
      "region_mismatch", config
    ))
  }
  if (grepl("model|deployment|inference.?profile", lower) &&
      grepl("not found|does not exist|unknown|invalid", lower)) {
    return(new_llm_access_condition(
      "sas2r_llm_model_not_found",
      paste0("The configured model or deployment was not found (", context,
             "). Provider detail: ", message),
      "model_not_found", config
    ))
  }
  if (grepl("endpoint|host|url", lower) &&
      grepl("invalid|malformed|unknown|not found", lower)) {
    return(new_llm_access_condition(
      "sas2r_llm_endpoint_invalid",
      paste0("The configured endpoint is invalid (", context,
             "). Provider detail: ", message),
      "endpoint_invalid", config
    ))
  }
  if (grepl("could not resolve|network|connection|dns|unreachable|timed? out|timeout", lower)) {
    return(new_llm_access_condition(
      "sas2r_llm_network_error",
      paste0("Network access failed for ", context, ". Provider detail: ", message),
      "network_failure", config
    ))
  }
  new_llm_access_condition(
    "sas2r_llm_access_failed",
    paste0("LLM access validation failed for ", context,
           ". Provider detail: ", message),
    "access_failed", config
  )
}

llm_inventory_export <- function(provider) {
  spec <- llm_provider_spec_or_null(provider)
  if (is.null(spec)) NULL else spec$models_export
}

llm_inventory_args <- function(config) {
  spec <- llm_provider_spec_or_null(config$provider)
  if (is.null(spec)) return(list())
  compact_non_null(spec$build_models_args(config))
}

llm_model_inventory <- function(config) {
  if (!llm_inventory_selector_supported(config)) {
    return(structure(list(), class = "sas2r_inventory_unavailable"))
  }
  export <- llm_inventory_export(config$provider)
  if (is.null(export) || !requireNamespace("ellmer", quietly = TRUE) ||
      !exists(export, asNamespace("ellmer"), mode = "function", inherits = FALSE)) {
    return(structure(list(), class = "sas2r_inventory_unavailable"))
  }
  do.call(getExportedValue("ellmer", export), llm_inventory_args(config))
}

#' List models visible to the configured provider identity
#'
#' Uses the provider's read-only inventory route when ellmer exposes one. An
#' unavailable or non-authoritative inventory is reported explicitly and is
#' never represented as an empty list of models. Bedrock inference-profile and
#' custom-runtime selectors require probe-based access validation because the
#' foundation-model inventory does not enumerate them safely.
#' @param config An `llm:` mapping, a `sas2r_config`, or a normalized LLM list.
#' @return A `sas2r_llm_model_inventory` record.
#' @examples
#' \dontrun{
#' # Contacts the provider's read-only inventory route; needs credentials.
#' sas_llm_models(list(provider = "anthropic", model = "claude-sonnet-4-6"))
#'
#' # Or straight from a project configuration.
#' sas_llm_models(sas_config("path/to/_sas2r.yml"))
#' }
#' @export
sas_llm_models <- function(config) {
  config <- normalize_llm_config(config)
  if (is.null(config)) {
    llm_config_abort("LLM configuration is required to discover models")
  }
  public_redactor <- new_llm_audit_redactor(llm_config_secret_values(config))
  models <- if (!llm_inventory_selector_supported(config)) {
    structure(list(), class = "sas2r_inventory_unavailable")
  } else {
    tryCatch(
      llm_model_inventory(config),
      error = function(error) stop(sanitize_llm_public(
        classify_llm_access_error(error, config), public_redactor
      ))
    )
  }
  unavailable <- inherits(models, "sas2r_inventory_unavailable") ||
    (is.null(models) && is.null(llm_inventory_export(config$provider)))
  structure(list(
    status = if (unavailable) "inventory_unavailable" else "available",
    provider = config$provider,
    identity = llm_selector_identity(config),
    configured_model = config$model %||% configured_llm_models(config)[[1]],
    models = if (unavailable) NULL else models
  ), class = c("sas2r_llm_model_inventory", "list"))
}

llm_inventory_model_ids <- function(models) {
  id_fields <- c("id", "model_id", "modelId", "model", "name")
  clean_ids <- function(value) {
    if (!is.character(value)) return(NULL)
    unique(value[!is.na(value) & nzchar(value)])
  }

  if (is.character(models)) return(clean_ids(models))
  if (is.data.frame(models)) {
    field <- id_fields[id_fields %in% names(models)]
    if (!length(field)) return(NULL)
    return(clean_ids(as.character(models[[field[[1]]]])))
  }
  if (!is.list(models)) return(NULL)
  if (!length(models)) return(character())

  field <- id_fields[id_fields %in% names(models)]
  if (length(field)) {
    value <- models[[field[[1]]]]
    if (is.atomic(value)) return(clean_ids(as.character(value)))
  }
  container <- intersect(c("models", "data", "items", "results"), names(models))
  if (length(container)) {
    return(llm_inventory_model_ids(models[[container[[1]]]]))
  }

  records <- lapply(models, llm_inventory_model_ids)
  if (any(vapply(records, is.null, logical(1)))) return(NULL)
  unique(unlist(records, use.names = FALSE))
}
