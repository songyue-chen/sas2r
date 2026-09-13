# Explicit model settings are requirements. The shipped temperature default is
# still optional; it must not make reasoning-only models unusable.
required_model_settings <- function(spec, llm) {
  names <- union(names(llm$model_parameters), intersect(
    names(spec), c("reasoning_effort", "max_output_tokens", "top_p")
  ))
  names <- intersect(LLM_OPTIONAL_PARAMETERS, names)
  compact_non_null(stats::setNames(lapply(names, function(name) {
    resolve_model_parameter(spec, llm, name)
  }), names))
}

llm_settings_error <- function(message, reason = "settings_unverified") {
  structure(list(message = message, call = NULL, reason = reason),
            class = c("sas2r_llm_settings_error", "error", "condition"))
}

llm_settings_key <- function(llm, capabilities, parameters) {
  as.character(cli::hash_sha256(jsonlite::toJSON(list(
    provider = llm$provider, endpoint = llm$endpoint,
    model = capabilities$model %||% llm$model, api_version = llm$api_version,
    connector_version = if (isTRUE(attr(llm, "is_ellmer")))
      as.character(utils::packageVersion("ellmer")) else NULL,
    capability_hash = capabilities$record_hash,
    parameters = parameters[sort(names(parameters))]
  ), auto_unbox = TRUE, null = "null", digits = NA)))
}

llm_request_capabilities <- function(llm, request) {
  capabilities <- llm_capabilities_for(llm, request$tier, request$model)
  required <- request$required_parameters %||% character()
  parameters <- compact_non_null(request$parameters[required])
  if (!length(parameters)) return(capabilities)
  key <- llm_settings_key(llm, capabilities, parameters)
  verified <- is.environment(llm$verified_settings) &&
    exists(key, llm$verified_settings, inherits = FALSE)
  if (isTRUE(request$settings_probe) || verified) {
    for (name in names(parameters)) {
      if (!identical(capabilities[[name]], "unsupported")) {
        capabilities[[name]] <- "supported"
      }
    }
    capabilities$source <- paste(capabilities$source,
      if (isTRUE(request$settings_probe)) "settings_probe" else "verified_settings", sep = "+")
    capabilities <- rehash_capabilities(capabilities)
  }
  capabilities
}

# Check the provider's actual rejection before normalization removes HTTP
# details. In particular, a timeout or authentication failure is not evidence
# that a setting is unsupported.
rejected_required_setting <- function(error, required) {
  if (!condition_status_code(error) %in% c(400L, 422L)) return(FALSE)
  message <- tolower(conditionMessage(error))
  aliases <- list(
    reasoning_effort = c("reasoning_effort", "thinking_level", "thinkinglevel", "effort"),
    max_output_tokens = c("max_output_tokens", "max_tokens", "max_completion_tokens", "maxoutputtokens"),
    temperature = "temperature", top_p = c("top_p", "topp")
  )
  mentions <- any(vapply(unlist(aliases[required]), function(name) {
    grepl(paste0("(^|[^a-z0-9_])", name, "([^a-z0-9_]|$)"), message)
  }, logical(1)))
  mentions && grepl(
    "invalid|unsupported|not supported|unknown|unrecognized|not allowed|must be|should be|not permitted",
    message
  )
}

with_required_ellmer_settings <- function(request, code) {
  withCallingHandlers(code, warning = function(warning) {
    message <- conditionMessage(warning)
    required <- request$required_parameters %||% character()
    wire_names <- c(required, if ("max_output_tokens" %in% required) "max_tokens")
    if (grepl("Ignoring unsupported parameters", message, fixed = TRUE) &&
        any(vapply(wire_names, grepl, logical(1), x = message, fixed = TRUE))) {
      stop(llm_settings_error(paste(
        "The installed ellmer connector cannot forward a required setting:",
        message
      ), "settings_unsupported"))
    }
  })
}

assert_required_settings <- function(request, response) {
  required <- request$required_parameters %||% character()
  if (identical(response$error$class, "sas2r_llm_settings_error")) {
    stop(llm_condition_from_failure_response(response))
  }
  if (!length(required) || !identical(response$status, "completed")) {
    return(invisible(NULL))
  }
  missing <- required[!vapply(required, function(name) {
    isTRUE(all.equal(request$parameters[[name]],
                     response$effective_parameters[[name]]))
  }, logical(1))]
  if (length(missing)) stop(llm_settings_error(paste0(
    "Required model settings were not forwarded: ", paste(missing, collapse = ", "),
    ". Translation stopped; check the connector and model configuration."
  )))
  invisible(NULL)
}

settings_event <- function(llm, capabilities, parameters, status, log_dir) {
  entry <- list(
    timestamp = usage_timestamp(), agent = "settings", type = status,
    provider = llm$provider, resolved_model = capabilities$model %||% llm$model,
    requested_parameters = parameters,
    effective_parameters = if (status %in% c("verified", "cached")) parameters else NULL,
    capability_hash = capabilities$record_hash
  )
  llm_log(entry, dir = log_dir, redactor = llm_audit_redactor(llm))
  signalCondition(structure(c(entry, list(
    message = "", call = NULL, phase = "settings", event = status,
    status = status
  )), class = c("sas2r_progress", "condition")))
}

ensure_llm_settings <- function(llm, parameters, tier, log_dir, usage_budget) {
  if (is.null(llm) || !length(parameters) ||
      !usage_budget_allows_future(usage_budget)) return(invisible(NULL))
  capabilities <- llm_capabilities_for(llm, tier)
  key <- llm_settings_key(llm, capabilities, parameters)
  if (exists(key, llm$verified_settings, inherits = FALSE)) {
    return(invisible(key))
  }
  unsupported <- names(parameters)[vapply(names(parameters), function(name) {
    identical(capabilities[[name]], "unsupported")
  }, logical(1))]
  if (length(unsupported)) stop(llm_settings_error(paste0(
    "Configured model settings are declared unsupported: ",
    paste(unsupported, collapse = ", "),
    ". Remove the setting to use provider defaults, or use a compatible connector/model."
  ), "settings_unsupported"))
  settings_event(llm, capabilities, parameters, "probing", log_dir)
  candidate <- llm
  candidate$model_parameters <- parameters
  # A successful HTTP response alone cannot distinguish support from an endpoint
  # silently ignoring reasoning_effort. For unknown support, require rejection
  # of an invalid level followed by success with the exact requested level.
  if (!is.null(parameters$reasoning_effort) &&
      identical(capabilities$reasoning_effort, "unknown")) {
    candidate$model_parameters$reasoning_effort <- "sas2r-invalid-effort"
    control <- tryCatch(
      sas_llm_probe(candidate, log_dir = log_dir, tier = tier,
                    usage_budget = usage_budget),
      error = function(error) error
    )
    if (!inherits(control, "sas2r_llm_settings_error") ||
        !identical(control$reason, "settings_rejected")) {
      if (inherits(control, "condition")) stop(control)
      stop(llm_settings_error(paste(
        "Reasoning support remains unknown: the endpoint did not reject an invalid level.",
        "Translation stopped. Use a connector/model that validates reasoning settings,",
        "or explicitly declare supported capability after independently verifying it."
      )))
    }
    candidate$model_parameters <- parameters
  }
  passed <- sas_llm_probe(candidate, log_dir = log_dir, tier = tier,
                          usage_budget = usage_budget)
  if (!isTRUE(passed)) stop(llm_settings_error(paste(
    "Model settings could not be verified (incomplete response, unavailable structured output,",
    "or exhausted probe budget). Translation stopped; support remains unknown."
  )))
  assign(key, TRUE, llm$verified_settings)
  settings_event(llm, capabilities, parameters, "verified", log_dir)
  invisible(key)
}

prepare_migration_llm_settings <- function(state) {
  specs <- load_agent_specs()
  specs$translator <- load_agent_specs(state$project$project_dir)$translator
  seen <- character()
  for (role in c("translator", "reviewer", "fixer")) {
    llm <- state[[paste0(role, "_llm")]]
    if (is.null(llm)) next
    parameters <- required_model_settings(specs[[role]], llm)
    tier <- specs[[role]]$tier %||% "frontier"
    capabilities <- llm_capabilities_for(llm, tier)
    key <- llm_settings_key(llm, capabilities, parameters)
    if (key %in% seen) next
    seen <- c(seen, key)
    cached <- exists(key, llm$verified_settings, inherits = FALSE)
    ensure_llm_settings(llm, parameters, tier, state$paths$state, state$usage_budget)
    if (!length(parameters) || cached) {
      settings_event(llm, capabilities, parameters,
                     if (cached) "cached" else "provider_defaults", state$paths$state)
    }
  }
  invisible(NULL)
}
