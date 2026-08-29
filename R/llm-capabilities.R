LLM_OPTIONAL_PARAMETERS <- c(
  "temperature", "top_p", "reasoning_effort", "max_output_tokens"
)
LLM_CONSTRAINABLE_CAPABILITIES <- c(
  LLM_OPTIONAL_PARAMETERS, "structured_output", "tool_calling",
  "tools_with_structured_output"
)
LLM_OPTIONAL_PARAMETER_WIRE_NAMES <- c(
  max_output_tokens = "max_tokens"
)

LLM_CAPABILITY_STATES <- c("supported", "unsupported", "unknown")
LLM_STRUCTURED_OUTPUT_MODES <- c("native", "fallback", "unsupported", "unknown")
LLM_TOOL_CALLING_MODES <- c("native", "unsupported", "unknown")

.llm_capability_cache <- new.env(parent = emptyenv())
.llm_tested_capability_registry <- new.env(parent = emptyenv())
.llm_rejected_capabilities <- new.env(parent = emptyenv())

validate_capability_value <- function(value, allowed, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !value %in% allowed) {
    cli::cli_abort(
      "invalid LLM capability {.field {field}}: {.val {value}}",
      class = "sas2r_llm_capability_error"
    )
  }
  value
}

#' Construct a normalized LLM capability record
#'
#' Optional parameter support is tri-state. Structured-output and tool-calling
#' fields are explicit transport modes rather than inferred model families.
#' @noRd
llm_capabilities <- function(
    temperature = "unknown", top_p = "unknown",
    reasoning_effort = "unknown", max_output_tokens = "unknown",
    structured_output = "unknown", tool_calling = "unknown",
    tools_with_structured_output = "unknown", source = "unknown",
    provider = NULL, endpoint = NULL, model = NULL, api_version = NULL,
    cache_key = NULL, record_hash = NULL) {
  values <- list(
    temperature = validate_capability_value(
      temperature, LLM_CAPABILITY_STATES, "temperature"
    ),
    top_p = validate_capability_value(top_p, LLM_CAPABILITY_STATES, "top_p"),
    reasoning_effort = validate_capability_value(
      reasoning_effort, LLM_CAPABILITY_STATES, "reasoning_effort"
    ),
    max_output_tokens = validate_capability_value(
      max_output_tokens, LLM_CAPABILITY_STATES, "max_output_tokens"
    ),
    structured_output = validate_capability_value(
      structured_output, LLM_STRUCTURED_OUTPUT_MODES, "structured_output"
    ),
    tool_calling = validate_capability_value(
      tool_calling, LLM_TOOL_CALLING_MODES, "tool_calling"
    ),
    tools_with_structured_output = validate_capability_value(
      tools_with_structured_output, LLM_CAPABILITY_STATES,
      "tools_with_structured_output"
    ),
    source = as.character(source)[1],
    provider = provider,
    endpoint = endpoint,
    model = model,
    api_version = api_version,
    cache_key = cache_key,
    record_hash = record_hash
  )
  structure(values, class = c("sas2r_llm_capabilities", "list"))
}

capability_cache_key <- function(provider, endpoint, model, api_version) {
  identity <- list(
    provider = provider, endpoint = endpoint, model = model,
    api_version = api_version
  )
  paste0(
    "cap_",
    as.character(cli::hash_sha256(jsonlite::toJSON(
      identity, auto_unbox = TRUE, null = "null", na = "null"
    )))
  )
}

# Provider-adapter support only. These defaults describe what the adapter can
# carry, never what an individual model supports, so they can only fill fields
# that are still "unknown" and never turn an unknown model feature into a
# supported one.
adapter_capability_metadata <- function(provider) {
  if (identical(provider, "mock")) return(LLM_MOCK_CAPABILITY_DEFAULTS)
  spec <- llm_provider_spec_or_null(provider)
  if (is.null(spec)) list() else spec$capability_defaults
}

tested_capability_metadata <- function(cache_key) {
  if (!exists(cache_key, envir = .llm_tested_capability_registry,
              inherits = FALSE)) return(list())
  get(cache_key, envir = .llm_tested_capability_registry, inherits = FALSE)
}

apply_capability_defaults <- function(record, values, source = NULL) {
  if (!length(values)) return(record)
  eligible <- names(values)[vapply(names(values), function(name) {
    identical(record[[name]], "unknown")
  }, logical(1))]
  if (!length(eligible)) return(record)
  apply_capability_values(record, values[eligible], source = source)
}

rehash_capabilities <- function(record) {
  payload <- unclass(record)
  payload$record_hash <- NULL
  record$record_hash <- as.character(cli::hash_sha256(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null"
  )))
  record
}

normalize_transport_constraints <- function(constraints = list()) {
  if (is.null(constraints)) constraints <- list()
  if (!is.list(constraints)) {
    cli::cli_abort(
      "LLM transport constraints must be a named list",
      class = "sas2r_llm_capability_error"
    )
  }
  if (!length(constraints)) return(list())
  constraint_names <- names(constraints)
  if (is.null(constraint_names) || anyNA(constraint_names) ||
      any(!nzchar(constraint_names)) || anyDuplicated(constraint_names)) {
    cli::cli_abort(
      "LLM transport constraints must have unique non-empty names",
      class = "sas2r_llm_capability_error"
    )
  }
  unknown <- setdiff(constraint_names, LLM_CONSTRAINABLE_CAPABILITIES)
  if (length(unknown)) {
    cli::cli_abort(
      "unknown LLM transport constraint{?s}: {.field {unknown}}",
      class = "sas2r_llm_capability_error"
    )
  }
  invalid <- constraint_names[!vapply(
    constraints, identical, logical(1), y = "unsupported"
  )]
  if (length(invalid)) {
    cli::cli_abort(
      "LLM transport constraints may only set capabilities to {.val unsupported}: {.field {invalid}}",
      class = "sas2r_llm_capability_error"
    )
  }
  constraints
}

apply_transport_constraints <- function(record, constraints = list()) {
  if (!inherits(record, "sas2r_llm_capabilities")) {
    cli::cli_abort(
      "expected a sas2r_llm_capabilities record",
      class = "sas2r_llm_capability_error"
    )
  }
  constraints <- normalize_transport_constraints(constraints)
  if (!length(constraints)) return(record)

  args <- unclass(record)
  for (name in names(constraints)) args[[name]] <- "unsupported"
  source_parts <- strsplit(args$source %||% "unknown", "+", fixed = TRUE)[[1]]
  args$source <- paste(
    unique(c(source_parts, "transport_constraint")), collapse = "+"
  )
  rehash_capabilities(do.call(llm_capabilities, args))
}

apply_capability_values <- function(record, values, source = NULL) {
  if (is.null(values)) return(record)
  unknown <- setdiff(names(values), LLM_CONSTRAINABLE_CAPABILITIES)
  if (length(unknown)) {
    cli::cli_abort(
      "unknown capability override{?s}: {.field {unknown}}",
      class = "sas2r_llm_capability_error"
    )
  }
  args <- unclass(record)
  for (name in names(values)) args[[name]] <- values[[name]]
  if (!is.null(source)) args$source <- source
  do.call(llm_capabilities, args)
}

#' Resolve exact provider/deployment/model capabilities
#'
#' Resolution order is explicit override, adapter metadata, tested exact
#' registry, then unknown. The cache stores only the non-override base record.
#' @noRd
resolve_model_capabilities <- function(provider, endpoint = NULL, model,
                                       api_version = NULL, overrides = NULL) {
  if (!is.character(provider) || length(provider) != 1L || !nzchar(provider)) {
    cli::cli_abort("provider is required for capability resolution",
                   class = "sas2r_llm_capability_error")
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    cli::cli_abort("resolved model is required for capability resolution",
                   class = "sas2r_llm_capability_error")
  }
  key <- capability_cache_key(provider, endpoint, model, api_version)
  if (exists(key, envir = .llm_capability_cache, inherits = FALSE)) {
    base <- get(key, envir = .llm_capability_cache, inherits = FALSE)
  } else {
    base <- llm_capabilities(
      provider = provider, endpoint = endpoint, model = model,
      api_version = api_version, cache_key = key
    )
    metadata <- adapter_capability_metadata(provider)
    base <- apply_capability_defaults(base, metadata, source = "adapter_metadata")
    registry <- tested_capability_metadata(key)
    base <- apply_capability_defaults(base, registry, source = "tested_registry")
    base <- rehash_capabilities(base)
    assign(key, base, envir = .llm_capability_cache)
  }

  out <- if (is.null(overrides)) base else
    apply_capability_values(base, overrides, source = "explicit_override")
  if (exists(key, envir = .llm_rejected_capabilities, inherits = FALSE)) {
    rejected <- get(key, envir = .llm_rejected_capabilities, inherits = FALSE)
    for (parameter in rejected) out[[parameter]] <- "unsupported"
    out$source <- "runtime_rejection"
  }
  rehash_capabilities(out)
}

new_request_id <- function() {
  paste0("req_", substr(as.character(cli::hash_sha256(tempfile("sas2r-request-"))), 1L, 24L))
}

validate_llm_messages <- function(messages) {
  if (!is.list(messages) || !length(messages)) {
    cli::cli_abort("LLM request messages must be a non-empty list",
                   class = "sas2r_llm_request_error")
  }
  roles <- vapply(messages, function(message) {
    if (!is.list(message) || !is.character(message$role) ||
        length(message$role) != 1L) return(NA_character_)
    message$role
  }, character(1))
  allowed <- c("system", "user", "assistant", "tool")
  if (anyNA(roles) || any(!roles %in% allowed)) {
    cli::cli_abort("LLM messages contain an invalid role",
                   class = "sas2r_llm_request_error")
  }
  invisible(messages)
}

#' Construct the provider-neutral request contract
#' @noRd
llm_request <- function(messages, tier = "frontier", tools = list(),
                        output_schema = NULL, schema_name = NULL,
                        schema_version = NULL, schema_mode = NULL,
                        temperature = NULL, top_p = NULL,
                        reasoning_effort = NULL, max_output_tokens = NULL,
                        model = NULL, request_id = NULL,
                        phase = "finalization") {
  validate_llm_messages(messages)
  if (!is.list(tools)) {
    cli::cli_abort("registered tools must be a list",
                   class = "sas2r_llm_request_error")
  }
  if (!phase %in% c("gathering", "finalization", "probe")) {
    cli::cli_abort("invalid LLM request phase {.val {phase}}",
                   class = "sas2r_llm_request_error")
  }
  if (!is.null(schema_mode) &&
      !schema_mode %in% c("native", "fallback")) {
    cli::cli_abort("invalid schema mode {.val {schema_mode}}",
                   class = "sas2r_llm_request_error")
  }
  structure(list(
    request_id = request_id %||% new_request_id(),
    messages = messages,
    tier = as.character(tier)[1],
    model = model,
    tools = tools,
    output_schema = output_schema,
    schema_name = schema_name,
    schema_version = schema_version,
    schema_mode = schema_mode,
    phase = phase,
    parameters = list(
      temperature = temperature,
      top_p = top_p,
      reasoning_effort = reasoning_effort,
      max_output_tokens = max_output_tokens
    )
  ), class = c("sas2r_llm_request", "list"))
}

#' Select only explicitly requested, supported model parameters
#' @noRd
effective_model_params <- function(request, capabilities) {
  if (!inherits(request, "sas2r_llm_request")) {
    cli::cli_abort("expected a sas2r_llm_request",
                   class = "sas2r_llm_request_error")
  }
  if (!inherits(capabilities, "sas2r_llm_capabilities")) {
    cli::cli_abort("expected a sas2r_llm_capabilities record",
                   class = "sas2r_llm_capability_error")
  }
  out <- list()
  for (name in LLM_OPTIONAL_PARAMETERS) {
    value <- request$parameters[[name]]
    if (!is.null(value) && identical(capabilities[[name]], "supported")) {
      out[[name]] <- value
    }
  }
  out
}

#' Resolve one optional model parameter from the agent spec, then the config
#'
#' A shipped agent spec sets `temperature: 0` deliberately, so a project-level
#' default must not silently undo it: the spec wins where it speaks, and the
#' configured value fills in where it does not. `reasoning_effort` is set by no
#' shipped spec, so in practice it comes entirely from configuration.
#' @noRd
resolve_model_parameter <- function(spec, llm, name) {
  spec[[name]] %||% (llm$model_parameters %||% list())[[name]]
}

#' Optional parameters the capability gate withheld from a request
#'
#' `effective_model_params()` forwards a parameter only when its capability is
#' exactly `"supported"`, so a request that asks for `temperature = 0` against a
#' provider whose capability is `"unknown"` is answered at the provider default.
#' That omission is invisible on the wire; naming it here keeps the audit record
#' honest about what was actually sent.
#' @noRd
withheld_model_params <- function(request, capabilities) {
  effective <- effective_model_params(request, capabilities)
  requested <- LLM_OPTIONAL_PARAMETERS[
    vapply(LLM_OPTIONAL_PARAMETERS,
           function(name) !is.null(request$parameters[[name]]), logical(1))
  ]
  setdiff(requested, names(effective))
}

llm_optional_parameter_error <- function(parameter, message = NULL) {
  if (!parameter %in% LLM_OPTIONAL_PARAMETERS) {
    cli::cli_abort("unknown optional LLM parameter {.val {parameter}}",
                   class = "sas2r_llm_capability_error")
  }
  structure(
    list(
      message = message %||% paste0("optional parameter not supported: ", parameter),
      call = NULL, parameter = parameter, status_code = 400L
    ),
    class = c("sas2r_llm_optional_parameter_error", "error", "condition")
  )
}

optional_parameter_from_error <- function(error, sent_parameters) {
  if (inherits(error, "sas2r_llm_optional_parameter_error")) {
    parameter <- error$parameter
    if (is.character(parameter) && length(parameter) == 1L &&
        parameter %in% sent_parameters) return(parameter)
    return(NULL)
  }
  if (!condition_status_code(error) %in% c(400L, 422L)) return(NULL)
  message <- tolower(conditionMessage(error))
  unsupported <- grepl(
    "unsupported|not supported|unknown|unrecognized|not allowed", message
  )
  if (!unsupported) return(NULL)
  matches <- sent_parameters[vapply(sent_parameters, function(parameter) {
    candidates <- c(
      parameter, unname(LLM_OPTIONAL_PARAMETER_WIRE_NAMES[parameter])
    )
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    any(vapply(candidates, function(candidate) {
      grepl(
        paste0("(^|[^a-z0-9_])", candidate, "([^a-z0-9_]|$)"),
        message
      )
    }, logical(1)))
  }, logical(1))]
  if (length(matches) == 1L) matches else NULL
}

record_capability_rejection <- function(capabilities, parameter) {
  key <- capabilities$cache_key
  if (is.null(key)) return(invisible(NULL))
  rejected <- if (exists(key, envir = .llm_rejected_capabilities,
                         inherits = FALSE)) {
    get(key, envir = .llm_rejected_capabilities, inherits = FALSE)
  } else character()
  assign(key, unique(c(rejected, parameter)), envir = .llm_rejected_capabilities)
  invisible(NULL)
}

#' Invoke a transport with one narrowly classified capability downgrade retry
#' @noRd
invoke_with_capability_retry <- function(request, capabilities, transport) {
  params <- effective_model_params(request, capabilities)
  withheld <- withheld_model_params(request, capabilities)
  first <- tryCatch(
    list(value = invoke_capability_transport(
      request, params, transport, retry_of = NULL
    ), error = NULL),
    error = function(error) list(value = NULL, error = error)
  )
  if (is.null(first$error)) {
    response <- normalize_provider_response(first$value, request = request)
    response$retry_count <- 0L
    response$downgraded_parameters <- character()
    response$effective_parameters <- params
    response$withheld_parameters <- withheld
    return(response)
  }

  parameter <- optional_parameter_from_error(first$error, names(params))
  can_downgrade <- !is.null(parameter)
  if (!can_downgrade) {
    response <- normalize_provider_response(first$error, request = request)
    response$retry_count <- 0L
    response$downgraded_parameters <- character()
    response$effective_parameters <- params
    response$withheld_parameters <- withheld
    return(response)
  }

  downgraded <- capabilities
  downgraded[[parameter]] <- "unsupported"
  source_parts <- strsplit(
    downgraded$source %||% "unknown", "+", fixed = TRUE
  )[[1L]]
  downgraded$source <- paste(
    unique(c(source_parts, "runtime_rejection")), collapse = "+"
  )
  downgraded <- rehash_capabilities(downgraded)
  record_capability_rejection(capabilities, parameter)
  retry_params <- effective_model_params(request, downgraded)
  second <- tryCatch(
    invoke_capability_transport(
      request, retry_params, transport, retry_of = request$request_id
    ),
    error = function(error) error
  )
  response <- normalize_provider_response(second, request = request)
  response$retry_count <- 1L
  response$downgraded_parameters <- parameter
  response$effective_parameters <- retry_params
  # A runtime rejection is a downgrade, not a capability-gated omission: report
  # only what the gate withheld before the provider was ever asked.
  response$withheld_parameters <- withheld
  response$capability_hash <- downgraded$record_hash
  response
}

invoke_capability_transport <- function(request, params, transport,
                                        retry_of = NULL) {
  # Read from the call scope, never from the request: the hook closes over the
  # audit context and the adapter's redactor, and the request goes on the wire.
  callback <- current_usage_attempt_callback()
  if (is.function(callback)) {
    callback(request, params, transport, retry_of = retry_of)
  } else {
    transport(request, params)
  }
}
