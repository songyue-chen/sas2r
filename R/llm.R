if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

new_llm_audit_redactor <- function(redaction_secrets = character()) {
  known_secrets <- llm_credential_secret_values(redaction_secrets)
  local({
    secrets <- known_secrets
    function(value) redact_llm_secrets(
      value,
      secret_values = unique(c(
        secrets, llm_secret_values(value), llm_registered_secret_values()
      ))
    )
  })
}

sanitize_llm_public <- function(value, redactor = redact_llm_secrets) {
  if (!is.function(redactor)) {
    cli::cli_abort("LLM public redactor must be a function",
                   class = "sas2r_llm_redaction_error")
  }
  public_value <- value
  if (inherits(value, "condition")) {
    allowed <- c(
      "message", "reason", "provider", "command", "identity",
      "status", "status_code", "stage", "requested_model", "resolved_model"
    )
    public_value <- unclass(value)[intersect(allowed, names(value))]
    public_value$message <- conditionMessage(value)
    public_value$call <- NULL
  }
  sanitized <- redactor(public_value)
  if (is.null(sanitized)) return(NULL)
  if (is.object(value)) class(sanitized) <- class(value)
  original_usage <- attr(value, "usage", exact = TRUE)
  if (!is.null(original_usage)) {
    attr(sanitized, "usage") <- closed_llm_failure_usage(original_usage)
  }
  original_cost <- attr(value, "cost_usd", exact = TRUE)
  if (!is.null(original_cost)) {
    attr(sanitized, "cost_usd") <- scalar_number_or_na(original_cost)
  }
  for (attribute in c(
    "cost_status", "cost_currency", "cost_source", "cost_source_version",
    "cost_effective_date", "cost_rate_dimensions", "cost_provenance"
  )) {
    attribute_value <- attr(value, attribute, exact = TRUE)
    if (!is.null(attribute_value)) attr(sanitized, attribute) <- attribute_value
  }
  if (inherits(value, "condition") && is.list(sanitized)) {
    sanitized$call <- NULL
  }
  sanitized
}

closed_llm_failure_usage <- function(usage) {
  normalized_usage(list(usage = usage))
}

LLM_FAILURE_REASONS <- list(
  sas2r_auth_required = c("not_logged_in", "expired_login"),
  sas2r_llm_authentication_error = "authentication_rejected",
  sas2r_llm_permission_denied = "permission_denied",
  sas2r_llm_rate_limit = "rate_limited",
  sas2r_llm_timeout = "timed_out",
  sas2r_llm_invalid_schema = "invalid_schema",
  sas2r_llm_transport_error = "transport_failure",
  sas2r_llm_config_error = "configuration_error",
  sas2r_llm_capability_error = "capability_resolution_failed",
  sas2r_llm_region_mismatch = "region_mismatch",
  sas2r_llm_model_not_found = "model_not_found",
  sas2r_llm_endpoint_invalid = "endpoint_invalid",
  sas2r_llm_network_error = "network_failure",
  sas2r_llm_access_failed = "access_failed",
  sas2r_llm_inventory_shape_error = "inventory_shape"
)

LLM_FAILURE_DIAGNOSTIC_FIELDS <- c(
  "provider", "command", "identity", "stage",
  "requested_model", "resolved_model"
)

recognized_llm_failure_class <- function(error) {
  reason <- error$reason %||% NULL
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) ||
      !nzchar(reason)) {
    return(NULL)
  }
  classes <- intersect(class(error), names(LLM_FAILURE_REASONS))
  for (candidate in classes) {
    if (reason %in% LLM_FAILURE_REASONS[[candidate]]) return(candidate)
  }
  NULL
}

recognized_llm_failure_reason <- function(error) {
  if (is.null(recognized_llm_failure_class(error))) return(NULL)
  error$reason
}

normalized_llm_failure_metadata <- function(error, failure_class) {
  trusted <- recognized_llm_failure_class(error)
  reason <- recognized_llm_failure_reason(error)
  if (is.null(reason) && failure_class %in% names(LLM_FAILURE_REASONS)) {
    reason <- LLM_FAILURE_REASONS[[failure_class]][[1L]]
  }
  diagnostics <- if (is.null(trusted)) {
    list()
  } else {
    raw <- unclass(error)
    raw[intersect(LLM_FAILURE_DIAGNOSTIC_FIELDS, names(raw))]
  }
  metadata <- c(list(
    class = failure_class,
    reason = reason,
    message = conditionMessage(error)
  ), diagnostics)
  redact_llm_secrets(
    metadata,
    secret_values = unique(c(
      llm_secret_values(error), llm_registered_secret_values()
    ))
  )
}

llm_failure_reason <- function(error, fallback = "failed") {
  reason <- error$reason %||% NULL
  if (is.character(reason) && length(reason) == 1L && !is.na(reason) &&
      nzchar(reason)) {
    return(reason)
  }
  classes <- intersect(class(error), names(LLM_FAILURE_REASONS))
  if (length(classes)) LLM_FAILURE_REASONS[[classes[[1]]]][[1]] else fallback
}

copy_llm_failure_metrics <- function(source, target) {
  usage <- attr(source, "usage", exact = TRUE)
  if (!is.null(usage)) attr(target, "usage") <- closed_llm_failure_usage(usage)
  cost <- attr(source, "cost_usd", exact = TRUE)
  if (!is.null(cost)) attr(target, "cost_usd") <- scalar_number_or_na(cost)
  for (attribute in c(
    "cost_status", "cost_currency", "cost_source", "cost_source_version",
    "cost_effective_date", "cost_rate_dimensions", "cost_provenance"
  )) {
    attribute_value <- attr(source, attribute, exact = TRUE)
    if (!is.null(attribute_value)) attr(target, attribute) <- attribute_value
  }
  target
}

LLM_REQUEST_POLICY_FIELDS <- c(
  "timeout_scope", "timeout_seconds", "transport_max_tries"
)

normalize_llm_request_policy <- function(policy = list()) {
  if (is.null(policy)) return(list())
  if (!is.list(policy) ||
      (length(policy) &&
       (is.null(names(policy)) || any(!nzchar(names(policy))) ||
        anyDuplicated(names(policy))))) {
    cli::cli_abort(
      "LLM request policy must be a named mapping",
      class = "sas2r_llm_error"
    )
  }
  unknown <- setdiff(names(policy), LLM_REQUEST_POLICY_FIELDS)
  if (length(unknown)) {
    cli::cli_abort(
      "Unknown LLM request policy field{?s}: {.field {unknown}}",
      class = "sas2r_llm_error"
    )
  }
  if (length(policy) &&
      !setequal(names(policy), LLM_REQUEST_POLICY_FIELDS)) {
    cli::cli_abort(
      "A non-empty LLM request policy must define every policy field",
      class = "sas2r_llm_error"
    )
  }
  if (length(policy) &&
      !identical(policy$timeout_scope, "http_attempt_absolute")) {
    cli::cli_abort(
      "LLM timeout_scope must be http_attempt_absolute",
      class = "sas2r_llm_error"
    )
  }
  if (length(policy) &&
      (!is.numeric(policy$timeout_seconds) ||
       length(policy$timeout_seconds) != 1L ||
       is.na(policy$timeout_seconds) ||
       !is.finite(policy$timeout_seconds) ||
       policy$timeout_seconds <= 0)) {
    cli::cli_abort("invalid LLM request-policy timeout", class = "sas2r_llm_error")
  }
  if (length(policy) &&
      (!is.numeric(policy$transport_max_tries) ||
       length(policy$transport_max_tries) != 1L ||
       is.na(policy$transport_max_tries) ||
       !is.finite(policy$transport_max_tries) ||
       policy$transport_max_tries < 1 ||
       policy$transport_max_tries != round(policy$transport_max_tries) ||
       policy$transport_max_tries > .Machine$integer.max)) {
    cli::cli_abort("invalid LLM request-policy tries", class = "sas2r_llm_error")
  }
  if (!length(policy)) return(list())
  list(
    timeout_scope = policy$timeout_scope,
    timeout_seconds = as.numeric(policy$timeout_seconds),
    transport_max_tries = as.integer(policy$transport_max_tries)
  )
}

new_llm <- function(request, provider, capabilities = NULL, model = NULL,
                    endpoint = NULL, api_version = NULL,
                    capabilities_for = NULL, redaction_secrets = character(),
                    model_parameters = list(), request_policy = list(),
                    transport_constraints = list()) {
  if (!is.function(request)) {
    cli::cli_abort("LLM transport must be a request function",
                   class = "sas2r_llm_error")
  }
  if (is.null(capabilities)) {
    capabilities <- if (identical(provider, "mock")) {
      do.call(llm_capabilities, c(
        LLM_MOCK_CAPABILITY_DEFAULTS, list(source = "mock")
      ))
    } else if (!is.null(model)) {
      resolve_model_capabilities(provider, endpoint, model, api_version)
    } else {
      llm_capabilities()
    }
  }
  request_policy <- normalize_llm_request_policy(request_policy)
  transport_constraints <- normalize_transport_constraints(
    transport_constraints
  )
  capabilities <- apply_transport_constraints(
    capabilities, transport_constraints
  )
  audit_redactor <- new_llm_audit_redactor(redaction_secrets)
  structure(list(
    request = request, provider = provider, capabilities = capabilities,
    model = model, endpoint = endpoint, api_version = api_version,
    model_parameters = model_parameters,
    request_policy = request_policy,
    capabilities_for = capabilities_for,
    transport_constraints = transport_constraints
  ), class = "sas2r_llm", audit_redactor = audit_redactor)
}

llm_audit_redactor <- function(llm) {
  redactor <- attr(llm, "audit_redactor", exact = TRUE)
  if (is.function(redactor)) redactor else redact_llm_secrets
}

llm_capabilities_for <- function(llm, tier = "frontier", model = NULL) {
  capabilities <- if (is.function(llm$capabilities_for)) {
    llm$capabilities_for(tier = tier, model = model)
  } else {
    llm$capabilities %||% llm_capabilities()
  }
  if (!inherits(capabilities, "sas2r_llm_capabilities")) {
    cli::cli_abort(
      "LLM capability resolver must return a sas2r_llm_capabilities record",
      class = "sas2r_llm_capability_error"
    )
  }
  apply_transport_constraints(
    capabilities, llm$transport_constraints %||% list()
  )
}

#' Mock LLM backend for offline testing
#' @noRd
mock_llm <- function(responses, capabilities = NULL) {
  i <- 0L
  new_llm(function(request) {
    if (!inherits(request, "sas2r_llm_request")) {
      cli::cli_abort("mock_llm requires a sas2r_llm_request",
                     class = "sas2r_llm_request_error")
    }
    i <<- i + 1L
    if (i > length(responses)) {
      cli::cli_abort(
        "mock_llm exhausted after {length(responses)} response{?s}",
        class = "sas2r_llm_error"
      )
    }
    normalize_provider_response(
      responses[[i]], request = request, provider = "mock"
    )
  }, provider = "mock", capabilities = capabilities)
}

strip_fences <- function(txt) {
  if (!is.character(txt) || length(txt) == 0L) return(txt)
  text <- trimws(txt)
  if (grepl("^```(?:json)?", text, ignore.case = TRUE)) {
    text <- sub("^```(?:json)?[ \t]*\r?\n?", "", text, ignore.case = TRUE)
    text <- sub("\r?\n?```[ \t]*$", "", text)
    text <- trimws(text)
  }
  text
}

.sas2r_state <- new.env(parent = emptyenv())
.sas2r_state$warned_cost_unknown <- FALSE

selected_llm_model <- function(cfg, request) {
  selected_tier <- request$tier %||% "frontier"
  if (identical(selected_tier, "cheap") && !is.null(cfg$tiers) &&
      is.null(cfg$tiers$cheap) && !is.null(cfg$tiers$fast)) {
    selected_tier <- "fast"
  }
  model <- if (!is.null(cfg$tiers) && !is.null(cfg$tiers[[selected_tier]])) {
    cfg$tiers[[selected_tier]]
  } else {
    request$model %||% cfg$model
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    cli::cli_abort("an explicit LLM model or tier mapping is required",
                   class = "sas2r_llm_model_required")
  }
  model
}

ellmer_constructor <- function(provider) {
  llm_provider_spec(provider)$chat_export
}

map_json_schema_children <- function(schema, mapper) {
  if (!is.list(schema)) return(schema)
  schema_maps <- c(
    "$defs", "definitions", "dependentSchemas", "patternProperties",
    "properties"
  )
  for (name in intersect(names(schema) %||% character(), schema_maps)) {
    if (is.list(schema[[name]])) {
      schema[[name]] <- lapply(schema[[name]], mapper)
    }
  }
  schema_arrays <- c("allOf", "anyOf", "oneOf", "prefixItems")
  for (name in intersect(names(schema) %||% character(), schema_arrays)) {
    value <- schema[[name]]
    if (!is.list(value)) next
    schema[[name]] <- if (length(names(value) %||% character())) {
      mapper(value)
    } else {
      lapply(value, mapper)
    }
  }
  schema_singles <- c(
    "additionalProperties", "contains", "contentSchema", "else", "if",
    "not", "propertyNames", "then", "unevaluatedItems",
    "unevaluatedProperties"
  )
  for (name in intersect(names(schema) %||% character(), schema_singles)) {
    if (is.list(schema[[name]])) schema[[name]] <- mapper(schema[[name]])
  }
  if (is.list(schema$items)) {
    value <- schema$items
    schema$items <- if (
      length(value) && !length(names(value) %||% character()) &&
        all(vapply(value, is.list, logical(1)))
    ) {
      lapply(value, mapper)
    } else {
      mapper(value)
    }
  }
  schema
}

normalize_json_schema_arrays <- function(schema) {
  if (!is.list(schema)) return(schema)
  array_keywords <- c(
    "required", "enum", "allOf", "anyOf", "oneOf", "prefixItems", "examples"
  )
  for (name in intersect(names(schema) %||% character(), array_keywords)) {
    value <- schema[[name]]
    schema[[name]] <- if (!is.list(value)) {
      as.list(value)
    } else if (length(names(value) %||% character())) {
      list(value)
    } else {
      value
    }
  }
  if (is.list(schema$dependentRequired)) {
    schema$dependentRequired <- lapply(schema$dependentRequired, function(value) {
      if (is.list(value)) value else as.list(value)
    })
  }
  map_json_schema_children(schema, normalize_json_schema_arrays)
}

normalize_strict_output_objects <- function(schema) {
  if (!is.list(schema)) return(schema)
  # Strict schema dialects reject document fragment identifiers, schema refs,
  # and validation keywords unsupported by strict grammar parsers.
  schema$`$id` <- NULL
  schema$`$schema` <- NULL
  schema$`x-sas2r-schema-version` <- NULL
  schema$uniqueItems <- NULL
  schema$minLength <- NULL
  schema$maxLength <- NULL
  schema$minimum <- NULL
  schema$maximum <- NULL
  schema$pattern <- NULL
  schema$format <- NULL

  # Strict dialects require anyOf instead of oneOf
  if (!is.null(schema$oneOf)) {
    schema$anyOf <- schema$oneOf
    schema$oneOf <- NULL
  }

  # Multi-type arrays must be translated to anyOf with typed schemas
  if (is.character(schema$type) && length(schema$type) > 1L) {
    types <- schema$type
    schema$type <- NULL
    schema$anyOf <- lapply(types, function(t) list(type = t))
  }

  # Empty schemas without a type must declare permissive union types
  if (length(schema) == 0L) {
    return(list(anyOf = list(
      list(type = "string"),
      list(type = "number"),
      list(type = "boolean"),
      list(type = "null")
    )))
  }

  type <- schema$type %||% NULL
  if (is.character(type) && length(type) == 1L && "object" %in% type) {
    if (is.null(schema$properties)) {
      schema$properties <- structure(list(), names = character(0))
    }
    schema$required <- names(schema$properties) %||% character()
    schema$additionalProperties <- FALSE
  }

  map_json_schema_children(schema, normalize_strict_output_objects)
}

ellmer_type_from_schema <- function(schema, required = TRUE,
                                    strict_objects = FALSE) {
  if (isTRUE(strict_objects)) {
    schema <- normalize_strict_output_objects(schema)
  }
  schema <- normalize_json_schema_arrays(schema)
  schema_text <- jsonlite::toJSON(
    schema, auto_unbox = TRUE, null = "null", na = "null"
  )
  type <- getExportedValue("ellmer", "type_from_schema")(text = schema_text)
  getExportedValue("S7", "prop<-")(
    type, "required", value = isTRUE(required)
  )
}

ellmer_output_type <- function(cfg, schema) {
  dialect <- llm_provider_spec(cfg$provider)$output_schema_dialect
  ellmer_type_from_schema(
    schema,
    strict_objects = identical(dialect, "strict_all_properties")
  )
}

ellmer_tool_function <- function(tool) {
  # `names(list())` is NULL, and mget(NULL) aborts with "invalid first
  # argument", so an argument-less tool must normalise to character(0).
  argument_names <- names(tool$schema$properties %||% list()) %||% character()
  required <- as.character(unlist(tool$schema$required %||% character()))
  fn <- function() NULL
  formals(fn) <- as.pairlist(stats::setNames(
    lapply(argument_names, function(argument) {
      if (argument %in% required) quote(expr = ) else quote(NULL)
    }),
    argument_names
  ))
  environment(fn) <- list2env(
    list(.tool = tool, .argument_names = argument_names),
    parent = environment()
  )
  body(fn) <- quote({
    args <- mget(.argument_names, envir = environment(), inherits = FALSE)
    args <- args[!vapply(args, is.null, logical(1))]
    .tool$call(args)
  })
  fn
}

ellmer_tool_contract <- function(tool) {
  properties <- tool$schema$properties %||% list()
  required <- as.character(unlist(tool$schema$required %||% character()))
  arguments <- lapply(names(properties), function(name) {
    ellmer_type_from_schema(properties[[name]], required = name %in% required)
  })
  names(arguments) <- names(properties)
  do.call(getExportedValue("ellmer", "tool"), list(
    fun = ellmer_tool_function(tool), name = tool$name,
    description = tool$description %||% tool$name,
    arguments = arguments
  ))
}

ellmer_content <- function(export, ...) {
  do.call(getExportedValue("ellmer", export), list(...))
}

ellmer_tool_request <- function(call) {
  ellmer_content(
    "ContentToolRequest", id = call$id %||% "",
    name = call$name %||% "", arguments = call$arguments %||% list()
  )
}

ellmer_turn_from_message <- function(message, tool_request = NULL) {
  role <- message$role
  if (identical(role, "system")) {
    return(ellmer_content(
      "SystemTurn",
      contents = list(ellmer_content("ContentText", text = message$content %||% ""))
    ))
  }
  if (identical(role, "user")) {
    return(ellmer_content(
      "UserTurn",
      contents = list(ellmer_content("ContentText", text = message$content %||% ""))
    ))
  }
  if (identical(role, "assistant")) {
    content <- if (!is.null(message$tool_call)) {
      tool_request %||% ellmer_tool_request(message$tool_call)
    } else {
      ellmer_content("ContentText", text = message$content %||% "")
    }
    return(ellmer_content(
      "AssistantTurn", contents = list(content),
      finish_reason = if (is.null(message$tool_call)) "stop" else "tool"
    ))
  }
  if (identical(role, "tool")) {
    value <- strict_json_list(message$content) %||% message$content %||% ""
    if (is.null(tool_request)) {
      tool_request <- ellmer_tool_request(list(
        id = message$tool_call_id, name = message$name,
        arguments = message$arguments %||% list()
      ))
    }
    return(ellmer_content(
      "UserTurn",
      contents = list(ellmer_content(
        "ContentToolResult", request = tool_request, value = value
      ))
    ))
  }
  cli::cli_abort("unsupported ellmer message role {.val {role}}",
                 class = "sas2r_llm_request_error")
}

ellmer_turns_from_messages <- function(messages) {
  requests <- new.env(parent = emptyenv())
  lapply(messages, function(message) {
    request <- NULL
    if (identical(message$role, "assistant") && !is.null(message$tool_call)) {
      request <- ellmer_tool_request(message$tool_call)
      id <- message$tool_call$id %||% ""
      if (nzchar(id)) assign(id, request, envir = requests)
    } else if (identical(message$role, "tool")) {
      id <- message$tool_call_id %||% ""
      if (nzchar(id) && exists(id, envir = requests, inherits = FALSE)) {
        request <- get(id, envir = requests, inherits = FALSE)
      }
    }
    ellmer_turn_from_message(message, tool_request = request)
  })
}

ellmer_prepare_conversation <- function(chat, messages) {
  current <- messages[[length(messages)]]
  if (!identical(current$role, "user")) {
    cli::cli_abort(
      "ellmer requests must end with a current user message",
      class = "sas2r_llm_request_error"
    )
  }
  history <- messages[-length(messages)]
  if (length(history)) {
    if (is.null(chat$set_turns) || !is.function(chat$set_turns)) {
      cli::cli_abort("ellmer chat does not expose public turn history",
                     class = "sas2r_llm_transport_error")
    }
    chat$set_turns(ellmer_turns_from_messages(history))
  }
  as.character(current$content %||% "")
}

ellmer_is_public <- function(object, export) {
  tryCatch(
    getExportedValue("S7", "S7_inherits")(
      object, getExportedValue("ellmer", export)
    ),
    error = function(error) FALSE
  )
}

ellmer_public_prop <- function(object, name) {
  getExportedValue("S7", "prop")(object, name)
}

ellmer_conversation_messages <- function(chat) {
  if (is.null(chat$get_turns) || !is.function(chat$get_turns)) return(NULL)
  turns <- tryCatch(
    chat$get_turns(include_system_prompt = TRUE),
    error = function(error) NULL
  )
  if (!is.list(turns) || !length(turns)) return(NULL)
  messages <- list()
  append_message <- function(message) {
    messages[[length(messages) + 1L]] <<- message
  }
  for (turn in turns) {
    contents <- tryCatch(
      ellmer_public_prop(turn, "contents"), error = function(error) list()
    )
    if (ellmer_is_public(turn, "SystemTurn")) {
      append_message(list(
        role = "system", content = getExportedValue("ellmer", "contents_text")(turn)
      ))
    } else if (ellmer_is_public(turn, "AssistantTurn")) {
      requests <- Filter(
        function(content) ellmer_is_public(content, "ContentToolRequest"),
        contents
      )
      if (length(requests)) {
        for (request in requests) {
          append_message(list(
            role = "assistant", content = "",
            tool_call = list(
              id = ellmer_public_prop(request, "id"),
              name = ellmer_public_prop(request, "name"),
              arguments = ellmer_public_prop(request, "arguments")
            )
          ))
        }
      } else {
        append_message(list(
          role = "assistant",
          content = getExportedValue("ellmer", "contents_text")(turn)
        ))
      }
    } else if (ellmer_is_public(turn, "UserTurn")) {
      results <- Filter(
        function(content) ellmer_is_public(content, "ContentToolResult"),
        contents
      )
      if (length(results)) {
        for (result in results) {
          request <- ellmer_public_prop(result, "request")
          append_message(list(
            role = "tool",
            name = ellmer_public_prop(request, "name"),
            tool_call_id = ellmer_public_prop(request, "id"),
            content = jsonlite::toJSON(
              ellmer_public_prop(result, "value"),
              auto_unbox = TRUE, null = "null", force = TRUE
            )
          ))
        }
      } else {
        append_message(list(
          role = "user", content = getExportedValue("ellmer", "contents_text")(turn)
        ))
      }
    }
  }
  interleave_tool_messages(messages)
}

#' Pair each assistant tool call with the tool message answering it
#'
#' ellmer groups an assistant turn's parallel tool requests together and their
#' results in the following turn, which flattens to
#' `assistant(c1), assistant(c2), tool(c1), tool(c2)`. Every OpenAI-compatible
#' API rejects that shape: an assistant message carrying tool calls must be
#' followed by the tool messages answering it. Reordering to
#' `assistant(c1), tool(c1), assistant(c2), tool(c2)` preserves both the
#' single-tool-call message format and the order the model chose.
#' @noRd
interleave_tool_messages <- function(messages) {
  if (!is.list(messages) || length(messages) < 2L) return(messages)
  ids <- vapply(messages, function(message) {
    if (identical(message$role, "tool")) message$tool_call_id %||% "" else ""
  }, character(1))
  if (!any(nzchar(ids))) return(messages)

  consumed <- rep(FALSE, length(messages))
  out <- vector("list", 0L)
  for (i in seq_along(messages)) {
    if (consumed[[i]]) next
    message <- messages[[i]]
    out[[length(out) + 1L]] <- message
    call_id <- if (identical(message$role, "assistant")) {
      message$tool_call$id %||% ""
    } else ""
    if (!nzchar(call_id)) next
    match <- which(ids == call_id & !consumed & seq_along(messages) != i)
    if (!length(match)) next
    consumed[[match[[1]]]] <- TRUE
    out[[length(out) + 1L]] <- messages[[match[[1]]]]
  }
  out
}

compact_non_null <- function(values) {
  values[!vapply(values, is.null, logical(1))]
}

ellmer_constructor_args <- function(cfg, model, params = list()) {
  spec <- llm_provider_spec(cfg$provider)
  common <- list(model = model)
  if (length(params)) {
    wire <- params
    if (!is.null(wire$max_output_tokens)) {
      wire$max_tokens <- wire$max_output_tokens
      wire$max_output_tokens <- NULL
    }
    common$params <- do.call(getExportedValue("ellmer", "params"), wire)
  }
  c(common, compact_non_null(spec$build_chat_args(cfg)))
}

ellmer_endpoint_identity <- function(cfg) {
  llm_provider_spec(cfg$provider)$endpoint_identity(cfg)
}

ellmer_api_version_identity <- function(cfg) {
  llm_provider_spec(cfg$provider)$api_version_identity(cfg)
}

ellmer_token_totals <- function(tokens) {
  if (is.null(tokens)) {
    return(normalized_usage(list()))
  }
  sum_field <- function(candidates) {
    for (name in candidates) {
      value <- if (is.data.frame(tokens) || is.list(tokens)) {
        tokens[[name]]
      } else if (is.numeric(tokens) && !is.null(names(tokens))) {
        tokens[name]
      } else NULL
      if (!is.null(value)) {
        value <- suppressWarnings(as.numeric(value))
        value <- value[is.finite(value)]
        if (length(value)) return(sum(value))
      }
    }
    NA_real_
  }
  normalized_usage(list(usage = list(
    input_tokens = sum_field(c("input_tokens", "prompt_tokens", "input")),
    output_tokens = sum_field(c("output_tokens", "completion_tokens", "output")),
    cached_input_tokens = sum_field(c(
      "cached_input_tokens", "cached_input", "cached_tokens"
    )),
    cache_write_tokens = sum_field(c(
      "cache_write_tokens", "cache_creation_input_tokens"
    )),
    reasoning_tokens = sum_field(c("reasoning_tokens"))
  )))
}

# The public sources the adapter can read a number from. The durable ledger
# keeps one `raw_usage_provenance` string per call, so it has to name the API
# the number actually came from instead of assuming the conversation
# tabulation always won.
ELLMER_TABULATED_TOKEN_PROVENANCE <- "ellmer public get_tokens()"
ELLMER_TURN_TOKEN_PROVENANCE <- "ellmer public Turn@tokens"
ELLMER_TURN_JSON_USAGE_PROVENANCE <-
  "ellmer public AssistantTurn@json usage"
ELLMER_COST_PROVENANCE <- "ellmer public get_cost(include = 'last')"

# One public per-turn token record. ellmer's default `AssistantTurn@tokens` is
# an unnamed `c(NA, NA, NA)`, while a provider-populated turn carries
# `c(input =, output =, cached_input =)`. Without the name guard an unpopulated
# default would be read positionally.
ellmer_turn_token_record <- function(turn) {
  tokens <- tryCatch(
    getExportedValue("S7", "prop")(turn, "tokens"),
    error = function(error) NULL
  )
  if (!is.numeric(tokens) || is.null(names(tokens))) return(NULL)
  tokens
}

# The same turns ellmer's own `get_tokens()` tabulates: every complete
# (non-partial) assistant turn of the conversation. A user turn carries a tool
# result and never a token record.
ellmer_counted_turns <- function(chat) {
  turns <- if (is.null(chat$get_turns) || !is.function(chat$get_turns)) {
    NULL
  } else {
    tryCatch(chat$get_turns(), error = function(error) NULL)
  }
  if (!is.list(turns) || !length(turns)) {
    # An object without the public `get_turns()` still exposes the last turn.
    turn <- if (is.null(chat$last_turn) || !is.function(chat$last_turn)) {
      NULL
    } else {
      tryCatch(chat$last_turn(), error = function(error) NULL)
    }
    return(if (is.null(turn)) list() else list(turn))
  }
  Filter(function(turn) {
    role <- tryCatch(
      getExportedValue("S7", "prop")(turn, "role"),
      error = function(error) NULL
    )
    identical(role, "assistant") &&
      !any(grepl("AssistantPartialTurn", class(turn), fixed = TRUE))
  }, turns)
}

# ellmer's public `get_tokens()` tabulates the whole conversation and errors
# whenever the user and assistant turn counts differ -- a replayed history that
# ends on a user or tool turn, for instance. Without a fallback, usage and cost
# metering silently degrades to unknown for the package's main path. The
# fallback has to sum the same turns `get_tokens()` would have summed: a single
# request can append both a tool-request turn and a final answer turn, so
# reading only `last_turn()` would drop the tool-request turn's tokens and
# under-report the request.
ellmer_turn_tokens <- function(chat) {
  totals <- list()
  for (turn in ellmer_counted_turns(chat)) {
    tokens <- ellmer_turn_token_record(turn)
    if (is.null(tokens)) next
    token_names <- names(tokens)
    for (index in seq_along(tokens)) {
      name <- token_names[[index]]
      if (is.na(name) || !nzchar(name)) next
      totals[[name]] <- c(totals[[name]], unname(tokens[[index]]))
    }
  }
  if (!length(totals)) NULL else totals
}

ellmer_turn_json_usage_record <- function(turn) {
  json <- tryCatch(
    getExportedValue("S7", "prop")(turn, "json"),
    error = function(error) NULL
  )
  if (!is.list(json) || !length(json)) return(NULL)
  usage <- normalized_usage(json)
  measured <- c(
    usage$input_tokens, usage$output_tokens, usage$total_input_tokens,
    usage$total_output_tokens, usage$total_tokens, usage$cached_input_tokens,
    usage$cache_write_tokens, usage$reasoning_tokens
  )
  if (all(is.na(measured))) NULL else usage
}

ellmer_turn_json_usage <- function(chat) {
  records <- Filter(
    Negate(is.null),
    lapply(ellmer_counted_turns(chat), ellmer_turn_json_usage_record)
  )
  if (!length(records)) return(NULL)
  fields <- names(normalized_usage(list()))
  usage <- lapply(fields, function(field) {
    values <- vapply(records, function(record) {
      nonnegative_number_or_na(record[[field]])
    }, numeric(1))
    if (all(is.na(values)) || anyNA(values)) NA_real_ else sum(values)
  })
  names(usage) <- fields
  aggregate_accounting <- function(status_attribute, delta_attribute) {
    statuses <- vapply(records, function(record) {
      attr(record, status_attribute, exact = TRUE) %||% "unavailable"
    }, character(1))
    status <- if (any(statuses == "mismatch")) {
      "mismatch"
    } else if (all(statuses == "consistent")) {
      "consistent"
    } else {
      "unavailable"
    }
    deltas <- vapply(records, function(record) {
      scalar_number_or_na(attr(record, delta_attribute, exact = TRUE))
    }, numeric(1))
    list(
      status = status,
      delta = if (anyNA(deltas)) NA_real_ else sum(deltas)
    )
  }
  input_accounting <- aggregate_accounting(
    "input_accounting_status", "input_accounting_delta_tokens"
  )
  total_accounting <- aggregate_accounting(
    "total_accounting_status", "total_accounting_delta_tokens"
  )
  attr(usage, "input_accounting_status") <- input_accounting$status
  attr(usage, "input_accounting_delta_tokens") <- input_accounting$delta
  attr(usage, "total_accounting_status") <- total_accounting$status
  attr(usage, "total_accounting_delta_tokens") <- total_accounting$delta
  usage
}

ellmer_usage_has_tokens <- function(usage) {
  fields <- c(
    "input_tokens", "output_tokens", "total_input_tokens",
    "total_output_tokens", "total_tokens", "cached_input_tokens",
    "cache_write_tokens", "reasoning_tokens"
  )
  any(vapply(fields, function(field) {
    !is.na(nonnegative_number_or_na(usage[[field]]))
  }, logical(1)))
}

ellmer_usage <- function(chat) {
  usage <- ellmer_turn_json_usage(chat)
  if (!is.null(usage)) {
    attr(usage, "token_provenance") <- ELLMER_TURN_JSON_USAGE_PROVENANCE
    return(usage)
  }
  tokens <- if (is.null(chat$get_tokens) || !is.function(chat$get_tokens)) {
    NULL
  } else {
    tryCatch(chat$get_tokens(), error = function(error) NULL)
  }
  usage <- ellmer_token_totals(tokens)
  provenance <- ELLMER_TABULATED_TOKEN_PROVENANCE
  if (!ellmer_usage_has_tokens(usage)) {
    usage <- ellmer_token_totals(ellmer_turn_tokens(chat))
    provenance <- ELLMER_TURN_TOKEN_PROVENANCE
  }
  if (!ellmer_usage_has_tokens(usage)) provenance <- NULL
  attr(usage, "token_provenance") <- provenance
  usage
}

# Tokens and cost are read from two different public APIs and either can be
# unknown on its own, so the recorded provenance names exactly the ones that
# produced a number. Cost is never derived from tokens.
ellmer_usage_provenance <- function(usage, cost) {
  parts <- c(
    attr(usage, "token_provenance", exact = TRUE),
    if (is.na(cost)) NULL else ELLMER_COST_PROVENANCE
  )
  if (!length(parts)) NULL else paste(parts, collapse = " / ")
}

ellmer_cost <- function(chat) {
  if (is.null(chat$get_cost) || !is.function(chat$get_cost)) return(NA_real_)
  value <- tryCatch(
    suppressWarnings(as.numeric(chat$get_cost(include = "last"))),
    error = function(error) NA_real_
  )
  if (length(value) != 1L || is.na(value) || !is.finite(value)) NA_real_ else value
}

ellmer_finish_reason <- function(chat) {
  if (is.null(chat$last_turn) || !is.function(chat$last_turn)) return(NULL)
  turn <- tryCatch(chat$last_turn(), error = function(error) NULL)
  if (is.null(turn)) return(NULL)
  value <- tryCatch(
    getExportedValue("S7", "prop")(turn, "finish_reason"),
    error = function(error) NULL
  )
  if (is.character(value) && length(value) == 1L) value else NULL
}

ellmer_last_text <- function(chat, fallback = NULL) {
  if (is.null(chat$last_turn) || !is.function(chat$last_turn)) return(fallback)
  turn <- tryCatch(chat$last_turn(), error = function(error) NULL)
  if (is.null(turn)) return(fallback)
  value <- tryCatch(
    getExportedValue("ellmer", "contents_text")(turn),
    error = function(error) NULL
  )
  if (is.character(value) && length(value) == 1L) value else fallback
}

#' Evaluate `expr` with ellmer's per-request limits scoped to the call
#'
#' sas2r scopes ellmer to a 300-second absolute deadline for each HTTP transfer
#' attempt and one total transport attempt by default. Explicit retry counts can
#' multiply wall time and spend beneath the budget ledger, which only counts
#' sas2r-level retries. Options are the only levers ellmer exposes, so scope
#' them to the request rather than mutating globally.
#' @noRd
with_ellmer_limits <- function(timeout_seconds, max_tries, expr) {
  previous <- options(
    ellmer_timeout_s = timeout_seconds,
    ellmer_max_tries = max_tries
  )
  on.exit(options(previous), add = TRUE)
  force(expr)
}

ELLMER_TRANSPORT_CONSTRAINTS <- list(
  tools_with_structured_output = "unsupported"
)

ellmer_transport_request <- function(cfg, request, model, params) {
  if (length(request$tools) && !is.null(request$output_schema)) {
    cli::cli_abort(
      "ellmer transport cannot combine registered tools with structured output",
      class = "sas2r_llm_capability_error"
    )
  }
  constructor <- getExportedValue("ellmer", ellmer_constructor(cfg$provider))
  if (length(params) &&
      !exists("params", asNamespace("ellmer"), mode = "function", inherits = FALSE)) {
    cli::cli_abort(
      "ellmer::params is required for supported optional model parameters",
      class = "sas2r_llm_transport_error"
    )
  }
  constructor_args <- ellmer_constructor_args(cfg, model, params)
  chat <- do.call(constructor, constructor_args)
  prompt <- ellmer_prepare_conversation(chat, request$messages)
  if (length(request$tools)) {
    if (is.null(chat$register_tool) || !is.function(chat$register_tool)) {
      cli::cli_abort("ellmer chat does not expose native tool registration",
                     class = "sas2r_llm_transport_error")
    }
    for (tool in request$tools) chat$register_tool(ellmer_tool_contract(tool))
  }

  raw <- if (identical(request$schema_mode, "native") &&
             !is.null(request$output_schema)) {
    if (is.null(chat$chat_structured) || !is.function(chat$chat_structured)) {
      cli::cli_abort("ellmer chat does not expose native structured output",
                     class = "sas2r_llm_invalid_schema")
    }
    list(
      type = "final",
      data = chat$chat_structured(
        prompt, type = ellmer_output_type(cfg, request$output_schema)
      )
    )
  } else {
    value <- chat$chat(prompt)
    value <- ellmer_last_text(chat, fallback = value)
    if (identical(request$schema_mode, "fallback")) {
      parsed <- strict_json_list(value, strip_markdown_fences = TRUE)
      if (is.null(parsed)) {
        list(type = "final", data = list(raw = value, parse_error = TRUE))
      } else if (!is.null(parsed$type) && !is.null(parsed$data)) {
        parsed
      } else {
        list(type = "final", data = parsed)
      }
    } else if (identical(request$phase, "gathering")) {
      list(
        type = "final", data = list(gathered = value),
        conversation = ellmer_conversation_messages(chat)
      )
    } else value
  }
  usage <- ellmer_usage(chat)
  attr(raw, "usage") <- usage
  cost <- ellmer_cost(chat)
  if (!is.na(cost)) {
    attr(raw, "cost_usd") <- cost
    attr(raw, "cost_status") <- "catalog_estimate"
    attr(raw, "cost_currency") <- "USD"
    attr(raw, "cost_source") <- "ellmer / LiteLLM catalog"
    attr(raw, "cost_source_version") <- as.character(
      utils::packageVersion("ellmer")
    )
  }
  provenance <- ellmer_usage_provenance(usage, cost)
  if (!is.null(provenance)) attr(raw, "cost_provenance") <- provenance
  if (is.null(raw$finish_reason)) raw$finish_reason <- ellmer_finish_reason(chat)
  raw
}

#' Construct an LLM adapter backed by ellmer public APIs
#' @noRd
ellmer_llm <- function(cfg) {
  if (!isTRUE(cfg$provider %in% llm_provider_ids())) {
    cli::cli_abort("unknown LLM provider {.val {cfg$provider}}",
                   class = "sas2r_llm_error")
  }
  cfg <- normalize_llm_config(cfg)
  configured_models <- c(cfg$model, unlist(cfg$tiers, use.names = FALSE))
  configured_models <- configured_models[
    !is.na(configured_models) & nzchar(configured_models)
  ]
  if (!length(configured_models)) {
    cli::cli_abort("an explicit LLM model or tier mapping is required",
                   class = "sas2r_llm_model_required")
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    cli::cli_abort(
      "agents need the {.pkg ellmer} package; deterministic mode works without it",
      class = "sas2r_llm_unavailable"
    )
  }
  base_model <- configured_models[[1]]
  endpoint_identity <- ellmer_endpoint_identity(cfg)
  api_version_identity <- ellmer_api_version_identity(cfg)
  base_capabilities <- resolve_model_capabilities(
    cfg$provider, endpoint_identity, base_model,
    api_version_identity, overrides = cfg$capabilities
  )
  capabilities_for <- function(tier = "frontier", model = NULL) {
    resolved_model <- selected_llm_model(
      cfg, list(tier = tier, model = model)
    )
    resolve_model_capabilities(
      cfg$provider, endpoint_identity, resolved_model,
      api_version_identity, overrides = cfg$capabilities
    )
  }
  adapter <- new_llm(function(request) {
    if (!inherits(request, "sas2r_llm_request")) {
      cli::cli_abort("ellmer adapter requires a sas2r_llm_request",
                     class = "sas2r_llm_request_error")
    }
    model <- selected_llm_model(cfg, request)
    capabilities <- llm_capabilities_for(
      adapter, tier = request$tier, model = request$model
    )
    response <- invoke_with_capability_retry(
      request, capabilities,
      function(request, params) with_ellmer_limits(
        cfg$timeout_seconds, cfg$max_tries,
        ellmer_transport_request(cfg, request, model, params)
      )
    )
    response$provider <- cfg$provider
    response$requested_model <- request$model %||% model
    response$resolved_model <- response$resolved_model %||% model
    response$schema_mode <- request$schema_mode
    response$schema_version <- request$schema_version
    response$capability_hash <- response$capability_hash %||%
      capabilities$record_hash
    response
  }, provider = cfg$provider, capabilities = base_capabilities,
  model = base_model, endpoint = endpoint_identity,
  api_version = api_version_identity, capabilities_for = capabilities_for,
  transport_constraints = ELLMER_TRANSPORT_CONSTRAINTS,
  redaction_secrets = llm_config_secret_values(cfg),
  # Optional model parameters configured at project level. They still pass the
  # capability gate before reaching the wire; carrying them here only stops
  # them being discarded before the runner can offer them.
  model_parameters = compact_non_null(
    cfg[intersect(LLM_OPTIONAL_PARAMETERS, names(cfg))]
  ), request_policy = list(
    timeout_scope = "http_attempt_absolute",
    timeout_seconds = cfg$timeout_seconds,
    transport_max_tries = cfg$max_tries
  ))
  attr(adapter, "is_ellmer") <- TRUE
  adapter <- with_usage_managed_request(adapter)
  attr(adapter, "auth_context") <- llm_selector_identity(cfg)
  adapter
}

#' Construct an LLM adapter from a project's `llm:` configuration
#'
#' The supported way to obtain the `llm` argument that [sas_translate()] accepts.
#' The adapter is backed by public `ellmer` APIs for one of the twelve registered provider
#' ids; credentials stay with `ellmer` and are never read or stored by sas2r.
#' Constructing an adapter contacts no network -- use [sas_llm_probe()] to
#' validate connectivity and model capabilities, and [sas_llm_models()] to see
#' the inventory visible to the configured identity.
#'
#' @param config An `llm:` mapping, a `sas2r_config` from [sas_config()], or an
#'   already-normalized LLM list.
#' @return A `sas2r_llm` adapter.
#' @seealso [sas_llm_probe()], [sas_llm_models()]
#' @export
sas_llm <- function(config) {
  config <- normalize_llm_config(config)
  if (is.null(config)) {
    llm_config_abort("LLM configuration is required to construct an adapter")
  }
  ellmer_llm(config)
}

probe_failure_condition <- function(error, auth_context = NULL,
                                    classify_access = FALSE,
                                    local_stage = NULL) {
  if (!inherits(error, "condition")) {
    error <- simpleError(as.character(error)[[1]])
  }
  classes <- class(error)
  semantic_reason <- error$reason %||% NULL
  access_reclassifiable <- inherits(error, c(
    "sas2r_llm_authentication_error", "sas2r_llm_permission_denied",
    "sas2r_llm_transport_error"
  )) && (is.null(recognized_llm_failure_reason(error)) ||
         isTRUE(attr(error, "sas2r_private_reason_synthesized")))
  generic <- !any(startsWith(classes, "sas2r_"))
  if (isTRUE(classify_access) && is.list(auth_context) &&
      !is.null(auth_context$provider) &&
      (generic || access_reclassifiable)) {
    return(copy_llm_failure_metrics(
      error, classify_llm_access_error(error, auth_context)
    ))
  }
  if (generic) {
    if (!is.null(local_stage)) {
      normalized <- structure(
        list(
          message = conditionMessage(error), call = NULL,
          reason = paste0(local_stage, "_failed"), stage = local_stage
        ),
        class = unique(c(
          "sas2r_llm_probe_local_error", class(error), "error", "condition"
        ))
      )
      return(copy_llm_failure_metrics(error, normalized))
    }
    failure <- failure_class(error)
    normalized <- structure(
      list(
        message = conditionMessage(error), call = NULL,
        reason = llm_failure_reason(structure(list(), class = failure))
      ),
      class = c(failure, "error", "condition")
    )
    return(copy_llm_failure_metrics(error, normalized))
  }
  if (is.null(semantic_reason)) error$reason <- llm_failure_reason(error)
  attr(error, "sas2r_private_reason_synthesized") <- NULL
  error
}

llm_condition_from_failure_response <- function(response, auth_context = NULL) {
  reason_synthesized <- isTRUE(attr(
    response, "sas2r_private_reason_synthesized"
  ))
  response_classes <- class(response)[startsWith(class(response), "sas2r_")]
  failure <- response$error$class %||%
    if (length(response_classes)) response_classes[[1]] else
      "sas2r_llm_transport_error"
  candidate <- response$error
  candidate$message <- response$error$message %||% "provider request failed"
  candidate$call <- NULL
  class(candidate) <- unique(c(failure, "error", "condition"))
  metadata <- normalized_llm_failure_metadata(candidate, failure)
  diagnostics <- metadata[
    intersect(LLM_FAILURE_DIAGNOSTIC_FIELDS, names(metadata))
  ]
  error <- structure(
    c(list(
      message = metadata$message, call = NULL, reason = metadata$reason
    ), diagnostics),
    class = unique(c(failure, "error", "condition"))
  )
  if (reason_synthesized) {
    attr(error, "sas2r_private_reason_synthesized") <- TRUE
  }
  attr(error, "usage") <- response$usage %||% attr(response, "usage", exact = TRUE)
  cost <- extract_response_cost(response)
  attr(error, "cost_usd") <- scalar_number_or_na(cost)
  cost_attributes <- c(
    cost_status = "status", cost_currency = "currency",
    cost_source = "source", cost_source_version = "source_version",
    cost_effective_date = "effective_date",
    cost_rate_dimensions = "rate_dimensions", cost_provenance = "provenance"
  )
  for (attribute in names(cost_attributes)) {
    value <- response$cost[[cost_attributes[[attribute]]]]
    if (!is.null(value)) attr(error, attribute) <- value
  }
  error <- probe_failure_condition(error, auth_context, classify_access = TRUE)
  attr(error, "sas2r_private_reason_synthesized") <- NULL
  error
}

llm_failure_metadata <- function(error) {
  list(
    class = class(error)[[1]],
    reason = llm_failure_reason(error),
    message = conditionMessage(error)
  )
}

probe_terminal_audit <- function(error, log_dir, redactor, tier, attempt,
                                 provider = NULL, requested_model = NULL,
                                 resolved_model = NULL,
                                 capability_hash = NULL,
                                 cumulative_spend = 0,
                                 fallback_usage = NULL,
                                 fallback_cost = NA_real_) {
  if (!is.null(requested_model)) error$requested_model <- requested_model
  if (!is.null(resolved_model)) error$resolved_model <- resolved_model
  error <- sanitize_llm_public(error, redactor)
  terminal_class <- class(error)[[1]]
  terminal_reason <- llm_failure_reason(error)
  usage <- closed_llm_failure_usage(
    attr(error, "usage", exact = TRUE) %||% fallback_usage %||% list()
  )
  input_tokens <- scalar_number_or_na(usage$input_tokens)
  output_tokens <- scalar_number_or_na(usage$output_tokens)
  cost <- scalar_number_or_na(attr(error, "cost_usd", exact = TRUE))
  if (is.na(cost)) cost <- scalar_number_or_na(fallback_cost)
  cost_record <- normalized_cost(error)
  if (is.na(cost_record$amount_usd) && !is.na(cost)) {
    cost_record$amount_usd <- cost
    cost_record$status <- "catalog_estimate"
  }
  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    agent = "probe", type = "terminal_error", status = "failed",
    terminal_class = terminal_class, terminal_reason = terminal_reason,
    provider = provider, requested_model = requested_model,
    resolved_model = resolved_model, tier = tier,
    attempt = attempt,
    input_tokens = input_tokens, output_tokens = output_tokens,
    total_input_tokens = scalar_number_or_na(usage$total_input_tokens),
    total_output_tokens = scalar_number_or_na(usage$total_output_tokens),
    total_tokens = scalar_number_or_na(usage$total_tokens),
    cached_input_tokens = scalar_number_or_na(usage$cached_input_tokens),
    cache_write_tokens = scalar_number_or_na(usage$cache_write_tokens),
    reasoning_tokens = scalar_number_or_na(usage$reasoning_tokens),
    cost_usd = cost,
    cost_status = cost_record$status,
    cumulative_spend_usd = cumulative_spend,
    error = llm_failure_metadata(error)
  )
  if (!is.null(capability_hash)) {
    entry <- append(
      entry, list(capability_hash = capability_hash),
      after = match("tier", names(entry))
    )
  }
  tryCatch(
    llm_log(entry, dir = log_dir, redactor = redactor),
    error = function(audit_error) NULL
  )
  error
}

probe_attempt_failure_audit <- function(error, log_dir, redactor, tier, attempt,
                                        provider = NULL, requested_model = NULL,
                                        resolved_model = NULL,
                                        capability_hash = NULL,
                                        cumulative_spend = 0) {
  error <- sanitize_llm_public(error, redactor)
  usage <- closed_llm_failure_usage(
    attr(error, "usage", exact = TRUE) %||% list()
  )
  cost <- scalar_number_or_na(attr(error, "cost_usd", exact = TRUE))
  cost_record <- normalized_cost(error)
  llm_log(list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    agent = "probe", type = "error", status = "failed",
    provider = provider, requested_model = requested_model,
    resolved_model = resolved_model, tier = tier,
    capability_hash = capability_hash,
    attempt = attempt,
    input_tokens = scalar_number_or_na(usage$input_tokens),
    output_tokens = scalar_number_or_na(usage$output_tokens),
    total_input_tokens = scalar_number_or_na(usage$total_input_tokens),
    total_output_tokens = scalar_number_or_na(usage$total_output_tokens),
    total_tokens = scalar_number_or_na(usage$total_tokens),
    cached_input_tokens = scalar_number_or_na(usage$cached_input_tokens),
    cache_write_tokens = scalar_number_or_na(usage$cache_write_tokens),
    reasoning_tokens = scalar_number_or_na(usage$reasoning_tokens),
    cost_usd = cost,
    cost_status = cost_record$status,
    cumulative_spend_usd = cumulative_spend,
    error = llm_failure_metadata(error)
  ), dir = log_dir, redactor = redactor)
  invisible(error)
}

probe_tier_model <- function(llm, tier, auth_context = NULL) {
  if (is.list(auth_context) && length(configured_llm_models(auth_context))) {
    return(selected_llm_model(
      auth_context, list(tier = tier, model = NULL)
    ))
  }
  llm$model %||% NULL
}

sas_llm_probe_impl <- function(llm, max_retries, log_dir, on_charge, tier,
                               can_attempt, usage_budget, audit_context, state) {
  if (!inherits(llm, "sas2r_llm") && is.list(llm)) {
    state$stage <- "configuration"
    config <- normalize_llm_config(llm)
    state$redactor <- new_llm_audit_redactor(llm_config_secret_values(config))
    state$provider <- config$provider
    selected_model <- selected_llm_model(config, list(tier = tier, model = NULL))
    state$requested_model <- selected_model
    state$resolved_model <- selected_model
    state$auth_context <- llm_selector_identity(config)
    inventory_config <- config
    inventory_config$model <- selected_model
    state$stage <- "model_inventory"
    state$classify_access <- TRUE
    inventory <- sas_llm_models(inventory_config)
    state$classify_access <- FALSE
    if (identical(inventory$status, "available")) {
      inventory_ids <- llm_inventory_model_ids(inventory$models)
      if (is.null(inventory_ids)) {
        stop(new_llm_access_condition(
          "sas2r_llm_inventory_shape_error",
          "The provider returned an unsupported model inventory structure.",
          "inventory_shape", inventory_config
        ))
      }
      if (!selected_model %in% inventory_ids) {
        stop(new_llm_access_condition(
          "sas2r_llm_model_not_found",
          paste0("The configured model `", selected_model,
                 "` is not visible to the selected identity."),
          "model_not_found", inventory_config
        ))
      }
    }
    state$stage <- "adapter_setup"
    llm <- ellmer_llm(config)
  }
  state$stage <- "audit_redactor"
  state$redactor <- llm_audit_redactor(llm)
  state$provider <- llm$provider %||% state$provider
  state$auth_context <- attr(llm, "auth_context", exact = TRUE) %||%
    state$auth_context
  selected_model <- probe_tier_model(llm, tier, state$auth_context)
  state$requested_model <- state$requested_model %||% selected_model
  state$resolved_model <- state$resolved_model %||% selected_model
  cumulative_spend <- 0
  state$stage <- "capabilities"
  capabilities <- llm_capabilities_for(llm, tier = tier)
  state$capability_hash <- capabilities$record_hash
  schema_mode <- capabilities$structured_output
  if (!schema_mode %in% c("native", "fallback")) return(FALSE)
  last_failure <- NULL
  for (i in seq_len(max_retries)) {
    state$attempt <- i
    state$cumulative_spend <- cumulative_spend
    state$usage <- NULL
    state$cost <- NA_real_
    state$stage <- "can_attempt"
    if (is.function(can_attempt) && !isTRUE(can_attempt())) {
      state$stage <- "audit_write"
      llm_log(list(
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
        agent = "probe", type = "budget_exhausted", tier = tier, attempt = i,
        capability_hash = state$capability_hash,
        input_tokens = NA_real_, output_tokens = NA_real_, cost_usd = NA_real_,
        cost_status = "unknown", cumulative_spend_usd = cumulative_spend
      ), dir = log_dir, redactor = state$redactor)
      return(FALSE)
    }
    state$stage <- "request_build"
    messages <- list(list(role = "user", content = "Return a JSON object with ok=true, nothing else."))
    # The configured ceiling lives on the adapter's model_parameters (a raw
    # config carries it at the top level). Reading only llm$max_output_tokens
    # dropped a configured 16384 to the 32-token floor, and a reasoning model
    # then spent the whole ping thinking: the truncated response read as
    # offline even though authentication had succeeded.
    configured_ceiling <- llm$max_output_tokens %||%
      llm$model_parameters$max_output_tokens
    probe_reasoning <- llm$reasoning_effort %||%
      llm$model_parameters$reasoning_effort
    probe_max_tokens <- if (!is.null(configured_ceiling) &&
                            is.numeric(configured_ceiling)) {
      max(as.integer(configured_ceiling), 32L)
    } else if (!is.null(probe_reasoning)) {
      # Extended thinking needs headroom before any answer text emerges.
      2048L
    } else {
      32L
    }
    request <- llm_request(
      messages = messages, tier = tier,
      output_schema = list(
        type = "object", properties = list(ok = list(type = "boolean")),
        required = "ok", additionalProperties = FALSE
      ),
      schema_name = "probe", schema_version = "1", schema_mode = schema_mode,
      phase = "probe", max_output_tokens = probe_max_tokens
    )
    request$parent_request_id <- state$last_request_id
    request$retry_of <- if (i > 1L) state$last_request_id else NULL
    state$stage <- "provider_request"
    response <- attempt_llm_request(
      request, llm, usage_budget = usage_budget,
      audit_context = utils::modifyList(audit_context, list(
        provider = state$provider,
        requested_model = state$requested_model,
        resolved_model = state$resolved_model,
        agent = "probe", purpose = "probe", tier = tier,
        capability_hash = state$capability_hash,
        parent_request_id = state$last_request_id,
        retry_of = if (i > 1L) state$last_request_id else NULL
      ))
    )
    state$last_request_id <- request$request_id
    if (identical(response$status, "budget_exhausted")) return(FALSE)
    state$stage <- "response_normalization"
    state$stage <- "response_sanitization"
    reason_synthesized <- isTRUE(attr(
      response, "sas2r_private_reason_synthesized"
    ))
    response <- sanitize_llm_public(response, state$redactor)
    if (reason_synthesized) {
      attr(response, "sas2r_private_reason_synthesized") <- TRUE
    }
    state$requested_model <- response$requested_model %||%
      state$requested_model
    state$resolved_model <- response$resolved_model %||% state$resolved_model
    state$stage <- "metric_normalization"
    call_cost <- extract_response_cost(response, tier = tier)
    usage <- closed_llm_failure_usage(
      response$usage %||% attr(response, "usage", exact = TRUE) %||% list()
    )
    state$usage <- usage
    state$cost <- call_cost
    if (!is.na(call_cost)) {
      cumulative_spend <- cumulative_spend + call_cost
      state$cumulative_spend <- cumulative_spend
      state$stage <- "on_charge"
      charged_total <- if (is.function(on_charge)) on_charge(call_cost) else NULL
      if (is.numeric(charged_total) && length(charged_total) == 1L &&
          !is.na(charged_total) && is.finite(charged_total)) {
        cumulative_spend <- charged_total
        state$cumulative_spend <- cumulative_spend
      }
    } else if (is.function(on_charge)) {
      state$stage <- "on_charge"
      on_charge(NA_real_)
    }

    if (identical(response$status, "failed")) {
      last_failure <- llm_condition_from_failure_response(
        response, state$auth_context
      )
      retryable <- inherits(last_failure, c(
        "sas2r_llm_rate_limit", "sas2r_llm_timeout",
        "sas2r_llm_transport_error", "sas2r_llm_network_error",
        "sas2r_llm_access_failed"
      ))
      if (!retryable || identical(i, max_retries)) stop(last_failure)
      state$stage <- "audit_write"
      probe_attempt_failure_audit(
        last_failure, log_dir, state$redactor, tier, i,
        provider = state$provider,
        requested_model = state$requested_model,
        resolved_model = state$resolved_model,
        capability_hash = state$capability_hash,
        cumulative_spend = cumulative_spend
      )
      next
    }

    state$stage <- "audit_write"
    audit_entry <- list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      agent = "probe",
      type = if (identical(response$action, "final")) "final" else response$action,
      status = response$status, provider = state$provider,
      requested_model = state$requested_model,
      resolved_model = state$resolved_model,
      capability_hash = state$capability_hash,
      tier = tier, attempt = i,
      input_tokens = usage$input_tokens, output_tokens = usage$output_tokens,
      total_input_tokens = usage$total_input_tokens,
      total_output_tokens = usage$total_output_tokens,
      total_tokens = usage$total_tokens,
      cached_input_tokens = usage$cached_input_tokens,
      cache_write_tokens = usage$cache_write_tokens,
      reasoning_tokens = usage$reasoning_tokens,
      cost_usd = call_cost,
      cost_status = response$cost$status %||%
        if (is.na(call_cost)) "unknown" else "catalog_estimate",
      cumulative_spend_usd = cumulative_spend,
      request_id = response$request_id
    )
    llm_log(audit_entry, dir = log_dir, redactor = state$redactor)
    if (identical(response$status, "completed") && isTRUE(response$data$ok)) {
      return(TRUE)
    }
    return(FALSE)
  }
  if (!is.null(last_failure)) stop(last_failure)
  FALSE
}

#' Probe LLM connectivity with a tiny structured ping
#'
#' Validates authentication, endpoint reachability, and structured-output
#' support on the configured model with one minimal 32-token request. The
#' probe never launches an interactive browser or device login: a missing or
#' expired ambient session raises a classed condition naming the command to
#' run instead.
#'
#' The ping carries no tools, so it does **not** exercise tool calling. A
#' model that answers the probe may still lack tool support; that is caught
#' at the first tool-using phase, not here.
#'
#' An attempt that reaches the provider is written to the audit log. A call
#' refused before transport -- by the capability gate, or by an exhausted
#' budget -- returns `FALSE` without an audit entry. Reaching the durable
#' usage ledger additionally requires `usage_budget` to carry a
#' `ledger_path`, as it does when a translation run probes through its own
#' shared budget; a standalone call with no `usage_budget` gets a transient
#' budget with none, so it is audited but not ledgered.
#'
#' @param llm A `sas2r_llm` adapter from [sas_llm()], or an `llm:` mapping.
#' @param max_retries Maximum retry attempts for a retryable failure.
#' @param log_dir Directory the audit log is written to.
#' @param on_charge Deprecated compatibility callback.
#' @param tier Tier whose model is probed (`"cheap"` by default).
#' @param can_attempt Optional predicate consulted before each attempt.
#' @param usage_budget Optional shared usage budget. Pass one with a
#'   `ledger_path` to persist the attempt; omitted, a transient budget is
#'   created and the call is audited but not ledgered.
#' @param audit_context Optional named list merged into the audit context.
#' @return `TRUE` when the model answered the ping, `FALSE` on a definitive
#'   nonanswer. Access, authentication, and transport failures are raised as
#'   classed conditions with secrets redacted.
#' @seealso [sas_llm()], [sas_llm_models()]
#' @export
sas_llm_probe <- function(llm, max_retries = 2L, log_dir = tempdir(),
                          on_charge = NULL, tier = "cheap", can_attempt = NULL,
                          usage_budget = NULL, audit_context = list()) {
  if (is.null(usage_budget)) usage_budget <- new_usage_budget()
  assert_usage_budget(usage_budget)
  state <- new.env(parent = emptyenv())
  state$redactor <- redact_llm_secrets
  state$provider <- NULL
  state$requested_model <- NULL
  state$resolved_model <- NULL
  state$auth_context <- NULL
  state$stage <- "setup"
  state$classify_access <- FALSE
  state$attempt <- 1L
  state$cumulative_spend <- 0
  state$usage <- NULL
  state$cost <- NA_real_
  state$last_request_id <- NULL
  state$capability_hash <- NULL

  tryCatch(
    {
      state$redactor <- if (inherits(llm, "sas2r_llm")) {
        llm_audit_redactor(llm)
      } else {
        new_llm_audit_redactor(llm_config_secret_values(llm))
      }
      state$provider <- if (is.list(llm)) llm$provider %||% NULL else NULL
      state$auth_context <- if (inherits(llm, "sas2r_llm")) {
        attr(llm, "auth_context", exact = TRUE)
      } else NULL
      state$requested_model <- if (inherits(llm, "sas2r_llm")) {
        probe_tier_model(llm, tier, state$auth_context)
      } else if (is.list(llm)) {
        llm$model %||% NULL
      } else NULL
      state$resolved_model <- state$requested_model
      sas_llm_probe_impl(
        llm, max_retries, log_dir, on_charge, tier, can_attempt,
        usage_budget, audit_context, state
      )
    },
    error = function(error) {
      error <- probe_failure_condition(
        error, state$auth_context,
        classify_access = state$classify_access,
        local_stage = state$stage
      )
      error <- probe_terminal_audit(
        error, log_dir, state$redactor, tier, state$attempt,
        provider = state$provider,
        requested_model = state$requested_model,
        resolved_model = state$resolved_model,
        capability_hash = state$capability_hash,
        cumulative_spend = state$cumulative_spend,
        fallback_usage = state$usage, fallback_cost = state$cost
      )
      stop(error)
    }
  )
}

#' Write pinned LLM configuration and prompt hashes to lockfile
#' @noRd
write_llm_lock <- function(cfg, prompts_dir, path = "_sas2r.lock", specs = NULL,
                           requested_parameters = NULL,
                           effective_parameters = NULL,
                           capability_record = NULL) {
  prompts <- if (!is.null(specs)) {
    prompt_files <- c(
      vapply(specs, function(spec) spec$prompt, character(1)),
      specs$translator$prompt_macro %||% "translator-macro.md"
    )
    prompt_files <- unique(prompt_files[nzchar(prompt_files)])
    resolved <- vapply(prompt_files, function(prompt) {
      if (grepl("[/\\]", prompt) && file.exists(prompt)) {
        prompt
      } else if (!grepl("[/\\]", prompt)) {
        packaged <- system.file("prompts", prompt, package = "sas2r")
        if (nzchar(packaged) && file.exists(packaged)) packaged else ""
      } else {
        cli::cli_warn("prompt {.val {prompt}} could not be resolved for lockfile pinning")
        ""
      }
    }, character(1))
    resolved[nzchar(resolved) & file.exists(resolved)]
  } else if (is.character(prompts_dir) && length(prompts_dir) == 1L &&
             dir.exists(prompts_dir)) {
    list.files(prompts_dir, pattern = "\\.md$", full.names = TRUE)
  } else if (is.character(prompts_dir)) {
    prompts_dir[file.exists(prompts_dir)]
  } else character()

  hashes <- as.list(tools::md5sum(prompts))
  names(hashes) <- basename(prompts)
  hashes <- hashes[nzchar(names(hashes))]
  if (is.null(requested_parameters)) {
    requested_parameters <- lapply(LLM_OPTIONAL_PARAMETERS, function(name) cfg[[name]])
    names(requested_parameters) <- LLM_OPTIONAL_PARAMETERS
  }
  if (is.null(effective_parameters)) {
    effective_parameters <- stats::setNames(
      rep(list(NULL), length(LLM_OPTIONAL_PARAMETERS)), LLM_OPTIONAL_PARAMETERS
    )
    lock_model <- cfg$model %||% unlist(cfg$tiers, use.names = FALSE)[1]
    if (!is.null(lock_model) && length(lock_model) == 1L && !is.na(lock_model) &&
        nzchar(lock_model) && isTRUE(cfg$provider %in% llm_provider_ids())) {
      capability_record <- capability_record %||% resolve_model_capabilities(
        cfg$provider, ellmer_endpoint_identity(cfg), lock_model,
        ellmer_api_version_identity(cfg), overrides = cfg$capabilities
      )
      lock_request <- do.call(llm_request, c(
        list(messages = list(list(role = "user", content = "lockfile"))),
        requested_parameters
      ))
      supported <- effective_model_params(lock_request, capability_record)
      for (name in names(supported)) effective_parameters[[name]] <- supported[[name]]
    }
  }
  payload <- list(
    provider = cfg$provider, model = cfg$model, tiers = cfg$tiers,
    identity = llm_selector_identity(cfg),
    requested_parameters = requested_parameters,
    effective_parameters = effective_parameters,
    capability_record_hash = capability_record$record_hash %||% NULL,
    prompts = hashes
  )
  jsonlite::write_json(redact_llm_secrets(
    payload, secret_values = unique(c(
      llm_config_secret_values(cfg), llm_registered_secret_values()
    ))
  ), path, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
  invisible(path)
}

#' Append LLM request/response entry to local JSONL log
#' @noRd
llm_log <- function(entry, dir = ".sas2r", redactor = redact_llm_secrets) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  if (!is.function(redactor)) {
    cli::cli_abort("LLM audit redactor must be a function",
                   class = "sas2r_llm_redaction_error")
  }
  entry <- redactor(entry)
  cat(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", na = "null"),
      "\n", file = file.path(dir, "llm_log.jsonl"), append = TRUE, sep = "")
  invisible(entry)
}
