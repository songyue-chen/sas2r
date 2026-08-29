AGENT_OUTPUT_SCHEMA_VERSION <- "1"
OUTPUT_SCHEMAS <- c(
  "program_translation_v1", "program_review_v1", "program_fix_v1"
)

agent_output_schema <- function(schema_name) {
  if (!is.character(schema_name) || length(schema_name) != 1L ||
      !schema_name %in% OUTPUT_SCHEMAS) {
    cli::cli_abort("unknown agent output schema {.val {schema_name}}",
                   class = "sas2r_output_schema_error")
  }
  file_name <- switch(schema_name,
    program_translation_v1 = "program-translation-v1.json",
    program_review_v1 = "program-review-v1.json",
    program_fix_v1 = "program-fix-v1.json"
  )
  path <- system.file("schemas", file_name, package = "sas2r")
  if (!nzchar(path) || !file.exists(path)) {
    dev_path <- file.path(getwd(), "inst", "schemas", file_name)
    if (file.exists(dev_path)) path <- dev_path
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("agent output schema asset is unavailable",
                   class = "sas2r_output_schema_error")
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

json_type_matches <- function(value, type) {
  if (length(type) > 1L) {
    return(any(vapply(type, function(t) json_type_matches(value, t), logical(1))))
  }
  switch(type,
    object = is.list(value) && (length(value) == 0L || !is.null(names(value))),
    array = !is.null(value) && (
      (is.list(value) && (length(value) == 0L || is.null(names(value)))) ||
      is.atomic(value)
    ),
    string = is.character(value) && length(value) == 1L && !is.na(value),
    number = is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value),
    integer = is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) && value == as.integer(value),
    boolean = is.logical(value) && length(value) == 1L && !is.na(value),
    "null" = is.null(value),
    FALSE
  )
}

validate_schema_value <- function(value, schema, path = "$", errors = character()) {
  if (!is.null(schema$oneOf)) {
    match_count <- 0L
    for (sub_s in schema$oneOf) {
      sub_errs <- validate_schema_value(value, sub_s, path, character())
      if (length(sub_errs) == 0L) {
        match_count <- match_count + 1L
      }
    }
    if (match_count != 1L) {
      return(c(errors, paste0("value at ", path, " must match exactly one of oneOf schemas")))
    }
    return(errors)
  }
  if (!is.null(schema$anyOf)) {
    matched <- FALSE
    for (sub_s in schema$anyOf) {
      sub_errs <- validate_schema_value(value, sub_s, path, character())
      if (length(sub_errs) == 0L) {
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      return(c(errors, paste0("value at ", path, " does not match any of anyOf schemas")))
    }
    return(errors)
  }
  type <- schema$type %||% NULL
  if (!is.null(type) && !json_type_matches(value, type)) {
    return(c(errors, paste0("wrong type at ", path, ": expected ", paste(type, collapse = "|"))))
  }
  if (!is.null(schema$enum)) {
    allowed <- unlist(schema$enum)
    if (!is.atomic(value) || length(value) != 1L || !value %in% allowed) {
      return(c(errors, paste0("value at ", path, " not in enum")))
    }
  }
  if (identical(type, "object") || (is.null(type) && is.list(value) && !is.null(names(value)))) {
    required <- as.character(unlist(schema$required %||% character()))
    val_names <- names(value) %||% character()
    missing <- required[!required %in% val_names]
    if (length(missing)) {
      errors <- c(errors, paste0("missing field: ", missing))
    }
    properties <- schema$properties %||% list()
    if (identical(schema$additionalProperties, FALSE)) {
      extra <- setdiff(val_names, names(properties))
      if (length(extra)) errors <- c(errors, paste0("unexpected field: ", extra))
    }
    for (name in intersect(val_names, names(properties))) {
      errors <- validate_schema_value(
        value[[name]], properties[[name]], paste0(path, ".", name), errors
      )
    }
  } else if (identical(type, "array")) {
    values <- if (is.list(value)) value else as.list(value)
    if (!is.null(schema$minItems) && length(values) < schema$minItems) {
      errors <- c(errors, paste0("too few items at ", path))
    }
    if (!is.null(schema$maxItems) && length(values) > schema$maxItems) {
      errors <- c(errors, paste0("too many items at ", path))
    }
    if (isTRUE(schema$uniqueItems) && length(values) > 1L) {
      is_scalar_list <- all(vapply(values, function(x) is.atomic(x) && length(x) == 1L && !is.na(x), logical(1)))
      if (is_scalar_list) {
        if (anyDuplicated(unlist(values)) > 0L) {
          errors <- c(errors, paste0("duplicate items at ", path))
        }
      } else {
        ser <- vapply(values, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), character(1))
        if (anyDuplicated(ser) > 0L) {
          errors <- c(errors, paste0("duplicate items at ", path))
        }
      }
    }
    if (!is.null(schema$items)) {
      for (i in seq_along(values)) {
        errors <- validate_schema_value(
          values[[i]], schema$items, paste0(path, "[", i, "]"), errors
        )
      }
    }
  } else if (identical(type, "string")) {
    n <- nchar(value, type = "chars")
    if (!is.null(schema$minLength) && n < schema$minLength) {
      errors <- c(errors, paste0("string too short at ", path))
    }
    if (!is.null(schema$maxLength) && n > schema$maxLength) {
      errors <- c(errors, paste0("string too long at ", path))
    }
  } else if (identical(type, "number") || identical(type, "integer")) {
    if (!is.null(schema$minimum) && value < schema$minimum) {
      errors <- c(errors, paste0("value below minimum at ", path))
    }
    if (!is.null(schema$maximum) && value > schema$maximum) {
      errors <- c(errors, paste0("value above maximum at ", path))
    }
  }
  errors
}

#' Validate LLM output against a named schema
#'
#' @param data Extracted data list.
#' @param schema_name Name of schema in `OUTPUT_SCHEMAS`.
#' @return List with `ok` (logical) and `errors` (character vector).
#' @noRd
validate_output <- function(data, schema_name) {
  schema <- tryCatch(agent_output_schema(schema_name), error = function(error) NULL)
  if (is.null(schema)) return(list(ok = FALSE, errors = "unknown schema"))
  errors <- validate_schema_value(data, schema)
  list(ok = !length(errors), errors = errors)
}

#' Validate agent output against a named schema
#'
#' @param data Extracted output data list.
#' @param schema_name Name of the schema in `OUTPUT_SCHEMAS`.
#' @return The validated `data` invisibly, or aborts with class `sas2r_schema_error`.
#' @noRd
validate_agent_output <- function(data, schema_name) {
  v <- validate_output(data, schema_name)
  if (!isTRUE(v$ok)) {
    cli::cli_abort(
      c("Output failed validation against schema {.val {schema_name}}:",
        v$errors),
      class = "sas2r_schema_error"
    )
  }
  invisible(data)
}

#' Render prompt markdown with placeholder substitutions
#'
#' @param file Basename of prompt markdown file.
#' @param vars Named list of variable replacements.
#' @return Rendered prompt text string.
#' @noRd
render_prompt <- function(file, vars = list()) {
  path <- if (grepl("[/\\]", file)) {
    if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", file)) {
      cli::cli_abort("relative prompt path {.path {file}} not allowed; use a bare filename or absolute path",
                     class = "sas2r_prompt_error")
    }
    if (file.exists(file)) file else ""
  } else {
    system.file("prompts", file, package = "sas2r")
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("prompt file {.path {file}} not found", class = "sas2r_prompt_error")
  }
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  for (k in names(vars)) {
    txt <- gsub(paste0("{{", k, "}}"),
                paste(as.character(vars[[k]]), collapse = ", "),
                txt, fixed = TRUE)
  }
  gsub("\\{\\{[a-z_]+\\}\\}", "", txt)   # unset placeholders vanish
}

#' Extract explicitly classified cost from transport response metadata
#' @param resp Response object returned from LLM backend.
#' @param tier Model tier name, retained for compatibility.
#' @return Numeric cost in USD, or `NA_real_` when no classified amount exists.
#' @noRd
extract_response_cost <- function(resp, tier = "frontier") {
  if (inherits(resp, "sas2r_llm_response") && is.list(resp$cost) &&
      !is.null(resp$cost$amount_usd)) {
    c_val <- scalar_number_or_na(resp$cost$amount_usd)
    if (!is.na(c_val)) return(c_val)
  }
  if (!is.null(attr(resp, "cost_usd"))) {
    c_val <- scalar_number_or_na(attr(resp, "cost_usd"))
    if (!is.na(c_val)) return(c_val)
  }
  NA_real_
}

transport_usage <- function(resp) {
  usage <- if (inherits(resp, "sas2r_llm_response")) resp$usage else NULL
  if (is.null(usage)) usage <- attr(resp, "usage")
  if (is.null(usage) || !is.list(usage)) {
    return(normalized_usage(list()))
  }
  normalized_usage(list(usage = usage))
}

new_agent_tool_state <- function(limit, usage_budget = NULL) {
  state <- new.env(parent = emptyenv())
  state$count <- 0L
  state$limit <- as.integer(limit)
  state$exhausted <- FALSE
  state$usage_budget <- usage_budget
  state$audit_context <- list()
  state
}

reserve_agent_tool_call <- function(state, tool_name = NULL,
                                    arguments = NULL) {
  dynamic_context <- current_usage_tool_audit_context()
  if (!is.list(dynamic_context)) dynamic_context <- list()
  audit_context <- utils::modifyList(state$audit_context, dynamic_context)
  if (state$count >= state$limit) {
    state$exhausted <- TRUE
    # Per invocation, not global: tool_state is built inside run_agent(), so
    # this allowance covers one unit and resets for the next. The project-wide
    # ceiling is budget: max_tool_calls, enforced separately by the ledger.
    refuse_usage_tool_call(
      state$usage_budget, tool_name = tool_name, arguments = arguments,
      audit_context = audit_context, result_status = "agent_tool_limit",
      error_class = "sas2r_agent_tool_limit"
    )
    cli::cli_abort(
      "agent reached its per-call tool_call_limit of {state$limit}",
      class = "sas2r_agent_tool_limit"
    )
  }
  reservation <- reserve_usage_tool_call(
    state$usage_budget, tool_name = tool_name, arguments = arguments,
    audit_context = audit_context
  )
  state$count <- state$count + 1L
  invisible(reservation)
}

record_agent_tool_result <- function(state, reservation, value = NULL,
                                     error = NULL) {
  result_status <- if (is.list(value) &&
                       is.character(value$error) &&
                       length(value$error) == 1L) value$error else NULL
  refused_results <- c(
    "budget_exhausted", "tool_budget_hard_stop", "unknown_tool"
  )
  refused_errors <- c(
    "sas2r_tool_budget_error", "sas2r_tool_arguments_error",
    "sas2r_agent_tool_limit", "sas2r_budget_error"
  )
  outcome <- if (!is.null(error)) {
    if (any(inherits(error, refused_errors))) "refused" else "failed"
  } else if (!is.null(result_status) && result_status %in% refused_results) {
    "refused"
  } else {
    "completed"
  }
  complete_usage_tool_call(
    state$usage_budget, reservation, outcome = outcome,
    result_status = result_status,
    error_class = if (is.null(error)) NULL else class(error)[[1L]]
  )
  invisible(outcome)
}

bind_transport_tool_limits <- function(tools, state) {
  lapply(tools, function(tool) {
    bound <- tool
    call <- tool$call
    bound$call <- function(args) {
      reservation <- reserve_agent_tool_call(
        state, tool_name = tool$name %||% NULL, arguments = args
      )
      value <- tryCatch(call(args), error = identity)
      if (inherits(value, "condition")) {
        record_agent_tool_result(state, reservation, error = value)
        stop(value)
      }
      record_agent_tool_result(state, reservation, value = value)
      value
    }
    bound
  })
}

#' Execute agent deterministic loop
#'
#' @param spec Agent spec.
#' @param llm `sas2r_llm` instance.
#' @param tools Named list of tools.
#' @param user_content Initial user prompt.
#' @param log_dir Directory to append LLM audit log.
#' @param prompt_vars Named list of prompt replacements.
#' @param on_charge Deprecated compatibility callback called with numeric cost.
#' @param usage_budget Shared usage ledger and request-reservation policy.
#' @return Status list.
#' @noRd
# One agent request is retried at most this many extra times on a transient
# transport failure (rate limit, timeout, connection). Retries happen here at
# the sas2r level -- never inside ellmer -- so every attempt passes through
# attempt_llm_request() and is counted by the usage ledger.
AGENT_TRANSIENT_RETRY_LIMIT <- 2L

agent_transient_backoff_seconds <- function(attempt) {
  base <- getOption("sas2r.agent_backoff_base", 2)
  min(base ^ attempt, 30)
}

run_agent <- function(spec, llm, tools, user_content, log_dir = ".sas2r",
                      prompt_vars = list(), on_charge = NULL,
                      usage_budget = NULL, audit_context = list()) {
  if (is.null(usage_budget)) usage_budget <- new_usage_budget()
  assert_usage_budget(usage_budget)
  start_known <- usage_budget$known_amount
  start_billed <- usage_budget$billed_amount
  start_estimated <- usage_budget$estimated_amount
  start_unknown <- usage_budget$unknown_count
  messages <- list(
    list(role = "system", content = render_prompt(spec$prompt, prompt_vars)),
    list(role = "user", content = user_content))
  tool_calls <- 0L; retries <- 0L; transient_attempts <- 0L
  # Set once the tool allowance runs out and the model has been asked to answer
  # with what it already gathered. Without it the exhausted check below would
  # abandon the unit on the very pass that is meant to rescue it.
  tool_limit_finalized <- FALSE
  audit_redactor <- llm_audit_redactor(llm)
  tier <- spec$tier %||% "frontier"
  capabilities <- llm_capabilities_for(llm, tier = tier)
  has_tools <- length(tools) > 0L
  tool_state <- new_agent_tool_state(spec$tool_call_limit, usage_budget)
  transport_tools <- bind_transport_tool_limits(tools, tool_state)
  if (has_tools && !identical(capabilities$tool_calling, "native")) {
    return(list(
      status = "tool_calling_unavailable", tool_calls = 0L,
      spend_usd = 0, known_cost_usd = 0, cost_unknown = FALSE
    ))
  }
  schema_mode <- capabilities$structured_output
  if (!schema_mode %in% c("native", "fallback")) {
    return(list(
      status = "structured_output_unavailable", tool_calls = 0L,
      spend_usd = 0, known_cost_usd = 0, cost_unknown = FALSE
    ))
  }
  coexist <- has_tools &&
    identical(capabilities$tools_with_structured_output, "supported")
  phase <- if (has_tools && !coexist) "gathering" else "finalization"
  schema <- agent_output_schema(spec$output_schema)
  parent_request_id <- NULL
  retry_of <- NULL
  result_costs <- function(result) {
    result$known_cost_usd <- usage_budget$known_amount - start_known
    result$estimated_cost_usd <- usage_budget$estimated_amount - start_estimated
    result$spend_usd <- usage_budget$billed_amount - start_billed
    result$cost_unknown <- usage_budget$unknown_count > start_unknown
    result
  }
  repeat {
    capabilities <- llm_capabilities_for(llm, tier = tier)
    request <- llm_request(
      messages = messages,
      tier = tier,
      tools = if (identical(phase, "gathering") || coexist)
        transport_tools else list(),
      output_schema = if (identical(phase, "finalization")) schema else NULL,
      schema_name = if (identical(phase, "finalization")) spec$output_schema else NULL,
      schema_version = if (identical(phase, "finalization"))
        AGENT_OUTPUT_SCHEMA_VERSION else NULL,
      schema_mode = if (identical(phase, "finalization")) schema_mode else NULL,
      temperature = resolve_model_parameter(spec, llm, "temperature"),
      reasoning_effort = resolve_model_parameter(spec, llm, "reasoning_effort"),
      max_output_tokens = resolve_model_parameter(spec, llm, "max_output_tokens"),
      model = capabilities$model %||% llm$model,
      phase = phase
    )
    request$parent_request_id <- parent_request_id
    request$retry_of <- retry_of
    request_context <- utils::modifyList(audit_context, list(
      provider = llm$provider,
      requested_model = request$model %||% llm$model,
      resolved_model = request$model %||% llm$model,
      agent = spec$name %||% "agent", tier = tier,
      purpose = audit_context$purpose %||% phase,
      parent_request_id = parent_request_id,
      retry_of = retry_of,
      max_tool_calls = spec$tool_call_limit,
      capability_hash = capabilities$record_hash
    ))
    tool_state$audit_context <- utils::modifyList(request_context, list(
      request_id = request$request_id,
      phase = phase
    ))
    resp <- attempt_llm_request(
      request, llm, usage_budget = usage_budget,
      audit_context = request_context
    )
    resp <- sanitize_llm_public(resp, audit_redactor)
    executing_request_id <- resp$request_id %||% request$request_id
    tool_state$audit_context$request_id <- executing_request_id
    if (!identical(executing_request_id, request$request_id)) {
      tool_state$audit_context$parent_request_id <- request$request_id
    }
    parent_request_id <- request$request_id
    retry_of <- NULL
    tool_calls <- tool_state$count
    call_cost <- extract_response_cost(resp, tier = tier)
    usage <- transport_usage(resp)
    if (is.function(on_charge)) on_charge(call_cost)
    if (identical(resp$status, "budget_exhausted")) {
      return(result_costs(list(
        status = "budget_exhausted", tool_calls = tool_calls,
        budget_error = attr(resp, "budget_error", exact = TRUE)
      )))
    }
    audit_entry <- list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      agent = spec$name %||% "agent",
      role = audit_context$role %||% spec$name %||% "agent",
      type = resp$action %||% "none",
      status = resp$status,
      request_id = resp$request_id,
      response_id = resp$response_id,
      provider = llm$provider,
      requested_model = resp$requested_model,
      resolved_model = resp$resolved_model,
      tier = tier,
      phase = phase,
      schema_mode = request$schema_mode,
      schema_version = request$schema_version,
      capability_hash = resp$capability_hash %||% capabilities$record_hash,
      retry_count = resp$retry_count %||% 0L,
      requested_parameters = request$parameters,
      effective_parameters = resp$effective_parameters %||%
        effective_model_params(request, capabilities),
      downgraded_parameters = resp$downgraded_parameters %||% character(),
      withheld_parameters = resp$withheld_parameters %||%
        withheld_model_params(request, capabilities),
      input_tokens = usage$input_tokens,
      output_tokens = usage$output_tokens,
      total_input_tokens = usage$total_input_tokens,
      total_output_tokens = usage$total_output_tokens,
      total_tokens = usage$total_tokens,
      cached_input_tokens = usage$cached_input_tokens,
      cache_write_tokens = usage$cache_write_tokens,
      reasoning_tokens = usage$reasoning_tokens,
      cost_usd = call_cost,
      cost_status = resp$cost$status %||%
        if (is.na(call_cost)) "unknown" else "catalog_estimate"
    )
    for (k in c("component_id", "revision_id", "round", "attempt_id", "prompt_hash", "skill_hash", "skill_provenance")) {
      if (!is.null(audit_context[[k]])) {
        audit_entry[[k]] <- audit_context[[k]]
      }
    }
    if (identical(resp$status, "failed")) {
      audit_entry$error <- llm_failure_metadata(
        llm_condition_from_failure_response(resp)
      )
    }
    llm_log(audit_entry, dir = log_dir, redactor = audit_redactor)
    # A spent tool allowance only ends the unit when the model still wants
    # tools. The transport resolves tool calls inside one request, so the
    # allowance is spent by the bound tool wrappers rather than by the
    # tool_call branch below, and the model goes on to answer -- it simply
    # stops being given tools. Bailing on exhaustion alone discarded a
    # completed 53,244-token translation because the tools used along the way
    # had run out.
    if (isTRUE(tool_state$exhausted) && !tool_limit_finalized &&
        !identical(resp$action, "final")) {
      return(result_costs(list(
        status = "agent_tool_limit_reached", tool_calls = tool_calls
      )))
    }
    if (identical(resp$status, "refused")) {
      return(result_costs(list(status = "refused", tool_calls = tool_calls)))
    }
    if (identical(resp$status, "incomplete")) {
      return(result_costs(list(
        status = "incomplete", finish_reason = resp$finish_reason,
        tool_calls = tool_calls
      )))
    }
    if (identical(resp$status, "failed")) {
      failure_status <- switch(resp$error$class %||% "",
        sas2r_llm_authentication_error = "authentication_failed",
        sas2r_llm_rate_limit = "rate_limited",
        sas2r_llm_timeout = "timed_out",
        sas2r_llm_invalid_schema = "invalid_schema",
        "transport_failed"
      )
      # Gate on the error class, not the coarse status: budget and tool-limit
      # refusals also surface as transport_failed but are deterministic.
      transient <- (resp$error$class %||% "") %in% c(
        "sas2r_llm_rate_limit", "sas2r_llm_timeout",
        "sas2r_llm_transport_error", "sas2r_llm_network_error"
      )
      if (transient && transient_attempts < AGENT_TRANSIENT_RETRY_LIMIT) {
        transient_attempts <- transient_attempts + 1L
        Sys.sleep(agent_transient_backoff_seconds(transient_attempts))
        retry_of <- executing_request_id
        next
      }
      return(result_costs(list(
        status = failure_status, error = resp$error,
        tool_calls = tool_calls
      )))
    }
    if (identical(resp$action, "tool_call")) {
      limit_reached <- FALSE
      tool_name <- if (is.character(resp$tool_name) &&
                       length(resp$tool_name) == 1L) resp$tool_name else ""
      tool_arguments <- resp$tool_arguments %||% list()
      tool_reservation <- NULL
      reserved <- tryCatch(
        {
          tool_reservation <- reserve_agent_tool_call(
            tool_state, tool_name = tool_name, arguments = tool_arguments
          )
          TRUE
        },
        sas2r_agent_tool_limit = function(error) {
          limit_reached <<- TRUE
          FALSE
        },
        sas2r_budget_error = function(error) FALSE
      )
      tool_calls <- tool_state$count
      if (!reserved) {
        # A spent tool allowance is not a spent budget. Everything gathered so
        # far has already been paid for, so ask for the answer with what is in
        # hand rather than discarding the unit and the spend together. A real
        # budget failure still stops, because there is nothing left to spend.
        if (limit_reached && !tool_limit_finalized) {
          tool_limit_finalized <- TRUE
          messages <- c(messages, list(list(
            role = "user",
            content = paste(
              "The tool call allowance for this unit is used up.",
              "Return the complete final answer in the required schema using",
              "the context already gathered."
            )
          )))
          phase <- "finalization"
          parent_request_id <- request$request_id
          next
        }
        return(result_costs(list(
          status = if (limit_reached) "agent_tool_limit_reached" else
            "agent_budget_exhausted",
          tool_calls = tool_calls
        )))
      }
      tool <- tools[[tool_name]]
      out <- if (is.null(tool)) list(error = "unknown_tool") else
        tryCatch(tool$call(resp$tool_arguments %||% list()),
                 sas2r_tool_budget_error = function(e)
                   list(error = "tool_budget_hard_stop"),
                 error = function(e) {
                   record_agent_tool_result(
                     tool_state, tool_reservation, error = e
                   )
                   stop(e)
                 })
      record_agent_tool_result(
        tool_state, tool_reservation, value = out
      )
      messages <- c(messages, list(
        list(
          role = "assistant", content = "",
          tool_call = list(
            id = resp$tool_call_id, name = tool_name,
            arguments = resp$tool_arguments %||% list()
          )
        ),
        list(
        role = "tool",
        name = tool_name,
        tool_call_id = resp$tool_call_id,
        content = jsonlite::toJSON(out, auto_unbox = TRUE, force = TRUE)),
        list(
          role = "user",
          content = "Continue using the retained tool result."
        )
      ))
      parent_request_id <- request$request_id
    } else if (identical(phase, "gathering") &&
               identical(resp$action, "final")) {
      if (is.list(resp$conversation) && length(resp$conversation)) {
        messages <- resp$conversation
      } else {
        messages <- c(messages, list(list(
          role = "assistant",
          content = jsonlite::toJSON(
            resp$data %||% list(), auto_unbox = TRUE,
            null = "null", force = TRUE
          )
        )))
      }
      messages <- c(messages, list(list(
        role = "user",
        content = "Return the complete final answer in the required schema."
      )))
      phase <- "finalization"
      parent_request_id <- request$request_id
    } else if (identical(resp$action, "final")) {
      v <- validate_output(resp$data, spec$output_schema)
      if (v$ok) return(result_costs(list(
        status = "ok", data = resp$data, tool_calls = tool_calls,
        fallback_json = identical(schema_mode, "fallback")
      )))
      retries <- retries + 1L
      if (retries > spec$retry_limit)
        return(result_costs(list(
          status = "invalid_output", errors = v$errors
        )))
      retry_of <- request$request_id
      messages <- c(messages, list(
        list(
          role = "assistant",
          content = jsonlite::toJSON(
            resp$data %||% list(), auto_unbox = TRUE,
            null = "null", force = TRUE
          )
        ),
        list(
          role = "user",
          content = paste(
            "Output invalid:", paste(v$errors, collapse = "; "),
            "- resend the complete structured object."
          )
        )
      ))
    } else {
      retries <- retries + 1L
      if (retries > spec$retry_limit)
        return(result_costs(list(
          status = "invalid_output", errors = "malformed response"
        )))
      retry_of <- request$request_id
      messages <- c(messages, list(
        list(
          role = "assistant",
          content = if (is.null(resp$data)) "" else jsonlite::toJSON(
            resp$data, auto_unbox = TRUE, null = "null", force = TRUE
          )
        ),
        list(
          role = "user",
          content = "Malformed response; resend the complete structured object."
        )
      ))
    }
  }
}
