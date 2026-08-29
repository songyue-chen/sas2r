strict_json_list <- function(value, strip_markdown_fences = FALSE) {
  if (is.list(value)) return(value)
  if (!is.character(value) || length(value) != 1L || is.na(value)) return(NULL)
  text <- if (strip_markdown_fences) strip_fences(value) else trimws(value)
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (is.list(parsed)) parsed else NULL
}

scalar_number_or_na <- function(value) {
  if (!is.atomic(value) || length(value) != 1L) return(NA_real_)
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value)) NA_real_ else value
}

nonnegative_number_or_na <- function(value) {
  value <- scalar_number_or_na(value)
  if (is.na(value) || value < 0) NA_real_ else value
}

normalized_usage <- function(raw) {
  usage <- raw[["usage", exact = TRUE]]
  usage_metadata <- raw[["usageMetadata", exact = TRUE]]
  metadata_usage <- is.null(usage) &&
    is.list(usage_metadata) && length(usage_metadata)
  if (metadata_usage) usage <- usage_metadata
  if (is.null(usage)) usage <- attr(raw, "usage", exact = TRUE)
  if (is.null(usage) || !is.list(usage)) usage <- list()
  input_details <- usage$input_tokens_details %||%
    usage$prompt_tokens_details %||% list()
  output_details <- usage$output_tokens_details %||%
    usage$completion_tokens_details %||% list()
  if (!is.list(input_details)) input_details <- list()
  if (!is.list(output_details)) output_details <- list()
  first_number <- function(...) {
    for (value in list(...)) {
      number <- nonnegative_number_or_na(value)
      if (!is.na(number)) return(number)
    }
    NA_real_
  }
  present_list <- function(value) is.list(value) && length(value)
  any_present <- function(...) {
    any(!vapply(list(...), is.null, logical(1)))
  }
  safe_difference <- function(total, ...) {
    parts <- c(...)
    if (is.na(total) || anyNA(parts)) return(NA_real_)
    max(0, total - sum(parts))
  }

  inclusive_input_details <- present_list(usage$input_tokens_details)
  compatible_input_details <- present_list(usage$prompt_tokens_details)
  partitioned_cache_usage <- any_present(
    usage$cache_creation_input_tokens, usage$cache_read_input_tokens
  )
  hit_miss_cache_usage <- any_present(
    usage$prompt_cache_hit_tokens, usage$prompt_cache_miss_tokens
  )
  camel_case_usage <- any_present(
    usage$inputTokens, usage$outputTokens, usage$totalTokens,
    usage$cacheReadInputTokens, usage$cacheWriteInputTokens
  )
  raw_output_details <- present_list(usage$output_tokens_details) ||
    present_list(usage$completion_tokens_details)
  canonical_input <- !is.null(usage$total_input_tokens)
  canonical_output <- !is.null(usage$total_output_tokens)

  reported_input <- first_number(
    usage$input_tokens, usage$prompt_tokens, usage$input,
    usage$inputTokens, usage$promptTokenCount
  )
  reported_output <- first_number(
    usage$output_tokens, usage$completion_tokens, usage$output,
    usage$outputTokens, usage$candidatesTokenCount
  )
  total_input <- first_number(usage$total_input_tokens)
  total_output <- first_number(usage$total_output_tokens)
  cached_input <- first_number(
    usage$cached_input_tokens, usage$prompt_cache_hit_tokens,
    usage$cache_read_input_tokens, input_details$cached_tokens,
    usage$cacheReadInputTokens, usage$cachedContentTokenCount,
    usage$cached_input
  )
  cache_write <- first_number(
    usage$cache_write_tokens, usage$cache_creation_input_tokens,
    input_details$cache_write_tokens, usage$cacheWriteInputTokens
  )
  reasoning <- first_number(
    usage$reasoning_tokens, output_details$reasoning_tokens,
    output_details$thinking_tokens, usage$thoughtsTokenCount
  )
  native_miss <- first_number(usage$prompt_cache_miss_tokens)
  tool_input <- first_number(usage$toolUsePromptTokenCount)

  if (inclusive_input_details) {
    if (is.na(cached_input)) cached_input <- 0
    if (is.na(cache_write)) cache_write <- 0
  } else if (compatible_input_details && is.na(cached_input)) {
    cached_input <- 0
  }
  if (metadata_usage && is.na(cached_input)) cached_input <- 0
  if (camel_case_usage) {
    if (is.na(cached_input)) cached_input <- 0
    if (is.na(cache_write)) cache_write <- 0
  }
  if (partitioned_cache_usage) {
    if (is.na(cached_input)) cached_input <- 0
    if (is.na(cache_write)) cache_write <- 0
  }
  if (hit_miss_cache_usage && is.na(cached_input)) cached_input <- 0
  input <- reported_input
  accounting_parts <- NULL
  if (canonical_input) {
    total_input <- first_number(usage$total_input_tokens)
  } else if (metadata_usage) {
    prompt_input <- reported_input
    if (is.na(tool_input)) tool_input <- 0
    total_input <- if (is.na(prompt_input)) NA_real_ else
      prompt_input + tool_input
    input <- safe_difference(prompt_input, cached_input)
    if (!is.na(input)) input <- input + tool_input
    accounting_parts <- c(input, cached_input)
  } else if (camel_case_usage) {
    input <- reported_input
    total_input <- if (is.na(input)) NA_real_ else
      input + cached_input + cache_write
    accounting_parts <- c(input, cached_input, cache_write)
  } else if (partitioned_cache_usage) {
    input <- reported_input
    total_input <- if (is.na(input)) NA_real_ else
      input + cached_input + cache_write
    accounting_parts <- c(input, cached_input, cache_write)
  } else if (hit_miss_cache_usage) {
    total_input <- reported_input
    input <- if (is.na(native_miss)) {
      safe_difference(total_input, cached_input)
    } else {
      native_miss
    }
    accounting_parts <- c(input, cached_input)
  } else if (inclusive_input_details) {
    total_input <- reported_input
    input <- safe_difference(total_input, cached_input, cache_write)
    accounting_parts <- c(input, cached_input, cache_write)
  } else if (compatible_input_details) {
    total_input <- reported_input
    known_write <- if (is.na(cache_write)) 0 else cache_write
    input <- safe_difference(total_input, cached_input, known_write)
    accounting_parts <- c(
      input, cached_input, if (is.na(cache_write)) NULL else cache_write
    )
  } else if (is.na(total_input) && !is.na(input)) {
    known_buckets <- c(cached_input, cache_write)
    total_input <- input + sum(known_buckets[!is.na(known_buckets)])
  }

  output <- reported_output
  if (canonical_output) {
    total_output <- first_number(usage$total_output_tokens)
  } else if (metadata_usage) {
    total_output <- if (is.na(output) || is.na(reasoning)) NA_real_ else
      output + reasoning
  } else if (camel_case_usage) {
    total_output <- reported_output
  } else if (raw_output_details && !is.na(reasoning)) {
    total_output <- reported_output
    output <- safe_difference(total_output, reasoning)
  } else if (is.na(total_output) && !is.na(output)) {
    total_output <- output + if (is.na(reasoning)) 0 else reasoning
  }

  reported_total <- first_number(
    usage$total_tokens, usage$totalTokens, usage$totalTokenCount
  )
  total_tokens <- reported_total
  if (is.na(total_tokens) && !is.na(total_input) && !is.na(total_output)) {
    total_tokens <- total_input + total_output
  }

  accounting_status <- attr(
    usage, "input_accounting_status", exact = TRUE
  )
  accounting_delta <- scalar_number_or_na(attr(
    usage, "input_accounting_delta_tokens", exact = TRUE
  ))
  if (is.null(accounting_status)) {
    if (!is.null(accounting_parts) && !is.na(total_input) &&
        !anyNA(accounting_parts)) {
      accounting_delta <- total_input - sum(accounting_parts)
      accounting_status <- if (accounting_delta == 0) {
        "consistent"
      } else {
        "mismatch"
      }
    } else {
      accounting_status <- "unavailable"
      accounting_delta <- NA_real_
    }
  }
  total_accounting_status <- attr(
    usage, "total_accounting_status", exact = TRUE
  )
  total_accounting_delta <- scalar_number_or_na(attr(
    usage, "total_accounting_delta_tokens", exact = TRUE
  ))
  if (is.null(total_accounting_status)) {
    derived_total <- if (is.na(total_input) || is.na(total_output)) {
      NA_real_
    } else {
      total_input + total_output
    }
    if (!is.na(reported_total) && !is.na(derived_total)) {
      total_accounting_delta <- reported_total - derived_total
      total_accounting_status <- if (total_accounting_delta == 0) {
        "consistent"
      } else {
        "mismatch"
      }
    } else {
      total_accounting_status <- "unavailable"
      total_accounting_delta <- NA_real_
    }
  }

  normalized <- list(
    input_tokens = input,
    output_tokens = output,
    total_input_tokens = total_input,
    total_output_tokens = total_output,
    total_tokens = total_tokens,
    cached_input_tokens = cached_input,
    cache_write_tokens = cache_write,
    reasoning_tokens = reasoning,
    tool_charges = first_number(usage$tool_charges)
  )
  attr(normalized, "input_accounting_status") <- accounting_status
  attr(normalized, "input_accounting_delta_tokens") <- accounting_delta
  attr(normalized, "total_accounting_status") <- total_accounting_status
  attr(normalized, "total_accounting_delta_tokens") <-
    total_accounting_delta
  provenance <- attr(usage, "token_provenance", exact = TRUE)
  if (!is.null(provenance)) attr(normalized, "token_provenance") <- provenance
  normalized
}

normalized_cost <- function(raw) {
  amount <- nonnegative_number_or_na(attr(raw, "cost_usd", exact = TRUE))
  supplied_status <- attr(raw, "cost_status", exact = TRUE)
  status <- if (is.null(supplied_status)) {
    if (is.na(amount)) "unknown" else "catalog_estimate"
  } else if (is.character(supplied_status) && length(supplied_status) == 1L &&
             !is.na(supplied_status) && supplied_status %in% USAGE_COST_STATES) {
    supplied_status
  } else {
    "unknown"
  }
  if (is.na(amount)) status <- "unknown"
  currency <- attr(raw, "cost_currency", exact = TRUE) %||%
    if (is.na(amount)) NULL else "USD"
  if (!is.na(amount) &&
      (!is.character(currency) || length(currency) != 1L ||
       is.na(currency) || !identical(toupper(currency), "USD"))) {
    status <- "unknown"
  }
  if (identical(status, "unknown")) amount <- NA_real_
  list(
    amount_usd = amount,
    status = status,
    currency = if (identical(status, "unknown")) NULL else currency,
    source = attr(raw, "cost_source", exact = TRUE) %||%
      if (is.na(amount)) NULL else "adapter cost metadata",
    source_version = attr(raw, "cost_source_version", exact = TRUE),
    effective_date = attr(raw, "cost_effective_date", exact = TRUE),
    rate_dimensions = attr(raw, "cost_rate_dimensions", exact = TRUE),
    provenance = attr(raw, "cost_provenance", exact = TRUE)
  )
}

terminal_finish_outcome <- function(finish_reason) {
  if (!is.character(finish_reason) || length(finish_reason) != 1L ||
      is.na(finish_reason)) return(NULL)
  reason <- tolower(gsub("[ -]+", "_", finish_reason))
  if (reason %in% c(
    "max_tokens", "max_output_tokens", "context_window", "length",
    "model_context_window_exceeded"
  )) return("incomplete")
  if (reason %in% c(
    "content_filter", "content_filtered", "guardrail_intervened",
    "safety", "recitation", "blocklist", "prohibited_content", "spii",
    "model_armor", "image_safety", "image_prohibited_content",
    "image_recitation", "escalation"
  )) return("refused")
  NULL
}

apply_terminal_finish <- function(response) {
  if (!identical(response$status, "completed")) return(response)
  outcome <- terminal_finish_outcome(response$finish_reason)
  if (is.null(outcome)) return(response)
  response$status <- outcome
  response$action <- "none"
  response$data <- NULL
  extra <- if (identical(outcome, "refused")) {
    "sas2r_llm_refused"
  } else {
    "sas2r_llm_incomplete"
  }
  class(response) <- unique(c(extra, class(response)))
  response
}

terminal_response_from_reason <- function(finish_reason, raw, request,
                                          provider, resolved_model = NULL) {
  outcome <- terminal_finish_outcome(finish_reason)
  if (is.null(outcome)) return(NULL)
  new_llm_response(
    status = outcome, action = "none", finish_reason = finish_reason,
    request = request, resolved_model = resolved_model,
    usage = normalized_usage(raw), cost = normalized_cost(raw),
    provider = provider,
    extra_class = if (identical(outcome, "refused")) {
      "sas2r_llm_refused"
    } else {
      "sas2r_llm_incomplete"
    }
  )
}

condition_status_code <- function(error) {
  candidates <- list(error$status_code, error$status)
  response <- error$response %||% error$resp
  if (!is.null(response)) {
    candidates <- c(candidates, list(
      tryCatch(response$status_code, error = function(e) NULL),
      tryCatch(response$status, error = function(e) NULL)
    ))
  }
  class_status <- sub(
    ".*httr2_http_([0-9]{3}).*", "\\1",
    paste(class(error), collapse = " ")
  )
  if (grepl("^[0-9]{3}$", class_status)) {
    candidates <- c(candidates, list(class_status))
  }
  for (candidate in candidates) {
    value <- suppressWarnings(as.integer(candidate))
    if (length(value) == 1L && !is.na(value)) return(value)
  }
  NA_integer_
}

failure_class <- function(error) {
  trusted <- recognized_llm_failure_class(error)
  if (!is.null(trusted)) return(trusted)
  known <- c(
    "sas2r_budget_exhausted", "sas2r_budget_unmeterable",
    "sas2r_llm_authentication_error", "sas2r_llm_permission_denied",
    "sas2r_llm_rate_limit",
    "sas2r_llm_timeout", "sas2r_llm_invalid_schema",
    "sas2r_llm_transport_error", "sas2r_llm_optional_parameter_error"
  )
  hit <- known[vapply(known, function(class) inherits(error, class), logical(1))]
  if (length(hit)) return(hit[[1]])

  status_code <- condition_status_code(error)
  message <- tolower(conditionMessage(error))
  if (!is.na(status_code) && status_code == 401L) {
    return("sas2r_llm_authentication_error")
  }
  if (!is.na(status_code) && status_code == 403L) {
    return("sas2r_llm_permission_denied")
  }
  if (!is.na(status_code) && status_code == 429L) {
    return("sas2r_llm_rate_limit")
  }
  if (grepl("timed? out|timeout", message)) return("sas2r_llm_timeout")
  if (grepl("invalid.{0,40}schema|schema.{0,40}invalid", message)) {
    return("sas2r_llm_invalid_schema")
  }
  "sas2r_llm_transport_error"
}

new_llm_response <- function(status, action = "none", data = NULL,
                             tool_name = NULL, tool_arguments = NULL,
                             tool_call_id = NULL, finish_reason = NULL,
                             response_id = NULL, request = NULL,
                             requested_model = NULL, resolved_model = NULL,
                             usage = NULL, cost = NULL, error = NULL,
                             provider = NULL, schema_mode = NULL,
                             schema_version = NULL, capability_hash = NULL,
                             conversation = NULL, extra_class = NULL) {
  if (is.null(usage)) {
    usage <- list(
      input_tokens = NA_real_, output_tokens = NA_real_,
      total_input_tokens = NA_real_, total_output_tokens = NA_real_,
      total_tokens = NA_real_,
      cached_input_tokens = NA_real_, cache_write_tokens = NA_real_,
      reasoning_tokens = NA_real_, tool_charges = NA_real_
    )
  }
  if (is.null(cost)) {
    cost <- list(amount_usd = NA_real_, status = "unknown", source = NULL)
  }
  response <- list(
    status = status,
    action = action,
    data = data,
    tool_name = tool_name,
    tool_arguments = tool_arguments,
    tool_call_id = tool_call_id,
    finish_reason = finish_reason,
    response_id = response_id,
    request_id = if (inherits(request, "sas2r_llm_request")) request$request_id else NULL,
    requested_model = requested_model %||%
      if (inherits(request, "sas2r_llm_request")) request$model else NULL,
    resolved_model = resolved_model,
    usage = usage,
    cost = cost,
    provider = provider,
    schema_mode = schema_mode %||%
      if (inherits(request, "sas2r_llm_request")) request$schema_mode else NULL,
    schema_version = schema_version %||%
      if (inherits(request, "sas2r_llm_request")) request$schema_version else NULL,
    capability_hash = capability_hash,
    conversation = conversation,
    error = error
  )
  class(response) <- unique(c(extra_class, "sas2r_llm_response", "list"))
  response <- apply_terminal_finish(response)
  if (!is.na(cost$amount_usd)) attr(response, "cost_usd") <- cost$amount_usd
  attr(response, "usage") <- usage
  response
}

normalize_legacy_response <- function(raw, request, provider) {
  type <- raw$type %||% ""
  if (identical(type, "tool") || identical(raw$action, "tool_call")) {
    return(new_llm_response(
      status = "completed", action = "tool_call",
      tool_name = raw$tool %||% raw$tool_name,
      tool_arguments = raw$args %||% raw$tool_arguments %||% list(),
      tool_call_id = raw$tool_call_id, finish_reason = raw$finish_reason,
      response_id = raw$id, request = request,
      resolved_model = raw$model, usage = normalized_usage(raw),
      cost = normalized_cost(raw), provider = provider
    ))
  }
  if (identical(type, "refusal") || identical(raw$status, "refused")) {
    return(new_llm_response(
      status = "refused", action = "none",
      finish_reason = raw$finish_reason %||% "refusal",
      response_id = raw$id, request = request, usage = normalized_usage(raw),
      cost = normalized_cost(raw), provider = provider,
      extra_class = "sas2r_llm_refused"
    ))
  }
  if (identical(type, "incomplete") || identical(raw$status, "incomplete")) {
    return(new_llm_response(
      status = "incomplete", action = "none",
      finish_reason = raw$finish_reason %||% "incomplete",
      response_id = raw$id, request = request, usage = normalized_usage(raw),
      cost = normalized_cost(raw), provider = provider,
      extra_class = "sas2r_llm_incomplete"
    ))
  }
  data <- raw$data
  if (is.list(data) && inherits(request, "sas2r_llm_request") && !is.null(request$schema_name)) {
    if (identical(request$schema_name, "program_fix_v1") && is.null(data$diagnosis) && !is.null(data$r_code)) {
      diag_src <- data$assumptions %||% data$summary
      diag <- if (length(diag_src)) paste(unlist(diag_src), collapse = "; ") else "repaired logic"
      flags_src <- if (length(data$flags)) {
        data$flags
      } else if (length(data$uncertainty) && is.list(data$uncertainty)) {
        vapply(data$uncertainty, function(u) if (is.list(u)) u$claim %||% "" else as.character(u), character(1))
      } else {
        list()
      }
      flags_list <- if (length(flags_src)) as.list(unlist(flags_src)) else list()
      data <- list(
        r_code = data$r_code,
        diagnosis = diag,
        summary = "applied fix",
        evidence_ids = list(),
        changed_interfaces = list(),
        affected_outputs = list(),
        remaining_uncertainty = flags_list,
        bundle_helper_patch = NULL
      )
    } else if (identical(request$schema_name, "program_translation_v1") && is.null(data$summary) && !is.null(data$r_code)) {
      summ <- if (length(data$assumptions)) paste(unlist(data$assumptions), collapse = "; ") else "translated logic"
      unc <- if (length(data$flags)) lapply(data$flags, function(f) list(severity = "low", claim = as.character(f), evidence = "model flag", affected_outputs = list())) else list()
      data <- list(
        r_code = data$r_code,
        summary = summ,
        parameters = list(),
        defaults = structure(list(), names = character(0)),
        reads = list(),
        writes = list(),
        side_effects = list(),
        helper_use = list(),
        discovered_dependencies = list(),
        suspected_dependencies = list(),
        affected_outputs = list(),
        uncertainty = unc
      )
    } else if (identical(request$schema_name, "program_review_v1")) {
      if (is.null(data$verdict) && !is.null(data$status)) {
        verdict <- switch(data$status,
          no_issue = "reviewed_no_material_finding",
          issue_found = "repair_required",
          "review_unavailable"
        )
        data$verdict <- verdict
        data$static_runnability <- if (identical(verdict, "reviewed_no_material_finding")) "looks_runnable" else "material_issue"
        data$findings <- if (identical(data$status, "no_issue")) list() else list(
          list(
            severity = "material",
            sas_evidence = data$explanation %||% "source statement",
            r_evidence = paste(unlist(data$issue_codes %||% "issue"), collapse = ", "),
            affected_outputs = list(),
            confidence = data$confidence %||% 0.9,
            unresolved_dependencies = list()
          )
        )
        data$unresolved_dependencies <- list()
      }
      if (is.null(data$unresolved_dependencies)) {
        data$unresolved_dependencies <- list()
      }
      if (is.null(data$findings)) {
        data$findings <- list()
      } else if (is.list(data$findings)) {
        data$findings <- lapply(data$findings, function(f) {
          if (!is.list(f)) return(f)
          if (is.null(f$severity)) f$severity <- "medium"
          if (is.null(f$sas_evidence)) f$sas_evidence <- f$source_evidence %||% "(source statement)"
          if (is.null(f$r_evidence)) f$r_evidence <- f$code_evidence %||% "(r code)"
          if (is.null(f$affected_outputs)) f$affected_outputs <- list()
          if (is.null(f$confidence)) f$confidence <- 0.9
          if (is.null(f$unresolved_dependencies)) f$unresolved_dependencies <- list()
          f
        })
      }
      if (is.null(data$static_runnability)) {
        data$static_runnability <- if (identical(data$verdict, "reviewed_no_material_finding")) "looks_runnable" else "unknown"
      }
    } else if (identical(request$schema_name, "program_translation_v1")) {
      if (is.null(data$parameters)) data$parameters <- list()
      if (is.null(data$defaults)) data$defaults <- structure(list(), names = character(0))
      if (is.null(data$reads)) data$reads <- list()
      if (is.null(data$writes)) data$writes <- list()
      if (is.null(data$side_effects)) data$side_effects <- list()
      if (is.null(data$helper_use)) data$helper_use <- list()
      if (is.null(data$discovered_dependencies)) data$discovered_dependencies <- list()
      if (is.null(data$suspected_dependencies)) data$suspected_dependencies <- list()
      if (is.null(data$affected_outputs)) data$affected_outputs <- list()
      if (is.null(data$uncertainty)) data$uncertainty <- list()
    } else if (identical(request$schema_name, "program_fix_v1")) {
      if (is.null(data$evidence_ids)) data$evidence_ids <- list()
      if (is.null(data$changed_interfaces)) data$changed_interfaces <- list()
      if (is.null(data$affected_outputs)) data$affected_outputs <- list()
      if (is.null(data$remaining_uncertainty)) data$remaining_uncertainty <- list()
      if (!"bundle_helper_patch" %in% names(data) || is.null(data$bundle_helper_patch)) {
        data["bundle_helper_patch"] <- list(NULL)
      }
    } else if (identical(request$schema_name, "reviewer_output_v1") && is.null(data$status) && !is.null(data$verdict)) {
      status_val <- switch(data$verdict,
        reviewed_no_material_finding = "no_issue",
        repair_required = "issue_found",
        review_unavailable = "uncertain",
        "uncertain"
      )
      action_val <- attr(data, "mock_action", exact = TRUE) %||% switch(data$verdict,
        reviewed_no_material_finding = "keep",
        repair_required = "repair",
        review_unavailable = "human_review",
        "human_review"
      )
      issue_codes <- character(0)
      explanation_val <- attr(data, "mock_explanation", exact = TRUE) %||% data$explanation %||% ""
      conf_val <- attr(data, "mock_confidence", exact = TRUE) %||% 0.9
      if (length(data$findings) > 0L) {
        issue_codes <- unique(vapply(data$findings, function(f) f$r_evidence %||% f$severity %||% "material", character(1)))
        if (!nzchar(explanation_val)) {
          explanation_val <- paste(vapply(data$findings, function(f) f$sas_evidence %||% "", character(1)), collapse = "; ")
        }
        if (is.null(attr(data, "mock_confidence", exact = TRUE))) {
          conf_val <- max(vapply(data$findings, function(f) f$confidence %||% 0.9, numeric(1)))
        }
      }
      data <- list(
        status = status_val,
        issue_codes = as.list(issue_codes),
        explanation = explanation_val,
        recommended_action = action_val,
        confidence = conf_val
      )
    } else if (identical(request$schema_name, "translation") && is.null(data$assumptions) && !is.null(data$r_code)) {
      assump_src <- data$diagnosis %||% data$summary %||% "repaired logic"
      flags_src <- if (length(data$flags)) {
        data$flags
      } else if (length(data$remaining_uncertainty)) {
        data$remaining_uncertainty
      } else if (length(data$uncertainty) && is.list(data$uncertainty)) {
        vapply(data$uncertainty, function(u) if (is.list(u)) u$claim %||% "" else as.character(u), character(1))
      } else {
        list()
      }
      data <- list(
        r_code = data$r_code,
        assumptions = as.list(unlist(assump_src)),
        confidence = data$confidence %||% 0.9,
        flags = as.list(unlist(flags_src))
      )
    }
  }
  new_llm_response(
    status = raw$status %||% "completed",
    action = raw$action %||% "final",
    data = data,
    finish_reason = raw$finish_reason, response_id = raw$id,
    request = request, resolved_model = raw$model,
    usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider,
    conversation = raw$conversation
  )
}

normalize_openai_response <- function(raw, request, provider) {
  if (identical(raw$status, "incomplete")) {
    return(new_llm_response(
      status = "incomplete", action = "none",
      finish_reason = raw$incomplete_details$reason %||% "incomplete",
      response_id = raw$id, request = request,
      resolved_model = raw$model, usage = normalized_usage(raw),
      cost = normalized_cost(raw), provider = provider,
      extra_class = "sas2r_llm_incomplete"
    ))
  }
  output <- raw$output %||% list()
  for (item in output) {
    if (identical(item$type, "function_call")) {
      return(new_llm_response(
        status = "completed", action = "tool_call",
        tool_name = item$name,
        tool_arguments = strict_json_list(item$arguments) %||% item$arguments %||% list(),
        tool_call_id = item$call_id %||% item$id,
        finish_reason = item$status %||% raw$finish_reason,
        response_id = raw$id, request = request, resolved_model = raw$model,
        usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider
      ))
    }
  }
  for (item in rev(output)) {
    if (!identical(item$type, "message")) next
    content <- item$content %||% list()
    refusals <- Filter(function(part) identical(part$type, "refusal"), content)
    if (length(refusals)) {
      return(new_llm_response(
        status = "refused", action = "none", finish_reason = "refusal",
        response_id = raw$id, request = request, resolved_model = raw$model,
        usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider,
        extra_class = "sas2r_llm_refused"
      ))
    }
    for (part in rev(content)) {
      data <- part$json %||% part$parsed %||% part$data
      if (is.null(data) && identical(part$type, "output_text")) {
        data <- strict_json_list(part$text)
      }
      if (!is.null(data)) {
        return(new_llm_response(
          status = "completed", action = "final", data = data,
          finish_reason = raw$finish_reason %||% item$status,
          response_id = raw$id, request = request, resolved_model = raw$model,
          usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider
        ))
      }
    }
  }
  new_llm_response(
    status = "completed", action = "none", finish_reason = raw$finish_reason,
    response_id = raw$id, request = request, resolved_model = raw$model,
    usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider
  )
}

normalize_bedrock_response <- function(raw, request, provider) {
  terminal <- terminal_response_from_reason(
    raw$stopReason, raw, request, provider, resolved_model = raw$model
  )
  if (!is.null(terminal)) return(terminal)
  content <- raw$output$message$content %||% list()
  for (part in content) {
    tool <- part$toolUse
    if (!is.null(tool)) {
      return(new_llm_response(
        status = "completed", action = "tool_call", tool_name = tool$name,
        tool_arguments = tool$input %||% list(), tool_call_id = tool$toolUseId,
        finish_reason = raw$stopReason, request = request,
        resolved_model = raw$model, usage = normalized_usage(raw),
        cost = normalized_cost(raw), provider = provider
      ))
    }
  }
  data <- NULL
  for (part in rev(content)) {
    data <- part$json %||% strict_json_list(part$text)
    if (!is.null(data)) break
  }
  new_llm_response(
    status = "completed", action = if (is.null(data)) "none" else "final",
    data = data, finish_reason = raw$stopReason, request = request,
    resolved_model = raw$model, usage = normalized_usage(raw),
    cost = normalized_cost(raw), provider = provider
  )
}

normalize_vertex_response <- function(raw, request, provider) {
  candidate <- if (length(raw$candidates)) raw$candidates[[1]] else list()
  terminal <- terminal_response_from_reason(
    candidate$finishReason, raw, request, provider,
    resolved_model = raw$modelVersion %||% raw$model
  )
  if (!is.null(terminal)) return(terminal)
  parts <- candidate$content$parts %||% list()
  for (part in parts) {
    call <- part$functionCall
    if (!is.null(call)) {
      return(new_llm_response(
        status = "completed", action = "tool_call", tool_name = call$name,
        tool_arguments = call$args %||% list(), tool_call_id = call$id,
        finish_reason = candidate$finishReason, request = request,
        resolved_model = raw$modelVersion %||% raw$model,
        usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider
      ))
    }
  }
  data <- NULL
  for (part in rev(parts)) {
    data <- part$structured_output %||% part$json %||% strict_json_list(part$text)
    if (!is.null(data)) break
  }
  new_llm_response(
    status = "completed",
    action = if (is.null(data)) "none" else "final", data = data,
    finish_reason = candidate$finishReason, request = request,
    resolved_model = raw$modelVersion %||% raw$model,
    usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider
  )
}

normalize_ollama_response <- function(raw, request, provider) {
  calls <- raw$message$tool_calls %||% list()
  if (length(calls)) {
    call <- calls[[1]]$`function` %||% calls[[1]]
    return(new_llm_response(
      status = "completed", action = "tool_call", tool_name = call$name,
      tool_arguments = strict_json_list(call$arguments) %||% call$arguments %||% list(),
      tool_call_id = calls[[1]]$id, finish_reason = raw$done_reason,
      request = request, resolved_model = raw$model,
      usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider
    ))
  }
  data <- raw$message$structured_output %||% strict_json_list(raw$message$content)
  new_llm_response(
    status = if (isFALSE(raw$done)) "incomplete" else "completed",
    action = if (is.null(data)) "none" else "final", data = data,
    finish_reason = raw$done_reason, request = request, resolved_model = raw$model,
    usage = normalized_usage(raw), cost = normalized_cost(raw), provider = provider,
    extra_class = if (isFALSE(raw$done)) "sas2r_llm_incomplete" else NULL
  )
}

#' Normalize all provider and adapter outcomes to sas2r's transport contract
#' @noRd
normalize_provider_response <- function(raw, request = NULL, provider = NULL) {
  if (is.function(raw)) {
    raw <- raw(request)
  }
  if (inherits(raw, "sas2r_llm_response")) {
    if (is.null(raw$request_id) && inherits(request, "sas2r_llm_request")) {
      raw$request_id <- request$request_id
    }
    return(apply_terminal_finish(raw))
  }
  if (inherits(raw, "condition")) {
    class <- failure_class(raw)
    response <- new_llm_response(
      status = "failed", action = "none", request = request, provider = provider,
      usage = normalized_usage(raw), cost = normalized_cost(raw),
      error = normalized_llm_failure_metadata(raw, class),
      extra_class = class
    )
    if (is.null(recognized_llm_failure_reason(raw))) {
      attr(response, "sas2r_private_reason_synthesized") <- TRUE
    }
    return(response)
  }
  if (!is.list(raw)) {
    error <- simpleError("provider returned a non-list response")
    class(error) <- c("sas2r_llm_transport_error", class(error))
    return(normalize_provider_response(error, request = request, provider = provider))
  }
  if (!is.null(raw$error)) {
    error_payload <- raw$error
    message <- if (is.list(error_payload)) {
      error_payload$message %||% error_payload$detail %||% "provider error"
    } else error_payload
    error <- simpleError(as.character(message)[1])
    error$status_code <- if (is.list(error_payload)) {
      error_payload$status_code %||% error_payload$code
    } else NULL
    return(normalize_provider_response(error, request = request, provider = provider))
  }
  if (!is.null(raw$type)) return(normalize_legacy_response(raw, request, provider))
  if (!is.null(raw$output) && !is.null(raw$status)) {
    return(normalize_openai_response(raw, request, provider))
  }
  if (!is.null(raw$stopReason) || !is.null(raw$output$message)) {
    return(normalize_bedrock_response(raw, request, provider))
  }
  if (!is.null(raw$candidates)) return(normalize_vertex_response(raw, request, provider))
  if (!is.null(raw$message) && (!is.null(raw$done) || identical(provider, "ollama"))) {
    return(normalize_ollama_response(raw, request, provider))
  }
  if ((!is.null(raw$status) && !is.null(raw$action)) ||
      identical(raw$status, "refused") || identical(raw$status, "incomplete")) {
    return(normalize_legacy_response(raw, request, provider))
  }
  # Native structured methods return the schema object directly.
  normalize_legacy_response(
    structure(list(type = "final", data = raw),
              usage = attr(raw, "usage", exact = TRUE),
              cost_usd = attr(raw, "cost_usd", exact = TRUE)),
    request, provider
  )
}

is_schema_retryable_response <- function(response) {
  inherits(response, "sas2r_llm_response") &&
    identical(response$status, "completed") &&
    identical(response$action, "final")
}
