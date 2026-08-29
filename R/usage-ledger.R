USAGE_COST_STATES <- c(
  "billed_amount", "contract_estimate", "catalog_estimate",
  "incomplete_estimate", "unknown"
)

USAGE_TOKEN_FIELDS <- c(
  "input_tokens", "output_tokens", "cached_input_tokens",
  "cache_write_tokens", "reasoning_tokens"
)

USAGE_TOTAL_TOKEN_FIELDS <- c(
  "total_input_tokens", "total_output_tokens", "total_tokens"
)

usage_cache_read_rate <- function(total_input_tokens, cached_input_tokens) {
  total <- nonnegative_number_or_na(total_input_tokens)
  cached <- nonnegative_number_or_na(cached_input_tokens)
  if (is.na(total) || is.na(cached) || total <= 0 || cached > total) {
    return(NA_real_)
  }
  cached / total
}

usage_safe_request_failure_value <- function(value, allowed) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !value %in% allowed) return(NULL)
  value
}

usage_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

new_usage_run_id <- function() {
  paste0(
    "run_",
    substr(as.character(cli::hash_sha256(tempfile("sas2r-run-"))), 1L, 24L)
  )
}

# `usage_provenance` names the public token source the caller actually read;
# the adapter can fall back from the conversation tabulation to the per-turn
# records, so the audit string must never be hard-coded to one of them.
usage_from_ellmer <- function(cost, input = NA_real_, output = NA_real_,
                              ellmer_version = NULL,
                              usage_provenance =
                                ELLMER_TABULATED_TOKEN_PROVENANCE) {
  amount <- nonnegative_number_or_na(cost)
  provenance <- c(
    usage_provenance, if (is.na(amount)) NULL else ELLMER_COST_PROVENANCE
  )
  list(
    input_tokens = nonnegative_number_or_na(input),
    output_tokens = nonnegative_number_or_na(output),
    total_input_tokens = nonnegative_number_or_na(input),
    total_output_tokens = nonnegative_number_or_na(output),
    total_tokens = if (is.na(nonnegative_number_or_na(input)) ||
                       is.na(nonnegative_number_or_na(output))) {
      NA_real_
    } else {
      nonnegative_number_or_na(input) + nonnegative_number_or_na(output)
    },
    cost_status = if (is.na(amount)) "unknown" else "catalog_estimate",
    amount_usd = amount,
    currency = if (is.na(amount)) NULL else "USD",
    rate_source = if (is.na(amount)) NULL else "ellmer / LiteLLM catalog",
    source_version = if (is.na(amount)) NULL else ellmer_version,
    effective_date = NULL,
    rate_dimensions = NULL,
    raw_usage_provenance = if (!length(provenance)) {
      NULL
    } else {
      paste(provenance, collapse = " / ")
    }
  )
}

usage_limit <- function(value, name, integer = FALSE) {
  if (is.null(value) || identical(value, Inf)) return(Inf)
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || value < 0 ||
      (!is.finite(value) && !identical(value, Inf))) {
    cli::cli_abort(
      "usage limit {.field {name}} must be one non-negative number",
      class = "sas2r_budget_config_error"
    )
  }
  if (integer && is.finite(value)) value <- floor(value)
  value
}

normalize_usage_rates <- function(rates) {
  if (is.null(rates)) return(list())
  if (is.data.frame(rates)) {
    return(lapply(seq_len(nrow(rates)), function(i) as.list(rates[i, , drop = FALSE])))
  }
  if (!is.list(rates)) {
    cli::cli_abort("pricing rates must be a list or data frame",
                   class = "sas2r_budget_config_error")
  }
  if (length(rates) && !is.list(rates[[1]])) rates <- list(rates)
  rates
}

new_usage_budget <- function(mode = c("observe", "soft", "strict"),
                             max_usd = Inf, rates = NULL,
                             pricing_source = "adapter",
                             ledger_path = NULL, run_id = NULL,
                             resume = FALSE,
                             max_calls = Inf, max_retries = Inf,
                             max_tool_calls = Inf, max_wall_time = Inf,
                             max_request_bytes = Inf,
                             max_request_chars = Inf,
                             max_input_tokens = Inf,
                             max_output_tokens = Inf) {
  mode <- match.arg(mode)
  if (!is.character(pricing_source) || length(pricing_source) != 1L ||
      is.na(pricing_source) || !pricing_source %in% c(
        "adapter", "organization", "external"
      )) {
    cli::cli_abort(
      "pricing_source must be adapter, organization, or external",
      class = "sas2r_budget_config_error"
    )
  }
  budget <- new.env(parent = emptyenv())
  budget$mode <- mode
  budget$max_usd <- usage_limit(max_usd, "max_usd")
  budget$rates <- normalize_usage_rates(rates)
  budget$pricing_source <- pricing_source
  budget$ledger_path <- ledger_path
  budget$run_id <- run_id %||% new_usage_run_id()
  budget$start_time <- Sys.time()
  budget$max_calls <- usage_limit(max_calls, "max_calls", integer = TRUE)
  budget$max_retries <- usage_limit(max_retries, "max_retries", integer = TRUE)
  budget$max_tool_calls <- usage_limit(
    max_tool_calls, "max_tool_calls", integer = TRUE
  )
  budget$max_wall_time <- usage_limit(max_wall_time, "max_wall_time")
  budget$max_request_bytes <- usage_limit(
    max_request_bytes, "max_request_bytes", integer = TRUE
  )
  budget$max_request_chars <- usage_limit(
    max_request_chars, "max_request_chars", integer = TRUE
  )
  budget$max_input_tokens <- usage_limit(
    max_input_tokens, "max_input_tokens", integer = TRUE
  )
  budget$max_output_tokens <- usage_limit(
    max_output_tokens, "max_output_tokens", integer = TRUE
  )
  budget$known_amount <- 0
  budget$billed_amount <- 0
  budget$estimated_amount <- 0
  budget$unknown_count <- 0L
  budget$reserved_amount <- 0
  budget$reservations <- list()
  budget$request_count <- 0L
  budget$retry_count <- 0L
  budget$tool_request_count <- 0L
  budget$tool_count <- 0L
  budget$tool_completed_count <- 0L
  budget$tool_refused_count <- 0L
  budget$tool_failed_count <- 0L
  budget$input_tokens <- 0
  budget$output_tokens <- 0
  budget$cached_input_tokens <- 0
  budget$cache_write_tokens <- 0
  budget$reasoning_tokens <- 0
  budget$total_input_tokens <- 0
  budget$total_output_tokens <- 0
  budget$total_tokens <- 0
  budget$records <- list()
  budget$tool_events <- list()
  # Raw tool names may be needed transiently to reconcile a private pricing
  # table, but they must never enter the durable audit records. Keep them in a
  # process-local map keyed by the already-redacted tool event id.
  budget$tool_pricing_names <- new.env(parent = emptyenv())
  budget$active_redactor <- redact_llm_secrets
  budget$summary_written <- FALSE
  budget$remaining_usd <- budget$max_usd
  class(budget) <- c("sas2r_usage_budget", "environment")

  if (isTRUE(resume) && !is.null(ledger_path) && file.exists(ledger_path)) {
    reconstruct_usage_budget(budget, read_usage_records(ledger_path))
  }
  update_usage_remaining(budget)
  budget
}

load_usage_budget <- function(path, mode = "observe", max_usd = Inf,
                              rates = NULL, pricing_source = "adapter",
                              run_id = NULL, ...) {
  new_usage_budget(
    mode = mode, max_usd = max_usd, rates = rates,
    pricing_source = pricing_source, ledger_path = path,
    run_id = run_id, resume = TRUE, ...
  )
}

assert_usage_budget <- function(budget) {
  if (!inherits(budget, "sas2r_usage_budget")) {
    cli::cli_abort("expected a sas2r usage budget",
                   class = "sas2r_budget_config_error")
  }
  invisible(budget)
}

update_usage_remaining <- function(budget) {
  if (is.finite(budget$max_usd)) {
    budget$remaining_usd <- budget$max_usd - budget$known_amount -
      budget$reserved_amount
  } else {
    budget$remaining_usd <- Inf
  }
  invisible(budget$remaining_usd)
}

read_usage_records <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lapply(seq_along(lines), function(i) {
    tryCatch(
      jsonlite::fromJSON(lines[[i]], simplifyVector = FALSE),
      error = function(error) cli::cli_abort(
        "usage ledger {.file {path}} has invalid JSON at line {i}",
        class = "sas2r_usage_ledger_error", parent = error
      )
    )
  })
}

usage_scalar_column <- function(values) {
  scalar <- vapply(values, function(value) {
    is.null(value) || (is.atomic(value) && length(value) <= 1L)
  }, logical(1))
  if (!all(scalar)) return(I(values))
  present <- Filter(function(value) !is.null(value) && length(value), values)
  if (!length(present)) return(rep(NA, length(values)))
  if (any(vapply(present, is.character, logical(1)))) {
    return(vapply(values, function(value) {
      if (is.null(value) || !length(value)) NA_character_ else as.character(value)[1]
    }, character(1)))
  }
  if (any(vapply(present, is.numeric, logical(1)))) {
    return(vapply(values, function(value) {
      if (is.null(value) || !length(value)) NA_real_ else as.numeric(value)[1]
    }, numeric(1)))
  }
  vapply(values, function(value) {
    if (is.null(value) || !length(value)) NA else as.logical(value)[1]
  }, logical(1))
}

read_usage_ledger <- function(path) {
  records <- read_usage_records(path)
  if (!length(records)) return(data.frame())
  fields <- unique(unlist(lapply(records, names), use.names = FALSE))
  columns <- lapply(fields, function(field) {
    usage_scalar_column(lapply(records, function(record) record[[field]]))
  })
  names(columns) <- fields
  as.data.frame(columns, stringsAsFactors = FALSE, optional = TRUE)
}

sanitize_usage_metadata_strings <- function(value) {
  if (is.list(value)) return(lapply(value, sanitize_usage_metadata_strings))
  if (!is.character(value)) return(value)
  value <- gsub(
    "(?i)(https?://)[^/@[:space:]]+@", "\\1", value, perl = TRUE
  )
  value <- gsub(
    "(?i)(https?://[^?#[:space:]\\\"']+)[?#][^[:space:]\\\"']*",
    "\\1", value, perl = TRUE
  )
  credential <- paste0(
    "(?i)\\b(api[_-]?key|access[_-]?token|token|secret|password|",
    "credential)([\"']?[[:space:]]*[:=][[:space:]]*)"
  )
  value <- gsub(
    paste0(credential, "\"(?:\\\\.|[^\"\\\\])*\""),
    "\\1\\2\"[REDACTED]\"", value, perl = TRUE
  )
  value <- gsub(
    paste0(credential, "'(?:\\\\.|[^'\\\\])*'"),
    "\\1\\2'[REDACTED]'", value, perl = TRUE
  )
  gsub(
    paste0(credential, "(?![\"'])([^&[:space:],;]+)"),
    "\\1\\2[REDACTED]", value, perl = TRUE
  )
}

sanitize_usage_record <- function(record, redactor = redact_llm_secrets) {
  if (!is.function(redactor)) redactor <- redact_llm_secrets
  sanitized <- redactor(record)
  if (!is.list(sanitized)) {
    cli::cli_abort(
      "usage audit redactor must return a metadata record",
      class = "sas2r_llm_redaction_error"
    )
  }
  sanitize_usage_metadata_strings(sanitized)
}

append_usage_record <- function(budget, record, redactor = NULL) {
  assert_usage_budget(budget)
  record$schema_version <- record$schema_version %||% "usage-ledger-v1"
  record <- sanitize_usage_record(
    record, redactor %||% budget$active_redactor %||% redact_llm_secrets
  )
  path <- budget$ledger_path
  if (!is.null(path)) {
    json <- jsonlite::toJSON(
      record, auto_unbox = TRUE, null = "null", na = "null"
    )
    existing <- if (file.exists(path)) {
      readLines(path, warn = FALSE)
    } else character()
    atomic_write_file(
      function(file) writeLines(c(existing, json), file, useBytes = TRUE),
      path, pattern = "usage_"
    )
  }
  budget$records[[length(budget$records) + 1L]] <- record
  invisible(record)
}

last_usage_records_by_id <- function(records, record_type) {
  selected <- Filter(
    function(record) identical(record$record_type, record_type) &&
      is.character(record$request_id) && length(record$request_id) == 1L,
    records
  )
  if (!length(selected)) return(list())
  selected[!duplicated(vapply(selected, `[[`, "", "request_id"), fromLast = TRUE)]
}

reconstruct_usage_budget <- function(budget, records) {
  starts <- last_usage_records_by_id(records, "request_started")
  completed <- last_usage_records_by_id(records, "request_completed")
  completed_ids <- vapply(completed, `[[`, "", "request_id")
  budget$request_count <- length(starts)
  budget$retry_count <- sum(vapply(starts, function(record) {
    !is.null(record$retry_of) && nzchar(record$retry_of)
  }, logical(1)))
  for (record in completed) {
    amount <- nonnegative_number_or_na(
      record$per_call_amount %||% record$amount_usd
    )
    status <- record$cost_status %||% "unknown"
    if (status %in% USAGE_COST_STATES && !identical(status, "unknown") &&
        !is.na(amount)) {
      budget$known_amount <- budget$known_amount + amount
      if (identical(status, "billed_amount")) {
        budget$billed_amount <- budget$billed_amount + amount
      } else {
        budget$estimated_amount <- budget$estimated_amount + amount
      }
    } else {
      budget$unknown_count <- budget$unknown_count + 1L
    }
    for (field in c(USAGE_TOKEN_FIELDS, USAGE_TOTAL_TOKEN_FIELDS)) {
      value <- nonnegative_number_or_na(record[[field]])
      if (!is.na(value)) budget[[field]] <- budget[[field]] + value
    }
  }
  for (record in starts) {
    if (record$request_id %in% completed_ids) next
    amount <- nonnegative_number_or_na(record$reserved_amount)
    if (is.na(amount)) amount <- 0
    budget$reservations[[record$request_id]] <- list(
      request_id = record$request_id,
      amount = amount,
      cost_status = record$cost_status %||% "unknown",
      recovered = TRUE
    )
    budget$reserved_amount <- budget$reserved_amount + amount
  }
  tool_events <- Filter(function(record) {
    record$record_type %in% c(
      "tool_attempted", "tool_executed", "tool_refused", "tool_failed"
    ) &&
      is.character(record$tool_event_id) && length(record$tool_event_id) == 1L
  }, records)
  if (length(tool_events)) {
    event_ids <- vapply(tool_events, `[[`, "", "tool_event_id")
    # The attempt is durable before local execution begins; the later terminal
    # record, when present, is the authoritative state for that same event.
    tool_events <- tool_events[!duplicated(event_ids, fromLast = TRUE)]
  }
  budget$tool_events <- tool_events
  summary_tool_request_counts <- vapply(Filter(function(record) {
    identical(record$record_type, "run_summary")
  }, records), function(record) {
    value <- nonnegative_number_or_na(
      record$tool_request_count %||% record$tool_count
    )
    if (is.na(value)) 0 else value
  }, numeric(1))
  budget$tool_request_count <- as.integer(max(
    length(tool_events), summary_tool_request_counts, 0, na.rm = TRUE
  ))
  summary_tool_counts <- vapply(Filter(function(record) {
    identical(record$record_type, "run_summary")
  }, records), function(record) {
    value <- nonnegative_number_or_na(record$tool_count)
    if (is.na(value)) 0 else value
  }, numeric(1))
  admitted_events <- sum(!vapply(tool_events, function(record) {
    isFALSE(record$admitted %||% TRUE)
  }, logical(1)))
  budget$tool_count <- as.integer(max(
    admitted_events, summary_tool_counts, 0, na.rm = TRUE
  ))
  terminal_counts <- function(record_type, summary_field) {
    event_count <- sum(vapply(tool_events, function(record) {
      identical(record$record_type, record_type)
    }, logical(1)))
    summary_counts <- vapply(Filter(function(record) {
      identical(record$record_type, "run_summary")
    }, records), function(record) {
      value <- nonnegative_number_or_na(record[[summary_field]])
      if (is.na(value)) 0 else value
    }, numeric(1))
    as.integer(max(event_count, summary_counts, 0, na.rm = TRUE))
  }
  budget$tool_completed_count <- terminal_counts(
    "tool_executed", "tool_completed_count"
  )
  budget$tool_refused_count <- terminal_counts(
    "tool_refused", "tool_refused_count"
  )
  budget$tool_failed_count <- terminal_counts(
    "tool_failed", "tool_failed_count"
  )
  budget$records <- records
  budget$summary_written <- any(vapply(records, function(record) {
    identical(record$record_type, "run_summary") &&
      identical(record$run_id, budget$run_id)
  }, logical(1)))
  update_usage_remaining(budget)
  invisible(budget)
}

request_text_metrics <- function(request) {
  tool_contracts <- lapply(request$tools %||% list(), function(tool) {
    list(
      name = tool$name %||% NULL,
      description = tool$description %||% NULL,
      schema = tool$schema %||% NULL
    )
  })
  text <- jsonlite::toJSON(list(
    messages = request$messages,
    tools = tool_contracts,
    output_schema = request$output_schema,
    schema_name = request$schema_name,
    schema_version = request$schema_version,
    parameters = request$parameters,
    model = request$model
  ), auto_unbox = TRUE, null = "null", na = "null")
  list(
    chars = nchar(text, type = "chars"),
    bytes = nchar(text, type = "bytes"),
    conservative_input_tokens = nchar(text, type = "bytes")
  )
}

rate_match_value <- function(rate_value, actual) {
  if (is.null(rate_value)) return(TRUE)
  if (length(rate_value) != 1L || (!is.atomic(rate_value) && !is.null(rate_value))) {
    return(FALSE)
  }
  if (is.na(rate_value) || identical(rate_value, "*")) return(TRUE)
  identical(as.character(rate_value), as.character(actual))
}

usage_rate_for_request <- function(budget, request, audit_context = list()) {
  if (!length(budget$rates)) return(NULL)
  provider <- audit_context$provider %||% "unknown"
  model <- audit_context$resolved_model %||% request$model %||% "unknown"
  region <- audit_context$region %||% audit_context$endpoint_region %||% "*"
  tier <- audit_context$service_tier %||% request$tier %||% "frontier"
  matches <- Filter(function(rate) {
    rate_match_value(rate$provider, provider) &&
      rate_match_value(rate$resolved_model %||% rate$model, model) &&
      rate_match_value(rate$region, region) &&
      rate_match_value(rate$service_tier %||% rate$tier, tier)
  }, budget$rates)
  if (length(matches) == 1L) matches[[1]] else NULL
}

complete_rate_dimensions <- function(rate, request, audit_context) {
  numeric_dimensions <- c(
    "input_per_million", "output_per_million",
    "cached_input_per_million", "cache_write_per_million",
    "reasoning_per_million"
  )
  valid <- !is.null(rate) && all(vapply(numeric_dimensions, function(field) {
    value <- scalar_number_or_na(rate[[field]])
    !is.na(value) && value >= 0
  }, logical(1))) && all(vapply(
    c("currency", "source", "source_version", "effective_date"),
    function(field) {
      value <- rate[[field]]
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1)
  )) && identical(toupper(rate$currency), "USD")
  if (!valid) return(FALSE)
  tool_names <- vapply(request$tools %||% list(), function(tool) {
    tool$name %||% ""
  }, character(1))
  tool_names <- tool_names[nzchar(tool_names)]
  if (!length(tool_names)) return(TRUE)
  tool_rates <- rate$tool_rates
  max_tool_calls <- audit_context$max_tool_calls %||% Inf
  is.list(tool_rates) && all(tool_names %in% names(tool_rates)) &&
    all(vapply(tool_names, function(name) {
      value <- scalar_number_or_na(tool_rates[[name]])
      !is.na(value) && value >= 0
    }, logical(1))) && is.finite(max_tool_calls)
}

rate_reservation_amount <- function(rate, input_tokens, output_tokens,
                                    request, audit_context = list(),
                                    actual_usage = NULL) {
  if (is.null(actual_usage)) {
    usage <- list(
      input_tokens = input_tokens,
      output_tokens = output_tokens,
      cached_input_tokens = input_tokens,
      cache_write_tokens = input_tokens,
      reasoning_tokens = output_tokens
    )
  } else {
    usage <- lapply(USAGE_TOKEN_FIELDS, function(field) {
      value <- nonnegative_number_or_na(actual_usage[[field]])
      if (is.na(value)) 0 else value
    })
    names(usage) <- USAGE_TOKEN_FIELDS
  }
  amount <- (
    usage$input_tokens * rate$input_per_million +
      usage$output_tokens * rate$output_per_million +
      usage$cached_input_tokens * rate$cached_input_per_million +
      usage$cache_write_tokens * rate$cache_write_per_million +
      usage$reasoning_tokens * rate$reasoning_per_million
  ) / 1e6
  tool_names <- vapply(request$tools %||% list(), function(tool) {
    tool$name %||% ""
  }, character(1))
  tool_names <- tool_names[nzchar(tool_names)]
  if (length(tool_names)) {
    if (is.null(actual_usage)) {
      max_tool_calls <- audit_context$max_tool_calls %||% 0
      max_rate <- max(vapply(tool_names, function(name) {
        scalar_number_or_na(rate$tool_rates[[name]])
      }, numeric(1)))
      amount <- amount + max_tool_calls * max_rate
    } else {
      actual_tools <- audit_context$actual_tool_names %||% character()
      actual_tools <- actual_tools[actual_tools %in% names(rate$tool_rates)]
      if (length(actual_tools)) {
        amount <- amount + sum(vapply(actual_tools, function(name) {
          scalar_number_or_na(rate$tool_rates[[name]])
        }, numeric(1)))
      } else {
        explicit_charge <- nonnegative_number_or_na(actual_usage$tool_charges)
        if (!is.na(explicit_charge)) amount <- amount + explicit_charge
      }
    }
  }
  amount
}

usage_budget_condition <- function(class, message, reason) {
  structure(
    list(message = message, call = NULL, reason = reason),
    class = c(class, "sas2r_budget_error", "error", "condition")
  )
}

usage_reservation_quote <- function(budget, request, audit_context = list()) {
  assert_usage_budget(budget)
  metrics <- request_text_metrics(request)
  retry_of <- audit_context$retry_of %||% request$retry_of %||% NULL
  elapsed <- as.numeric(difftime(Sys.time(), budget$start_time, units = "secs"))
  deny <- function(class, message, reason) list(
    ok = FALSE, error = usage_budget_condition(class, message, reason)
  )
  if (budget$request_count >= budget$max_calls) {
    return(deny("sas2r_budget_exhausted", "LLM call ceiling reached", "calls"))
  }
  if (!is.null(retry_of) && budget$retry_count >= budget$max_retries) {
    return(deny("sas2r_budget_exhausted", "LLM retry ceiling reached", "retries"))
  }
  if (elapsed >= budget$max_wall_time) {
    return(deny("sas2r_budget_exhausted", "LLM wall-time ceiling reached", "wall_time"))
  }
  if (metrics$chars > budget$max_request_chars) {
    return(deny("sas2r_budget_exhausted", "LLM request character ceiling exceeded", "request_chars"))
  }
  if (metrics$bytes > budget$max_request_bytes) {
    return(deny("sas2r_budget_exhausted", "LLM request byte ceiling exceeded", "request_bytes"))
  }
  exact_input <- scalar_number_or_na(audit_context$exact_input_tokens)
  input_bound <- if (is.na(exact_input)) metrics$conservative_input_tokens else exact_input
  input_provenance <- if (is.na(exact_input)) {
    "conservative_utf8_byte_upper_bound"
  } else {
    "exact_provider_tokenizer"
  }
  if (input_bound > budget$max_input_tokens) {
    return(deny("sas2r_budget_exhausted", "LLM input ceiling exceeded", "input_tokens"))
  }
  output_bound <- scalar_number_or_na(request$parameters$max_output_tokens)
  if (is.na(output_bound)) output_bound <- budget$max_output_tokens
  if (is.finite(budget$max_output_tokens) && is.finite(output_bound) &&
      output_bound > budget$max_output_tokens) {
    return(deny("sas2r_budget_exhausted", "LLM output ceiling exceeded", "output_tokens"))
  }
  if (identical(budget$mode, "soft") && is.finite(budget$max_usd) &&
      budget$known_amount >= budget$max_usd) {
    return(deny("sas2r_budget_exhausted", "soft dollar threshold reached", "soft_dollars"))
  }
  rate <- usage_rate_for_request(budget, request, audit_context)
  amount <- 0
  cost_status <- "unknown"
  if (identical(budget$mode, "strict")) {
    if (identical(budget$pricing_source, "external") && is.null(rate)) {
      return(deny(
        "sas2r_budget_unmeterable",
        "strict dollar enforcement is unavailable for external pricing without organization rates",
        "external_pricing"
      ))
    }
    if (!is.finite(output_bound) || !complete_rate_dimensions(
      rate, request, audit_context
    )) {
      return(deny(
        "sas2r_budget_unmeterable",
        "strict mode requires a finite provider output ceiling and complete organization rates",
        "missing_rate_dimension"
      ))
    }
    amount <- rate_reservation_amount(
      rate, input_bound, output_bound, request, audit_context
    )
    if (!is.finite(amount) || amount > budget$remaining_usd) {
      return(deny(
        "sas2r_budget_exhausted",
        "strict worst-case reservation exceeds the remaining budget",
        "strict_dollars"
      ))
    }
    cost_status <- "contract_estimate"
  }
  list(
    ok = TRUE, amount = amount, cost_status = cost_status, rate = rate,
    input_bound = input_bound, input_provenance = input_provenance,
    output_bound = output_bound, metrics = metrics
  )
}

can_reserve_request <- function(budget, request, audit_context = list()) {
  isTRUE(usage_reservation_quote(budget, request, audit_context)$ok)
}

usage_audit_identity <- function(budget, request, audit_context = list(),
                                 record_type, status, attempt,
                                 reserved_amount = 0) {
  list(
    record_type = record_type,
    run_id = budget$run_id,
    request_id = request$request_id,
    parent_request_id = audit_context$parent_request_id %||%
      request$parent_request_id %||% NULL,
    retry_of = audit_context$retry_of %||% request$retry_of %||% NULL,
    timestamp = usage_timestamp(),
    provider = audit_context$provider %||% "unknown",
    endpoint = audit_context$endpoint %||% NULL,
    endpoint_region = audit_context$endpoint_region %||%
      audit_context$region %||% NULL,
    requested_model = request$model %||% audit_context$requested_model %||% NULL,
    resolved_model = audit_context$resolved_model %||% request$model %||% NULL,
    agent = audit_context$agent %||% "agent",
    role = audit_context$role %||% audit_context$agent %||% "agent",
    component_id = audit_context$component_id %||% NULL,
    revision_id = audit_context$revision_id %||% NULL,
    round = if (!is.null(audit_context$round)) as.integer(audit_context$round) else NULL,
    attempt_id = audit_context$attempt_id %||% NULL,
    prompt_hash = audit_context$prompt_hash %||% NULL,
    skill_hash = audit_context$skill_hash %||% NULL,
    # Which unit the spend belongs to. Absent for run-level requests such as
    # the connectivity probe, which are not about any one unit.
    unit_id = audit_context$unit_id %||% NULL,
    tier = request$tier %||% audit_context$tier %||% "frontier",
    service_tier = audit_context$service_tier %||% request$tier %||%
      audit_context$tier %||% "frontier",
    purpose = audit_context$purpose %||% request$phase %||% "request",
    attempt = as.integer(attempt),
    status = status,
    schema_name = request$schema_name,
    agent_schema_version = request$schema_version,
    capability_version = audit_context$capability_version %||% NULL,
    capability_hash = audit_context$capability_hash %||% NULL,
    timeout_scope = audit_context$timeout_scope %||% NULL,
    timeout_seconds = audit_context$timeout_seconds %||% NULL,
    transport_max_tries = audit_context$transport_max_tries %||% NULL,
    cost_status = "unknown",
    currency = NULL,
    rate_source = NULL,
    source_version = NULL,
    effective_date = NULL,
    rate_dimensions = NULL,
    per_call_amount = NA_real_,
    reserved_amount = reserved_amount,
    cumulative_amount = budget$known_amount,
    skill_provenance = audit_context$skill_provenance %||% NULL
  )
}

reserve_usage_request <- function(budget, request, audit_context = list()) {
  started_elapsed <- unname(proc.time()[["elapsed"]])
  quote <- usage_reservation_quote(budget, request, audit_context)
  if (!isTRUE(quote$ok)) stop(quote$error)
  attempt <- budget$request_count + 1L
  reservation <- list(
    request_id = request$request_id,
    amount = quote$amount,
    cost_status = quote$cost_status,
    rate = quote$rate,
    request = request,
    audit_context = audit_context,
    attempt = attempt,
    input_bound = quote$input_bound,
    input_provenance = quote$input_provenance,
    output_bound = quote$output_bound,
    started_elapsed = started_elapsed,
    tool_event_offset = length(budget$tool_events)
  )
  record <- usage_audit_identity(
    budget, request, audit_context, "request_started", "started", attempt,
    reserved_amount = quote$amount
  )
  record$cost_status <- quote$cost_status
  record$input_token_bound <- quote$input_bound
  record$input_token_provenance <- quote$input_provenance
  record$max_output_tokens <- if (is.finite(quote$output_bound)) {
    quote$output_bound
  } else NA_real_
  if (!is.null(quote$rate)) {
    record$currency <- quote$rate$currency
    record$rate_source <- quote$rate$source
    record$source_version <- quote$rate$source_version
    record$effective_date <- quote$rate$effective_date
    record$rate_dimensions <- names(quote$rate)
  }
  append_usage_record(
    budget, record, redactor = audit_context$.usage_redactor %||% NULL
  )
  budget$request_count <- budget$request_count + 1L
  if (!is.null(audit_context$retry_of %||% request$retry_of)) {
    budget$retry_count <- budget$retry_count + 1L
  }
  budget$reservations[[request$request_id]] <- reservation
  budget$reserved_amount <- budget$reserved_amount + quote$amount
  update_usage_remaining(budget)
  reservation
}

normalized_response_cost_record <- function(response) {
  cost <- response$cost %||% list()
  if (!is.list(cost)) cost <- list()
  amount <- nonnegative_number_or_na(cost$amount_usd)
  status <- cost$status
  if (is.null(status)) {
    status <- if (is.na(amount)) "unknown" else "catalog_estimate"
  }
  if (!is.character(status) || length(status) != 1L || is.na(status) ||
      !status %in% USAGE_COST_STATES) status <- "unknown"
  if (is.na(amount)) status <- "unknown"
  currency <- cost$currency %||% if (is.na(amount)) NULL else "USD"
  if (!is.na(amount) &&
      (!is.character(currency) || length(currency) != 1L ||
       is.na(currency) || !identical(toupper(currency), "USD"))) {
    status <- "unknown"
  }
  if (identical(status, "unknown")) amount <- NA_real_
  list(
    amount = amount, status = status,
    currency = if (identical(status, "unknown")) NULL else currency,
    rate_source = cost$rate_source %||% cost$source %||% NULL,
    source_version = cost$source_version %||% NULL,
    effective_date = cost$effective_date %||% NULL,
    rate_dimensions = cost$rate_dimensions %||% NULL,
    provenance = cost$provenance %||% cost$source %||% NULL
  )
}

reconcile_usage_request <- function(budget, reservation, response) {
  assert_usage_budget(budget)
  request_id <- reservation$request_id
  held <- budget$reservations[[request_id]]$amount %||% reservation$amount %||% 0
  usage <- response$usage %||% normalized_usage(response)
  usage$cache_write_tokens <- nonnegative_number_or_na(
    usage$cache_write_tokens %||% usage$cache_creation_input_tokens
  )
  cost <- normalized_response_cost_record(response)
  has_locked_rate <- complete_rate_dimensions(
    reservation$rate, reservation$request, reservation$audit_context
  )
  if (identical(budget$pricing_source, "external") && !has_locked_rate) {
    cost <- list(
      amount = NA_real_, status = "unknown", currency = NULL,
      rate_source = NULL, source_version = NULL, effective_date = NULL,
      rate_dimensions = NULL, provenance = NULL
    )
  }
  use_locked_rate <- has_locked_rate &&
    (!identical(cost$status, "billed_amount") ||
       identical(budget$pricing_source, "external")) &&
    (is.na(cost$amount) || identical(budget$mode, "strict") ||
       budget$pricing_source %in% c("organization", "external"))
  if (use_locked_rate) {
    actual_context <- reservation$audit_context
    first_tool_event <- reservation$tool_event_offset + 1L
    request_tool_events <- if (length(budget$tool_events) >= first_tool_event) {
      budget$tool_events[seq.int(first_tool_event, length(budget$tool_events))]
    } else list()
    actual_tool_names <- vapply(request_tool_events, function(event) {
      event_id <- event$tool_event_id %||% ""
      if (inherits(budget$tool_pricing_names, "environment") &&
          is.character(event_id) && length(event_id) == 1L &&
          !is.na(event_id) && nzchar(event_id)) {
        return(get0(
          event_id, envir = budget$tool_pricing_names, inherits = FALSE,
          ifnotfound = event$tool_name %||% ""
        ))
      }
      event$tool_name %||% ""
    }, character(1))
    actual_tool_names <- actual_tool_names[nzchar(actual_tool_names)]
    if (!length(actual_tool_names) &&
        identical(response$action, "tool_call") &&
        is.character(response$tool_name) && length(response$tool_name) == 1L &&
        !is.na(response$tool_name) && nzchar(response$tool_name)) {
      actual_tool_names <- response$tool_name
    }
    actual_context$actual_tool_names <- actual_tool_names
    cost$amount <- rate_reservation_amount(
      reservation$rate, reservation$input_bound, reservation$output_bound,
      reservation$request, actual_context,
      actual_usage = usage
    )
    input_accounting_status <- attr(
      usage, "input_accounting_status", exact = TRUE
    ) %||% "unavailable"
    total_accounting_status <- attr(
      usage, "total_accounting_status", exact = TRUE
    ) %||% "unavailable"
    complete_usage <- all(vapply(USAGE_TOKEN_FIELDS, function(field) {
      !is.na(nonnegative_number_or_na(usage[[field]]))
    }, logical(1))) &&
      !identical(input_accounting_status, "mismatch") &&
      !identical(total_accounting_status, "mismatch")
    cost$status <- if (complete_usage) "contract_estimate" else
      "incomplete_estimate"
    if (!complete_usage && identical(budget$mode, "strict")) {
      cost$amount <- max(cost$amount, held)
    }
    cost$currency <- reservation$rate$currency
    cost$rate_source <- reservation$rate$source
    cost$source_version <- reservation$rate$source_version
    cost$effective_date <- reservation$rate$effective_date
    cost$rate_dimensions <- names(reservation$rate)
    cost$provenance <- "organization pricing table"
  }
  known_increment <- 0
  billed_increment <- 0
  estimated_increment <- 0
  unknown_increment <- 0L
  if (!is.na(cost$amount) && !identical(cost$status, "unknown")) {
    known_increment <- cost$amount
    if (identical(cost$status, "billed_amount")) {
      billed_increment <- cost$amount
    } else {
      estimated_increment <- cost$amount
    }
  } else {
    unknown_increment <- 1L
  }
  aggregate_fields <- c(USAGE_TOKEN_FIELDS, USAGE_TOTAL_TOKEN_FIELDS)
  token_increments <- lapply(aggregate_fields, function(field) {
    value <- nonnegative_number_or_na(usage[[field]])
    if (is.na(value)) 0 else value
  })
  names(token_increments) <- aggregate_fields
  next_known_amount <- budget$known_amount + known_increment
  record <- usage_audit_identity(
    budget, reservation$request, reservation$audit_context,
    "request_completed", response$status %||% "failed",
    reservation$attempt, reserved_amount = held
  )
  record$resolved_model <- response$resolved_model %||% record$resolved_model
  record$input_tokens <- nonnegative_number_or_na(usage$input_tokens)
  record$output_tokens <- nonnegative_number_or_na(usage$output_tokens)
  record$total_input_tokens <- nonnegative_number_or_na(
    usage$total_input_tokens
  )
  record$total_output_tokens <- nonnegative_number_or_na(
    usage$total_output_tokens
  )
  record$total_tokens <- nonnegative_number_or_na(usage$total_tokens)
  record$cached_input_tokens <- nonnegative_number_or_na(usage$cached_input_tokens)
  record$cache_write_tokens <- nonnegative_number_or_na(usage$cache_write_tokens)
  record$reasoning_tokens <- nonnegative_number_or_na(usage$reasoning_tokens)
  record$tool_charges <- nonnegative_number_or_na(usage$tool_charges)
  record$cache_read_rate <- usage_cache_read_rate(
    usage$total_input_tokens, usage$cached_input_tokens
  )
  record$input_accounting_status <- attr(
    usage, "input_accounting_status", exact = TRUE
  ) %||% "unavailable"
  record$input_accounting_delta_tokens <- scalar_number_or_na(attr(
    usage, "input_accounting_delta_tokens", exact = TRUE
  ))
  record$total_accounting_status <- attr(
    usage, "total_accounting_status", exact = TRUE
  ) %||% "unavailable"
  record$total_accounting_delta_tokens <- scalar_number_or_na(attr(
    usage, "total_accounting_delta_tokens", exact = TRUE
  ))
  record$raw_usage_provenance <- attr(
    usage, "token_provenance", exact = TRUE
  ) %||% cost$provenance
  record$cost_provenance <- cost$provenance
  record$cost_status <- cost$status
  record$currency <- cost$currency
  record$rate_source <- cost$rate_source
  record$source_version <- cost$source_version
  record$effective_date <- cost$effective_date
  record$rate_dimensions <- cost$rate_dimensions
  record$per_call_amount <- cost$amount
  record$cumulative_amount <- next_known_amount
  duration_ms <- (
    unname(proc.time()[["elapsed"]]) - reservation$started_elapsed
  ) * 1000
  record$duration_ms <- max(0, duration_ms)
  if (identical(response$status, "failed")) {
    record["error_class"] <- list(usage_safe_request_failure_value(
      response$error$class %||% NULL, names(LLM_FAILURE_REASONS)
    ))
    allowed_reasons <- if (is.null(record$error_class)) {
      character()
    } else {
      LLM_FAILURE_REASONS[[record$error_class]]
    }
    record["error_reason"] <- list(usage_safe_request_failure_value(
      response$error$reason %||% NULL,
      allowed_reasons
    ))
  } else {
    record["error_class"] <- list(NULL)
    record["error_reason"] <- list(NULL)
  }
  record <- append_usage_record(
    budget, record,
    redactor = reservation$audit_context$.usage_redactor %||% NULL
  )

  budget$reserved_amount <- max(0, budget$reserved_amount - held)
  budget$reservations[[request_id]] <- NULL
  budget$known_amount <- next_known_amount
  budget$billed_amount <- budget$billed_amount + billed_increment
  budget$estimated_amount <- budget$estimated_amount + estimated_increment
  budget$unknown_count <- budget$unknown_count + unknown_increment
  for (field in aggregate_fields) {
    budget[[field]] <- budget[[field]] + token_increments[[field]]
  }
  update_usage_remaining(budget)
  invisible(record)
}

apply_reconciled_usage_cost <- function(value, record) {
  amount <- nonnegative_number_or_na(record$per_call_amount)
  status <- record$cost_status %||% "unknown"
  if (!is.character(status) || length(status) != 1L || is.na(status) ||
      !status %in% USAGE_COST_STATES || is.na(amount)) {
    status <- "unknown"
    amount <- NA_real_
  }
  cost <- list(
    amount_usd = amount,
    status = status,
    currency = if (identical(status, "unknown")) NULL else record$currency,
    source = record$rate_source %||% NULL,
    source_version = record$source_version %||% NULL,
    effective_date = record$effective_date %||% NULL,
    rate_dimensions = record$rate_dimensions %||% NULL,
    provenance = record$cost_provenance %||%
      record$raw_usage_provenance %||% NULL
  )
  if (inherits(value, "sas2r_llm_response")) {
    value$cost <- cost
    attr(value, "cost_usd") <- NULL
    if (!is.na(amount)) attr(value, "cost_usd") <- amount
    return(value)
  }
  for (attribute in c(
    "cost_usd", "cost_status", "cost_currency", "cost_source",
    "cost_source_version", "cost_effective_date", "cost_rate_dimensions",
    "cost_provenance"
  )) attr(value, attribute) <- NULL
  if (!is.na(amount)) attr(value, "cost_usd") <- amount
  attr(value, "cost_status") <- status
  if (!is.null(cost$currency)) attr(value, "cost_currency") <- cost$currency
  if (!is.null(cost$source)) attr(value, "cost_source") <- cost$source
  if (!is.null(cost$source_version)) {
    attr(value, "cost_source_version") <- cost$source_version
  }
  if (!is.null(cost$effective_date)) {
    attr(value, "cost_effective_date") <- cost$effective_date
  }
  if (!is.null(cost$rate_dimensions)) {
    attr(value, "cost_rate_dimensions") <- cost$rate_dimensions
  }
  if (!is.null(cost$provenance)) {
    attr(value, "cost_provenance") <- cost$provenance
  }
  value
}

budget_exhausted_response <- function(request, llm, error) {
  response <- new_llm_response(
    status = "budget_exhausted", action = "none", request = request,
    resolved_model = request$model %||% llm$model,
    provider = llm$provider, extra_class = class(error)[[1]]
  )
  attr(response, "budget_error") <- error
  response
}

prepare_budget_request <- function(request, budget) {
  if (is.finite(budget$max_output_tokens)) {
    requested <- scalar_number_or_na(request$parameters$max_output_tokens)
    if (is.na(requested)) {
      request$parameters$max_output_tokens <- as.integer(budget$max_output_tokens)
    }
  }
  request
}

effective_output_ceiling_error <- function(budget, request, params) {
  requires_output_enforcement <- identical(budget$mode, "strict") ||
    is.finite(budget$max_output_tokens)
  requested_ceiling <- nonnegative_number_or_na(
    request$parameters$max_output_tokens
  )
  if (!requires_output_enforcement || !is.finite(requested_ceiling)) {
    return(NULL)
  }
  effective_ceiling <- nonnegative_number_or_na(params$max_output_tokens)
  if (is.finite(effective_ceiling) && effective_ceiling <= requested_ceiling) {
    return(NULL)
  }
  usage_budget_condition(
    "sas2r_budget_unmeterable",
    "the effective transport parameters do not enforce the required output-token ceiling",
    "output_ceiling_unenforceable"
  )
}

with_usage_managed_request <- function(llm) {
  if (!inherits(llm, "sas2r_llm")) {
    cli::cli_abort("expected a sas2r_llm adapter",
                   class = "sas2r_llm_error")
  }
  attr(llm, "usage_managed_request") <- TRUE
  llm
}

#' Hand a request to an adapter, with the audit context alongside it
#'
#' The audit context carries a redactor closing over the auth-bearing adapter,
#' so it must never be stapled onto the request: the request travels to the
#' provider and comes back on the response, where any caller can print it. An
#' adapter that wants the context declares an `audit_context` argument and
#' receives a copy with the redactor stripped out; every other adapter is
#' called with the request alone, exactly as before.
#'
#' @param llm A `sas2r_llm` adapter.
#' @param request A `sas2r_llm_request`.
#' @param context The audit context for this attempt.
#' @return Whatever the adapter transport returns.
#' @noRd
call_llm_transport <- function(llm, request, context) {
  transport <- llm$request
  if (!"audit_context" %in% names(formals(transport))) {
    return(transport(request))
  }
  visible <- context[setdiff(names(context), ".usage_redactor")]
  transport(request, audit_context = visible)
}

# The per-attempt metering hook the managed branch interposes between an
# adapter's capability-retry loop and its transport. It is scoped to the call,
# not carried on the request: the closure holds the audit context and the
# redactor that closes over the auth-bearing adapter, and the request travels
# to the provider and comes back on the response where any caller can print it.
.usage_attempt_scope <- new.env(parent = emptyenv())
.usage_attempt_scope$callback <- NULL
.usage_attempt_scope$tool_audit_context <- NULL

with_usage_attempt_callback <- function(callback, expr) {
  previous <- .usage_attempt_scope$callback
  .usage_attempt_scope$callback <- callback
  on.exit(.usage_attempt_scope$callback <- previous, add = TRUE)
  expr
}

current_usage_attempt_callback <- function() {
  .usage_attempt_scope$callback
}

with_usage_tool_audit_context <- function(context, expr) {
  previous <- .usage_attempt_scope$tool_audit_context
  .usage_attempt_scope$tool_audit_context <- context
  on.exit(.usage_attempt_scope$tool_audit_context <- previous, add = TRUE)
  expr
}

current_usage_tool_audit_context <- function() {
  .usage_attempt_scope$tool_audit_context
}

attempt_usage_transport <- function(request, provider, usage_budget,
                                    audit_context, transport) {
  reservation <- reserve_usage_request(
    usage_budget, request, audit_context
  )
  raw <- tryCatch(transport(), error = identity)
  response <- normalize_provider_response(
    raw, request = request, provider = provider
  )
  reconciliation <- reconcile_usage_request(
    usage_budget, reservation, response
  )
  response <- apply_reconciled_usage_cost(response, reconciliation)
  if (inherits(raw, "condition")) {
    stop(apply_reconciled_usage_cost(raw, reconciliation))
  }
  response
}

attempt_llm_request <- function(request, llm, usage_budget = NULL,
                                audit_context = list()) {
  if (!inherits(request, "sas2r_llm_request")) {
    cli::cli_abort("attempt_llm_request requires a sas2r_llm_request",
                   class = "sas2r_llm_request_error")
  }
  if (is.null(usage_budget)) usage_budget <- new_usage_budget()
  assert_usage_budget(usage_budget)
  request <- prepare_budget_request(request, usage_budget)
  adapter_identity <- attr(llm, "auth_context", exact = TRUE)
  if (!is.list(adapter_identity)) adapter_identity <- list()
  request_redactor <- llm_audit_redactor(llm)
  usage_budget$active_redactor <- request_redactor
  context <- utils::modifyList(list(
    provider = llm$provider %||% "unknown",
    endpoint = adapter_identity$endpoint %||% adapter_identity$base_url %||%
      llm$endpoint,
    endpoint_region = adapter_identity$region %||%
      adapter_identity$location %||% NULL,
    requested_model = request$model %||% llm$model,
    resolved_model = request$model %||% llm$model,
    tier = request$tier,
    .usage_redactor = request_redactor
  ), audit_context)
  context[intersect(names(context), LLM_REQUEST_POLICY_FIELDS)] <- NULL
  context <- utils::modifyList(
    context, llm$request_policy %||% list()
  )
  requires_output_enforcement <- identical(usage_budget$mode, "strict") ||
    is.finite(usage_budget$max_output_tokens)
  if (requires_output_enforcement &&
      is.finite(scalar_number_or_na(request$parameters$max_output_tokens))) {
    capabilities <- tryCatch(
      llm_capabilities_for(llm, tier = request$tier, model = request$model),
      error = function(error) NULL
    )
    if (is.null(capabilities) ||
        !identical(capabilities$max_output_tokens, "supported")) {
      error <- usage_budget_condition(
        "sas2r_budget_unmeterable",
        "the configured policy requires a provider-enforced maximum output-token ceiling",
        "output_ceiling_unenforceable"
      )
      return(budget_exhausted_response(request, llm, error))
    }
  }
  if (isTRUE(attr(llm, "usage_managed_request", exact = TRUE))) {
    logical_request <- request
    subattempt <- 0L
    # Never stapled onto the request. This closure holds the audit context --
    # the redactor that closes over the auth-bearing adapter, the purpose, the
    # endpoint -- so putting it in a request slot would carry all of that to
    # the provider and back out on the response, which is exactly what
    # dropping `request$audit_context` was meant to prevent. The managed
    # branch is the path every shipped adapter takes (`ellmer_llm()` ends in
    # `with_usage_managed_request()`), so it is the branch that matters.
    attempt_callback <- function(
        attempted_request, params, transport, retry_of = NULL) {
      subattempt <<- subattempt + 1L
      subrequest <- attempted_request
      effective_error <- effective_output_ceiling_error(
        usage_budget, subrequest, params
      )
      if (inherits(effective_error, "condition")) stop(effective_error)
      subcontext <- context
      if (subattempt > 1L) {
        subrequest$request_id <- new_request_id()
        subrequest$parent_request_id <- logical_request$request_id
        subrequest$retry_of <- retry_of %||% logical_request$request_id
        subcontext$parent_request_id <- logical_request$request_id
        subcontext$retry_of <- retry_of %||% logical_request$request_id
      }
      subcontext$request_id <- subrequest$request_id
      subcontext$parent_request_id <- subrequest$parent_request_id %||%
        subcontext$parent_request_id
      attempt_usage_transport(
        subrequest, llm$provider, usage_budget, subcontext,
        function() with_usage_tool_audit_context(
          subcontext, transport(subrequest, params)
        )
      )
    }
    raw <- with_usage_attempt_callback(
      attempt_callback,
      tryCatch(call_llm_transport(llm, request, context), error = identity)
    )
    response <- normalize_provider_response(
      raw, request = logical_request, provider = llm$provider
    )
    budget_class <- response$error$class %||% ""
    if (budget_class %in% c(
      "sas2r_budget_exhausted", "sas2r_budget_unmeterable"
    )) {
      error <- usage_budget_condition(
        budget_class, response$error$message %||% "request budget denied",
        response$error$reason %||% "request_denied"
      )
      return(budget_exhausted_response(logical_request, llm, error))
    }
    return(response)
  }
  reservation <- tryCatch(
    reserve_usage_request(usage_budget, request, context),
    sas2r_budget_error = identity
  )
  if (inherits(reservation, "sas2r_budget_error")) {
    return(budget_exhausted_response(request, llm, reservation))
  }
  transport_context <- utils::modifyList(context, list(
    request_id = request$request_id,
    parent_request_id = request$parent_request_id %||%
      context$parent_request_id
  ))
  raw <- tryCatch(
    with_usage_tool_audit_context(
      transport_context, call_llm_transport(llm, request, context)
    ),
    error = identity
  )
  response <- normalize_provider_response(
    raw, request = request, provider = llm$provider
  )
  reconciliation <- reconcile_usage_request(
    usage_budget, reservation, response
  )
  apply_reconciled_usage_cost(response, reconciliation)
}

#' A bounded, single-line rendering of what a tool was asked for
#'
#' The ledger recorded that a tool ran but never its subject, so a run could
#' not distinguish a lookup that found what it needed from one searching
#' blindly. Arguments are model-authored and unbounded, so the rendering is
#' capped: the point is to identify the request, not to reproduce it.
#' @noRd
USAGE_TOOL_IDENTIFIER_ARGUMENTS <- list(
  query_project_graph = "dataset",
  lookup_rulebook = c("name", "functions", "procs"),
  find_macro = "name",
  get_macro_source = "name",
  search_docs = c("construct", "package"),
  read_skill = "name",
  read_comparison_report = "report_id"
)

usage_safe_identifier <- function(value, max_values = 12L,
                                  max_chars = 128L) {
  is.character(value) && length(value) <= max_values &&
    !anyNA(value) && all(nchar(value, type = "chars") <= max_chars) &&
    all(grepl("^[A-Za-z_][A-Za-z0-9_.:-]*$", value))
}

usage_value_fingerprint <- function(value) {
  serialized <- tryCatch(
    jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", force = TRUE),
    error = function(error) paste(class(value), length(value), sep = ":")
  )
  list(
    value_count = length(value),
    sha256 = as.character(cli::hash_sha256(as.character(serialized)[1L]))
  )
}

usage_safe_field_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    name <- "<missing-field-name>"
  }
  paste0("field_sha256_", substr(
    as.character(cli::hash_sha256(as.character(name)[1L])), 1L, 16L
  ))
}

usage_safe_tool_name <- function(tool_name) {
  if (is.null(tool_name)) return(NULL)
  tool_name <- as.character(tool_name)[1L]
  if (is.na(tool_name)) tool_name <- "<missing-tool-name>"
  known_tools <- tryCatch(names(TOOL_ARGUMENT_SCHEMAS), error = function(error) {
    character()
  })
  if (tool_name %in% known_tools) {
    return(tool_name)
  }
  paste0("tool_sha256_", substr(
    as.character(cli::hash_sha256(tool_name)), 1L, 16L
  ))
}

USAGE_SAFE_RESULT_STATUSES <- c(
  "agent_tool_limit", "backend_unavailable", "budget_exhausted",
  "empty_query", "global_tool_limit", "no_evidence", "not_in_index",
  "policy_refused", "report_not_registered", "report_path_rejected",
  "search_docs_disabled", "skill_not_registered", "tool_budget_hard_stop",
  "unknown_tool"
)

USAGE_SAFE_ERROR_CLASSES <- c(
  "simpleError", "sas2r_agent_tool_limit", "sas2r_budget_error",
  "sas2r_budget_exhausted", "sas2r_tool_arguments_error",
  "sas2r_tool_budget_error"
)

usage_safe_terminal_value <- function(value, allowed, prefix) {
  if (is.null(value)) return(NULL)
  value <- as.character(value)[1L]
  if (!is.na(value) && value %in% allowed) return(value)
  if (is.na(value)) value <- "<missing-terminal-value>"
  paste0(prefix, "_sha256_", substr(
    as.character(cli::hash_sha256(value)), 1L, 16L
  ))
}

usage_tool_argument_digest <- function(arguments, tool_name = NULL,
                                       limit = 300L) {
  if (is.null(arguments) || !length(arguments)) return(NULL)
  if (!is.list(arguments)) {
    projected <- list(
      argument_count = length(arguments),
      arguments = usage_value_fingerprint(arguments)
    )
  } else {
    argument_names <- names(arguments) %||% rep("", length(arguments))
    tool_key <- if (is.character(tool_name) && length(tool_name) == 1L &&
                    !is.na(tool_name)) tool_name else ""
    allowed <- if (tool_key %in% names(USAGE_TOOL_IDENTIFIER_ARGUMENTS)) {
      USAGE_TOOL_IDENTIFIER_ARGUMENTS[[tool_key]]
    } else {
      character()
    }
    projected <- list(argument_count = length(arguments))
    for (field in intersect(argument_names, allowed)) {
      value <- arguments[[field]]
      if (usage_safe_identifier(value)) {
        projected[[field]] <- value
      } else {
        projected[[paste0(field, "_fingerprint")]] <-
          usage_value_fingerprint(value)
      }
    }
    withheld <- setdiff(seq_along(arguments), match(allowed, argument_names))
    withheld <- withheld[!is.na(withheld)]
    if (length(withheld)) {
      fingerprints <- lapply(withheld, function(index) {
        usage_value_fingerprint(arguments[[index]])
      })
      names(fingerprints) <- vapply(withheld, function(index) {
        usage_safe_field_name(argument_names[[index]])
      }, character(1))
      projected$argument_fingerprints <- fingerprints
    }
  }
  text <- tryCatch(
    jsonlite::toJSON(projected, auto_unbox = TRUE, null = "null", force = TRUE),
    error = function(error) NULL
  )
  if (is.null(text)) return(NULL)
  text <- as.character(text)[1]
  if (nchar(text) <= limit) return(text)
  paste0(substr(text, 1L, limit), "...<truncated>")
}

begin_usage_tool_call <- function(budget, tool_name = NULL,
                                  arguments = NULL,
                                  audit_context = list(), admitted = TRUE) {
  if (is.null(budget)) return(invisible(NULL))
  assert_usage_budget(budget)
  next_request_count <- budget$tool_request_count + 1L
  next_count <- budget$tool_count + as.integer(isTRUE(admitted))
  event_id <- paste0("tool_", new_request_id())
  started_at <- Sys.time()
  pricing_tool_name <- if (is.character(tool_name) && length(tool_name) == 1L &&
                           !is.na(tool_name) && nzchar(tool_name)) {
    tool_name
  } else {
    NULL
  }
  event <- append_usage_record(budget, list(
    record_type = "tool_attempted", run_id = budget$run_id,
    tool_event_id = event_id,
    request_id = audit_context$request_id %||% NULL,
    parent_request_id = audit_context$parent_request_id %||% NULL,
    timestamp = usage_timestamp(started_at),
    tool_name = usage_safe_tool_name(tool_name),
    tool_arguments = usage_tool_argument_digest(arguments, tool_name),
    tool_request_count = next_request_count,
    tool_count = next_count,
    admitted = isTRUE(admitted),
    provider = audit_context$provider %||% NULL,
    requested_model = audit_context$requested_model %||% NULL,
    resolved_model = audit_context$resolved_model %||% NULL,
    agent = audit_context$agent %||% NULL,
    unit_id = audit_context$unit_id %||% NULL,
    tier = audit_context$tier %||% NULL,
    purpose = audit_context$purpose %||% NULL,
    phase = audit_context$phase %||% NULL,
    status = "attempted"
  ))
  budget$tool_request_count <- next_request_count
  budget$tool_count <- next_count
  budget$tool_events[[length(budget$tool_events) + 1L]] <- event
  if (!is.null(pricing_tool_name)) {
    assign(event_id, pricing_tool_name, envir = budget$tool_pricing_names)
  }
  invisible(list(
    tool_event_id = event_id,
    tool_request_count = next_request_count,
    tool_count = next_count,
    started_at = started_at,
    event = event
  ))
}

refuse_usage_tool_call <- function(budget, tool_name = NULL,
                                   arguments = NULL,
                                   audit_context = list(),
                                   result_status = "policy_refused",
                                   error_class = NULL) {
  reservation <- begin_usage_tool_call(
    budget, tool_name = tool_name, arguments = arguments,
    audit_context = audit_context, admitted = FALSE
  )
  complete_usage_tool_call(
    budget, reservation, outcome = "refused",
    result_status = result_status, error_class = error_class
  )
  invisible(reservation)
}

reserve_usage_tool_call <- function(budget, tool_name = NULL,
                                    arguments = NULL,
                                    audit_context = list()) {
  if (is.null(budget)) return(invisible(NULL))
  assert_usage_budget(budget)
  if (budget$tool_count >= budget$max_tool_calls) {
    refuse_usage_tool_call(
      budget, tool_name = tool_name, arguments = arguments,
      audit_context = audit_context, result_status = "global_tool_limit",
      error_class = "sas2r_budget_exhausted"
    )
    stop(usage_budget_condition(
      "sas2r_budget_exhausted", "LLM tool-call ceiling reached", "tool_calls"
    ))
  }
  begin_usage_tool_call(
    budget, tool_name = tool_name, arguments = arguments,
    audit_context = audit_context, admitted = TRUE
  )
}

complete_usage_tool_call <- function(budget, reservation,
                                     outcome = c("completed", "refused", "failed"),
                                     result_status = NULL,
                                     error_class = NULL) {
  if (is.null(budget) || is.null(reservation)) return(invisible(NULL))
  assert_usage_budget(budget)
  outcome <- match.arg(outcome)
  event_id <- reservation$tool_event_id
  already_done <- any(vapply(budget$records, function(record) {
    identical(record$tool_event_id %||% NULL, event_id) &&
      record$record_type %in% c("tool_executed", "tool_refused", "tool_failed")
  }, logical(1)))
  if (already_done) return(invisible(NULL))

  terminal_type <- switch(
    outcome,
    completed = "tool_executed",
    refused = "tool_refused",
    failed = "tool_failed"
  )
  completed_at <- Sys.time()
  event <- reservation$event
  event$record_type <- terminal_type
  event$timestamp <- usage_timestamp(completed_at)
  event$status <- outcome
  event$outcome <- outcome
  event$result_status <- usage_safe_terminal_value(
    result_status, USAGE_SAFE_RESULT_STATUSES, "result"
  )
  event$error_class <- usage_safe_terminal_value(
    error_class, USAGE_SAFE_ERROR_CLASSES, "error"
  )
  event$duration_ms <- as.numeric(difftime(
    completed_at, reservation$started_at, units = "secs"
  )) * 1000
  event <- append_usage_record(budget, event)

  matched <- which(vapply(budget$tool_events, function(candidate) {
    identical(candidate$tool_event_id %||% NULL, event_id)
  }, logical(1)))
  if (length(matched)) budget$tool_events[[matched[[1L]]]] <- event
  counter <- paste0("tool_", outcome, "_count")
  budget[[counter]] <- (budget[[counter]] %||% 0L) + 1L
  invisible(event)
}

usage_budget_allows_future <- function(budget) {
  if (is.null(budget)) return(TRUE)
  assert_usage_budget(budget)
  elapsed <- as.numeric(difftime(Sys.time(), budget$start_time, units = "secs"))
  if (budget$request_count >= budget$max_calls ||
      elapsed >= budget$max_wall_time) return(FALSE)
  if (identical(budget$mode, "soft") && is.finite(budget$max_usd) &&
      budget$known_amount >= budget$max_usd) return(FALSE)
  if (identical(budget$mode, "strict") && is.finite(budget$max_usd) &&
      budget$remaining_usd <= 0) return(FALSE)
  TRUE
}

finalize_usage_run <- function(budget, terminal_status = "completed") {
  assert_usage_budget(budget)
  if (isTRUE(budget$summary_written)) return(invisible(NULL))
  existing <- c(
    budget$records,
    if (!is.null(budget$ledger_path) && file.exists(budget$ledger_path)) {
      read_usage_records(budget$ledger_path)
    } else list()
  )
  if (any(vapply(existing, function(record) {
    identical(record$record_type, "run_summary") &&
      identical(record$run_id, budget$run_id)
  }, logical(1)))) {
    budget$summary_written <- TRUE
    return(invisible(NULL))
  }
  record <- list(
    record_type = "run_summary", schema_version = "usage-ledger-v1",
    run_id = budget$run_id, timestamp = usage_timestamp(),
    request_count = budget$request_count,
    retry_count = budget$retry_count,
    tool_request_count = budget$tool_request_count,
    tool_count = budget$tool_count,
    tool_completed_count = budget$tool_completed_count,
    tool_refused_count = budget$tool_refused_count,
    tool_failed_count = budget$tool_failed_count,
    input_tokens = budget$input_tokens,
    output_tokens = budget$output_tokens,
    total_input_tokens = budget$total_input_tokens,
    total_output_tokens = budget$total_output_tokens,
    total_tokens = budget$total_tokens,
    cached_input_tokens = budget$cached_input_tokens,
    cache_write_tokens = budget$cache_write_tokens,
    reasoning_tokens = budget$reasoning_tokens,
    cache_read_rate = usage_cache_read_rate(
      budget$total_input_tokens, budget$cached_input_tokens
    ),
    billed_amount = budget$billed_amount,
    estimated_amount = budget$estimated_amount,
    unknown_cost_count = budget$unknown_count,
    remaining_reservations = budget$reserved_amount,
    cumulative_amount = budget$known_amount,
    cost_unknown = budget$unknown_count > 0L || budget$reserved_amount > 0,
    terminal_status = terminal_status,
    finished_at = usage_timestamp()
  )
  append_usage_record(budget, record)
  budget$summary_written <- TRUE
  invisible(record)
}

run_with_budget <- function(llm, mode = "observe", max_usd = Inf,
                            request = NULL, ...) {
  budget <- new_usage_budget(mode = mode, max_usd = max_usd, ...)
  on.exit(finalize_usage_run(budget, "failed"), add = TRUE)
  request <- request %||% llm_request(
    messages = list(list(role = "user", content = "ping")),
    max_output_tokens = 1L
  )
  response <- attempt_llm_request(
    request, llm, budget, list(purpose = "budget_check")
  )
  error <- attr(response, "budget_error", exact = TRUE)
  if (inherits(error, "condition")) stop(error)
  finalize_usage_run(budget, response$status %||% "completed")
  response
}
