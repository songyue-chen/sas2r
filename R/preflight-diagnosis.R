# One bounded advisory request. Static findings and execution gates are never
# inputs to a model-authored mutation.
preflight_diagnosis_context <- function(check, config, error = NULL, limit = 30000L) {
  header <- list(sas2r_version = as.character(utils::packageVersion("sas2r")),
    execution_order = config$migration$execution_order,
    status = check$status %||% "inspection_failed",
    error = if (!is.null(error)) list(class = class(error), message = conditionMessage(error)),
    findings = check$findings, inputs = check$inputs,
    warnings = check$readiness$warnings, pipeline = check$pipeline)
  text <- jsonlite::toJSON(header, auto_unbox = TRUE, null = "null", na = "null")
  truncated <- nchar(text) > limit %/% 2L
  text <- substr(text, 1L, limit %/% 2L)
  statements <- check$project$statements
  if (!is.null(statements) && nrow(statements)) {
    affected <- unique(check$inputs$file[check$inputs$status != "generated"])
    files <- unique(c(affected, statements$file))
    for (file in files) {
      rows <- statements[statements$file == file, ]
      block <- paste0("\nSource: ", file, "\n",
        paste(paste0(rows$line_start, ": ", rows$text, ";"), collapse = "\n"))
      remaining <- limit - nchar(text)
      if (nchar(block) > remaining) {
        text <- paste0(text, substr(block, 1L, remaining))
        truncated <- TRUE
        break
      }
      text <- paste0(text, block)
    }
  }
  list(text = paste0(text, if (truncated) "\n[Context truncated; omitted source/facts remain unknown.]"),
       truncated = truncated)
}

preflight_diagnosis <- function(check = NULL, config = NULL, budget = NULL,
                                llm = NULL, diagnose = "auto", error = NULL) {
  result <- list(status = "unavailable", reason = NULL, advisory = NULL,
    issue_url = NULL, model = NULL, provider = NULL, context_truncated = FALSE,
    model_calls = 0L, usage = NULL, finish_reason = NULL, response_status = NULL,
    max_output_tokens = NULL)
  if (diagnose == "off") {
    result$status <- "disabled"
    result$reason <- "AI diagnosis disabled; static inspection only."
    return(result)
  }
  if (is.null(llm) && is.null(config) && !is.null(error)) {
    result$reason <- "Preflight failed before LLM configuration could be resolved."
    return(result)
  }
  if (is.null(llm) && is.null(config$llm)) {
    result$status <- "not_configured"
    result$reason <- "AI diagnosis unavailable: no LLM configured."
    return(result)
  }
  if (is.null(error) && identical(check$status, "ready_for_translation") &&
      !nrow(check$unsupported) && !length(check$readiness$warnings)) {
    result$status <- "not_needed"
    result$reason <- "No actionable preflight findings; no model call."
    return(result)
  }
  if (is.null(budget)) {
    result$reason <- "Preflight failed before a valid request budget was available."
    return(result)
  }
  redact <- if (is.null(llm)) new_llm_audit_redactor(llm_config_secret_values(config$llm)) else llm_audit_redactor(llm)
  tryCatch({
    # The adapter's transport settings are captured at construction. Rebuild
    # configured adapters with a single attempt; arbitrary supplied adapters
    # retain their transport contract and the shared admission ceiling.
    settings <- if (is.null(llm)) config$llm else attr(llm, "parallel_config", exact = TRUE)
    if (!is.null(settings)) {
      settings$max_tries <- 1L
      settings$timeout_seconds <- min(settings$timeout_seconds %||% 120, 120)
      llm <- sas_llm(settings)
    }
    if (!inherits(llm, "sas2r_llm")) stop("AI diagnosis requires a sas2r_llm adapter.")
    redact <- llm_audit_redactor(llm)
    result$model <- llm$model
    result$provider <- llm$provider
    budget$max_calls <- min(budget$max_calls, 1L)
    budget$max_retries <- 0L
    packet <- preflight_diagnosis_context(check, config, error)
    result$context_truncated <- packet$truncated
    schema <- jsonlite::read_json(system.file("schemas", "preflight-diagnosis-v1.json", package = "sas2r"))
    prompt <- paste(readLines(system.file("prompts", "preflight-diagnosis.md", package = "sas2r"),
                              warn = FALSE), collapse = "\n")
    capabilities <- llm_capabilities_for(llm)
    result$max_output_tokens <- llm$model_parameters$max_output_tokens %||% 4096L
    request <- llm_request(
      messages = list(list(role = "system", content = prompt),
        list(role = "user", content = redact(packet$text))),
      output_schema = schema, schema_name = "preflight_diagnosis_v1", schema_version = "1",
      schema_mode = if (identical(capabilities$structured_output, "native")) "native" else "fallback",
      max_output_tokens = result$max_output_tokens,
      reasoning_effort = llm$model_parameters$reasoning_effort,
      temperature = llm$model_parameters$temperature, top_p = llm$model_parameters$top_p)
    cli::cli_inform("AI diagnosis may send bounded SAS statements, source paths and findings to {llm$provider}, subject to budget admission; no dataset contents. Use diagnose = \"off\" to disable.")
    response <- attempt_llm_request(request, llm, usage_budget = budget,
      audit_context = list(purpose = "preflight_diagnosis", agent = "preflight_diagnosis"))
    result$finish_reason <- redact(response$finish_reason)
    result$response_status <- response$status
    if (!identical(response$status, "completed") || !identical(response$action, "final")) {
      result$reason <- redact(response$error$message %||% response$reason %||%
        paste("Diagnosis unavailable:", response$status))
    } else {
      errors <- validate_schema_value(response$data, schema)
      if (length(errors)) result$reason <- paste("Invalid diagnosis response:", paste(errors, collapse = "; "))
      else {
        result$status <- "completed"
        result$advisory <- redact(response$data)
        if (any(vapply(response$data$findings, function(f) f$classification == "suspected_sas2r_bug", logical(1))))
          result$issue_url <- "https://github.com/songyue-chen/sas2r/issues/new"
      }
    }
    result$model <- response$resolved_model %||% result$model
  }, error = function(e) {
    result$reason <<- redact(conditionMessage(e))
  })
  result$usage <- migration_usage_summary(budget)
  result$model_calls <- result$usage$calls
  result
}

preflight_diagnosis_lines <- function(diagnosis) {
  if (is.null(diagnosis)) return(character())
  if (diagnosis$status != "completed") return(c(
    diagnosis$reason %||% diagnosis$status,
    if (identical(diagnosis$response_status, "incomplete")) paste0(
      "Finish reason: ", diagnosis$finish_reason %||% "unknown",
      "; max_output_tokens = ", diagnosis$max_output_tokens, ". ",
      "Review llm.max_output_tokens and the model's context limit, or use diagnose = \"off\".")))
  advice <- diagnosis$advisory
  c("AI preflight diagnosis (advisory; static findings remain unchanged):",
    advice$summary,
    vapply(advice$findings, function(f) paste0(f$classification, ": ", f$explanation,
      "\nEvidence: ", f$evidence, "\nSuggested action: ", f$suggestion,
      if (nzchar(f$uncertainty)) paste0("\nUncertainty: ", f$uncertainty)), ""),
    if (!is.null(diagnosis$issue_url)) paste("Suggested issue (not submitted):", diagnosis$issue_url),
    if (!is.null(diagnosis$issue_url)) advice$issue_title,
    if (!is.null(diagnosis$issue_url)) paste("The issue tracker is public: review the draft, remove study code, identifiers and paths,",
      "and use a minimal synthetic example before submitting."),
    if (diagnosis$context_truncated) "The supplied context was truncated; omitted facts remain unknown.")
}
