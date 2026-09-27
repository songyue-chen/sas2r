# One-way report advice. Never used for repairs, candidate selection or status.
current_report_findings <- function(state) {
  result <- list()
  for (cid in names(state$histories)) {
    events <- current_component_evidence(state$histories[[cid]])$events %||% list()
    reviews <- Filter(function(e) e$type %in% c("review_completed", "source_mismatch_review"), events)
    if (!length(reviews)) next
    # Keep only the latest opinion about the selected revision.
    event <- utils::tail(reviews, 1L)[[1L]]
    for (finding in event$findings %||% list()) {
      for (target in unique(c(event$target_keys, finding$affected_outputs))) {
        result[[length(result) + 1L]] <- list(target = target, component_id = cid,
          sas_evidence = finding$sas_evidence, r_evidence = finding$r_evidence,
          possible_cause = "A source/code review raised the finding below. Whether it explains these comparison differences is unverified.",
          next_action = "Check the cited SAS and R operations and related programs before changing code.")
      }
    }
  }
  result
}

report_diagnosis_context <- function(state, summaries, limit = 24000L) {
  text <- as.character(jsonlite::toJSON(list(discrepancies = summaries,
    execution_order = state$graph$execution_order), auto_unbox = TRUE, null = "null"))
  truncated <- nchar(text) > limit
  text <- substr(text, 1L, limit)
  ids <- unique(unlist(lapply(names(summaries), function(key)
    state$assessment$lineage_by_target[[key]]$upstream_components %||%
      graph_output_writers(state$graph, key)), use.names = FALSE))
  ids <- unique(c(ids, unlist(lapply(ids, function(cid)
    context_component_dependencies(state$graph, cid)), use.names = FALSE)))
  for (cid in ids) {
    block <- paste0("\nComponent: ", cid, "\nSAS:\n", component_source_text(state$graph, cid),
      "\nSelected R:\n", state$selected_revisions[[cid]]$r_code %||% "Unavailable")
    remaining <- max(0L, limit - nchar(text))
    text <- paste0(text, substr(block, 1L, remaining))
    if (nchar(block) > remaining) { truncated <- TRUE; break }
  }
  list(text = paste0(text, if (truncated) "\n[Context truncated; omitted code is unknown.]"), truncated = truncated)
}

migration_report_diagnosis <- function(state) {
  result <- list(status = "unavailable", reason = NULL, explanations = list(), origin = NULL)
  if (identical(state$config$migration$report_diagnosis, "off")) {
    result$status <- "disabled"; result$reason <- "Explanatory AI diagnosis is disabled. Measured comparisons remain available."
    return(result)
  }
  targets <- Filter(function(t) identical(t$kind %||% "dataset", "dataset") &&
    !identical(t$status, "passed"), state$assessment$targets %||% list())
  if (!length(targets)) {
    result$status <- "not_needed"; result$reason <- "No unresolved dataset comparisons need explanation."
    return(result)
  }
  existing <- Filter(function(f) f$target %in% names(targets), current_report_findings(state))
  if (length(existing)) {
    result$status <- "completed"; result$origin <- "existing_source_review"
    result$reason <- paste0("Reused current source/code review evidence for ",
      length(unique(vapply(existing, `[[`, "", "target"))), " of ", length(targets),
      " unresolved targets; other causes remain unresolved. No additional model call.")
    result$explanations <- existing
    return(result)
  }
  llm <- state$reviewer_llm
  if (is.null(llm)) {
    result$status <- "not_configured"; result$reason <- "Explanatory AI diagnosis was not performed: no reviewer LLM configured."
    return(result)
  }
  budget <- state$usage_budget
  if (is.null(budget) || !usage_budget_allows_future(budget)) {
    result$status <- "budget_unavailable"; result$reason <- "Explanatory AI diagnosis was not performed: no request budget remains."
    return(result)
  }
  redact <- llm_audit_redactor(llm)
  tryCatch({
    settings <- attr(llm, "parallel_config", exact = TRUE)
    if (!is.null(settings)) {
      settings$max_tries <- 1L
      settings$timeout_seconds <- min(settings$timeout_seconds %||% 120, 120)
      llm <- sas_llm(settings)
    }
    summaries <- lapply(names(targets), function(key) reviewer_discrepancy_summary(targets[[key]], key))
    names(summaries) <- names(targets)
    result$provider <- llm$provider
    result$model <- llm$model
    packet <- report_diagnosis_context(state, summaries)
    schema <- jsonlite::read_json(system.file("schemas", "report-diagnosis-v1.json", package = "sas2r"))
    prompt <- paste(readLines(system.file("prompts", "report-diagnosis.md", package = "sas2r"), warn = FALSE), collapse = "\n")
    request <- llm_request(messages = list(list(role = "system", content = prompt),
      list(role = "user", content = redact(packet$text))), output_schema = schema,
      schema_name = "report_diagnosis_v1", schema_version = "1",
      schema_mode = if (identical(llm_capabilities_for(llm)$structured_output, "native")) "native" else "fallback",
      max_output_tokens = llm$model_parameters$max_output_tokens %||% 4096L,
      reasoning_effort = llm$model_parameters$reasoning_effort,
      temperature = llm$model_parameters$temperature, top_p = llm$model_parameters$top_p)
    cli::cli_inform('AI report diagnosis may send bounded SAS/R code and aggregate comparison patterns to {llm$provider}, subject to budget admission; no records or raw logs. Use migration.report_diagnosis: off to disable.')
    response <- attempt_llm_request(request, llm, usage_budget = budget,
      audit_context = list(purpose = "report_diagnosis", agent = "report_diagnosis"))
    result$response_status <- response$status
    result$finish_reason <- redact(response$finish_reason)
    result$model <- response$resolved_model %||% result$model
    result$origin <- "report_diagnosis"
    result$context_truncated <- packet$truncated
    if (identical(response$status, "completed") && identical(response$action, "final") &&
        !length(validate_schema_value(response$data, schema))) {
      result$status <- "completed"
      result$explanations <- Filter(function(f) f$target %in% names(targets) &&
        f$component_id %in% names(state$selected_revisions), redact(response$data$explanations))
      result$reason <- "AI explanation is advisory. Source, input and reference consistency may require programmer investigation."
    } else result$reason <- paste0("AI explanation unavailable: ", response$status,
      "; finish reason: ", result$finish_reason %||% "unknown",
      ". The response was incomplete, failed or invalid; no retry was made.")
  }, error = function(e) {
    result$reason <<- paste("AI explanation unavailable:", redact(conditionMessage(e)))
  })
  result
}
