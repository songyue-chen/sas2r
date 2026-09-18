# Explicit fresh-process factory: no captured test harness or parent budget.
parallel_test_llm <- function(responses, delay = 0.1, marker_dir = NULL, crash_component = NULL) {
  env <- list2env(list(responses = responses, delay = delay, marker_dir = marker_dir, crash_component = crash_component),
    parent = asNamespace("sas2r"))
  factory <- function() {
    new_llm(function(request, audit_context = list()) {
      started <- as.numeric(Sys.time())
      Sys.sleep(if (length(delay) > 1L) delay[[audit_context$component_id]] else delay)
      if (identical(audit_context$component_id, crash_component)) quit(save = "no", status = 7L)
      role <- audit_context$agent %||% audit_context$purpose
      key <- paste(role, audit_context$component_id, sep = ":")
      response <- responses[[key]] %||% responses[[role]] %||% responses[[1L]]
      if (!is.null(marker_dir)) saveRDS(list(start = started, end = as.numeric(Sys.time()),
        component_id = audit_context$component_id, agent = role, request = list(
          messages = request$messages, parameters = request$parameters,
          tools = lapply(request$tools, function(tool) tool[c("name", "schema", "description")]))),
        file.path(marker_dir, paste0(request$request_id, ".rds")))
      attr(response, "cost_usd") <- 0.01
      attr(response, "cost_status") <- "billed_amount"
      attr(response, "cost_currency") <- "USD"
      normalize_provider_response(response, request, "mock")
    }, provider = "mock", capabilities = llm_capabilities(structured_output = "native",
      tool_calling = "native", tools_with_structured_output = "supported", max_output_tokens = "supported"))
  }
  environment(factory) <- env
  adapter <- factory()
  attr(adapter, "parallel_factory") <- factory
  adapter
}
