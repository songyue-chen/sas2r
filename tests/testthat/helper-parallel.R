# Explicit fresh-process factory: no captured test harness or parent budget.
parallel_test_llm <- function(responses, delay = 0.1, marker_dir = NULL, crash_component = NULL,
                              write_failure_component = NULL, translator_barrier = NULL) {
  env <- list2env(list(responses = responses, delay = delay, marker_dir = marker_dir,
    crash_component = crash_component, write_failure_component = write_failure_component,
    translator_barrier = translator_barrier),
    parent = asNamespace("sas2r"))
  factory <- function() {
    new_llm(function(request, audit_context = list()) {
      started <- as.numeric(Sys.time())
      role <- audit_context$agent %||% audit_context$purpose
      if (identical(role, "translator") && !is.null(translator_barrier)) {
        file.create(file.path(translator_barrier$dir, audit_context$component_id))
        deadline <- Sys.time() + 30
        while (length(list.files(translator_barrier$dir)) < translator_barrier$count) {
          if (Sys.time() >= deadline) stop("parallel test translators did not reach the barrier")
          Sys.sleep(0.02)
        }
      }
      Sys.sleep(if (length(delay) > 1L) delay[[audit_context$component_id]] else delay)
      if (identical(audit_context$component_id, crash_component)) quit(save = "no", status = 7L)
      if (identical(role, "fixer") && identical(audit_context$component_id, write_failure_component) &&
          !is.null(.parallel_worker$client)) {
        # Inject a callback failure when the successful fixer response is
        # persisted, inside the component handler. Only this child is changed.
        failing_write <- local({
          original_write <- atomic_write_file
          revision_root <- file.path(.parallel_worker$client$dir, "components", write_failure_component)
          function(write_fn, target_file, pattern = "atomic_") {
            if (startsWith(target_file, paste0(revision_root, "/")) &&
                identical(basename(target_file), "program.R")) {
              write_fn <- function(path) stop("simulated revision write failure")
            }
            original_write(write_fn, target_file, pattern)
          }
        })
        assignInNamespace("atomic_write_file", failing_write, ns = "sas2r")
      }
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
