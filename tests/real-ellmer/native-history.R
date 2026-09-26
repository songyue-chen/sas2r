# Real provider serializers, two tool batches and finalization; loopback only.
run_native_history_contract <- function() {
  library(sas2r)
  port <- tempfile(); log <- tempfile()
  server <- processx::process$new(Sys.which("python3"), c(
    "tests/real-ellmer/replay_server.py", "--port-file", port, "--log-file", log),
    stdout = "|", stderr = "|", cleanup_tree = TRUE)
  on.exit(server$kill(), add = TRUE)
  deadline <- Sys.time() + 10
  while (!file.exists(port) && Sys.time() < deadline) Sys.sleep(0.02)
  stopifnot(file.exists(port))
  assert_history <- function(wire, provider, check_batch = TRUE, truncated = FALSE) {
    stopifnot(length(wire) == if (truncated) 2L else 4L)
    # Every later request carries all earlier tool calls with their native
    # reasoning/signatures, plus paired results. No extra user batch prompt.
    for (i in 2:length(wire)) {
      body <- wire[[i]]$body
      expected <- min(i - 1L, 2L)
      if (provider == "deepseek") {
        calls <- Filter(function(m) !is.null(m$tool_calls), body$messages)
        results <- Filter(function(m) identical(m$role, "tool"), body$messages)
        users <- Filter(function(m) identical(m$role, "user"), body$messages)
        stopifnot(length(calls) == expected, length(results) == expected,
          length(users) == if (i == length(wire)) 2L else 1L)
        for (j in seq_len(expected)) stopifnot(
          calls[[j]]$reasoning_content == paste("native reasoning batch", j),
          calls[[j]]$tool_calls[[1]]$id == paste0("native_", j),
          results[[j]]$tool_call_id == paste0("native_", j),
          (!check_batch || jsonlite::fromJSON(results[[j]]$content)$batch == j))
      } else {
        parts <- unlist(lapply(body$contents, `[[`, "parts"), recursive = FALSE)
        calls <- Filter(function(p) !is.null(p$functionCall), parts)
        results <- Filter(function(p) !is.null(p$functionResponse), parts)
        texts <- Filter(function(t) identical(t$role, "user") &&
          any(vapply(t$parts, function(p) !is.null(p$text), logical(1))), body$contents)
        stopifnot(length(calls) == expected, length(results) == expected,
          length(texts) == if (i == length(wire)) 2L else 1L)
        for (j in seq_len(expected)) stopifnot(
          calls[[j]]$thoughtSignature == paste0("c2lnbmF0dXJl", j),
          results[[j]]$functionResponse$name == calls[[j]]$functionCall$name)
      }
    }
  }
  for (provider in c("deepseek", "gemini")) {
    before <- if (file.exists(log)) length(readLines(log)) else 0L
    model <- paste0("offline-native-", if (provider == "deepseek") "deepseek" else "gemini")
    adapter <- sas2r:::ellmer_llm(list(provider = provider, model = model,
      base_url = sprintf("http://127.0.0.1:%s/v1", readLines(port, warn = FALSE)[1]),
      auth_mode = "api_key", api_key = "offline-native-history", max_tries = 1L,
      capabilities = list(structured_output = if (provider == "deepseek") "fallback" else "native",
        tool_calling = "native")))
    executed <- 0L
    lookup <- sas2r:::make_tool("lookup", function(args) {
      executed <<- executed + 1L
      list(name = args$name, batch = executed)
    }, max_calls = 3L, schema = list(type = "object",
      properties = list(name = list(type = "string")), required = "name", additionalProperties = FALSE))
    budget <- sas2r:::new_usage_budget()
    result <- sas2r:::run_agent(list(name = "native-history", prompt = "translator.md",
      tier = "frontier", tool_call_limit = 3L, retry_limit = 0L,
      output_schema = "program_translation_v1"), adapter, list(lookup = lookup),
      paste("translate", provider), usage_budget = budget, log_dir = tempfile())
    stopifnot(result$status == "ok", executed == 2L)
    wire <- lapply(readLines(log), jsonlite::fromJSON, simplifyVector = FALSE)
    wire <- wire[seq_along(wire) > before]
    stopifnot(length(wire) == 4L)
    if (sas2r:::ellmer_has_request_callbacks()) {
      stopifnot(budget$request_count == 4L)
      completed <- Filter(function(x) identical(x$record_type, "request_completed"), budget$records)
      stopifnot(length(completed) == 4L,
        all(vapply(completed, function(x) x$input_tokens == 100 && x$output_tokens == 25, logical(1))))
    } else stopifnot(budget$request_count == 2L)
    assert_history(wire, provider)
    # Exhaustion after a tool result must retain that native result and its
    # reasoning/signature when the final request closes tools.
    before <- length(readLines(log)); executed <- 0L
    quota <- sas2r:::run_agent(list(name = "native-quota", prompt = "translator.md",
      tier = "frontier", tool_call_limit = 1L, retry_limit = 0L,
      output_schema = "program_translation_v1"), adapter, list(lookup = lookup),
      "translate with one lookup", log_dir = tempfile())
    stopifnot(quota$status == "ok", executed == 1L)
    wire <- lapply(readLines(log), jsonlite::fromJSON, simplifyVector = FALSE)
    assert_history(wire[seq_along(wire) > before], provider, truncated = TRUE)
    if (sas2r:::ellmer_has_request_callbacks()) {
      before <- length(readLines(log))
      budget <- sas2r:::new_usage_budget(max_calls = 1L)
      capped <- sas2r:::run_agent(list(name = "native-cap", prompt = "translator.md",
        tier = "frontier", tool_call_limit = 3L, retry_limit = 0L,
        output_schema = "program_translation_v1"), adapter, list(lookup = lookup),
        "translate with a request ceiling", usage_budget = budget, log_dir = tempfile())
      stopifnot(capped$status != "ok", budget$request_count == 1L,
        length(readLines(log)) - before == 1L, length(budget$reservations) == 0L)
      source("tests/testthat/helper-agents.R", local = TRUE)
      root <- tempfile("native-workers-"); dir.create(root)
      on.exit(unlink(root, recursive = TRUE), add = TRUE)
      for (j in 1:2) writeLines(sprintf("data work.out%d; x=%d; run;", j, j),
        file.path(root, paste0("p", j, ".sas")))
      config <- attr(adapter, "parallel_config")
      config$model <- sub("offline-native", "offline-parallel-native", model)
      worker_adapter <- sas2r:::ellmer_llm(config)
      setup <- sas2r:::translation_setup(root, NULL, NULL, FALSE)
      state <- sas2r:::new_migration_state(setup$project, file.path(root, "out"),
        llm = worker_adapter, config = setup$config, execute = FALSE, plan = setup$plan)
      state$output_contracts <- setup$plan$contracts
      state$parallel <- sas2r:::resolve_parallel_execution(state, 2L)
      for (cid in state$schedule$component_id) {
        state <- sas2r:::initialize_program_component(state, cid)
        state <- sas2r:::check_component_revision(state, cid)
      }
      before <- length(readLines(log))
      before_calls <- state$usage_budget$request_count
      checked <- sas2r:::finalize_parallel_component_reviews(state)
      stopifnot(all(vapply(checked$histories, sas2r:::component_review_verdict, "") ==
        "reviewed_no_material_finding"))
      wire <- lapply(readLines(log), jsonlite::fromJSON, simplifyVector = FALSE)
      wire <- wire[seq_along(wire) > before]
      stopifnot(length(wire) == 8L, checked$usage_budget$request_count - before_calls == 8L)
      # Task data lives in the first user turn. Each worker retains its own
      # component context and native history across tool batches.
      key <- vapply(wire, function(entry) {
        if (provider == "deepseek") {
          prompt <- Filter(function(m) identical(m$role, "user"), entry$body$messages)[[1]]$content
        } else {
          prompt <- Filter(function(m) identical(m$role, "user"), entry$body$contents)[[1]]$parts
        }
        digest::digest(prompt)
      }, "")
      stopifnot(length(unique(key)) == 2L)
      # Workflow tools return rulebook records rather than our batch marker.
      for (group in split(wire, key)) assert_history(group, provider, check_batch = FALSE)
    }
    cat(sprintf("ellmer %s %s: native history retained through two tool batches and finalization (serial and available parallel paths).\n",
      packageVersion("ellmer"), provider))
  }
}
run_native_history_contract()
