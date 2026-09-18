# Installed-package, real-adapter process contract. All HTTP traffic is loopback.
local({
  library(sas2r)
  source("tests/testthat/helper-agents.R", local = TRUE)
  for (name in c("ellmer_llm", "translation_setup", "new_migration_state", "resolve_parallel_execution",
    "initialize_program_component", "check_component_revision", "finalize_parallel_component_reviews",
    "component_review_verdict")) {
    if (!exists(name, inherits = FALSE)) assign(name, get(name, asNamespace("sas2r")))
  }
  port <- tempfile(); log <- tempfile()
  server <- processx::process$new(Sys.which("python3"), c(
    "tests/real-ellmer/replay_server.py", "--port-file", port, "--log-file", log),
    stdout = "|", stderr = "|", cleanup_tree = TRUE)
  on.exit(server$kill(), add = TRUE)
  deadline <- Sys.time() + 10
  while (!file.exists(port) && Sys.time() < deadline) Sys.sleep(0.02)
  stopifnot(file.exists(port))
  root <- tempfile("parallel-real-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (i in 1:2) writeLines(sprintf("data work.out%d; x = %d; run;", i, i),
    file.path(root, paste0("p", i, ".sas")))
  adapter <- ellmer_llm(list(provider = "openai", model = "offline-parallel-model",
    base_url = sprintf("http://127.0.0.1:%s/v1", readLines(port, warn = FALSE)[1]),
    api_key = "offline-parallel-secret", max_tries = 1L, timeout_seconds = 2,
    capabilities = list(structured_output = "native", tool_calling = "native",
      max_output_tokens = "supported", temperature = "supported")))
  setup <- translation_setup(root, NULL, NULL, FALSE)
  state <- new_migration_state(setup$project, file.path(root, "out"), llm = adapter,
    config = setup$config, execute = FALSE, plan = setup$plan)
  state$output_contracts <- setup$plan$contracts
  state$parallel <- resolve_parallel_execution(state, 2L)
  for (cid in state$schedule$component_id) {
    state <- initialize_program_component(state, cid)
    state <- check_component_revision(state, cid)
  }
  before_calls <- state$usage_budget$request_count
  before_tools <- state$usage_budget$tool_count
  result <- finalize_parallel_component_reviews(state)
  stopifnot(all(vapply(result$histories, component_review_verdict, "") == "reviewed_no_material_finding"))
  wire <- lapply(readLines(log, warn = FALSE), jsonlite::fromJSON, simplifyVector = FALSE)
  stopifnot(length(wire) == result$usage_budget$request_count,
    result$usage_budget$tool_completed_count - before_tools == 2L,
    result$usage_budget$tool_count - before_tools == 2L,
    result$usage_budget$request_count - before_calls == 6L,
    length(result$usage_budget$reservations) == 0L)
  # Each continuation must preserve its own SAS source, tool request/result,
  # and the configured timeout/retry policy in the authoritative ledger.
  started <- Filter(function(record) identical(record$record_type, "request_started"), result$usage_budget$records)
  stopifnot(all(vapply(started, function(record) record$timeout_seconds == 2 && record$transport_max_tries == 1L, logical(1))))
  files <- list.files(file.path(state$paths$diagnostics, "workers"), pattern = "[.](request|reply)$", recursive = TRUE)
  stopifnot(length(files) == 0L)
  cat(sprintf("Real ellmer %s: %d HTTP requests, %d admissions, %d tools; two reviews passed.\n",
    packageVersion("ellmer"), length(wire), result$usage_budget$request_count, result$usage_budget$tool_completed_count))
  ceiling <- result$usage_budget$request_count + 1L
  state$usage_budget$max_calls <- ceiling
  capped <- finalize_parallel_component_reviews(state)
  stopifnot(capped$usage_budget$request_count == ceiling,
    length(readLines(log, warn = FALSE)) == ceiling,
    all(vapply(capped$histories, component_review_verdict, "") == "review_unavailable"))
  cat("One remaining call allowed one HTTP request, then deferred both incomplete reviews.\n")

  rejection_config <- attr(adapter, "parallel_config")
  rejection_config$model <- "offline-parallel-rejection-model"
  rejection_adapter <- ellmer_llm(rejection_config)
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- rejection_adapter
  state$usage_budget$max_calls <- Inf
  learned <- finalize_parallel_component_reviews(state)
  stopifnot(all(vapply(learned$histories, component_review_verdict, "") == "reviewed_no_material_finding"))
  wire <- lapply(readLines(log, warn = FALSE), jsonlite::fromJSON, simplifyVector = FALSE)
  rejected <- Filter(function(entry) identical(entry$body$model, "offline-parallel-rejection-model") &&
    !is.null(entry$body$temperature), wire)
  # Either worker can learn first; a request already admitted by its peer may
  # still encounter the same rejection. Later workers must retain that finding.
  stopifnot(length(rejected) >= 1L, length(rejected) <= 2L)
  before <- length(wire)
  learned_again <- finalize_parallel_component_reviews(state)
  wire <- lapply(readLines(log, warn = FALSE), jsonlite::fromJSON, simplifyVector = FALSE)
  next_requests <- wire[seq_along(wire) > before]
  stopifnot(length(next_requests) == 6L,
    all(vapply(next_requests, function(entry) is.null(entry$body$temperature), logical(1))),
    all(vapply(learned_again$histories, component_review_verdict, "") == "reviewed_no_material_finding"),
    length(wire) == learned_again$usage_budget$request_count)
  cat("Workers reported unsupported optional settings; later workers reused that knowledge without another rejected request.\n")

})
