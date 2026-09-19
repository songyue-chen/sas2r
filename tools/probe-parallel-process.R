# Offline process/polling measurement, not a live-model speed benchmark.
pkgload::load_all(quiet = TRUE)
results <- lapply(c(0.25, 0.025), function(interval) {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- fx$state
  llm <- parallel_test_llm(list(reviewer = valid_program_review_response()), delay = 0.05)
  state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
  for (cid in fx$ids) state <- check_component_revision(state, cid)
  pool <- parallel_new_pool(state)
  on.exit(parallel_stop_pool(pool), add = TRUE)
  start <- Sys.time()
  for (cid in fx$ids) parallel_start_job(pool, state, cid, "review", FALSE, 0L)
  while (length(pool$jobs)) {
    parallel_poll(pool)
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > 30)
      stop("Process probe exceeded 30 seconds")
    if (length(pool$jobs)) Sys.sleep(interval)
  }
  c(list(parent_poll_seconds = interval,
    elapsed_seconds = as.numeric(difftime(Sys.time(), start, units = "secs")),
    external_provider_calls = 0L, mock_calls = state$usage_budget$request_count),
    parallel_observations(pool))
})
cat(jsonlite::toJSON(results, auto_unbox = TRUE, pretty = TRUE), "\n")
