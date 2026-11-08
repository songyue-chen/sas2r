# Run after sas_translate() returns `result`.
run_dir <- file.path(result$out_dir, result$run_id)
log_file <- file.path(run_dir, "diagnostics", "logs", "llm_log.jsonl")
if (file.exists(log_file)) {
  log <- jsonlite::stream_in(file(log_file), verbose = FALSE)
  activity <- log[log$type == "final", c("agent", "status", "tier")]
  print(activity, row.names = FALSE)
} else {
  message("No LLM log exists for this run: ", log_file)
}
# `completed` describes the model request, not the review verdict or equivalence.
# Review details and diagnostics via START_HERE.html. Fixer calls are conditional.
