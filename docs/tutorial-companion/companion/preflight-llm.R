# Run in the prepared RStudio project after saving _sas2r.yml.
# This script makes a real provider request (billable when applicable).
# Its video demonstration uses an offline test adapter instead.
library(sas2r)
project_dir <- normalizePath(".", winslash = "/")
cfg <- sas_config(file.path(project_dir, "_sas2r.yml"))
llm <- sas_llm(cfg)  # Construction is offline. Do not print the adapter.
probe_log_dir <- file.path(project_dir, "llm-probe")

probe_ok <- tryCatch(
  sas_llm_probe(
    llm, tier = "frontier", max_retries = 1L,
    log_dir = probe_log_dir
  ),
  error = function(e) {
    message(conditionMessage(e))
    FALSE
  }
)
print(probe_ok)

# Read the most recent probe event, including after a logged failure.
log_file <- file.path(probe_log_dir, "llm_log.jsonl")
if (file.exists(log_file)) {
  last <- jsonlite::fromJSON(tail(readLines(log_file, warn = FALSE), 1))
  fields <- c("agent", "type", "status", "tier", "attempt")
  print(as.data.frame(last[intersect(fields, names(last))]), row.names = FALSE)
  # Inspect the complete last event when debugging, without printing your key:
  if (!isTRUE(probe_ok)) str(last)
}
if (!isTRUE(probe_ok)) {
  stop("LLM probe did not pass. Resolve the diagnostic before translating.")
}
# TRUE checks the structured ping; it does not validate tool calling,
# a full migration, or equivalence to SAS reference results.
