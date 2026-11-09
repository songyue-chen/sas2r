# sas2r tutorial: extract the entire companion folder into a writable project.
# Run this script section by section, stopping at the configuration pause.
# Install from CRAN: install.packages(c("sas2r", "ellmer", "pdftools"))
# Store OPENAI_API_KEY in your user .Renviron, then restart R.
# file.edit(path.expand("~/.Renviron"))
# nzchar(Sys.getenv("OPENAI_API_KEY"))  # Presence only; never print the key.

library(sas2r)
project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
example_dir <- file.path(project_dir, "example")
example_files <- c("demo.sas", "_sas2r.yml", "make-input.R")
stopifnot(all(file.exists(file.path(example_dir, example_files))))
# Copy the tutorial example supplied with this companion download.
# Preserve existing project files and your edited YAML on repeat setup.
for (name in example_files) {
  destination <- file.path(project_dir, name)
  if (!file.exists(destination)) {
    stopifnot(file.copy(file.path(example_dir, name), destination))
  }
}
if (!identical(readLines("demo.sas", warn = FALSE),
               readLines(file.path(example_dir, "demo.sas"), warn = FALSE))) {
  stop("demo.sas differs from the corrected tutorial. Use a fresh project for this walkthrough.")
}
demo_input <- new.env()
sys.source(file.path(project_dir, "make-input.R"), envir = demo_input)
demo_input$make_demo_input(file.path(project_dir, "data"))
input_path <- file.path(project_dir, "data", "input_ds.rds")
stopifnot(file.exists(input_path), nrow(readRDS(input_path)) == 5L)

# PAUSE: copy the commented OpenAI YAML template below into the SAME
# _sas2r.yml, below outputs. Remove only each leading "# ", then save.
# Keep libraries and outputs. Update llm if it already exists.

# Offline source/input preflight first; no provider requests here.
migration_dir <- file.path(project_dir, "migration-output")
check <- sas_preflight(
  path = file.path(project_dir, "demo.sas"),
  config = file.path(project_dir, "_sas2r.yml"),
  out_dir = migration_dir,
  diagnose = "off"
)
print(check)
print(check$inputs)
input_row <- check$inputs[check$inputs$dataset == "raw.input_ds", ]
if (nrow(input_row) != 1L || input_row$status != "available") {
  stop("raw.input_ds must be available before making provider requests. Check data/input_ds.rds and libraries.raw in _sas2r.yml.")
}

# These next two steps make real, potentially billable provider requests.
source("preflight-llm.R", local = TRUE)
result <- sas_translate(
  path = check$project,
  out_dir = migration_dir,
  execute = TRUE,
  max_parallel_translations = 1,
  max_program_repair_rounds = 2,
  agent_evidence = "code_only"
)
source("read-llm-log.R", local = TRUE)

result$status
result$status_reason
result$outputs_dir
browseURL(file.path(result$out_dir, result$run_id, "START_HERE.html"))
cat(sas_code(result, 1))

# Inspect actual output; this stops if execution/output checks did not pass.
source("verify-output.R", local = TRUE)
final_data <- verify_demo_output(result, project_dir)
View(final_data)

# Export only after inspecting status and the tutorial checks above.
# Use a new destination if you already exported a previous run.
export_dir <- file.path(project_dir, "exported-r")
sas_write(result, export_dir)

# OpenAI YAML template for the SAME project _sas2r.yml (not R code).
# Keep the source template commented in demo.R.
# Copy only the 11 lines below. Remove only their leading "# ".
# Preserve the remaining indentation; llm aligns with libraries and outputs.
# Update an existing llm section instead of creating a duplicate.
# Keep OPENAI_API_KEY in .Renviron; never put credentials in YAML.
# llm:
#   provider: openai
#   auth_mode: api_key
#   model: gpt-5.6-luna
#   reasoning_effort: high
#   max_output_tokens: 128000
#   capabilities:
#     structured_output: native
#     tool_calling: native
#   timeout_seconds: 900
#   max_tries: 1
