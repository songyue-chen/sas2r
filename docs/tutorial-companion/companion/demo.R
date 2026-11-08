# sas2r tutorial: run interactively in a NEW, empty RStudio project.
# Install once:
# install.packages(c("sas2r", "ellmer"))
# Configure OPENAI_API_KEY in your user .Renviron, save, then restart R.
# file.edit(path.expand("~/.Renviron"))
# Check presence without printing the secret:
# nzchar(Sys.getenv("OPENAI_API_KEY"))
# TRUE is not an authentication test.

library(sas2r)
project_dir <- normalizePath(".", winslash = "/")
installed_demo <- system.file(
  "examples", "migration-demo", package = "sas2r"
)
file.copy(
  list.files(installed_demo, full.names = TRUE),
  project_dir,
  recursive = TRUE
)
demo_input <- new.env()
sys.source(
  file.path(project_dir, "make-input.R"),
  envir = demo_input
)
demo_input$make_demo_input(file.path(project_dir, "data"))

# PAUSE: open _sas2r.yml and add the OpenAI llm block supplied with this tutorial.
# Save it before continuing. Do not put an API key in YAML.

# Copy preflight-llm.R into this project first. This probe uses the provider.
source("preflight-llm.R")

migration_dir <- file.path(project_dir, "migration-output")
check <- sas_preflight(
  path = file.path(project_dir, "demo.sas"),
  config = file.path(project_dir, "_sas2r.yml"),
  out_dir = migration_dir,
  diagnose = "off"
)
print(check)
check$inputs

# Review findings. The bundled example has a dynamic_dataset_reference warning
# for `set &plotds`; review and execution must assess that behavior.
# THE NEXT CALL MAKES PAID PROVIDER REQUESTS WHEN YOU RUN IT.
result <- sas_translate(
  path = check$project,
  out_dir = migration_dir,
  execute = TRUE,
  max_parallel_translations = 1,
  agent_evidence = "code_only"
)
# Optional: copy read-llm-log.R into this project to inspect model activity.
source("read-llm-log.R")

result$status
result$status_reason
result$outputs_dir
browseURL(file.path(result$out_dir, result$run_id, "START_HERE.html"))
cat(sas_code(result, 1))

# Inspect only outputs actually produced; do not assume the migration succeeded.
if (!is.null(result$outputs_dir)) {
  dataset_path <- file.path(
    result$outputs_dir, "datasets", "adam", "final_ds.rds"
  )
  if (file.exists(dataset_path)) {
    final_data <- readRDS(dataset_path)
    View(final_data)
  }
}

# Use a new, empty export folder. Exporting does not change validation status.
export_dir <- file.path(project_dir, "exported-r")
sas_write(result, export_dir)
