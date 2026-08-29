if (!identical(Sys.getenv("SAS2R_SMOKE_VERTEX"), "true")) {
  stop("Set SAS2R_SMOKE_VERTEX=true to opt in", call. = FALSE)
}

required_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) stop("Missing ", name, call. = FALSE)
  value
}

config <- list(
  provider = "vertex", auth_mode = "ambient",
  project_id = required_env("SAS2R_SMOKE_GCP_PROJECT"),
  location = required_env("SAS2R_SMOKE_GCP_LOCATION"),
  model = required_env("SAS2R_SMOKE_VERTEX_MODEL"),
  capabilities = list(
    structured_output = "native", max_output_tokens = "supported"
  )
)
config <- sas2r:::normalize_llm_config(config)
selector <- sas2r:::redact_llm_secrets(sas2r:::llm_selector_identity(config))
cat("Vertex selector:", selector$project_id, selector$location, selector$model, "\n")
cat("Ceiling: one structured request, 32 requested output tokens; USD unknown without classified adapter cost metadata\n")
print(sas2r::sas_llm_models(config))
stopifnot(sas2r:::sas_llm_probe(config, max_retries = 1L))
