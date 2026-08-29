if (!identical(Sys.getenv("SAS2R_SMOKE_BEDROCK"), "true")) {
  stop("Set SAS2R_SMOKE_BEDROCK=true to opt in", call. = FALSE)
}

required_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) stop("Missing ", name, call. = FALSE)
  value
}

config <- list(
  provider = "bedrock", auth_mode = "ambient",
  profile = required_env("SAS2R_SMOKE_AWS_PROFILE"),
  region = required_env("SAS2R_SMOKE_AWS_REGION"),
  model = required_env("SAS2R_SMOKE_BEDROCK_MODEL"),
  capabilities = list(
    structured_output = "native", max_output_tokens = "supported"
  )
)
config <- sas2r:::normalize_llm_config(config)
selector <- sas2r:::redact_llm_secrets(sas2r:::llm_selector_identity(config))
cat("Bedrock selector:", selector$profile, selector$region, selector$model, "\n")
cat("Ceiling: one structured request, 32 requested output tokens; USD unknown without classified adapter cost metadata\n")
print(sas2r::sas_llm_models(config))
stopifnot(sas2r:::sas_llm_probe(config, max_retries = 1L))
