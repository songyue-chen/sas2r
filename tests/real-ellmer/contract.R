run_real_ellmer_contract <- function() {
`%||%` <- function(x, y) if (is.null(x)) y else x
expected_version <- Sys.getenv("SAS2R_EXPECTED_ELLMER_VERSION")
if (!expected_version %in% c("0.4.2", "current")) {
  stop("SAS2R_EXPECTED_ELLMER_VERSION must be 0.4.2 or current")
}
if (!requireNamespace("ellmer", quietly = TRUE)) {
  stop("the real ellmer package is required for this contract")
}
if (!requireNamespace("sas2r", quietly = TRUE)) {
  stop("an installed sas2r package is required for this contract")
}
if (!requireNamespace("processx", quietly = TRUE)) {
  stop("processx is required to manage the loopback replay server")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat is required for offline cloud authentication boundaries")
}

ellmer_path <- normalizePath(find.package("ellmer"), mustWork = TRUE)
ellmer_description <- utils::packageDescription("ellmer")
installed_version <- as.character(utils::packageVersion("ellmer"))
if (identical(
  ellmer_description$Title,
  "Isolated Ellmer S7 Contract Fixture"
) || grepl("tests/fixtures/ellmer-s7", ellmer_path, fixed = TRUE)) {
  stop("the committed ellmer stub must not satisfy the real contract matrix")
}
if (identical(expected_version, "0.4.2") &&
    !identical(installed_version, "0.4.2")) {
  stop("minimum contract expected ellmer 0.4.2, found ", installed_version)
}
if (utils::compareVersion(installed_version, "0.4.2") < 0L) {
  stop("real ellmer contract requires version 0.4.2 or newer")
}

server_script <- normalizePath(
  file.path("tests", "real-ellmer", "replay_server.py"), mustWork = TRUE
)
python <- Sys.which("python3")
if (!nzchar(python)) stop("python3 is required for the loopback replay server")
port_file <- tempfile("sas2r-ellmer-port-")
log_file <- tempfile("sas2r-ellmer-requests-")
server <- processx::process$new(
  python,
  c(server_script, "--port-file", port_file, "--log-file", log_file),
  stdout = "|", stderr = "|", cleanup_tree = TRUE
)
on.exit(try(server$kill(), silent = TRUE), add = TRUE)

deadline <- Sys.time() + 10
while (!file.exists(port_file) && Sys.time() < deadline && server$is_alive()) {
  Sys.sleep(0.05)
}
if (!file.exists(port_file)) {
  stop(
    "loopback replay server did not start: ",
    paste(c(server$read_all_output(), server$read_all_error()), collapse = "\n")
  )
}
port <- suppressWarnings(as.integer(readLines(port_file, warn = FALSE)[[1]]))
if (length(port) != 1L || is.na(port) || port < 1L) {
  stop("loopback replay server returned an invalid port")
}
base_url <- sprintf("http://127.0.0.1:%d/v1", port)
old_no_proxy <- Sys.getenv(c("NO_PROXY", "no_proxy"), unset = NA_character_)
Sys.setenv(NO_PROXY = "127.0.0.1,localhost", no_proxy = "127.0.0.1,localhost")
on.exit({
  for (name in names(old_no_proxy)) {
    value <- old_no_proxy[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, setNames(list(value), name))
    }
  }
}, add = TRUE)

assert_public_formals <- function(export, required, forbidden = character()) {
  if (!export %in% getNamespaceExports("ellmer")) {
    stop("real ellmer does not export ", export)
  }
  arguments <- names(formals(getExportedValue("ellmer", export)))
  missing <- setdiff(required, arguments)
  invalid <- intersect(forbidden, arguments)
  if (length(missing)) {
    stop(export, " lacks public argument(s): ", paste(missing, collapse = ", "))
  }
  if (length(invalid)) {
    stop(export, " unexpectedly accepts: ", paste(invalid, collapse = ", "))
  }
  invisible(arguments)
}

assert_public_formals(
  "chat_aws_bedrock", c("base_url", "model", "profile", "cache"),
  c("region", "api_key", "credentials")
)
assert_public_formals(
  "models_aws_bedrock", c("profile", "base_url"),
  c("region", "api_key", "credentials")
)
assert_public_formals(
  "chat_azure_openai",
  c("endpoint", "model", "api_version", "api_key", "credentials")
)
if ("models_azure_openai" %in% getNamespaceExports("ellmer")) {
  stop("ellmer now exports models_azure_openai; update sas2r discovery support")
}
assert_public_formals(
  "chat_google_vertex", c("location", "project_id", "model"),
  c("api_key", "credentials")
)
assert_public_formals(
  "models_google_vertex", c("location", "project_id", "credentials"),
  "api_key"
)
assert_public_formals(
  "chat_anthropic",
  c("base_url", "model", "cache", "api_key", "credentials"),
  c("region", "workspace", "account", "endpoint")
)
assert_public_formals(
  "models_anthropic", c("base_url", "api_key", "credentials"),
  c("model", "region")
)
assert_public_formals(
  "chat_databricks", c("workspace", "model"),
  c("base_url", "credentials", "api_key", "endpoint", "region")
)
assert_public_formals(
  "chat_deepseek", c("base_url", "model", "api_key", "credentials"),
  c("workspace", "account", "cache")
)
assert_public_formals(
  "models_deepseek", c("base_url", "api_key", "credentials"), "model"
)
assert_public_formals(
  "chat_github", c("base_url", "model", "api_key", "credentials"),
  c("models_base_url", "workspace", "account")
)
assert_public_formals(
  "models_github", c("base_url", "api_key", "credentials"), "model"
)
assert_public_formals(
  "chat_google_gemini", c("base_url", "model", "api_key", "credentials"),
  c("project_id", "location", "workspace")
)
assert_public_formals(
  "models_google_gemini", c("base_url", "api_key", "credentials"), "model"
)
assert_public_formals(
  "chat_posit", c("base_url", "model", "cache", "credentials"),
  c("api_key", "workspace", "account")
)
assert_public_formals(
  "models_posit", c("base_url", "credentials"), c("api_key", "model")
)
assert_public_formals(
  "chat_snowflake", c("account", "model", "credentials"),
  c("base_url", "api_key", "workspace")
)
for (absent in c("models_databricks", "models_snowflake")) {
  if (absent %in% getNamespaceExports("ellmer")) {
    stop("ellmer now exports ", absent, "; update sas2r discovery support")
  }
}

# GitHub Models publishes different documented chat and inventory paths. The
# `models_base_url` selector and the `inventory_unavailable` fallback exist
# only because of that split, so the split itself is part of the contract.
if (identical(
  formals(ellmer::chat_github)$base_url,
  formals(ellmer::models_github)$base_url
)) {
  stop("GitHub chat and inventory defaults converged; revisit models_base_url")
}

# The cache vocabularies sas2r validates against, read from real ellmer.
cache_options <- function(export, argument = "cache") {
  eval(formals(getExportedValue("ellmer", export))[[argument]])
}
for (export in c("chat_anthropic", "chat_posit")) {
  if (!identical(cache_options(export), c("5m", "1h", "none"))) {
    stop(export, " no longer offers exactly 5m, 1h, and none")
  }
}
if (!identical(cache_options("chat_aws_bedrock"), c("auto", "5m", "1h", "none"))) {
  stop("chat_aws_bedrock no longer offers its distinct auto cache option")
}

# Registry/ellmer drift: every selectable provider must still name a real
# public export in the installed ellmer.
for (spec in sas2r:::llm_provider_registry()) {
  stopifnot(spec$chat_export %in% getNamespaceExports("ellmer"))
  if (!is.null(spec$models_export)) {
    stopifnot(spec$models_export %in% getNamespaceExports("ellmer"))
  }
}

# Every argument any registry builder can emit must be a public formal of the
# export it is handed to, so a selector ellmer does not publish is a hard
# contract failure rather than a runtime `unused argument` surprise.
published_formals <- function(export) {
  names(formals(getExportedValue("ellmer", export)))
}
for (spec in sas2r:::llm_provider_registry()) {
  probe_config <- c(
    list(provider = spec$id, auth_mode = spec$default_auth_mode, model = "m"),
    stats::setNames(
      rep(list("offline-probe"), length(spec$config_fields)), spec$config_fields
    )
  )
  probe_config$credentials <- function() "offline-probe-credential"
  chat_arguments <- names(sas2r:::ellmer_constructor_args(probe_config, "m"))
  unpublished <- setdiff(chat_arguments, published_formals(spec$chat_export))
  if (length(unpublished)) {
    stop(spec$chat_export, " does not publish argument(s): ",
         paste(unpublished, collapse = ", "))
  }
  if (!is.null(spec$models_export)) {
    inventory_arguments <- names(sas2r:::llm_inventory_args(probe_config))
    unpublished <- setdiff(
      inventory_arguments, published_formals(spec$models_export)
    )
    if (length(unpublished)) {
      stop(spec$models_export, " does not publish argument(s): ",
           paste(unpublished, collapse = ", "))
    }
  }
}

cloud_base_url <- sub("/v1$", "", base_url)
bedrock_control_base_url <- paste0(cloud_base_url, "/bedrock-control")
offline_aws_credentials <- list(
  access_key_id = "offline-access-key",
  secret_access_key = "offline-secret-key",
  session_token = "offline-session-token",
  expiration = Sys.time() + 3600,
  region = "us-east-1"
)
bedrock_chat <- testthat::with_mocked_bindings(
  ellmer::chat_aws_bedrock(
    base_url = cloud_base_url, model = "offline-bedrock-model",
    profile = "sas2r-offline-contract", cache = "none"
  ),
  locate_aws_credentials = function(profile) offline_aws_credentials,
  .package = "ellmer"
)
azure_chat <- ellmer::chat_azure_openai(
  endpoint = cloud_base_url, model = "offline-azure-deployment",
  api_version = "2025-04-01-preview",
  credentials = function() "offline-azure-token"
)
vertex_chat <- testthat::with_mocked_bindings(
  ellmer::chat_google_vertex(
    location = "us-central1", project_id = "offline-project",
    model = "offline-vertex-model"
  ),
  default_google_credentials = function(...) {
    function() list(Authorization = "Bearer offline-google-token")
  },
  .package = "ellmer"
)

# The remaining agreed providers are constructed behind loopback endpoints or
# mocked credential boundaries. No test contacts a provider, starts OAuth,
# invokes a CLI, or reads a developer credential file.
if (!requireNamespace("gitcreds", quietly = TRUE)) {
  stop("gitcreds is required to construct a GitHub Models chat offline")
}
old_databricks_token <- Sys.getenv("DATABRICKS_TOKEN", unset = NA_character_)
Sys.setenv(DATABRICKS_TOKEN = "offline-databricks-token")
on.exit({
  if (is.na(old_databricks_token)) {
    Sys.unsetenv("DATABRICKS_TOKEN")
  } else {
    Sys.setenv(DATABRICKS_TOKEN = old_databricks_token)
  }
}, add = TRUE)
databricks_chat <- ellmer::chat_databricks(
  workspace = cloud_base_url, model = "offline-databricks-model"
)
deepseek_chat <- testthat::with_mocked_bindings(
  ellmer::chat_deepseek(base_url = base_url, model = "offline-deepseek-model"),
  deepseek_key = function() "offline-deepseek-key",
  .package = "ellmer"
)
github_chat <- testthat::with_mocked_bindings(
  ellmer::chat_github(base_url = base_url, model = "offline-github-model"),
  github_key = function() "offline-github-token",
  .package = "ellmer"
)
posit_chat <- ellmer::chat_posit(
  base_url = cloud_base_url, model = "offline-posit-model", cache = "none",
  credentials = function() "offline-posit-token"
)
snowflake_chat <- ellmer::chat_snowflake(
  account = "offline-account", model = "offline-snowflake-model",
  credentials = function() list(
    Authorization = "Bearer offline-snowflake-token",
    `X-Snowflake-Authorization-Token-Type` = "OAUTH"
  )
)

cloud_chats <- list(
  bedrock_chat, azure_chat, vertex_chat, databricks_chat, deepseek_chat,
  github_chat, posit_chat, snowflake_chat
)
if (any(vapply(cloud_chats, is.null, logical(1)))) {
  stop("cloud constructor returned no chat object")
}
azure_inventory <- sas2r::sas_llm_models(list(
  provider = "azure", auth_mode = "ambient", endpoint = cloud_base_url,
  api_version = "2025-04-01-preview", model = "offline-azure-deployment",
  credentials = function() "offline-azure-token"
))
if (!identical(azure_inventory$status, "inventory_unavailable")) {
  stop("Azure without models_azure_openai must report inventory_unavailable")
}

# The production regional selector must point at the Bedrock control plane,
# while the replay invocation substitutes a path-distinct loopback control
# endpoint. The replay server rejects runtime or arbitrary GET paths.
production_bedrock_inventory_args <- sas2r:::llm_inventory_args(
  sas2r:::normalize_llm_config(list(
    provider = "bedrock", auth_mode = "ambient", region = "us-east-1",
    profile = "sas2r-offline-contract", model = "offline-bedrock-model"
  ))
)
if (!identical(
  production_bedrock_inventory_args$base_url,
  "https://bedrock.us-east-1.amazonaws.com"
)) {
  stop("production Bedrock inventory selector is not the regional control plane")
}
bedrock_inventory <- testthat::with_mocked_bindings(
  tryCatch(
    ellmer::models_aws_bedrock(
      profile = "sas2r-offline-contract", base_url = bedrock_control_base_url
    ),
    error = identity
  ),
  locate_aws_credentials = function(profile) offline_aws_credentials,
  .package = "ellmer"
)
if (inherits(bedrock_inventory, "error")) {
  stop(
    "models_aws_bedrock did not complete against the distinct control-plane ",
    "replay endpoint: ", conditionMessage(bedrock_inventory)
  )
}

# Vertex has no endpoint override. Stop at its credential callback, before an
# HTTP request can be built, while still exercising the real inventory export.
# ellmer's `check_credentials()` rejects any credentials function that declares
# arguments before it ever calls it, so this boundary must take none.
offline_credentials_boundary <- function() {
  stop(structure(
    list(message = "offline credential boundary", call = NULL),
    class = c("sas2r_offline_credentials_boundary", "error", "condition")
  ))
}
vertex_inventory <- tryCatch(
  ellmer::models_google_vertex(
    location = "us-central1", project_id = "offline-project",
    credentials = offline_credentials_boundary
  ),
  error = identity
)
if (!inherits(vertex_inventory, "sas2r_offline_credentials_boundary")) {
  stop("models_google_vertex bypassed the offline credential boundary")
}

# The remaining public inventory exports are reached with the exact arguments
# the registry builds, and stopped at their credential callback before any
# HTTP request can be built.
for (inventory in list(
  list(export = "models_anthropic", args = list(base_url = cloud_base_url)),
  list(export = "models_deepseek", args = list(base_url = base_url)),
  list(export = "models_github", args = list(base_url = base_url)),
  list(export = "models_google_gemini", args = list(base_url = base_url)),
  list(export = "models_posit", args = list(base_url = cloud_base_url))
)) {
  result <- tryCatch(
    do.call(
      getExportedValue("ellmer", inventory$export),
      c(inventory$args, list(credentials = offline_credentials_boundary))
    ),
    error = identity
  )
  if (!inherits(result, "sas2r_offline_credentials_boundary")) {
    stop(inventory$export, " bypassed the offline credential boundary")
  }
}

# Providers ellmer publishes no inventory for, and a custom GitHub chat
# endpoint without an explicit inventory endpoint, are reported explicitly and
# never as an authoritative empty model list.
for (unavailable in list(
  list(provider = "databricks", auth_mode = "ambient",
       workspace = cloud_base_url, model = "offline-databricks-model"),
  list(provider = "snowflake", auth_mode = "ambient",
       account = "offline-account", model = "offline-snowflake-model"),
  list(provider = "github", auth_mode = "api_key", base_url = base_url,
       model = "offline-github-model")
)) {
  inventory <- sas2r::sas_llm_models(unavailable)
  if (!identical(inventory$status, "inventory_unavailable") ||
      !is.null(inventory$models)) {
    stop("provider ", unavailable$provider,
         " must report inventory_unavailable without a model list")
  }
}

model <- "gpt-4o-mini"
api_key <- "offline-loopback-key"
public_chat <- ellmer::chat_openai(
  base_url = base_url, model = model, api_key = api_key
)
required_methods <- c(
  "set_turns", "get_turns", "register_tool", "chat", "chat_structured",
  "last_turn", "get_tokens", "get_cost"
)
missing_methods <- required_methods[!vapply(
  required_methods,
  function(name) is.function(tryCatch(public_chat[[name]], error = function(e) NULL)),
  logical(1)
)]
if (length(missing_methods)) {
  stop("real ellmer Chat lacks public methods: ", paste(missing_methods, collapse = ", "))
}

production_sources <- paste(c(
  deparse(body(sas2r:::ellmer_prepare_conversation)),
  deparse(body(sas2r:::ellmer_transport_request)),
  deparse(body(sas2r:::ellmer_finish_reason)),
  deparse(body(sas2r:::ellmer_usage)),
  deparse(body(sas2r:::ellmer_counted_turns)),
  deparse(body(sas2r:::ellmer_cost))
), collapse = "\n")
for (method in c(
  "set_turns", "register_tool", "chat", "chat_structured", "last_turn",
  "get_turns", "get_tokens", "get_cost"
)) {
  if (!grepl(method, production_sources, fixed = TRUE)) {
    stop("production adapter no longer calls real Chat$", method)
  }
}

capabilities <- list(
  structured_output = "native",
  tool_calling = "native",
  tools_with_structured_output = "supported",
  max_output_tokens = "supported"
)
adapter <- sas2r:::ellmer_llm(list(
  provider = "openai", model = model, base_url = base_url,
  api_key = api_key, capabilities = capabilities
))
effective_capabilities <- sas2r:::llm_capabilities_for(adapter)
if (!identical(
  effective_capabilities$tools_with_structured_output, "unsupported"
)) {
  stop("ellmer transport constraint did not override configured coexistence")
}
if (!grepl(
  "transport_constraint", effective_capabilities$source, fixed = TRUE
)) {
  stop("effective ellmer capability source lost its transport marker")
}

timeout_adapter <- sas2r:::ellmer_llm(list(
  provider = "openai", model = "offline-timeout-model",
  base_url = base_url, api_key = "offline-loopback-key",
  timeout_seconds = 0.05,
  capabilities = list(structured_output = "fallback")
))
timeout_response <- timeout_adapter$request(sas2r:::llm_request(
  messages = list(list(role = "user", content = "timeout contract")),
  output_schema = list(
    type = "object", properties = list(ok = list(type = "boolean")),
    required = "ok", additionalProperties = FALSE
  ),
  schema_name = "timeout", schema_version = "1", schema_mode = "fallback"
))
if (!identical(timeout_response$status, "failed") ||
    !identical(timeout_response$error$class, "sas2r_llm_timeout")) {
  stop("real ellmer timeout was not normalized")
}

translation_schema <- list(
  type = "object",
  properties = list(
    r_code = list(type = "string"),
    assumptions = list(type = "array", items = list(type = "string")),
    confidence = list(type = "number"),
    flags = list(type = "array", items = list(type = "string"))
  ),
  required = c("r_code", "assumptions", "confidence"),
  additionalProperties = FALSE
)
direct <- adapter$request(sas2r:::llm_request(
  messages = list(
    list(role = "system", content = "policy"),
    list(role = "user", content = "question"),
    list(
      role = "assistant", content = "",
      tool_call = list(
        id = "real_call_1", name = "lookup", arguments = list(name = "round")
      )
    ),
    list(
      role = "tool", name = "lookup", tool_call_id = "real_call_1",
      content = '{"name":"round"}'
    ),
    list(role = "user", content = "final answer")
  ),
  output_schema = translation_schema,
  schema_name = "fixture", schema_version = "1", schema_mode = "native",
  max_output_tokens = 321L
))

lookup_executions <- 0L
lookup <- sas2r:::make_tool(
  "lookup", function(args) {
    lookup_executions <<- lookup_executions + 1L
    list(name = args$name)
  }, max_calls = 2L,
  schema = list(
    type = "object",
    properties = list(
      name = list(type = "string"),
      functions = list(type = "array", items = list(type = "string")),
      operators = list(type = "array", items = list(type = "string")),
      procs = list(type = "array", items = list(type = "string"))
    ),
    required = "name", additionalProperties = FALSE
  )
)
agent <- sas2r:::run_agent(
  list(
    name = "fixture", prompt = "translator.md", tier = "frontier",
    tool_call_limit = 2L, retry_limit = 0L, temperature = NULL,
    max_output_tokens = 321L, output_schema = "program_translation_v1"
  ),
  adapter, list(lookup = lookup), "translate",
  log_dir = tempfile("sas2r-real-ellmer-")
)

if (!identical(direct$data$r_code, "x <- 1")) {
  stop("real Chat$chat_structured did not normalize its structured result")
}
if (!identical(direct$finish_reason, "success")) {
  stop(
    "real Chat$last_turn normalized completed finish reason was not success: ",
    direct$finish_reason %||% "<missing>"
  )
}
if (!identical(direct$usage$input_tokens, 100) ||
    !identical(direct$usage$output_tokens, 25)) {
  stop("real Chat$get_tokens metadata did not normalize")
}
if (is.na(direct$cost$amount_usd) || !is.finite(direct$cost$amount_usd)) {
  stop("real Chat$get_cost(include = 'last') did not report model cost")
}
if (!identical(agent$status, "ok") ||
    !identical(agent$tool_calls, 1L) ||
    !identical(lookup_executions, 1L)) {
  stop("real Chat$register_tool/chat path did not execute exactly one tool call")
}

# The production adapter over the two native (non-OpenAI) protocol families
# ellmer speaks for the agreed providers: Anthropic Messages and Gemini
# generateContent. Both must normalize into the same sas2r_llm_response.
native_request <- function() sas2r:::llm_request(
  messages = list(list(role = "user", content = "native protocol")),
  output_schema = translation_schema, schema_name = "fixture",
  schema_version = "1", schema_mode = "fallback"
)
native_capabilities <- list(
  structured_output = "fallback", tool_calling = "unsupported"
)
anthropic_response <- testthat::with_mocked_bindings(
  sas2r:::ellmer_llm(list(
    provider = "anthropic", auth_mode = "api_key",
    model = "offline-anthropic-model",
    base_url = paste0(cloud_base_url, "/anthropic/v1"),
    capabilities = native_capabilities
  ))$request(native_request()),
  anthropic_key = function() "offline-anthropic-key",
  .package = "ellmer"
)
gemini_response <- sas2r:::ellmer_llm(list(
  provider = "gemini", auth_mode = "ambient",
  model = "offline-gemini-model",
  base_url = paste0(cloud_base_url, "/gemini/v1beta/"),
  credentials = function() "offline-gemini-token",
  capabilities = native_capabilities
))$request(native_request())

for (native in list(
  list(name = "anthropic", response = anthropic_response),
  list(name = "gemini", response = gemini_response)
)) {
  response <- native$response
  if (!inherits(response, "sas2r_llm_response")) {
    stop(native$name, " native protocol did not produce a sas2r_llm_response")
  }
  if (!identical(response$status, "completed") ||
      !identical(response$data$r_code, "x <- 1")) {
    stop(native$name, " native protocol did not normalize its final answer")
  }
  if (!identical(response$finish_reason, "success")) {
    stop(native$name, " native finish reason was not normalized to success: ",
         response$finish_reason %||% "<missing>")
  }
  if (!identical(response$usage$input_tokens, 100) ||
      !identical(response$usage$output_tokens, 25)) {
    stop(native$name, " native usage metadata did not normalize")
  }
  if (!identical(response$provider, native$name)) {
    stop(native$name, " native response lost its provider identity")
  }
}

deadline <- Sys.time() + 5
while ((!file.exists(log_file) || !length(readLines(log_file, warn = FALSE))) &&
       Sys.time() < deadline && server$is_alive()) {
  Sys.sleep(0.05)
}
request_lines <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
requests <- lapply(request_lines[nzchar(request_lines)], function(line) {
  jsonlite::fromJSON(line, simplifyVector = FALSE)
})
timeout_requests <- Filter(function(request) {
  identical(request$body$model %||% NULL, "offline-timeout-model")
}, requests)
if (length(timeout_requests) != 1L) {
  stop(
    "default ellmer retry policy submitted ", length(timeout_requests),
    " timeout attempts instead of one"
  )
}
bedrock_control_requests <- Filter(
  function(request) startsWith(
    request$path, "/bedrock-control/foundation-models"
  ),
  requests
)
if (length(bedrock_control_requests) != 1L) {
  stop("real Bedrock inventory did not use the distinct control-plane replay path")
}
transport_requests <- Filter(
  function(request) grepl("/(chat/completions|responses)$", request$path), requests
)
if (length(transport_requests) < 4L) {
  stop("real ellmer did not make the expected loopback OpenAI requests")
}
anthropic_requests <- Filter(
  function(request) endsWith(request$path, "/anthropic/v1/messages"), requests
)
gemini_requests <- Filter(
  function(request) grepl("^/gemini/v1beta/models/.*:generateContent$",
                          request$path),
  requests
)
if (length(anthropic_requests) != 1L) {
  stop("the production adapter did not reach the Anthropic Messages protocol")
}
if (length(gemini_requests) != 1L) {
  stop("the production adapter did not reach the Gemini generateContent protocol")
}
if (!identical(anthropic_requests[[1]]$body$model, "offline-anthropic-model")) {
  stop("the Anthropic Messages request lost the configured model selector")
}
has_field <- function(request, name) !is.null(request$body[[name]])
contains_pair <- function(value, name, expected) {
  if (!is.list(value)) return(FALSE)
  if (identical(value[[name]], expected)) return(TRUE)
  any(vapply(value, contains_pair, logical(1), name = name, expected = expected))
}
contains_field <- function(value, name) {
  if (!is.list(value)) return(FALSE)
  if (!is.null(value[[name]])) return(TRUE)
  any(vapply(value, contains_field, logical(1), name = name))
}
contains_number <- function(value, name, expected) {
  if (!is.list(value)) return(FALSE)
  candidate <- suppressWarnings(as.numeric(value[[name]]))
  if (length(candidate) == 1L && is.finite(candidate) && candidate == expected) {
    return(TRUE)
  }
  any(vapply(
    value, contains_number, logical(1), name = name, expected = expected
  ))
}
contains_value <- function(value, expected) {
  if (is.atomic(value) && length(value) == 1L) {
    return(identical(value, expected))
  }
  if (!is.list(value)) return(FALSE)
  any(vapply(value, contains_value, logical(1), expected = expected))
}
contains_text <- function(value, expected) {
  if (is.character(value)) {
    return(any(grepl(expected, value, fixed = TRUE)))
  }
  if (!is.list(value)) return(FALSE)
  any(vapply(value, contains_text, logical(1), expected = expected))
}
count_value <- function(value, expected) {
  own <- is.atomic(value) && length(value) == 1L && identical(value, expected)
  if (!is.list(value)) return(as.integer(own))
  as.integer(own) + sum(vapply(value, count_value, integer(1), expected = expected))
}
wire_history_positions <- function(request) {
  items <- request$body$messages %||% request$body$input
  if (!is.list(items)) return(NULL)
  first_position <- function(predicate) {
    positions <- which(vapply(items, predicate, logical(1)))
    if (length(positions)) positions[[1]] else NA_integer_
  }
  c(
    original_user = first_position(function(item) contains_value(item, "translate")),
    assistant_tool_request = first_position(function(item) {
      (contains_pair(item, "role", "assistant") &&
         contains_field(item, "tool_calls")) ||
        contains_pair(item, "type", "function_call")
    }),
    paired_tool_result = first_position(function(item) {
      contains_pair(item, "role", "tool") ||
        contains_pair(item, "type", "function_call_output")
    }),
    assistant_answer = first_position(function(item) {
      contains_value(item, "gathered context")
    }),
    current_final_prompt = first_position(function(item) {
      contains_value(
        item, "Return the complete final answer in the required schema."
      )
    })
  )
}
has_structured_format <- function(request) {
  has_field(request, "response_format") ||
    contains_pair(request$body, "type", "json_schema") ||
    contains_pair(request$body, "type", "json_object")
}
if (!any(vapply(transport_requests, has_structured_format, logical(1)))) {
  stop("real Chat$chat_structured did not send native response_format")
}
if (!any(vapply(transport_requests, has_field, logical(1), name = "tools"))) {
  stop("real Chat$register_tool did not send a native tool definition")
}
if (!any(vapply(
  transport_requests,
  function(request) {
    contains_pair(request$body, "role", "tool") ||
      contains_pair(request$body, "type", "function_call_output")
  }, logical(1)
))) {
  stop("real Chat$chat did not submit the auto-executed tool result")
}
final_prompt <- "Return the complete final answer in the required schema."
finalization_requests <- Filter(function(request) {
  has_structured_format(request) && contains_value(request$body, final_prompt)
}, transport_requests)
if (length(finalization_requests) != 1L) {
  stop("expected exactly one final structured request over gathered history")
}
finalization_request <- finalization_requests[[1]]
if (has_field(finalization_request, "tools") &&
    length(finalization_request$body$tools)) {
  stop("ellmer finalization request still contained registered tools")
}
if (!contains_text(
  finalization_request$body,
  "You translate SAS source code into an idiomatic, faithful R script"
)) {
  stop("final structured request lost its system policy")
}
positions <- wire_history_positions(finalization_request)
if (is.null(positions) || anyNA(positions) ||
    !all(diff(unname(positions)) > 0L)) {
  stop(
    "final structured request did not preserve ordered user/tool/answer history: ",
    paste(names(positions), positions, sep = "=", collapse = ", ")
  )
}
if (count_value(finalization_request$body, "real_auto_1") < 2L) {
  stop("final structured request did not retain a paired tool request/result ID")
}
if (!any(vapply(
  transport_requests,
  function(request) any(vapply(
    c("max_tokens", "max_completion_tokens", "max_output_tokens"),
    function(name) contains_number(request$body, name, 321), logical(1)
  )), logical(1)
))) {
  stop("ellmer::params(max_tokens = 321) did not reach the real provider request")
}

tool_definitions <- unlist(lapply(transport_requests, function(request) {
  request$body$tools %||% list()
}), recursive = FALSE)
tool_parameters <- lapply(tool_definitions, function(tool) {
  tool[["function"]]$parameters %||% tool$parameters
})
tool_parameters <- Filter(Negate(is.null), tool_parameters)
# `name` is the only required argument. ellmer encodes the optional ones
# either by omitting them from `required` or, under the provider's strict
# function-calling schema, by publishing them with a nullable type.
tool_schema_matches <- function(parameters) {
  properties <- parameters$properties %||% list()
  if (!setequal(
    names(properties), c("name", "functions", "operators", "procs")
  )) {
    return(FALSE)
  }
  required <- as.character(unlist(parameters$required %||% character()))
  nullable <- names(properties)[vapply(names(properties), function(name) {
    "null" %in% as.character(unlist(properties[[name]]$type %||% character()))
  }, logical(1))]
  optional <- union(setdiff(names(properties), required), nullable)
  setequal(optional, c("functions", "operators", "procs")) &&
    "name" %in% required
}
if (!any(vapply(tool_parameters, tool_schema_matches, logical(1)))) {
  stop("real Chat$register_tool did not receive the exact required/optional schema")
}

for (native in list(
  list(name = "anthropic", request = anthropic_requests[[1]]),
  list(name = "gemini", request = gemini_requests[[1]])
)) {
  if (!contains_text(native$request$body, "native protocol")) {
    stop(native$name, " native protocol request lost its user turn")
  }
  if (contains_text(native$request$body, "offline-anthropic-key") ||
      contains_text(native$request$body, "offline-gemini-token")) {
    stop(native$name, " native protocol request body carried a credential")
  }
}

message(
  "real ellmer loopback contract passed: version=", installed_version,
  " path=", ellmer_path,
  " requests=", length(transport_requests),
  " native=", length(anthropic_requests) + length(gemini_requests)
)
}

run_real_ellmer_contract()
