# Offline feasibility gate: do ellmer's public async calls retain sas2r's
# per-request timeout scope? Only the existing loopback replay server is used.
local({
  pkgload::load_all(".", quiet = TRUE)
  port_file <- tempfile("sas2r-async-port-")
  log_file <- tempfile("sas2r-async-requests-")
  server <- processx::process$new(Sys.which("python3"), c(
    "tests/real-ellmer/replay_server.py", "--port-file", port_file,
    "--log-file", log_file
  ), stdout = "|", stderr = "|", cleanup_tree = TRUE)
  on.exit(server$kill(), add = TRUE)
  deadline <- Sys.time() + 10
  while (!file.exists(port_file) && server$is_alive() && Sys.time() < deadline) {
    Sys.sleep(0.02)
  }
  stopifnot(file.exists(port_file))
  base <- sprintf("http://127.0.0.1:%s/v1", readLines(port_file, warn = FALSE)[[1L]])
  previous <- options(ellmer_timeout_s = 1, ellmer_max_tries = 1)
  on.exit(options(previous), add = TRUE)
  chat <- function() ellmer::chat_openai_compatible(base_url = base,
    api_key = "offline-test-key", model = "offline-timeout-model", echo = "none")
  limits <- get("with_ellmer_limits", asNamespace("sas2r"))
  synchronous <- tryCatch(limits(0.02, 1L, chat()$chat("offline timeout check")),
    error = identity)
  completed <- list()
  for (name in c("short", "long")) {
    local({
      id <- name
      timeout <- if (id == "short") 0.02 else 0.5
      pending <- limits(timeout, 1L,
        chat()$chat_async("offline timeout check", tool_mode = "sequential"))
      promises::then(pending,
        onFulfilled = function(value) completed[[id]] <<- "completed",
        onRejected = function(error) completed[[id]] <<- class(error)[[1L]])
    })
  }
  deadline <- Sys.time() + 10
  while (length(completed) < 2L && Sys.time() < deadline) later::run_now(0.05)
  stopifnot(length(completed) == 2L)
  result <- list(
    ellmer = as.character(utils::packageVersion("ellmer")),
    synchronous_short_timeout_rejected = inherits(synchronous, "error"),
    async_short = completed$short, async_long = completed$long,
    gate = if (inherits(synchronous, "error") &&
      identical(completed$short, "completed")) "failed_request_settings_scope" else
      "requires_further_investigation",
    external_provider_calls = 0L,
    explanation = paste("The replay endpoint delays 150 ms. The 20 ms synchronous",
      "request should fail, as must an equivalent asynchronous request.",
      "A completed short async request shows the scoped options were lost.",
      "This rejects the direct public-API substitution, not asynchronous R itself.")
  )
  cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
})
