# These tests run the real ellmer/httr2/curl request path against a loopback
# server that replays a scripted provider response. No provider is contacted.

scripted_anthropic_llm <- function(server, env = parent.frame()) {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key-not-a-secret", .local_envir = env)
  ellmer_llm(list(
    provider = "anthropic", model = "claude-scripted", base_url = server$url,
    capabilities = list(
      structured_output = "native", tool_calling = "native",
      max_output_tokens = "supported"
    )
  ))
}

skip_unless_scripted_requests_can_run <- function() {
  skip_on_cran()
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")
  skip_if_not_installed("coro")
}

test_that("a tool-phase request to Anthropic is sent as a stream and returns the answer", {
  # Break caught: sending the request as one whole response leaves the
  # connection silent while the model works, and a network that closes silent
  # connections then ends every long request.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = NULL)
  server <- local_scripted_http_server(list(
    anthropic_event_stream(list(list(type = "text", text = "the answer")),
                           output_tokens = 9L)
  ))
  llm <- scripted_anthropic_llm(server)

  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    phase = "gathering"
  ))

  sent <- server$requests()
  expect_length(sent, 1L)
  expect_true(sent[[1]]$stream)
  expect_identical(response$status, "completed")
  expect_identical(response$data$gathered, "the answer")
  expect_identical(response$usage$total_input_tokens, 11)
  expect_identical(response$usage$total_output_tokens, 9)
})

test_that("a streamed tool loop runs the tool and returns the answer written after it", {
  # Break caught: a streamed request that drops the tool round, the tool
  # result, or the allowance callbacks would answer without what it looked up.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = NULL)
  server <- local_scripted_http_server(list(
    anthropic_event_stream(
      list(list(type = "tool_use", id = "toolu_1", name = "lookup",
                input = list(name = "visit"))),
      stop_reason = "tool_use"
    ),
    anthropic_event_stream(list(list(type = "text", text = "after the tool")))
  ))
  llm <- scripted_anthropic_llm(server)
  looked_up <- character()
  state <- new_agent_tool_state(3L)
  tools <- bind_transport_tool_limits(list(lookup = make_tool(
    "lookup",
    function(args) {
      looked_up <<- c(looked_up, args$name)
      list(found = args$name)
    },
    20L,
    schema = closed_tool_schema(list(name = list(type = "string")), required = "name")
  )), state)

  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    tools = tools, phase = "gathering"
  ))

  sent <- server$requests()
  expect_length(sent, 2L)
  expect_true(sent[[1]]$stream)
  expect_true(sent[[2]]$stream)
  expect_identical(looked_up, "visit")
  expect_identical(state$count, 1L)
  result_blocks <- unlist(lapply(sent[[2]]$messages, function(message) {
    vapply(message$content, function(block) block$type %||% "", character(1))
  }))
  expect_true("tool_result" %in% result_blocks)
  expect_identical(response$status, "completed")
  expect_identical(response$data$gathered, "after the tool")
})

test_that("a stream that ends before the provider closes it is a transient transport failure", {
  # Break caught: a stream cut midway must not be read as a finished answer,
  # and must be classed so the runner retries it like any dropped connection.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = NULL)
  server <- local_scripted_http_server(list(
    anthropic_event_stream(list(list(type = "text", text = "half an ans")),
                           complete = FALSE)
  ))
  llm <- scripted_anthropic_llm(server)

  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    phase = "gathering"
  ))

  expect_true(server$requests()[[1]]$stream)
  expect_identical(response$status, "failed")
  expect_identical(response$error$class, "sas2r_llm_transport_error")
  expect_null(response$data$gathered)
})

test_that("streaming can be switched off, and the request is then sent whole", {
  # Break caught: without a way back, a provider or proxy that mishandles
  # streamed requests would stop every run until a new release.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = FALSE)
  server <- local_scripted_http_server(
    list(anthropic_message_body(list(list(type = "text", text = "whole answer")))),
    content_type = "application/json"
  )
  llm <- scripted_anthropic_llm(server)

  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    phase = "gathering"
  ))

  expect_false(isTRUE(server$requests()[[1]]$stream))
  expect_identical(response$status, "completed")
  expect_identical(response$data$gathered, "whole answer")
})

test_that("only providers whose cut streams raise an error are streamed by default", {
  # Break caught: ellmer reads an early end of stream as a finished turn for
  # most providers, so streaming them by default could pass off a cut answer
  # as complete. They stay on whole responses unless the option says otherwise.
  withr::local_options(sas2r.stream_requests = NULL)
  expect_true(ellmer_streams_requests(list(provider = "anthropic")))
  for (provider in c("openai", "gemini", "bedrock", "azure", "ollama", "deepseek")) {
    expect_false(ellmer_streams_requests(list(provider = provider)), info = provider)
  }
  withr::local_options(sas2r.stream_requests = TRUE)
  expect_true(ellmer_streams_requests(list(provider = "openai")))
  withr::local_options(sas2r.stream_requests = FALSE)
  expect_false(ellmer_streams_requests(list(provider = "anthropic")))
})

test_that("a chat object without a stream method still gets a whole-response request", {
  # Break caught: test doubles and older transports expose only `chat()`;
  # asking them to stream would fail every request.
  chat <- list(chat = function(prompt) paste("whole:", prompt))
  expect_identical(
    ellmer_gather_tools(chat, "prompt", list(), stream = TRUE), "whole: prompt"
  )
})

# One scripted tool-phase exchange, sent streamed or whole. The model asks for
# the same tool twice while the allowance is one call, so the package closes
# the tools after the first result.
tools_closed_exchange <- function(stream) {
  withr::local_options(sas2r.stream_requests = stream)
  blocks <- list(
    list(type = "text", text = "Looking it up."),
    list(type = "tool_use", id = "toolu_1", name = "lookup", input = list(name = "visit"))
  )
  server <- if (stream) {
    local_scripted_http_server(list(anthropic_event_stream(blocks, stop_reason = "tool_use")))
  } else {
    local_scripted_http_server(
      list(anthropic_message_body(blocks, stop_reason = "tool_use")),
      content_type = "application/json"
    )
  }
  llm <- scripted_anthropic_llm(server)
  state <- new_agent_tool_state(1L)
  tools <- bind_transport_tool_limits(list(lookup = make_tool(
    "lookup", function(args) list(found = args$name), 20L,
    schema = closed_tool_schema(list(name = list(type = "string")), required = "name")
  )), state)
  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    tools = tools, phase = "gathering"
  ))
  list(
    status = response$status, gathered = response$data$gathered,
    conversation = lapply(response$conversation, function(message) {
      message[intersect(c("role", "content", "name", "tool_call"), names(message))]
    }),
    tool_calls = state$count, exhausted = isTRUE(state$exhausted),
    requests = length(server$requests())
  )
}

test_that("closing the tools at the allowance gives the same result streamed or whole", {
  # Break caught: the allowance is enforced by stopping ellmer's loop from a
  # callback. If that stop behaved differently inside a streamed loop, a
  # component would lose its gathered conversation or keep calling tools.
  skip_unless_scripted_requests_can_run()

  streamed <- tools_closed_exchange(stream = TRUE)
  whole <- tools_closed_exchange(stream = FALSE)

  expect_identical(streamed$gathered, AGENT_TOOL_LIMIT_MESSAGE)
  expect_true(streamed$exhausted)
  expect_identical(streamed$requests, 1L)
  expect_identical(streamed, whole)
})

test_that("a streamed request records the provider's token totals and never a wrong reasoning count", {
  # Break caught: usage read from a streamed turn that loses the output total,
  # or that reports a reasoning count the provider did not send. ellmer 0.4.2
  # and 0.5.0 keep only the output total from the end of a stream, so the
  # reasoning count is unknown there; ellmer 0.5.2 carries the provider's whole
  # usage, so the count is recorded. Either is right; any other number is not.
  skip_unless_scripted_requests_can_run()
  blocks <- list(list(type = "text", text = "answer"))
  ask <- function(llm) llm$request(llm_request(
    messages = list(list(role = "user", content = "question")), phase = "gathering"
  ))$usage

  withr::local_options(sas2r.stream_requests = TRUE)
  streamed <- ask(scripted_anthropic_llm(local_scripted_http_server(
    list(anthropic_event_stream(blocks, output_tokens = 9L))
  )))
  withr::local_options(sas2r.stream_requests = FALSE)
  whole <- ask(scripted_anthropic_llm(local_scripted_http_server(
    list(anthropic_message_body(blocks, output_tokens = 9L)),
    content_type = "application/json"
  )))

  expect_identical(streamed$total_output_tokens, 9)
  expect_identical(whole$total_output_tokens, 9)
  expect_identical(whole$reasoning_tokens, 4)
  expect_true(
    is.na(streamed$reasoning_tokens) || identical(streamed$reasoning_tokens, 4),
    info = paste("streamed reasoning count:", format(streamed$reasoning_tokens))
  )
})

test_that("a provider error sent inside a stream is a transient transport failure", {
  # Break caught: once a stream has started, a provider reports a server-side
  # failure as an event, not as an HTTP status. Sent whole, the same failure is
  # a 5xx that the runner retries; left unclassed it would not be retried.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = NULL)
  server <- local_scripted_http_server(list(anthropic_event_stream(
    list(list(type = "text", text = "half an ans")),
    error = list(type = "api_error", message = "Internal server error")
  )))
  llm <- scripted_anthropic_llm(server)

  response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    phase = "gathering"
  ))

  expect_identical(response$status, "failed")
  expect_identical(response$error$class, "sas2r_llm_transport_error")
  expect_match(response$error$message, "Internal server error", fixed = TRUE)
})

test_that("the runner retries a stream cut midway and uses the answer from the retry", {
  # Break caught: the whole chain a real run depends on. A cut stream has to
  # come back from the transport as a transient failure, the agent loop has to
  # send the request again, and the answer streamed on the second try has to
  # pass the same schema check as an answer sent whole.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = NULL, sas2r.agent_backoff_base = 0)
  answer <- as.character(jsonlite::toJSON(list(
    r_code = "x <- 1", summary = "test translation",
    parameters = list(), defaults = list(), reads = character(),
    writes = character(), side_effects = character(), helper_use = character(),
    discovered_dependencies = character(), suspected_dependencies = character(),
    affected_outputs = character(), uncertainty = list()
  ), auto_unbox = TRUE))
  server <- local_scripted_http_server(list(
    anthropic_event_stream(list(list(type = "text", text = "{\"r_code\": \"x <-")),
                           complete = FALSE),
    anthropic_event_stream(list(list(type = "text", text = answer)))
  ))
  llm <- scripted_anthropic_llm(server)
  echo <- make_tool(
    "echo", function(args) list(got = args$x), max_calls = 2L,
    schema = closed_tool_schema(list(x = list(type = "number")), required = "x")
  )

  result <- run_agent(
    list(name = "t", prompt = "translator.md", tools = list(), tool_call_limit = 2,
         retry_limit = 1, output_schema = "program_translation_v1",
         on_budget_exhausted = "downgrade"),
    llm, tools = list(echo = echo), user_content = "unit",
    log_dir = withr::local_tempdir()
  )

  sent <- server$requests()
  expect_length(sent, 2L)
  expect_true(sent[[1]]$stream && sent[[2]]$stream)
  expect_identical(result$status, "ok")
  expect_identical(result$data$r_code, "x <- 1")
})

test_that("a session's ellmer echo setting does not change how a request is sent", {
  # Break caught: with `ellmer_echo` set, or with the caller counted as user
  # facing, ellmer streams the request and prints the model's text on its own.
  # The package would then send a request it meant to send whole as a stream,
  # and write model output to the console and worker logs.
  skip_unless_scripted_requests_can_run()
  withr::local_options(sas2r.stream_requests = FALSE, ellmer_echo = "output")
  server <- local_scripted_http_server(
    list(anthropic_message_body(list(list(type = "text", text = "whole answer")))),
    content_type = "application/json"
  )
  llm <- scripted_anthropic_llm(server)

  printed <- utils::capture.output(response <- llm$request(llm_request(
    messages = list(list(role = "user", content = "question")),
    phase = "gathering"
  )))

  expect_false(isTRUE(server$requests()[[1]]$stream))
  expect_identical(response$data$gathered, "whole answer")
  expect_identical(printed, character())
})
