# Which requests are streamed. The tests that send a request to a local test
# server stay in the development suite.

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

