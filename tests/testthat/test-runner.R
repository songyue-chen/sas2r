spec_min <- function(schema = "program_translation_v1", tool_limit = 3, retries = 1)
  list(name = "t", prompt = "translator.md", tools = list(),
       tool_call_limit = tool_limit, retry_limit = retries,
       temperature = 0, output_schema = schema, on_budget_exhausted = "downgrade")

good <- list(
  type = "final",
  data = list(
    r_code = "x <- 1",
    summary = "test translation",
    parameters = list(),
    defaults = list(),
    reads = character(),
    writes = character(),
    side_effects = character(),
    helper_use = character(),
    discovered_dependencies = character(),
    suspected_dependencies = character(),
    affected_outputs = character(),
    uncertainty = list()
  )
)

test_that("happy path returns validated data", {
  r <- run_agent(spec_min(), mock_llm(list(good)), tools = list(),
                 user_content = "unit", log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
  expect_identical(r$data$r_code, "x <- 1")
})

test_that("an agent without a log destination only writes temporary audit files", {
  working <- withr::local_tempdir()
  withr::local_dir(working)
  result <- run_agent(spec_min(), mock_llm(list(good)), tools = list(),
                       user_content = "unit")
  expect_identical(result$status, "ok")
  expect_true(file.exists(file.path(tempdir(), "llm_log.jsonl")))
  expect_length(list.files(working, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("invalid output retries with feedback, then downgrades", {
  bad <- list(type = "final", data = list(assumptions = list()))
  r <- run_agent(spec_min(retries = 1), mock_llm(list(bad, bad)), list(),
                 "unit", log_dir = withr::local_tempdir())
  expect_identical(r$status, "invalid_output")
  r2 <- run_agent(spec_min(retries = 1), mock_llm(list(bad, good)), list(),
                  "unit", log_dir = withr::local_tempdir())
  expect_identical(r2$status, "ok")
})

test_that("schema repair is submitted as a current user turn", {
  bad <- list(type = "final", data = list(assumptions = list()))
  responses <- list(bad, good)
  requests <- list()
  llm <- new_llm(function(request) {
    requests[[length(requests) + 1L]] <<- request
    normalize_provider_response(
      responses[[length(requests)]], request = request, provider = "mock"
    )
  }, provider = "mock")

  result <- run_agent(
    spec_min(retries = 1L), llm, list(), "unit",
    log_dir = withr::local_tempdir()
  )

  expect_identical(result$status, "ok")
  expect_identical(
    vapply(requests[[2]]$messages, `[[`, "", "role"),
    c("system", "user", "assistant", "user")
  )
  expect_match(requests[[2]]$messages[[3]]$content, "assumptions")
})

test_that("tool calls route through budgets and exhaustion downgrades", {
  tool_resp <- list(type = "tool", tool = "echo", args = list(x = 1))
  tools <- list(echo = make_tool(
    "echo", function(a) list(got = a$x), 5,
    schema = list(
      type = "object", properties = list(x = list(type = "number")),
      required = "x", additionalProperties = FALSE
    )
  ))
  r <- run_agent(spec_min(tool_limit = 1),
                 mock_llm(list(tool_resp, good)), tools, "unit",
                 log_dir = withr::local_tempdir())
  expect_identical(r$status, "ok")
  expect_identical(r$tool_calls, 1L)
  # Exhausting the tool allowance no longer discards the unit: the model is
  # asked to answer with what it gathered, and that answer is used.
  r2 <- run_agent(spec_min(tool_limit = 1),
                  mock_llm(list(tool_resp, tool_resp, good)), tools, "unit",
                  log_dir = withr::local_tempdir())
  expect_identical(r2$status, "ok")
  expect_identical(r2$tool_calls, 1L)
})
