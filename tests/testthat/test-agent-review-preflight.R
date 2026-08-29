test_that("Tasks 15A-C prerequisites expose one normalized agent contract", {
  ns <- asNamespace("sas2r")
  required <- c(
    "load_semantic_registry", "semantic_rule",
    "new_llm", "llm_request", "llm_capabilities", "effective_model_params",
    "resolve_model_capabilities",
    "normalize_provider_response", "attempt_llm_request",
    "new_usage_budget", "read_usage_ledger", "sas_llm_models",
    "sas_llm_probe"
  )
  expect_true(all(vapply(required, exists, logical(1),
                         envir = ns, mode = "function", inherits = FALSE)))
})

test_that("CAMIS-backed rounding semantics remain explicit", {
  round <- sas2r:::semantic_rule("functions.round")
  expect_identical(round$classification, "adapter_required")
  expect_identical(round$strategy, "sas_round")
  expect_match(round$sas_default, "away from zero", ignore.case = TRUE)
  expect_match(round$r_default, "ties to even", ignore.case = TRUE)
  expect_true(any(grepl("CAMIS/Comp/r-sas_rounding",
                        vapply(round$sources, `[[`, "", "url"))))
})

test_that("unknown temperature is omitted and a denied request never calls transport", {
  req <- sas2r:::llm_request(
    messages = list(list(role = "user", content = "ping")),
    tier = "frontier", temperature = 0
  )
  caps <- sas2r:::llm_capabilities(temperature = "unknown")
  expect_null(sas2r:::effective_model_params(req, caps)$temperature)

  calls <- 0L
  llm <- sas2r:::new_llm(function(request) {
    calls <<- calls + 1L
    structure(list(status = "completed", action = "final",
                   data = list(ok = TRUE), usage = list()),
              class = "sas2r_llm_response")
  }, provider = "mock")
  budget <- sas2r:::new_usage_budget(mode = "soft", max_calls = 0L)
  result <- sas2r:::attempt_llm_request(req, llm, usage_budget = budget,
                                        audit_context = list(purpose = "preflight"))
  expect_identical(result$status, "budget_exhausted")
  expect_identical(calls, 0L)
})
