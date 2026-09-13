test_that("helper documentation stays synchronized with the runtime help", {
  man_dir <- testthat::test_path("..", "..", "man")
  skip_if_not(dir.exists(man_dir))
  expect_equal(helper_documentation(), runtime_helper_documentation(man_dir))
})

test_that("helper call checks reject invented arguments using actual signatures", {
  for (code in c('sas_merge(a, b, by = "id", all.x = TRUE)',
                 'sas_merge(a, b, by = "id", in_a = "left")',
                 'sas_sort(df, "id", nodupkey = TRUE)',
                 'sas_if_else(test = x, yes = y, no = z)')) {
    expect_true(any(lint_r_code(code)$kind == "helper_misuse"), info = code)
  }
  for (code in c('sas_merge(a, b, "id", "left")',
                 'sas_merge(by = "id", b = right, a = left, keep = "full")',
                 'sas_sum(x, y, z)', 'sas_min(a = x, b = y)',
                 'wrapper <- function(...) sas_sum(...)',
                 'x |> sas_sort("id")')) {
    expect_false(any(lint_r_code(code)$kind == "helper_misuse"), info = code)
  }
})

test_that("helper call violations fail the program gate even without declared helper_use", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines('out <- sas_merge(a, b, by = "id", type = "left")', path)
  checks <- check_program_revision(path, new_behavioral_contract("arbitrary_program"))
  expect_false(checks$pass)
  expect_true(any(grepl("unused argument", checks$errors)))
})

test_that("a missing helper reference gives an actionable error instead of empty interfaces", {
  for (path in c("", tempfile("missing-reference-"))) {
    lookup <- helper_documentation
    environment(lookup) <- list2env(list(system.file = function(...) path),
                                    parent = environment(helper_documentation))
    expect_error(lookup(), "Reinstall sas2r", class = "sas2r_helper_reference_missing")
  }
})

test_that("the shared helper reference includes argument rules and return values", {
  docs <- helper_documentation()
  expect_match(docs$chr_cmp$text, 'op: NULL for a three-way comparison', fixed = TRUE)
  expect_match(docs$chr_cmp$text, 'integer vector of -1, 0, 1', fixed = TRUE)
  expect_match(docs$chr_cmp$text, 'op = "=="', fixed = TRUE)
  expect_match(docs$sas_sort$text, 'descending: Character vector', fixed = TRUE)
  expect_match(docs$sas_sort$text, 'Returns:\ndf reordered.', fixed = TRUE)
  expect_match(docs$sas_merge$text, 'a, b: Data frames, in statement order.', fixed = TRUE)
  expect_match(docs$sas_merge$text, 'Returns:\nThe merged data frame.', fixed = TRUE)
})

test_that("all three agents receive complete helper contracts without tool calls", {
  specs <- load_agent_specs()
  responses <- list(translator = valid_program_translation_response(),
                    reviewer = valid_program_review_response(),
                    fixer = valid_program_fix_response())
  for (agent in names(responses)) {
    captured <- NULL
    llm <- new_llm(function(request) {
      captured <<- request
      normalize_provider_response(responses[[agent]], request = request, provider = "mock")
    }, provider = "mock")
    result <- run_agent(as.list(specs[[agent]]), llm, tools = list(),
      user_content = "Compare source and translated values.", log_dir = withr::local_tempdir())
    expect_identical(result$status, "ok", info = agent)
    expect_equal(result$tool_calls, 0L, info = agent)
    system <- captured$messages[[1L]]$content
    expect_match(system, 'op: NULL for a three-way comparison', fixed = TRUE, info = agent)
    expect_match(system, 'integer vector of -1, 0, 1', fixed = TRUE, info = agent)
    expect_match(system, 'op = "=="', fixed = TRUE, info = agent)
    expect_match(system, 'Returns:\nThe merged data frame.', fixed = TRUE, info = agent)
  }
})
