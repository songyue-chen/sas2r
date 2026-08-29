test_that("shipped specs load, validate, and carry required fields", {
  specs <- load_agent_specs()
  expect_setequal(names(specs), c("translator", "fixer", "reviewer"))
  tr <- specs$translator
  for (f in c("name", "description", "prompt", "tier", "tools",
              "tool_call_limit", "retry_limit",
              "output_schema", "on_budget_exhausted")) {
    expect_true(!is.null(tr[[f]]), info = f)
    expect_true(!is.null(specs$reviewer[[f]]), info = paste0("reviewer:", f))
  }
  expect_identical(specs$fixer$max_repair_iterations, 3L)
  expect_identical(tr$temperature, 0)
  expect_identical(specs$reviewer$temperature, 0)
  expect_false(isTRUE(tr$tools$search_docs$enabled))   # off by default
})

test_that("temperature is an optional agent preference", {
  dir <- withr::local_tempdir()
  ov <- file.path(dir, ".sas2r", "agents")
  dir.create(ov, recursive = TRUE)
  writeLines("temperature: null", file.path(ov, "translator.yml"))

  specs <- load_agent_specs(project_dir = dir)

  expect_null(specs$translator$temperature)
})

test_that("project overrides deep-merge and can disable tools", {
  dir <- withr::local_tempdir()
  ov <- file.path(dir, ".sas2r", "agents"); dir.create(ov, recursive = TRUE)
  writeLines(c("tool_call_limit: 5",
               "tools:",
               "  lookup_rulebook: { enabled: false }"),
             file.path(ov, "translator.yml"))
  specs <- load_agent_specs(project_dir = dir)
  expect_identical(specs$translator$tool_call_limit, 5L)
  expect_false("lookup_rulebook" %in% names(specs$translator$tools))
})

test_that("immutable fields reject violating overrides", {
  dir <- withr::local_tempdir()
  ov <- file.path(dir, ".sas2r", "agents"); dir.create(ov, recursive = TRUE)
  writeLines("on_budget_exhausted: silently_accept",
             file.path(ov, "translator.yml"))
  expect_error(load_agent_specs(project_dir = dir),
               class = "sas2r_agent_spec_error")
})

test_that("translator and fixer carry macro-hunting tools with correct budgets", {
  specs <- load_agent_specs()
  expect_identical(specs$translator$tools$find_macro$max_calls, 4L)
  expect_identical(specs$translator$tools$get_macro_source$max_calls, 3L)
  expect_identical(specs$translator$tools$list_macro_files$max_calls, 2L)

  expect_identical(specs$fixer$tools$find_macro$max_calls, 3L)
  expect_identical(specs$fixer$tools$get_macro_source$max_calls, 2L)
  expect_null(specs$fixer$tools$list_macro_files)
})

test_that("an agent is only offered read_unit_context when its prompt lacks the unit", {
  # The tool returns ctx$unit_stmts$text and infer_schemas(project) -- the very
  # objects that build {{unit}} and {{context}}. Where a prompt already renders
  # both, the tool can only repeat them: it was the first call in all 61 units
  # of a live sweep, 12.6% of every call made. Where a prompt renders neither,
  # it is the agent's only route to that information and must stay.
  specs <- load_agent_specs()
  prompt_text <- function(spec) {
    path <- system.file("prompts", spec$prompt, package = "sas2r")
    if (!nzchar(path) || !file.exists(path)) return("")
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }

  for (name in names(specs)) {
    spec <- specs[[name]]
    body <- prompt_text(spec)
    self_sufficient <- grepl("{{unit}}", body, fixed = TRUE) &&
      grepl("{{context}}", body, fixed = TRUE)
    offered <- "read_unit_context" %in% names(spec$tools %||% list())
    if (self_sufficient) {
      expect_false(offered, info = paste(name, "prompt already carries unit and context"))
    }
  }
})

test_that("agents whose prompt lacks the unit keep the tool", {
  specs <- load_agent_specs()

  expect_true("read_unit_context" %in% names(specs$fixer$tools))
})

test_that("migration_agent_names returns exact active worker roles", {
  expect_identical(migration_agent_names(), c("translator", "reviewer", "fixer"))
})

