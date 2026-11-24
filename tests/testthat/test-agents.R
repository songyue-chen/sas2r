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

test_that("lookup tools share the agent allowance and honor explicit overrides", {
  specs <- load_agent_specs()
  for (role in names(specs)) {
    for (tool in specs[[role]]$tools) {
      expect_identical(tool$max_calls, specs[[role]]$tool_call_limit)
    }
  }
  expect_null(specs$fixer$tools$list_macro_files)
  dir <- withr::local_tempdir()
  overrides <- file.path(dir, ".sas2r", "agents")
  dir.create(overrides, recursive = TRUE)
  writeLines(c("tool_call_limit: 30", "tools:",
               "  search_skills: { max_calls: 4 }"),
             file.path(overrides, "translator.yml"))
  spec <- load_agent_specs(dir)$translator
  expect_identical(spec$tools$search_skills$max_calls, 4L)
  expect_identical(spec$tools$lookup_rulebook$max_calls, 30L)
})

test_that("each agent's prompt carries its unit, so no tool repeats it", {
  # read_unit_context returned the SAS unit and the inferred input schemas.
  # Every request now carries both: it was the first call in all 61 units of a
  # live sweep, 12.6% of every call made, and could only repeat the prompt. The
  # fixer was the last role to keep it, for the schemas its prompt lacked.
  specs <- load_agent_specs()
  prompt_text <- function(spec) {
    path <- system.file("prompts", spec$prompt, package = "sas2r")
    if (!nzchar(path) || !file.exists(path)) return("")
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  for (name in migration_agent_names()) {
    body <- prompt_text(specs[[name]])
    expect_match(body, "{{unit}}", fixed = TRUE, info = name)
    expect_true(grepl("{{context}}", body, fixed = TRUE) ||
      grepl("{{input_schemas}}", body, fixed = TRUE), info = name)
    expect_false("read_unit_context" %in% names(specs[[name]]$tools %||% list()), info = name)
  }
  expect_false("read_unit_context" %in% names(TOOL_ARGUMENT_SCHEMAS))
  expect_false("read_unit_context" %in% names(TOOL_IMPLS))
  expect_false("read_unit_context" %in% skill_allowed_tools())
})

test_that("a project override naming the removed unit tool registers nothing", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, ".sas2r", "agents"), recursive = TRUE)
  yaml::write_yaml(list(tools = list(read_unit_context = list())),
    file.path(root, ".sas2r", "agents", "fixer.yml"))
  spec <- load_agent_specs(root)$fixer
  tools <- build_tools(spec, list(agent_role = "fixer"))
  expect_false("read_unit_context" %in% names(tools))
  expect_true("get_macro_source" %in% names(tools))
})

test_that("migration_agent_names returns exact active worker roles", {
  expect_identical(migration_agent_names(), c("translator", "reviewer", "fixer"))
})

