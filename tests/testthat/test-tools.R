test_that("tool budgets refuse once then abort", {
  t <- make_tool("x", function(args) "ok", max_calls = 1)
  expect_identical(t$call(list()), "ok")
  r <- t$call(list())
  expect_identical(r$error, "budget_exhausted")
  expect_error(t$call(list()), class = "sas2r_tool_budget_error")
})

test_that("every registered tool exposes a closed argument schema", {
  specs <- load_agent_specs()
  ctx <- list(
    project = list(lineage = tibble::tibble()),
    schemas = list(),
    config = list(search_docs = list(enabled = FALSE)),
    macro_index = tibble::tibble()
  )
  tools <- build_tools(specs$translator, ctx)

  expect_true(length(tools) > 0L)
  for (tool in tools) {
    expect_identical(tool$schema$type, "object")
    expect_false(tool$schema$additionalProperties)
  }
})

test_that("tool wrappers reject arguments outside their closed schema", {
  tool <- make_tool(
    "closed", function(args) args, max_calls = 1L,
    schema = list(
      type = "object", properties = list(x = list(type = "string")),
      required = "x", additionalProperties = FALSE
    )
  )

  expect_error(
    tool$call(list(x = "ok", injected = TRUE)),
    class = "sas2r_tool_arguments_error"
  )
})

test_that("tool execution locally enforces schema types, items, and string bounds", {
  tool <- make_tool(
    "typed", function(args) args, max_calls = 10L,
    schema = list(
      type = "object",
      properties = list(
        count = list(type = "integer", minimum = 1L, maximum = 3L),
        label = list(type = "string", minLength = 2L, maxLength = 4L),
        tags = list(
          type = "array", minItems = 1L, maxItems = 2L,
          items = list(type = "string", minLength = 1L)
        )
      ),
      required = c("count", "label", "tags"),
      additionalProperties = FALSE
    )
  )

  expect_error(tool$call(list(count = 1.5, label = "ok", tags = list("x"))),
               class = "sas2r_tool_arguments_error")
  expect_error(tool$call(list(count = 4L, label = "ok", tags = list("x"))),
               class = "sas2r_tool_arguments_error")
  expect_error(tool$call(list(count = 1L, label = "x", tags = list("x"))),
               class = "sas2r_tool_arguments_error")
  expect_error(tool$call(list(count = 1L, label = "okay", tags = list(1))),
               class = "sas2r_tool_arguments_error")
  expect_error(tool$call(list(count = 1L, label = "okay", tags = list())),
               class = "sas2r_tool_arguments_error")
  expect_identical(
    tool$call(list(count = 2L, label = "okay", tags = list("x", "y")))$count,
    2L
  )
})

test_that("rulebook tool requires name while batch selectors remain optional", {
  tool <- make_tool(
    "lookup_rulebook", function(args) args, max_calls = 2L,
    schema = tool_argument_schema("lookup_rulebook")
  )

  expect_error(
    tool$call(list(functions = "round")),
    class = "sas2r_tool_arguments_error"
  )
  result <- tool$call(list(name = "round"))
  expect_identical(result$name, "round")
  expect_null(result$functions)
  expect_null(result$operators)
  expect_null(result$procs)
})

test_that("read_unit_context returns SAS text and schemas, nothing else", {
  dir <- withr::local_tempdir()
  writeLines("data w1; set w.root; where sex = 'M'; run;", file.path(dir, "a.sas"))
  p <- sas_project(dir)
  ctx <- list(project = p,
              unit_stmts = p$statements[p$statements$unit_type == "data_step", ],
              schemas = infer_schemas(p))
  spec <- list(tools = list(read_unit_context = list(max_calls = 2)))
  tools <- build_tools(spec, ctx)
  out <- tools$read_unit_context$call(list())
  expect_match(out$sas, "where sex = 'M'")
  expect_true("w.root" %in% names(out$input_schemas))
  expect_null(out$data)   # there is never data
})


test_that("multi-tool spec dispatches each tool independently with separate budgets", {
  spec <- list(tools = list(
    lookup_rulebook = list(max_calls = 1),
    query_project_graph = list(max_calls = 3)
  ))
  p <- list(lineage = tibble::tibble(dataset = "work.a", unit_id = 1L, role = "creates"))
  ctx <- list(project = p)
  tools <- build_tools(spec, ctx)
  rb_out <- tools$lookup_rulebook$call(list(name = "abs", functions = "abs"))
  expect_true("abs" %in% names(rb_out$functions))
  pg_out <- tools$query_project_graph$call(list(dataset = "work.a"))
  expect_identical(pg_out$dataset, "work.a")
  # check separate budget enforcement
  expect_identical(
    tools$lookup_rulebook$call(list(name = "abs"))$error,
    "budget_exhausted"
  )
  expect_false(identical(tools$query_project_graph$call(list(dataset = "work.a"))$error, "budget_exhausted"))
})

test_that("rulebook lookup returns dispatch targets with semantic evidence", {
  spec <- list(tools = list(lookup_rulebook = list(max_calls = 1)))
  tools <- build_tools(spec, list())
  out <- tools$lookup_rulebook$call(list(
    name = "round",
    functions = c("round", "abs"),
    operators = "=",
    procs = c("sql", "print")
  ))

  expect_identical(out$functions$round, "sas_round")
  expect_identical(out$functions$abs, "abs")
  expect_identical(out$operators[["="]], "==")
  expect_match(out$semantics$functions$round$semantic_delta$sas_default,
               "away from zero", ignore.case = TRUE)
  expect_match(out$semantics$functions$round$semantic_delta$r_default,
               "ties to even", ignore.case = TRUE)
  expect_identical(out$semantics$functions$round$strategy, "sas_round")
  expect_identical(
    out$semantics$functions$round$implementation_status,
    "implemented_unverified"
  )
  expect_true(any(grepl(
    "CAMIS", out$semantics$functions$round$source_urls
  )))
  expect_identical(out$procs$sql$target, "emit_proc_sql")
  expect_identical(out$procs$sql$status, "partial")
  expect_identical(
    out$procs$sql$semantic$implementation_status,
    "implemented_unverified"
  )
  expect_identical(out$procs$print$status, "skip_note")
  expect_null(out$procs$print$semantic)
})

test_that("search_docs is only instantiated when enabled in project config", {
  specs <- load_agent_specs()
  tr_spec <- specs$translator
  # disabled by default
  tools_def <- build_tools(tr_spec, list(config = list()))
  expect_null(tools_def$search_docs)

  # enabled via config
  mirror_dir <- withr::local_tempdir()
  writeLines("mutate creates new variables in R", file.path(mirror_dir, "dplyr.txt"))
  cfg <- list(search_docs = list(enabled = TRUE, backend = "mirror", mirror_dir = mirror_dir))
  tools_en <- build_tools(tr_spec, list(config = cfg))
  expect_false(is.null(tools_en$search_docs))
  res <- tools_en$search_docs$call(list(topic = "mutate"))
  expect_identical(length(res$snippets), 1L)

  # per-agent disable override disables the tool even when globally enabled
  tr_spec_dis <- tr_spec
  tr_spec_dis$tools$search_docs$enabled <- FALSE
  tools_dis <- build_tools(tr_spec_dis, list(config = cfg))
  expect_null(tools_dis$search_docs)
})

test_that("search_skills and read_skill tools enforce closed argument schemas", {
  spec <- list(tools = list(
    search_skills = list(max_calls = 2L),
    read_skill = list(max_calls = 2L)
  ))
  ctx <- list(skill_catalog = sas2r:::agent_skill_catalog())
  tools <- build_tools(spec, ctx)

  expect_true("search_skills" %in% names(tools))
  expect_true("read_skill" %in% names(tools))
  expect_false(tools$search_skills$schema$additionalProperties)
  expect_false(tools$read_skill$schema$additionalProperties)

  # Schema argument violations
  expect_error(tools$search_skills$call(list(extra = "foo")),
               class = "sas2r_tool_arguments_error")
  expect_error(tools$read_skill$call(list(extra = "foo")),
               class = "sas2r_tool_arguments_error")
})

test_that("search_skills and read_skill tools operate on bound context catalogue", {
  catalog <- sas2r:::agent_skill_catalog()
  spec <- list(tools = list(
    search_skills = list(max_calls = 2L),
    read_skill = list(max_calls = 2L)
  ))
  ctx <- list(skill_catalog = catalog)
  tools <- build_tools(spec, ctx)

  search_res <- tools$search_skills$call(list(query = "missing sort"))
  expect_true(length(search_res$skills) >= 1L)
  expect_identical(search_res$skills[[1]]$name, "sas-missing-sort-semantics")

  read_res <- tools$read_skill$call(list(name = "sas-missing-sort-semantics"))
  expect_identical(read_res$name, "sas-missing-sort-semantics")
  expect_match(read_res$body, "PROC SORT")

  # Unknown skill returns structured error
  read_unknown <- tools$read_skill$call(list(name = "unknown-skill"))
  expect_identical(read_unknown$error, "skill_not_registered")
})

test_that("read_comparison_report answers the model instead of aborting the run", {
  # A bounded tool must hand the model a typed record. Letting the condition
  # escape aborts the whole sas_refine_with_outputs() call and discards every
  # comparison report already computed, because a model named a bad report_id.
  registry <- new.env(parent = emptyenv())
  spec <- list(tools = list(read_comparison_report = list(max_calls = 2L)))
  tools <- build_tools(spec, list(report_registry = registry))

  unregistered <- tools$read_comparison_report$call(list(report_id = "no-such-report"))
  expect_identical(unregistered$error, "report_not_registered")
  expect_true(nzchar(unregistered$message))

  # The traversal class inherits from the not-registered class, so this also
  # pins the handler ordering.
  traversal <- tools$read_comparison_report$call(list(report_id = "../secret.json"))
  expect_identical(traversal$error, "report_path_rejected")
})
