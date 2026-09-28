
test_that("consumer context is available without changing the macro's dependencies", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  writeLines("%macro check(); %put 1; %mend;", file.path(root, "macros", "check.sas"))
  writeLines("%check();", file.path(root, "main.sas"))
  project <- sas_project(file.path(root, "main.sas"), config = list(macro_search_path = file.path(root, "macros")))
  fx <- list(project = project, revisions = list(macro__check = list(revision_id = "r2", r_code = "check <- function() 1")))
  selected <- c(fx$revisions, list(main = list(revision_id = "r3", r_code = "answer <- check()")))
  ctx <- list(project = fx$project, component_id = "macro__check", selected_revisions = selected)
  expect_match(build_agent_guidance(fx$project, "macro__check", selected_revisions = selected)$text,
    "Additional related code IDs: main", fixed = TRUE)
  expect_identical(read_dependency_context(ctx, "main", "r")$code, "answer <- check()")
  old <- build_agent_guidance(fx$project, "macro__check", selected_revisions = selected)$identity
  selected$main$r_code <- "answer <- check() + 1"
  expect_false(identical(old, build_agent_guidance(fx$project, "macro__check", selected_revisions = selected)$identity))
  expect_length(direct_component_dependencies(fx$project$graph, "macro__check"), 0)
})

test_that("dependency retrieval records the selected component and page without code", {
  digest <- usage_tool_argument_digest(list(component_id = "macro__plot", language = "r", offset = 12001L), "read_dependency_context")
  out <- jsonlite::fromJSON(digest)
  expect_identical(out$component_id, "macro__plot")
  expect_identical(out$language, "r")
  expect_equal(out$offset, 12001)
})
