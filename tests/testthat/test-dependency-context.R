test_that("all roles can retrieve a late dependency's complete source and selected R", {
  skip_on_cran() # Extended integration scenario; both installed-package CI jobs run it.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  names <- c(paste0("a", 1:4), "z_template")
  for (name in names) writeLines(c(paste0("%macro ", name, "();"),
    if (name == "z_template") paste0("%put ", seq_len(220), "_", strrep("x", 70), ";"),
    "%mend;"), file.path(root, "macros", paste0(name, ".sas")))
  writeLines(paste0("%", names, "();"), file.path(root, "main.sas"))
  project <- sas_project(file.path(root, "main.sas"), config = list(macro_search_path = file.path(root, "macros")))
  selected <- stats::setNames(lapply(names, function(name) list(revision_id = "selected-2",
    r_code = paste0(name, " <- function() {\n", paste(rep(paste0("# ", strrep("x", 70)), 220), collapse = "\n"), "\n 42\n}"))), paste0("macro__", names))
  guidance <- build_agent_guidance(project, "main", selected_revisions = selected)
  expect_lte(nchar(guidance$text), 24000L)
  tail <- strsplit(guidance$text, "dependency_body z_template", fixed = TRUE)[[1]][2]
  expect_match(tail, "sas truncated")
  expect_match(tail, "r truncated")
  expect_match(tail, "available characters:")
  expect_false(grepl("Additional downstream consumer IDs:", guidance$text, fixed = TRUE))
  ctx <- list(project = project, component_id = "main", selected_revisions = selected)
  source_reads <- character()
  read_source <- component_source_text
  testthat::local_mocked_bindings(component_source_text = function(graph, component_id) {
    source_reads <<- c(source_reads, component_id)
    read_source(graph, component_id)
  })
  for (role in c("translator", "reviewer", "fixer")) {
    tools <- build_tools(load_agent_specs()[[role]], ctx)
    expect_false(any(c("read_dataset_preview", "read_comparison_report") %in% names(tools)))
    for (language in c("sas", "r")) {
      offset <- 1L; code <- character(); pages <- 0L
      repeat {
        out <- tools$read_dependency_context$call(list(component_id = "macro__z_template", language = language, offset = offset))
        expect_identical(out$status, "available")
        expect_identical(out$revision_id, "selected-2")
        expect_lte(nchar(out$code), 12000L)
        code <- c(code, out$code); pages <- pages + 1L
        if (is.null(out$next_offset)) break
        offset <- out$next_offset
      }
      expect_gt(pages, 1L)
      expected <- if (language == "sas") component_source_text(project$graph, "macro__z_template") else selected$macro__z_template$r_code
      expect_identical(paste0(code, collapse = ""), expected)
    }
    expect_error(tools$read_dependency_context$call(list(component_id = "macro__z_template", language = "R")),
      class = "sas2r_tool_arguments_error")
  }
  expect_true(length(source_reads) > 1L)
  expect_identical(unique(source_reads), "macro__z_template")
  # Code not yet selected is unavailable, even though its SAS body is present.
  ctx$selected_revisions <- list()
  expect_identical(read_dependency_context(ctx, "macro__z_template", "r")$status, "unavailable")
  expect_identical(read_dependency_context(ctx, "macro__z_template", "sas")$status, "available")
  expect_identical(read_dependency_context(ctx, "unrelated", "r")$error, "not_a_direct_dependency_or_consumer")
})

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
    "Additional downstream consumer IDs: main", fixed = TRUE)
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
