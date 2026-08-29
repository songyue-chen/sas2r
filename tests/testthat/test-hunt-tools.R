hunt_ctx <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(c("%macro one; data a; run; %mend;", "%macro two; %mend;"),
             file.path(dir, "utilities.sas"))
  list(macro_index = build_macro_index(dir))
}

test_that("find_macro returns verified hits with spans", {
  tools <- build_tools(list(tools = list(find_macro = list(max_calls = 2))), hunt_ctx())
  out <- tools$find_macro$call(list(name = "one"))
  expect_identical(out$hits[[1]]$verified, TRUE)
  expect_match(out$hits[[1]]$file, "utilities\\.sas")
})

test_that("get_macro_source returns the definition block only", {
  tools <- build_tools(list(tools = list(get_macro_source = list(max_calls = 1))), hunt_ctx())
  out <- tools$get_macro_source$call(list(name = "one"))
  expect_match(out$source, "%macro one")
  expect_match(out$source, "%mend")
  expect_false(grepl("%macro two", out$source, fixed = TRUE))
})

test_that("list_macro_files returns names only and paths cannot be injected", {
  tools <- build_tools(list(tools = list(list_macro_files = list(max_calls = 1),
                                         get_macro_source = list(max_calls = 1))),
                       hunt_ctx())
  expect_error(
    tools$list_macro_files$call(list(path = "/etc")),
    class = "sas2r_tool_arguments_error"
  )
  out <- tools$list_macro_files$call(list())
  expect_identical(out$files, "utilities.sas")
  miss <- tools$get_macro_source$call(list(name = "../../etc/passwd"))
  expect_identical(miss$error, "not_in_index")
})

test_that("orchestrator translates macro stubs before caller stubs", {
  ids <- order_stubs_for_agents(tibble::tibble(
    unit_id = c(10L, 4L), unit_type = c("data_step", "macro_def")))
  expect_identical(ids, c(4L, 10L))
})

test_that("find_macro handles missing macros and multiple shadowed hits", {
  dir <- withr::local_tempdir()
  d1 <- file.path(dir, "root1"); dir.create(d1)
  d2 <- file.path(dir, "root2"); dir.create(d2)
  writeLines("%macro dup; %mend;", file.path(d1, "m1.sas"))
  writeLines("%macro dup; %mend;", file.path(d2, "m2.sas"))
  idx <- build_macro_index(c(d1, d2))
  tools <- build_tools(list(tools = list(find_macro = list(max_calls = 3))), list(macro_index = idx))

  # Case insensitivity & hit count
  out <- tools$find_macro$call(list(name = "DUP"))
  expect_identical(length(out$hits), 2L)
  expect_null(out$hits[[1]]$shadowed_by)
  expect_identical(out$hits[[2]]$shadowed_by, out$hits[[1]]$file)

  # Missing macro
  miss <- tools$find_macro$call(list(name = "nonexistent"))
  expect_identical(miss$error, "not_in_index")
})

test_that("get_macro_source caps at 400 lines and sets truncated flag", {
  dir <- withr::local_tempdir()
  # Create a macro definition spanning 450 lines
  long_lines <- c("%macro long_macro;", rep("x = 1;", 448), "%mend;")
  writeLines(long_lines, file.path(dir, "long.sas"))
  idx <- build_macro_index(dir)
  tools <- build_tools(list(tools = list(get_macro_source = list(max_calls = 1))), list(macro_index = idx))

  out <- tools$get_macro_source$call(list(name = "long_macro"))
  expect_true(out$truncated)
  ret_lines <- strsplit(out$source, "\n")[[1]]
  expect_identical(length(ret_lines), 400L)
})
