test_that("translation and macro indexing leave source projects unchanged", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  writeLines("%macro helper(); %mend;", file.path(root, "macros", "helper.sas"))
  writeLines("data work.out; x = 1; run;", file.path(root, "p.sas"))
  config <- list(macro_search_path = "macros")
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  withr::local_options(sas2r.progress = FALSE)
  result <- suppressMessages(sas_translate(root, config = config, execute = FALSE,
    usage_limits = list(max_calls = 0)))
  on.exit(unlink(result$out_dir, recursive = TRUE), add = TRUE)
  project <- result$project
  first <- project_macro_index(project, project$config)
  second <- project_macro_index(project, project$config)
  expect_identical(first, second)
  expect_true("helper" %in% first$name)
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE), before)
  expect_false(dir.exists(file.path(root, ".sas2r")))
  expect_true(startsWith(normalizePath(result$out_dir), normalizePath(tempdir())))
  expect_identical(result$usage$request_count, 0L)
})

test_that("quiet translation keeps status and reports without console output", {
  root <- withr::local_tempdir()
  writeLines("data work.out; x = 1; run;", file.path(root, "p.sas"))
  withr::local_options(sas2r.progress = FALSE)
  messages <- capture.output(
    output <- capture.output(result <- suppressMessages(sas_translate(root,
      out_dir = withr::local_tempdir(), execute = FALSE))), type = "message")
  expect_length(output, 0L)
  expect_length(messages, 0L)
  expect_identical(result$status, "blocked")
  expect_true(file.exists(result$report_json_path))
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages[["Bundle execution"]], "NOT RUN", fixed = TRUE)
})
