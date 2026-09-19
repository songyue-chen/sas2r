test_that("parallel translation shares relative output paths across different working directories", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "programs"))
  for (i in 1:2) writeLines(sprintf("data work.out%d; retain x %d; run;", i, i),
    file.path(root, "programs", paste0("p", i, ".sas")))
  cat("\n%let label=%qtrim(%qleft(example));\n", file = file.path(root, "programs", "p1.sas"), append = TRUE)
  helpers <- normalizePath(test_path(c("helper-agents.R", "helper-parallel.R")))
  # Bound the whole run: the regression otherwise waits forever for a reply in
  # the wrong directory. Real child workers and mock models exercise the public API.
  observed <- callr::r(function(package_path, libpath, root, helpers) {
    .libPaths(libpath)
    if (file.exists(file.path(package_path, "Meta", "package.rds"))) {
      loadNamespace("sas2r", lib.loc = dirname(package_path))
    } else pkgload::load_all(package_path, quiet = TRUE)
    fixtures <- new.env(parent = asNamespace("sas2r"))
    for (helper in helpers) sys.source(helper, envir = fixtures)
    setwd(root)
    events <- list()
    responses <- list(reviewer = fixtures$valid_program_review_response())
    for (i in 1:2) responses[[paste0("translator:p", i)]] <- fixtures$valid_program_translation_response(
      code = sprintf("lib_write(data.frame(x = %d), 'work', 'out%d')", i, i))
    responses[["translator:p1"]]$data$suspected_dependencies <- list("qtrim", "%QLEFT")
    result <- withCallingHandlers(sas2r::sas_translate("programs", out_dir = "migration_output",
      config = list(), llm = fixtures$parallel_test_llm(responses, delay = 0.2),
      outputs = c("work.out1", "work.out2"),
      max_parallel_translations = 4L, max_program_repair_rounds = 0L,
      max_bundle_repair_rounds = 0L, execute = TRUE),
      sas2r_progress = function(event) {
        events[[length(events) + 1L]] <<- list(event = event$event, agent = event$agent,
          component_id = event$component_id)
      })
    list(out_dir = result$out_dir, bundle = result$bundle_dir, outputs = result$outputs_dir,
      report = result$report_json_path, parallel = result$diagnostics$parallel,
      events = events, requests = result$usage$request_count,
      values = lapply(list.files(result$outputs_dir, pattern = "[.]rds$", recursive = TRUE,
        full.names = TRUE), readRDS))
  }, args = list(find.package("sas2r"), .libPaths(), root, helpers), timeout = 90)

  expect_identical(observed$out_dir, normalizePath(file.path(root, "migration_output")))
  expect_true(dir.exists(observed$bundle))
  expect_true(file.exists(observed$report))
  report <- read_json_record(observed$report)
  expect_match(report$outcome$stages[["Bundle execution"]], "EXECUTED", fixed = TRUE)
  expect_length(report$diagnostics$parallel_deferred, 0L)
  expect_identical(observed$parallel$effective, 4L)
  expect_gte(observed$parallel$observed$peak_workers, 2L)
  started <- Filter(function(event) identical(event$event, "agent_started") &&
    identical(event$agent, "translator"), observed$events)
  expect_setequal(vapply(started, `[[`, "", "component_id"), c("p1", "p2"))
  expect_gte(observed$requests, 4L)
  expect_length(observed$values, 2L)
  expect_equal(sort(vapply(observed$values, function(x) x$x, numeric(1))), c(1, 2))
  expect_false(dir.exists(file.path(root, "programs", "migration_output")))
  jobs <- list.files(file.path(root, "migration_output"), pattern = "job.json$",
                     recursive = TRUE, full.names = TRUE)
  expect_gte(length(jobs), 4L)
  expect_true(all(vapply(jobs, function(path)
    identical(read_json_record(path)$status, "accepted"), logical(1))))
  expect_length(list.files(file.path(root, "migration_output"), pattern = "[.](request|reply)$",
    recursive = TRUE, all.files = TRUE), 0L)
})
