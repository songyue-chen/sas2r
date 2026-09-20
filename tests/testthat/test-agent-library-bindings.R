test_that("generated LIBNAMEs retain configured fallbacks and genuine source rebindings", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "programs"))
  configured <- file.path(root, "configured"); dir.create(configured)
  other <- file.path(root, "other"); dir.create(other)
  source <- file.path(root, "programs", "plot.sas")
  writeLines('libname analysis "data/analysis"; data work.out; set analysis.derived; run;', source)
  config <- list(libraries = list(analysis = list(path = configured, engine = "rds")))
  p <- sas_project(source, config = config)
  bad <- 'sas2r_libname_assign("analysis", "data/analysis")'
  good <- sprintf('sas2r_libname_assign("analysis", %s)', deparse(configured))
  expect_match(check_component_library_assignments(bad, p, "plot"), "preflight selected", fixed = TRUE)
  expect_match(check_component_library_assignments(paste0("sas2r::", bad), p, "plot"), "preflight selected", fixed = TRUE)
  expect_length(check_component_library_assignments(good, p, "plot"), 0)
  expect_match(render_component_libraries(p, "plot"), sprintf('sas2r_libname_assign("analysis", %s,',
    deparse(normalizePath(configured, winslash = "/"))), fixed = TRUE)
  # Named arguments and harmless path syntax still use physical resolution.
  equivalent <- sprintf('sas2r_libname_assign(read_path = %s, libref = "analysis")', deparse(file.path(root, "programs", "..", "configured")))
  expect_length(check_component_library_assignments(equivalent, p, "plot"), 0)
  expect_length(check_component_library_assignments('sas2r_libname_assign(lib, path)', p, "plot"), 0)
  code_file <- withr::local_tempfile(fileext = ".R"); writeLines(bad, code_file)
  checks <- check_program_revision(code_file, contract = list(component_id = "plot"), project = p)
  expect_false(checks$pass)
  expect_match(checks$errors, "library_binding", fixed = TRUE)

  writeLines(c(sprintf('libname analysis %s;', deparse(configured)),
    'data work.first; set analysis.derived; run;',
    sprintf('libname analysis %s;', deparse(other)),
    'data work.second; set analysis.derived; run;'), source)
  p <- sas_project(source, config = config)
  both <- paste(good, sprintf('sas2r_libname_assign("analysis", %s)', deparse(other)), sep = "\n")
  expect_length(check_component_library_assignments(both, p, "plot"), 0)
})

test_that("a contradictory path is corrected before the generated consumer executes", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "programs"))
  input <- file.path(root, "input"); dir.create(input)
  output <- file.path(root, "analysis"); dir.create(output)
  saveRDS(data.frame(value = 1:3), file.path(input, "data.rds"))
  writeLines('libname analysis "&ANALYSIS_PATH"; data analysis.derived; set raw.data; run;', file.path(root, "programs", "derive.sas"))
  writeLines('libname analysis "data/analysis"; data work.result; set analysis.derived; retain marker 1; run;', file.path(root, "programs", "plot.sas"))
  config <- list(libraries = list(raw = list(path = input, engine = "rds"), analysis = list(path = output, engine = "rds")))
  n <- 0L
  llm <- recording_reviewer(function(req) {
    if (req$agent == "reviewer") return(valid_program_review_response())
    n <<- n + 1L
    path <- if (n == 1L) "data/analysis" else output
    valid_program_translation_response(paste0('sas2r_libname_assign("analysis", ', deparse(path), ')\n',
      'x <- lib_read("analysis", "derived"); x$marker <- rep(1, nrow(x)); lib_write(x, "work", "result")'))
  })
  result <- sas_translate(file.path(root, "programs"), file.path(root, "run"), config = config,
    llm = llm, outputs = "work.result", max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  expect_identical(n, 2L)
  expect_identical(result$status, "migration_ready")
  report <- read_json_record(result$report_json_path)
  expect_match(report$component_evidence$plot$mechanical_retry$errors, "library_binding")
  messages <- vapply(llm$requests(), function(request)
    paste(vapply(request$messages, `[[`, "", "content"), collapse = "\n"), "")
  expect_true(any(grepl("preflight selected", messages, fixed = TRUE)))
})
