# Every dataset in these tests is created here from synthetic constants.
startup_library_fixture <- function(envir = parent.frame()) {
  root <- normalizePath(withr::local_tempdir(.local_envir = envir), winslash = "/")
  # This fixture needs the native format selected by a plain SAS LIBNAME.
  withr::local_options(lifecycle_verbosity = "quiet")
  for (name in c("first", "second", "fallback")) {
    dir.create(file.path(root, name))
    haven::write_sas(data.frame(value = match(name, c("first", "second", "fallback"))),
      file.path(root, name, "source.sas7bdat"))
  }
  writeLines("libname input 'first';", file.path(root, "autoexec.sas"))
  writeLines("data work.result; set input.source; run;", file.path(root, "main.sas"))
  root
}

startup_library_llm <- function(code) recording_reviewer(function(req) {
  if (identical(req$role, "translator")) return(good_translation(code[[req$component_id]]))
  if (identical(req$role, "reviewer")) return(good_review())
  stop("The faithful startup fixture must not need a fixer")
})

test_that("autoexec-only inputs work in preflight, smoke, bundle and exported execution", {
  skip_if_not_installed("dplyr")
  for (scenario in c("direct", "include", "clear")) {
    root <- startup_library_fixture()
    config <- list()
    expected <- match(scenario, c("direct", "include", "clear"))
    if (scenario != "direct") {
      writeLines("libname input 'second';", file.path(root, "binding.inc"))
      writeLines(c("libname input 'first';", "%include 'binding.inc';"),
        file.path(root, "autoexec.sas"))
    }
    if (scenario == "clear") {
      writeLines("libname input clear;", file.path(root, "clear.sas"))
      config <- list(autoexec = file.path(root, c("autoexec.sas", "clear.sas")),
        libraries = list(input = file.path(root, "fallback")))
    }
    input <- file.path(root, c("first", "second", "fallback")[expected], "source.sas7bdat")
    before <- cli::hash_file_sha256(input)
    main <- file.path(root, "main.sas")
    check <- sas_preflight(main, config = config, diagnose = "off")
    expect_identical(check$inputs$status[check$inputs$dataset == "input.source"], "available")
    result <- sas_translate(main, config = config, out_dir = withr::local_tempdir(),
      outputs = list(datasets = "work.result"),
      llm = startup_library_llm(list(main = "lib_write(lib_read('input', 'source'), 'work', 'result')")))
    expect_identical(result$status, "migration_ready")
    expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "result.rds"))$value, expected)
    events <- current_component_evidence(result$component_evidence$main)$events
    expect_true(any(vapply(events, function(event)
      identical(event$type, "program_smoke") && identical(event$status, "passed"), logical(1))))
    # The independently emitted, movable bundle must use the same startup map.
    moved <- file.path(withr::local_tempdir(), "moved")
    materialize_user_bundle(result$bundle_dir, moved, result$project)
    value <- callr::r(function(dir) {
      setwd(dir)
      source("run.R")
      readRDS(file.path("output", "work", "result.rds"))$value
    }, args = list(dir = moved), user_profile = FALSE, system_profile = FALSE)
    expect_equal(value, expected)
    expect_identical(cli::hash_file_sha256(input), before)
  }
})

test_that("startup projection respects ordered includes, CLEAR and configured fallback", {
  root <- startup_library_fixture()
  writeLines("libname input 'second';", file.path(root, "binding.inc"))
  writeLines(c("libname input 'first';", "%include 'binding.inc';"),
    file.path(root, "autoexec.sas"))
  writeLines("libname input clear;", file.path(root, "clear.sas"))
  main <- file.path(root, "main.sas")
  project <- sas_project(main)
  effective <- effective_librefs(project)
  expect_equal(effective$startup$input$path, file.path(root, "second"))
  expect_null(effective$seed$input)
  # Resolving the first actual read agrees with the startup projection.
  expect_equal(resolve_libref_at(project$libref_registry, "input", main, 1L)$selected_path,
    effective$startup$input$path)
  config <- list(autoexec = file.path(root, c("autoexec.sas", "clear.sas")))
  expect_null(effective_librefs(sas_project(main, config = config))$startup$input)
  config$libraries <- list(input = file.path(root, "fallback"))
  effective <- effective_librefs(sas_project(main, config = config))
  expect_equal(effective$startup$input$path, file.path(root, "fallback"))
  expect_equal(effective$seed$input$path, file.path(root, "fallback"))
  # CLEAR ALL restores only configured fallbacks, including autoexec-only names.
  writeLines("libname _all_ clear;", file.path(root, "clear.sas"))
  expect_equal(effective_librefs(sas_project(main, config = config))$startup$input$path,
    file.path(root, "fallback"))
})

test_that("program assignments occur at their source position and do not leak across roots", {
  skip_if_not_installed("dplyr")
  root <- startup_library_fixture()
  unlink(file.path(root, "main.sas"))
  writeLines(c("data work.before; set input.source; run;", "libname input 'second';",
    "data work.after; set input.source; run;"), file.path(root, "a.sas"))
  writeLines("data work.next_root; set input.source; run;", file.path(root, "b.sas"))
  code_a <- paste(
    "lib_write(lib_read('input', 'source'), 'work', 'before')",
    sprintf("sas2r_libname_assign('input', %s, engine = 'sas7bdat')", deparse(file.path(root, "second"))),
    "lib_write(lib_read('input', 'source'), 'work', 'after')", sep = "\n")
  code_b <- "lib_write(lib_read('input', 'source'), 'work', 'next_root')"
  result <- sas_translate(root, out_dir = withr::local_tempdir(),
    outputs = list(datasets = c("work.before", "work.after", "work.next_root")),
    llm = startup_library_llm(list(a = code_a, b = code_b)))
  expect_identical(result$status, "migration_ready")
  values <- function(dir) vapply(c("before", "after", "next_root"), function(name)
    readRDS(file.path(dir, "work", paste0(name, ".rds")))$value, numeric(1))
  expect_equal(unname(values(file.path(result$outputs_dir, "datasets"))), c(1, 2, 1))
  manual <- callr::r(function(dir) {
    setwd(dir)
    source("run.R")
    vapply(c("before", "after", "next_root"), function(name)
      readRDS(file.path("output", "work", paste0(name, ".rds")))$value, numeric(1))
  }, args = list(dir = result$bundle_dir), user_profile = FALSE, system_profile = FALSE)
  expect_equal(unname(manual), c(1, 2, 1))
  # Exercise the flat snapshot's entrypoint too, with fresh output directories.
  report <- read_json_record(result$report_json_path)
  snapshot <- file.path(dirname(dirname(result$report_json_path)), "diagnostics",
    "bundle_attempts", report$selected_attempt_id, "bundle")
  flat <- withr::local_tempdir()
  file.copy(list.files(snapshot, full.names = TRUE), flat, recursive = TRUE)
  write_autoexec(result$project, flat,
    library_map = build_attempt_library_map(result$project, flat))
  flat_values <- callr::r(function(dir) {
    setwd(dir)
    source("run.R")
    vapply(c("before", "after", "next_root"), function(name)
      readRDS(file.path("work", paste0(name, ".rds")))$value, numeric(1))
  }, args = list(dir = flat), user_profile = FALSE, system_profile = FALSE)
  expect_equal(unname(flat_values), c(1, 2, 1))
})

test_that("unresolved and conditional startup assignments remain deferred", {
  root <- startup_library_fixture()
  main <- file.path(root, "main.sas")
  for (text in c("libname input '&missing';",
    "%macro setup(); libname input 'first'; %mend;",
    "%if &switch %then %do; libname input 'first'; %end;")) {
    writeLines(text, file.path(root, "autoexec.sas"))
    expect_null(effective_librefs(sas_project(main))$startup$input)
    project <- sas_project(main, config = list(libraries = list(input = file.path(root, "fallback"))))
    expect_equal(effective_librefs(project)$startup$input$path, file.path(root, "fallback"))
  }
  writeLines("libname input 'first';", file.path(root, "binding.inc"))
  writeLines("%if &switch %then %do; %include 'binding.inc'; %end;", file.path(root, "autoexec.sas"))
  project <- sas_project(main)
  expect_null(effective_librefs(project)$startup$input)
  expect_true("autoexec_bindings_deferred" %in% project$flags$kind)
  check <- sas_preflight(main, diagnose = "off")
  expect_false(any(check$inputs$status == "available"))
})
