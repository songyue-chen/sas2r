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
  expected_values <- c(direct = 1, conditional_fallback = 3)
  for (scenario in names(expected_values)) {
    root <- startup_library_fixture()
    config <- list()
    expected <- unname(expected_values[scenario])
    if (scenario == "conditional_fallback") {
      writeLines("%if &switch %then %do; libname input 'first'; %end;",
        file.path(root, "autoexec.sas"))
    }
    advisory <- scenario == "conditional_fallback"
    if (advisory)
      config$libraries <- list(input = file.path(root, "fallback"))
    input <- file.path(root, c("first", "second", "fallback")[expected], "source.sas7bdat")
    before <- cli::hash_file_sha256(input)
    main <- file.path(root, "main.sas")
    check <- sas_preflight(main, config = config, diagnose = "off")
    expect_identical(check$inputs$status[check$inputs$dataset == "input.source"], "available")
    expect_false(any(vapply(check$readiness$warnings, `[[`, logical(1), "blocks_execution")))
    result <- sas_translate(main, config = config, out_dir = withr::local_tempdir(),
      outputs = list(datasets = "work.result"),
      llm = startup_library_llm(list(main = "lib_write(lib_read('input', 'source'), 'work', 'result')")))
    expect_identical(any(check$findings$kind %in%
      c("autoexec_bindings_deferred", "autoexec_library_shadows_config")), advisory)
    expect_identical(result$status, if (advisory) "needs_review" else "migration_ready")
    if (advisory) expect_match(result$status_reason, "startup_libraries_require_review", fixed = TRUE)
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

test_that("startup control follows block scope, include occurrences and reassignment order", {
  root <- startup_library_fixture()
  main <- file.path(root, "main.sas")
  config <- list(libraries = list(input = file.path(root, "fallback")))
  writeLines("%include 'binding.inc';", file.path(root, "nested.inc"))
  writeLines("libname input 'second';", file.path(root, "binding.inc"))
  cases <- list(
    # A possible later assignment displaces certainty about an earlier one.
    later_conditional = c("libname input 'first';",
      "%if &switch %then %do; %include 'nested.inc'; %end;"),
    # A later unconditional assignment establishes the binding again.
    later_unconditional = c("%if &switch %then %do; libname input 'first'; %end;",
      "%include 'nested.inc';"),
    nested_control = c("%if &a %then %do; %if &b %then %do; options nonumber; %end; %end;",
      "libname input 'second';"),
    # The same include is conditional in one occurrence and unconditional later.
    repeated_include = c("%if &switch %then %do; %include 'nested.inc'; %end;",
      "%include 'nested.inc';"),
    macro_include = c("libname input 'first';",
      "%macro unused(); %include 'nested.inc'; %mend;"),
    conditional_clear = c("libname input 'first';",
      "%if &switch %then %do; libname input clear; %end;"),
    unrelated_library = c("%if &switch %then %do; libname other 'second'; %end;",
      "libname input 'first';"),
    # Cross-file blocks are deliberately deferred, not partially interpreted.
    unbalanced = c("%if &switch %then %do;", "libname input 'first';"),
    jump = c("%if &switch %then %goto done;", "libname input 'first';", "%done:;"),
    inline_assignment = c("libname input 'first';", "%if &switch %then libname input 'second';"),
    inline_include = c("libname input 'first';", "%if &switch %then %include 'nested.inc';"))
  expected <- c("fallback", "second", "second", "second", "first", "fallback", "first",
    "fallback", "fallback", "fallback", "fallback")
  for (i in seq_along(cases)) {
    writeLines(cases[[i]], file.path(root, "autoexec.sas"))
    project <- sas_project(main, config = config)
    selected <- file.path(root, expected[i])
    expect_equal(startup_libref_map(project)$input$path, selected, info = names(cases)[i])
    expect_equal(resolve_libref_at(project$libref_registry, "input", main, 1L)$selected_path,
      selected, info = names(cases)[i])
  }
})

test_that("preflight makes a shadowed configured library visible without blocking execution", {
  root <- startup_library_fixture()
  main <- file.path(root, "main.sas")
  config <- list(libraries = list(input = file.path(root, "fallback")))
  check <- sas_preflight(main, config = config, diagnose = "off")
  warnings <- Filter(function(x) x$kind == "autoexec_library_shadows_config", check$readiness$warnings)
  expect_length(warnings, 1L)
  expect_false(warnings[[1L]]$blocks_execution)
  expect_match(warnings[[1L]]$detail, file.path(root, "first"), fixed = TRUE)
  expect_match(warnings[[1L]]$detail, file.path(root, "fallback"), fixed = TRUE)
  expect_match(warnings[[1L]]$detail, "autoexec.sas", fixed = TRUE)
  # Console wrapping depends on width and platform-specific temporary paths.
  for (width in c(32L, 80L, 160L)) {
    printed <- withr::with_options(list(cli.width = width),
      paste(capture.output(print(check), type = "message"), collapse = "\n"))
    printed <- gsub("[[:space:]]+", " ", printed)
    expect_match(printed, "takes precedence over configured fallback", fixed = TRUE)
  }
  for (source in c("libname input 'fallback';", "libname input clear;",
    "libname input '/unavailable-source-folder';",
    "%if &switch %then %do; libname input 'first'; %end;")) {
    writeLines(source, file.path(root, "autoexec.sas"))
    check <- sas_preflight(main, config = config, diagnose = "off")
    expect_false("autoexec_library_shadows_config" %in% check$findings$kind)
  }
})

test_that("an include in an unrelated startup macro does not defer other bindings", {
  root <- startup_library_fixture()
  writeLines("options nonumber;", file.path(root, "options.inc"))
  writeLines(c("%macro setup_options(); %include 'options.inc'; %mend;",
    "libname input 'first';"), file.path(root, "autoexec.sas"))
  check <- sas_preflight(file.path(root, "main.sas"), diagnose = "off")
  expect_false("autoexec_bindings_deferred" %in% check$findings$kind)
  expect_false(any(vapply(check$readiness$warnings, `[[`, logical(1), "blocks_execution")))
  expect_equal(check$inputs$library_path, file.path(root, "first"))
  expect_identical(check$inputs$status, "available")
})

test_that("startup shadow warnings follow reads and writes through includes and program changes", {
  root <- startup_library_fixture()
  writeLines("libname input 'first';", file.path(root, "binding.inc"))
  writeLines("%include 'binding.inc';", file.path(root, "autoexec.sas"))
  config <- list(libraries = list(input = file.path(root, "fallback")))
  cases <- list(
    unused = "data work.result; value = 1; run;",
    replaced = c("libname input 'second';", "data work.result; set input.source; run;"),
    cleared = c("libname input clear;", "data work.result; set input.source; run;"),
    read_before_clear = c("data work.result; set input.source; run;", "libname input clear;"),
    write = "data input.result; value = 1; run;")
  for (name in names(cases)) {
    writeLines(cases[[name]], file.path(root, "main.sas"))
    project <- sas_project(file.path(root, "main.sas"), config = config)
    expect_identical("autoexec_library_shadows_config" %in% project$flags$kind,
      name %in% c("read_before_clear", "write"), info = name)
  }
})

test_that("possible startup uses distinguish literal library prefixes from dynamic names", {
  root <- startup_library_fixture()
  config <- list(libraries = list(input = file.path(root, "fallback")))
  # Every case checks the public preflight result. None adds static input
  # lineage; these uses must retain the appropriate startup advisory anyway.
  cases <- list(
    macro_read = "%macro readit; data work.result; set input.source; run; %mend;",
    macro_write = "%macro writeit; data input.result; value = 1; run; %mend;",
    macro_sql = "%macro readit; proc sql; create table work.result as select * from input.source; quit; %mend;",
    dynamic_member = "data work.result; set input.&member; run;",
    dynamic_name = "data work.result; set &dataset; run;",
    dynamic_write = "data &outlib..result; value = 1; run;",
    copy_in = "proc copy in=input out=work; run;",
    copy_out = "proc copy in=work out=input; run;",
    copy_dynamic = "proc copy in=&inlib out=work; run;",
    datasets_library = "proc datasets library=input; contents data=_all_; quit;",
    datasets_lib = "proc datasets lib=input; contents data=_all_; quit;",
    work_member = "data work.result; set work.&member; run;",
    other_member = "data work.result; set other.&member; run;",
    dataset_option = "data work.result; set work.source(where=(value=&limit)); run;",
    source_string = "data work.result; text='input.source &inlib'; run;")
  for (conditional in c(FALSE, TRUE)) {
    writeLines(if (conditional)
      "%if &switch %then %do; libname input 'first'; %end;" else "libname input 'first';",
      file.path(root, "autoexec.sas"))
    kind <- if (conditional) "autoexec_bindings_deferred" else "autoexec_library_shadows_config"
    for (name in names(cases)) {
      writeLines(cases[[name]], file.path(root, "main.sas"))
      check <- sas_preflight(file.path(root, "main.sas"), config = config, diagnose = "off")
      expect_identical(kind %in% check$findings$kind,
        !name %in% c("work_member", "other_member", "dataset_option", "source_string"), info = name)
    }
  }
})
