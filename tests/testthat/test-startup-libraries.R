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
  expected_values <- c(direct = 1, include = 2, clear = 3, unrelated_control = 1,
    conditional_fallback = 3, shadow_config = 1, unused_conditional = 1,
    superseded_conditional = 1, program_clear = 3, conditional_clear = 3,
    conditional_unavailable = 3)
  for (scenario in names(expected_values)) {
    root <- startup_library_fixture()
    config <- list()
    expected <- unname(expected_values[scenario])
    if (scenario %in% c("include", "clear")) {
      writeLines("libname input 'second';", file.path(root, "binding.inc"))
      writeLines(c("libname input 'first';", "%include 'binding.inc';"),
        file.path(root, "autoexec.sas"))
    }
    if (scenario == "clear") {
      writeLines("libname input clear;", file.path(root, "clear.sas"))
      config <- list(autoexec = file.path(root, c("autoexec.sas", "clear.sas")),
        libraries = list(input = file.path(root, "fallback")))
    }
    if (scenario == "unrelated_control") {
      writeLines(c("%if &sysscp = WIN %then %do; options nonumber; %end;",
        "libname input 'first';"), file.path(root, "autoexec.sas"))
    }
    if (scenario == "conditional_fallback") {
      writeLines("%if &switch %then %do; libname input 'first'; %end;",
        file.path(root, "autoexec.sas"))
    }
    if (scenario == "unused_conditional") {
      writeLines(c("%if &switch %then %do; libname scratch 'second'; %end;",
        "libname input 'first';"), file.path(root, "autoexec.sas"))
    }
    if (scenario == "superseded_conditional") {
      writeLines(c("%if &switch %then %do; libname input 'second'; %end;",
        "libname input 'first';"), file.path(root, "autoexec.sas"))
    }
    if (scenario == "program_clear") {
      writeLines(c("libname input clear;", "data work.result; set input.source; run;"),
        file.path(root, "main.sas"))
    }
    if (scenario == "conditional_clear") {
      writeLines(c("libname input 'first';",
        "%if &switch %then %do; libname input clear; %end;"), file.path(root, "autoexec.sas"))
    }
    if (scenario == "conditional_unavailable") {
      writeLines("%if &switch %then %do; libname input 'unavailable'; %end;",
        file.path(root, "autoexec.sas"))
    }
    if (scenario %in% c("unused_conditional", "superseded_conditional"))
      config$libraries <- list(input = file.path(root, "first"))
    advisory <- scenario %in% c("conditional_fallback", "shadow_config", "conditional_clear",
      "conditional_unavailable")
    if (advisory || scenario == "program_clear")
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
  expect_true(any(vapply(check$readiness$warnings, function(x)
    x$kind == "input_unresolved" && x$blocks_execution, logical(1))))
  expect_false(any(vapply(check$readiness$warnings, function(x)
    x$kind == "autoexec_bindings_deferred" && x$blocks_execution, logical(1))))
  result <- sas_translate(main, out_dir = withr::local_tempdir(),
    outputs = list(datasets = "work.result"),
    llm = startup_library_llm(list(main = "lib_write(lib_read('input', 'source'), 'work', 'result')")))
  report <- read_json_record(result$report_json_path)
  expect_identical(result$status, "needs_review")
  expect_identical(report$outcome$stages$`Bundle execution`, "NOT RUN (0 attempts)")
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

test_that("startup macro library review is limited to used libraries", {
  skip_if_not_installed("dplyr")
  for (used in c(FALSE, TRUE)) {
    root <- startup_library_fixture()
    libref <- if (used) "input" else "scratch"
    path <- if (used) "first" else "second"
    writeLines(c("%macro tmp;", sprintf("libname %s '%s';", libref, path),
      "%mend;", "libname input 'first';"), file.path(root, "autoexec.sas"))
    code <- sprintf("tmp <- function() sas2r_libname_assign('%s', %s, engine = 'sas7bdat')",
      libref, deparse(file.path(root, path)))
    # This startup macro uses the same translation/repair path whether its
    # library is used or unused; the advisory itself does not invoke repair.
    llm <- recording_reviewer(function(req) {
      if (identical(req$role, "reviewer")) return(good_review())
      if (identical(req$role, "fixer")) return(valid_program_fix_response(code = code))
      good_translation(if (identical(req$component_id, "main"))
        "lib_write(lib_read('input', 'source'), 'work', 'result')" else code)
    })
    result <- sas_translate(file.path(root, "main.sas"), out_dir = withr::local_tempdir(),
      config = list(libraries = list(input = file.path(root, "first"))),
      outputs = list(datasets = "work.result"), llm = llm)
    expect_identical(result$status, if (used) "needs_review" else "migration_ready")
    expect_identical("autoexec_bindings_deferred" %in% result$project$flags$kind, used)
    expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "result.rds"))$value, 1)
    if (used) {
      expect_match(result$status_reason, "startup_libraries_require_review", fixed = TRUE)
      # An open-code call must retain the warning too: the static projection
      # does not evaluate a macro that can reassign this used library.
      writeLines(c("%macro tmp; libname input 'second'; %mend;",
        "libname input 'first';", "%tmp;"), file.path(root, "autoexec.sas"))
      project <- sas_project(file.path(root, "main.sas"))
      expect_true("autoexec_bindings_deferred" %in% project$flags$kind)
    }
  }
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

test_that("startup advisories survive macro, dynamic-name and library-level execution", {
  skip_if_not_installed("dplyr")
  for (conditional in c(FALSE, TRUE)) {
    for (use in c("macro", "autocall", "dynamic", "copy")) {
      root <- startup_library_fixture()
      main <- file.path(root, "main.sas")
      if (conditional) writeLines(
        "%if &switch %then %do; libname input 'first'; %end;", file.path(root, "autoexec.sas"))
      config <- list(libraries = list(input = file.path(root, "fallback")))
      macro <- c("%macro readit;", "data work.result; set input.source; run;", "%mend;")
      macro_r <- "readit <- function() lib_write(lib_read('input', 'source'), 'work', 'result')"
      if (use == "macro") {
        writeLines(c(macro, "%readit;"), main)
        main_r <- paste(macro_r, "readit()", sep = "\n")
      } else if (use == "autocall") {
        dir.create(file.path(root, "macros"))
        writeLines(macro, file.path(root, "macros", "readit.sas"))
        config$macro_search_path <- file.path(root, "macros")
        writeLines("%readit;", main)
        main_r <- "readit()"
      } else if (use == "dynamic") {
        writeLines(c("%let inlib = input;", "data work.result; set &inlib..source; run;"), main)
        main_r <- "lib_write(lib_read('input', 'source'), 'work', 'result')"
      } else {
        writeLines(c("proc copy in=input out=work; run;",
          "data work.result; set work.source; run;"), main)
        main_r <- paste("for (m in lib_members('input')) lib_write(lib_read('input', m), 'work', m)",
          "lib_write(lib_read('work', 'source'), 'work', 'result')", sep = "\n")
      }
      kind <- if (conditional) "autoexec_bindings_deferred" else "autoexec_library_shadows_config"
      check <- sas_preflight(main, config = config, diagnose = "off")
      expect_true(kind %in% check$findings$kind, info = use)
      code_for <- function(id) if (identical(id, "main")) main_r else macro_r
      llm <- recording_reviewer(function(req) {
        if (identical(req$role, "reviewer")) return(good_review())
        if (identical(req$role, "fixer")) return(valid_program_fix_response(code = code_for(req$component_id)))
        good_translation(code_for(req$component_id))
      })
      result <- sas_translate(main, config = config, out_dir = withr::local_tempdir(),
        outputs = list(datasets = "work.result"), llm = llm)
      expect_identical(result$status, "needs_review", info = use)
      expect_match(result$status_reason, "startup_libraries_require_review", fixed = TRUE)
      expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "result.rds"))$value,
        if (conditional) 3 else 1, info = use)
    }
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
