ordered_future_writer_fixture <- function(macro = FALSE, envir = parent.frame()) {
  writer <- if (macro) "%derive;" else "data adam.input; id=1; run;"
  fx <- ordered_fixture(list("a.sas" = c("%setup;", "data adam.out; set adam.input; run;"),
    "b.sas" = writer), envir = envir)
  dir.create(file.path(fx$root, "macros"))
  dir.create(file.path(fx$root, "adam"))
  fx$config$macro_search_path <- file.path(fx$root, "macros")
  fx$config$libraries <- list(adam = file.path(fx$root, "adam"))
  writeLines("%macro setup; %put NOTE: setup; %mend;", file.path(fx$root, "macros", "setup.sas"))
  writeLines("%macro derive; data adam.input; id=1; run; %mend;",
    file.path(fx$root, "macros", "derive.sas"))
  fx
}

test_that("only preceding writes explain uncertain permanent inputs", {
  for (macro in c(FALSE, TRUE)) {
    fx <- ordered_future_writer_fixture(macro)
    check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(check$inputs$status, "missing")
    expect_true(any(vapply(check$readiness$warnings, function(w)
      w$kind == "input_missing" && w$blocks_execution, logical(1))))
    fx$config$migration$execution_order <- c("b.sas", "a.sas")
    check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(check$inputs$status, "deferred")
  }
  # Multi-statement procedures remain uncertain, but their possible writes
  # enter the prefix only after the unit, not before its input reads.
  fx <- ordered_future_writer_fixture()
  writeLines(c("%setup;", "proc sql; create table adam.input as select * from adam.input;",
    "select * from adam.input; quit;", "data out; set adam.input; run;"), file.path(fx$root, "a.sas"))
  check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_identical(check$inputs$status, c("missing", "missing", "deferred"))
})

test_that("macro positions remain distinct on one line and through includes", {
  fx <- ordered_future_writer_fixture(TRUE)
  fx$config$migration$execution_order <- "a.sas"
  unlink(file.path(fx$root, "b.sas"))
  writeLines("%setup; data out; set adam.input; run; %derive;", file.path(fx$root, "a.sas"))
  expect_identical(sas_preflight(fx$root, config = fx$config, diagnose = "off")$inputs$status, "missing")
  writeLines("%setup; %derive; data out; set adam.input; run;", file.path(fx$root, "a.sas"))
  expect_identical(sas_preflight(fx$root, config = fx$config, diagnose = "off")$inputs$status, "deferred")
  writeLines("%macro second; data adam.second; id=2; run; %mend;",
    file.path(fx$root, "macros", "second.sas"))
  writeLines("%derive; %second;", file.path(fx$root, "macros", "included.sas"))
  writeLines(c("%setup; data before; set adam.input; run;",
    "%include 'macros/included.sas';", "data after; set adam.input adam.second; run;"),
    file.path(fx$root, "a.sas"))
  expect_identical(sas_preflight(fx$root, config = fx$config, diagnose = "off")$inputs$status,
    c("missing", "deferred", "deferred"))
})

test_that("reused macro summaries retain separate caller library bindings", {
  fx <- ordered_future_writer_fixture(TRUE)
  first <- fx$config$libraries$adam
  second <- file.path(fx$root, "second")
  dir.create(second)
  fx$config$libraries$adam <- NULL
  writeLines(c(sprintf("libname adam '%s';", first), "%derive;"), file.path(fx$root, "a.sas"))
  writeLines(c(sprintf("libname adam '%s';", second), "%derive;",
    "data out; set adam.input; run;"), file.path(fx$root, "b.sas"))
  expect_identical(sas_preflight(fx$root, config = fx$config, diagnose = "off")$inputs$status, "deferred")
  writeLines(c(sprintf("libname adam '%s';", second), "data out; set adam.input; run;"),
    file.path(fx$root, "b.sas"))
  expect_identical(sas_preflight(fx$root, config = fx$config, diagnose = "off")$inputs$status, "missing")
})

test_that("wrong declared orders stop before execution or fixer calls", {
  skip_if_not_installed("callr")
  for (macro in c(FALSE, TRUE)) {
    fx <- ordered_future_writer_fixture(macro)
    fixer_calls <- 0L
    llm <- new_llm(function(request, audit_context = list()) {
      role <- audit_context$agent %||% audit_context$purpose
      cid <- audit_context$component_id %||% ""
      code <- if (cid == "a") 'setup(); lib_write(lib_read("adam", "input"), "adam", "out")' else
        if (cid == "b") { if (macro) "derive()" else 'lib_write(data.frame(id=1), "adam", "input")' } else
        if (grepl("setup", cid)) "setup <- function() invisible(NULL)" else
        'derive <- function() { lib_write(data.frame(id=1), "adam", "input"); invisible(NULL) }'
      answer <- if (grepl("fix", request$schema_name %||% "")) {
        fixer_calls <<- fixer_calls + 1L
        valid_program_fix_response(code)
      } else if (identical(role, "translator")) valid_program_translation_response(code) else
        valid_program_review_response()
      normalize_provider_response(answer, request, "mock")
    }, provider = "mock", capabilities = llm_capabilities(structured_output = "native",
      tool_calling = "native", tools_with_structured_output = "supported"))
    result <- suppressMessages(suppressWarnings(sas_translate(fx$root, config = fx$config, llm = llm,
      out_dir = withr::local_tempdir(), outputs = "adam.out", max_program_repair_rounds = 0L,
      max_bundle_repair_rounds = 2L)))
    report <- jsonlite::fromJSON(result$report_json_path, simplifyVector = FALSE)
    expect_identical(result$status, "needs_review")
    expect_match(report$outcome$stages[["Bundle execution"]], "NOT RUN (0 attempts)", fixed = TRUE)
    expect_equal(fixer_calls, 0L)
  }
})

test_that("runtime input classification ignores later declared root writers", {
  fx <- ordered_future_writer_fixture()
  project <- sas_preflight(fx$root, config = fx$config, diagnose = "off")$project
  state <- new_migration_state(project, out_dir = withr::local_tempdir(), config = project$config)
  condition <- list(message = "Dataset not found: adam.input")
  expect_identical(non_translation_runtime_reason(state, "a", condition),
    "source_input_unavailable: adam.input")
  fx$config$migration$execution_order <- c("b.sas", "a.sas")
  project <- sas_preflight(fx$root, config = fx$config, diagnose = "off")$project
  state <- new_migration_state(project, out_dir = withr::local_tempdir(), config = project$config)
  expect_null(non_translation_runtime_reason(state, "a", condition))
})
