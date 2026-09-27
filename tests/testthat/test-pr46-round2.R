permanent_macro_fixture <- function(envir = parent.frame()) {
  fx <- ordered_fixture(list("a.sas" = "%derive;",
    "b.sas" = "data adam.final; set adam.stage; run;"), envir = envir)
  for (dir in c("macros", "raw", "adam")) dir.create(file.path(fx$root, dir))
  fx$config$macro_search_path <- file.path(fx$root, "macros")
  fx$config$libraries <- list(raw = file.path(fx$root, "raw"), adam = file.path(fx$root, "adam"))
  writeLines("%macro derive; data adam.stage; set raw.input; run; %mend;",
    file.path(fx$root, "macros", "derive.sas"))
  saveRDS(data.frame(id = 1:3), file.path(fx$root, "raw", "input.rds"))
  fx
}

test_that("called macro outputs defer permanent reads without hiding external inputs", {
  fx <- permanent_macro_fixture()
  writeLines("data adam.final; set adam.stage raw.absent; run;", file.path(fx$root, "b.sas"))
  for (body in c("data adam.stage; set raw.input; run;",
                 "proc sql; create table adam.stage as select * from raw.input; quit;",
                 "proc sort data=raw.input out=adam.stage; by id; run;")) {
    writeLines(paste("%macro derive;", body, "%mend;"),
      file.path(fx$root, "macros", "derive.sas"))
    check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(check$inputs$status, c("deferred", "missing"))
    expect_true(any(vapply(check$readiness$warnings, function(w)
      w$kind == "input_missing" && w$blocks_execution, logical(1))))
    expect_false(any(dataset_producers(check$project)$generated))
  }
  # Uncalled definitions and dynamic output names are not literal writers.
  for (body in list(c("%macro derive; %put setup; %mend;",
                     "%macro unused; data adam.stage; x=1; run; %mend;"),
                   "%macro derive; data adam.&name; x=1; run; %mend;")) {
    writeLines(c(body, "%derive;"), file.path(fx$root, "a.sas"))
    check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
    expect_identical(check$inputs$status, c("missing", "missing"))
  }
})

test_that("macro output identities use the called library and reachable nested definitions", {
  fx <- permanent_macro_fixture()
  writeLines("%macro derive; %inner; %mend;", file.path(fx$root, "macros", "derive.sas"))
  writeLines("%macro inner; data adam.stage; set raw.input; run; %mend;",
    file.path(fx$root, "macros", "inner.sas"))
  original <- fx$config$libraries$adam
  fx$config$libraries$adam <- NULL
  writeLines(c(sprintf("libname adam '%s';", original), "%derive;"), file.path(fx$root, "a.sas"))
  writeLines(c(sprintf("libname adam '%s';", original), "data final; set adam.stage; run;"),
    file.path(fx$root, "b.sas"))
  check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_identical(check$inputs$status, "deferred")
  other <- file.path(fx$root, "other")
  dir.create(other)
  writeLines(c(sprintf("libname adam '%s';", other), "data final; set adam.stage; run;"),
    file.path(fx$root, "b.sas"))
  check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_identical(check$inputs$status, "missing")
})

test_that("a declared migration runs a reader of a macro-created permanent dataset", {
  skip_if_not_installed("callr")
  fx <- permanent_macro_fixture()
  llm <- new_llm(function(request, audit_context = list()) {
    role <- audit_context$agent %||% audit_context$purpose
    cid <- audit_context$component_id %||% ""
    answer <- if (identical(role, "translator")) valid_program_translation_response(
      if (cid == "a") "derive()" else if (cid == "b")
        'lib_write(lib_read("adam", "stage"), "adam", "final")' else
        'derive <- function() { lib_write(lib_read("raw", "input"), "adam", "stage"); invisible(NULL) }'
    ) else valid_program_review_response()
    normalize_provider_response(answer, request, "mock")
  }, provider = "mock", capabilities = llm_capabilities(structured_output = "native",
    tool_calling = "native", tools_with_structured_output = "supported"))
  result <- suppressMessages(suppressWarnings(sas_translate(fx$root, config = fx$config,
    llm = llm, out_dir = withr::local_tempdir(), outputs = "adam.final",
    max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)))
  expect_identical(result$status, "migration_ready")
  output <- list.files(dirname(result$bundle_dir), pattern = "^final.rds$",
    recursive = TRUE, full.names = TRUE)
  expect_true(length(output) > 0L)
  for (file in output) expect_equal(readRDS(file), data.frame(id = 1:3))
})

test_that("preflight explains shorter bundle limits without changing configured values", {
  fx <- ordered_fixture(list("a.sas" = "data a; x=1; run;", "b.sas" = "data b; x=2; run;",
    "c.sas" = "data c; x=3; run;"))
  check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_match(paste(check$next_actions, collapse = "\n"), "120 seconds.*180 seconds")
  expect_equal(check$project$config$migration$bundle_timeout, 120)
  fx$config$migration$bundle_timeout <- 180
  check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_false(any(grepl("below the final ordered smoke", check$next_actions)))
  fx$config$migration$execution_order <- NULL
  fx$config$migration$bundle_timeout <- 1
  check <- sas_preflight(fx$root, config = fx$config, diagnose = "off")
  expect_false(any(grepl("below the final ordered smoke", check$next_actions)))
})

test_that("diagnosis notices describe admission and print incomplete response details", {
  llm <- diagnosis_mock(callback = function(request) stop("budget must prevent this call"))
  expect_message(check <- sas_preflight(diagnosis_fixture(), llm = llm,
    usage_limits = list(max_calls = 0)), "may send.*subject to budget admission")
  expect_equal(check$model_calls, 0L)
  llm <- new_llm(function(request, ...) new_llm_response(status = "incomplete",
    action = "none", finish_reason = "length", request = request, provider = "mock"), provider = "mock")
  check <- sas_preflight(diagnosis_fixture(), llm = llm)
  lines <- paste(preflight_diagnosis_lines(check$diagnosis), collapse = "\n")
  expect_match(lines, "Finish reason: length; max_output_tokens = 4096")
  expect_match(lines, "model's context limit")
  expect_equal(check$model_calls, 1L)
})
