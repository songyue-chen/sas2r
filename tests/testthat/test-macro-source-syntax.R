test_that("macro header options do not become parameters", {
  headers <- c(
    '%macro report(a, b=1) / des="Summary (Phase III)"',
    '%macro report(a, path="/study/(final)/", value=%sysfunc(cats(a,b))) / des="Summary (III)"',
    '%macro report / des="Summary (III)"',
    '%macro report() / des="Summary (III)"'
  )
  expected <- c("a, b=1", 'a, path="/study/(final)/", value=%sysfunc(cats(a,b))', "", "")
  for (i in seq_along(headers)) {
    units <- sas_units(sas_statements(paste0(headers[i], "; %mend;")))
    defs <- extract_macro_defs(units)
    expect_identical(defs$params, expected[i])
    expect_no_error(parse_macro_contract(defs$name, defs$params))
  }
  units <- sas_units(sas_statements("%macro report(a, b=1; %mend;"))
  expect_error(extract_macro_defs(units), "Unterminated parameter list",
               class = "sas2r_macro_contract_error")
})

test_that("documented NLS macro functions do not require project macro files", {
  # SAS NLS function/autocall entries; QKLOWCAS is the documented spelling.
  expressions <- c(
    "%kcmpres(a b)", "%kindex(a,b)", "%kleft(a)", "%klength(a)",
    "%klowcase(A)", "%kscan(a b,1)", "%ksubstr(abc,1,2)", "%ktrim(a)",
    "%kupcase(a)", "%kverify(a,b)", "%qkleft(a)", "%qklowcas(A)",
    "%qkscan(a b,1)", "%qksubstr(abc,1,2)", "%qktrim(a)", "%qkupcase(a)"
  )
  file <- withr::local_tempfile(fileext = ".sas")
  writeLines(paste0("%let value=", expressions, ";"), file)
  project <- sas_project(file)
  expect_equal(nrow(project$macros$calls), 0L)
  # A k/qk prefix alone must not hide user-defined functions or typos.
  units <- sas_units(sas_statements("%kproject(); %qkproject(); %qklength(a);"))
  expect_identical(extract_macro_calls(units)$name, c("kproject", "qkproject", "qklength"))
})

test_that("unterminated macro comments fail with the source location before translation", {
  root <- withr::local_tempdir()
  file <- file.path(root, "program.sas")
  out <- file.path(root, "out")
  requests <- 0L
  llm <- new_llm(function(request, ...) {
    requests <<- requests + 1L
    stop("provider must not be reached")
  }, provider = "mock")
  sources <- list(
    c("", "%* Don't run this step;", "data out; set inp; %util_score(var); run;"),
    c("", "%* missing terminator")
  )
  for (source in sources) {
    writeLines(source, file)
    error <- tryCatch(sas_translate(file, out_dir = out, llm = llm), error = identity)
    expect_s3_class(error, "sas2r_sas_parse_error")
    expect_match(conditionMessage(error), "program.sas:2", fixed = TRUE)
    expect_match(conditionMessage(error), "Unterminated macro comment", fixed = TRUE)
    expect_match(conditionMessage(error), "quotation marks", fixed = TRUE)
  }
  expect_identical(requests, 0L)
  expect_length(list.files(out, pattern = "START_HERE.html", recursive = TRUE), 2L)
  writeLines('%* "hidden; %also_hidden"; data out; %real_call; run;', file)
  expect_identical(sas_project(file)$macros$calls$name, "real_call")
  writeLines("/* Don't run %hidden; */ data out; %real_call; run;", file)
  expect_identical(sas_project(file)$macros$calls$name, "real_call")
})

test_that("unterminated library comments retain the library source location", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  file <- file.path(root, "program.sas")
  writeLines("%helper;", file)
  writeLines(c("%macro helper;", "%* Don't run;", "%mend;"),
             file.path(root, "macros", "helper.sas"))
  expect_error(sas_project(file, config = list(macro_search_path = file.path(root, "macros"))),
               "helper.sas:2", class = "sas2r_sas_parse_error")
})

test_that("NRSTR requires percent-marked unmatched quotation marks", {
  valid <- "%let text=%nrstr(Don%'t modify %literal); %actual;"
  units <- sas_units(sas_statements(valid))
  scan <- macro_call_scan(units)
  expect_equal(nrow(scan$findings), 0L)
  expect_identical(extract_macro_calls(units, scan)$name, "actual")
  invalid <- "%let text=%nrstr(Don't modify %literal); %actual;"
  scan <- macro_call_scan(sas_units(sas_statements(invalid)))
  expect_identical(scan$findings$kind, "macro_dependency_analysis_deferred")
})

test_that("double-quoted parameterless invocations cannot silently become literals", {
  file <- withr::local_tempfile(fileext = ".sas")
  writeLines('proc sql; select * from patients where site like "%Total%"; quit;', file)
  project <- sas_project(file)
  expect_identical(project$macros$resolution$name, "total")
  expect_identical(project$macros$resolution$status, "unresolved")
  writeLines("proc sql; select * from patients where site like '%Total%'; quit;", file)
  expect_equal(nrow(sas_project(file)$macros$calls), 0L)
  writeLines(c("%macro total; Total %mend;", 'title "10%total";'), file)
  project <- sas_project(file)
  expect_identical(project$macros$resolution$status, "resolved_project")
})

test_that("macro statements and adjacent percent words in star comments remain unverified", {
  file <- withr::local_tempfile(fileext = ".sas")
  for (comment in c("* incidence >5%All patients;", "* Note: %let value=1;", "* %Y-%m-%d;")) {
    writeLines(comment, file)
    project <- sas_project(file)
    expect_match(project$flags$detail[project$flags$kind == "macro_dependency_analysis_deferred"],
                 "statement comment requires expansion", fixed = TRUE)
  }
})
