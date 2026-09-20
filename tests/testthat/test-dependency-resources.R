test_that("SAS metadata findings do not invent producers or exempt ordinary datasets", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  metadata <- c("SASHELP.VEXTFL", "dictionary.extfiles", "SasHelp.VColumn",
    "DICTIONARY.LIBNAMES", "sashelp.vmacro", "dictionary.tables", "sashelp.voption")
  unknown <- c("sashelp.class", "sashelp.vunknown", "dictionary.unknown",
    "work.vextfl", "missing_program", "%missing_macro")
  state$selected_revisions$p01$contract$discovered_dependencies <- c(metadata, unknown)
  expect_identical(parallel_dependency_findings(state, "p01"), unknown)
  expect_identical(state$selected_revisions$p01$contract$discovered_dependencies, c(metadata, unknown))
})

test_that("configured LIBNAME paths cover source symbols without a naming convention", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "input")); dir.create(file.path(root, "output"))
  writeLines(c('libname sdtm "&SDTM_PATH";', 'libname adam "&ADAM_PATH";',
    'libname extra "&StudyRoot./archive";',
    'data adam.result; set sdtm.input; run;'), file.path(root, "derive.sas"))
  config <- list(libraries = list(sdtm = "input", adam = "output", extra = "input"))
  project <- sas_project(root, config = config)
  state <- new_migration_state(project, file.path(root, "migration"))
  found <- c("SDTM_PATH", "adam_path", "StudyRoot", "&sdtm_path.", "&ADAM_PATH")
  unknown <- c("UNCONFIGURED_PATH", "&UNCONFIGURED_PATH", "%SDTM_PATH", "work.missing")
  state$selected_revisions$derive$contract$suspected_dependencies <- c(found, unknown)
  expect_identical(parallel_dependency_findings(state, "derive"), unknown)
  expect_setequal(configured_path_dependency_symbols(project, "derive"),
    c("sdtm_path", "adam_path", "studyroot"))
  context <- build_translator_context("derive", project, graph = project$graph)$context_packet
  expect_match(context, "LIBNAME path variables covered", fixed = TRUE)
  expect_match(context, "sdtm_path, adam_path, studyroot", fixed = TRUE)

  unconfigured <- sas_project(root, config = list())
  state$project <- unconfigured
  state$graph <- unconfigured$graph
  expect_identical(parallel_dependency_findings(state, "derive"), c(found, unknown))
})

test_that("a path binding does not resolve variables used elsewhere or in another component", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "input"))
  config <- list(libraries = list(raw = "input"))
  for (extra in c('infile "&Root./external.csv";', 'value = &Root;',
                  'libname other "&Root";')) {
    writeLines(c('libname raw "&Root";', 'data work.out;', extra, 'run;'),
      file.path(root, "derive.sas"))
    project <- sas_project(root, config = config)
    expect_length(configured_path_dependency_symbols(project, "derive"), 0L)
  }
  writeLines(c('libname raw "&Root";', "data work.out; text='&Root'; run;"),
    file.path(root, "derive.sas"))
  writeLines('data work.other; value = &Root; run;', file.path(root, "other.sas"))
  project <- sas_project(root, config = config)
  expect_identical(configured_path_dependency_symbols(project, "derive"), "root")
  expect_length(configured_path_dependency_symbols(project, "other"), 0L)
  writeLines('libname raw "&Root"; libname other "&Root"; data out; x=1; run;',
    file.path(root, "derive.sas"))
  project <- sas_project(root, config = config)
  expect_length(configured_path_dependency_symbols(project, "derive"), 0L)
})

test_that("metadata context keeps unsupported behavior visible to the existing review", {
  root <- withr::local_tempdir()
  writeLines(c('%let dsid = %sysfunc(open(sashelp.vextfl));',
    'data work.out; value=1; run;'), file.path(root, "metadata.sas"))
  project <- sas_project(root)
  context <- render_component_libraries(project, "metadata")
  expect_match(context, "SAS session metadata resources: sashelp.vextfl", fixed = TRUE)
  expect_match(context, "not missing study datasets or upstream programs", fixed = TRUE)
  expect_match(context, "Keep unresolved behavior in uncertainty", fixed = TRUE)
})

test_that("environment observations reach bundle execution while required outputs are checked", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  source <- file.path(fx$root, "p01.sas")
  writeLines(c('libname raw "&INPUT_LOCATION";',
    sub("value =", "retain marker 1; value =", readLines(source), fixed = TRUE)), source)
  translated <- sub("x$value <-", "x$marker <- 1\nx$value <-", fx$fixed$p01, fixed = TRUE)
  observations <- c("SASHELP.VEXTFL", "DICTIONARY.EXTFILES", "INPUT_LOCATION")
  responses <- list(
    "translator:p01" = valid_program_translation_response(translated,
      suspected_dependencies = observations),
    "translator:p02" = valid_program_translation_response(fx$fixed$p02),
    reviewer = valid_program_review_response())
  result <- sas_translate(fx$root, out_dir = file.path(fx$root, "public-run"),
    config = fx$state$config, llm = parallel_test_llm(responses),
    max_parallel_translations = 4L, max_program_repair_rounds = 0L,
    max_bundle_repair_rounds = 0L, outputs = "work.out2")
  expect_length(result$diagnostics$dependency_findings, 0L)
  expect_identical(result$status, "migration_ready")
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages[["Bundle execution"]], "EXECUTED (1 attempt", fixed = TRUE)
  expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "out2.rds"))$value, 12:14)
  paths <- migration_paths(result$out_dir, result$run_id)
  manifest <- read_json_record(paths$manifest)
  contract <- read_json_record(file.path(dirname(manifest$components$p01$revision_path), "contract.json"))
  expect_identical(contract$suspected_dependencies, observations)
})

test_that("recognizing metadata does not bypass execution failures", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  source <- file.path(fx$root, "p01.sas")
  writeLines(sub("value =", "retain marker 1; value =", readLines(source), fixed = TRUE), source)
  responses <- list(translator = valid_program_translation_response(
    "stop('Required environment query is not implemented')",
    suspected_dependencies = "sashelp.vextfl"), reviewer = valid_program_review_response())
  result <- sas_translate(fx$root, out_dir = file.path(fx$root, "public-run"),
    config = fx$state$config, llm = parallel_test_llm(responses),
    max_parallel_translations = 2L, max_program_repair_rounds = 0L,
    max_bundle_repair_rounds = 0L, outputs = "work.out1")
  expect_identical(result$status, "blocked")
  report <- read_json_record(result$report_json_path)
  expect_match(report$outcome$stages[["Bundle execution"]], "1 failed", fixed = TRUE)
})

test_that("resume reassesses old dependency blocks and retains genuine ones", {
  for (finding in c("SASHELP.VEXTFL", "work.missing")) {
    fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
    state <- fx$state
    state$selected_revisions$p01$contract$discovered_dependencies <- finding
    llm <- parallel_test_llm(list(reviewer = valid_program_review_response()))
    state$translator_llm <- state$reviewer_llm <- state$fixer_llm <- llm
    state$parallel <- resolve_parallel_execution(state, 2L)
    state$resume_fingerprint <- migration_resume_fingerprint(state)
    # This is how the previous scheduler recorded both kinds of finding.
    blocked <- state
    blocked$diagnostics$dependency_findings$p01 <- list(findings = finding,
      affected = c("p01", "p02"), reason = "source_reconciliation_required")
    blocked$diagnostics$execution_deferred <- finding
    write_migration_checkpoint(blocked, state$resume_fingerprint)
    resumed <- restore_migration_checkpoint(state, state$resume_fingerprint)
    result <- run_program_pipeline(resumed, execute = FALSE)
    expect_null(result$diagnostics$execution_deferred)
    if (finding == "SASHELP.VEXTFL") {
      expect_length(result$diagnostics$dependency_findings, 0L)
      expect_identical(result$component_stage$p02, "settled")
    } else {
      expect_identical(result$component_stage$p02, "settled")
      expect_true(length(component_execution_reasons(result, "p02")) > 0L)
      expect_identical(result$diagnostics$dependency_findings$p01$findings, finding)
    }
  }
})

test_that("macro variables defined by project source are producers, not missing dependencies", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "input"))
  saveRDS(data.frame(id = 1:3, value = 10:12), file.path(root, "input", "input.rds"))
  config <- list(libraries = list(raw = list(path = file.path(root, "input"), engine = "rds")))
  writeLines(c("%let max_boxes_per_page = 20;", "%global study_flag other_flag;",
    "data work.out1; set raw.input; call symput('row_count', put(_n_, 8.)); run;",
    "proc sql noprint; select max(value), min(value) into :top_value, :low_value",
    "  from work.out1; quit;"), file.path(root, "caller.sas"))
  writeLines(c("data work.out2; set work.out1;",
    "  if value > &max_boxes_per_page then flag = \"&study_flag\";", "run;"),
    file.path(root, "consumer.sas"))
  project <- sas_project(root, config = config)
  expect_setequal(project_macro_variable_definitions(project),
    c("max_boxes_per_page", "study_flag", "other_flag", "row_count", "top_value", "low_value"))
  state <- new_migration_state(project, file.path(root, "migration"))
  defined <- c("max_boxes_per_page", "&MAX_BOXES_PER_PAGE.", "Study_Flag", "other_flag",
    "row_count", "&top_value", "low_value")
  unknown <- c("undefined_setting", "&undefined_setting", "%missing_macro", "work.missing")
  state$selected_revisions$consumer$contract$suspected_dependencies <- c(defined, unknown)
  expect_identical(parallel_dependency_findings(state, "consumer"), unknown)
  # A supplied project macro or another scheduled component is not missing
  # either, even when this component's own graph closure does not include it.
  writeLines(c("%macro helper(ds);", "  data &ds._h; set &ds; run;", "%mend helper;"),
    file.path(root, "helper.sas"))
  writeLines("data work.out3; set raw.input; run;", file.path(root, "other.sas"))
  state$project <- sas_project(root, config = config)
  state$graph <- state$project$graph
  state$schedule <- state$project$schedule
  state$selected_revisions$consumer$contract$suspected_dependencies <-
    c("helper", "%HELPER", "other", "caller", unknown)
  expect_identical(parallel_dependency_findings(state, "consumer"), unknown)
})

test_that("unverifiable agent-reported names warn while missing macros and datasets still defer execution", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  reported <- c("undefined_setting", "&undefined_setting", "%missing_macro", "work.missing")
  state <- record_dependency_finding(fx$state, "p01", reported)
  entry <- state$diagnostics$dependency_findings$p01
  expect_identical(entry$findings, c("%missing_macro", "work.missing"))
  expect_identical(entry$advisory, c("undefined_setting", "&undefined_setting"))
  expect_setequal(entry$affected, c("p01", "p02"))
  reasons <- component_execution_reasons(state, "p02")
  expect_length(reasons, 1L)
  expect_match(reasons, "p01 (%missing_macro, work.missing)", fixed = TRUE)
  expect_false(grepl("undefined_setting", reasons, fixed = TRUE))
  context <- paste(component_readiness_context(state$project, "p02"), collapse = "\n")
  expect_match(context, "%missing_macro, work.missing", fixed = TRUE)
  expect_match(context, "undefined_setting, &undefined_setting", fixed = TRUE)
  expect_match(context, "not blocking execution", fixed = TRUE)

  advisory_only <- record_dependency_finding(fx$state, "p01", "undefined_setting")
  expect_length(advisory_only$diagnostics$dependency_findings$p01$findings, 0L)
  expect_identical(advisory_only$diagnostics$dependency_findings$p01$advisory, "undefined_setting")
  expect_length(component_execution_reasons(advisory_only), 0L)
  expect_match(paste(component_readiness_context(advisory_only$project, "p02"), collapse = "\n"),
    "undefined_setting", fixed = TRUE)
})

test_that("a revised contract retracts the dependency findings it no longer reports", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  state <- record_dependency_finding(fx$state, "p01", c("work.missing", "%missing_macro"))
  expect_true(length(component_execution_reasons(state, "p02")) > 0L)
  state <- record_dependency_finding(state, "p01", "%missing_macro")
  expect_identical(state$diagnostics$dependency_findings$p01$findings, "%missing_macro")
  expect_match(component_execution_reasons(state, "p02"), "p01 (%missing_macro)", fixed = TRUE)
  state <- record_dependency_finding(state, "p01", character())
  expect_null(state$diagnostics$dependency_findings$p01)
  expect_length(component_execution_reasons(state, "p02"), 0L)
  expect_false(any(grepl("missing", component_readiness_context(state$project, "p02"), fixed = TRUE)))
})
