test_that("critical errors still stop instead of being downgraded to component warnings", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  local_mocked_bindings(check_component_revision = function(...) stop(llm_settings_error("provider unavailable")))
  expect_error(run_program_pipeline(fx$state), class = "sas2r_llm_settings_error")
  empty <- withr::local_tempdir()
  writeLines("/* no active source */", file.path(empty, "empty.sas"))
  expect_error(sas_translate(empty, out_dir = file.path(empty, "out")), class = "sas2r_no_translation_source")
})

test_that("environment recognition is source based and preserves actual data dependencies", {
  root <- withr::local_tempdir()
  writeLines(c('options sasautos=("macros") fmtsearch=(work library);',
    '%put &SYSDATE9 &SYSSCP &SYSVER &SYSLAST;',
    'proc sql; select * from dictionary.tables; quit;',
    'data out; set sashelp.class; run;'), file.path(root, "p.sas"))
  check <- sas_preflight(root)
  expect_identical(check$inputs$status[check$inputs$dataset == "dictionary.tables"], "environment")
  expect_false(check$inputs$status[check$inputs$dataset == "sashelp.class"] == "environment")
  names <- c("SYSDATE9", "&SYSSCP.", "SYSVER", "SYSLAST", "SASAUTOS", "FMTSEARCH")
  real <- c("work.sysdate9", "%sysdate9", "SYSUNKNOWN", "sashelp.class", "sashelp.vfake", "SASHELP.VSVW")
  expect_identical(filter_dependency_resources(c(names, real), check$project, "p"), real)
  context <- paste(render_dependency_resources(check$project, "p"), collapse = "\n")
  expect_match(context, "R's version is not SAS's version", fixed = TRUE)
  expect_match(context, "depend on prior operations", fixed = TRUE)
  writeLines('data out; x=1; run;', file.path(root, "q.sas"))
  expect_identical(filter_dependency_resources(names, sas_project(root), "q"), names)
})

test_that("late macro input reports reconcile against the source-bound library", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  dir.create("input")
  dir.create("other")
  haven::write_xpt(data.frame(value = 1), "input/member.xpt")
  writeLines(c("%macro copy(indata);", "data work.out; set &indata; run;", "%mend;",
    "%copy(indata=RAW.MEMBER);"), "derive.sas")
  config <- list(libraries = list(raw = list(path = "input", engine = "xpt")))
  evaluate <- function() {
    setup <- translation_setup(root, config, outputs = NULL, recursive = FALSE)
    state <- list(project = setup$project, graph = setup$plan$graph,
      schedule = setup$plan$schedule)
    state$selected_revisions$derive$contract$discovered_dependencies <-
      c("raw.member", "raw.unconfirmed")
    expect_false("raw.member" %in% state$project$lineage$dataset)
    reports <- parallel_dependency_findings(state, "derive")
    state <- record_dependency_finding(state, "derive", reports)
    state$diagnostics$dependency_findings$derive$findings
  }
  file.copy("input/member.xpt", "input/unconfirmed.xpt")
  expect_identical(evaluate(), "raw.unconfirmed")
  file.rename("input/member.xpt", "input/MEMBER.XPT")
  expect_identical(evaluate(), "raw.unconfirmed")
  # The configured path has the member, but executable SAS selects another
  # library directory at the point of use.
  writeLines(c("libname raw 'other';", readLines("derive.sas")), "derive.sas")
  expect_identical(evaluate(), c("raw.member", "raw.unconfirmed"))
  # A local member that disappears remains eligible to block execution.
  writeLines(readLines("derive.sas")[-1L], "derive.sas")
  unlink("input/MEMBER.XPT")
  expect_identical(evaluate(), c("raw.member", "raw.unconfirmed"))
})
