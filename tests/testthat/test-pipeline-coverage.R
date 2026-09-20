test_that("preflight accounts for roots, includes, startup, multiple macros and empty files", {
  root <- withr::local_tempdir()
  for (dir in c("programs", "includes", "macros")) dir.create(file.path(root, dir))
  writeLines("%let marker=1;", file.path(root, "programs", "autoexec.sas"))
  writeLines("/* intentionally empty */", file.path(root, "programs", "empty.sas"))
  writeLines("data work.seed; x=1; run;", file.path(root, "programs", "seed.sas"))
  writeLines("data work.stage; set work.seed; run;", file.path(root, "includes", "derive_stage.sas"))
  writeLines(c("%macro alpha; %put alpha; %mend;", "%macro beta; %put beta; %mend;"),
    file.path(root, "macros", "library.sas"))
  writeLines(c("%include '../includes/derive_stage.sas';", "%alpha; %beta;",
    "data work.out; set work.stage; run;"), file.path(root, "programs", "main.sas"))
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  check <- sas_preflight(file.path(root, "programs"),
    config = list(macro_search_path = file.path(root, "macros")))
  pipeline <- check$pipeline
  expect_identical(pipeline$status, "complete")
  expect_length(pipeline$issues, 0L)
  expect_setequal(pipeline$sources$file, check$sources$file)
  source <- function(name) pipeline$sources[basename(pipeline$sources$file) == name, ]
  expect_identical(source("empty.sas")$status, "excluded")
  expect_identical(source("autoexec.sas")$execution_roles[[1L]], "startup")
  expect_identical(source("derive_stage.sas")$execution_roles[[1L]], "included by caller")
  expect_setequal(source("library.sas")$components[[1L]], c("macro__alpha", "macro__beta"))
  expect_identical(source("library.sas")$execution_roles[[1L]], c("called macro", "called macro"))
  expect_identical(pipeline$execution_order, c("seed", "main"))
  expect_identical(pipeline$execution_order, build_bundle_execution_plan(check$project$graph)$execution_order)
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE), before)
})

test_that("a planner omission is reported by preflight and refused before provider setup", {
  root <- withr::local_tempdir()
  writeLines("data work.out; retain x 1; run;", file.path(root, "program.sas"))
  original <- translation_plan
  # Inject an ordinary planner omission, leaving the scanner's input intact.
  testthat::local_mocked_bindings(translation_plan = function(...) {
    plan <- original(...)
    plan$schedule <- plan$schedule[FALSE, ]
    plan
  }, sas_llm = function(...) stop("provider setup must not run"))
  check <- sas_preflight(root)
  expect_identical(check$status, "needs_attention")
  expect_identical(check$pipeline$status, "invalid")
  expect_identical(check$pipeline$sources$status, "unplanned")
  expect_match(check$pipeline$issues, "translation components program", fixed = TRUE)
  expect_error(sas_translate(root, out_dir = file.path(root, "out"),
    config = list(llm = list(provider = "openai", model = "unused"))),
    class = "sas2r_pipeline_coverage_error")
  reports <- list.files(file.path(root, "out"), pattern = "^report.json$", recursive = TRUE, full.names = TRUE)
  report <- read_json_record(reports[[1L]])
  expect_identical(report$diagnostics$pipeline$status, "invalid")
  expect_identical(report$usage$calls, 0L)
  expect_identical(report$outcome$severity, "error")
})

test_that("known cycles remain explicit readiness warnings with complete source coverage", {
  root <- withr::local_tempdir()
  writeLines("data work.a; set work.b; run;", file.path(root, "a.sas"))
  writeLines("data work.b; set work.a; run;", file.path(root, "b.sas"))
  check <- sas_preflight(root)
  expect_identical(check$pipeline$status, "complete")
  expect_identical(check$status, "needs_attention")
  expect_true(any(vapply(check$readiness$warnings, function(x) x$kind == "dependency_cycle", logical(1))))
})

test_that("a source name colliding with reserved startup identity is not silently merged", {
  root <- withr::local_tempdir()
  writeLines("%let marker=1;", file.path(root, "autoexec.sas"))
  writeLines("data work.out; x=1; run;", file.path(root, "setup.sas"))
  check <- sas_preflight(root)
  expect_identical(check$pipeline$status, "invalid")
  expect_match(paste(check$pipeline$issues, collapse = " "),
    "Component setup combines different source files", fixed = TRUE)
  expect_error(sas_translate(check$project, out_dir = file.path(root, "out")),
    "Rename the conflicting source file", class = "sas2r_pipeline_coverage_error")
})
