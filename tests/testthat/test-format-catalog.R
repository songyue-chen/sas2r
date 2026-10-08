test_that("named formats work through both helpers in caller scope", {
  catalog <- list(yn = list(values = c("0" = "NO", "1" = "YES")),
                  "$yn" = list(values = c("0" = "zero", "1" = "one")))
  bundled <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), bundled)
  for (helpers in list(list(apply_format = apply_format, sas_put = sas_put), bundled)) {
    scope <- new.env(parent = globalenv())
    scope$.sas2r_formats <- catalog
    scope$format <- helpers$apply_format
    scope$put <- helpers$sas_put
    expect_identical(evalq(format(c(0, 1), "YN."), scope), c("NO", "YES"))
    expect_identical(evalq(put(c("0", "1"), "$YN."), scope), c("zero", "one"))
    expect_identical(evalq(local({ f <- function(x) put(x, "yn"); f(c(1, 0)) }), scope),
                     c("YES", "NO"))
    expect_identical(helpers$sas_put(c(0, 1), "yn", catalog = catalog), c("NO", "YES"))
    expect_identical(helpers$sas_put(c(0, 1), catalog$yn), c("NO", "YES"))
    expect_identical(helpers$sas_put(c(1, NA), NULL), c("1", NA_character_))
    expect_error(helpers$sas_put(1, "absent", catalog = catalog), "SAS format not found.*absent")
    expect_error(helpers$sas_put(1, "$yn", catalog = catalog["yn"]), "SAS format not found")
    expect_error(helpers$sas_put(1, "yn", catalog = catalog["$yn"]), "SAS format not found")
    expect_error(helpers$sas_put(1, "yn", catalog = NULL), "SAS format not found")
    expect_error(helpers$sas_put(1, NA_character_, catalog = catalog), "one non-empty string")
  }
})

test_that("startup loads one compiled catalog for programs and macro functions", {
  root <- withr::local_tempdir()
  writeLines("proc format; value yn 0='NO' 1='YES'; value $yn '0'='zero' '1'='one'; run;",
    file.path(root, "autoexec.sas"))
  writeLines("data work.result; flag=put(1,yn.); run;", file.path(root, "main.sas"))
  project <- sas_project(file.path(root, "main.sas"))
  out <- withr::local_tempdir()
  write_helpers(out)
  write_formats(compile_format_catalog(project)$catalog, out)
  write_autoexec(project, out)
  dir.create(file.path(out, "R", "macros"), recursive = TRUE)
  writeLines('format_flag <- function(x) sas_put(x, "yn.")',
    file.path(out, "R", "macros", "format_flag.R"))
  scope <- new.env(parent = globalenv())
  sys.source(file.path(out, "autoexec.R"), envir = scope, chdir = TRUE)
  expect_identical(evalq(sas_put(c(0, 1), "YN."), scope), c("NO", "YES"))
  expect_identical(scope$format_flag(c(1, 0)), c("YES", "NO"))
  expect_identical(evalq(sas_put(c("1", "0"), "$yn"), scope), c("one", "zero"))
  expect_false(exists("sas_formats", envir = scope, inherits = FALSE))

  selected <- list(setup = list(revision_id = "r1", r_code = "sas_formats <- list()"))
  context <- list(project = project, component_id = "main", selected_revisions = selected)
  dependency <- read_dependency_context(context, "setup", "r")
  expect_identical(dependency$code, selected$setup$r_code)
  expect_match(dependency$execution_note, "not executed as a program", fixed = TRUE)
  guidance <- build_agent_guidance(project, "main", selected_revisions = selected)
  expect_match(guidance$text, dependency$execution_note, fixed = TRUE)
})
