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
