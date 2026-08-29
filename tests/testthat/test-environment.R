env_project <- function(with_config = TRUE) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(c("libname adam '/data/adam';",
               "%macro setup_flags; %mend;",
               "options mprint sasautos=('/org/macros');"),
             file.path(dir, "autoexec.sas"))
  writeLines("data w1; set adam.adsl; %setup_flags() run;", file.path(dir, "a.sas"))
  if (with_config)
    writeLines(c("environment:", "  autoexec:", "    - autoexec.sas"),
               file.path(dir, "_sas2r.yml"))
  sas_project(dir)
}

test_that("autoexec seeds librefs and session-compiled macros", {
  p <- env_project()
  expect_true("adam" %in% p$librefs$libref)
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "setup_flags"], "resolved_project")
  expect_identical(unique(p$files$origin[basename(p$files$file) == "autoexec.sas"]),
                   "environment")
})

test_that("options sasautos extends the macro search path with provenance", {
  p <- env_project()
  expect_true("/org/macros" %in% p$config$macro_search_path)
  expect_true("sasautos_from_environment" %in% p$flags$kind)
})

test_that("autoexec.sas is auto-discovered under zero config", {
  p <- env_project(with_config = FALSE)
  expect_true("autoexec_autodiscovered" %in% p$flags$kind)
  expect_true("adam" %in% p$librefs$libref)
})

test_that("environment units are not translation targets", {
  p <- env_project()
  m <- sas_transpile(p, withr::local_tempdir())$manifest
  expect_false(any(grepl("autoexec", basename(m$file))))
})

test_that("options sasautos supports single unparenthesized path", {
  dir <- withr::local_tempdir()
  writeLines("options sasautos='/single/macro/dir';", file.path(dir, "autoexec.sas"))
  writeLines("data w1; set work.a; run;", file.path(dir, "a.sas"))
  p <- sas_project(dir)
  expect_true("/single/macro/dir" %in% p$config$macro_search_path)
  expect_true("sasautos_from_environment" %in% p$flags$kind)
})
