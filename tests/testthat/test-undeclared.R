test_that("referenced-but-undeclared librefs are flagged once each", {
  dir <- withr::local_tempdir()
  writeLines("data w1; set adam.adsl; run;\ndata w2; set adam.advs; run;",
             file.path(dir, "a.sas"))
  p <- sas_project(dir)
  fl <- p$flags[p$flags$kind == "libref_undeclared", ]
  expect_identical(fl$detail, "adam")
})

test_that("declared librefs (statement or config) are not flagged", {
  dir <- withr::local_tempdir()
  writeLines("libname sdtm '/x';\ndata w; set sdtm.dm; run;", file.path(dir, "a.sas"))
  p <- sas_project(dir)
  expect_false("libref_undeclared" %in% p$flags$kind)

  dir2 <- withr::local_tempdir()
  writeLines("libraries:\n  sdtm: { path: /x }", file.path(dir2, "_sas2r.yml"))
  writeLines("data w; set sdtm.dm; run;", file.path(dir2, "a.sas"))
  p2 <- sas_project(dir2)
  expect_false("libref_undeclared" %in% p2$flags$kind)
})

test_that("project flags and registry surface the gap actionably", {
  dir <- withr::local_tempdir()
  writeLines("data w1; set adam.adsl; run;", file.path(dir, "a.sas"))
  p <- sas_project(dir)
  fl <- p$flags[p$flags$kind == "libref_undeclared", ]
  expect_identical(fl$detail, "adam")

  reg_dir <- withr::local_tempdir()
  sas2r:::write_registry(p, reg_dir)
  reg <- paste(readLines(file.path(reg_dir, "_sas2r_registry.R")), collapse = "\n")
  expect_match(reg, "#  adam = list\\(read_path = \"<FILL")
})

test_that("project tracks environment-provided context with autoexec and sasautos", {
  dir <- withr::local_tempdir()
  writeLines(c("libname sdtm '/x';",
               "options sasautos=('/macros');"),
             file.path(dir, "autoexec.sas"))
  writeLines("data w1; set adam.adsl; run;", file.path(dir, "a.sas"))
  p <- sas_project(dir)
  expect_true(any(p$files$origin == "environment"))
  expect_true("adam" %in% p$flags$detail[p$flags$kind == "libref_undeclared"])
})

test_that("a clear-only libname is not a declaration", {
  dir <- withr::local_tempdir()
  # `libname adam clear;` unbinds; it never says where `adam` lives, so it
  # cannot stand in for the declaration the reference still needs.
  writeLines(c("libname adam clear;", "data w; set adam.adsl; run;"),
             file.path(dir, "a.sas"))
  p <- sas_project(dir)
  expect_identical(p$flags$detail[p$flags$kind == "libref_undeclared"], "adam")
})
