test_that("quoted includes are resolved, scanned, and their macros participate", {
  dir <- withr::local_tempdir()
  writeLines("%include 'macros/prep.sas';\n%prep()\ndata a; set b; run;",
             file.path(dir, "driver.sas"))
  dir.create(file.path(dir, "macros"))
  writeLines("%macro prep; options nodate; %mend;",
             file.path(dir, "macros", "prep.sas"))
  p <- sas_project(dir)
  expect_identical(sort(unique(p$files$origin)), c("included", "program"))
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "prep"], "resolved_project")
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("include chains resolve transitively", {
  dir <- withr::local_tempdir()
  writeLines("%include 'b.sas';\ndata x; run;", file.path(dir, "a.sas"))
  writeLines("%include 'c.sas';", file.path(dir, "b.sas"))
  writeLines("%macro deep; %mend;", file.path(dir, "c.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_identical(sum(p$files$origin == "included"), 2L)
})

test_that("include cycles are flagged, not infinite", {
  dir <- withr::local_tempdir()
  writeLines("%include 'b.sas';", file.path(dir, "a.sas"))
  writeLines("%include 'a.sas';", file.path(dir, "b.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_true("include_cycle" %in% p$flags$kind)
})

test_that("missing target keeps unresolved_include; config include_roots searched", {
  dir <- withr::local_tempdir()
  writeLines("%include 'nowhere.sas';", file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_true("unresolved_include" %in% p$flags$kind)
  shared <- withr::local_tempdir()
  writeLines("%macro fromroot; %mend;", file.path(shared, "found.sas"))
  writeLines(sprintf("includes:\n  roots:\n    - %s", shared),
             file.path(dir, "_sas2r.yml"))
  writeLines("%include 'found.sas';", file.path(dir, "b.sas"))
  p2 <- sas_project(file.path(dir, "b.sas"))
  expect_false("unresolved_include" %in% p2$flags$kind)
})
