test_that("warm scans reuse the cache and produce identical projects", {
  dir <- withr::local_tempdir()
  writeLines("data a; set w.b; run;", file.path(dir, "a.sas"))
  p1 <- sas_project(dir, cache = TRUE)
  expect_true(file.exists(file.path(dir, ".sas2r", "scan_cache.rds")))
  p2 <- sas_project(dir, cache = TRUE)
  expect_identical(p1$units, p2$units)
  expect_identical(p1$lineage, p2$lineage)
})

test_that("edited files bypass the stale cache entry", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "a.sas")
  writeLines("data a; run;", f)
  invisible(sas_project(dir, cache = TRUE))
  writeLines("data b; run;", f)
  p <- sas_project(dir, cache = TRUE)
  expect_true("work.b" %in% p$lineage$dataset)
  expect_false("work.a" %in% p$lineage$dataset)
})

test_that("empty files in project handle fast_bind correctly with and without cache", {
  dir <- withr::local_tempdir()
  writeLines("", file.path(dir, "empty1.sas"))
  writeLines("", file.path(dir, "empty2.sas"))
  p1 <- sas_project(dir, cache = FALSE)
  expect_equal(nrow(p1$statements), 0L)
  expect_equal(nrow(p1$units), 0L)

  p2 <- sas_project(dir, cache = TRUE)
  expect_equal(nrow(p2$statements), 0L)
  expect_equal(nrow(p2$units), 0L)

  p3 <- sas_project(dir, cache = TRUE)
  expect_identical(p2$statements, p3$statements)
  expect_identical(p2$units, p3$units)
})

test_that("warm scan genuinely reads from cache file", {
  dir <- withr::local_tempdir()
  writeLines("data a; x = 1; run;", file.path(dir, "a.sas"))
  p1 <- sas_project(dir, cache = TRUE)
  cfile <- file.path(dir, ".sas2r", "scan_cache.rds")
  expect_true(file.exists(cfile))

  # Mutate cached label to verify the warm scan hit path actually reads the cache
  cdata <- readRDS(cfile)
  key <- names(cdata)[1]
  cdata[[key]]$unit_rows$label <- "injected_cache_hit_label"
  saveRDS(cdata, cfile)

  p2 <- sas_project(dir, cache = TRUE)
  expect_identical(p2$units$label[1], "injected_cache_hit_label")
})

test_that("cached scans reproduce identical include graphs", {
  dir <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(dir, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "%include 'inc.sas';"),
             file.path(dir, "driver.sas"))
  driver <- file.path(dir, "driver.sas")

  p1 <- sas_project(driver, cache = TRUE)
  p2 <- sas_project(driver, cache = TRUE)

  expect_identical(nrow(p1$include_graph$occurrences), 2L)
  expect_true(all(p2$include_graph$occurrences$status == "resolved"))
  expect_identical(p1$include_graph$occurrences, p2$include_graph$occurrences)
  expect_identical(p1$include_graph$files, p2$include_graph$files)
  expect_identical(p1$includes, p2$includes)
})
