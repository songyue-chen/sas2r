test_that("report paths use the run output root and retain meaningful subfolders", {
  root <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$.sas2r_output_root <- root
  path <- sas_output_path("outputs/tables/report.pdf", env)
  expect_identical(path, file.path(root, "tlf", "tables", "report.pdf"))
  expect_true(dir.exists(dirname(path)))
  expect_identical(sas_output_path("outputs/tables/../table.pdf", env),
    file.path(root, "tlf", "table.pdf"))
  expect_error(sas_output_path("../table.pdf", env), "escapes")
  expect_identical(sas_output_path("figures/report.pdf", env),
    file.path(root, "tlf", "figures", "report.pdf"))
  expect_error(sas_output_path("report.pdf", new.env(parent = emptyenv())), "output root")
})

test_that("TLF lookup respects full logical paths instead of matching a basename", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "tlf", "tables"), recursive = TRUE)
  dir.create(file.path(root, "tlf", "figures"), recursive = TRUE)
  writeLines("table", file.path(root, "tlf", "tables", "same.pdf"))
  writeLines("figure", file.path(root, "tlf", "figures", "same.pdf"))
  contract <- list(kind = "tlf", target_key = "outputs/tables/same.pdf",
    logical_name = "outputs/tables/same.pdf")
  expect_identical(find_attempt_candidate_file(contract, list(attempt_dir = root)),
    normalizePath(file.path(root, "tlf", "tables", "same.pdf"), winslash = "/"))
  contract$target_key <- contract$logical_name <- "outputs/missing/same.pdf"
  expect_true(is.na(find_attempt_candidate_file(contract, list(attempt_dir = root))))
})

test_that("the vendored runtime routes reports identically from an exported bundle", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  other <- withr::local_tempdir()
  write_helpers(root)
  runtime <- new.env(parent = baseenv())
  runtime$.sas2r_output_root <- file.path(root, "output")
  sys.source(file.path(root, "sas2r-helpers.R"), runtime)
  withr::local_dir(other)
  evalq(writeLines("report", sas_output_path("outputs/table.html", environment())), runtime)
  expect_identical(readLines(file.path(root, "output", "tlf", "table.html")), "report")
  expect_false(dir.exists(file.path(other, "outputs")))
})


test_that("TLF contracts with only a target key retain exact-path lookup", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, "tlf"))
  writeLines("report", file.path(root, "tlf", "table.html"))
  contract <- list(kind = "tlf", target_key = "outputs/table.html")
  expect_identical(find_attempt_candidate_file(contract, list(attempt_dir = root)),
    file.path(root, "tlf", "table.html"))
})
