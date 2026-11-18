test_that("report paths use the run output root and retain meaningful subfolders", {
  root <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$.sas2r_output_root <- root
  path <- sas_output_path("outputs/tables/report.pdf", env)
  expect_identical(path, file.path(root, "tlf", "outputs", "tables", "report.pdf"))
  expect_true(dir.exists(dirname(path)))
  expect_identical(sas_output_path("outputs/tables/../table.pdf", env),
    file.path(root, "tlf", "outputs", "table.pdf"))
  expect_error(sas_output_path("../table.pdf", env), "escapes")
  expect_identical(sas_output_path("figures/report.pdf", env),
    file.path(root, "tlf", "figures", "report.pdf"))
  expect_error(sas_output_path("report.pdf", new.env(parent = emptyenv())), "output root")
})

test_that("TLF lookup respects full logical paths instead of matching a basename", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "tlf", "outputs", "tables"), recursive = TRUE)
  dir.create(file.path(root, "tlf", "figures"), recursive = TRUE)
  writeLines("table", file.path(root, "tlf", "outputs", "tables", "same.pdf"))
  writeLines("figure", file.path(root, "tlf", "figures", "same.pdf"))
  contract <- list(kind = "tlf", target_key = "outputs/tables/same.pdf",
    logical_name = "outputs/tables/same.pdf")
  expect_identical(find_attempt_candidate_file(contract, list(attempt_dir = root)),
    normalizePath(file.path(root, "tlf", "outputs", "tables", "same.pdf"), winslash = "/"))
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
  expect_identical(readLines(file.path(root, "output", "tlf", "outputs", "table.html")), "report")
  expect_false(dir.exists(file.path(other, "outputs")))
})


test_that("TLF contracts with only a target key retain exact-path lookup", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, "tlf", "outputs"), recursive = TRUE)
  writeLines("report", file.path(root, "tlf", "outputs", "table.html"))
  contract <- list(kind = "tlf", target_key = "outputs/table.html")
  expect_identical(find_attempt_candidate_file(contract, list(attempt_dir = root)),
    file.path(root, "tlf", "outputs", "table.html"))
})

test_that("distinct SAS report destinations survive assessment and materialization", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  reports <- c("table.html", "outputs/table.html")
  writeLines(sprintf("ods html file='%s'; proc print; run; ods html close;", reports),
    file.path(root, "report.sas"))
  contracts <- infer_output_contracts(sas_project(root))
  contracts <- contracts[contracts$kind == "tlf", ]
  expect_setequal(contracts$logical_name, reports)
  attempt <- list(attempt_dir = root, outputs_dir = file.path(root, "outputs"),
    completed = TRUE, passed = TRUE)
  dir.create(attempt$outputs_dir)
  runtime <- new.env(parent = baseenv())
  runtime$.sas2r_output_root <- root
  for (report in reports) {
    path <- sas_output_path(report, runtime)
    writeLines(paste0("<html><body>", report, "</body></html>"), path)
  }
  targets <- assess_final_outputs(contracts, attempt)$targets
  expect_true(all(vapply(targets, `[[`, logical(1), "passed")))
  paths <- vapply(targets[reports], `[[`, "", "candidate_path")
  expect_equal(length(unique(paths)), 2L)
  files <- substring(paths, nchar(root) + 2L)
  attempt$output_hashes <- as.list(stats::setNames(vapply(paths, cli::hash_file_sha256, ""), files))
  saved <- withr::local_tempdir()
  outputs <- materialize_run_outputs(list(selected_attempt = attempt, execute = TRUE,
    output_contracts = contracts, paths = list(outputs = saved)))
  for (report in reports) {
    expect_identical(outputs[[report]]$attempt_path, file.path("tlf", report))
    expect_identical(readLines(file.path(saved, "tlf", report)),
      paste0("<html><body>", report, "</body></html>"))
  }
  unlink(paths[[reports[2L]]])
  expect_identical(assess_final_outputs(contracts, attempt)$targets[[reports[2L]]]$status,
    "missing_candidate")
  # A legacy writer created only its literal prefixed path in the attempt directory.
  unlink(paths[[reports[1L]]])
  writeLines("<html><body>prefixed</body></html>", file.path(root, reports[2L]))
  targets <- assess_final_outputs(contracts, attempt)$targets
  expect_identical(targets[[reports[1L]]]$status, "missing_candidate")
  expect_true(targets[[reports[2L]]]$passed)
})

test_that("nested report writers use their enclosing runtime", {
  root <- withr::local_tempdir()
  runtime <- new.env(parent = globalenv())
  runtime$.sas2r_output_root <- root
  write_helpers(root)
  sys.source(file.path(root, "sas2r-helpers.R"), runtime)
  writer <- evalq(function() {
    nested <- function() sas_output_path("table.pdf", environment())
    nested()
  }, runtime)
  expect_identical(writer(), file.path(root, "tlf", "table.pdf"))
  expect_identical(evalq((function() sas_output_path("table.pdf"))(), runtime), writer())
  expect_identical(sas_output_path("table.pdf", new.env(parent = runtime)), writer())
  error <- callr::r(function(root) {
    runtime <- new.env(parent = globalenv())
    sys.source(file.path(root, "sas2r-helpers.R"), runtime)
    assign(".sas2r_output_root", root, envir = globalenv())
    tryCatch(runtime$sas_output_path("table.pdf", new.env(parent = globalenv())),
             error = conditionMessage)
  }, list(root))
  expect_match(error, "output root")
})

test_that("an older flattened report remains readable for an unambiguous contract", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "tlf"))
  writeLines("<html><body>report</body></html>", file.path(root, "tlf", "report.html"))
  contracts <- merge_output_overrides(empty_output_contracts(), list(tlfs = "outputs/report.html"))
  targets <- assess_final_outputs(contracts, list(attempt_dir = root))$targets
  expect_true(targets[["outputs/report.html"]]$passed)
})

test_that("legacy literal reports are collected once per source destination", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "tlf"))
  reports <- c("table.html", "tlf/table.html")
  for (name in reports) writeLines(paste0("<html><body>", name, "</body></html>"), file.path(root, name))
  contracts <- merge_output_overrides(empty_output_contracts(), list(tlfs = reports))
  attempt <- list(attempt_dir = root)
  targets <- assess_final_outputs(contracts, attempt)$targets
  paths <- vapply(targets[reports], `[[`, "", "candidate_path")
  expect_identical(unname(paths), normalizePath(file.path(root, reports), winslash = "/"))
  unlink(paths[1L])
  targets <- assess_final_outputs(contracts, attempt)$targets
  expect_false(any(vapply(targets, `[[`, logical(1), "passed")))
})

test_that("equivalent report paths share an assessed and saved file", {
  root <- withr::local_tempdir()
  reports <- c("./outputs/t.html", "outputs/t.html")
  for (i in seq_along(reports)) writeLines(
    sprintf("ods html file='%s'; proc print; run; ods html close;", reports[i]),
    file.path(root, paste0(i, ".sas")))
  contracts <- infer_output_contracts(sas_project(root))
  contracts <- contracts[contracts$kind == "tlf", ]
  expect_setequal(contracts$target_key, reports)
  relative <- "tlf/outputs/t.html"
  path <- file.path(root, relative)
  dir.create(dirname(path), recursive = TRUE)
  writeLines("<html><body>report</body></html>", path)
  attempt <- list(attempt_dir = root,
    output_hashes = stats::setNames(list(cli::hash_file_sha256(path)), relative))
  targets <- assess_final_outputs(contracts, attempt)$targets
  for (key in reports) {
    expect_identical(targets[[key]]$status, "passed")
    expect_identical(targets[[key]]$candidate_path, normalizePath(path, winslash = "/"))
  }
  saved <- withr::local_tempdir()
  outputs <- materialize_run_outputs(list(selected_attempt = attempt, execute = TRUE,
    output_contracts = contracts, paths = list(outputs = saved, work = withr::local_tempdir())))
  expect_setequal(names(outputs), reports)
  expect_identical(outputs[[reports[1L]]], outputs[[reports[2L]]])
  expect_identical(outputs[[reports[1L]]]$attempt_path, relative)
  expect_identical(list.files(saved, recursive = TRUE), relative)
})
