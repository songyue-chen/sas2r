test_that("reuse compares effective scan settings and survives a working directory change", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "programs")); dir.create(file.path(root, "programs", "macros"))
  writeLines(c("options sasautos=('macros');", "data adam.out; set raw.dm; run;"),
    file.path(root, "programs", "main.sas"))
  withr::local_dir(root)
  cfg <- list(libraries = list(raw = "missing/../raw", adam = "."))
  check <- sas_preflight("programs/", config = cfg)
  dir.create(file.path(root, "programs", "missing"))
  dir.create(file.path(root, "programs", "raw"))
  saveRDS(data.frame(x = 1), file.path(root, "programs", "raw", "dm.rds"))
  withr::with_dir(tempdir(), {
    for (update in list(list(), list(comparison_rules = list(min_rows = 1)), check$project$config)) {
      again <- sas_preflight(check$project, config = update)
      expect_identical(again$inputs$status, "available")
      expect_true(all(file.exists(again$project$files$file)))
      expect_match(component_source_text(again$project$graph, "main"), "set raw.dm", fixed = TRUE)
    }
    reordered <- check$project$config$libraries[c("adam", "raw")]
    expect_no_error(sas_preflight(check$project, config = list(libraries = reordered)))
    expect_error(sas_preflight(check$project, config = list(macro_search_path = "different")),
      class = "sas2r_config_error")
  })
  absolute <- sas_preflight(file.path(root, "programs"), config = cfg)
  spelling <- sas_preflight("./programs/", config = cfg)
  expect_identical(spelling$project$graph, absolute$project$graph)
})

test_that("explicit NULL output reset clears old references and assertions in both entry points", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  check <- sas_preflight(file, outputs = list(references = list("work.out" = "old.rds"),
    assertions = list("work.out" = list(row_count = 5))))
  again <- sas_preflight(check$project, config = list(outputs = NULL))
  expect_true(all(is.na(again$outputs$reference_path)))
  expect_identical(nrow(again$outputs), 0L) # WORK target existed only through the override.
  result <- sas_translate(check$project, config = list(outputs = NULL), execute = FALSE)
  expect_identical(result$project$output_contracts, again$outputs)
})

test_that("collision component ids depend on project layout and not its location", {
  roots <- c(withr::local_tempdir(), withr::local_tempdir())
  ids <- lapply(roots, function(root) {
    for (dir in c("prod", "qc")) {
      dir.create(file.path(root, dir))
      writeLines("data out; x=1; run;", file.path(root, dir, "x.sas"))
    }
    sort(sas_preflight(root, recursive = TRUE)$schedule$component_id)
  })
  expect_length(ids[[1]], 2L)
  expect_identical(ids[[1]], ids[[2]])
})

test_that("configuration typos never bind similarly named fields and output paths are scalar", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  for (key in c("llm_backup", "outputs_old", "libraries_x", "comparsion_rules")) {
    expect_error(sas_preflight(file, config = stats::setNames(list(list()), key)),
      class = "sas2r_config_error")
  }
  for (path in list("", NA_character_, c("a", "b"), 1)) {
    expect_error(sas_preflight(file, out_dir = path), class = "sas2r_invalid_argument")
    expect_error(sas_translate(file, out_dir = path), class = "sas2r_invalid_argument")
  }
  check <- sas_preflight(file, out_dir = paste0(root, "/new/"), config = list())
  expect_false(grepl("new//", check$destinations$state, fixed = TRUE))
  expect_no_error(print(check$project$config))
  expect_false(dir.exists(file.path(root, ".sas2r")))
})

test_that("preflight and invalid QC scanning do not write a macro index", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  writeLines("%macro util; %put ok; %mend;", file.path(root, "macros", "lib.sas"))
  file <- review_source(root, "options sasautos=('macros'); %util; data out; x=1; run;")
  check <- sas_preflight(file)
  expect_identical(check$project$macros$resolution$status, "resolved_content")
  expect_false(dir.exists(file.path(root, ".sas2r")))
  for (entry in list(sas_preflight, sas_translate)) {
    expect_error(entry(file, config = list(comparison_rules = list(unique_keys = TRUE)), outputs = "work.out"),
      "requires keys", class = "sas2r_output_contract_error")
    expect_false(dir.exists(file.path(root, ".sas2r")))
  }
})

test_that("configuration refuses multiple YAML documents instead of ignoring later settings", {
  path <- withr::local_tempfile(fileext = ".yml")
  for (lines in list(c("libraries: {}", "---", "comparison_rules: {min_rows: 1}"),
    c("---", "libraries: {}", "---", "comparison_rules: {min_rows: 1}"))) {
    writeLines(lines, path)
    expect_error(sas_config(path), "one YAML document", class = "sas2r_config_error")
  }
  writeLines(c("---", "comparison_rules: {min_rows: 1}"), path)
  expect_equal(sas_config(path)$comparison_rules$min_rows, 1)
})

test_that("scan cache drops entries superseded by source edits", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  sas_project(file, cache = TRUE)
  path <- file.path(project_cache_dir(root), "scan_cache.rds")
  first <- names(readRDS(path))
  writeLines("data out; x=2; run;", file)
  sas_project(file, cache = TRUE)
  updated <- names(readRDS(path))
  expect_length(updated, 1L)
  expect_length(intersect(first, updated), 0L)
})
