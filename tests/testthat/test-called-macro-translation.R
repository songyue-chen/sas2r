called_macro_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "programs"))
  dir.create(file.path(root, "macros"))
  writeLines(c("macros:", "  search_path:", "    - macros"), file.path(root, "_sas2r.yml"))
  writeLines(c("%macro add(value=1);", "%scale(value=&value);", "%mend;"),
             file.path(root, "macros", "add.sas"))
  writeLines(c("%macro unused();", "%never_needed;", "%mend;",
               "%macro scale(value=1);", "%put &value;", "%mend;"),
             file.path(root, "macros", "utilities.sas"))
  writeLines("%macro untouched(); %mend;", file.path(root, "macros", "untouched.sas"))
  writeLines("%add(value=3);", file.path(root, "programs", "first.sas"))
  writeLines("%add(value=4);", file.path(root, "programs", "second.sas"))
  root
}

test_that("configured called macros and their dependencies become selective translation units", {
  root <- called_macro_fixture()
  p <- sas_project(file.path(root, "programs"))
  expect_setequal(basename(p$files$file), c("first.sas", "second.sas", "add.sas", "utilities.sas"))
  expect_setequal(p$macros$defs$name, c("add", "scale"))
  expect_true(all(p$units$unit_type[p$units$origin == "macro_search_path"] == "macro_def"))
  expect_false("never_needed" %in% p$macros$calls$name)
  macros <- called_macro_units(p)
  expect_setequal(macros$staged_file, c("R/macros/add.R", "R/macros/scale.R"))
  g <- p$graph
  edges <- g$edges[g$edges$type == "calls_macro", ]
  expect_true(all(edges$resolution == "resolved"))
  expect_false(any(g$nodes$type[g$nodes$node_id %in% edges$from] == "external_input"))
  order <- p$schedule$component_id
  expect_lt(match("macro__scale", order), match("macro__add", order))
  expect_lt(match("macro__add", order), match("first", order))
  expect_lt(match("macro__add", order), match("second", order))
  expect_setequal(build_bundle_execution_plan(g)$root_programs, c("first", "second"))
  pre <- sas_preflight(file.path(root, "programs"))
  expect_setequal(pre$called_macros$name, c("add", "scale"))
  expect_false(dir.exists(file.path(root, "programs", ".sas2r")))
})

test_that("missing definitions and executable library initializers remain unresolved", {
  root <- called_macro_fixture()
  writeLines("%macro another(); %mend;", file.path(root, "macros", "add.sas"))
  p <- sas_project(file.path(root, "programs"))
  expect_true("macro_definition_missing" %in% p$flags$kind)
  expect_true(all(p$macros$resolution$status == "unresolved"))
  writeLines(c("data work.setup; x=1; run;", "%macro add(value=1); %mend;"),
             file.path(root, "macros", "add.sas"))
  p <- sas_project(file.path(root, "programs"))
  expect_true("macro_library_initialization_unsupported" %in% p$flags$kind)
  expect_equal(nrow(called_macro_units(p)), 0L)
  for (setup in c("%let scale_factor=2;", "options obs=1;")) {
    writeLines(c(setup, "%macro add(value=1); %mend;"),
               file.path(root, "macros", "add.sas"))
    p <- sas_project(file.path(root, "programs"))
    expect_true("macro_library_initialization_unsupported" %in% p$flags$kind)
    expect_true(all(p$macros$resolution$status == "unresolved"))
  }
})

test_that("standalone macro gates reject missing zero-argument functions and top-level execution", {
  contract <- list(parameters = list(), macro_contract = parse_macro_contract("example", ""))
  contract$macro_contract$standalone <- TRUE
  file <- withr::local_tempfile(fileext = ".R")
  writeLines("other <- function() 1", file)
  expect_false(check_program_revision(file, contract)$pass)
  writeLines(c("example <- function() 1", "example()"), file)
  expect_false(check_program_revision(file, contract)$pass)
  writeLines("example <- function() 1", file)
  expect_true(check_program_revision(file, contract)$pass)
})

test_that("dynamic calls are not guessed or promoted", {
  root <- called_macro_fixture()
  file <- file.path(root, "programs", "first.sas")
  writeLines("%&selected(value=3);", file)
  p <- sas_project(file)
  expect_equal(nrow(called_macro_units(p)), 0L)
  expect_true(all(p$macros$resolution$status == "dynamic"))
})
