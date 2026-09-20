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

test_that("local definitions and configured directory precedence remain authoritative", {
  root <- called_macro_fixture()
  second <- file.path(root, "other")
  dir.create(second)
  writeLines("%macro add(value=2); %mend;", file.path(second, "ADD.SAS"))
  cfg <- list(macro_search_path = c(second, file.path(root, "macros")))
  p <- sas_project(file.path(root, "programs", "first.sas"), config = cfg)
  expect_identical(p$macros$defs$file, include_normalize_path(file.path(second, "ADD.SAS")))
  expect_true("macro_shadowing" %in% p$flags$kind)
  writeLines(c("%macro add(value=7); %mend;", "%add(value=3);"),
             file.path(root, "programs", "first.sas"))
  p <- sas_project(file.path(root, "programs", "first.sas"), config = cfg)
  expect_equal(nrow(p$files), 1L)
  expect_equal(nrow(called_macro_units(p)), 0L)
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
    expect_error(require_resolved_macros(p), "macro_library_initialization_unsupported",
                 class = "sas2r_macro_dependency_error")
  }
})

test_that("called macros generate reusable files and execute through the public bundle workflow", {
  root <- called_macro_fixture()
  writeLines(c("%macro unused(); %never_needed; %mend;",
               '%macro scale(value=1) / des="Scale (shared)"; %eval(&value*2); %mend;'),
             file.path(root, "macros", "utilities.sas"))
  writeLines("data work.first; x=%add(value=3); run;", file.path(root, "programs", "first.sas"))
  writeLines("data work.second; x=%add(value=4); run;", file.path(root, "programs", "second.sas"))
  translated <- character()
  reviewed <- character()
  llm <- new_llm(function(request, audit_context = list()) {
    id <- audit_context$component_id
    if (id == "macro__scale") {
      expect_false(grepl("never_needed", request$messages[[1L]]$content, fixed = TRUE))
    }
    response <- if (audit_context$role == "reviewer") {
      reviewed <<- c(reviewed, id)
      valid_program_review_response()
    } else {
      translated <<- c(translated, id)
      code <- switch(id,
        macro__scale = "scale <- function(value = 1) value * 2",
        macro__add = "add <- function(value = 1) scale(value = value)",
        first = "lib_write(data.frame(x = add(value = 3)), 'work', 'first')",
        second = "lib_write(data.frame(x = add(value = 4)), 'work', 'second')",
        stop(paste("Unexpected translation:", id)))
      if (id %in% c("first", "second")) {
        expect_match(request$messages[[1L]]$content, "call add; loaded by autoexec.R", fixed = TRUE)
      }
      valid_program_translation_response(code = code,
        helper_use = if (id == "macro__add") "scale" else character())
    }
    normalize_provider_response(response, request, provider = "mock")
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native", tool_calling = "native", tools_with_structured_output = "supported"))
  out <- withr::local_tempdir()
  result <- sas_translate(file.path(root, "programs"), out_dir = out, llm = llm,
                          max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  expect_setequal(unique(translated), c("macro__scale", "macro__add", "first", "second"))
  expect_equal(sum(translated == "macro__scale"), 1L)
  expect_equal(sum(translated == "macro__add"), 1L)
  expect_true(all(table(reviewed) == 1L))
  expect_true(file.exists(file.path(result$bundle_dir, "macros/add.R")))
  expect_true(file.exists(file.path(result$bundle_dir, "macros/scale.R")))
  expect_true(file.exists(file.path(result$bundle_dir, "tests_macros/test-add.R")))
  expect_false(file.exists(file.path(result$bundle_dir, "macros/unused.R")))
  expect_false(identical(result$status, "blocked"))
  exported <- withr::local_tempdir()
  suppressWarnings(sas_write(result, exported))
  values <- callr::r(function(bundle) {
    setwd(bundle)
    source("autoexec.R")
    source("programs/first.R")
    source("programs/second.R")
    c(lib_read("work", "first")$x, lib_read("work", "second")$x)
  }, args = list(bundle = exported))
  expect_identical(values, c(6, 8))
  tests <- testthat::test_dir(file.path(exported, "tests_macros"), reporter = "silent", stop_on_failure = FALSE)
  expect_equal(sum(as.data.frame(tests)$failed), 0L)
  expect_match(readLines(file.path(exported, "macros/add.R"))[1L], "llm_authored", fixed = TRUE)
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

test_that("several called definitions in one library file stay separate in staging and agent context", {
  root <- called_macro_fixture()
  file <- file.path(root, "macros", "utilities.sas")
  writeLines(c("%macro scale(value=1); %eval(&value*2); %mend;",
               "%macro shift(value=0); %eval(&value+1); %mend;",
               "%macro unused(); %never_needed; %mend;"), file)
  writeLines("%shift(value=4);", file.path(root, "programs", "second.sas"))
  p <- sas_project(file.path(root, "programs"), cache = TRUE)
  again <- sas_project(file.path(root, "programs"), cache = TRUE)
  expect_identical(p$units, again$units)
  expect_setequal(called_macro_units(p)$name, c("add", "scale", "shift"))
  out <- withr::local_tempdir()
  baseline <- sas_transpile(p, out)
  for (name in c("scale", "shift")) {
    id <- paste0("macro__", name)
    context <- build_translator_context(id, p, baseline, p$graph, p$schedule)
    expect_match(context$sas_text, paste0("%macro ", name), fixed = TRUE)
    expect_false(grepl("unused", context$sas_text, fixed = TRUE))
    expect_false(grepl(paste0("%macro ", setdiff(c("scale", "shift"), name)), context$sas_text, fixed = TRUE))
    expect_identical(component_source_text(p$graph, id), context$sas_text)
    rows <- baseline$manifest[baseline$manifest$unit_id %in% context$comp_stmts$unit_id, ]
    expect_identical(unique(rows$staged_file), paste0("R/macros/", name, ".R"))
  }
  writeLines("data work.second; x=4; run;", file.path(root, "programs", "second.sas"))
  changed <- sas_project(file.path(root, "programs"), cache = TRUE)
  expect_setequal(called_macro_units(changed)$name, c("add", "scale"))
})

test_that("recursive call cycles terminate discovery and remain visible", {
  root <- called_macro_fixture()
  writeLines("%macro scale(value=1); %add(value=&value); %mend;",
             file.path(root, "macros", "utilities.sas"))
  p <- sas_project(file.path(root, "programs"))
  expect_setequal(called_macro_units(p)$name, c("add", "scale"))
  expect_true("dependency_cycle" %in% p$flags$kind)
  expect_true(any(p$schedule$group_kind == "cycle"))
})

test_that("dynamic calls are not guessed or promoted", {
  root <- called_macro_fixture()
  file <- file.path(root, "programs", "first.sas")
  writeLines("%&selected(value=3);", file)
  p <- sas_project(file)
  expect_equal(nrow(called_macro_units(p)), 0L)
  expect_true(all(p$macros$resolution$status == "dynamic"))
})

test_that("called macro components cannot absorb a similarly named program", {
  root <- called_macro_fixture()
  writeLines("data work.x; x=1; run;", file.path(root, "programs", "macro__add.sas"))
  expect_error(sas_project(file.path(root, "programs")), class = "sas2r_component_id_collision")
})

test_that("uncalled macro includes do not activate files and called includes defer explicitly", {
  root <- called_macro_fixture()
  file <- file.path(root, "macros", "utilities.sas")
  writeLines(c("%macro scale(value=1); %eval(&value*2); %mend;",
               "%macro unused(); %include 'missing.sas'; %mend;"), file)
  p <- sas_project(file.path(root, "programs"))
  expect_setequal(called_macro_units(p)$name, c("add", "scale"))
  expect_equal(nrow(p$includes), 0L)
  expect_false("unresolved_include" %in% p$flags$kind)
  writeLines("%macro scale(value=1); %include 'missing.sas'; %mend;", file)
  p <- sas_project(file.path(root, "programs"))
  expect_true("macro_include_requires_expansion" %in% p$flags$kind)
  expect_equal(nrow(called_macro_units(p)), 1L)
  expect_identical(p$macros$resolution$status[p$macros$resolution$name == "scale"], "unresolved")
})

test_that("a macro without an AI translation is emitted as deferred with failing interface checks", {
  root <- called_macro_fixture()
  result <- sas_translate(file.path(root, "programs", "first.sas"),
    out_dir = withr::local_tempdir(), execute = FALSE,
    max_program_repair_rounds = 0L, max_bundle_repair_rounds = 0L)
  expect_identical(result$status, "blocked")
  header <- readLines(file.path(result$bundle_dir, "macros/add.R"))[1L]
  expect_match(header, "macro_deferred", fixed = TRUE)
  expect_false(grepl("llm_authored", header, fixed = TRUE))
})

test_that("an autocall file symlink resolves to its scanned definition", {
  root <- called_macro_fixture()
  external <- file.path(root, "shared_add.sas")
  writeLines("%macro add(value=1); %eval(&value+1); %mend;", external)
  link <- file.path(root, "macros", "add.sas")
  unlink(link)
  skip_if_not(file.symlink(external, link), "symlinks unavailable")
  p <- sas_project(file.path(root, "programs"))
  edges <- p$graph$edges[p$graph$edges$type == "calls_macro", ]
  providers <- p$graph$nodes[p$graph$nodes$node_id %in% edges$from, ]
  expect_identical(unique(providers$type), "macro")
  expect_identical(called_macro_units(p)$file, include_normalize_path(external))
})

test_that("nested macro definitions are deferred instead of sharing a translation unit", {
  root <- called_macro_fixture()
  writeLines(c("%macro add(value=1);", "%macro inner(); %put nested; %mend;",
               "%inner;", "%mend;"), file.path(root, "macros", "add.sas"))
  p <- sas_preflight(file.path(root, "programs"))
  expect_true("macro_nested_definition_unsupported" %in% p$findings$kind)
  expect_equal(nrow(p$called_macros), 0L)
  expect_identical(p$status, "needs_attention")
})

test_that("unavailable macros produce drafts and actionable warnings", {
  root <- called_macro_fixture()
  file <- file.path(root, "programs", "first.sas")
  for (source in c("%not_in_library;", "%&name;")) {
    writeLines(source, file)
    result <- sas_translate(file, out_dir = file.path(root, "out"), execute = FALSE)
    expect_identical(result$status, "needs_review")
    expect_true(length(result$diagnostics$readiness$warnings) > 0L)
    expect_match(paste(readiness_warning_lines(result$diagnostics$readiness), collapse = " "),
      "macros.search_path", fixed = TRUE)
    expect_true(length(result$component_evidence) > 0L)
  }
  writeLines("%add(value=3);", file)
  writeLines("%macro scale(value=1); %missing_nested; %mend;",
             file.path(root, "macros", "utilities.sas"))
  result <- sas_translate(file, out_dir = file.path(root, "out"), execute = FALSE)
  expect_match(paste(readiness_warning_lines(result$diagnostics$readiness), collapse = " "),
    "missing_nested", fixed = TRUE)
  expect_length(list.files(file.path(root, "out"), pattern = "START_HERE.html", recursive = TRUE), 3L)
})

test_that("local macro definitions and SAS builtins need no search path", {
  root <- withr::local_tempdir()
  file <- file.path(root, "program.sas")
  writeLines(c("%macro local_helper(value=1); %put &value; %mend;",
               "%local_helper(value=3);", "%put %sysfunc(today());",
               "%put %qsysfunc(pathname(work));",
               "%put %sysmacexec(local_helper) %sysmacexist(local_helper);",
               "%put %sysmexecdepth %sysmexecname(0);"), file)
  project <- sas_project(file)
  expect_identical(project$macros$resolution$name, "local_helper")
  expect_identical(project$macros$resolution$status, "resolved_project")
  expect_no_error(require_resolved_macros(project))
})
