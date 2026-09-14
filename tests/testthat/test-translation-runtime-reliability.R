test_that("source macro headers preserve defaults and executable argument behavior", {
  contract <- parse_macro_contract("summarize", "input, blank=, n=-2.5, title='A ''quote'''")
  text <- render_macro_interface(contract)
  header <- strsplit(text, "\n", fixed = TRUE)[[1L]][1L]
  env <- new.env()
  eval(parse(text = paste(header, "list(input, blank, n, title)")), env)
  expect_identical(env$summarize(), list("", "", -2.5, "A 'quote'"))
  expect_identical(env$summarize("raw", n = 4), list("raw", "", 4, "A 'quote'"))
  expect_true(validate_macro_contract(paste(header, "NULL"), contract)$pass)
  expect_false(validate_macro_contract(
    "summarize <- function(input=NULL, blank=NULL, n=-2.5, title=\"A 'quote'\") NULL", contract)$pass)
  dynamic <- render_macro_interface(parse_macro_contract("later", "ds=&source"))
  expect_match(dynamic, "Unresolved defaults")
  expect_match(dynamic, "&source", fixed = TRUE)
  expect_false(grepl('ds = ""', dynamic, fixed = TRUE))
})

test_that("all roles and callers receive the same source macro interface", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, "programs")); dir.create(file.path(root, "macros"))
  writeLines(c("macros:", "  search_path: [macros]"), file.path(root, "_sas2r.yml"))
  writeLines("%macro summarize(input, blank=, n=2); %put &input; %mend;",
             file.path(root, "macros", "summarize.sas"))
  writeLines("%summarize(raw);", file.path(root, "programs", "report.sas"))
  p <- sas_project(file.path(root, "programs"))
  cid <- "macro__summarize"
  contract <- component_macro_contract(p, p$graph, cid)
  header <- strsplit(render_macro_interface(contract), "\n", fixed = TRUE)[[1L]][1L]
  context <- build_translator_context(cid, p, graph = p$graph)
  expect_match(context$context_packet, header, fixed = TRUE)
  behavioral <- build_behavioral_contract(cid, p, graph = p$graph,
    r_code_text = 'summarize <- function(input="", blank="", n=2) input')
  expect_identical(behavioral$macro_contract, contract)
  expect_match(render_dependency_interfaces(p, "report"), header, fixed = TRUE)
  caller <- build_translator_context("report", p, graph = p$graph,
    resolved_contracts = setNames(list(behavioral), cid))
  expect_match(caller$context_packet, header, fixed = TRUE)
  revision <- list(component_id = cid, revision_id = "r1", contract = behavioral,
    r_code = 'summarize <- function(input="", blank="", n=2) input')
  llm <- recording_reviewer(function(request) {
    expect_match(request$messages[[1L]]$content, header, fixed = TRUE)
    valid_program_review_response()
  })
  review_program_revision(revision, list(project = p), llm = llm,
                          paths = init_migration_paths(withr::local_tempdir()))
  fixer <- recording_fixer(function(request) {
    expect_match(request$messages[[1L]]$content, header, fixed = TRUE)
    valid_program_fix_response(code = revision$r_code)
  })
  fix_program_revision(revision, project = p, llm = fixer,
    checks = list(check_id = "bad_default", errors = "default mismatch"),
    paths = init_migration_paths(withr::local_tempdir()))
})

test_that("statistical guidance routes by source and illustrates an actual divergence", {
  sas <- "proc template; define statgraph distribution; boxplot y=value x=group; end; run;"
  for (role in c("translator", "reviewer", "fixer")) {
    routed <- route_agent_skills(list(agent = role, flags = skill_flags_from_sas(sas)))
    expect_true("sas-statistical-defaults" %in% vapply(routed, `[[`, "", "skill_id"))
    unrelated <- route_agent_skills(list(agent = role,
      flags = skill_flags_from_sas("data a; set b; x=y+1; run;")))
    expect_false("sas-statistical-defaults" %in% vapply(unrelated, `[[`, "", "skill_id"))
  }
  # Independently fixed expected order statistics for a seven-value example.
  x <- c(0, 1, 2, 3, 4, 5, 20)
  expect_equal(unname(stats::quantile(x, c(.25, .75), type = 2)), c(1, 5))
  expect_equal(grDevices::boxplot.stats(x)$stats[c(2, 4)], c(1.5, 4.5))
})

test_that("agent context preserves the offline selected library and fallback reason", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, "programs")); dir.create(file.path(root, "input"))
  saveRDS(data.frame(value = 1), file.path(root, "input", "raw.rds"))
  writeLines(c("libraries:", "  source:", "    path: input", "    engine: rds"),
             file.path(root, "_sas2r.yml"))
  writeLines(c('libname source "unavailable";',
               "data work.result; set source.raw; run;"),
             file.path(root, "programs", "report.sas"))
  p <- sas_project(file.path(root, "programs"))
  bindings <- effective_librefs(p)$bindings
  selected <- bindings[bindings$libref == "source", , drop = FALSE]
  expect_true(all(selected$selected_path == file.path(root, "input")))
  expect_true(all(selected$selection_origin == "configured_fallback"))
  context <- render_component_libraries(p, "report")
  expect_match(context, paste0("selected_path=", file.path(root, "input")), fixed = TRUE)
  expect_match(context, paste0("reason=", selected$fallback_reason[1L]), fixed = TRUE)
  expect_match(context, p$libref_registry$project_root, fixed = TRUE)
  packet <- build_translator_context("report", p, graph = p$graph)$context_packet
  expect_match(packet, context, fixed = TRUE)
})

test_that("relative assignments retain generated data and real reassignment across layouts", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, "input")); dir.create(file.path(root, "other"))
  saveRDS(data.frame(value = 1), file.path(root, "input", "raw.rds"))
  saveRDS(data.frame(value = 20), file.path(root, "other", "raw.rds"))
  original <- cli::hash_file_sha256(file.path(root, "input", "raw.rds"))
  for (layout in c("smoke", "bundle", "export")) {
    attempt <- normalizePath(withr::local_tempdir(), winslash = "/")
    bundle <- if (layout == "bundle") file.path(attempt, "bundle") else attempt
    dir.create(bundle, showWarnings = FALSE)
    write_helpers(bundle)
    libraries <- list(
      study = list(read_path = file.path(root, "input"),
        write_path = file.path(attempt, "study"), engine = "rds", write = "rds"),
      work = list(read_path = file.path(attempt, "work"),
        write_path = file.path(attempt, "work"), engine = "rds", write = "rds"))
    write_autoexec(list(project_dir = root), bundle, library_map = libraries, output_root = attempt)
    env <- new.env(parent = globalenv())
    sys.source(file.path(bundle, "autoexec.R"), env, chdir = TRUE)
    withr::with_dir(tempdir(), {
      env$lib_write(data.frame(value = 2), "study", "derived")
      env$sas2r_libname_assign("study", "input", engine = "rds")
      expect_equal(env$lib_read("study", "derived")$value, 2)
      expect_equal(env$lib_read("study", "raw")$value, 1)
      env$sas2r_libname_assign("study", "other", engine = "rds")
      expect_equal(env$lib_read("study", "raw")$value, 20)
      env$lib_write(data.frame(value = 21), "study", "raw")
      expect_equal(env$lib_read("study", "raw")$value, 21)
      expect_equal(readRDS(file.path(root, "other", "raw.rds"))$value, 20)
      env$sas2r_libname_clear("study")
      env$sas2r_libname_assign("renamed", file.path(root, "input"), engine = "rds")
      expect_equal(env$lib_read("renamed", "derived")$value, 2)
      err <- tryCatch(env$lib_read("renamed", "absent"), error = identity)
      expect_match(conditionMessage(err), file.path(root, "input", "absent.rds"), fixed = TRUE)
      expect_match(conditionMessage(err), file.path(attempt, "study", "absent.xpt"), fixed = TRUE)
      expect_match(conditionMessage(err), paste0("Execution root: ", root), fixed = TRUE)
      expect_false(file.exists(file.path(root, "input", "derived.rds")))
    })
    expect_identical(cli::hash_file_sha256(file.path(root, "input", "raw.rds")), original)
    expect_true(any(grepl("libraries/", names(attempt_output_hashes(attempt)), fixed = TRUE)))
  }
})

test_that("run environment records actual loaded versions and effective overrides", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, ".sas2r", "agents"), recursive = TRUE)
  writeLines(c("tool_call_limit: 42", "tools:", "  search_skills: {max_calls: 3}"),
             file.path(root, ".sas2r", "agents", "reviewer.yml"))
  writeLines("data x; a=1; run;", file.path(root, "p.sas"))
  p <- sas_project(root)
  state <- new_migration_state(p, withr::local_tempdir(), max_bundle_repair_rounds = 7)
  info <- migration_environment(state)
  expect_identical(info$versions$sas2r, as.character(getNamespaceVersion("sas2r")))
  expect_identical(info$versions$R, as.character(getRversion()))
  expect_identical(info$agents$translator$tool_call_limit, 30L)
  expect_identical(info$agents$reviewer$tool_call_limit, 42L)
  expect_identical(info$agents$reviewer$tool_overrides$search_skills, 3L)
  expect_equal(info$repairs$bundle_overall_cap, 7)
})

test_that("mechanically invalid revisions go to repair before independent review", {
  root <- withr::local_tempdir()
  writeLines("data work.out; x=1; run;", file.path(root, "calc.sas"))
  state <- new_migration_state(sas_project(root), withr::local_tempdir())
  code <- "out <- ("
  binding <- new_component_binding(migration_hash("source"), migration_hash(code),
    migration_hash("helper"), migration_hash("prompt"), migration_hash("closure"))
  contract <- new_behavioral_contract("calc", writes = "work.out", binding = binding)
  path <- file.path(state$paths$programs, "bad.R")
  writeLines(code, path)
  state$selected_revisions$calc <- list(component_id = "calc", revision_id = "r1",
    r_code = code, r_path = path, contract = contract, binding = binding)
  state$histories$calc <- new_component_evidence_history("calc", binding)
  sequence <- character()
  state$reviewer_llm <- recording_reviewer(function(request) {
    sequence <<- c(sequence, "review")
    expect_false(grepl("out <- (", request$messages[[1]]$content, fixed = TRUE))
    valid_program_review_response()
  })
  state$fixer_llm <- recording_fixer(function(request) {
    sequence <<- c(sequence, "fix")
    expect_match(request$messages[[1]]$content, "parse", ignore.case = TRUE)
    valid_program_fix_response(code = "lib_write(data.frame(x=1), 'work', 'out')")
  })
  result <- process_program_component(state, "calc", execute = TRUE, max_program_repair_rounds = 1L)
  expect_identical(sequence, c("fix", "review"))
  expect_true(result$selected_revisions$calc$checks$pass)
  expect_true(result$selected_revisions$calc$smoke$passed)
  expect_equal(result$repair_counts$calc, 1L)
})
