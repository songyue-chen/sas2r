test_that("relative list references survive preflight reuse and translation", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "programs")); dir.create(file.path(root, "programs", "refs"))
  file <- file.path(root, "programs", "main.sas")
  writeLines("data adam.out; x=1; run;", file)
  ref <- file.path(root, "programs", "refs", "out.rds")
  saveRDS(data.frame(x = 1), ref)
  withr::local_dir(root)
  cfg <- list(libraries = list(adam = "."), outputs = list(references = list("adam.out" = "refs/out.rds")))
  first <- sas_preflight("programs/main.sas", config = cfg)
  again <- sas_preflight(first$project)
  expect_identical(first$references, again$references)
  expect_identical(first$references$status, "available")
  expect_identical(first$outputs$reference_path, include_normalize_path(ref))
  testthat::local_mocked_bindings(build_dependency_graph = function(...) stop("must reuse planned graph"))
  result <- sas_translate(again$project, out_dir = file.path(root, "out"), execute = FALSE,
    usage_limits = list(max_calls = 0))
  contracts <- jsonlite::read_json(result$output_contracts_path)
  expect_identical(contracts[[1L]]$reference_path, include_normalize_path(ref))
})

test_that("explicit config replaces discovery and stale scan settings require a rescan", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data adam.out; set raw.dm; run;")
  writeLines("comparison_rules: {ignore_columns: ID}", file.path(root, "_sas2r.yml"))
  cfg <- list(libraries = list(raw = "old", adam = "."))
  check <- sas_preflight(file, config = cfg)
  expect_identical(check$inputs$status, "missing")
  changed <- cfg; changed$libraries$raw <- "new"
  expect_error(sas_preflight(check$project, config = changed), class = "sas2r_config_error")
  out <- file.path(root, "out")
  expect_error(sas_translate(check$project, config = changed, out_dir = out), class = "sas2r_config_error")
  expect_false(dir.exists(out))
  expect_no_error(sas_preflight(file, config = changed))
  expect_error(sas_translate(file, out_dir = out), class = "sas2r_output_contract_error")
  expect_false(dir.exists(out))
  expect_false(dir.exists(file.path(root, ".sas2r")))
  expect_error(sas_translate(file, config = list(llm = list(provider = "invalid_provider")), out_dir = out),
    class = "sas2r_llm_config_error")
  expect_false(dir.exists(out))
  expect_false(dir.exists(file.path(root, ".sas2r")))
})

test_that("all path entry points reject invalid shapes with an argument error", {
  for (path in list(NULL, character(), c("a.sas", "b.sas"), NA_character_, 1)) {
    expect_error(sas_preflight(path), class = "sas2r_invalid_argument")
    expect_error(sas_translate(path), class = "sas2r_invalid_argument")
    expect_error(sas_project(path), class = "sas2r_invalid_argument")
  }
})

test_that("preflight and gates resolve global and target references identically", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  saveRDS(data.frame(x = 1), file.path(root, "ref.rds"))
  for (rules in list(list(reference_path = "ref.rds"), list(references = list("work.out" = "ref.rds")))) {
    check <- sas_preflight(file, config = list(comparison_rules = rules), outputs = "work.out")
    expect_identical(check$references$status, "available")
    assessment <- assess_dataset_target(check$outputs, data.frame(x = 1), check$project$config$comparison_rules)
    expect_true(assessment$reference_passed)
    expect_identical(assessment$reference_path, check$references$path)
  }
  check <- sas_preflight(file, config = list(comparison_rules = list(reference_path = "missing.rds")), outputs = "work.out")
  expect_identical(check$references$status, "missing")
  expect_identical(check$status, "needs_attention")
  expect_error(sas_preflight(file, config = list(comparison_rules = list(reference_path = ""))),
    "comparison_rules", class = "sas2r_output_contract_error")
})

test_that("same-basename programs retain separate components and genuine cycles", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "prod")); dir.create(file.path(root, "qc"))
  writeLines("data a; set b; run;", file.path(root, "prod", "x.sas"))
  writeLines("data b; set a; run;", file.path(root, "qc", "x.sas"))
  check <- sas_preflight(root, recursive = TRUE)
  expect_length(unique(check$schedule$component_id), 2L)
  for (cid in check$schedule$component_id) {
    statements <- component_statements(check$project, cid)
    expect_length(unique(statements$file), 1L)
    expect_true(all(statements$file %in% check$project$graph$nodes$source_file[
      check$project$graph$nodes$component_id == cid]))
  }
  expect_true(all(check$schedule$group_kind == "cycle"))
  expect_true("dependency_cycle" %in% check$findings$kind)
  expect_identical(check$status, "needs_attention")
  expect_identical(check$schedule, sas_preflight(check$project)$schedule)
})

test_that("later same-file writes cannot be supplied by stale files", {
  root <- withr::local_tempdir()
  saveRDS(data.frame(x = 1), file.path(root, "stage.rds"))
  for (prefix in c("work", "raw")) {
    file <- review_source(root, sprintf("data out; set %s.stage; run; data %s.stage; x=2; run;", prefix, prefix))
    check <- sas_preflight(file, config = list(libraries = list(raw = root)))
    expect_identical(check$inputs$status, "backward_dependency")
    expect_true("backward_dependency" %in% check$findings$kind)
    expect_identical(check$status, "needs_attention")
  }
})

test_that("graph and input readiness agree on point-of-use library identity", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "a")); dir.create(file.path(root, "b"))
  writeLines("libname raw 'a'; data raw.stage; x=1; run;", file.path(root, "01.sas"))
  writeLines("libname raw 'b'; data out; set raw.stage; run;", file.path(root, "02.sas"))
  check <- sas_preflight(root)
  expect_identical(check$inputs$status, "missing")
  edges <- check$project$graph$edges
  expect_identical(edges$resolution[edges$type == "reads_dataset"], "external")
})

test_that("dynamic dataset positions share grammar with static lineage", {
  root <- withr::local_tempdir()
  dynamic <- c("data out; set a(obs=1) raw.&ds; run;",
    "data out; merge a(keep=x) b(where=(x=1)) &other; by x; run;",
    "proc sql; create table out as select * from raw.dm(keep=x y), raw.&ds; quit;",
    'data out; set "&root/dm.sas7bdat"; run;',
    "proc print data=raw.&ds; run;")
  for (code in dynamic) {
    check <- sas_preflight(review_source(root, code))
    expect_true("dynamic_dataset_reference" %in% check$findings$kind, info = code)
    expect_false("work.raw" %in% check$inputs$dataset, info = code)
  }
  static <- c("proc freq data=raw.dm; table &var; run;",
    "proc tabulate data=raw.dm; table trt*&stat; run;",
    "proc sql; update raw.dm set x = &y; quit;",
    "data out; set raw.dm(where=(x=&limit)); run;")
  for (code in static) {
    check <- sas_preflight(review_source(root, code))
    expect_false("dynamic_dataset_reference" %in% check$findings$kind, info = code)
    expect_identical(check$inputs$dataset, "raw.dm")
  }
  check <- sas_preflight(review_source(root, "%macro m(ds); data out; set &ds; run; %mend;"))
  expect_false("dynamic_dataset_reference" %in% check$findings$kind)
  expect_identical(check$status, "ready_for_translation")
  expect_identical(check$unsupported$reason, "macro_deferred")
})

test_that("effective key requirements are checked after inheritance before writes", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  outputs <- list(profiles = list(subject = qc_profile(unique_keys = TRUE)),
    assertions = list("work.out" = list(profile = "subject")))
  check <- sas_preflight(file, config = list(comparison_rules = list(keys = "ID")), outputs = outputs)
  result <- assess_dataset_target(check$outputs, data.frame(ID = c("1", "1")), check$project$config$comparison_rules)
  expect_false(result$checks$unique_keys$passed)
  expect_error(sas_translate(file, config = list(comparison_rules = list(unique_keys = TRUE)),
    out_dir = file.path(root, "out"), outputs = "work.out"), "requires keys", class = "sas2r_output_contract_error")
  expect_false(dir.exists(file.path(root, "out")))
  expect_false(dir.exists(file.path(root, ".sas2r")))
  expect_error(sas_preflight(file, config = list(comparison_rules = list(min_rows = 3)),
    outputs = list(assertions = list("work.out" = list(max_rows = 1)))), "contradict",
    class = "sas2r_output_contract_error")
})

test_that("YAML scalar types preserve metadata names and valid numeric requirements", {
  root <- withr::local_tempdir()
  path <- file.path(root, "config.yml")
  writeLines(c("comparison_rules:", "  min_rows: 1e3", "  labels: {N: Count, Y: Yes, yes: affirmative, no: negative}",
    "outputs:", "  assertions:", "    work.out:", "      unique_keys: true", "      keys: ID"), path)
  cfg <- sas_config(path)
  expect_identical(names(cfg$comparison_rules$labels), c("N", "Y", "yes", "no"))
  expect_equal(cfg$comparison_rules$min_rows, 1000)
  expect_true(cfg$outputs$assertions[[1]]$unique_keys)
  writeLines("outputs: {assertions: {work.out: [row_count, 306]}}", path)
  expect_error(sas_config(path), class = "sas2r_output_contract_error")
  writeLines("outputs:\n  assertions:\n    work.out:", path)
  expect_identical(sas_config(path)$outputs$assertions[[1]], list())
  expect_identical(qc_profile(labels = list(AGE = c(foo = "Age")))$labels, list(AGE = "Age"))
  expect_true(check_dataset_qc(data.frame(TRT = ordered("A")), qc_profile(types = c(TRT = "factor")))$types$passed)
})

test_that("every assessment stores its explanation and unavailable is not failed", {
  root <- withr::local_tempdir()
  for (assess in list(assess_dataset_target, assess_tlf_target)) {
    result <- assess(list(target_key = "missing.rds"), list(outputs_dir = root))
    expect_identical(result$reason, output_checks_reason(result$checks))
    expect_match(result$reason, "candidate_exists")
  }
  writeLines("<html><body>Count</body></html>", file.path(root, "table.html"))
  contract <- list(target_key = "table.html", kind = "tlf", reference_path = file.path(root, "table.html"))
  result <- assess_tlf_target(contract, list(outputs_dir = root))
  expect_true(result$passed)
  expect_match(result$reason, "not evaluated")
  expect_identical(jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE))$reason, result$reason)
})

test_that("output overrides survive reuse without rebuilding a graph", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  check <- sas_preflight(file, outputs = list(assertions = list("work.out" = list(row_count = 1))))
  testthat::local_mocked_bindings(build_dependency_graph = function(...) stop("must reuse plan"))
  again <- sas_preflight(check$project)
  expect_identical(again$outputs, check$outputs)
  expect_identical(again$outputs$assertions[[1]]$row_count, 1)
})

test_that("repeated WORK stages select one writer rather than all historical writers", {
  root <- withr::local_tempdir()
  code <- rep("data stage; x=1; run; data out; set stage; run;", 30)
  check <- sas_preflight(review_source(root, code))
  edges <- check$project$graph$edges
  expect_true(all(check$inputs$status == "generated"))
  expect_equal(sum(edges$type == "reads_dataset"), 30)
  expect_equal(sum(edges$type == "writes_dataset" & edges$detail == "work.stage"), 30)
})

test_that("fence lengths agree between prose and executable documentation checks", {
  lines <- c("````text", "```", "`sas_typo()`", "````", "Use `sas_translate()`.")
  expect_identical(doc_prose_calls(lines), "sas_translate")
})

test_that("each emitted scanner finding has an explicit readiness policy", {
  # Inspect installed function bodies too, including conditional and rep()
  # expressions in the kind argument, not only literal source assignments.
  strings <- function(expr) {
    if (is.character(expr)) return(expr)
    if (is.call(expr) || is.expression(expr) || is.pairlist(expr))
      return(unlist(lapply(as.list(expr), strings), use.names = FALSE))
    character()
  }
  kinds <- function(expr) {
    if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) return(character())
    args <- as.list(expr)
    c(if ("kind" %in% names(args)) strings(args[["kind"]]),
      unlist(lapply(args, kinds), use.names = FALSE))
  }
  raised <- unique(unlist(lapply(list(scan_project, dynamic_dataset_findings,
    deferred_dataset_findings), function(fn) kinds(body(fn)))))
  # Macro discovery returns its selected reason through a variable; its concrete
  # failure inputs are exercised in test-called-macro-translation.R.
  raised <- unique(c(raised, "macro_definition_missing",
    "macro_library_initialization_unsupported", "macro_include_requires_expansion",
    "macro_nested_definition_unsupported"))
  expect_setequal(raised, c(preflight_blocking_findings(), preflight_advisory_findings()))
  root <- withr::local_tempdir()
  check <- sas_preflight(review_source(root, "libname remote xml 'remote'; data remote.out; x=1; run;"))
  expect_true("libref_engine_unsupported" %in% check$findings$kind)
  expect_identical(check$status, "needs_attention")
})

test_that("historical checkpoints explain regeneration instead of silently respending", {
  root <- withr::local_tempdir()
  # This is the actual version-1 shape produced before the planning fix.
  saveRDS(list(fingerprint = "prior", selected_revisions = list()), file.path(root, "resume.rds"))
  state <- list(paths = list(state = root), diagnostics = list())
  expect_message(result <- restore_migration_checkpoint(state, "new"), "older planning policy",
    class = "sas2r_resume_invalidated")
  expect_match(result$diagnostics$resume_invalidated, "older planning policy")
  unlink(file.path(root, "resume.rds"))
  expect_identical(restore_migration_checkpoint(state, "new"), state)
})

test_that("direct reference arguments keep the calling directory on project reuse", {
  root <- withr::local_tempdir(); elsewhere <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  withr::local_dir(elsewhere)
  saveRDS(data.frame(x = 1), "ref.rds")
  check <- sas_preflight(file, outputs = list(references = list("work.out" = "ref.rds")))
  expect_identical(check$references$status, "available")
  expect_identical(sas_preflight(check$project)$references, check$references)
})
