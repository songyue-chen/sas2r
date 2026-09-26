ordered_fixture <- function(code, order = names(code), envir = parent.frame(), libraries = list()) {
  root <- withr::local_tempdir(.local_envir = envir)
  for (name in names(code)) {
    file <- file.path(root, name)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(code[[name]], file)
  }
  config <- list(migration = list(execution_order = order), libraries = libraries)
  list(root = root, config = config)
}

test_that("declared order follows the latest WORK write and preserves in-place inputs", {
  fx <- ordered_fixture(list(
    "z_seed.sas" = "data work.tmp; x=1; run;",
    "a_update.sas" = "data work.tmp; set work.tmp; x=x+1; run;",
    "b_read.sas" = "data work.out; set work.tmp; run;"))
  p <- sas_preflight(fx$root, config = fx$config)
  expect_identical(p$pipeline$execution_order, c("z_seed", "a_update", "b_read"))
  expect_identical(p$pipeline$order_source, "configured")
  expect_identical(p$inputs$status, c("generated", "generated"))
  producers <- dataset_producers(p$project)
  expect_identical(basename(vapply(producers$events, `[[`, "", "writer_root")),
                   c("z_seed.sas", "a_update.sas"))
  expect_false(any(producers$backward))
  expect_false(any(p$schedule$group_kind == "cycle"))
  expect_identical(dependency_closure(p$project$graph, "b_read"), c("z_seed", "a_update"))
  expect_match(paste(pipeline_coverage_lines(p$pipeline), collapse = "\n"), "configured execution order")
})

test_that("future writes never supply reads or resurrect an older WORK version", {
  fx <- ordered_fixture(list(
    "a.sas" = c("data out; set work.tmp; run;", "data work.tmp; x=2; run;"),
    "z.sas" = "data work.tmp; x=1; run;"))
  p <- sas_preflight(fx$root, config = fx$config)
  expect_identical(p$inputs$status, "no_producer")
  expect_true(is.na(dataset_producers(p$project)$events[[1L]]$writer))
  expect_identical(p$pipeline$execution_order, c("a", "z"))
  fx$config$migration$execution_order <- rev(fx$config$migration$execution_order)
  reversed <- sas_preflight(fx$root, config = fx$config)
  expect_identical(reversed$inputs$status, "generated")
  expect_identical(reversed$pipeline$execution_order, c("z", "a"))
})

test_that("order configuration anchors paths and rejects incomplete or stale plans", {
  fx <- ordered_fixture(list("programs/z.sas" = "data work.tmp; x=1; run;",
    "programs/a.sas" = "data out; set work.tmp; run;"))
  config <- file.path(fx$root, "_sas2r.yml")
  yaml::write_yaml(fx$config, config)
  p <- withr::with_dir(tempdir(), sas_preflight(file.path(fx$root, "programs"), config = config))
  expect_identical(p$pipeline$execution_order, c("z", "a"))
  expect_identical(sas_preflight(p$project)$pipeline, p$pipeline)
  expect_error(sas_preflight(p$project, config = list(migration = list(
    execution_order = rev(p$project$config$migration$execution_order)))), "rescan")
  for (order in list("programs/z.sas", c("programs/z.sas", "programs/z.sas"),
                    c("programs/z.sas", "missing.sas"))) {
    fx$config$migration$execution_order <- order
    expect_error(sas_preflight(fx$root, config = fx$config, recursive = TRUE),
                 class = "sas2r_config_error")
  }
  for (order in list(character(), 1, list("a.sas", 2), NA_character_))
    expect_error(normalize_migration_config(list(execution_order = order)), class = "sas2r_config_error")
})

test_that("macro output and later scratch reuse do not create reverse dependencies", {
  fx <- ordered_fixture(list(
    "a.sas" = c("%macro mk(out=); data &out; x=1; run; %mend;",
      "%mk(out=work.tmp);", "data work.tmp; set work.tmp; x=x+1; run;",
      "data result.final; set work.tmp; run;"),
    "b.sas" = "data work.tmp; set result.final; run;"))
  fx$config$libraries <- list(result = fx$root)
  p <- sas_preflight(fx$root, config = fx$config)
  expect_false(any(p$schedule$group_kind == "cycle"))
  expect_identical(p$pipeline$execution_order, c("a", "b"))
  expect_identical(p$inputs$status, c("deferred", "generated", "generated"))
  expect_true(is.na(dataset_producers(p$project)$events[[1L]]$writer))
})

test_that("intervening macro effects and deletion invalidate old producer claims", {
  for (effect in c("%macro overwrite; data tmp; x=2; run; %mend;\n%overwrite;",
                   "proc datasets library=work kill nolist; quit;")) {
    fx <- ordered_fixture(list("a.sas" = c("data tmp; x=1; run;", effect,
      "data out; set tmp; run;")))
    p <- sas_preflight(fx$root, config = fx$config)
    expect_identical(p$inputs$status, "deferred")
    expect_true(is.na(dataset_producers(p$project)$events[[1L]]$writer))
  }
})

test_that("repeated nested includes consume the state at each call site", {
  fx <- ordered_fixture(list(
    "z.sas" = c("data tmp; x=1; run;", "%include 'inc/update.sas';"),
    "a.sas" = c("data tmp; x=10; run;", "%include 'inc/update.sas';",
      "data out; set tmp; run;"),
    "inc/update.sas" = "%include 'inner.sas';",
    "inc/inner.sas" = "data tmp; set tmp; x=x+1; run;"), order = c("z.sas", "a.sas"))
  p <- sas_preflight(fx$root, config = fx$config)
  expect_false(any(p$schedule$group_kind == "cycle"))
  expect_identical(p$pipeline$execution_order, c("z", "a"))
  expect_true(all(p$inputs$status == "generated"))
  events <- dataset_producers(p$project)$events
  inner <- Filter(function(e) basename(p$project$lineage$file[e$row]) == "inner.sas", events)
  expect_length(inner, 2L)
  expect_identical(basename(vapply(inner, `[[`, "", "writer_root")), c("z.sas", "a.sas"))
})

test_that("ordered permanent dataset writes retain bound library identity", {
  fx <- ordered_fixture(list("z.sas" = "data adam.tmp; x=1; run;",
    "a.sas" = "data adam.tmp; set adam.tmp; x=x+1; run;",
    "b.sas" = "data out; set adam.tmp; run;"))
  fx$config$libraries <- list(adam = fx$root)
  p <- sas_preflight(fx$root, config = fx$config)
  expect_identical(p$inputs$status, c("generated", "generated"))
  expect_identical(basename(vapply(dataset_producers(p$project)$events, `[[`, "", "writer_root")),
    c("z.sas", "a.sas"))
})

test_that("ordered bundle and smoke execution see the latest shared WORK contents", {
  fx <- ordered_fixture(list(
    "z.sas" = "data work.tmp; x=1; run;",
    "a.sas" = "data work.tmp; set work.tmp; x=x+1; run;",
    "b.sas" = "data adam.out; set work.tmp; run;"))
  fx$config$libraries <- list(adam = fx$root)
  p <- sas_preflight(fx$root, config = fx$config)$project
  state <- new_migration_state(p, out_dir = withr::local_tempdir(), config = p$config, execute = TRUE)
  code <- c(z = "lib_write(data.frame(x=1), 'work', 'tmp')",
    a = "x <- lib_read('work', 'tmp'); x$x <- x$x + 1; lib_write(x, 'work', 'tmp')",
    b = "lib_write(lib_read('work', 'tmp'), 'adam', 'out')")
  state$selected_revisions <- lapply(names(code), function(id) list(component_id = id,
    r_code = code[[id]], staged_file = paste0(id, ".R"),
    contract = list(component_id = id, staged_file = paste0(id, ".R"))))
  names(state$selected_revisions) <- names(code)
  smoke <- build_program_smoke_plan(state$graph, "b", state$selected_revisions)
  expect_identical(smoke$dependency_prefix, c("z", "a"))
  prepared <- prepare_program_smoke(state, smoke, withr::local_tempdir())
  result <- run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir)
  expect_identical(result$exit_status, 0L)
  attempt <- run_bundle_attempt(state)
  expect_identical(attempt$execution_order, c("z", "a", "b"))
  expect_identical(attempt$exit_status, 0L)
  out <- list.files(attempt$attempt_dir, pattern = "^out.rds$", recursive = TRUE, full.names = TRUE)
  expect_length(out, 1L)
  expect_equal(readRDS(out)$x, 2)
  state$diagnostics$deferred_components <- list(a = "missing source")
  partial <- run_bundle_attempt(state, sequence = 2L)
  expect_identical(partial$execution_order, "z")
  expect_setequal(partial$deferred_component_ids, c("a", "b"))
})

test_that("ordered smoke and bundle run repeated nested includes only at their call sites", {
  fx <- ordered_fixture(list(
    "z.sas" = c("data tmp; x=1; run;", "%include 'inc/update.sas';"),
    "a.sas" = c("%include 'inc/update.sas';", "data adam.out; set tmp; run;"),
    "inc/update.sas" = "%include 'inner.sas';",
    "inc/inner.sas" = "data tmp; set tmp; x=x+1; run;"), order = c("z.sas", "a.sas"))
  fx$config$libraries <- list(adam = fx$root)
  p <- sas_preflight(fx$root, config = fx$config)$project
  state <- new_migration_state(p, out_dir = withr::local_tempdir(), config = p$config, execute = TRUE)
  nodes <- state$graph$nodes
  source_nodes <- nodes[!duplicated(nodes$component_id) & nodes$type == "source_unit", ]
  code <- c(z.sas = "lib_write(data.frame(x=1), 'work', 'tmp'); sas2r_source_include('inc/update.R')",
    a.sas = "sas2r_source_include('inc/update.R'); lib_write(lib_read('work', 'tmp'), 'adam', 'out')",
    update.sas = "sas2r_source_include('inc/inner.R')",
    inner.sas = "x <- lib_read('work', 'tmp'); x$x <- x$x + 1; lib_write(x, 'work', 'tmp')")
  state$selected_revisions <- stats::setNames(lapply(seq_len(nrow(source_nodes)), function(i) {
    id <- source_nodes$component_id[i]
    staged <- canonical_include_staged_path(source_nodes$source_file[i], p$project_dir)
    list(component_id = id, r_code = code[[basename(source_nodes$source_file[i])]],
      staged_file = staged, contract = list(component_id = id, staged_file = staged))
  }), source_nodes$component_id)
  smoke <- build_program_smoke_plan(state$graph, "a", state$selected_revisions)
  expect_identical(smoke$dependency_prefix, "z")
  expect_length(smoke$included_modules, 2L)
  prepared <- prepare_program_smoke(state, smoke, withr::local_tempdir())
  result <- run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir)
  expect_identical(result$exit_status, 0L)
  out <- list.files(prepared$attempt_dir, pattern = "^out.rds$", recursive = TRUE, full.names = TRUE)
  expect_length(out, 1L)
  expect_equal(readRDS(out)$x, 3)
  attempt <- run_bundle_attempt(state)
  expect_identical(attempt$exit_status, 0L)
  out <- list.files(attempt$attempt_dir, pattern = "^out.rds$", recursive = TRUE, full.names = TRUE)
  expect_length(out, 1L)
  expect_equal(readRDS(out)$x, 3)
})

test_that("unmodeled within-step effects do not publish misleading producer versions", {
  effects <- c(
    "%if 0 %then %do; data tmp; x=2; run; %end;",
    "data tmp; set tmp; call execute('data tmp; x=10; run;'); run;",
    "proc sql; create table tmp as select * from tmp; drop table tmp; quit;",
    "proc sql; create table first as select * from tmp; create table tmp as select * from first; quit;",
    "proc sql; create table tmp as select * from tmp; create table tmp as select * from tmp; quit;")
  for (effect in effects) {
    fx <- ordered_fixture(list("a.sas" = c("data tmp; x=1; run;", effect,
      "data out; set tmp; run;")))
    p <- sas_preflight(fx$root, config = fx$config)
    expect_identical(tail(p$inputs$status, 1L), "deferred")
    expect_true(is.na(tail(dataset_producers(p$project)$events, 1L)[[1L]]$writer))
  }
})
