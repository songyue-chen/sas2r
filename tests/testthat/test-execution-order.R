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
