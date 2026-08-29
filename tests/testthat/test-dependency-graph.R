dependency_fixture_project <- function() {
  dir <- withr::local_tempdir()

  # 1. Autoexec file for setup
  writeLines("options nodate nonumber;", file.path(dir, "autoexec.sas"))

  # 2. Included macro definition file
  mac_dir <- file.path(dir, "macros")
  dir.create(mac_dir)
  writeLines(c(
    "%macro my_mac(ds=);",
    "  data work.step1;",
    "    set work.raw;",
    "  run;",
    "%mend;"
  ), file.path(mac_dir, "my_mac.sas"))

  # 3. Main program file
  # Note line numbers:
  # L1: %include 'macros/my_mac.sas';
  # L2: proc format; value yn 1 = 'Y' 0 = 'N'; run;
  # L3: %include &dynamic_target;
  # L4: %my_mac(ds=raw);
  # L5: data work.final; set work.step1; format flag yn.; run;
  writeLines(c(
    "%include 'macros/my_mac.sas';",
    "proc format; value yn 1 = 'Y' 0 = 'N'; run;",
    "%include &dynamic_target;",
    "%my_mac(ds=raw);",
    "data work.final; set work.step1; format flag yn.; run;"
  ), file.path(dir, "main.sas"))

  sas_project(dir, config = list(autoexec = file.path(dir, "autoexec.sas")))
}

test_that("project graph records explicit dependencies and honest unknowns", {
  fx <- dependency_fixture_project()
  graph <- build_dependency_graph(fx)

  expect_setequal(unique(graph$edges$type), c(
    "setup_before", "includes", "calls_macro", "reads_dataset",
    "writes_dataset", "uses_format"
  ))
  call <- graph$edges[graph$edges$type == "calls_macro", ]
  expect_identical(call$resolution, "resolved")
  expect_true(nzchar(call$source_file) && call$line == 4L)
  unknown <- graph$edges[graph$edges$resolution == "dynamic", ]
  expect_identical(unknown$resolution, "dynamic")
})

test_that("dependency graph conforms to canonical schema and node types", {
  fx <- dependency_fixture_project()
  graph <- build_dependency_graph(fx)

  expect_identical(graph$schema_version, "1")
  expect_named(graph$nodes, c(
    "node_id", "component_id", "type", "source_file", "line",
    "original_index", "content_hash"
  ))
  expect_named(graph$edges, c(
    "edge_id", "from", "to", "type", "resolution", "source_file",
    "line", "detail"
  ))

  # Node types must be canonical
  allowed_node_types <- c("setup", "source_unit", "external_input", "unresolved_dependency", "final_output")
  expect_true(all(graph$nodes$type %in% allowed_node_types))

  # Edge types and resolutions must be canonical
  allowed_edge_types <- c(
    "includes", "calls_macro", "uses_format", "uses_function",
    "reads_dataset", "writes_dataset", "setup_before", "contributes_to_output"
  )
  expect_true(all(graph$edges$type %in% allowed_edge_types))

  allowed_resolutions <- c("resolved", "ambiguous", "dynamic", "external", "unresolved")
  expect_true(all(graph$edges$resolution %in% allowed_resolutions))

  # Node IDs must be unique
  expect_identical(anyDuplicated(graph$nodes$node_id), 0L)
  # Edge IDs must be unique
  expect_identical(anyDuplicated(graph$edges$edge_id), 0L)

  # from and to must refer to existing node_ids
  expect_true(all(graph$edges$from %in% graph$nodes$node_id))
  expect_true(all(graph$edges$to %in% graph$nodes$node_id))
})

test_that("sas_project attaches dependency_facts and source_hashes", {
  fx <- dependency_fixture_project()
  expect_true(!is.null(fx$dependency_facts))
  expect_true(!is.null(fx$source_hashes))
  expect_type(fx$dependency_facts, "list")
  expect_type(fx$source_hashes, "character")
})
