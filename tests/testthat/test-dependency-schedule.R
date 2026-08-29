schedule_fixture_graph <- function() {
  nodes <- tibble::tibble(
    node_id = c("node_setup", "node_a", "node_b", "node_c", "node_d"),
    component_id = c("setup", "a", "b", "c", "d"),
    type = c("setup", "source_unit", "source_unit", "source_unit", "source_unit"),
    source_file = c("autoexec.sas", "a.sas", "b.sas", "c.sas", "d.sas"),
    line = c(1L, 1L, 1L, 1L, 1L),
    original_index = c(1L, 2L, 3L, 4L, 5L),
    content_hash = c("h0", "h1", "h2", "h3", "h4")
  )

  edges <- tibble::tibble(
    edge_id = c("e1", "e2", "e3", "e4", "e5"),
    from = c("node_setup", "node_a", "node_b", "node_c", "node_c"),
    to = c("node_a", "node_b", "node_b", "node_c", "node_d"),
    type = c("setup_before", "reads_dataset", "reads_dataset", "reads_dataset", "reads_dataset"),
    resolution = rep("resolved", 5L),
    source_file = c("autoexec.sas", "b.sas", "b.sas", "c.sas", "d.sas"),
    line = c(1L, 1L, 1L, 1L, 1L),
    detail = c("setup", "ds_a", "ds_b", "ds_c", "ds_c")
  )

  list(
    schema_version = "1",
    nodes = nodes,
    edges = edges
  )
}

test_that("scheduler is stable and groups cycles without dropping them", {
  graph <- schedule_fixture_graph()
  schedule <- stable_dependency_schedule(graph)
  expect_identical(schedule$component_id, c("setup", "a", "b", "c", "d"))
  expect_identical(schedule$group_kind[schedule$component_id %in% c("b", "c")],
                   c("cycle", "cycle"))
  expect_true(schedule$sequence[schedule$component_id == "d"] >
              schedule$sequence[schedule$component_id == "c"])
})

test_that("only changed dependency closures are requeued", {
  old <- c(a = "A1", b = "B1", c = "C1")
  now <- c(a = "A2", b = "B1", c = "C1")
  expect_identical(
    requeue_components(schedule_fixture_graph(), old, now),
    c("a", "b")
  )
})

test_that("tie breaking uses original_index deterministically for independent components", {
  nodes <- tibble::tibble(
    node_id = c("n_z", "n_m", "n_a"),
    component_id = c("z", "m", "a"),
    type = rep("source_unit", 3L),
    source_file = c("z.sas", "m.sas", "a.sas"),
    line = rep(1L, 3L),
    original_index = c(10L, 20L, 30L),
    content_hash = c("hz", "hm", "ha")
  )
  edges <- tibble::tibble(
    edge_id = character(), from = character(), to = character(),
    type = character(), resolution = character(), source_file = character(),
    line = integer(), detail = character()
  )
  graph <- list(schema_version = "1", nodes = nodes, edges = edges)
  schedule <- stable_dependency_schedule(graph)

  expect_identical(schedule$component_id, c("z", "m", "a"))
  expect_identical(schedule$group_kind, c("independent", "independent", "independent"))
  expect_identical(schedule$sequence, 1:3)
})

test_that("group_kind correctly classifies singleton, cycle, and independent", {
  nodes <- tibble::tibble(
    node_id = c("n_conn1", "n_conn2", "n_cyc1", "n_cyc2", "n_indep"),
    component_id = c("conn1", "conn2", "cyc1", "cyc2", "indep"),
    type = rep("source_unit", 5L),
    source_file = paste0(c("conn1", "conn2", "cyc1", "cyc2", "indep"), ".sas"),
    line = rep(1L, 5L),
    original_index = 1:5,
    content_hash = paste0("h", 1:5)
  )
  edges <- tibble::tibble(
    edge_id = c("e1", "e2", "e3"),
    from = c("n_conn1", "n_cyc1", "n_cyc2"),
    to = c("n_conn2", "n_cyc2", "n_cyc1"),
    type = rep("reads_dataset", 3L),
    resolution = rep("resolved", 3L),
    source_file = rep("test.sas", 3L),
    line = rep(1L, 3L),
    detail = rep("ds", 3L)
  )
  graph <- list(schema_version = "1", nodes = nodes, edges = edges)
  schedule <- stable_dependency_schedule(graph)

  expect_identical(schedule$group_kind[schedule$component_id == "conn1"], "singleton")
  expect_identical(schedule$group_kind[schedule$component_id == "conn2"], "singleton")
  expect_identical(schedule$group_kind[schedule$component_id %in% c("cyc1", "cyc2")], c("cycle", "cycle"))
  expect_identical(schedule$group_kind[schedule$component_id == "indep"], "independent")
})

test_that("unresolved dependencies are attached to schedule components", {
  nodes <- tibble::tibble(
    node_id = c("n_a", "n_unres"),
    component_id = c("a", "dynamic_macro_x"),
    type = c("source_unit", "unresolved_dependency"),
    source_file = c("a.sas", "a.sas"),
    line = c(1L, 5L),
    original_index = c(1L, NA_integer_),
    content_hash = c("ha", "hunres")
  )
  edges <- tibble::tibble(
    edge_id = "e1",
    from = "n_unres",
    to = "n_a",
    type = "calls_macro",
    resolution = "dynamic",
    source_file = "a.sas",
    line = 5L,
    detail = "dynamic_macro_x"
  )
  graph <- list(schema_version = "1", nodes = nodes, edges = edges)
  schedule <- stable_dependency_schedule(graph)

  expect_identical(schedule$component_id, "a")
  expect_identical(schedule$unresolved_dependencies[[1]], "dynamic_macro_x")
})

test_that("dependency_closure computes complete transitive provider closure across multi-hop chains", {
  nodes <- tibble::tibble(
    node_id = c("n_setup", "n_a", "n_b", "n_c", "n_d"),
    component_id = c("setup", "a", "b", "c", "d"),
    type = c("setup", rep("source_unit", 4L)),
    source_file = c("autoexec.sas", "a.sas", "b.sas", "c.sas", "d.sas"),
    line = rep(1L, 5L),
    original_index = 1:5,
    content_hash = paste0("h", 1:5)
  )
  edges <- tibble::tibble(
    edge_id = paste0("e", 1:4),
    from = c("n_setup", "n_a", "n_b", "n_c"),
    to = c("n_a", "n_b", "n_c", "n_d"),
    type = rep("reads_dataset", 4L),
    resolution = rep("resolved", 4L),
    source_file = c("a.sas", "b.sas", "c.sas", "d.sas"),
    line = rep(1L, 4L),
    detail = c("ds0", "ds1", "ds2", "ds3")
  )
  chain_graph <- list(schema_version = "1", nodes = nodes, edges = edges)

  expect_identical(dependency_closure(chain_graph, "setup"), character())
  expect_identical(dependency_closure(chain_graph, "a"), "setup")
  expect_identical(dependency_closure(chain_graph, "b"), c("setup", "a"))
  expect_identical(dependency_closure(chain_graph, "c"), c("setup", "a", "b"))
  expect_identical(dependency_closure(chain_graph, "d"), c("setup", "a", "b", "c"))
})

test_that("dependency_closure_hashes incorporates helper_hash and prompt_skill_hash", {
  graph <- schedule_fixture_graph()
  revs <- c(setup = "S1", a = "A1", b = "B1", c = "C1", d = "D1")

  h1 <- dependency_closure_hashes(graph, revs, helper_hash = "help1", prompt_skill_hash = "ps1")
  h2 <- dependency_closure_hashes(graph, revs, helper_hash = "help2", prompt_skill_hash = "ps1")
  h3 <- dependency_closure_hashes(graph, revs, helper_hash = "help1", prompt_skill_hash = "ps2")

  expect_named(h1, c("setup", "a", "b", "c", "d"))
  expect_false(any(h1 == h2))
  expect_false(any(h1 == h3))
})

test_that("requeue_components handles runtime_deferred components", {
  graph <- schedule_fixture_graph()
  old <- c(a = "A1", b = "B1", c = "C1")
  now <- c(a = "A1", b = "B1", c = "C1")

  # Nothing changed, nothing deferred
  expect_identical(requeue_components(graph, old, now), character())

  # d was runtime_deferred, prerequisite c is present in now
  expect_identical(requeue_components(graph, old, now, runtime_deferred = "d"), "d")
})

