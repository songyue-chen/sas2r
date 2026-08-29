test_that("component evidence is honest and binding scoped", {
  b1 <- new_component_binding("src1", "r1", "helper1", "prompt1", "deps1")
  history <- new_component_evidence_history("macro_a", b1)

  history <- record_review_unavailable(history, "provider_timeout")
  expect_null(current_component_evidence(history)$level)
  expect_true(current_component_evidence(history)$review_unavailable)

  history <- record_completed_review(history,
    verdict = "reviewed_no_material_finding",
    runtime_deferred = "no_callable_path"
  )
  expect_identical(current_component_evidence(history)$level, "reviewed_only")
  expect_identical(current_component_evidence(history)$runtime_deferred,
                   "no_callable_path")

  history <- promote_component_evidence(history, "runtime_verified",
                                        coverage = "call:macro_a(x=1)")
  expect_identical(current_component_evidence(history)$level, "runtime_verified")

  b2 <- new_component_binding("src1", "r2", "helper1", "prompt1", "deps1")
  history <- activate_component_binding(history, b2)
  expect_null(current_component_evidence(history)$level)
  expect_identical(length(history$revisions), 2L)
})

test_that("new_component_binding derives deterministic hash from 5 canonical fields", {
  b1 <- new_component_binding(
    source_hash = "src_123",
    r_hash = "r_456",
    helper_hash = "hlp_789",
    prompt_skill_hash = "ps_abc",
    dependency_closure_hash = "dep_def"
  )
  b2 <- new_component_binding(
    source_hash = "src_123",
    r_hash = "r_456",
    helper_hash = "hlp_789",
    prompt_skill_hash = "ps_abc",
    dependency_closure_hash = "dep_def"
  )
  expect_identical(b1$binding_hash, b2$binding_hash)
  expect_identical(b1$source_hash, "src_123")
  expect_identical(b1$r_hash, "r_456")
  expect_identical(b1$helper_hash, "hlp_789")
  expect_identical(b1$prompt_skill_hash, "ps_abc")
  expect_identical(b1$dependency_closure_hash, "dep_def")

  # Changing any single field changes binding_hash
  b3 <- new_component_binding("src_DIFFERENT", "r_456", "hlp_789", "ps_abc", "dep_def")
  expect_false(identical(b1$binding_hash, b3$binding_hash))

  # Validation errors on missing/invalid arguments
  expect_error(new_component_binding("", "r", "h", "p", "d"), class = "sas2r_invalid_argument")
  expect_error(new_component_binding("s", NULL, "h", "p", "d"), class = "sas2r_invalid_argument")
  expect_error(new_component_binding("s", "r", 123, "p", "d"), class = "sas2r_invalid_argument")
})

test_that("evidence promotion is strictly monotonic and rejects skips without evidence", {
  b <- new_component_binding("src1", "r1", "helper1", "prompt1", "deps1")
  history <- new_component_evidence_history("comp_x", b)

  # Cannot jump directly to runtime_verified or output_verified before review
  expect_error(
    promote_component_evidence(history, "runtime_verified", coverage = "call:comp_x()"),
    class = "sas2r_invalid_evidence_promotion"
  )
  expect_error(
    promote_component_evidence(history, "output_verified", coverage = "dataset:out"),
    class = "sas2r_invalid_evidence_promotion"
  )

  # Completed review is the only gateway to reviewed_only
  history <- record_completed_review(history, verdict = "reviewed_no_material_finding")
  expect_identical(current_component_evidence(history)$level, "reviewed_only")

  # Cannot jump to output_verified from reviewed_only without runtime_verified
  expect_error(
    promote_component_evidence(history, "output_verified", coverage = "dataset:out"),
    class = "sas2r_invalid_evidence_promotion"
  )

  # Cannot promote to runtime_verified without coverage
  expect_error(
    promote_component_evidence(history, "runtime_verified", coverage = NULL),
    class = "sas2r_invalid_evidence_promotion"
  )
  expect_error(
    promote_component_evidence(history, "runtime_verified", coverage = ""),
    class = "sas2r_invalid_evidence_promotion"
  )

  # Valid promotion to runtime_verified
  history <- promote_component_evidence(history, "runtime_verified", coverage = "call:comp_x(1)")
  expect_identical(current_component_evidence(history)$level, "runtime_verified")

  # Cannot promote to output_verified without coverage or basis
  expect_error(
    promote_component_evidence(history, "output_verified", coverage = NULL, basis_ids = character()),
    class = "sas2r_invalid_evidence_promotion"
  )

  # Valid promotion to output_verified
  history <- promote_component_evidence(history, "output_verified",
                                        coverage = "dataset:work.out",
                                        basis_ids = "attempt_1_out")
  expect_identical(current_component_evidence(history)$level, "output_verified")

  # Cannot promote to reference_validated without basis_ids
  expect_error(
    promote_component_evidence(history, "reference_validated", basis_ids = character()),
    class = "sas2r_invalid_evidence_promotion"
  )

  # Valid promotion to reference_validated
  history <- promote_component_evidence(history, "reference_validated",
                                        basis_ids = "ref_report_001")
  expect_identical(current_component_evidence(history)$level, "reference_validated")

  # Demotion is rejected
  expect_error(
    promote_component_evidence(history, "reviewed_only"),
    class = "sas2r_invalid_evidence_promotion"
  )
  expect_error(
    promote_component_evidence(history, "runtime_verified"),
    class = "sas2r_invalid_evidence_promotion"
  )
})

test_that("review_unavailable adds blocker and record_completed_review resolves it", {
  b <- new_component_binding("src1", "r1", "helper1", "prompt1", "deps1")
  history <- new_component_evidence_history("comp_y", b)

  history <- record_review_unavailable(history, "llm_rate_limit")
  rev <- current_component_evidence(history)
  expect_null(rev$level)
  expect_true(rev$review_unavailable)
  expect_true("llm_rate_limit" %in% rev$blockers)

  # Promotion while review is unavailable is rejected
  expect_error(
    promote_component_evidence(history, "runtime_verified", coverage = "call:comp_y()"),
    class = "sas2r_invalid_evidence_promotion"
  )

  # Repair required review also does not grant reviewed_only
  history <- record_completed_review(history, verdict = "repair_required")
  rev <- current_component_evidence(history)
  expect_null(rev$level)
  expect_true("repair_required" %in% rev$blockers)

  # Successful completed review clears review_unavailable blocker and grants reviewed_only
  history <- record_completed_review(history, verdict = "reviewed_no_material_finding")
  rev <- current_component_evidence(history)
  expect_identical(rev$level, "reviewed_only")
  expect_false(rev$review_unavailable)
  expect_false("llm_rate_limit" %in% rev$blockers)
  expect_false("repair_required" %in% rev$blockers)

  # Event history preserves all intermediate events
  events <- rev$events
  expect_gte(length(events), 3L)
  event_types <- vapply(events, `[[`, character(1), "type")
  expect_true("review_unavailable" %in% event_types)
  expect_true("review_completed" %in% event_types)
})

test_that("activating a new binding creates a fresh active revision and preserves prior revisions", {
  b1 <- new_component_binding("src1", "r1", "helper1", "prompt1", "deps1")
  history <- new_component_evidence_history("macro_m", b1)
  history <- record_completed_review(history, verdict = "reviewed_no_material_finding")
  history <- promote_component_evidence(history, "runtime_verified", coverage = "call:macro_m()")
  history <- promote_component_evidence(history, "output_verified", coverage = "dataset:m_out", basis_ids = "att1")

  expect_identical(current_component_evidence(history)$level, "output_verified")
  expect_identical(length(history$revisions), 1L)

  # Activating new binding (e.g. after code repair)
  b2 <- new_component_binding("src1", "r2_fixed", "helper1", "prompt1", "deps1")
  history <- activate_component_binding(history, b2)

  # Active revision is reset with no evidence
  active_rev <- current_component_evidence(history)
  expect_null(active_rev$level)
  expect_identical(active_rev$binding$r_hash, "r2_fixed")
  expect_identical(length(history$revisions), 2L)

  # Historical revision 1 is completely preserved and untouched
  rev1 <- history$revisions[[1]]
  expect_identical(rev1$level, "output_verified")
  expect_identical(rev1$binding$r_hash, "r1")
  expect_identical(rev1$coverage, c("call:macro_m()", "dataset:m_out"))
})

test_that("evidence_for_output_lineage evaluates upstream lineage and ignores unrelated low evidence", {
  # Build a mock graph:
  # setup -> data_prep -> table_gen -> final_output (node_output_table1)
  # unrelated_macro (isolated, has failed review / no evidence)
  nodes <- tibble::tibble(
    node_id = c("n_setup", "n_prep", "n_table", "n_out_table1", "n_unrelated"),
    component_id = c("setup", "data_prep", "table_gen", "table1", "unrelated_macro"),
    type = c("setup", "source_unit", "source_unit", "final_output", "source_unit"),
    source_file = c("setup.sas", "prep.sas", "table.sas", "table.sas", "unrelated.sas"),
    line = c(1L, 1L, 1L, 10L, 1L),
    original_index = c(1L, 2L, 3L, 4L, 5L),
    content_hash = c("h1", "h2", "h3", "h4", "h5")
  )

  edges <- tibble::tibble(
    edge_id = c("e1", "e2", "e3"),
    from = c("n_setup", "n_prep", "n_table"),
    to = c("n_prep", "n_table", "n_out_table1"),
    type = c("setup_before", "reads_dataset", "writes_output"),
    resolution = c("resolved", "resolved", "resolved"),
    source_file = c("prep.sas", "table.sas", "table.sas"),
    line = c(1L, 1L, 10L),
    detail = c("setup", "work.prep_ds", "table1")
  )
  graph <- list(schema_version = "1", nodes = nodes, edges = edges)

  # Create histories
  b_setup <- new_component_binding("s_s", "r_s", "h", "p", "d_s")
  h_setup <- new_component_evidence_history("setup", b_setup)
  h_setup <- record_completed_review(h_setup, "reviewed_no_material_finding")
  h_setup <- promote_component_evidence(h_setup, "runtime_verified", coverage = "source:setup.R")

  b_prep <- new_component_binding("s_p", "r_p", "h", "p", "d_p")
  h_prep <- new_component_evidence_history("data_prep", b_prep)
  h_prep <- record_completed_review(h_prep, "reviewed_no_material_finding")
  h_prep <- promote_component_evidence(h_prep, "runtime_verified", coverage = "call:data_prep()")
  h_prep <- promote_component_evidence(h_prep, "output_verified", coverage = "dataset:work.prep_ds", basis_ids = "att1")

  b_table <- new_component_binding("s_t", "r_t", "h", "p", "d_t")
  h_table <- new_component_evidence_history("table_gen", b_table)
  h_table <- record_completed_review(h_table, "reviewed_no_material_finding")
  h_table <- promote_component_evidence(h_table, "runtime_verified", coverage = "call:table_gen()")

  # Unrelated component is blocked with review unavailable
  b_unrelated <- new_component_binding("s_u", "r_u", "h", "p", "d_u")
  h_unrelated <- new_component_evidence_history("unrelated_macro", b_unrelated)
  h_unrelated <- record_review_unavailable(h_unrelated, "provider_crash")

  histories <- list(
    setup = h_setup,
    data_prep = h_prep,
    table_gen = h_table,
    unrelated_macro = h_unrelated
  )

  # Check lineage for table1
  lineage_ev <- evidence_for_output_lineage(graph, histories, target_id = "table1")

  expect_setequal(lineage_ev$upstream_components, c("setup", "data_prep", "table_gen"))
  expect_false("unrelated_macro" %in% lineage_ev$upstream_components)
  expect_false(lineage_ev$is_blocked)
  expect_false(lineage_ev$review_unavailable)
  expect_identical(lineage_ev$min_level, "runtime_verified")
  expect_identical(length(lineage_ev$blockers), 0L)

  # If one component in the actual lineage has review_unavailable, lineage is blocked
  h_prep_blocked <- record_review_unavailable(h_prep, "prep_timeout")
  histories$data_prep <- h_prep_blocked
  lineage_ev_blocked <- evidence_for_output_lineage(graph, histories, target_id = "table1")
  expect_true(lineage_ev_blocked$is_blocked)
  expect_true(lineage_ev_blocked$review_unavailable)
  expect_true("prep_timeout" %in% lineage_ev_blocked$blockers)
})

test_that("evidence history constructors and edge cases behave correctly", {
  # Empty history without binding
  h_empty <- new_component_evidence_history("empty_comp")
  expect_null(current_component_evidence(h_empty))

  # Recording review on empty history errors
  expect_error(record_review_unavailable(h_empty), class = "sas2r_invalid_state")
  expect_error(record_completed_review(h_empty), class = "sas2r_invalid_state")
  expect_error(promote_component_evidence(h_empty, "runtime_verified"), class = "sas2r_invalid_state")

  # Activating binding on empty history creates first revision
  b <- new_component_binding("s", "r", "h", "p", "d")
  h_populated <- activate_component_binding(h_empty, b)
  expect_identical(length(h_populated$revisions), 1L)
  expect_identical(current_component_evidence(h_populated)$component_id, "empty_comp")

  # Invalid component_id
  expect_error(new_component_evidence_history(""), class = "sas2r_invalid_argument")
  expect_error(new_component_evidence_history(123), class = "sas2r_invalid_argument")

  # Invalid target_id for output lineage
  expect_error(evidence_for_output_lineage(list(), list(), ""), class = "sas2r_invalid_argument")
})
