# Test suite for output-driven bundle repair with fresh complete reruns

sequential_bundle_defects_fixture <- function(envir = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = envir)
  input_dir <- file.path(base, "inputs", "adam")
  dir.create(input_dir, recursive = TRUE)
  input_file <- file.path(input_dir, "adsl.rds")
  saveRDS(data.frame(USUBJID = c("01", "02"), TRT = c("A", "B"), stringsAsFactors = FALSE), input_file)

  # Program A creates adam.out1 from adam.adsl
  prog_a <- file.path(base, "prog_a.sas")
  writeLines("data adam.out1; set adam.adsl; run;", prog_a)

  # Program B creates adam.out2 from adam.out1
  prog_b <- file.path(base, "prog_b.sas")
  writeLines("data adam.out2; set adam.out1; DERIVED = 1; run;", prog_b)

  config <- list(libraries = list(adam = list(path = input_dir, engine = "rds", write = "rds")))
  project <- sas_project(base, config = config)

  out_dir <- file.path(base, "migration_out")
  state <- new_migration_state(project, out_dir = out_dir, config = config, execute = TRUE)

  # Initial buggy code: prog_a has Bug A, prog_b has Bug B
  buggy_r_code_a <- "stop('Bug A in prog_a: unhandled syntax')"
  buggy_r_code_b <- "stop('Bug B in prog_b: variable missing')"

  # Good fixes
  fixed_r_code_a <- paste(
    "adsl <- lib_read('adam', 'adsl')",
    "lib_write(adsl, 'adam', 'out1')",
    sep = "\n"
  )
  fixed_r_code_b <- paste(
    "out1 <- lib_read('adam', 'out1')",
    "out2 <- transform(out1, DERIVED = 1)",
    "lib_write(out2, 'adam', 'out2')",
    sep = "\n"
  )

  state$selected_revisions <- list(
    prog_a = list(
      component_id = "prog_a",
      revision_id = "r1",
      r_code = buggy_r_code_a,
      staged_file = "prog_a.R",
      contract = list(component_id = "prog_a", staged_file = "prog_a.R", sas_text = "data adam.out1; set adam.adsl; run;")
    ),
    prog_b = list(
      component_id = "prog_b",
      revision_id = "r1",
      r_code = buggy_r_code_b,
      staged_file = "prog_b.R",
      contract = list(component_id = "prog_b", staged_file = "prog_b.R", sas_text = "data adam.out2; set adam.out1; DERIVED = 1; run;")
    )
  )

  outputs <- data.frame(
    target_id = c("adam.out1", "adam.out2"),
    target_key = c("adam.out1", "adam.out2"),
    logical_name = c("adam.out1", "adam.out2"),
    kind = c("dataset", "dataset"),
    required = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  state$output_contracts <- outputs

  # Mock fixer LLM that fixes prog_a on round 1, then prog_b on round 2
  fixer_calls <- 0L
  fixer_requests <- list()
  fixer_llm <- recording_fixer(function(context) {
    fixer_calls <<- fixer_calls + 1L
    fixer_requests[[length(fixer_requests) + 1L]] <<- context
    if (fixer_calls == 1L) {
      valid_program_fix_response(
        code = fixed_r_code_a,
        diagnosis = "Fixed syntax in prog_a",
        summary = "Repaired prog_a to read adsl and write out1",
        evidence_ids = c("bundle_attempt_001")
      )
    } else {
      valid_program_fix_response(
        code = fixed_r_code_b,
        diagnosis = "Fixed variable in prog_b",
        summary = "Repaired prog_b to read out1 and write out2",
        evidence_ids = c("bundle_attempt_002")
      )
    }
  })
  state$fixer_llm <- fixer_llm

  reviewer_llm <- recording_reviewer(function(context) {
    valid_program_review_response(verdict = "reviewed_no_material_finding")
  })
  state$reviewer_llm <- reviewer_llm

  list(
    base = base,
    project = project,
    state = state,
    outputs = outputs,
    fixed_r_code_a = fixed_r_code_a,
    fixed_r_code_b = fixed_r_code_b,
    get_fixer_calls = function() fixer_calls
  )
}

test_that("bundle repair exposes downstream failures through fresh reruns", {
  fx <- sequential_bundle_defects_fixture()
  result <- run_bundle_pipeline(
    fx$state, max_bundle_repair_rounds = 2L, execute = TRUE
  )
  expect_identical(result$attempts$sequence, 1:3)
  expect_true(all(result$attempts$fresh_work))
  expect_identical(result$attempts$assessed_target_count,
                   rep(nrow(fx$outputs), 3L))
  expect_identical(result$status, "migration_ready")
})

test_that("zero bundle rounds performs one authoritative attempt with no fixer call", {
  fx <- sequential_bundle_defects_fixture()
  result <- run_bundle_pipeline(
    fx$state, max_bundle_repair_rounds = 0L, execute = TRUE
  )
  expect_identical(result$attempts$sequence, 1L)
  expect_identical(fx$get_fixer_calls(), 0L)
  expect_identical(result$status, "blocked")
  expect_length(result$repairs, 0L)
})

test_that("regressive patch preserves prior selected attempt and stops", {
  fx <- sequential_bundle_defects_fixture()
  # Start with prog_a working, prog_b working, but out2 fails an assertion
  fx$state$selected_revisions$prog_a$r_code <- fx$fixed_r_code_a
  fx$state$selected_revisions$prog_b$r_code <- fx$fixed_r_code_b
  fx$state$output_contracts$assertions <- list(
    list(),
    list(required_columns = c("NONEXISTENT_COL"))
  )

  # Fixer in round 1 produces a regressive patch for prog_a that breaks execution
  regressive_code_a <- "stop('Regressive break in prog_a')"
  fx$state$fixer_llm <- recording_fixer(function(context) {
    valid_program_fix_response(
      code = regressive_code_a,
      diagnosis = "Bad fix broke prog_a",
      summary = "Regressed",
      evidence_ids = c("bundle_attempt_001")
    )
  })

  result <- run_bundle_pipeline(
    fx$state, max_bundle_repair_rounds = 2L, execute = TRUE
  )

  # Should run attempt 1 (out1 passes), attempt 2 (crashes, 0 pass), and stop early
  expect_identical(result$attempts$sequence, 1:2)
  # Attempt 1 should remain selected
  expect_identical(result$selected_attempt$attempt_id, "bundle_attempt_001")
})

test_that("no-op identical patch stops bundle repair early", {
  fx <- sequential_bundle_defects_fixture()
  # Fixer returns identical buggy code
  fx$state$fixer_llm <- recording_fixer(function(context) {
    valid_program_fix_response(
      code = fx$state$selected_revisions$prog_a$r_code,
      diagnosis = "No changes made",
      summary = "Identical code",
      evidence_ids = c("bundle_attempt_001")
    )
  })

  result <- run_bundle_pipeline(
    fx$state, max_bundle_repair_rounds = 2L, execute = TRUE
  )

  expect_identical(result$attempts$sequence, 1L)
  expect_identical(result$status, "blocked")
})

test_that("helper snapshot patch invalidates closures and re-reviews before rerun", {
  fx <- sequential_bundle_defects_fixture()
  helper_patch_content <- "# patched helper\nsas_custom_helper <- function(x) x\n"

  fx$state$fixer_llm <- recording_fixer(function(context) {
    valid_program_fix_response(
      code = fx$fixed_r_code_a,
      diagnosis = "Patched helper and prog_a",
      summary = "Updated helper snapshot",
      evidence_ids = c("bundle_attempt_001"),
      bundle_helper_patch = list(
        path = "sas2r-helpers.R",
        content = helper_patch_content,
        reason = "missing helper function"
      )
    )
  })

  result <- run_bundle_pipeline(
    fx$state, max_bundle_repair_rounds = 1L, execute = TRUE
  )

  # Check that repair record includes helper patch
  expect_length(result$repairs, 1L)
  expect_false(is.null(result$repairs[[1L]]$helper_patch))
  expect_identical(result$repairs[[1L]]$helper_patch$path, "sas2r-helpers.R")
})

test_that("bundle repair loop emits structured progress events", {
  fx <- sequential_bundle_defects_fixture()
  events_captured <- list()

  withCallingHandlers(
    run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L, execute = TRUE),
    sas2r_bundle_event = function(e) {
      events_captured[[length(events_captured) + 1L]] <<- e
    }
  )

  ev_types <- vapply(events_captured, function(e) e$event, character(1))
  expect_true("bundle_attempt_started" %in% ev_types)
  expect_true("bundle_attempt_completed" %in% ev_types)
  expect_true("bundle_gate_evaluated" %in% ev_types)
  expect_true("bundle_fixer_invoked" %in% ev_types)
  expect_true("bundle_attempt_selected" %in% ev_types)
})

test_that("bundle repair packet honors the agent_evidence policy sas_translate stored", {
  attempt <- list(attempt_id = "att_1", passed = TRUE)
  assessment <- list(targets = list())

  # The sas_translate() argument is stored on the state and must reach the
  # diagnostics policy; the documented default is code_only, not bounded.
  state_arg <- list(agent_evidence = "code_only", config = list(),
                    selected_revisions = list())
  packet <- build_bundle_repair_packet(state_arg, attempt, assessment)
  expect_identical(packet$bounded_diagnostics$policy, "code_only")

  # No policy anywhere: the documented default is code_only.
  state_default <- list(config = list(), selected_revisions = list())
  packet <- build_bundle_repair_packet(state_default, attempt, assessment)
  expect_identical(packet$bounded_diagnostics$policy, "code_only")

  # Config-level policy is honored when no argument-level policy exists.
  state_cfg <- list(config = list(agent_evidence = "bounded"),
                    selected_revisions = list())
  packet <- build_bundle_repair_packet(state_cfg, attempt, assessment)
  expect_identical(packet$bounded_diagnostics$policy, "bounded")
})

test_that("bundle repair evidence never carries raw cell values to the fixer", {
  base <- withr::local_tempdir()
  input_dir <- file.path(base, "inputs", "adam")
  dir.create(input_dir, recursive = TRUE)
  # Distinctive sentinels: one lives only in the reference, one only in the
  # wrong candidate output. Neither may appear in any LLM-bound message.
  ref_sentinel <- 736.25191
  cand_sentinel <- 999.777333
  saveRDS(data.frame(USUBJID = c("01", "02"), AVAL = c(111.5, ref_sentinel),
                     stringsAsFactors = FALSE),
          file.path(input_dir, "adsl.rds"))
  ref_dir <- file.path(base, "reference")
  dir.create(ref_dir)
  ref_path <- file.path(ref_dir, "out1.rds")
  saveRDS(data.frame(USUBJID = c("01", "02"), AVAL = c(111.5, ref_sentinel),
                     stringsAsFactors = FALSE), ref_path)

  writeLines("data adam.out1; set adam.adsl; run;", file.path(base, "prog_a.sas"))

  config <- list(
    libraries = list(adam = list(path = input_dir, engine = "rds", write = "rds")),
    comparison_rules = list(references = list("adam.out1" = ref_path))
  )
  project <- sas_project(base, config = config)
  state <- new_migration_state(project, out_dir = file.path(base, "migration_out"),
                               config = config, execute = TRUE)

  # The staged code runs cleanly but writes a wrong cell value, so the failed
  # target carries a real comparison with both sentinels in its details. The
  # wrong value is computed, not written literally: the fixer legitimately
  # sees the staged code, so a literal sentinel there would not be a leak.
  buggy <- paste(
    "adsl <- lib_read('adam', 'adsl')",
    "adsl$AVAL[2] <- adsl$AVAL[2] + 263.525423",
    "lib_write(adsl, 'adam', 'out1')",
    sep = "\n"
  )
  state$selected_revisions <- list(
    prog_a = list(
      component_id = "prog_a", revision_id = "r1", r_code = buggy,
      staged_file = "prog_a.R",
      contract = list(component_id = "prog_a", staged_file = "prog_a.R",
                      sas_text = "data adam.out1; set adam.adsl; run;")
    )
  )
  state$output_contracts <- data.frame(
    target_id = "adam.out1", target_key = "adam.out1",
    logical_name = "adam.out1", kind = "dataset", required = TRUE,
    stringsAsFactors = FALSE
  )

  fixer_llm <- recording_fixer(function(context) {
    valid_program_fix_response(
      code = paste("adsl <- lib_read('adam', 'adsl')",
                   "lib_write(adsl, 'adam', 'out1')", sep = "\n"),
      diagnosis = "Removed the wrong assignment",
      summary = "Write adsl through unchanged",
      evidence_ids = c("bundle_attempt_001")
    )
  })
  state$fixer_llm <- fixer_llm
  state$reviewer_llm <- recording_reviewer(function(context) {
    valid_program_review_response(verdict = "reviewed_no_material_finding")
  })

  res <- run_bundle_pipeline(state, max_bundle_repair_rounds = 1L, execute = TRUE)

  reqs <- fixer_llm$requests()
  expect_true(length(reqs) > 0L)
  all_text <- paste(unlist(lapply(reqs, function(rq) {
    vapply(rq$messages, function(m) as.character(m$content %||% ""), character(1))
  })), collapse = "\n")

  # The redacted digest still reaches the fixer...
  expect_match(all_text, "n_mismatch")
  # ...but no raw cell value from either side does.
  expect_no_match(all_text, "736\\.2519")
  expect_no_match(all_text, "999\\.777")
})
