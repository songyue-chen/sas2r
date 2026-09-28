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

previously_selected_bundle_fixture <- function(missing_output = FALSE, envir = parent.frame()) {
  fx <- sequential_bundle_defects_fixture(envir)
  prior_state <- fx$state
  prior_state$selected_revisions$prog_a$r_code <- fx$fixed_r_code_a
  prior_state$selected_revisions$prog_b$r_code <- if (missing_output) {
    paste("make_output <- function() {", fx$fixed_r_code_b, "}", sep = "\n")
  } else fx$fixed_r_code_b
  prior <- run_bundle_pipeline(prior_state, max_bundle_repair_rounds = 0L)

  # A real new invocation shares selection state, but has fresh run/attempt paths.
  current <- new_migration_state(fx$project, out_dir = prior_state$paths$root,
    config = prior_state$config, execute = TRUE)
  current$selected_revisions <- fx$state$selected_revisions
  current$output_contracts <- fx$state$output_contracts
  current$fixer_llm <- fx$state$fixer_llm
  current$reviewer_llm <- fx$state$reviewer_llm
  fx$state <- current
  fx$prior <- prior
  fx
}

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
    if (!any(vapply(context$messages, function(m) identical(m$role, "tool"), logical(1))))
      return(list(type = "tool", tool = "read_comparison_report", args = list(report_id = "report_requested")))
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
    text <- paste(vapply(context$messages, function(m) as.character(m$content), ""), collapse = "\n")
    if (grepl("263.525423", text, fixed = TRUE)) material_review_response(
      sas_evidence = "SET copies all source values unchanged", r_evidence = "AVAL[2] receives an extra addition") else valid_program_review_response()
  })

  res <- run_bundle_pipeline(state, max_bundle_repair_rounds = 1L, execute = TRUE)

  reqs <- fixer_llm$requests()
  expect_true(length(reqs) > 0L)
  all_text <- paste(unlist(lapply(reqs, function(rq) {
    vapply(rq$messages, function(m) as.character(m$content %||% ""), character(1))
  })), collapse = "\n")

  expect_no_match(all_text, "n_mismatch|rows_base|ROW_COUNT_DELTA|Output Differences")
  expect_match(all_text, "unknown_tool")
  expect_length(reqs, 2L)
  # Neither initial prompts nor actual tool-result turns contain reference answers.
  expect_no_match(all_text, "736\\.2519")
  expect_no_match(all_text, "999\\.777")
})
