test_that("migration workers have separate authority and exact schemas", {
  expect_identical(migration_agent_names(), c("translator", "reviewer", "fixer"))

  review <- list(
    verdict = "reviewed_no_material_finding",
    static_runnability = "looks_runnable",
    findings = list(),
    unresolved_dependencies = list()
  )
  expect_silent(validate_agent_output(review, "program_review_v1"))
  review$verdict <- "output_verified"
  expect_error(validate_agent_output(review, "program_review_v1"),
               class = "sas2r_schema_error")
})

test_that("program_translation_v1 validates closed structure and required fields", {
  valid_trans <- list(
    r_code = "lib_write(lib_read('work', 'a'), 'work', 'b')",
    summary = "Copies table work.a to work.b",
    parameters = list(
      list(
        name = "ds_in",
        type = "character",
        required = TRUE,
        default = "work.a"
      )
    ),
    defaults = list(mode = "overwrite"),
    reads = list("work.a"),
    writes = list("work.b"),
    side_effects = list(),
    helper_use = list("lib_read", "lib_write"),
    discovered_dependencies = list("work.a"),
    suspected_dependencies = list(),
    affected_outputs = list("work.b"),
    uncertainty = list(
      list(
        severity = "low",
        claim = "assumes work.a is sorted",
        evidence = "data step without sort statement",
        affected_outputs = list("work.b")
      )
    )
  )

  expect_silent(validate_agent_output(valid_trans, "program_translation_v1"))

  # Missing required field
  bad_missing <- valid_trans
  bad_missing$summary <- NULL
  expect_error(validate_agent_output(bad_missing, "program_translation_v1"),
               class = "sas2r_schema_error")

  # Unexpected extra field (closed schema)
  bad_extra <- valid_trans
  bad_extra$extra_field <- "forbidden"
  expect_error(validate_agent_output(bad_extra, "program_translation_v1"),
               class = "sas2r_schema_error")

  # Invalid uncertainty severity enum
  bad_sev <- valid_trans
  bad_sev$uncertainty[[1]]$severity <- "critical"
  expect_error(validate_agent_output(bad_sev, "program_translation_v1"),
               class = "sas2r_schema_error")

  # Duplicate items in unique string array
  bad_dup <- valid_trans
  bad_dup$reads <- list("work.a", "work.a")
  expect_error(validate_agent_output(bad_dup, "program_translation_v1"),
               class = "sas2r_schema_error")
})

test_that("program_review_v1 rejects runtime evidence fields and enforces closed findings", {
  valid_review <- list(
    verdict = "repair_required",
    static_runnability = "known_blocker",
    unresolved_dependencies = list("work.missing_table"),
    findings = list(
      list(
        severity = "material",
        sas_evidence = "set missing_table;",
        r_evidence = "lib_read('work', 'missing_table')",
        affected_outputs = list("work.target"),
        confidence = 0.95,
        unresolved_dependencies = list("work.missing_table")
      )
    )
  )

  expect_silent(validate_agent_output(valid_review, "program_review_v1"))

  # Reject runtime evidence fields in review schema
  runtime_claim <- valid_review
  runtime_claim$findings[[1]]$runtime_evidence <- "executed and crashed"
  expect_error(validate_agent_output(runtime_claim, "program_review_v1"),
               class = "sas2r_schema_error")

  # Reject invalid verdict
  bad_verdict <- valid_review
  bad_verdict$verdict <- "runtime_verified"
  expect_error(validate_agent_output(bad_verdict, "program_review_v1"),
               class = "sas2r_schema_error")

  # Reject out of bounds confidence
  bad_conf <- valid_review
  bad_conf$findings[[1]]$confidence <- 1.5
  expect_error(validate_agent_output(bad_conf, "program_review_v1"),
               class = "sas2r_schema_error")
})

test_that("program_fix_v1 validates patch and closed evidence requirements", {
  valid_fix_no_patch <- list(
    r_code = "x <- lib_read('work', 'a')",
    diagnosis = "Variable x missing initial read",
    summary = "Added lib_read for work.a",
    evidence_ids = list("finding_01"),
    changed_interfaces = list("work.a"),
    affected_outputs = list("work.out"),
    remaining_uncertainty = list(),
    bundle_helper_patch = NULL
  )

  expect_silent(validate_agent_output(valid_fix_no_patch, "program_fix_v1"))

  valid_fix_with_patch <- valid_fix_no_patch
  valid_fix_with_patch$bundle_helper_patch <- list(
    path = "sas2r-helpers.R",
    content = "custom_helper <- function() {}",
    reason = "Provide fallback helper for date formatting"
  )

  expect_silent(validate_agent_output(valid_fix_with_patch, "program_fix_v1"))

  # Invalid helper patch (missing required field 'reason')
  bad_patch <- valid_fix_with_patch
  bad_patch$bundle_helper_patch$reason <- NULL
  expect_error(validate_agent_output(bad_patch, "program_fix_v1"),
               class = "sas2r_schema_error")
})

test_that("shipped agent YAML specifications define authoritative role invariants", {
  specs <- load_agent_specs()
  expect_true(all(c("translator", "reviewer", "fixer") %in% names(specs)))

  # Translator generates but never certifies
  expect_identical(specs$translator$output_schema, "program_translation_v1")
  expect_match(specs$translator$description, "certif", ignore.case = TRUE)

  # Reviewer is read-only and never claims execution
  expect_identical(specs$reviewer$output_schema, "program_review_v1")
  expect_match(specs$reviewer$description, "read-only|never executes", ignore.case = TRUE)

  # Fixer patches only from cited evidence and never changes original source/inputs
  expect_identical(specs$fixer$output_schema, "program_fix_v1")
  expect_match(specs$fixer$description, "cited|evidence", ignore.case = TRUE)
})

test_that("reviewer prompt context excludes translator reasoning and assumptions", {
  # Verify reviewer prompt template renders source slices and staged R,
  # but does not accept or include translator reasoning fields
  reviewer_prompt_text <- render_prompt("reviewer.md", list(
    unit = "data work.b; set work.a; run;",
    comments = "No comments",
    staged_r = "b <- lib_read('work', 'a')",
    context = "lineage: work.a -> work.b",
    skills = "No skills"
  ))

  expect_match(reviewer_prompt_text, "data work.b; set work.a; run;", fixed = TRUE)
  expect_match(reviewer_prompt_text, "b <- lib_read('work', 'a')", fixed = TRUE)
  expect_no_match(reviewer_prompt_text, "translator reasoning", ignore.case = TRUE)
  expect_no_match(reviewer_prompt_text, "{{uncertainty}}", fixed = TRUE)
  expect_no_match(reviewer_prompt_text, "{{assumptions}}", fixed = TRUE)
  expect_no_match(reviewer_prompt_text, "{{claim}}", fixed = TRUE)
})

test_that("prompt and skill binding hashes cover YAML, prompt template, schema, and skills", {
  h_trans <- worker_prompt_hash("translator")
  expect_type(h_trans, "character")
  expect_identical(nchar(h_trans), 64L)

  h_rev <- worker_prompt_hash("reviewer")
  expect_type(h_rev, "character")
  expect_identical(nchar(h_rev), 64L)
  expect_false(identical(h_trans, h_rev))

  h_fix <- worker_prompt_hash("fixer")
  expect_type(h_fix, "character")
  expect_identical(nchar(h_fix), 64L)
  expect_false(identical(h_rev, h_fix))

  # Skill hash computation
  empty_skill_hash <- worker_skill_hash(list())
  expect_identical(nchar(empty_skill_hash), 64L)

  dummy_skills <- list(
    list(
      skill_id = "sas-missing-sort-semantics",
      version = 1L,
      content_hash = paste(rep("a", 64), collapse = ""),
      body = "skill body content"
    )
  )
  skill_hash <- worker_skill_hash(dummy_skills)
  expect_false(identical(empty_skill_hash, skill_hash))

  # Combined binding hash
  b1 <- worker_binding_hash("translator", skills = list())
  b2 <- worker_binding_hash("translator", skills = dummy_skills)
  expect_false(identical(b1, b2))
  expect_identical(nchar(b1), 64L)
})

test_that("runner audit entry and ledger record include role, component_id, revision_id, round, attempt_id, prompt_hash, skill_hash", {
  dir <- withr::local_tempdir()
  spec <- list(
    name = "translator",
    prompt = "translator.md",
    tools = list(),
    tool_call_limit = 1L,
    retry_limit = 1L,
    temperature = 0,
    output_schema = "program_translation_v1",
    on_budget_exhausted = "downgrade"
  )

  good_trans <- list(
    type = "final",
    data = list(
      r_code = "x <- 1",
      summary = "assign x",
      parameters = list(),
      defaults = structure(list(), names = character(0)),
      reads = list(),
      writes = list(),
      side_effects = list(),
      helper_use = list(),
      discovered_dependencies = list(),
      suspected_dependencies = list(),
      affected_outputs = list(),
      uncertainty = list()
    )
  )

  budget <- new_usage_budget()
  audit_ctx <- list(
    role = "translator",
    component_id = "prog_01",
    revision_id = "rev_abc123",
    round = 1L,
    attempt_id = "attempt_01",
    prompt_hash = worker_prompt_hash("translator"),
    skill_hash = worker_skill_hash(list())
  )

  result <- run_agent(
    spec, mock_llm(list(good_trans)), tools = list(),
    user_content = "unit", log_dir = dir, usage_budget = budget,
    audit_context = audit_ctx
  )

  expect_identical(result$status, "ok")

  # Check llm_log.jsonl
  log_lines <- readLines(file.path(dir, "llm_log.jsonl"), warn = FALSE)
  expect_true(length(log_lines) >= 1L)
  entry <- jsonlite::fromJSON(log_lines[[1]], simplifyVector = FALSE)

  expect_identical(entry$role, "translator")
  expect_identical(entry$component_id, "prog_01")
  expect_identical(entry$revision_id, "rev_abc123")
  expect_identical(as.integer(entry$round), 1L)
  expect_identical(entry$attempt_id, "attempt_01")
  expect_identical(entry$prompt_hash, audit_ctx$prompt_hash)
  expect_identical(entry$skill_hash, audit_ctx$skill_hash)

  # Check usage budget records
  records <- budget$records
  expect_true(length(records) >= 1L)
  first_rec <- records[[1]]
  expect_identical(first_rec$role, "translator")
  expect_identical(first_rec$component_id, "prog_01")
  expect_identical(first_rec$revision_id, "rev_abc123")
  expect_identical(as.integer(first_rec$round), 1L)
  expect_identical(first_rec$attempt_id, "attempt_01")
  expect_identical(first_rec$prompt_hash, audit_ctx$prompt_hash)
  expect_identical(first_rec$skill_hash, audit_ctx$skill_hash)
})
