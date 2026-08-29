generation_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(c(
    "%macro calc_risk(in_ds=, out_ds=, mult=1.5);",
    "data &out_ds; set &in_ds; risk = score * &mult; run;",
    "%mend;"
  ), file.path(dir, "macro_calc.sas"))

  writeLines(c(
    "data work.raw; score = 10; run;",
    "%calc_risk(in_ds=work.raw, out_ds=work.scored, mult=2.0);",
    "data adam.adsl; set work.scored; run;"
  ), file.path(dir, "driver.sas"))

  project <- sas_project(dir)
  paths <- init_migration_paths(withr::local_tempdir(.local_envir = parent.frame()))
  baseline <- sas_transpile(project, paths$root)
  outputs <- infer_output_contracts(project)
  graph <- build_dependency_graph(project, output_contracts = outputs)
  schedule <- stable_dependency_schedule(graph)

  needs_agent <- vapply(schedule$component_id, function(cid) {
    comp_units <- baseline$manifest[
      tools::file_path_sans_ext(basename(baseline$manifest$file)) == cid |
        tools::file_path_sans_ext(basename(baseline$manifest$staged_file)) == cid, ,
      drop = FALSE
    ]
    any(comp_units$tier == "stub" & !(comp_units$reason %in% "include_site_not_emitted"))
  }, logical(1))

  list(
    dir = dir,
    project = project,
    paths = paths,
    baseline = baseline,
    outputs = outputs,
    graph = graph,
    schedule = schedule,
    known_components = schedule$component_id,
    needs_agent = needs_agent
  )
}

recording_translation_llm <- function(handler, budget = new_usage_budget()) {
  captured_requests <- list()
  llm <- new_llm(function(request, audit_context = list()) {
    captured_requests[[length(captured_requests) + 1L]] <<- request
    comp_id <- audit_context$component_id %||% request$component_id %||% "unknown"
    req_context <- utils::modifyList(audit_context, list(
      request = request,
      component_id = comp_id,
      resolved_dependencies = audit_context$resolved_dependencies %||% character()
    ))
    resp <- handler(comp_id, req_context)
    normalize_provider_response(resp, request = request, provider = "mock")
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))
  llm$requests <- function() captured_requests
  llm$budget <- budget
  llm
}

test_that("generation follows the stable dependency schedule", {
  fx <- generation_fixture()
  calls <- character()
  llm <- recording_translation_llm(function(component_id, request) {
    calls <<- c(calls, component_id)
    expect_true(all(request$resolved_dependencies %in% fx$known_components))
    valid_program_translation_response(
      code = sprintf("%s_fn <- function() { 1 }", component_id),
      summary = sprintf("translated %s", component_id)
    )
  })

  generated <- generate_program_revisions(
    fx$project, fx$baseline, fx$graph, fx$schedule, fx$outputs,
    llm = llm, paths = fx$paths
  )
  expect_identical(calls, fx$schedule$component_id[fx$needs_agent])
  expect_setequal(generated$component_id, fx$schedule$component_id)
  expect_true(all(file.exists(generated$r_path)))
  expect_true(all(file.exists(generated$contract_path)))
})

test_that("behavioral contract aggregates deterministic facts and translator output", {
  fx <- generation_fixture()
  llm <- recording_translation_llm(function(component_id, request) {
    valid_program_translation_response(
      code = "calc_risk <- function(in_ds = '', out_ds = '', mult = 1.5) {\n  df <- lib_read('work', in_ds)\n  df$risk <- df$score * mult\n  lib_write(df, 'work', out_ds)\n}",
      summary = "translated macro calc_risk",
      parameters = list(
        list(name = "in_ds", type = "character", required = TRUE, default = NULL),
        list(name = "out_ds", type = "character", required = TRUE, default = NULL),
        list(name = "mult", type = "numeric", required = FALSE, default = 1.5)
      ),
      defaults = list(mult = 1.5),
      reads = "work.raw",
      writes = "work.scored",
      side_effects = "dataset_write",
      helper_use = c("lib_read", "lib_write"),
      suspected_dependencies = "upstream_lookup",
      affected_outputs = "adam.adsl"
    )
  })

  generated <- generate_program_revisions(
    fx$project, fx$baseline, fx$graph, fx$schedule, fx$outputs,
    llm = llm, paths = fx$paths
  )

  macro_row <- generated[generated$component_id == "macro_calc", ]
  expect_identical(nrow(macro_row), 1L)

  contract <- macro_row$contract[[1L]]
  expect_identical(contract$schema_version, "1")
  expect_identical(contract$component_id, "macro_calc")
  expect_true(all(c("lib_read", "lib_write") %in% contract$helper_use))
  expect_true("upstream_lookup" %in% contract$suspected_dependencies)
  expect_true("adam.adsl" %in% contract$affected_outputs)

  # Check binding hashes
  expect_true(!is.null(contract$binding$source_hash) && nzchar(contract$binding$source_hash))
  expect_true(!is.null(contract$binding$r_hash) && nzchar(contract$binding$r_hash))
  expect_true(!is.null(contract$binding$helper_hash) && nzchar(contract$binding$helper_hash))
  expect_true(!is.null(contract$binding$prompt_skill_hash) && nzchar(contract$binding$prompt_skill_hash))
  expect_true(!is.null(contract$binding$dependency_closure_hash) && nzchar(contract$binding$dependency_closure_hash))

  # Check JSON roundtrip
  loaded_json <- jsonlite::fromJSON(macro_row$contract_path[[1L]], simplifyVector = FALSE)
  expect_identical(loaded_json$component_id, "macro_calc")
  expect_identical(loaded_json$binding$source_hash, contract$binding$source_hash)
})

test_that("generate_program_revision produces single component revision", {
  fx <- generation_fixture()
  llm <- recording_translation_llm(function(component_id, request) {
    valid_program_translation_response(
      code = "calc_risk <- function(in_ds = '', out_ds = '', mult = 1.5) { 1 }",
      summary = "single revision"
    )
  })

  rev <- generate_program_revision(
    component_id = "macro_calc",
    project = fx$project,
    baseline = fx$baseline,
    graph = fx$graph,
    schedule = fx$schedule,
    outputs = fx$outputs,
    llm = llm,
    paths = fx$paths,
    revision_id = "r1"
  )

  expect_identical(rev$component_id, "macro_calc")
  expect_identical(rev$revision_id, "r1")
  expect_true(file.exists(rev$r_path))
  expect_true(file.exists(rev$contract_path))
  expect_identical(rev$status, "ok")
  expect_true(isTRUE(rev$checks$pass))
})

test_that("check_program_revision verifies parse, lint, helpers, and interfaces", {
  temp_dir <- withr::local_tempdir()

  # Valid code
  good_r <- file.path(temp_dir, "good.R")
  writeLines(c(
    "calc <- function(a, b) {",
    "  df <- lib_read('work', 'ds')",
    "  a + b",
    "}"
  ), good_r)
  good_contract <- new_behavioral_contract(
    component_id = "calc",
    parameters = list(list(name = "a"), list(name = "b")),
    helper_use = "lib_read"
  )
  chk_good <- check_program_revision(good_r, good_contract)
  expect_true(chk_good$pass)
  expect_length(chk_good$errors, 0L)

  # Parse error
  bad_parse_r <- file.path(temp_dir, "bad_parse.R")
  writeLines("calc <- function(a, b) { a +", bad_parse_r)
  chk_parse <- check_program_revision(bad_parse_r)
  expect_false(chk_parse$pass)
  expect_true(any(grepl("parse_error", chk_parse$errors)))

  # Banned function lint error
  bad_lint_r <- file.path(temp_dir, "bad_lint.R")
  writeLines("calc <- function(a) { system('ls') }", bad_lint_r)
  chk_lint <- check_program_revision(bad_lint_r)
  expect_false(chk_lint$pass)
  expect_true(any(grepl("lint_error", chk_lint$errors)))

  # Unknown helper error
  bad_helper_contract <- new_behavioral_contract(
    component_id = "calc",
    helper_use = "completely_fictitious_helper_xyz"
  )
  chk_helper <- check_program_revision(good_r, bad_helper_contract)
  expect_false(chk_helper$pass)
  expect_true(any(grepl("unknown_helper", chk_helper$errors)))

  # Parameter mismatch error
  mismatch_contract <- new_behavioral_contract(
    component_id = "calc",
    parameters = list(list(name = "x"), list(name = "y"))
  )
  chk_mismatch <- check_program_revision(good_r, mismatch_contract)
  expect_false(chk_mismatch$pass)
  expect_true(any(grepl("parameter_mismatch", chk_mismatch$errors)))
})

test_that("suspected dependencies remain review findings and do not reorder schedule", {
  fx <- generation_fixture()
  llm <- recording_translation_llm(function(component_id, request) {
    valid_program_translation_response(
      code = sprintf("%s_fn <- function() { 1 }", component_id),
      summary = "translated with suspected dependency",
      suspected_dependencies = "future_component_maybe"
    )
  })

  generated <- generate_program_revisions(
    fx$project, fx$baseline, fx$graph, fx$schedule, fx$outputs,
    llm = llm, paths = fx$paths
  )

  # Check that suspected dependencies are recorded in contract
  macro_contract <- generated$contract[[which(generated$component_id == "macro_calc")]]
  expect_true("future_component_maybe" %in% macro_contract$suspected_dependencies)

  # The schedule order should NOT have changed or failed due to unconfirmed suspected dependency
  expect_identical(generated$component_id, fx$schedule$component_id)
})

test_that("mechanical failure triggers bounded retry and succeeds on valid correction", {
  fx <- generation_fixture()
  attempt <- 0L
  llm <- recording_translation_llm(function(component_id, request) {
    attempt <<- attempt + 1L
    if (attempt == 1L) {
      # First attempt has a parse syntax error
      valid_program_translation_response(
        code = "calc_risk <- function(in_ds, out_ds) { a + ",
        summary = "broken parse"
      )
    } else {
      # Corrected code
      valid_program_translation_response(
        code = "calc_risk <- function(in_ds = '', out_ds = '', mult = 1.5) { 1 }",
        summary = "fixed parse"
      )
    }
  })

  rev <- generate_program_revision(
    component_id = "macro_calc",
    project = fx$project,
    baseline = fx$baseline,
    graph = fx$graph,
    schedule = fx$schedule,
    outputs = fx$outputs,
    llm = llm,
    paths = fx$paths,
    revision_id = "r1"
  )

  expect_true(attempt >= 2L)
  expect_identical(rev$status, "ok")
  expect_true(isTRUE(rev$checks$pass))
})


test_that("an unreachable agent is recorded on the revision, never silently swallowed", {
  withr::local_options(sas2r.agent_backoff_base = 0)
  fx <- generation_fixture()
  llm <- new_llm(function(request) {
    stop(structure(list(message = "rate limited", status_code = 429L),
                   class = c("sas2r_llm_rate_limit", "error", "condition")))
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))

  generated <- generate_program_revisions(
    fx$project, fx$baseline, fx$graph, fx$schedule, fx$outputs,
    llm = llm, paths = fx$paths
  )

  expect_true("agent_status" %in% names(generated))
  agent_cids <- fx$schedule$component_id[fx$needs_agent]
  degraded <- generated$agent_status[generated$component_id %in% agent_cids]
  expect_true(all(degraded == "rate_limited"))
  # Components the deterministic core translated alone carry no agent status.
  clean <- generated$agent_status[!generated$component_id %in% agent_cids]
  expect_true(all(is.na(clean)))
})

test_that("program-level translator tools can resolve macros from the configured search path", {
  dir <- withr::local_tempdir()
  macro_dir <- file.path(dir, "macros")
  dir.create(macro_dir)
  writeLines(c("%macro dostuff(x);", "  y = &x;", "%mend;"),
             file.path(macro_dir, "dostuff.sas"))
  writeLines(c("data work.w1;", "  set work.w0;", "  retain c 0;", "run;"),
             file.path(dir, "prog.sas"))

  cfg <- list(macro_search_path = "macros")
  proj <- sas_project(dir, config = cfg)
  out <- withr::local_tempdir()
  paths <- init_migration_paths(out)
  baseline <- sas_transpile(proj, paths$root)
  graph <- build_dependency_graph(proj)
  schedule <- stable_dependency_schedule(graph)
  outputs <- infer_output_contracts(proj)

  hits <- list()
  llm <- new_llm(function(request) {
    tool <- NULL
    for (t in request$tools) if (identical(t$name, "find_macro")) tool <- t
    if (!is.null(tool)) hits[[length(hits) + 1L]] <<- tool$call(list(name = "dostuff"))
    normalize_provider_response(
      valid_program_translation_response(code = "x <- 1"),
      request = request, provider = "mock"
    )
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))

  generated <- generate_program_revisions(
    proj, baseline, graph, schedule, outputs,
    llm = llm, paths = paths, config = cfg
  )

  expect_true(length(hits) > 0L)
  expect_null(hits[[1L]]$error)
  expect_true(length(hits[[1L]]$hits) >= 1L)
})
