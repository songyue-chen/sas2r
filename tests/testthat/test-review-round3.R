test_that("input manifests exclude generated files before inspecting them", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  out <- file.path(root, "out")
  dir.create(out)
  writeLines("source", file.path(root, "input.txt"))
  transient <- file.path(out, "worker.reply")
  writeLines("transient", transient)
  info <- base::file.info
  local_mocked_bindings(file.info = function(...) {
    paths <- unlist(list(...))
    if (transient %in% paths) stop("excluded worker file must not be inspected")
    info(...)
  }, .package = "base")
  p <- list(libraries = list(raw = root), input_manifest_exclude = out)
  expect_named(input_hash_manifest(p, metadata_only = TRUE), "raw/input.txt")
  expect_named(input_hash_manifest(p), "raw/input.txt")
})

test_that("an input removed between listing and stat is an absent manifest entry", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  input <- file.path(root, "input.txt")
  writeLines("source", input)
  p <- list(libraries = list(raw = root))
  before <- input_hash_manifest(p)
  info <- base::file.info
  local_mocked_bindings(file.info = function(...) {
    if (input %in% unlist(list(...))) unlink(input)
    info(...)
  }, .package = "base")
  after <- input_hash_manifest(p, metadata_only = TRUE)
  expect_length(after, 0L)
  expect_false(identical(input_hash_manifest(p), before))
})

test_that("all-optional outputs cannot report required validation passed", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  saveRDS(data.frame(id = 1:2), file.path(root, "data", "cls.rds"))
  writeLines('proc export data=raw.cls outfile="out/class.csv" dbms=csv replace; run;', file.path(root, "p.sas"))
  code <- 'x <- lib_read("raw", "cls")\ndir.create("out", showWarnings = FALSE)\nutils::write.csv(x, "out/class.csv", row.names = FALSE)'
  llm <- new_llm(function(request, audit_context = list()) {
    response <- if (identical(audit_context$agent, "translator"))
      valid_program_translation_response(code) else valid_program_review_response()
    attr(response, "cost_usd") <- 0
    normalize_provider_response(response, request, "mock")
  }, provider = "mock", capabilities = llm_capabilities(structured_output = "native", tool_calling = "native", tools_with_structured_output = "supported"))
  result <- sas_translate(root, llm = llm, config = list(
    libraries = list(raw = list(path = file.path(root, "data"), engine = "rds")),
    outputs = list(optional = "out/class.csv")))
  expect_identical(result$status, "needs_review")
  report <- jsonlite::fromJSON(result$report_json_path, simplifyVector = FALSE)
  expect_identical(report$outcome$stages[["Required validation"]], "REVIEW REQUIRED")
  expect_false(result$output_assessments[[1]]$required)
  expect_identical(result$output_assessments[[1]]$status, "unassessed_file")
  # A passing optional target still does not establish a required output contract.
  assessment <- list(execution = list(completed = TRUE, passed = TRUE),
    targets = list(list(required = FALSE, passed = TRUE)))
  expect_identical(derive_bundle_status(assessment), "needs_review")
  assessment$targets[[1]]$required <- TRUE
  expect_identical(derive_bundle_status(assessment), "migration_ready")
})

test_that("fixer dependency source stays in task data and policy stays in system", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  marker <- "CONSUMER_CODE_ROUND3"
  fx$state$selected_revisions$p02$r_code <- paste("#", marker, "\n", fx$fixed$p02)
  fix_program_revision(fx$state$selected_revisions$p01,
    checks = list(check_id = "shape", errors = "producer/consumer mismatch"),
    llm = fx$state$fixer_llm, mode = "bundle", project = fx$state$project,
    selected_revisions = fx$state$selected_revisions, paths = fx$state$paths)
  messages <- fx$state$fixer_llm$requests()[[1]]$messages
  for (source in c(marker, "data work.out2; set work.out1")) {
    expect_false(grepl(source, messages[[1]]$content, fixed = TRUE))
    expect_match(messages[[2]]$content, source, fixed = TRUE)
  }
  expect_match(messages[[1]]$content, "call by name; do not redefine", fixed = TRUE)
})

test_that("lineage guidance does not mask a failed source repair", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  saveRDS(data.frame(id = 1:2), file.path(root, "data", "input.rds"))
  writeLines('%let name=out; data work.&name; set raw.input; run;', file.path(root, "p.sas"))
  code <- 'x <- lib_read("raw", "input"); lib_write(x, "work", "out")'
  llm <- new_llm(function(request, audit_context = list()) {
    role <- audit_context$agent
    if (identical(role, "fixer")) stop("fixture repair failed")
    response <- if (identical(role, "translator"))
      valid_program_translation_response(code, writes = "work.out", reads = "raw.input") else
      material_review_response(affected_outputs = "work.out")
    attr(response, "cost_usd") <- 0
    normalize_provider_response(response, request, "mock")
  }, provider = "mock", capabilities = llm_capabilities(structured_output = "native", tool_calling = "native", tools_with_structured_output = "supported"))
  result <- sas_translate(root, llm = llm, outputs = "work.out",
    max_program_repair_rounds = 1L, max_bundle_repair_rounds = 1L,
    config = list(libraries = list(raw = list(path = file.path(root, "data"), engine = "rds"))))
  expect_identical(result$status, "needs_review")
  expect_match(result$status_reason, "repair_failed", fixed = TRUE)
  expect_match(result$status_reason, "unknown_output_lineage: work.out", fixed = TRUE)
  report <- jsonlite::fromJSON(result$report_json_path, simplifyVector = FALSE)
  expect_identical(report$outcome$reason, result$status_reason)
})

test_that("abbreviated include executes without a spurious missing macro", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  saveRDS(data.frame(id = 1:2, value = 3:4), file.path(root, "data", "input.rds"))
  writeLines("data work.a; set raw.input; run;", file.path(root, "included.sas"))
  main <- file.path(root, "main.sas")
  writeLines(c('%inc "included.sas";', 'data work.b; set work.a; run;'), main)
  cfg <- list(libraries = list(raw = list(path = file.path(root, "data"), engine = "rds")))
  preflight <- sas_preflight(main, config = cfg)
  expect_false(any(vapply(preflight$readiness$warnings,
    function(finding) identical(finding$kind, "source_dependency"), logical(1))))
  result <- sas_translate(main, config = cfg, outputs = "work.b", usage_limits = list(max_calls = 0))
  report <- jsonlite::fromJSON(result$report_json_path, simplifyVector = FALSE)
  expect_match(report$outcome$stages[["Bundle execution"]], "1 ran to completion", fixed = TRUE)
  expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "b.rds"))$value, 3:4)
})

test_that("legacy ellmer accepts locally enforced limits and names unsupported ones", {
  local_mocked_bindings(ellmer_has_request_callbacks = function() FALSE)
  llm <- mock_llm(list())
  attr(llm, "is_ellmer") <- TRUE
  for (limit in c("max_retries", "max_tool_calls", "max_wall_time")) {
    budget <- do.call(new_usage_budget, stats::setNames(list(2), limit))
    expect_no_error(validate_ellmer_budget(llm, budget))
  }
  for (limit in c("max_calls", "max_request_bytes", "max_request_chars", "max_input_tokens", "max_output_tokens")) {
    budget <- do.call(new_usage_budget, stats::setNames(list(2), limit))
    expect_error(validate_ellmer_budget(llm, budget), limit, class = "sas2r_budget_unmeterable")
  }
  expect_error(validate_ellmer_budget(llm, new_usage_budget(mode = "strict")),
    "strict", class = "sas2r_budget_unmeterable")
})
