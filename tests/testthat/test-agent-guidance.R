test_that("helper bookkeeping does not change valid R or permit invented calls", {
  code <- 'local_helper <- function(x) x\nx <- dplyr::filter(local_helper(data), flag)\nupstream(x)'
  declared <- c("dplyr::filter", "local_helper", "local local_helper function", "upstream")
  expect_length(reconcile_helper_use(code, declared, "upstream"), 0L)
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(code, path)
  expect_true(check_program_revision(path,
    new_behavioral_contract("example", helper_use = declared, dependency_functions = "upstream"))$pass)
  expect_identical(paste(readLines(path), collapse = "\n"), code)
  bad <- 'invented(1)'
  expect_identical(reconcile_helper_use(bad, "invented"), "invented")
  expect_equal(reconcile_helper_use('sas2r::sas_sum(1)', 'sas2r::sas_sum'), "sas_sum")
  expect_false(any(lint_r_code('f <- function(callback, x) callback(x)')$level == "error"))
})

test_that("ordinary dynamic calls do not turn unrelated prose into missing helpers", {
  code <- 'y <- data.frame(x = 1); do.call(rbind, list(y))'
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(code, path)
  declared <- c("local sas_missing helper", "dplyr::filter")
  expect_length(reconcile_helper_use(code, declared), 0)
  expect_true(check_program_revision(path,
    new_behavioral_contract("example", helper_use = declared))$pass)
  expect_identical(paste(readLines(path), collapse = "\n"), code)
  # R permits nonsyntactic targets. Preserve actual symbol/literal references,
  # including simple aliases, rather than assuming spaces imply prose.
  for (use in c('get("local helper")(1)', 'do.call("local helper", list(1))',
                '`local helper`(1)', 'target <- "local helper"; do.call(target, list(1))')) {
    expect_identical(reconcile_helper_use(use, "local helper"), "local helper")
  }
  env <- new.env()
  env[["local helper"]] <- identity
  expect_identical(do.call("local helper", list(1), envir = env), 1)
})

test_that("configured namespaces share one policy for metadata, facts and lint", {
  configured <- " base, dplyr, tidyr, haven, stats, utils, stringr, stringr "
  expected <- c("base", "dplyr", "tidyr", "haven", "stats", "utils", "stringr")
  expect_identical(normalize_package_allowlist(configured), expected)
  expect_identical(normalize_package_allowlist(as.list(expected)), expected)
  expect_identical(normalize_package_allowlist("stringr"), "stringr")
  expect_identical(agent_package_facts(configured)$allowed, expected)
  code <- 'out <- stringr::str_trim(" x ")'
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(code, path)
  contract <- new_behavioral_contract("example", helper_use = "stringr::str_trim")
  checked <- check_program_revision(path, contract, allowlist = configured,
    helper_patch = list(content = 'trim <- function(x) stringr::str_trim(x)'))
  expect_true(checked$pass)
  expect_false(any(checked$lint$kind == "disallowed_namespace"))
  expect_length(reconcile_helper_use(code, contract$helper_use, allowlist = configured), 0)
  outside <- withr::local_tempfile(fileext = ".R")
  writeLines('out <- jsonlite::toJSON(list(x = 1))', outside)
  default <- check_program_revision(outside,
    new_behavioral_contract("example", helper_use = "jsonlite::toJSON"))
  expect_false(default$pass)
  expect_true(any(default$lint$kind == "disallowed_namespace"))
})

test_that("macro text defaults are known without guessing expansion or omission", {
  contract <- parse_macro_contract("example", 'width=75px, text=two words, items=(1 2), expr=a+b, empty=, n=-2.5, dyn=&value, call=%sysfunc(today()), pct="100%"')
  expect_equal(contract$parameters$default_status,
    c(rep("known", 6), "unresolved", "unresolved", "known"))
  expect_identical(contract$parameters$r_default[[1]], "75px")
  expect_identical(contract$parameters$r_default[[4]], "a+b")
  for (value in c("12%dynamic", "a /*comment*/ b", '"unterminated')) {
    expect_identical(normalize_macro_default(value)$default_status, "unresolved")
  }
  dynamic <- parse_macro_contract("example", "value=&dynamic")
  expect_true(validate_macro_contract('example <- function(value) { if (missing(value)) stop("unresolved source default"); value }', dynamic)$pass)
  bad <- validate_macro_contract('example <- function(width = "wrong") width', parse_macro_contract("example", "width=75px"))
  expect_false(bad$pass)
})

test_that("existence shares dataset lookup but does not read or require rows", {
  input <- withr::local_tempdir()
  output <- withr::local_tempdir()
  .sas2r_registry <- list(raw = list(read_path = input, write_path = output))
  expect_false(lib_exists("raw", "absent"))
  saveRDS(data.frame(x = numeric()), file.path(input, "empty.rds"))
  expect_true(lib_exists("RAW", "empty"))
  expect_equal(nrow(lib_read("raw", "empty")), 0L)
  writeLines("not an RDS", file.path(output, "broken.rds"))
  expect_true(lib_exists("raw", "broken"))
  expect_error(lib_read("raw", "broken"))
  dir.create(file.path(output, "directory.rds"))
  expect_false(lib_exists("raw", "directory"))
  for (ext in c("sas7bdat", "xpt")) {
    file.create(file.path(input, paste0("present.", ext)))
    expect_true(lib_exists("raw", "present"))
  }
  saveRDS(data.frame(x = 1), file.path(input, "both.rds"))
  saveRDS(data.frame(x = 2), file.path(output, "both.rds"))
  expect_equal(lib_read("raw", "both")$x, 2)
  expect_error(lib_exists("unknown", "empty"), class = "sas2r_unknown_libref")
  expect_error(lib_exists("raw.empty"), "two separate strings")
  expect_error(lib_exists("raw", "../empty"), class = "sas2r_libref_member_error")
})

test_that("helper patches cannot bypass mechanical checks or inherit name-based privilege", {
  stock <- paste(readLines(system.file("templates", "sas2r-helpers.R", package = "sas2r")), collapse = "\n")
  expect_false(any(lint_helper_patch(stock)$level == "error"))
  expect_false(any(lint_helper_patch(paste(stock, 'extra <- function(x) sas_sum(x, 1)'))$level == "error"))
  for (patch in c('extra <- function() system("never executed")',
                  'lib_delete <- function(x) file.remove(x)', 'bad <- function(')) {
    expect_true(any(lint_helper_patch(patch)$level == "error"))
  }
  fx <- review_fix_fixture()
  fixer <- recording_fixer(function(req) valid_program_fix_response(code = "target <- source",
    bundle_helper_patch = list(path = "sas2r-helpers.R", content = 'bad <- function() system("never executed")', reason = "mock patch")))
  revision <- fix_program_revision(fx$revision, smoke = fx$failed_smoke, llm = fixer, paths = fx$paths)
  expect_false(revision$checks$pass)
  expect_true(any(grepl("helper bad.*system", revision$checks$errors)))
  expect_length(fixer$requests(), 2L)
  expect_equal(paste(readLines(fx$revision$r_path), collapse = "\n"), fx$revision$r_code)
})

test_that("direct I/O notices are advisory, bounded and do not accuse legitimate code", {
  code <- 'x <- readRDS(path); y <- readRDS(other); exists <- file.exists(path)'
  lint <- lint_r_code(code)
  expect_equal(sum(lint$kind == "direct_io"), 2L)
  expect_false(any(lint$level == "error"))
  expect_false(any(lint_r_code('readLines(textConnection("text"))')$kind == "direct_io"))
  expect_true(any(lint_r_code('haven::read_xpt(path)')$kind == "direct_io"))
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(code, path)
  expect_true(check_program_revision(path)$pass)
})

guidance_project <- function(envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  dir.create(file.path(root, "programs"))
  dir.create(file.path(root, "macros"))
  writeLines(c("macros:", "  search_path: [macros]"), file.path(root, "_sas2r.yml"))
  writeLines("%macro check(); %put 1; %mend;", file.path(root, "macros", "check.sas"))
  writeLines("%check();", file.path(root, "programs", "main.sas"))
  project <- sas_project(file.path(root, "programs"))
  macro <- component_macro_contract(project, project$graph, "macro__check")
  list(project = project, revisions = list(macro__check = list(revision_id = "r2",
    r_code = "check <- function() 1L", contract = list(macro_contract = macro))))
}

test_that("all actual role requests receive the same source context without reference answers", {
  fx <- guidance_project()
  state <- new_migration_state(fx$project, withr::local_tempdir())
  state$selected_revisions <- fx$revisions
  state$config$outputs$references <- list(answer = "FORBIDDEN_REFERENCE_PATH")
  state$config$comparison_rules <- list(target_count = "FORBIDDEN_TARGET_COUNT")
  state$config$allowlist <- "base, dplyr, tidyr, haven, stats, utils, stringr"
  code <- 'check(); cleaned <- stringr::str_trim(" x ")'
  translator <- recording_reviewer(function(req) valid_program_translation_response(code,
    helper_use = list("stringr::str_trim")))
  rev <- generate_program_revision("main", state$project, state$baseline, state$graph,
    state$schedule, state$output_contracts, llm = translator, paths = state$paths,
    selected_revisions = fx$revisions, config = state$config)
  reviewer <- recording_reviewer(function(req) valid_program_review_response())
  review_program_revision(rev, list(project = state$project,
    selected_revisions = fx$revisions, config = state$config), reviewer, paths = state$paths)
  expect_true(rev$checks$pass)
  expect_length(rev$contract$helper_use, 0)
  fixer <- recording_fixer(function(req) valid_program_fix_response(code))
  fixed <- fix_program_revision(rev, checks = list(check_id = "synthetic", errors = "example"),
    llm = fixer, paths = state$paths, project = state$project, config = state$config,
    selected_revisions = fx$revisions)
  expect_true(fixed$checks$pass)
  expect_false(any(fixed$checks$lint$kind == "disallowed_namespace"))
  guidance <- build_agent_guidance(state$project, "main", rev$contract, fx$revisions, config = state$config)
  for (llm in list(translator, reviewer, fixer)) {
    expect_gt(length(llm$requests()), 0)
    request <- llm$requests()[[1]]
    messages <- paste(vapply(request$messages, `[[`, "", "content"), collapse = "\n")
    expect_match(messages, guidance$text, fixed = TRUE)
    expect_match(messages, agent_guidance_policy(), fixed = TRUE)
    expect_match(messages, "check <- function() 1L", fixed = TRUE)
    expect_match(messages, "Allowlisted packages", fixed = TRUE)
    expect_match(messages, "haven [0-9]")
    expect_match(messages, "stringr [0-9]")
    expect_identical(grepl("cite its current context_fact_id", messages, fixed = TRUE),
      identical(llm, reviewer))
    # The style preference is for the roles that write code; the reviewer judges semantics only.
    expect_identical(grepl("tidyverse first", messages, fixed = TRUE), !identical(llm, reviewer))
    expect_false(grepl("FORBIDDEN_REFERENCE_PATH|FORBIDDEN_TARGET_COUNT", messages))
    expect_false("read_comparison_report" %in% names(request$tools))
    expect_identical(request$tools$read_dependency_context$call(list(
      component_id = "macro__check", language = "r"))$code, "check <- function() 1L")
  }
  expect_false("get_macro_source" %in% names(reviewer$requests()[[1]]$tools))
})

test_that("unrelated agent tasks do not receive translation policy", {
  spec <- load_agent_specs()$reviewer
  spec$name <- "output_pairing"
  llm <- recording_reviewer(function(req) valid_program_review_response())
  result <- run_agent(spec, llm, list(), "unrelated task", log_dir = withr::local_tempdir())
  expect_identical(result$status, "ok")
  messages <- paste(vapply(llm$requests()[[1]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_false(grepl(agent_guidance_policy(), messages, fixed = TRUE))
})

test_that("scoped missing-context facts do not excuse unrelated source defects", {
  fx <- guidance_project()
  short <- build_agent_guidance(fx$project, "main", selected_revisions = fx$revisions, body_limit = 8L)
  fact <- short$facts[[1]]
  make <- function(category, evidence, id = fact$id) list(category = category,
    context_fact_id = id, severity = "material", sas_evidence = "%check()",
    r_evidence = evidence, unresolved_dependencies = list())
  code <- "answer <- check(); x <- dplyr::filter(x, flag)"
  findings <- classify_review_findings(list(make("missing_context", "check()"),
    make("missing_context", "dplyr::filter(x, flag)"),
    make("translation_defect", "check()"), make("unsupported_capability", "check()"),
    make("missing_context", "check()", "stale")), short, code, list())
  expect_equal(vapply(findings, `[[`, "", "repair_disposition"), c("awaiting_context", rep("unverified", 4)))
  review <- list(verdict = "repair_required", findings = findings[1])
  expect_false(program_review_needs_repair(review))
  expect_length(source_grounded_review_findings(review), 0)
  review$findings <- findings
  expect_true(program_review_needs_repair(review))
  expect_length(source_grounded_review_findings(review), 4)
  complete <- build_agent_guidance(fx$project, "main", selected_revisions = fx$revisions)
  expect_false(identical(complete$identity, short$identity))
  resolved <- classify_review_findings(list(make("missing_context", "check()", complete$facts[[1]]$id)), complete, code, list())
  expect_identical(resolved[[1]]$repair_disposition, "unverified")
  changed <- fx$revisions
  changed$macro__check$r_code <- "check <- function() FALSE"
  expect_false(identical(complete$identity,
    build_agent_guidance(fx$project, "main", selected_revisions = changed)$identity))
  old <- classify_review_findings(list(list(r_evidence = "check()")), complete, code, list())
  expect_identical(old[[1]]$category, "unknown")
})

test_that("new review fields work through native and fallback response validation", {
  fx <- review_fix_fixture()
  for (mode in c("native", "fallback")) {
    response <- material_review_response()
    response$data$findings[[1]]$category <- "source_syntax_claim"
    response$data$findings[[1]]$context_fact_id <- ""
    llm <- new_llm(function(request) normalize_provider_response(response, request, provider = "mock"),
      provider = "mock", capabilities = llm_capabilities(structured_output = mode,
        tool_calling = "native", tools_with_structured_output = "supported"))
    review <- review_program_revision(fx$revision, fx$context, llm, paths = fx$paths)
    expect_identical(review$verdict, "repair_required")
    expect_identical(review$status, "ok")
    expect_false(program_review_needs_repair(review))
    response$data$findings[[1]]$category <- "invalid_category"
    invalid <- review_program_revision(fx$revision, fx$context, llm, paths = fx$paths)
    expect_identical(invalid$verdict, "review_unavailable")
  }
})

test_that("metadata-only mistakes need no fixer and syntax allegations do not veto runtime repairs", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  fx$state$selected_revisions$p01$contract$helper_use <- c("dplyr::mutate", "stale local function")
  unchanged <- fx$state$selected_revisions$p01$r_code
  state <- process_program_component(fx$state, "p01", execute = TRUE)
  expect_length(fx$state$fixer_llm$requests(), 0)
  expect_identical(state$selected_revisions$p01$r_code, unchanged)
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    response <- material_review_response("alleged source quoting error", "stop('translation fault p01')")
    response$data$findings[[1]]$category <- "source_syntax_claim"
    response
  })
  state <- process_program_component(fx$state, "p01", execute = TRUE)
  expect_length(fx$state$fixer_llm$requests(), 1)
  expect_identical(state$selected_revisions$p01$r_code, fx$fixed$p01)
  expect_identical(component_review_verdict(state$histories$p01), "repair_required")
  messages <- paste(vapply(fx$state$fixer_llm$requests()[[1]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_false(grepl("alleged source quoting error", messages, fixed = TRUE))
})

test_that("dependency notices include value-position aliases and remain advisory", {
  before <- "x <- count_values(x)"
  expect_length(dependency_symbol_notices(before, "f <- count_values; x <- f(x)", "count_values"), 0)
  expect_length(dependency_symbol_notices(before, "x <- length(unique(x))", "count_values"), 1)
  expect_true(grepl("advisory only", dependency_symbol_notices(before, "x <- 1", "count_values")))
})

test_that("candidate byte summaries require matched evidence and remain human-only", {
  record <- list(attempt_id = "one", execution_context = list(sources = list(p = "source-hash"), environment = list(R = "same")),
    execution_order = "p", executed_component_ids = "p", input_hashes_before = list(input = "same"),
    input_hashes_after = list(input = "same"), output_hashes = list("work/a.rds" = "old", "work/b.rds" = "gone"),
    revision_manifest = list(p = list(r_hash = "old-code", affected_outputs = "work.a")))
  candidate <- record
  candidate$attempt_id <- "two"
  candidate$output_hashes <- list("work/a.rds" = "new", "figure.pdf" = "added")
  candidate$revision_manifest$p$r_hash <- "new-code"
  report <- attempt_change_observation(record, candidate, list("work/a.rds" = "work.a"))
  expect_identical(report$status, "comparable")
  expect_equal(vapply(report$changes, `[[`, "", "status"), c("byte_changed", "removed", "added"))
  expect_true(report$changes[[1]]$declared_affected)
  expect_identical(report$changes[[2]]$declared_affected, "unknown")
  for (field in c("execution_context", "executed_component_ids", "input_hashes_before")) {
    other <- candidate
    other[[field]] <- NULL
    expect_identical(attempt_change_observation(record, other)$status, "not_comparable")
  }
  expect_identical(attempt_change_observation(NULL, candidate)$status, "not_comparable")
  fx <- review_fix_fixture()
  fixer <- recording_fixer(function(req) valid_program_fix_response("target <- source"))
  fx$failed_smoke$repair_observations <- list(record_level_difference = "FORBIDDEN_ROW_TARGET", output_changes = report)
  fix_program_revision(fx$revision, smoke = fx$failed_smoke, llm = fixer, paths = fx$paths)
  messages <- paste(vapply(fixer$requests()[[1]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_false(grepl("FORBIDDEN_ROW_TARGET|byte_changed|figure.pdf", messages))
})

test_that("existence and SQL guidance are reachable without a wrong expression mapping", {
  lookup <- TOOL_IMPLS$lookup_rulebook(list())
  rules <- lookup(list(functions = "exist", procs = "sql"))
  expect_identical(rules$semantics$functions$exist$strategy, "lib_exists")
  expect_match(rules$semantics$procs$sql$scope, "excludes missing values", fixed = TRUE)
  expect_false("exist" %in% names(load_rulebook()$functions))
})

test_that("shipped semantic examples have independent counts, delegation and loop outcomes", {
  lines <- strsplit(agent_skill_catalog()[["sas-macro-execution"]]$body, "\n", fixed = TRUE)[[1]]
  starts <- which(lines == "```r")
  ends <- which(lines == "```")
  code <- paste(unlist(lapply(starts, function(i) lines[seq.int(i + 1L, min(ends[ends > i]) - 1L)])), collapse = "\n")
  env <- new.env(parent = baseenv())
  eval(parse(text = code), env)
  cases <- list(c(NA, 1, 1), numeric(), c(NA, NA), c(1, 2, 2), c(haven::tagged_na("a"), 1, 1))
  expect_equal(vapply(cases, env$count_numeric_values, integer(1)), c(1L, 0L, 0L, 2L, 1L))
  seen <- 0L
  out <- env$label_numeric_values(c(NA, 1, 1), function(x) {
    seen <<- seen + 1L
    env$count_numeric_values(x)
  })
  expect_identical(out$count, 1L)
  expect_identical(seen, 1L)
  expect_false(identical(length(unique(c(NA, 1, 1))), out$count))
  expect_identical(env$walk_current_words("bad 1"), "bad")
  expect_identical(env$walk_current_words("1 bad 2"), c("1", "bad"))
  expect_identical(env$walk_current_words("1 2"), c("1", "2"))
  expect_identical(env$walk_current_words(""), character())
  # Numeric magnitude must survive display; stripping integer trailing zeroes
  # is the independent counterexample, not a SAS BEST-format oracle.
  expect_equal(sas_round(10000000000, 1), 10000000000)
  expect_false(identical(as.numeric(sub("0+$", "", "10000000000")), 10000000000))
})

test_that("a parse/eval implementation can still receive a simple supported correction", {
  fx <- review_fix_fixture()
  calls <- 0L
  fixer <- recording_fixer(function(req) {
    calls <<- calls + 1L
    valid_program_fix_response(if (calls == 1L) "x <- eval(parse(text = value))" else "x <- as.numeric(value)")
  })
  fixed <- fix_program_revision(fx$revision, smoke = fx$failed_smoke, llm = fixer, paths = fx$paths)
  expect_true(fixed$checks$pass)
  expect_true(fixed$mechanical_retry$dynamic_code)
  expect_length(fixer$requests(), 2)
  retry <- paste(vapply(fixer$requests()[[2]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_match(retry, "Do not replace banned parse/eval with a handwritten general interpreter", fixed = TRUE)
})

test_that("resumed completed reviews retain cause-specific repair decisions", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  fx$state$reviewer_llm <- recording_reviewer(function(req) {
    response <- material_review_response(sas_evidence = "unsupported syntax allegation", r_evidence = "x")
    response$data$findings[[1]]$category <- "source_syntax_claim"
    response
  })
  fx$state <- process_program_component(fx$state, "p01", execute = FALSE)
  fx$state$resumed_components <- "p01"
  state <- process_program_component(fx$state, "p01", execute = FALSE)
  expect_length(fx$state$fixer_llm$requests(), 0)
  expect_length(fx$state$reviewer_llm$requests(), 1)
  expect_identical(component_review_verdict(state$histories$p01), "repair_required")
})

test_that("direct dependency packets have a total limit as well as body limits", {
  fx <- guidance_project()
  graph <- fx$project$graph
  provider <- graph$nodes[graph$nodes$component_id == "macro__check", ]
  consumer <- graph$nodes$node_id[graph$nodes$component_id == "main"][1]
  edge <- graph$edges[graph$edges$from == provider$node_id[1] & graph$edges$to == consumer, ][1, ]
  selected <- list()
  for (i in seq_len(20)) {
    node <- provider
    node$node_id <- paste0("dep", i)
    node$component_id <- paste0("macro__dep", i)
    graph$nodes <- rbind(graph$nodes, node)
    link <- edge
    link$from <- node$node_id
    graph$edges <- rbind(graph$edges, link)
    selected[[node$component_id]] <- list(r_code = paste(rep("x <- 1", 2000), collapse = "\n"))
  }
  packet <- build_agent_guidance(fx$project, "main", selected_revisions = selected, graph = graph)
  expect_lte(nchar(packet$text), 24000L)
  expect_match(packet$text, "truncated|omitted")
})

test_that("unknown declarations used indirectly remain unresolved", {
  for (code in c('f <- invented; f(1)', 'get("invented")(1)', 'do.call(target, list(1))')) {
    expect_identical(reconcile_helper_use(code, "invented"), "invented")
  }
  expect_length(reconcile_helper_use('x <- "sas_sum(x)"', "sas_sum"), 0)
})

test_that("a missing recorded hash remains unknown rather than a byte change", {
  rec <- list(attempt_id = "one", execution_context = list(sources = list(p = "hash"), environment = list(R = "same")),
    execution_order = "p", executed_component_ids = "p", input_hashes_before = list(), input_hashes_after = list(),
    output_hashes = list("work/a.rds" = "hash"))
  other <- rec
  other$output_hashes["work/a.rds"] <- list(NULL)
  result <- attempt_change_observation(rec, other)
  expect_identical(result$changes[[1]]$status, "unknown")
  other$execution_context$sources <- list(p = NULL)
  expect_identical(attempt_change_observation(other, other)$status, "not_comparable")
})

test_that("shared-helper revisions keep their affected-output declarations in observations", {
  before <- list(attempt_id = "one", execution_context = list(sources = list(p = "source"), environment = list(R = "same")),
    execution_order = "p", executed_component_ids = "p", input_hashes_before = list(), input_hashes_after = list(),
    output_hashes = list("work/out.rds" = "old"),
    revision_manifest = list(p = list(revision_id = "old-helper", r_hash = "unchanged-code", affected_outputs = "work.out")))
  after <- before
  after$revision_manifest$p$revision_id <- "new-helper"
  after$output_hashes[["work/out.rds"]] <- "new"
  observation <- attempt_change_observation(before, after, list("work/out.rds" = "work.out"))
  expect_true(observation$changes[[1]]$declared_affected)
})

test_that("missing-context findings that name translated components are not repair items", {
  guidance <- list(facts = list(), identity = "identity")
  finding <- list(category = "missing_context", severity = "high",
    sas_evidence = "The caller invokes %util_a, %util_b and %util_c.", r_evidence = "util_a('x')",
    unresolved_dependencies = list("util_a", "%UTIL_B", "macro__util_c"))
  available <- c("macro__util_a", "macro__util_b", "macro__util_c", "main")
  out <- classify_review_findings(list(finding), guidance, "util_a('x')", list(), available = available)
  expect_identical(out[[1L]]$repair_disposition, "context_available")
  expect_length(actionable_review_findings(list(findings = out)), 0L)
  expect_false(program_review_needs_repair(list(verdict = "repair_required", findings = out)))
  partial <- classify_review_findings(list(finding), guidance, "util_a('x')", list(),
    available = c("macro__util_a", "main"))
  expect_identical(partial[[1L]]$repair_disposition, "unverified")
  none <- finding
  none$unresolved_dependencies <- list()
  expect_identical(classify_review_findings(list(none), guidance, "util_a('x')", list(),
    available = available)[[1L]]$repair_disposition, "unverified")
  defect <- finding
  defect$category <- "translation_defect"
  expect_identical(classify_review_findings(list(defect), guidance, "util_a('x')", list(),
    available = available)[[1L]]$repair_disposition, "unverified")
})

test_that("the reviewer grades unavailable SAS facilities by their effect on outputs", {
  prompt <- paste(readLines(system.file("prompts", "reviewer.md", package = "sas2r")), collapse = "\n")
  expect_match(prompt, "no R equivalent", fixed = TRUE)
  expect_match(prompt, "translated components in this run", fixed = TRUE)
})
