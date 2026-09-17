test_that("completed reviews are reused only for the same full request context", {
  fx <- review_fix_fixture()
  reviewer <- recording_reviewer(function(req) valid_program_review_response())
  review <- review_program_revision(fx$revision, fx$context, reviewer, paths = fx$paths)
  again <- review_program_revision(fx$revision, fx$context, reviewer,
    history = review$history, paths = fx$paths)
  expect_true(again$reused)
  expect_length(reviewer$requests(), 1)
  # A later smoke event is not lost by restoring the cached review's history.
  h <- record_runtime_deferred(review$history, "later event")
  again <- review_program_revision(fx$revision, fx$context, reviewer, history = h)
  expect_identical(again$history, h)
  changes <- list(
    list(phase = "bundle"),
    list(focus_outputs = "work.target"),
    list(phase = "bundle", full_review = TRUE, focus_outputs = "work.target"),
    list(helper_code = "helper <- function() 2"),
    list(execution = list(condition = list(message = "new failure"))),
    list(source_input_identity = "new source input"))
  for (change in changes) {
    new <- review_program_revision(fx$revision, utils::modifyList(fx$context, change),
      reviewer, history = h)
    expect_false(isTRUE(new$reused))
  }
  expect_length(reviewer$requests(), 7)
  reference_only <- utils::modifyList(fx$context, list(config = list(
    comparison_rules = list(reference = "DO_NOT_SEND"))))
  expect_true(review_program_revision(fx$revision, reference_only, reviewer, history = h)$reused)
  expect_length(reviewer$requests(), 7)
  changed_model <- reviewer; changed_model$model <- "different-reviewer"
  expect_false(isTRUE(review_program_revision(fx$revision, fx$context, changed_model, history = h)$reused))
  unavailable <- record_review_unavailable(h, "new missing context")
  expect_false(isTRUE(review_program_revision(fx$revision, fx$context, reviewer, history = unavailable)$reused))
  focused <- review_program_revision(fx$revision, utils::modifyList(fx$context,
    list(focus_outputs = "work.target")), reviewer, history = h)
  expect_false(isTRUE(review_program_revision(fx$revision, fx$context, reviewer, history = focused$history)$reused))
})

test_that("same-path helper edits and changed dependencies invalidate reuse", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  state <- process_program_component(fx$state, "p02", execute = FALSE)
  expect_length(state$reviewer_llm$requests(), 1)
  moved <- withr::local_tempfile()
  file.copy(state$runtime$helpers, moved)
  state$runtime$helpers <- moved
  state <- process_program_component(state, "p02", execute = FALSE)
  expect_length(state$reviewer_llm$requests(), 1)
  cat("\nextra <- function() 1\n", file = state$runtime$helpers, append = TRUE)
  state <- process_program_component(state, "p02", execute = FALSE)
  expect_length(state$reviewer_llm$requests(), 2)
  state$selected_revisions$p01$r_code <- paste(state$selected_revisions$p01$r_code, "# different", sep = "\n")
  state <- process_program_component(state, "p02", execute = FALSE)
  expect_length(state$reviewer_llm$requests(), 3)
  refreshed <- review_helper_consumers(state, state, "p02", 1)$state
  expect_length(state$reviewer_llm$requests(), 3)
  again <- process_program_component(refreshed, "p02", execute = FALSE)
  expect_length(again$reviewer_llm$requests(), 3)
})

test_that("bundle fix and full review receive integration focus without reference answers", {
  fx <- repair_workflow_fixture(n = 1L, failures = 1L)
  fx$state$config$outputs <- list(references = list(out = "SECRET_REFERENCE_FILE"))
  fx$state$config$comparison_rules <- list(expected_value = "SECRET_REFERENCE_VALUE")
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 1L)
  expect_true(result$attempt$passed)
  for (worker in list(fx$state$fixer_llm, fx$state$reviewer_llm)) {
    request <- worker$requests()[[1]]
    prompt <- paste(vapply(request$messages, `[[`, "", "content"), collapse = "\n")
    expect_match(prompt, "Bundle integration focus", fixed = TRUE)
    expect_match(prompt, "translation fault p01", fixed = TRUE)
    expect_match(prompt, "full semantic review", fixed = TRUE)
    expect_false(grepl("SECRET_REFERENCE", prompt))
    expect_false("read_comparison_report" %in% names(request$tools))
  }
  event <- tail(Filter(function(e) e$type == "review_completed",
    current_component_evidence(result$histories$p01)$events), 1)[[1]]
  expect_identical(event$review_record$phase, "bundle")
  expect_identical(event$review_record$review_scope, "full")
})

test_that("bundle review of a producer receives selected consumer source and code", {
  fx <- repair_workflow_fixture(n = 2L, failures = integer(), chain = TRUE)
  rev <- fx$state$selected_revisions$p01
  context <- list(project = fx$state$project, selected_revisions = fx$state$selected_revisions,
    config = fx$state$config, phase = "bundle", helper_code = runtime_helper_code(fx$state$runtime))
  reviewer <- fx$state$reviewer_llm
  reviewed <- review_program_revision(rev, context, reviewer, history = fx$state$histories$p01)
  prompt <- paste(vapply(reviewer$requests()[[1]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_match(prompt, "downstream caller/consumer", fixed = TRUE)
  expect_match(prompt, fx$fixed$p02, fixed = TRUE)
  expect_match(prompt, "data work.out2; set work.out1", fixed = TRUE)
  fix_program_revision(rev, checks = list(check_id = "shape", errors = "producer/consumer mismatch"),
    llm = fx$state$fixer_llm, mode = "bundle", project = fx$state$project,
    selected_revisions = context$selected_revisions, paths = fx$state$paths)
  request <- fx$state$fixer_llm$requests()[[1]]
  expect_match(paste(vapply(request$messages, `[[`, "", "content"), collapse = "\n"), fx$fixed$p02, fixed = TRUE)
  context$selected_revisions$p02$r_code <- "consumer_changed <- TRUE"
  expect_false(isTRUE(review_program_revision(rev, context, reviewer, history = reviewed$history)$reused))
})

projection_fixture <- function(source, consumer = "data work.final; set work.slice; where arm=1; run;",
                               envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  writeLines(source, file.path(root, "producer.sas"))
  writeLines(consumer, file.path(root, "consumer.sas"))
  sas_project(root)
}

test_that("plain source projections establish only exclusions", {
  p <- projection_fixture("data work.slice; set raw.input; keep id value; run;")
  fact <- source_output_projection(p, "work.slice")
  expect_identical(fact$operation, "keep")
  expect_identical(fact$columns, c("id", "value"))
  expect_identical(fact$component_id, "producer")
  expect_true(length(fact$unit_id) == 1 && length(fact$stmt_id) == 1)
  packet <- build_agent_guidance(p, "consumer")
  expect_match(packet$text, "source_output_excludes_column work.slice", fixed = TRUE)
  expect_match(packet$text, "outside: id, value", fixed = TRUE)
  expect_match(packet$text, "KEEP inclusion does not establish", fixed = TRUE)
  expect_lte(nchar(build_agent_guidance(p, "consumer", packet_limit = 1200L)$text), 1200L)
  drop <- projection_fixture("data work.slice; set raw.input; drop arm; run;")
  expect_match(build_agent_guidance(drop, "consumer")$text, "excludes these columns: arm", fixed = TRUE)
  expect_false(identical(packet$identity, build_agent_guidance(drop, "consumer")$identity))
  # No exemption: even with this fact, an invented R guard stays actionable.
  finding <- list(category = "translation_defect", severity = "material",
    sas_evidence = "set work.slice; no requirement for id_typo", r_evidence = "stopifnot('id_typo' %in% names(x))")
  classified <- classify_review_findings(list(finding), packet, finding$r_evidence, list())
  expect_true(program_review_needs_repair(list(verdict = "repair_required", findings = classified)))
})

test_that("ambiguous projections remain unknown as whole syntax classes", {
  statements <- c("keep id value:;", "keep v1-v3;", "keep 'odd name'n;",
    "keep _all_;", "keep _numeric_;", "keep _character_;",
    "keep id; drop arm;", "keep id; keep value;", "drop id; drop value;",
    "rename arm=group; keep id arm;", "keep id arm; rename arm=group;",
    "keep &list;", "%select_columns(); keep id;", "retain x; keep id;")
  sources <- c(paste0("data work.slice; set raw.input; ", statements, " run;"),
    "data work.slice(keep=id); set raw.input; keep id; run;",
    "data work.slice; set raw.input(rename=(x=id)); keep id; run;",
    "data work.slice; set work.slice; keep id; run;",
    "data work.slice; set raw.input; keep id; run; data work.slice; set raw.input; run;",
    "libname work '/tmp/other'; data work.slice; set raw.input; keep id; run;",
    "%include 'other.sas'; data work.slice; set raw.input; keep id; run;",
    "data work.slice work.extra; set raw.input; keep id; run;")
  for (source in sources) {
    p <- projection_fixture(source)
    expect_null(source_output_projection(p, "work.slice"), info = source)
  }
  p <- projection_fixture("data unknown.slice; set raw.input; keep id; run;")
  expect_null(source_output_projection(p, "unknown.slice"))
})

test_that("execution reporting retains failures and does not credit a later revision", {
  fx <- repair_workflow_fixture(n = 3L, failures = 2L, chain = TRUE)
  first <- run_bundle_attempt(fx$state, sequence = 1L)
  second <- run_bundle_attempt(fx$state, sequence = 2L)
  state <- fx$state
  state$attempt <- second
  status <- bundle_execution_report(state)
  expect_length(status$p01$attempts, 2)
  expect_identical(status$p01$current_revision$status, "passed")
  expect_identical(status$p02$current_revision$status, "failed")
  expect_identical(status$p03$current_revision$status, "not reached after p02")
  expect_null(status$p01$selected_attempt)
  expect_true(file.exists(status$p02$attempts[[1]]$record_path))
  state <- stage_workflow_revision(state, "p01", paste(fx$fixed$p01, "# new", sep = "\n"), "reviewed_no_material_finding")
  later <- bundle_execution_report(state)
  expect_null(later$p01$current_revision)
  expect_null(later$p02$current_revision) # its producer changed
  state$status <- "blocked"
  write_migration_report(state)
  manifest <- read_json_record(state$paths$manifest)
  expect_length(manifest$components$p02$bundle_execution$attempts, 2)
  expect_null(manifest$components$p01$bundle_execution$current_revision)
  for (path in c(state$paths$start_here, state$paths$report_md)) {
    text <- paste(readLines(path), collapse = "\n")
    expect_match(text, "bundle_attempt_002 failed", fixed = TRUE)
    expect_match(text, "unexecuted (no matching attempt)", fixed = TRUE)
  }
  expect_identical(read_attempt_record(first$attempt_dir)$helper_hash, first$helper_hash)
})

test_that("smoke hashes describe content and survive directory changes", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  plan <- build_program_smoke_plan(fx$state$graph, "p01", fx$state$selected_revisions)
  first <- prepare_program_smoke(fx$state, plan, withr::local_tempdir())
  second <- prepare_program_smoke(fx$state, plan, withr::local_tempdir())
  expect_identical(first$plan$code_hashes, second$plan$code_hashes)
  a <- run_program_smoke(first$plan, first$runtime, first$attempt_dir)
  b <- run_program_smoke(second$plan, second$runtime, second$attempt_dir)
  expect_true(a$passed && b$passed)
  expect_identical(a$output_hashes, b$output_hashes)
  expected <- unname(cli::hash_file_sha256(a$output_files[[1]]))
  expect_identical(a$output_hashes[[1]], expected)
  plan$selected_revisions$p01$r_code <- paste(fx$fixed$p01, "# changed", sep = "\n")
  third <- prepare_program_smoke(fx$state, plan, withr::local_tempdir())
  expect_false(identical(first$plan$code_hashes, third$plan$code_hashes))
  bundle <- run_bundle_attempt(fx$state, sequence = 1L)
  expect_identical(a$output_hashes[[1]], bundle$output_hashes[["work/out1.rds"]])
})

test_that("semantic policy keeps harmless representation separate from observable errors", {
  # SAS fixed-width padding is unobserved by this output comparison.
  expect_true(passed(compare_datasets(data.frame(id = 1, label = "AB  "),
    data.frame(id = 1, label = "AB"), keys = "id")))
  # Source LENGTH label $2; label='ABCD'; must truncate, not merely trim blanks.
  expect_false(passed(compare_datasets(data.frame(id = 1, label = "AB"),
    data.frame(id = 1, label = "ABCD"), keys = "id")))
  # Source ordinary missing sorts before numbers; dropping it changes the rows.
  values <- c(2, NA_real_, 1)
  expected <- data.frame(value = c(NA_real_, 1, 2))
  expect_true(passed(compare_datasets(expected, data.frame(value = sort(values, na.last = FALSE)))))
  expect_false(passed(compare_datasets(expected, data.frame(value = sort(values)))))
  p <- compare_profile()
  expect_identical(p$na_tags, "report")
  expect_identical(p$numeric, list(abs = 1e-8, rel = 1e-8))
  for (category in c("unknown", "unsupported_capability")) {
    f <- list(category = category, severity = "material", sas_evidence = "LENGTH label $2",
      r_evidence = "label <- 'ABCD'")
    expect_true(program_review_needs_repair(list(verdict = "repair_required", findings = list(f))))
  }
  expect_true(program_review_needs_repair(list(verdict = "repair_required", findings = list())))
})

test_that("generic macro examples preserve named returns, blank fields and caller state", {
  lines <- readLines(system.file("skills", "sas-macro-execution", "SKILL.md", package = "sas2r"))
  starts <- which(lines == "```r")
  env <- new.env(parent = baseenv())
  for (start in starts) {
    end <- which(seq_along(lines) > start & lines == "```")[1L]
    eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), envir = env)
  }
  expect_identical(env$make_summary(1:3)$count, c(total = 3L))
  expect_identical(env$use_summary(1:3), list(count = 3L, label = "",
    has_optional_field = TRUE, outer_value = "outer value"))
  expect_identical(env$use_summary(integer())$count, 0L)
  expect_true(is.function(env$make_summary))
  expect_identical(env$use_summary(1:2)$count, 2L)
  expect_identical(env$build_panels(list(1:3, numeric(), 7:9))[[3]]$limits, c(7L, 9L))
})

test_that("existing artifact success cannot cancel a source finding about missing statistics", {
  fx <- review_fix_fixture()
  fx$context$sas_source <- paste("ods pdf file='report.pdf'; proc sgplot data=work.source;",
    "vbox value; run; proc means data=work.source n mean; var value; run; ods pdf close;")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(pdf); graphics::boxplot(1:3); grDevices::dev.off()
  expect_gt(file.info(pdf)$size, 0)
  fx$revision$r_code <- "grDevices::pdf('report.pdf'); graphics::boxplot(x$value); grDevices::dev.off()"
  fx$context$phase <- "bundle"
  fx$context$execution <- list(passed = TRUE)
  reviewer <- recording_reviewer(function(req) material_review_response(
    sas_evidence = "PROC MEANS n mean requires a statistics section", r_evidence = "only boxplot is rendered",
    affected_outputs = "report.pdf"))
  review <- review_program_revision(fx$revision, fx$context, reviewer)
  expect_true(program_review_needs_repair(review))
  expect_identical(review$review_scope, "full")
  prompt <- paste(vapply(reviewer$requests()[[1]]$messages, `[[`, "", "content"), collapse = "\n")
  expect_match(prompt, "proc means", fixed = TRUE)
  expect_match(prompt, "graphics::boxplot", fixed = TRUE)
  expect_match(prompt, "A PDF that exists is not evidence", fixed = TRUE)
})
