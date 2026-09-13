# A source-defined workflow with seeded generated R execution or value errors.
repair_workflow_fixture <- function(n = 4L, failures = c(1L, 3L), chain = FALSE,
                                    value_errors = integer(), envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  inputs <- file.path(root, "inputs")
  dir.create(inputs)
  saveRDS(data.frame(id = 1:3, value = 10:12), file.path(inputs, "input.rds"))
  config <- list(libraries = list(raw = list(path = inputs, engine = "rds")))
  ids <- sprintf("p%02d", seq_len(n))
  code <- sas <- list()
  for (i in seq_len(n)) {
    id <- ids[i]
    parent <- if (chain && i > 1L) paste0("work.out", i - 1L) else "raw.input"
    sas[[id]] <- sprintf("data work.out%d; set %s; value = value + 1; run;", i, parent)
    writeLines(sas[[id]], file.path(root, paste0(id, ".sas")))
    bits <- strsplit(parent, ".", fixed = TRUE)[[1L]]
    code[[id]] <- sprintf("x <- lib_read('%s', '%s')\nx$value <- x$value + 1\nlib_write(x, 'work', 'out%d')", bits[1], bits[2], i)
  }
  project <- sas_project(root, config = config)
  state <- new_migration_state(project, file.path(root, "migration"), config = config)
  for (i in seq_len(n)) {
    id <- ids[i]
    r <- if (i %in% failures) sprintf("stop('translation fault %s')", id) else code[[id]]
    if (i %in% value_errors) r <- sub("+ 1", "+ 9", r, fixed = TRUE)
    r_path <- file.path(root, paste0(id, ".R"))
    writeLines(r, r_path)
    binding <- new_component_binding(migration_hash(sas[[id]]), migration_hash(r),
      migration_hash("helpers"), migration_hash("review"), migration_hash("closure"))
    state$selected_revisions[[id]] <- list(component_id = id, revision_id = "r1", r_code = r,
      staged_file = paste0(id, ".R"), r_path = r_path, binding = binding,
      contract = list(component_id = id, staged_file = paste0(id, ".R"),
        sas_text = sas[[id]], binding = binding))
    h <- new_component_evidence_history(id, binding)
    state$histories[[id]] <- record_completed_review(h)
  }
  state$output_contracts <- infer_output_contracts(project,
    overrides = list(datasets = paste0("work.out", seq_len(n))))
  state$fixer_llm <- recording_fixer(function(context) {
    valid_program_fix_response(code = code[[context$component_id]],
      diagnosis = paste("Repair", context$component_id), summary = "Restore source derivation")
  })
  state$reviewer_llm <- recording_reviewer(function(context) valid_program_review_response())
  list(state = state, root = root, fixed = code, ids = ids)
}

test_that("twenty dependent scripts can repair three newly exposed blockers by default", {
  fx <- repair_workflow_fixture(n = 20L, failures = c(5L, 12L, 18L), chain = TRUE)
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$status, "migration_ready")
  expect_identical(result$attempts$sequence, 1:4)
  expect_identical(vapply(result$repairs, `[[`, character(1), "component_id"),
                   c("p05", "p12", "p18"))
  expect_identical(result$attempt$executed_component_ids, fx$ids)
  out <- readRDS(file.path(result$selected_attempt$attempt_dir, "work", "out20.rds"))
  expect_equal(out$value, 30:32)
  expect_true(all(result$attempts$fresh_work))
  expect_true(all(vapply(result$diagnostics$bundle_repair$attempts, function(x) {
    length(x$executions) == 0L
  }, logical(1))))
})

test_that("an explicit overall cap remains a hard limit", {
  fx <- repair_workflow_fixture(n = 4L, failures = c(1L, 2L, 3L), chain = TRUE)
  result <- run_bundle_pipeline(fx$state, max_bundle_repair_rounds = 2L)
  expect_identical(result$status, "blocked")
  expect_identical(result$status_reason, "max_bundle_repair_rounds_reached")
  expect_length(result$repairs, 2L)
  expect_identical(result$attempt$condition$component_id, "p03")
})

test_that("independent failures are diagnosed in isolation and repaired before one rerun", {
  fx <- repair_workflow_fixture(n = 4L, failures = c(1L, 3L))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$status, "migration_ready")
  expect_identical(result$attempts$sequence, 1:2)
  expect_identical(vapply(result$repairs, `[[`, character(1), "component_id"), c("p01", "p03"))
  first <- result$diagnostics$bundle_repair$attempts[[1L]]
  expect_setequal(names(first$failures), c("p01", "p03"))
  expect_true(first$executions$p02$passed)
  expect_false(first$executions$p03$passed)
  expect_true(first$executions$p04$passed)
  for (record in first$executions) expect_identical(record$scope, "program_smoke")
  expect_false(file.exists(file.path(result$paths$attempts, "bundle_attempt_001", "work", "out2.rds")))
  expect_identical(result$attempt$executed_component_ids, fx$ids)
  requests <- fx$state$fixer_llm$requests()
  prompt <- paste(vapply(requests[[2L]]$messages, `[[`, character(1), "content"), collapse = "\n")
  expect_match(prompt, "translation fault p03", fixed = TRUE)
  expect_match(prompt, first$executions$p03$execution_id, fixed = TRUE)
})

test_that("independent reference mismatches are repaired together at their writers", {
  fx <- repair_workflow_fixture(n = 4L, failures = integer(), value_errors = c(1L, 3L))
  reference <- file.path(fx$root, "expected.rds")
  saveRDS(data.frame(id = 1:3, value = 11:13), reference)
  fx$state$comparison_rules <- list(references =
    stats::setNames(rep(list(reference), 4L), paste0("work.out", 1:4)))
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$status, "validated")
  expect_identical(result$attempts$sequence, 1:2)
  expect_identical(vapply(result$repairs, `[[`, character(1), "component_id"), c("p01", "p03"))
  expect_true(all(vapply(result$assessment$targets, function(x) isTRUE(x$passed), logical(1))))
})

test_that("a shared helper patch requires fresh evidence before another repair", {
  fx <- repair_workflow_fixture(n = 3L, failures = c(1L, 3L))
  repair_attempts <- character()
  fx$state$fixer_llm <- recording_fixer(function(context) {
    repair_attempts <<- c(repair_attempts, context$attempt_id)
    id <- context$component_id
    response <- valid_program_fix_response(code = fx$fixed[[id]],
      diagnosis = "Repair", summary = "Restore source derivation")
    if (id == "p01") {
      helper <- file.path(fx$state$paths$attempts, "bundle_attempt_001", "bundle", "sas2r-helpers.R")
      response$data$bundle_helper_patch <- list(path = "sas2r-helpers.R", reason = "Refresh helper definition",
        content = paste(c(readLines(helper, warn = FALSE), "# Helper revision"), collapse = "\n"))
    }
    response
  })
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$status, "migration_ready")
  expect_identical(result$attempts$sequence, 1:3)
  expect_identical(repair_attempts, c("bundle_attempt_001", "bundle_attempt_002"))
})

test_that("a stuck component does not consume independent components' allowances", {
  fx <- repair_workflow_fixture(n = 3L, failures = c(1L, 3L))
  calls <- list()
  fx$state$fixer_llm <- recording_fixer(function(context) {
    id <- context$component_id
    calls[[id]] <<- (calls[[id]] %||% 0L) + 1L
    code <- if (id == "p01") sprintf("stop('still broken, attempted fix %d')", calls[[id]]) else fx$fixed[[id]]
    valid_program_fix_response(code = code, diagnosis = "Attempted repair", summary = "Candidate")
  })
  result <- run_bundle_pipeline(fx$state, max_bundle_repairs_per_component = 2L)
  expect_identical(calls, list(p01 = 2L, p03 = 1L))
  expect_identical(result$status, "blocked")
  expect_identical(result$status_reason, "bundle_component_repair_limits_reached")
  expect_identical(result$selected_revisions$p03$r_code, fx$fixed$p03)
})

test_that("no-op repairs are deferred while independent repairs continue", {
  fx <- repair_workflow_fixture(n = 3L, failures = c(1L, 3L))
  fx$state$fixer_llm <- recording_fixer(function(context) {
    id <- context$component_id
    code <- if (id == "p01") fx$state$selected_revisions$p01$r_code else fx$fixed[[id]]
    valid_program_fix_response(code = code, diagnosis = "Repair", summary = "Candidate")
  })
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$status, "blocked")
  expect_identical(result$diagnostics$bundle_repair$deferred$p01, "identical_patch")
  expect_identical(result$diagnostics$bundle_repair$repair_counts, list(p01 = 1L, p03 = 1L))
  expect_identical(result$selected_revisions$p03$r_code, fx$fixed$p03)
})

test_that("all known mechanical failures are retained and queued", {
  fx <- repair_workflow_fixture(n = 3L, failures = c(1L, 3L))
  for (id in c("p01", "p03")) {
    fx$state$selected_revisions[[id]]$checks <- list(pass = FALSE, errors = "invalid translated interface")
  }
  result <- run_bundle_pipeline(fx$state)
  expect_identical(result$status, "migration_ready")
  expect_identical(result$attempts$sequence, 1:2)
  first <- read_attempt_record(file.path(result$paths$attempts, "bundle_attempt_001"))
  expect_setequal(names(first$mechanical_failures), c("p01", "p03"))
})

test_that("repair limit configuration rejects non-count values before translation", {
  for (bad in list(-1, 1.5, Inf, NA, "2")) {
    expect_error(sas_translate("not-a-project", max_bundle_repairs_per_component = bad),
                 class = "sas2r_invalid_argument")
    expect_error(sas_translate("not-a-project", max_bundle_repair_rounds = bad),
                 class = "sas2r_invalid_argument")
  }
})
