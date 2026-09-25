test_that("review is independent and fixer is evidence grounded", {
  fx <- review_fix_fixture()
  seen <- list()
  reviewer <- recording_reviewer(function(request) {
    seen$review <<- request
    material_review_response("R drops SAS missing-value branch")
  })
  fixer <- recording_fixer(function(request) {
    seen$fix <<- request
    valid_fix_response("fixed_program")
  })

  review <- review_program_revision(fx$revision, fx$context, reviewer)
  fixed <- fix_program_revision(
    fx$revision, review = review, smoke = fx$failed_smoke,
    mode = "program", llm = fixer
  )
  expect_false("translator_reasoning" %in% names(seen$review))
  expect_setequal(seen$fix$evidence_ids,
                  c(review$review_id, fx$failed_smoke$execution_id))
  expect_identical(fixed$mode, "program")
})

test_that("review_program_revision persists immutable review and records review_unavailable on exhausted failure", {
  withr::local_options(sas2r.agent_backoff_base = 0) # Keep every retry; omit the mock transport wait.
  fx <- review_fix_fixture()

  # 1. Successful review persists immutable review record bound to component binding
  good_llm <- recording_reviewer(function(request) {
    valid_program_review_response(verdict = "reviewed_no_material_finding")
  })
  good_review <- review_program_revision(fx$revision, fx$context, good_llm, paths = fx$paths)
  expect_identical(good_review$verdict, "reviewed_no_material_finding")
  expect_true(!is.null(good_review$review_id) && nzchar(good_review$review_id))
  expect_identical(good_review$binding_hash, fx$revision$binding$binding_hash)

  # 2. Exhausted failure converts to coordinator-authored review_unavailable record (never synthesizes success)
  failing_llm <- recording_reviewer(function(request) {
    list(status = "failed", action = "none", error = list(message = "provider rate limit exceeded", class = "sas2r_llm_rate_limit"))
  })
  unavail_review <- review_program_revision(fx$revision, fx$context, failing_llm, paths = fx$paths)
  expect_identical(unavail_review$verdict, "review_unavailable")
  expect_identical(unavail_review$status, "review_unavailable")
  expect_false(is.null(unavail_review$reason))
})

test_that("fix_program_revision requires material evidence ID and creates a new immutable revision", {
  fx <- review_fix_fixture()

  # Requires at least one evidence ID
  expect_error(
    fix_program_revision(fx$revision, review = NULL, smoke = NULL, bundle = NULL, outputs = NULL),
    class = "sas2r_fixer_missing_evidence"
  )

  # Creates new revision leaving prior revision immutable
  fixer_llm <- recording_fixer(function(request) {
    valid_program_fix_response(
      code = "target <- source |> dplyr::mutate(y = dplyr::if_else(is.na(x), 0, x * 2))",
      diagnosis = "handled missing values explicitly",
      summary = "added is.na check",
      evidence_ids = c("exec_smoke_9988")
    )
  })

  fixed <- fix_program_revision(
    fx$revision, smoke = fx$failed_smoke,
    mode = "program", llm = fixer_llm, paths = fx$paths
  )

  expect_identical(fixed$status, "ok")
  expect_identical(fixed$prior_revision_id, fx$revision$revision_id)
  expect_false(identical(fixed$revision_id, fx$revision$revision_id))
  expect_match(fixed$r_code, "dplyr::if_else")
  expect_identical(fixed$diagnosis, "handled missing values explicitly")
  expect_true("exec_smoke_9988" %in% fixed$evidence_ids)

  # Prior revision remained immutable
  prior_code <- paste(readLines(fx$revision$r_path), collapse = "\n")
  expect_match(prior_code, "target <- source |> dplyr::mutate(y = x * 2)", fixed = TRUE)
})

test_that("fix_program_revision rejects forbidden mutations", {
  fx <- review_fix_fixture()

  # Attempting to mutate source SAS or installed package
  bad_patch_llm <- recording_fixer(function(request) {
    valid_program_fix_response(
      code = "target <- source",
      bundle_helper_patch = list(
        path = "transform.sas",
        content = "hacked sas",
        reason = "forbidden"
      )
    )
  })

  expect_error(
    fix_program_revision(fx$revision, smoke = fx$failed_smoke, mode = "program", llm = bad_patch_llm),
    class = "sas2r_fixer_forbidden_mutation"
  )
})

test_that("an unavailable semantic review keeps its reason and can recover", {
  fx <- review_fix_fixture()
  unavailable <- recording_reviewer(function(request) {
    valid_program_review_response(verdict = "review_unavailable",
                                  unresolved_dependencies = "SAS source text")
  })
  first <- review_program_revision(fx$revision, fx$context, unavailable,
                                   history = new_component_evidence_history(
                                     fx$revision$component_id, fx$revision$binding))
  expect_match(first$reason, "SAS source text", fixed = TRUE)
  completed <- review_program_revision(fx$revision, fx$context,
    recording_reviewer(function(request) valid_program_review_response()), history = first$history)
  current <- current_component_evidence(completed$history)
  expect_false(current$review_unavailable)
  expect_length(current$blockers, 0L)
  expect_identical(current$level, "reviewed_only")
})

test_that("actual subprocess diagnostics reach the fixer without data previews", {
  fx <- review_fix_fixture()
  plan <- list(status = "runnable", component_id = fx$revision$component_id,
               dependency_prefix = character(), call_site = NULL,
               selected_revisions = stats::setNames(list("stop('missing required column: outcome')"), fx$revision$component_id))
  smoke <- run_program_smoke(plan, list(), withr::local_tempdir())
  fixer <- recording_fixer(function(req) valid_program_fix_response(evidence_ids = req$evidence_ids))
  fix_program_revision(fx$revision, smoke = smoke, llm = fixer)
  prompt <- paste(vapply(fixer$requests()[[1]]$messages, `[[`, character(1), "content"), collapse = "\n")
  expect_match(prompt, "missing required column: outcome", fixed = TRUE)
  expect_match(prompt, smoke$stderr_path, fixed = TRUE)
  expect_match(prompt, '"exit_status": 1', fixed = TRUE)
  expect_match(prompt, '"output_previews":', fixed = TRUE)
  expect_false(grepl('"output_previews": \\[\\{', prompt))
  expect_match(prompt, "sas_merge(a, b, by", fixed = TRUE)
  expect_match(prompt, "Many-to-many keys", fixed = TRUE)
})

test_that("an invalid equality operator reaches the fixer with its documented replacement", {
  fx <- review_fix_fixture()
  code <- 'flag <- sas_if_else(chr_cmp(3, 3, op = "<=") & chr_cmp(61, 61, op = "="), "Y", "N")'
  plan <- list(status = "runnable", component_id = fx$revision$component_id,
    dependency_prefix = character(), selected_revisions = stats::setNames(list(code), fx$revision$component_id))
  smoke <- run_program_smoke(plan, list(), withr::local_tempdir())
  expect_false(smoke$passed)
  expect_match(smoke$condition$message, 'Use op = "==" for equality', fixed = TRUE)
  fixer <- recording_fixer(function(req) valid_program_fix_response(evidence_ids = req$evidence_ids))
  fix_program_revision(fx$revision, smoke = smoke, llm = fixer)
  request <- fixer$requests()[[1L]]
  expect_match(request$messages[[1L]]$content, 'op = "=="', fixed = TRUE)
  prompt <- paste(vapply(request$messages, `[[`, character(1), "content"), collapse = "\n")
  expect_match(prompt, "chr_cmp: op must be NULL", fixed = TRUE)
})


test_that("repair refreshes helper metadata without authorizing unknown calls", {
  fx <- review_fix_fixture()
  fx$revision$contract$dependency_functions <- "upstream"
  fx$revision$contract$helper_use <- c("upstream", "lib_read", "invented")
  fixed <- fix_program_revision(fx$revision, smoke = fx$failed_smoke,
    llm = recording_fixer(function(request) valid_program_fix_response(
      code = "out <- sas_sum(upstream(1), 2)")), paths = fx$paths)
  expect_setequal(fixed$contract$helper_use, "sas_sum")
  fixed <- fix_program_revision(fx$revision, smoke = fx$failed_smoke,
    llm = recording_fixer(function(request) valid_program_fix_response(
      code = "out <- invented(1)")), paths = fx$paths)
  expect_identical(fixed$contract$helper_use, "invented")
  expect_false(check_program_revision(fixed$r_path, fixed$contract)$pass)
})
test_that("fixer corrects one mechanically invalid answer with the exact error and budget", {
  fx <- review_fix_fixture()
  for (bad in c("tryCatch({ x <- 1", "library(dplyr)\nx <- 1")) {
    purposes <- character()
    budget <- new_usage_budget()
    llm <- recording_fixer(function(req) {
      purposes <<- c(purposes, req$purpose)
      valid_program_fix_response(code = if (length(purposes) == 1L) bad else "x <- 1")
    })
    fixed <- fix_program_revision(fx$revision, smoke = fx$failed_smoke,
      llm = llm, usage = budget, paths = fx$paths)
    expect_identical(fixed$status, "ok")
    expect_identical(purposes, c("program_fix", "mechanical_retry"))
    expect_identical(budget$request_count, 2L)
    prompt <- paste(vapply(llm$requests()[[2L]]$messages, `[[`, "", "content"), collapse = "\n")
    expect_match(prompt, bad, fixed = TRUE)
    expect_match(prompt, "parse_error|lint_error")
    expect_match(prompt, "Smoke Execution Failure", fixed = TRUE)
  }
})

test_that("persistent invalid repairs and a call ceiling bound mechanical correction", {
  fx <- review_fix_fixture()
  for (limit in c(1, 10)) {
    budget <- new_usage_budget(mode = "soft", max_calls = limit)
    llm <- recording_fixer(function(req) valid_program_fix_response(code = "x <- function( {"))
    fixed <- fix_program_revision(fx$revision, smoke = fx$failed_smoke,
      llm = llm, usage = budget, paths = fx$paths)
    expect_identical(fixed$status, "check_failed")
    expect_length(llm$requests(), min(limit, 2))
    expect_equal(budget$request_count, min(limit, 2))
    expect_match(paste(fixed$checks$errors, collapse = "\n"), "parse_error")
    expect_identical(paste(readLines(fx$revision$r_path), collapse = "\n"), fx$revision$r_code)
  }
})
