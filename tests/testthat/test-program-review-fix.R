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
