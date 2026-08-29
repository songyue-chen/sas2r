test_that("resume_migration_attempts reuses completed attempts with matching binding", {
  base <- withr::local_tempdir()
  paths <- migration_paths(base)
  init_migration_paths(base)

  binding1 <- list(source_hash = "src_111", r_hash = "r_111")
  binding2 <- list(source_hash = "src_222", r_hash = "r_222")

  a1 <- init_attempt(paths, kind = "bundle", sequence = 1L)
  a1_comp <- complete_attempt(a1, passed = TRUE, exit_status = 0L, run_binding = binding1)

  # Resuming with matching binding1 reuses a1
  res1 <- resume_migration_attempts(paths, run_binding = binding1)
  expect_identical(res1$latest_completed$attempt_id, a1$attempt_id)
  expect_true(a1$attempt_id %in% res1$reusable_attempt_ids)

  # Resuming with non-matching binding2 does not reuse a1
  res2 <- resume_migration_attempts(paths, run_binding = binding2)
  expect_null(res2$latest_completed)
  expect_false(a1$attempt_id %in% res2$reusable_attempt_ids)
})

test_that("resume_migration_attempts ignores incomplete and interrupted attempts", {
  base <- withr::local_tempdir()
  paths <- migration_paths(base)
  init_migration_paths(base)

  binding <- list(source_hash = "src_111", r_hash = "r_111")

  a1 <- init_attempt(paths, kind = "bundle", sequence = 1L)
  a1_comp <- complete_attempt(a1, passed = TRUE, exit_status = 0L, run_binding = binding)

  # Incomplete / interrupted attempt 2
  a2 <- init_attempt(paths, kind = "bundle", sequence = 2L)

  res <- resume_migration_attempts(paths, run_binding = binding)
  expect_identical(res$latest_completed$attempt_id, a1$attempt_id)
  expect_true(a1$attempt_id %in% res$reusable_attempt_ids)
  expect_false(a2$attempt_id %in% res$reusable_attempt_ids)
  expect_true(a2$attempt_id %in% res$incomplete_attempt_ids)
})

test_that("resume_migration_attempts handles empty or missing attempts directory", {
  base <- withr::local_tempdir()
  paths <- migration_paths(base)

  res <- resume_migration_attempts(paths, run_binding = list(src = "1"))
  expect_null(res$latest_completed)
  expect_identical(res$reusable_attempt_ids, character())
})
