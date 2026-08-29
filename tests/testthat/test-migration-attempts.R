test_that("new_attempt_id generates structured attempt IDs", {
  expect_identical(new_attempt_id("smoke", 1L), "smoke_attempt_001")
  expect_identical(new_attempt_id("smoke", 12L), "smoke_attempt_012")
  expect_identical(new_attempt_id("bundle", 3L), "bundle_attempt_003")
  expect_identical(new_attempt_id("program", 100L), "program_attempt_100")

  expect_error(new_attempt_id("", 1L), class = "sas2r_invalid_argument")
  expect_error(new_attempt_id("smoke", NA_integer_), class = "sas2r_invalid_argument")
  expect_error(new_attempt_id("smoke", 0L), class = "sas2r_invalid_argument")
})

test_that("init_attempt creates directories and incomplete record", {
  base_dir <- withr::local_tempdir()
  paths <- migration_paths(base_dir)

  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)

  expect_identical(attempt$attempt_id, "smoke_attempt_001")
  expect_identical(attempt$kind, "smoke")
  expect_identical(attempt$sequence, 1L)
  expect_false(attempt$completed)
  expect_null(attempt$parent_attempt_id)

  expect_true(dir.exists(file.path(paths$attempts, "smoke_attempt_001")))
  expect_true(dir.exists(file.path(paths$attempts, "smoke_attempt_001", "bundle")))
  expect_true(dir.exists(file.path(paths$attempts, "smoke_attempt_001", "work")))
  expect_true(dir.exists(file.path(paths$attempts, "smoke_attempt_001", "outputs")))
  expect_true(dir.exists(file.path(paths$attempts, "smoke_attempt_001", "logs")))

  rec_file <- file.path(paths$attempts, "smoke_attempt_001", "record.json")
  expect_true(file.exists(rec_file))

  saved_rec <- jsonlite::fromJSON(rec_file)
  expect_identical(saved_rec$attempt_id, "smoke_attempt_001")
  expect_false(saved_rec$completed)
})

test_that("init_attempt autoincrements sequence when omitted", {
  base_dir <- withr::local_tempdir()
  paths <- migration_paths(base_dir)

  a1 <- init_attempt(paths, kind = "smoke")
  expect_identical(a1$sequence, 1L)
  expect_identical(a1$attempt_id, "smoke_attempt_001")

  a2 <- init_attempt(paths, kind = "smoke", parent_attempt_id = a1$attempt_id)
  expect_identical(a2$sequence, 2L)
  expect_identical(a2$attempt_id, "smoke_attempt_002")
  expect_identical(a2$parent_attempt_id, "smoke_attempt_001")
})

test_that("complete_attempt marks record completed and immutable", {
  base_dir <- withr::local_tempdir()
  paths <- migration_paths(base_dir)

  attempt <- init_attempt(paths, kind = "smoke", sequence = 1L)
  expect_false(attempt$completed)

  completed <- complete_attempt(
    attempt,
    passed = TRUE,
    exit_status = 0L,
    elapsed_sec = 0.42,
    executed_component_ids = c("comp_a", "comp_b"),
    input_hashes = list(adam = "hash123"),
    output_hashes = list(out = "hash456")
  )

  expect_true(completed$completed)
  expect_true(completed$passed)
  expect_identical(completed$exit_status, 0L)
  expect_identical(completed$elapsed_sec, 0.42)
  expect_identical(completed$executed_component_ids, c("comp_a", "comp_b"))
  expect_true(!is.null(completed$completed_at))

  rec_file <- file.path(paths$attempts, "smoke_attempt_001", "record.json")
  saved_rec <- jsonlite::fromJSON(rec_file)
  expect_true(saved_rec$completed)

  # Completed attempt is immutable
  expect_error(
    complete_attempt(completed, passed = FALSE),
    class = "sas2r_immutable_attempt"
  )
})

test_that("read_attempt_record retrieves on-disk record", {
  base_dir <- withr::local_tempdir()
  paths <- migration_paths(base_dir)

  attempt <- init_attempt(paths, kind = "bundle", sequence = 1L)
  read_before <- read_attempt_record(attempt$attempt_dir)
  expect_identical(read_before$attempt_id, "bundle_attempt_001")
  expect_false(read_before$completed)

  completed <- complete_attempt(attempt, passed = TRUE, exit_status = 0L)
  read_after <- read_attempt_record(attempt$attempt_dir)
  expect_true(read_after$completed)
  expect_true(read_after$passed)

  # Non-existent or NULL attempt_dir returns NULL
  expect_null(read_attempt_record(NULL))
  expect_null(read_attempt_record(file.path(base_dir, "nonexistent")))
  expect_null(read_attempt_record(123))
})

test_that("init_attempt and complete_attempt reject invalid arguments", {
  base_dir <- withr::local_tempdir()
  paths <- migration_paths(base_dir)

  expect_error(init_attempt(paths, kind = ""), class = "sas2r_invalid_argument")
  expect_error(init_attempt(12345, kind = "smoke"), class = "sas2r_invalid_argument")
  expect_error(init_attempt(paths, kind = "smoke", sequence = -1L), class = "sas2r_invalid_argument")
  expect_error(complete_attempt(list()), class = "sas2r_invalid_argument")
})
