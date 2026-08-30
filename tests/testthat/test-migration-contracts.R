test_that("migration records have deterministic identities and paths", {
  a <- migration_hash(list(b = 2, a = 1))
  b <- migration_hash(list(a = 1, b = 2))
  expect_identical(a, b)

  p <- migration_paths(file.path(tempdir(), "out"))
  expect_named(p, c(
    "root", "state", "graph", "programs", "staging", "attempts",
    "selected", "usage", "report_json", "report_md"
  ))
  expect_identical(p$staging, file.path(p$state, "staging"))
  expect_identical(p$attempts, file.path(p$root, "runs"))

  # A run-scoped view nests attempts, program evidence, and the markdown
  # report under the run id; nothing else moves (report_json in particular
  # stays at the state level -- resume reads it there).
  scoped <- migration_paths(file.path(tempdir(), "out"), run_id = "run_abc")
  expect_identical(scoped$attempts, file.path(scoped$root, "run_abc"))
  expect_identical(scoped$programs, file.path(scoped$root, "run_abc", "programs"))
  expect_identical(scoped$report_md, file.path(scoped$root, "run_abc", "report.md"))
  expect_identical(scoped$report_json, p$report_json)
  moved <- c("attempts", "programs", "report_md")
  expect_identical(scoped[setdiff(names(scoped), moved)],
                   p[setdiff(names(p), moved)])
  expect_identical(COMPONENT_EVIDENCE_LEVELS, c(
    "reviewed_only", "runtime_verified", "output_verified",
    "reference_validated"
  ))
  expect_identical(BUNDLE_STATUSES,
                   c("blocked", "needs_review", "migration_ready", "validated"))
})

test_that("migration_hash is invariant to key order but preserves vector element order", {
  h1 <- migration_hash(list(x = list(b = 2, a = 1), y = c("first", "second")))
  h2 <- migration_hash(list(y = c("first", "second"), x = list(a = 1, b = 2)))
  expect_identical(h1, h2)

  h3 <- migration_hash(list(y = c("second", "first"), x = list(a = 1, b = 2)))
  expect_false(identical(h1, h3))
})

test_that("init_migration_paths creates required directories and returns path list", {
  td <- file.path(tempdir(), paste0("mig_init_", as.integer(Sys.time())))
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  paths <- init_migration_paths(td)
  expect_true(dir.exists(paths$state))
  expect_true(dir.exists(paths$programs))
  expect_true(dir.exists(paths$attempts))
  expect_identical(paths$root, td)
})

test_that("atomic_write_json and read_json_record handle persistence safely", {
  td <- file.path(tempdir(), paste0("mig_json_", as.integer(Sys.time())))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  record_file <- file.path(td, "test_record.json")
  data <- list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    status = "migration_ready",
    count = 42L,
    tags = c("a", "b")
  )

  atomic_write_json(data, record_file)
  expect_true(file.exists(record_file))

  read_back <- read_json_record(record_file)
  expect_identical(read_back$schema_version, data$schema_version)
  expect_identical(read_back$status, data$status)
  expect_identical(as.integer(read_back$count), data$count)
  expect_identical(as.character(read_back$tags), data$tags)

  # Overwrite protection when overwrite = FALSE
  expect_error(
    atomic_write_json(data, record_file, overwrite = FALSE),
    class = "sas2r_record_exists"
  )

  # Reading non-existent file errors
  expect_error(
    read_json_record(file.path(td, "missing.json")),
    class = "sas2r_record_not_found"
  )
})

test_that("new_migration_run_record validates enums and sets defaults", {
  rec <- new_migration_run_record(
    run_id = "run-001",
    path = "/tmp/test.sas",
    out_dir = "/tmp/out",
    status = "needs_review",
    agent_evidence = "code_only"
  )
  expect_identical(rec$run_id, "run-001")
  expect_identical(rec$schema_version, MIGRATION_SCHEMA_VERSION)
  expect_identical(rec$status, "needs_review")
  expect_identical(rec$agent_evidence, "code_only")

  expect_error(
    new_migration_run_record(run_id = "run-bad", status = "invalid_status"),
    class = "sas2r_invalid_status"
  )

  expect_error(
    new_migration_run_record(run_id = "run-bad", agent_evidence = "unbounded_data"),
    class = "sas2r_invalid_evidence_policy"
  )
})

test_that("new_bundle_status_record validates status enum", {
  rec <- new_bundle_status_record(
    status = "validated",
    reason = "All reference comparisons passed."
  )
  expect_identical(rec$status, "validated")
  expect_identical(rec$reason, "All reference comparisons passed.")
  expect_identical(rec$schema_version, MIGRATION_SCHEMA_VERSION)

  expect_error(
    new_bundle_status_record(status = "passed"),
    class = "sas2r_invalid_status"
  )
})
