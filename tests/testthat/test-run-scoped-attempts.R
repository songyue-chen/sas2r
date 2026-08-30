# Repeated runs into one out_dir must never collide: attempts are scoped per
# run (runs/<run_id>/...), and the staged working bundle lives under
# .sas2r/staging/ instead of masquerading as a deliverable at the out_dir root.

scoped_demo_file <- function(envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".sas", .local_envir = envir)
  writeLines(c(
    "data work.demo;",
    "  x = 1;",
    "run;"
  ), f)
  f
}

test_that("two runs into the same out_dir keep separate attempt trees", {
  out <- withr::local_tempdir()

  r1 <- sas_translate(scoped_demo_file(), out_dir = out, execute = FALSE)
  r2 <- sas_translate(scoped_demo_file(), out_dir = out, execute = FALSE)

  expect_false(identical(r1$run_id, r2$run_id))

  top <- list.dirs(out, full.names = FALSE, recursive = FALSE)
  run_dirs <- top[grepl("^run_", top)]
  expect_setequal(run_dirs, c(r1$run_id, r2$run_id))
  # Nothing else visible shares the out_dir top level with the run folders.
  expect_identical(setdiff(top, c(run_dirs, ".sas2r")), character(0))

  # Each result's bundle lives inside its own run's attempts tree (compare by
  # path components: normalizePath() resolves /var -> /private/var on macOS,
  # so absolute-prefix comparison is not portable), and the first run's
  # bundle survives the second run untouched.
  run_scope_of <- function(bundle_dir) basename(dirname(dirname(bundle_dir)))
  expect_identical(run_scope_of(r1$bundle_dir), r1$run_id)
  expect_identical(run_scope_of(r2$bundle_dir), r2$run_id)
  expect_true(dir.exists(r1$bundle_dir))
  expect_true(length(list.files(r1$bundle_dir, pattern = "\\.R$")) > 0L)

  # The selected translation is materialized at the top of each run folder:
  # program .R files plus the runtime trio, with machine metadata left in
  # the attempt bundle.
  for (r in list(r1, r2)) {
    run_root_files <- list.files(file.path(out, r$run_id))
    expect_true("sas2r-helpers.R" %in% run_root_files)
    expect_true("_sas2r_registry.R" %in% run_root_files)
    programs_at_root <- setdiff(
      grep("\\.R$", run_root_files, value = TRUE),
      c("sas2r-helpers.R", "_sas2r_registry.R", "_sas2r_formats.R")
    )
    expect_gt(length(programs_at_root), 0L)
    expect_identical(grep("contract\\.json$", run_root_files, value = TRUE), character(0))
    expect_false("_sas2r_bundle_progress.json" %in% run_root_files)
  }

  # Run ids are timestamp-first (chronologically sortable directory names)
  # with a hash suffix so same-second runs stay distinct.
  expect_match(r1$run_id, "^run_[0-9]{8}T[0-9]{6}Z_[0-9a-f]{8}$")
  expect_match(r2$run_id, "^run_[0-9]{8}T[0-9]{6}Z_[0-9a-f]{8}$")

  # Each run folder carries its own report; no report surfaces at the
  # out_dir root, and each result points at its own run's copy.
  expect_true(file.exists(file.path(out, r1$run_id, "report.md")))
  expect_true(file.exists(file.path(out, r2$run_id, "report.json")))
  expect_false(file.exists(file.path(out, "report.md")))
  expect_identical(basename(dirname(r1$report_path)), r1$run_id)
  expect_identical(basename(dirname(r2$report_path)), r2$run_id)
  expect_true(file.exists(r1$report_path))
})

test_that("staging lives under .sas2r/ and no programs surface at the out_dir root", {
  out <- withr::local_tempdir()
  res <- sas_translate(scoped_demo_file(), out_dir = out, execute = FALSE)

  staging <- file.path(out, ".sas2r", "staging")
  expect_true(dir.exists(staging))
  expect_true(file.exists(file.path(staging, "_sas2r_registry.R")))

  # The out_dir root shows finished artifacts only -- no staged R programs
  # that a user could mistake for the selected translation.
  expect_identical(
    list.files(out, pattern = "\\.R$"),
    character(0)
  )
  expect_true(file.exists(res$report_path))

  # Program evidence and outputs no longer surface at the out_dir root:
  # revisions/reviews live inside the run's own folder, and the old empty
  # outputs/ decoy is gone.
  expect_false(dir.exists(file.path(out, "programs")))
  expect_false(dir.exists(file.path(out, "outputs")))
  expect_true(dir.exists(file.path(out, res$run_id, "programs")))

  # The run's program evidence (revisions, reviews) must survive attempt
  # pruning: prune only touches <kind>_attempt_NNN directories, never the
  # programs/ folder that shares the run directory with them.
  expect_gt(
    length(list.files(file.path(out, res$run_id, "programs"),
                      recursive = TRUE)),
    0L
  )
})

test_that("resume reruns into a fresh run scope without disturbing prior attempts", {
  out <- withr::local_tempdir()
  f <- scoped_demo_file()

  r1 <- sas_translate(f, out_dir = out, execute = FALSE)
  r2 <- sas_translate(f, out_dir = out, execute = FALSE, resume = TRUE)

  expect_false(identical(r1$run_id, r2$run_id))
  expect_true(dir.exists(file.path(out, r1$run_id)))
  expect_true(dir.exists(file.path(out, r2$run_id)))
})
