# Repeated runs into one out_dir must never collide: attempts are scoped per
# run (attempts/<run_id>/...), and the staged working bundle lives under
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

  run_dirs <- list.dirs(file.path(out, "attempts"), full.names = FALSE, recursive = FALSE)
  expect_setequal(run_dirs, c(r1$run_id, r2$run_id))

  # Each result's bundle lives inside its own run's attempts tree (compare by
  # path components: normalizePath() resolves /var -> /private/var on macOS,
  # so absolute-prefix comparison is not portable), and the first run's
  # bundle survives the second run untouched.
  run_scope_of <- function(bundle_dir) basename(dirname(dirname(bundle_dir)))
  expect_identical(run_scope_of(r1$bundle_dir), r1$run_id)
  expect_identical(run_scope_of(r2$bundle_dir), r2$run_id)
  expect_true(dir.exists(r1$bundle_dir))
  expect_true(length(list.files(r1$bundle_dir, pattern = "\\.R$")) > 0L)
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
  expect_true(file.exists(file.path(out, "report.md")))
})

test_that("resume reruns into a fresh run scope without disturbing prior attempts", {
  out <- withr::local_tempdir()
  f <- scoped_demo_file()

  r1 <- sas_translate(f, out_dir = out, execute = FALSE)
  r2 <- sas_translate(f, out_dir = out, execute = FALSE, resume = TRUE)

  expect_false(identical(r1$run_id, r2$run_id))
  expect_true(dir.exists(file.path(out, "attempts", r1$run_id)))
  expect_true(dir.exists(file.path(out, "attempts", r2$run_id)))
})
