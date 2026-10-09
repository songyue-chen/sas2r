# Test the migration workflow with a deterministic translator and reviewer.
# This covers synthetic data and PDF content, not independent SAS equivalence.

test_that("demo input generation only writes to an explicit destination", {
  script <- system.file("examples", "migration-demo", "make-input.R", package = "sas2r")
  expect_true(file.exists(script))
  elsewhere <- withr::local_tempdir()
  withr::local_dir(elsewhere)
  demo_input <- new.env()
  sys.source(script, envir = demo_input)
  expect_error(demo_input$make_demo_input(), "argument.*missing")
  # Even beside demo.sas, running the script needs an explicit destination.
  writeLines("data work.out; x=1; run;", "demo.sas")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(output, "status"), 1L)
  expect_match(paste(output, collapse = "\n"), "Pass the destination data directory")
  expect_false(dir.exists("data"))

  # The command-line destination works even when the caller is elsewhere.
  destination <- file.path(elsewhere, "copied demo", "data")
  output <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(script), shQuote(destination)),
                    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"))
  expect_equal(nrow(readRDS(file.path(destination, "input_ds.rds"))), 5L)
  expect_false(dir.exists("data"))
})
