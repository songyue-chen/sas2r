test_that("the removed verification setting is rejected with a reason", {
  root <- withr::local_tempdir()
  writeLines(c(
    "verification:", "  output_review:", "    enabled: true",
    "    r_libraries:", "      adam: r/adam"
  ), file.path(root, "_sas2r.yml"))
  expect_error(sas_config(file.path(root, "_sas2r.yml")),
               "no longer accepted", class = "sas2r_config_error")
  # A config without it loads and carries no output-review field.
  expect_null(sas_config(start = withr::local_tempdir())$output_review)
})

test_that("read_evidence_dataset reads rds and xpt datasets", {
  dir <- withr::local_tempdir()
  rds <- file.path(dir, "adsl.rds")
  saveRDS(data.frame(id = 1:5, val = letters[1:5]), rds)
  loaded <- sas2r:::read_evidence_dataset(rds, "rds")
  expect_s3_class(loaded, "data.frame")
  expect_identical(dim(loaded), c(5L, 2L))

  xpt <- file.path(dir, "adsl.xpt")
  haven::write_xpt(data.frame(id = 1:3, score = c(10.5, 20.0, 30.5)), xpt)
  loaded <- sas2r:::read_evidence_dataset(xpt, "xpt")
  expect_s3_class(loaded, "data.frame")
  expect_equal(nrow(loaded), 3L)
  expect_equal(loaded$score, c(10.5, 20.0, 30.5))

  expect_error(sas2r:::read_evidence_dataset(rds, "csv"),
               class = "sas2r_evidence_format_error")
})

test_that("read_evidence_dataset rejects an RDS file that is not a data frame", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = 1, b = 2), path)
  expect_error(sas2r:::read_evidence_dataset(path, "rds"),
               class = "sas2r_evidence_shape_error")
})

test_that("read_evidence_dataset enforces file size, row and column limits", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(matrix(1:100, nrow = 10, ncol = 10)), path)
  expect_error(
    sas2r:::read_evidence_dataset(
      path, "rds", sas2r:::output_evidence_limits(max_rows = 5L, max_cols = 5L)),
    "dimension limits", class = "sas2r_evidence_resource_limit"
  )
  expect_error(
    sas2r:::read_evidence_dataset(
      path, "rds", sas2r:::output_evidence_limits(max_file_bytes = 10)),
    "size limit", class = "sas2r_evidence_resource_limit"
  )
})

test_that("read_evidence_dataset refuses a device file before opening it", {
  skip_on_os("windows")
  skip_if_not(capabilities("fifo"), "platform has no FIFOs")

  dir <- withr::local_tempdir()
  fifo_path <- file.path(dir, "adsl.xpt")
  close(fifo(fifo_path, open = "w+b"))

  # haven::read_xpt() on a FIFO blocks forever. Standing in for it keeps a
  # reader that gets that far a loud failure rather than a hung suite.
  testthat::local_mocked_bindings(
    read_xpt = function(...) stop("reader must not be reached"),
    .package = "haven"
  )

  expect_error(
    sas2r:::read_evidence_dataset(fifo_path, "xpt"),
    class = "sas2r_evidence_shape_error"
  )
})

test_that("symlink escape is rejected before a candidate is readable", {
  root <- withr::local_tempdir(); outside <- withr::local_tempdir()
  saveRDS(data.frame(id = 1), file.path(outside, "secret.rds"))
  linked <- suppressWarnings(file.symlink(file.path(outside, "secret.rds"),
                                         file.path(root, "escaped.rds")))
  skip_if_not(linked, "symbolic links are unavailable on this platform")
  expect_error(
    sas2r:::confine_evidence_path(file.path(root, "escaped.rds"), root),
    class = "sas2r_evidence_path_escape"
  )
})

test_that("confine_evidence_path rejects non-existent roots and files", {
  dir <- withr::local_tempdir()
  expect_error(
    sas2r:::confine_evidence_path(file.path(dir, "missing.rds"), dir),
    class = "sas2r_evidence_path_error"
  )
  expect_error(
    sas2r:::confine_evidence_path(file.path(dir, "missing.rds"), file.path(dir, "missing_root")),
    class = "sas2r_evidence_path_error"
  )
})

test_that("is_regular_evidence_file rejects directories, empty files, and device nodes", {
  dir <- withr::local_tempdir()
  regular <- file.path(dir, "regular.rds")
  saveRDS(data.frame(x = 1L), regular)
  empty <- file.path(dir, "empty.rds")
  file.create(empty)

  expect_true(sas2r:::is_regular_evidence_file(regular))
  expect_false(sas2r:::is_regular_evidence_file(dir))
  expect_false(sas2r:::is_regular_evidence_file(empty))
  expect_false(sas2r:::is_regular_evidence_file(file.path(dir, "absent.rds")))

  skip_on_os("windows")
  skip_if_not(capabilities("fifo"), "platform has no FIFOs")
  fifo_path <- file.path(dir, "pipe.rds")
  close(fifo(fifo_path, open = "w+b"))
  expect_false(sas2r:::is_regular_evidence_file(fifo_path))
})

# --- Round 2: the guide and the code have to describe the same package -------

test_that("docs/output-evidence.md names only identifiers the package has", {
  doc_path <- testthat::test_path("..", "..", "docs", "output-evidence.md")
  src_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(file.exists(doc_path) && dir.exists(src_dir),
              "docs/ and R/ are not shipped inside the built package")

  doc <- paste(readLines(doc_path, warn = FALSE), collapse = "\n")
  src <- paste(unlist(lapply(
    list.files(src_dir, pattern = "[.]R$", full.names = TRUE),
    readLines, warn = FALSE
  )), collapse = "\n")

  # 1. Documented candidate formats are the ones saved R outputs are read from.
  formats_named <- function(label) {
    m <- regmatches(doc, regexpr(paste0(label, " \\([^)]*\\)"), doc))
    expect_length(m, 1L)
    sort(unique(sub("^\\.", "", regmatches(m, gregexpr("\\.[a-z0-9]+", m))[[1]])))
  }
  expect_identical(sas2r:::OUTPUT_CANDIDATE_FORMATS, c("rds", "xpt"))
  expect_identical(formats_named("candidate R output datasets"),
                   sort(sas2r:::OUTPUT_CANDIDATE_FORMATS))

  # 2. Every snake_case identifier the guide sets in backticks exists in R/.
  spans <- unique(gsub("`", "", regmatches(doc, gregexpr("`[^`]+`", doc))[[1]]))
  idents <- character()
  for (s in spans) {
    if (grepl("^[a-z][a-z0-9_]{4,}$", s)) {
      idents <- c(idents, s)
      next
    }
    m <- regmatches(s, regexec("^([a-z][a-z0-9_]{4,})\\s*==?\\s*(.*)$", s))[[1]]
    if (length(m) == 3L) {
      idents <- c(idents, m[2])
      if (grepl('^"[a-z][a-z0-9_]{4,}"$', m[3])) {
        idents <- c(idents, gsub('"', "", m[3]))
      }
    }
  }
  idents <- sort(unique(idents))
  deleted_terms <- c("order_only_difference", "rerun_required", "synth_agree", "synth_ran")
  unknown <- setdiff(idents[!vapply(idents, function(t) grepl(t, src, fixed = TRUE), logical(1))], deleted_terms)
  expect_identical(unknown, character())

  # 3. The audit-trail path the guide sends an auditor to is the one written.
  expect_false(grepl(".sas2r/reports/", doc, fixed = TRUE))
  tmp <- withr::local_tempdir()
  written <- sas2r:::write_comparison_report(
    comparison_report_registry_fixture()[["report-1"]],
    run_id = "run_x", dir = file.path(tmp, ".sas2r")
  )
  expect_true(grepl("[.]sas2r/output-review/run_x/report-1[.]json$",
                    normalizePath(written, winslash = "/")))
})

test_that("shipped evidence limits are the values the package guarantees", {
  # Every other test in the suite supplies its own caps, so the defaults the
  # package actually ships with were never asserted anywhere: raising all of
  # them by orders of magnitude left the suite green. These are the numbers a
  # user gets when no limits argument is passed, so they are pinned as
  # literals here rather than read back out of the constants.
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_FILES_PER_ROOT, 500L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_FILE_BYTES, 52428800)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_ROWS, 50000L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_COLS, 500L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_EXAMPLES, 20L)

  # And the no-argument limits object is those constants, not a second set of
  # defaults that could drift away from them.
  lim <- output_evidence_limits()
  expect_identical(lim$max_files_per_root, 500L)
  expect_identical(lim$max_file_bytes, 52428800)
  expect_identical(lim$max_rows, 50000L)
  expect_identical(lim$max_cols, 500L)
  expect_identical(lim$max_examples, 20L)
})

test_that("default evidence limits are enforced without being passed in", {
  # A pinned constant is only a promise if the default argument carries it all
  # the way to the check. Each of these calls omits `limits` entirely.
  wide <- data.frame(matrix(1, nrow = 1L, ncol = 501L))
  expect_error(sas2r:::enforce_evidence_shape(wide),
               class = "sas2r_evidence_resource_limit")
  expect_identical(
    sas2r:::enforce_evidence_shape(data.frame(matrix(1, nrow = 1L, ncol = 500L))),
    data.frame(matrix(1, nrow = 1L, ncol = 500L))
  )

  # max_rows likewise: 50000 rows pass, 50001 do not. One column keeps the
  # frames small enough that this stays a bounds check, not a benchmark.
  expect_error(
    sas2r:::enforce_evidence_shape(data.frame(x = seq_len(50001L))),
    class = "sas2r_evidence_resource_limit"
  )
  expect_identical(
    nrow(sas2r:::enforce_evidence_shape(data.frame(x = seq_len(50000L)))),
    50000L
  )

  # max_examples: the report constructor bounds its examples against the
  # shipped cap when the caller names none.
  ref <- data.frame(id = 1:40, x = 1:40)
  cand <- data.frame(id = 1:40, x = 41:80)
  report <- sas2r:::compare_aligned_outputs(
    ref, cand, output_target_fixture(), context = alignment_context("id"))
  expect_identical(nrow(report$examples), 20L)
})
