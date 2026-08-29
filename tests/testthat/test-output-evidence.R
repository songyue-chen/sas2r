test_that("no output-review config performs no inventory or reads", {
  cfg <- sas_config(start = withr::local_tempdir())
  expect_false(cfg$output_review$enabled)
  expect_identical(sas2r:::build_output_inventory(NULL, cfg)$inventory,
                   tibble::tibble())
})

test_that("output-review config contains roots, not mappings or policy knobs", {
  root <- withr::local_tempdir()
  writeLines(c(
    "verification:", "  output_review:", "    enabled: true",
    "    r_libraries:", "      adam: r/adam",
    "    mappings: []"
  ), file.path(root, "_sas2r.yml"))
  expect_error(sas_config(file.path(root, "_sas2r.yml")),
               class = "sas2r_config_error")
})

test_that("output-review config requires enabled to be logical", {
  root <- withr::local_tempdir()
  writeLines(c(
    "verification:", "  output_review:", "    enabled: \"yes\"",
    "    r_libraries:", "      adam: r/adam"
  ), file.path(root, "_sas2r.yml"))
  expect_error(sas_config(file.path(root, "_sas2r.yml")),
               class = "sas2r_config_error")
})

test_that("output-review config requires at least one r_libraries root when enabled", {
  root <- withr::local_tempdir()
  writeLines(c(
    "verification:", "  output_review:", "    enabled: true",
    "    r_libraries: {}"
  ), file.path(root, "_sas2r.yml"))
  expect_error(sas_config(file.path(root, "_sas2r.yml")),
               class = "sas2r_config_error")
})

test_that("symlink escape is rejected before a candidate is readable", {
  root <- withr::local_tempdir(); outside <- withr::local_tempdir()
  saveRDS(data.frame(id = 1), file.path(outside, "secret.rds"))
  expect_true(file.symlink(file.path(outside, "secret.rds"),
                           file.path(root, "escaped.rds")))
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

test_that("inventory table matches exact 14 columns and uses opaque candidate IDs", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  sas_dir <- file.path(root, "sas_adam")
  dir.create(r_dir, recursive = TRUE)
  dir.create(sas_dir, recursive = TRUE)

  df_ref <- data.frame(usubjid = c("01", "02"), aval = c(10, 20))
  df_cand <- data.frame(usubjid = c("01", "02"), aval = c(10, 20))

  saveRDS(df_cand, file.path(r_dir, "adsl.rds"))
  haven::write_xpt(df_ref, file.path(sas_dir, "adsl.xpt"))

  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(sas_dir, winslash = "/")),
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  expect_true(cfg$output_review$enabled)

  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  expect_s3_class(inv_obj, "sas2r_output_inventory")
  inv <- inv_obj$inventory

  expected_cols <- c(
    "candidate_id", "side", "root_id", "libref", "relative_name",
    "logical_name", "format", "size_bytes", "file_hash", "status",
    "reason", "nrow", "ncol", "column_signature"
  )
  expect_identical(names(inv), expected_cols)
  expect_equal(nrow(inv), 2L)

  # Check opaque IDs
  expect_true(all(grepl("^candidate_\\d{6}$", inv$candidate_id)))
  expect_true(all(grepl("^root_\\d{4}$", inv$root_id)))

  # Paths are NEVER exposed in inventory table
  for (col in names(inv)) {
    expect_false(any(grepl(root, as.character(inv[[col]]), fixed = TRUE)))
  }

  # Check format values
  expect_setequal(inv$format, c("rds", "xpt"))
  expect_setequal(inv$side, c("reference", "candidate"))
  expect_true(all(inv$status == "available"))
})

test_that("build_output_inventory extracts reference roots from project libref registry", {
  root <- withr::local_tempdir()
  sas_dir <- file.path(root, "sas_adam")
  r_dir <- file.path(root, "r_adam")
  dir.create(sas_dir, recursive = TRUE)
  dir.create(r_dir, recursive = TRUE)

  haven::write_xpt(data.frame(id = 1), file.path(sas_dir, "adsl.xpt"))
  saveRDS(data.frame(id = 1), file.path(r_dir, "adsl.rds"))

  prog_file <- file.path(root, "prog.sas")
  writeLines(paste0("libname adam '", sas_dir, "';\ndata adam.adsl; set adam.adsl; run;"), prog_file)

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  p <- sas_project(root)
  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(p, cfg)

  expect_equal(nrow(inv_obj$inventory), 2L)
  expect_setequal(inv_obj$inventory$side, c("reference", "candidate"))
})

test_that("build_output_inventory ignores unsupported file extensions", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)

  writeLines("csv data", file.path(r_dir, "adsl.csv"))
  writeLines("txt data", file.path(r_dir, "adsl.txt"))
  saveRDS(data.frame(id = 1), file.path(r_dir, "adsl.rds"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  expect_equal(nrow(inv_obj$inventory), 1L)
  expect_identical(inv_obj$inventory$format, "rds")
})

test_that("build_output_inventory marks files over size limit as resource_limit", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)

  saveRDS(data.frame(id = 1:100), file.path(r_dir, "large.rds"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  tiny_limits <- sas2r:::output_evidence_limits(max_file_bytes = 10)
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg, limits = tiny_limits)

  expect_equal(nrow(inv_obj$inventory), 1L)
  expect_identical(inv_obj$inventory$status, "resource_limit")
  expect_identical(inv_obj$inventory$reason, "file_size_limit_exceeded")
})

test_that("read_output_candidate loads data frames and enforces shape", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)

  df <- data.frame(id = 1:5, val = letters[1:5])
  saveRDS(df, file.path(r_dir, "adsl.rds"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  cand_id <- inv_obj$inventory$candidate_id[1]

  loaded <- sas2r:::read_output_candidate(cand_id, inv_obj)
  expect_s3_class(loaded, "data.frame")
  expect_equal(nrow(loaded), 5L)
  expect_equal(ncol(loaded), 2L)
})

test_that("read_output_candidate reads xpt files correctly", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)

  df <- data.frame(id = 1:3, score = c(10.5, 20.0, 30.5))
  haven::write_xpt(df, file.path(r_dir, "adsl.xpt"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  cand_id <- inv_obj$inventory$candidate_id[1]

  loaded <- sas2r:::read_output_candidate(cand_id, inv_obj)
  expect_s3_class(loaded, "data.frame")
  expect_equal(nrow(loaded), 3L)
  expect_equal(loaded$score, c(10.5, 20.0, 30.5))
})

test_that("read_output_candidate rejects non-data.frame RDS objects", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)

  saveRDS(list(a = 1, b = 2), file.path(r_dir, "not_df.rds"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  cand_id <- inv_obj$inventory$candidate_id[1]

  expect_error(
    sas2r:::read_output_candidate(cand_id, inv_obj),
    class = "sas2r_evidence_shape_error"
  )
})

test_that("read_output_candidate enforces resource limits on rows and columns", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)

  df <- data.frame(matrix(1:100, nrow = 10, ncol = 10))
  saveRDS(df, file.path(r_dir, "matrix.rds"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  cand_id <- inv_obj$inventory$candidate_id[1]

  strict_limits <- sas2r:::output_evidence_limits(max_rows = 5L, max_cols = 5L)
  expect_error(
    sas2r:::read_output_candidate(cand_id, inv_obj, limits = strict_limits),
    class = "sas2r_evidence_resource_limit"
  )
})

test_that("resolve_inventory_candidate aborts on unknown candidate ID", {
  inv <- structure(list(inventory = tibble::tibble(), resolver = new.env(parent = emptyenv())),
                   class = "sas2r_output_inventory")
  expect_error(
    sas2r:::resolve_inventory_candidate("candidate_999999", inv),
    class = "sas2r_candidate_not_found"
  )
})

test_that("print.sas2r_output_inventory works cleanly", {
  root <- withr::local_tempdir()
  r_dir <- file.path(root, "r_adam")
  dir.create(r_dir, recursive = TRUE)
  saveRDS(data.frame(id = 1), file.path(r_dir, "adsl.rds"))

  writeLines(c(
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(r_dir, winslash = "/"))
  ), file.path(root, "_sas2r.yml"))

  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  inv_obj <- sas2r:::build_output_inventory(NULL, cfg)
  expect_output(print(inv_obj), "sas2r output inventory")
})

test_that("a device file named like a dataset is never hashed, read, or made available", {
  skip_on_os("windows")
  skip_if_not(capabilities("fifo"), "platform has no FIFOs")

  dir <- withr::local_tempdir()
  ref_dir <- file.path(dir, "ref")
  dir.create(ref_dir, recursive = TRUE)
  fifo_path <- file.path(ref_dir, "adsl.xpt")
  # `fifo()` creates the node; the connection is closed immediately so nothing
  # in this test holds a writer open.
  con <- fifo(fifo_path, open = "w+b")
  close(con)
  expect_true(file.exists(fifo_path))

  saveRDS(data.frame(id = 1L), file.path(ref_dir, "real.xpt"))

  cfg <- structure(list(
    libraries = list(adam = list(path = ref_dir, engine = "xpt", write = "rds")),
    output_review = list(enabled = TRUE, r_libraries = list(adam = ref_dir))
  ), class = "sas2r_config")

  # Reading a FIFO with no writer blocks forever. Neutralising the hash makes
  # an inventory that reaches it fail loudly instead of hanging the suite.
  testthat::local_mocked_bindings(
    hash_file_sha256 = function(...) "0000",
    .package = "cli"
  )

  inv <- sas2r:::build_output_inventory(NULL, cfg)
  fifo_rows <- inv$inventory[inv$inventory$relative_name == "adsl.xpt", ]
  expect_gt(nrow(fifo_rows), 0L)
  expect_true(all(fifo_rows$status != "available"))
  expect_true(all(fifo_rows$reason == "not_a_regular_file"))
  expect_true(all(!nzchar(fifo_rows$file_hash)))

  # The regular file beside it is still inventoried normally.
  real_rows <- inv$inventory[inv$inventory$relative_name == "real.xpt", ]
  expect_true(any(real_rows$status == "available"))
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

test_that("read_output_candidate refuses a device file before opening it", {
  skip_on_os("windows")
  skip_if_not(capabilities("fifo"), "platform has no FIFOs")

  dir <- withr::local_tempdir()
  fifo_path <- file.path(dir, "adsl.xpt")
  close(fifo(fifo_path, open = "w+b"))

  resolver <- new.env(parent = emptyenv())
  resolver[["candidate_000001"]] <- list(
    candidate_id = "candidate_000001", side = "reference", root_id = "root_0001",
    libref = "adam", relative_name = "adsl.xpt", logical_name = "adam.adsl",
    format = "xpt", size_bytes = 0, file_hash = "", status = "available",
    reason = NA_character_,
    path = normalizePath(fifo_path, winslash = "/", mustWork = TRUE),
    root = normalizePath(dir, winslash = "/", mustWork = TRUE)
  )

  # haven::read_xpt() on a FIFO blocks forever. Standing in for it keeps a
  # reader that gets that far a loud failure rather than a hung suite.
  testthat::local_mocked_bindings(
    read_xpt = function(...) stop("reader must not be reached"),
    .package = "haven"
  )

  expect_error(
    sas2r:::read_output_candidate("candidate_000001", resolver),
    class = "sas2r_evidence_shape_error"
  )
})

test_that("a failed libref projection surfaces instead of silently re-deriving roots", {
  dir <- withr::local_tempdir()
  lib_dir <- file.path(dir, "adam")
  dir.create(lib_dir, recursive = TRUE)
  saveRDS(data.frame(id = 1L), file.path(lib_dir, "adsl.rds"))
  writeLines("data adam.adsl; set sdtm.dm; run;", file.path(dir, "adsl.sas"))
  writeLines(c(
    "libraries:", paste0("  adam: ", lib_dir),
    "verification:", "  output_review:", "    enabled: true",
    "    r_libraries:", paste0("      adam: ", lib_dir)
  ), file.path(dir, "_sas2r.yml"))
  p <- sas_project(dir)

  # effective_librefs() is the only projection of reference roots. A failure is
  # not "no roots": re-deriving them from config$libraries would silently skip
  # statement-level overrides, fallback bindings, and the `work` exclusion.
  testthat::local_mocked_bindings(
    effective_librefs = function(project) stop("registry resolution failed"),
    .package = "sas2r"
  )

  expect_error(
    sas2r:::build_output_inventory(p, p$config),
    class = "sas2r_output_evidence_libref_error"
  )
})

# --- Round 2: the guide and the code have to describe the same package -------

test_that("the inventory scan admits exactly the documented formats per side", {
  expect_identical(sas2r:::OUTPUT_REFERENCE_FORMATS, c("sas7bdat", "xpt"))
  expect_identical(sas2r:::OUTPUT_CANDIDATE_FORMATS, c("rds", "xpt"))

  # An `.rds` under a reference root is R output, not SAS output. Admitting it
  # would let a candidate be compared against a copy of itself and pass.
  inv <- output_inventory_fixture(reference = c("adsl.rds", "adsl.xpt"),
                                  candidate = "adsl.rds")
  ref_rows <- inv$inventory[inv$inventory$side == "reference", ]
  expect_identical(sort(unique(ref_rows$format)), "xpt")
})

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

  # 1. Documented input formats are the ones the inventory scan admits.
  formats_named <- function(label) {
    m <- regmatches(doc, regexpr(paste0(label, " \\([^)]*\\)"), doc))
    expect_length(m, 1L)
    sort(unique(sub("^\\.", "", regmatches(m, gregexpr("\\.[a-z0-9]+", m))[[1]])))
  }
  expect_identical(formats_named("authentic SAS outputs"),
                   sort(sas2r:::OUTPUT_REFERENCE_FORMATS))
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

  # 3. Every target status the guide names is a status the plan can carry.
  documented_statuses <- intersect(idents, c(
    sas2r:::OUTPUT_TARGET_STATUSES,
    "candidate_missing", "ambiguous_candidates", "dataset_size_exceeded"
  ))
  expect_true(all(documented_statuses %in% sas2r:::OUTPUT_TARGET_STATUSES))

  # 4. The audit-trail path the guide sends an auditor to is the one written.
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
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_ROOTS, 50L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_DEPTH, 10L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_FILES_PER_ROOT, 500L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_FILE_BYTES, 52428800)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_ROWS, 50000L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_COLS, 500L)
  expect_identical(sas2r:::OUTPUT_EVIDENCE_MAX_EXAMPLES, 20L)

  # And the no-argument limits object is those constants, not a second set of
  # defaults that could drift away from them.
  lim <- output_evidence_limits()
  expect_identical(lim$max_roots, 50L)
  expect_identical(lim$max_depth, 10L)
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

test_that("default file-per-root cap bounds an inventory built with no limits", {
  dir <- withr::local_tempdir()
  ref_dir <- file.path(dir, "ref")
  dir.create(ref_dir, recursive = TRUE)
  for (i in seq_len(505L)) {
    saveRDS(data.frame(x = i), file.path(ref_dir, sprintf("ds_%04d.rds", i)))
  }
  cfg <- structure(list(
    libraries = list(adam = list(path = ref_dir, engine = "rds", write = "rds")),
    output_review = list(enabled = TRUE, r_libraries = list(adam = ref_dir))
  ), class = "sas2r_config")

  inv <- sas2r:::build_output_inventory(NULL, cfg)
  expect_identical(sum(inv$inventory$status == "available"), 500L)
  expect_true(any(inv$inventory$reason %in% "max_files_per_root_exceeded"))
})
