test_that("normalized report retains bounded diagnostic examples, not frames", {
  ref <- data.frame(usubjid = sprintf("S%03d", 1:30), aval = 1:30)
  cand <- ref; cand$aval[1:25] <- cand$aval[1:25] + 1
  target <- output_target_fixture(unit_ids = 7L)
  report <- sas2r:::compare_aligned_outputs(
    ref, cand, target,
    context = alignment_context("usubjid")
  )
  expect_s3_class(report, "sas2r_comparison_report")
  expect_lte(nrow(report$examples), sas2r:::output_evidence_limits()$max_examples)
  expect_true(all(c("reference_row", "candidate_row", "key_values",
                    "variable", "reference_value", "candidate_value") %in%
                  names(report$examples)))
  # No dataset payload may ride along anywhere in the report, at any depth and
  # under any field name. Only these bounded diagnostic frames are permitted;
  # a new frame at any other path -- e.g. an embedded copy of either input --
  # is a leak regardless of what it is called.
  collect_frames <- function(x, path = "") {
    if (inherits(x, "data.frame")) {
      out <- list(x)
      names(out) <- path
      return(out)
    }
    if (!is.list(x)) return(list())
    nms <- names(x)
    out <- list()
    for (i in seq_along(x)) {
      part <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else as.character(i)
      out <- c(out, collect_frames(x[[i]], paste0(path, "$", part)))
    }
    out
  }
  frames <- collect_frames(report)
  expect_setequal(
    names(frames),
    c("$structure$column_kind_mismatches", "$structure$unsupported_column_kinds",
      "$mismatches$by_variable", "$mismatches$cosmetic", "$examples")
  )
  # ...and each permitted frame is bounded, so none can be a copy of the inputs.
  for (nm in names(frames)) {
    expect_lte(nrow(frames[[nm]]), sas2r:::output_evidence_limits()$max_examples)
  }
  expect_lt(max(vapply(frames, nrow, integer(1))), nrow(ref))
})

test_that("new output reports do not change legacy comparison authority", {
  b <- data.frame(id = 1, x = 1)
  c <- data.frame(id = 1, x = 2)
  legacy <- compare_datasets(b, c, keys = "id")
  expect_false(passed(legacy))
  report <- sas2r:::compare_aligned_outputs(
    b, c, output_target_fixture(), context = alignment_context("id")
  )
  expect_null(report$passed)
  expect_false("gate_reached" %in% names(report))
})

test_that("comparison report tool resolves IDs and rejects paths", {
  registry <- comparison_report_registry_fixture()
  expect_s3_class(sas2r:::read_comparison_report("report-1", registry),
                  "sas2r_comparison_report")
  # A path must be refused *as a path*; asserting only the shared parent class
  # would be satisfied by the ordinary unknown-identifier branch.
  for (bad in c("../report.json", "..", "/etc/passwd", "reports\\report-1")) {
    err <- expect_error(sas2r:::read_comparison_report(bad, registry),
                        class = "sas2r_report_path_rejected")
    expect_s3_class(err, "sas2r_report_not_registered")
  }
  missing <- expect_error(sas2r:::read_comparison_report("missing-report", registry),
                          class = "sas2r_report_not_registered")
  expect_false(inherits(missing, "sas2r_report_path_rejected"))
})

test_that("report contains exact 21 top-level fields", {
  ref <- data.frame(usubjid = "S001", aval = 10)
  cand <- data.frame(usubjid = "S001", aval = 20)
  target <- output_target_fixture(unit_ids = 1L)
  report <- sas2r:::compare_aligned_outputs(
    ref, cand, target,
    context = alignment_context("usubjid")
  )

  expected_fields <- c(
    "schema_version", "report_id", "target_id", "logical_dataset", "role",
    "contributing_unit_ids", "staged_code_hashes", "structure", "alignment",
    "order", "mismatches", "examples", "resource_state", "truncated",
    "truncated_fields", "stale", "stale_reason", "reference_hash",
    "candidate_hash", "profile_hash", "schema_hash"
  )
  expect_identical(names(report), expected_fields)
  expect_identical(report$schema_version, 2L)
  expect_identical(report$target_id, "target_0001")
  expect_identical(report$logical_dataset, "adam.adsl")
  expect_identical(report$role, "final_dataset")
  expect_identical(report$contributing_unit_ids, 1L)
  expect_true(nzchar(report$report_id))
  expect_true(nzchar(report$reference_hash))
  expect_true(nzchar(report$candidate_hash))
  expect_true(nzchar(report$profile_hash))
  expect_true(nzchar(report$schema_hash))
})

test_that("example rows are deterministically prioritized and capped with truncated = TRUE", {
  ref <- data.frame(
    id = sprintf("ID%02d", 1:50),
    v_num = 1:50,
    v_chr = letters[1:50],
    stringsAsFactors = FALSE
  )
  cand <- ref
  cand$v_num[1:30] <- cand$v_num[1:30] + 100
  cand$v_chr[21:50] <- toupper(cand$v_chr[21:50])

  target <- output_target_fixture(unit_ids = 5L)
  report <- sas2r:::compare_aligned_outputs(
    ref, cand, target,
    context = alignment_context("id")
  )

  expect_equal(nrow(report$examples), 20L)
  expect_true(report$truncated)
  expect_true(all(report$examples$variable[1:20] %in% c("v_num", "v_chr")))
})

test_that("sentinel rows beyond cap are absent from examples and serialized JSON", {
  ref <- data.frame(
    id = sprintf("ID%03d", 1:100),
    val = sprintf("SECRET_VAL_%03d", 1:100),
    stringsAsFactors = FALSE
  )
  cand <- ref
  cand$val <- sprintf("MODIFIED_VAL_%03d", 1:100)

  target <- output_target_fixture(unit_ids = 1L)
  report <- sas2r:::compare_aligned_outputs(
    ref, cand, target,
    context = alignment_context("id")
  )

  expect_equal(nrow(report$examples), 20L)
  expect_true(report$truncated)

  # Check sentinel row 99 is NOT in examples
  expect_false("SECRET_VAL_099" %in% report$examples$reference_value)
  expect_false("MODIFIED_VAL_099" %in% report$examples$candidate_value)

  # Write report to JSON and check content
  dir <- withr::local_tempdir()
  json_file <- sas2r:::write_comparison_report(report, run_id = "test_run", dir = dir)
  expect_true(file.exists(json_file))
  json_text <- readLines(json_file, warn = FALSE)
  expect_false(any(grepl("SECRET_VAL_099", json_text, fixed = TRUE)))
  expect_true(any(grepl("SECRET_VAL_001", json_text, fixed = TRUE)))
})

test_that("write_comparison_report supports both legacy sas2r_comparison and sas2r_comparison_report", {
  dir <- withr::local_tempdir()

  # 1. Legacy comparison object writes markdown
  b <- data.frame(id = 1:2, x = c("a", "b"))
  c <- data.frame(id = 1:2, x = c("a", "c"))
  legacy <- compare_datasets(b, c, keys = "id")
  md_file <- file.path(dir, "legacy_report.md")
  res_md <- write_comparison_report(legacy, md_file)
  expect_identical(res_md, md_file)
  expect_true(file.exists(md_file))
  expect_match(readLines(md_file)[1], "# Dataset comparison")

  # 2. Modern report object writes JSON
  target <- output_target_fixture()
  report <- sas2r:::compare_aligned_outputs(b, c, target, context = alignment_context("id"))
  json_file <- file.path(dir, "modern_report.json")
  res_json <- write_comparison_report(report, file = json_file)
  expect_identical(res_json, json_file)
  expect_true(file.exists(json_file))
  parsed <- jsonlite::fromJSON(json_file)
  expect_identical(parsed$schema_version, 2L)
  expect_identical(parsed$target_id, "target_0001")
})

test_that("report cannot modify verification units or clear review floor", {
  ref <- data.frame(id = 1, val = 1)
  cand <- data.frame(id = 1, val = 2)
  target <- output_target_fixture(unit_ids = 3L)
  report <- sas2r:::compare_aligned_outputs(ref, cand, target, context = alignment_context("id"))

  expect_null(report$diff_passed)
  expect_null(report$passed)
  expect_false("verification" %in% names(report))
  expect_false("approval" %in% names(report))
})

test_that("read_comparison_report tool works inside build_tools context", {
  registry <- comparison_report_registry_fixture()
  ctx <- list(report_registry = registry)
  spec <- list(tools = list(read_comparison_report = list(max_calls = 5L)))
  tools <- sas2r:::build_tools(spec, ctx)

  expect_true("read_comparison_report" %in% names(tools))
  tool_res <- tools$read_comparison_report$call(list(report_id = "report-1"))
  expect_s3_class(tool_res, "sas2r_comparison_report")
  expect_identical(tool_res$report_id, "report-1")

  expect_error(tools$read_comparison_report$call(list(report_id = "report-1", extra = 123)),
               class = "sas2r_tool_arguments_error")
})

# --- Round 2: caps sized to the byte budget, truncation that says what ------

test_that("an ADSL-shaped comparison is reported whole, not abridged", {
  n <- 150L
  mk <- function(v) stats::setNames(
    lapply(seq_len(n), function(i) rep(v, 3L)),
    sprintf("col%03d", seq_len(n))
  )
  ref <- tibble::as_tibble(c(list(usubjid = c("01", "02", "03")), mk("same")))
  cand <- ref
  cand[["col001"]] <- c("x", "same", "same")
  cand[["col002"]] <- c("same", "y", "same")

  report <- sas2r:::compare_aligned_outputs(
    ref, cand, output_target_fixture(),
    context = alignment_context("usubjid")
  )

  # 151 columns and two differing cells is the most ordinary real input there
  # is. A structurally abridged report on that is a defect, not a safeguard.
  expect_length(report$structure$columns_common, 151L)
  expect_equal(report$mismatches$total_mismatch_cells, 2L)
  expect_false(isTRUE(report$truncated))
  expect_identical(report$truncated_fields, character())
  expect_lt(sas2r:::comparison_report_serialized_bytes(report),
            sas2r:::OUTPUT_REPORT_MAX_SERIALIZED_BYTES)
})

test_that("truncated_fields names an abridged column list and nothing else", {
  n <- sas2r:::OUTPUT_REPORT_MAX_COLUMN_NAMES + 10L
  mk <- function(v) stats::setNames(
    lapply(seq_len(n), function(i) rep(v, 2L)),
    sprintf("c%05d", seq_len(n))
  )
  ref <- tibble::as_tibble(c(list(usubjid = c("01", "02")), mk("same")))
  cand <- ref
  cand[["c00001"]] <- c("x", "same")

  report <- sas2r:::compare_aligned_outputs(
    ref, cand, output_target_fixture(),
    context = alignment_context("usubjid")
  )

  expect_true(isTRUE(report$truncated))
  expect_true("columns_common" %in% report$truncated_fields)
  expect_false("examples" %in% report$truncated_fields)
  expect_false("example_values" %in% report$truncated_fields)
})

test_that("truncated_fields distinguishes dropped example rows from clipped values", {
  ref <- data.frame(
    id = sprintf("ID%02d", 1:50),
    v_num = 1:50,
    stringsAsFactors = FALSE
  )
  cand <- ref
  cand$v_num <- cand$v_num + 100L

  dropped <- sas2r:::compare_aligned_outputs(
    ref, cand, output_target_fixture(),
    context = alignment_context("id")
  )
  expect_true("examples" %in% dropped$truncated_fields)
  expect_false("example_values" %in% dropped$truncated_fields)

  long <- paste(rep("Q", 400L), collapse = "")
  ref2 <- data.frame(usubjid = c("01", "02"), note = c(long, "short"),
                     stringsAsFactors = FALSE)
  cand2 <- data.frame(usubjid = c("01", "02"), note = c(paste0(long, "X"), "short"),
                      stringsAsFactors = FALSE)
  clipped <- sas2r:::compare_aligned_outputs(
    ref2, cand2, output_target_fixture(),
    context = alignment_context("usubjid")
  )
  expect_true("example_values" %in% clipped$truncated_fields)
  expect_false("examples" %in% clipped$truncated_fields)
})

test_that("truncated and truncated_fields cannot disagree", {
  ref <- data.frame(usubjid = c("01", "02"), aval = c(1, 2))
  cand <- data.frame(usubjid = c("01", "02"), aval = c(1, 3))
  report <- sas2r:::compare_aligned_outputs(ref, cand, output_target_fixture())

  bad <- report
  bad$truncated <- TRUE
  expect_error(sas2r:::validate_comparison_report(bad),
               class = "sas2r_invalid_report_error")

  bad2 <- report
  bad2$truncated_fields <- "examples"
  expect_error(sas2r:::validate_comparison_report(bad2),
               class = "sas2r_invalid_report_error")
})

# The closed-schema and limits gate. It runs twice on every report the model
# can see -- once when new_comparison_report() assembles it, and again when
# read_comparison_report() resolves a registered ID for the reviewer's tool --
# so both call sites are exercised here, not just the function in isolation.

test_that("validate_comparison_report accepts a well-formed report unchanged", {
  ref <- data.frame(usubjid = c("01", "02"), aval = c(1, 2))
  cand <- data.frame(usubjid = c("01", "02"), aval = c(1, 3))
  report <- sas2r:::compare_aligned_outputs(ref, cand, output_target_fixture())

  expect_invisible(sas2r:::validate_comparison_report(report))
  expect_identical(sas2r:::validate_comparison_report(report), report)
  expect_silent(sas2r:::validate_comparison_report(report))
})

test_that("every field of the closed report schema is required", {
  report <- comparison_report_registry_fixture()[["report-1"]]
  # The fixture is a complete report to begin with: dropping fields from an
  # already-invalid object would prove nothing.
  expect_invisible(sas2r:::validate_comparison_report(report))
  expect_setequal(names(report), sas2r:::OUTPUT_REPORT_FIELDS)

  for (field in sas2r:::OUTPUT_REPORT_FIELDS) {
    missing_one <- report[setdiff(names(report), field)]
    class(missing_one) <- class(report)
    expect_error(sas2r:::validate_comparison_report(missing_one),
                 class = "sas2r_invalid_report_error",
                 info = paste("dropping field", field))
  }

  expect_error(sas2r:::validate_comparison_report("not a list"),
               class = "sas2r_invalid_report_error")
})

test_that("validate_comparison_report enforces identity, shape, and value bounds", {
  base <- comparison_report_registry_fixture()[["report-1"]]
  invalid <- function(mutate) {
    bad <- mutate(base)
    expect_error(sas2r:::validate_comparison_report(bad),
                 class = "sas2r_invalid_report_error")
  }

  # Schema version is pinned, not merely present.
  invalid(function(r) { r$schema_version <- sas2r:::OUTPUT_REPORT_SCHEMA_VERSION + 1L; r })
  # Report IDs address a registry, so a path-shaped or empty ID is refused.
  invalid(function(r) { r$report_id <- "../etc/passwd"; r })
  invalid(function(r) { r$report_id <- "sub/report-1"; r })
  invalid(function(r) { r$report_id <- ""; r })
  invalid(function(r) { r$report_id <- c("report-1", "report-2"); r })
  invalid(function(r) { r$target_id <- ""; r })
  # Examples are a data frame with a fixed column set.
  invalid(function(r) { r$examples <- list(reference_row = 1L); r })
  invalid(function(r) { r$examples <- r$examples[, setdiff(names(r$examples), "variable")]; r })
  # truncated_fields is a closed vocabulary of character.
  invalid(function(r) { r$truncated <- TRUE; r$truncated_fields <- "not_a_slot"; r })
  invalid(function(r) { r$truncated_fields <- NA_character_; r$truncated <- TRUE; r })
  invalid(function(r) { r$truncated <- NA; r })
  # A full dataset frame may never ride along on the report.
  invalid(function(r) { r$reference <- data.frame(x = 1); r })
  invalid(function(r) { r$candidate <- data.frame(x = 1); r })

  # Example rows answer to the shipped evidence cap.
  over_examples <- base
  over_examples$examples <- tibble::tibble(
    reference_row = 1:21, candidate_row = 1:21,
    key_values = sprintf("%02d", 1:21), variable = "aval",
    reference_value = "1", candidate_value = "2"
  )
  expect_error(sas2r:::validate_comparison_report(over_examples),
               class = "sas2r_invalid_report_error")
  under_examples <- over_examples
  under_examples$examples <- over_examples$examples[1:20, ]
  expect_invisible(sas2r:::validate_comparison_report(under_examples))

  # Each remaining model-visible collection answers to its own cap.
  limits <- sas2r:::output_report_limits()
  slot_over <- list(
    columns_common = function(r, n) { r$structure$columns_common <- sprintf("c%05d", seq_len(n)); r },
    columns_only_reference = function(r, n) { r$structure$columns_only_reference <- sprintf("c%05d", seq_len(n)); r },
    columns_only_candidate = function(r, n) { r$structure$columns_only_candidate <- sprintf("c%05d", seq_len(n)); r },
    candidate_keys = function(r, n) { r$alignment$candidate_keys <- sprintf("k%05d", seq_len(n)); r },
    by_variable = function(r, n) { r$mismatches$by_variable <- tibble::tibble(variable = sprintf("v%05d", seq_len(n))); r },
    cosmetic = function(r, n) { r$mismatches$cosmetic <- tibble::tibble(variable = sprintf("v%05d", seq_len(n))); r }
  )
  caps <- c(
    columns_common = limits$max_column_names,
    columns_only_reference = limits$max_column_names,
    columns_only_candidate = limits$max_column_names,
    candidate_keys = limits$max_candidate_keys,
    by_variable = limits$max_variable_rows,
    cosmetic = limits$max_variable_rows
  )
  for (slot in names(slot_over)) {
    cap <- caps[[slot]]
    expect_invisible(sas2r:::validate_comparison_report(slot_over[[slot]](base, cap)))
    expect_error(sas2r:::validate_comparison_report(slot_over[[slot]](base, cap + 1L)),
                 class = "sas2r_invalid_report_error",
                 info = paste("slot", slot))
  }
})

test_that("an oversized report is refused when the constructor assembles it", {
  # bound_comparison_report() is what normally keeps the object inside the
  # caps, so neutralizing it is the way to ask whether anything downstream
  # still checks. new_comparison_report() must refuse to hand back a report
  # that exceeds them even though it built it itself.
  testthat::local_mocked_bindings(
    bound_comparison_report = function(report, limits = output_report_limits()) {
      report$examples <- tibble::tibble(
        reference_row = 1:21, candidate_row = 1:21,
        key_values = sprintf("%02d", 1:21), variable = "aval",
        reference_value = "1", candidate_value = "2"
      )
      list(report = report, omitted = character())
    },
    .package = "sas2r"
  )
  ref <- data.frame(usubjid = c("01", "02"), aval = c(1, 2))
  cand <- data.frame(usubjid = c("01", "02"), aval = c(1, 3))
  expect_error(
    sas2r:::compare_aligned_outputs(ref, cand, output_target_fixture()),
    class = "sas2r_invalid_report_error"
  )
})

test_that("a malformed registered report is refused when the model tool resolves it", {
  registry <- comparison_report_registry_fixture()
  good <- registry[["report-1"]]

  malformed <- good
  malformed$schema_hash <- NULL
  malformed$report_id <- "report-malformed"
  registry[["report-malformed"]] <- malformed

  oversize <- good
  oversize$report_id <- "report-oversize"
  oversize$structure$columns_common <- sprintf(
    "c%05d", seq_len(sas2r:::output_report_limits()$max_column_names + 1L))
  registry[["report-oversize"]] <- oversize

  expect_s3_class(sas2r:::read_comparison_report("report-1", registry),
                  "sas2r_comparison_report")
  expect_error(sas2r:::read_comparison_report("report-malformed", registry),
               class = "sas2r_invalid_report_error")
  expect_error(sas2r:::read_comparison_report("report-oversize", registry),
               class = "sas2r_invalid_report_error")

  # And through the tool the reviewer agent is actually handed.
  tools <- sas2r:::build_tools(
    list(tools = list(read_comparison_report = list(max_calls = 8L))),
    list(report_registry = registry)
  )
  expect_s3_class(tools$read_comparison_report$call(list(report_id = "report-1")),
                  "sas2r_comparison_report")
  expect_error(tools$read_comparison_report$call(list(report_id = "report-malformed")),
               class = "sas2r_invalid_report_error")
  expect_error(tools$read_comparison_report$call(list(report_id = "report-oversize")),
               class = "sas2r_invalid_report_error")
})
