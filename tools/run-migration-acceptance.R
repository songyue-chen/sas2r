#!/usr/bin/env Rscript

# Acceptance Runner for sas2r Migration Pipeline
# Deterministic offline workflow checks using a synthetic study and mock agents.
# These checks do not establish SAS equivalence or live translation quality.

if (!"--installed" %in% commandArgs(trailingOnly = TRUE) && file.exists("DESCRIPTION") && any(grepl("^Package:\\s*sas2r", readLines("DESCRIPTION", warn = FALSE)))) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(quiet = TRUE)
  } else {
    library(sas2r)
  }
} else if (!requireNamespace("sas2r", quietly = TRUE)) {
  stop("sas2r package not found")
} else {
  library(sas2r)
}

# Fixture response builders are test assets, not installed package exports.
# Resolve them from this script's repository without relying on pkgload helpers.
script_file <- if (sys.nframe() > 0L) sys.frame(1L)$ofile else NULL
if (is.null(script_file)) {
  script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
}
repo_root <- dirname(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)))
source(file.path(repo_root, "tests", "testthat", "helper-agents.R"), local = TRUE)
`%||%` <- get("%||%", envir = asNamespace("sas2r"), inherits = TRUE)

# --- CLI Argument Parsing ---
args <- commandArgs(trailingOnly = TRUE)
unknown_options <- args[grepl("^--", args) &
  !args %in% c("--installed", "--fixture", "--artifacts") &
  !grepl("^--artifacts=", args)]
if (length(unknown_options)) {
  stop("Unknown option: ", paste(unknown_options, collapse = ", "),
       ". Use --fixture, --installed and --artifacts.", call. = FALSE)
}

get_arg_value <- function(args, prefix, default = NULL) {
  match_arg <- args[grepl(paste0("^", prefix, "="), args)]
  if (length(match_arg) > 0L) {
    return(sub(paste0("^", prefix, "="), "", match_arg[1L]))
  }
  idx <- which(args == prefix)
  if (length(idx) > 0L && idx[1L] < length(args)) {
    return(args[idx[1L] + 1L])
  }
  default
}

artifacts_dir <- get_arg_value(
  args,
  "--artifacts",
  default = file.path(tempdir(), "sas2r-acceptance-fixture")
)
dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)

# --- Helper Functions ---

compute_file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(cli::hash_file_sha256(path))
}

# --- FIXTURE ACCEPTANCE ---

run_fixture_acceptance <- function(artifacts_dir) {
  cat("\n========================================================================\n")
  cat("               sas2r Deterministic Fixture Acceptance Gate              \n")
  cat("========================================================================\n\n")

  tmp <- tempfile("fixture_acceptance_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Setup fixture project
  data_adam <- file.path(tmp, "data", "adam")
  ref_adam <- file.path(tmp, "ref", "adam")
  dir.create(data_adam, recursive = TRUE)
  dir.create(ref_adam, recursive = TRUE)

  # 1. Inputs
  adsl_in <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    TRTP = c("Placebo", "Active", "Active", "Placebo"),
    AVAL = c(10.0, 20.0, 30.0, 40.0),
    stringsAsFactors = FALSE
  )
  saveRDS(adsl_in, file.path(data_adam, "adsl.rds"))

  # 2. Reference output
  adsl_out_ref <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    TRTP = c("Placebo", "Active", "Active", "Placebo"),
    AVAL = c(10.0, 20.0, 30.0, 40.0),
    AVAL_DOUBLE = c(20.0, 40.0, 60.0, 80.0),
    stringsAsFactors = FALSE
  )
  saveRDS(adsl_out_ref, file.path(ref_adam, "adsl_out.rds"))

  # 3. Programs
  p1_file <- file.path(tmp, "01_adsl_prep.sas")
  writeLines(c(
    "data adam.adsl_out;",
    "  set adam.adsl;",
    "  aval_double = aval * 2;",
    "run;"
  ), p1_file)

  p2_file <- file.path(tmp, "02_report_table.sas")
  writeLines(c(
    "data work.plot_ds;",
    "  set adam.adsl_out;",
    "run;",
    "ods pdf file='outputs/vs_summary_table.pdf';",
    "proc print data=work.plot_ds; run;",
    "ods pdf close;"
  ), p2_file)

  cfg_file <- file.path(tmp, "_sas2r.yml")
  writeLines(c(
    "libraries:",
    paste0("  adam: ", normalizePath(data_adam, winslash = "/", mustWork = FALSE)),
    "outputs:",
    "  datasets:",
    "    - adam.adsl_out",
    "  tlfs:",
    "    - outputs/vs_summary_table.pdf",
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    paste0("      adam: ", normalizePath(ref_adam, winslash = "/", mustWork = FALSE))
  ), cfg_file)

  # Initial input hash
  adsl_hash_before <- compute_file_sha256(file.path(data_adam, "adsl.rds"))

  # Mock translation for deterministic execution
  p1_r_code <- paste(
    "adsl <- lib_read('adam', 'adsl')",
    "adsl_out <- transform(adsl, AVAL_DOUBLE = AVAL * 2)",
    "lib_write(adsl_out, 'adam', 'adsl_out')",
    sep = "\n"
  )
  p2_r_code <- paste(
    "adsl_out <- lib_read('adam', 'adsl_out')",
    "plot_ds <- adsl_out",
    "dir.create('outputs', showWarnings = FALSE, recursive = TRUE)",
    "pdf('outputs/vs_summary_table.pdf')",
    "plot.new()",
    "text(0, 1, paste(capture.output(print(plot_ds)), collapse = '\\n'),",
    "     adj = c(0, 1), family = 'mono', cex = 0.7)",
    "dev.off()",
    sep = "\n"
  )

  # Schema-routed mock, not a sequential list: the deterministic transpiler
  # decides which components need the agent at all, so a fixed response order
  # breaks whenever the pipeline's call order changes -- exactly what happened
  # when T1 components stopped consuming translator calls. Routing by the
  # requested schema (and, for translations, by which unit's SAS is in the
  # prompt) keeps the fixture deterministic under any call order.
  route_fixture_response <- function(request) {
    schema <- request$schema_name %||% ""
    if (identical(schema, "program_translation_v1")) {
      prompt_text <- paste(
        vapply(request$messages, function(m) m$content %||% "", character(1)),
        collapse = "\n"
      )
      if (grepl("plot_ds", prompt_text, fixed = TRUE)) {
        good_translation(p2_r_code)
      } else {
        good_translation(p1_r_code)
      }
    } else if (identical(schema, "program_fix_v1")) {
      valid_program_fix_response(
        code = p2_r_code, diagnosis = "fixture no-op",
        summary = "fixture no-op", evidence_ids = "fixture"
      )
    } else {
      good_review()
    }
  }
  mock <- sas2r:::new_llm(function(request) {
    sas2r:::normalize_provider_response(
      route_fixture_response(request), request = request, provider = "mock"
    )
  }, provider = "mock", capabilities = sas2r:::llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))

  out_dir <- file.path(tmp, "trans_out")
  res <- sas_translate(tmp, config = cfg_file, out_dir = out_dir, llm = mock, execute = TRUE)

  # Check gates:
  # Gate 1: Graph schedule order (01_adsl_prep scheduled before 02_report_table)
  sched <- sas2r:::stable_dependency_schedule(res$project$graph %||% sas2r:::build_dependency_graph(res$project))
  g1_passed <- FALSE
  if (nrow(sched) >= 2L) {
    i1 <- which(sched$component_id == "01_adsl_prep")
    i2 <- which(sched$component_id == "02_report_table")
    g1_passed <- length(i1) == 1L && length(i2) == 1L && i1 < i2
  }

  # Gate 2: Target output contracts inventory
  target_keys <- names(res$output_assessments %||% list())
  has_adsl_target <- "adam.adsl_out" %in% target_keys && isTRUE(res$output_assessments[["adam.adsl_out"]]$passed)
  has_tlf_target <- "outputs/vs_summary_table.pdf" %in% target_keys && isTRUE(res$output_assessments[["outputs/vs_summary_table.pdf"]]$passed)
  g2_passed <- has_adsl_target && has_tlf_target

  # Gate 3: Library / input immutability
  adsl_hash_after <- compute_file_sha256(file.path(data_adam, "adsl.rds"))
  g3_passed <- identical(adsl_hash_before, adsl_hash_after)

  # Gate 4: Bundle status
  g4_passed <- res$status %in% c("migration_ready", "validated")

  # Gate 5: Reproducibility across fresh roots
  fresh1 <- tempfile("fresh1_")
  fresh2 <- tempfile("fresh2_")
  dir.create(fresh1, recursive = TRUE)
  dir.create(fresh2, recursive = TRUE)
  on.exit({ unlink(fresh1, recursive = TRUE); unlink(fresh2, recursive = TRUE) }, add = TRUE)

  sas_write(res, fresh1)
  sas_write(res, fresh2)
  # Remove copied outputs so a stale file cannot make the execution gate pass.
  for (fdir in c(fresh1, fresh2)) {
    unlink(file.path(fdir, "output"), recursive = TRUE)
  }
  run_export <- function(root) {
    tryCatch(callr::r(function(bdir) {
      entry <- jsonlite::read_json(file.path(bdir, "run-order.json"))$entrypoint
      sys.source(file.path(bdir, entry), envir = globalenv(), chdir = TRUE)
      TRUE
    }, args = list(bdir = root), wd = root), error = function(e) {
      message("Export execution failed: ", conditionMessage(e))
      FALSE
    })
  }
  res1 <- run_export(fresh1)
  res2 <- run_export(fresh2)

  ds1_path <- file.path(fresh1, "output", "datasets", "adam", "adsl_out.rds")
  ds2_path <- file.path(fresh2, "output", "datasets", "adam", "adsl_out.rds")
  tlf1_path <- file.path(fresh1, "output", "tlf", "outputs", "vs_summary_table.pdf")
  tlf2_path <- file.path(fresh2, "output", "tlf", "outputs", "vs_summary_table.pdf")

  ds_match <- file.exists(ds1_path) && file.exists(ds2_path) &&
    identical(readRDS(ds1_path), readRDS(ds2_path))
  tlf_match <- file.exists(tlf1_path) && file.exists(tlf2_path) &&
    (file.info(tlf1_path)$size > 0L) && (file.info(tlf2_path)$size > 0L)

  g5_passed <- isTRUE(res1) && isTRUE(res2) && ds_match && tlf_match

  # Gate 6: Seeded material defect rejection
  # Verify broken R code stops and gets blocked
  broken_mock <- sas2r:::mock_llm(list(
    good_translation("stop('seeded failure')"),
    good_review()
  ))
  res_broken <- sas_translate(file.path(tmp, "01_adsl_prep.sas"), config = cfg_file,
                              out_dir = file.path(tmp, "broken_out"), llm = broken_mock, execute = TRUE)
  g6_passed <- !identical(res_broken$status, "migration_ready") && !identical(res_broken$status, "validated")

  # Gate 7: Usage records
  reported_usage <- jsonlite::read_json(res$report_json_path)$usage
  g7_passed <- !is.null(res$usage) && res$usage$request_count > 0L &&
    identical(as.numeric(reported_usage$calls), as.numeric(res$usage$request_count))

  all_passed <- g1_passed && g2_passed && g3_passed && g4_passed && g5_passed && g6_passed && g7_passed

  gates_summary <- tibble::tibble(
    Gate = c(
      "1. Graph Schedule Order (Provider -> Consumer)",
      "2. Target Output Inventory (Datasets + TLFs)",
      "3. Library & Input Immutability (Unchanged Hashes)",
      "4. Deterministic Bundle Status (Migration Ready / Validated)",
      "5. Reproducibility Across Fresh Roots",
      "6. Seeded Material Defect Rejection",
      "7. Usage Accounting & Ledger Integrity"
    ),
    Status = c(
      if (g1_passed) "PASS" else "FAIL",
      if (g2_passed) "PASS" else "FAIL",
      if (g3_passed) "PASS" else "FAIL",
      if (g4_passed) "PASS" else "FAIL",
      if (g5_passed) "PASS" else "FAIL",
      if (g6_passed) "PASS" else "FAIL",
      if (g7_passed) "PASS" else "FAIL"
    )
  )

  # Output Reports
  summary_obj <- list(
    mode = "fixture",
    timestamp = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    passed = all_passed,
    gates = gates_summary,
    bundle_status = res$status,
    adsl_hash_before = adsl_hash_before,
    adsl_hash_after = adsl_hash_after,
    artifacts_dir = artifacts_dir
  )

  writeLines(jsonlite::toJSON(summary_obj, pretty = TRUE, auto_unbox = TRUE), file.path(artifacts_dir, "summary.json"))
  utils::write.csv(gates_summary, file.path(artifacts_dir, "summary.csv"), row.names = FALSE)

  md_report <- c(
    "# sas2r Fixture Acceptance Gate Report",
    "",
    sprintf("- **Date/Time (UTC)**: %s", summary_obj$timestamp),
    sprintf("- **Overall Result**: %s", if (all_passed) "PASSED" else "FAILED"),
    sprintf("- **Bundle Status**: `%s`", res$status),
    "",
    "Synthetic workflow checks with mocked translation and review; not independent SAS equivalence evidence.",
    "",
    "## Gate Assessment",
    "",
    knitr::kable(gates_summary, format = "markdown"),
    "",
    "## Reproducibility & Integrity Evidence",
    "",
    sprintf("- **Input SHA-256 (Before)**: `%s`", adsl_hash_before),
    sprintf("- **Input SHA-256 (After)**: `%s`", adsl_hash_after),
    sprintf("- **Immutability Preserved**: %s", if (g3_passed) "YES" else "NO"),
    sprintf("- **Dual Fresh Root Execution**: %s", if (g5_passed) "REPRODUCIBLE" else "FAILED")
  )
  writeLines(md_report, file.path(artifacts_dir, "summary.md"))

  print(gates_summary)
  cat("\n========================================================================\n")
  cat(sprintf("Fixture Acceptance Gate: %s\n", if (all_passed) "PASSED" else "FAILED"))
  cat(sprintf("Artifacts written to: %s\n", artifacts_dir))
  cat("========================================================================\n\n")

  if (!all_passed) {
    quit(status = 1L)
  }
}

# --- MAIN ---
run_fixture_acceptance(artifacts_dir = artifacts_dir)
