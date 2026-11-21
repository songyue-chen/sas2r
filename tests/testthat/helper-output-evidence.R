# Deterministic, no-network test fixture constructors for output evidence

output_target_fixture <- function(target_id = "target_0001",
                                  logical_dataset = "adam.adsl",
                                  role = "final_dataset",
                                  program = "adsl.sas",
                                  consumer_proc = NA_character_,
                                  unit_ids = 1L) {
  tibble::tibble(
    target_id = target_id,
    logical_dataset = logical_dataset,
    role = role,
    program = program,
    consumer_proc = consumer_proc,
    contributing_unit_ids = list(as.integer(unit_ids)),
    reference_candidate_id = "candidate_000001",
    r_candidate_id = "candidate_000002",
    reference_options = list(character()),
    r_options = list(character()),
    pairing_method = "exact_logical_name",
    status = "paired",
    reason = NA_character_
  )
}

alignment_context <- function(by = character(),
                              sort = list(vars = by, descending = rep(FALSE, length(by))),
                              merge = list(),
                              known_identifiers = by,
                              order_contract = NULL) {
  list(
    by = as.character(by),
    sort = sort,
    merge = merge,
    known_identifiers = as.character(known_identifiers),
    order_contract = order_contract
  )
}

empty_alignment_context <- function() {
  list(
    by = character(),
    sort = list(vars = character(), descending = logical()),
    merge = list(),
    known_identifiers = character(),
    order_contract = NULL
  )
}

comparison_report_registry_fixture <- function() {
  env <- new.env(parent = emptyenv())
  rep <- list(
    schema_version = sas2r:::OUTPUT_REPORT_SCHEMA_VERSION,
    report_id = "report-1",
    target_id = "target-1",
    logical_dataset = "adam.adsl",
    role = "final_dataset",
    contributing_unit_ids = 1L,
    staged_code_hashes = "abc",
    structure = list(),
    alignment = list(),
    order = list(meaningful = FALSE),
    mismatches = list(),
    examples = tibble::tibble(
      reference_row = integer(),
      candidate_row = integer(),
      key_values = character(),
      variable = character(),
      reference_value = character(),
      candidate_value = character()
    ),
    resource_state = "complete",
    truncated = FALSE,
    truncated_fields = character(),
    stale = FALSE,
    stale_reason = NA_character_,
    reference_hash = "refhash",
    candidate_hash = "candhash",
    profile_hash = "profhash",
    schema_hash = "schhash"
  )
  class(rep) <- "sas2r_comparison_report"
  env[["report-1"]] <- rep
  env
}

# Every character leaf of an outbound request: prompt text, tool descriptions,
# tool argument schemas, model and schema names. Functions and environments
# are skipped because they never travel to the provider.
outbound_request_strings <- function(x) {
  if (is.function(x) || is.environment(x)) return(character())
  if (is.character(x)) return(as.character(x))
  if (is.list(x)) {
    parts <- unlist(lapply(x, outbound_request_strings), use.names = FALSE)
    return(if (is.null(parts)) character() else as.character(parts))
  }
  character()
}

test_usage_budget <- function(mode = "observe", max_calls = 100L) {
  new_usage_budget(mode = mode, max_calls = max_calls)
}

staged_file_hashes <- function(dir) {
  # Ask for the relative names directly rather than slicing the absolute ones.
  # Slicing mixed a normalized prefix length against unnormalized paths, and on
  # macOS -- where normalizePath() prepends "/private" -- that cut eight extra
  # characters off every name, yielding "" and "elpers.R" instead of the real
  # relative paths. Names identify which file moved, so wrong names make an
  # expect_identical() failure unreadable.
  rel <- list.files(dir, pattern = "\\.R$", recursive = TRUE)
  if (!length(rel)) return(character())
  hashes <- vapply(file.path(dir, rel),
                   function(f) as.character(cli::hash_file_sha256(f)),
                   character(1), USE.NAMES = FALSE)
  stats::setNames(hashes, rel)
}

output_contract_fixture_project <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "data/sdtm"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "reports"), recursive = TRUE, showWarnings = FALSE)

  code <- c(
    "data work.stage;",
    "  set sdtm.dm;",
    "run;",
    "",
    "data adam.adsl;",
    "  set work.stage;",
    "run;",
    "",
    "ods pdf file=\"reports/t14-1.pdf\";",
    "proc report data=adam.adsl;",
    "  columns usubjid trt;",
    "run;",
    "ods pdf close;",
    "",
    "ods rtf file=\"reports/l14-2.rtf\";",
    "proc print data=adam.adsl;",
    "run;",
    "ods rtf close;",
    "",
    "ods listing gpath=\"reports\";",
    "ods graphics on / reset imagename=\"f14-3\" imagefmt=png outdir=\"reports\";",
    "proc sgplot data=adam.adsl;",
    "  vbar trt;",
    "run;",
    "ods listing close;"
  )
  writeLines(code, file.path(dir, "report.sas"))

  writeLines(c(
    "libraries:",
    "  adam: data/adam",
    "  sdtm: data/sdtm"
  ), file.path(dir, "_sas2r.yml"))

  sas_project(dir)
}

dynamic_tlf_fixture_project <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "data/sdtm"), recursive = TRUE, showWarnings = FALSE)

  code <- c(
    "data adam.adsl;",
    "  set sdtm.dm;",
    "run;",
    "",
    "ods pdf file=\"&outdir/t14-1.pdf\";",
    "proc report data=adam.adsl;",
    "  columns usubjid;",
    "run;",
    "ods pdf close;"
  )
  writeLines(code, file.path(dir, "dynamic_report.sas"))

  writeLines(c(
    "libraries:",
    "  adam: data/adam",
    "  sdtm: data/sdtm"
  ), file.path(dir, "_sas2r.yml"))

  sas_project(dir)
}
