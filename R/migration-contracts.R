#' Migration schema and enum constants
#' @noRd
MIGRATION_SCHEMA_VERSION <- "1"

#' Allowed component evidence levels
#' @noRd
COMPONENT_EVIDENCE_LEVELS <- c(
  "reviewed_only",
  "runtime_verified",
  "output_verified",
  "reference_validated"
)

#' Allowed bundle statuses
#' @noRd
BUNDLE_STATUSES <- c(
  "blocked",
  "needs_review",
  "migration_ready",
  "validated"
)

#' Allowed agent evidence policies
#' @noRd
AGENT_EVIDENCE_POLICIES <- c(
  "code_only",
  "bounded"
)

#' Recursively canonicalize values for deterministic migration hashing
#'
#' Sorts named-list keys alphabetically while preserving vector element order.
#'
#' @param x Value or structure to canonicalize.
#' @return Canonicalized structure.
#' @noRd
canonicalize_migration_value <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    col_names <- names(x)
    if (!is.null(col_names) && length(col_names) > 0) {
      x <- x[, order(col_names), drop = FALSE]
    }
    for (nm in names(x)) {
      if (is.list(x[[nm]])) {
        x[[nm]] <- canonicalize_migration_value(x[[nm]])
      }
    }
    return(x)
  }
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      ord <- order(nm)
      x <- x[ord]
    }
    return(lapply(x, canonicalize_migration_value))
  }
  x
}

#' Generate a canonical SHA-256 hash for any migration data structure
#'
#' @param x Object to hash.
#' @return Character string containing the hex SHA-256 digest.
#' @noRd
migration_hash <- function(x) {
  normalized <- canonicalize_migration_value(x)
  payload <- jsonlite::toJSON(
    normalized,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    dataframe = "rows"
  )
  unname(cli::hash_sha256(as.character(payload)))
}

#' Get canonical on-disk paths for a migration output directory
#'
#' @param out_dir Output root directory.
#' @param run_id Optional run identifier. Runs are scoped under this name with
#'   separate bundle, outputs, report, and diagnostics folders. Direct internal
#'   tools without a run ID use `runs/`.
#' @return Named list of canonical paths.
#' @noRd
migration_paths <- function(out_dir, run_id = NULL) {
  if (!is_scalar_character(out_dir)) {
    cli::cli_abort("out_dir must be one nonempty path", class = "sas2r_invalid_argument")
  }
  root <- sub("(?<!^)/+$", "", out_dir, perl = TRUE)
  state <- file.path(root, ".sas2r")
  run_root <- file.path(root, run_id %||% "runs")
  diagnostics <- file.path(run_root, "diagnostics")
  report <- file.path(run_root, "report")
  bundle <- file.path(run_root, "bundle")
  list(
    root = root,
    run_id = run_id,
    state = state,
    run_root = run_root,
    graph = file.path(state, "graph.json"),
    component_revisions = file.path(diagnostics, "component_revisions"),
    # The staged working bundle lives under .sas2r/, not at the out_dir root:
    # staging is an intermediate the pipeline patches in place, and surfacing
    # it at the top level made users mistake it for the final translation.
    staging = file.path(state, "staging"),
    bundle = bundle,
    bundle_programs = file.path(bundle, "programs"),
    bundle_macros = file.path(bundle, "macros"),
    bundle_runtime = file.path(bundle, "runtime"),
    bundle_output = file.path(bundle, "output"),
    diagnostics = diagnostics,
    bundle_attempts = file.path(diagnostics, "bundle_attempts"),
    smoke_tests = file.path(diagnostics, "smoke_tests"),
    logs = file.path(diagnostics, "logs"),
    work = file.path(diagnostics, "work"),
    outputs = file.path(run_root, "outputs"),
    outputs_datasets = file.path(run_root, "outputs", "datasets"),
    outputs_tlf = file.path(run_root, "outputs", "tlf"),
    start_here = file.path(run_root, "START_HERE.html"),
    manifest = file.path(run_root, "manifest.json"),
    selected = file.path(state, "selected.json"),
    usage = file.path(state, "usage.json"),
    # Keep a latest-report cache for output-root readers; run reports are
    # independent and remain authoritative for their own invocation.
    latest_report_json = file.path(state, "report.json"),
    report_dir = report,
    report_json = file.path(report, "report.json"),
    report_md = file.path(report, "translation.md"),
    comparisons = file.path(report, "comparison-details")
  )
}

#' Initialize on-disk migration directories
#'
#' Creates shared `.sas2r/` staging/cache and run-scoped diagnostics and reports.
#'
#' @param out_dir Output root directory.
#' @param run_id Optional run identifier forwarded to [migration_paths()].
#' @return Named list of migration paths.
#' @noRd
init_migration_paths <- function(out_dir, run_id = NULL) {
  paths <- migration_paths(out_dir, run_id = run_id)
  # Workers use the source directory as their working directory. Resolve the
  # output root in the caller before deriving any paths shared across processes.
  paths <- migration_paths(config_anchor_paths(paths$root, getwd()), run_id = run_id)
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$state, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$staging, recursive = TRUE, showWarnings = FALSE)
  for (name in c("run_root", "bundle_attempts", "smoke_tests", "component_revisions",
                 "logs", "report_dir")) {
    dir.create(paths[[name]], recursive = TRUE, showWarnings = FALSE)
  }
  paths
}

#' Create a new behavioral contract record for a component
#'
#' @param component_id Unique component identifier.
#' @param parameters List of parameter definition objects ({name, type, required, default}).
#' @param defaults Named list/object of default values.
#' @param reads Character vector of datasets/tables read.
#' @param writes Character vector of datasets/tables written.
#' @param side_effects Character vector of side effects.
#' @param helper_use Character vector of helper function names used.
#' @param known_call_sites List/data.frame of known call sites.
#' @param resolved_dependencies Character vector of upstream resolved component IDs.
#' @param suspected_dependencies Character vector of suspected component IDs.
#' @param affected_outputs Character vector of final output target keys.
#' @param uncertainty List of uncertainty objects ({severity, claim, evidence, affected_outputs}).
#' @param binding Named list of binding hashes (source_hash, r_hash, helper_hash, prompt_skill_hash, dependency_closure_hash).
#' @param schema_version Schema version string.
#' @param created_at ISO 8601 creation timestamp.
#' @param ... Additional fields.
#' @return A named list representing the behavioral contract.
#' @noRd
new_behavioral_contract <- function(
  component_id,
  parameters = list(),
  defaults = structure(list(), names = character(0)),
  reads = character(),
  writes = character(),
  side_effects = character(),
  helper_use = character(),
  known_call_sites = list(),
  resolved_dependencies = character(),
  suspected_dependencies = character(),
  affected_outputs = character(),
  uncertainty = list(),
  binding = list(),
  schema_version = MIGRATION_SCHEMA_VERSION,
  created_at = NULL,
  ...
) {
  if (!is.character(component_id) || length(component_id) != 1L || !nzchar(component_id)) {
    cli::cli_abort("component_id must be a non-empty string", class = "sas2r_invalid_argument")
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  if (length(defaults) == 0 && is.list(defaults) && is.null(names(defaults))) {
    defaults <- structure(list(), names = character(0))
  }

  contract <- list(
    schema_version = schema_version,
    component_id = component_id,
    parameters = as.list(parameters),
    defaults = defaults,
    reads = unique(as.character(reads)),
    writes = unique(as.character(writes)),
    side_effects = unique(as.character(side_effects)),
    helper_use = unique(as.character(helper_use)),
    known_call_sites = as.list(known_call_sites),
    resolved_dependencies = unique(as.character(resolved_dependencies)),
    suspected_dependencies = unique(as.character(suspected_dependencies)),
    affected_outputs = unique(as.character(affected_outputs)),
    uncertainty = as.list(uncertainty),
    binding = as.list(binding),
    created_at = created_at
  )
  extra <- list(...)
  if (length(extra) > 0) {
    contract <- utils::modifyList(contract, extra)
  }
  contract
}
