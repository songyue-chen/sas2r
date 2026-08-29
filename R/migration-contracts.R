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
#' @return Named list of canonical paths.
#' @noRd
migration_paths <- function(out_dir) {
  root <- out_dir
  state <- file.path(root, ".sas2r")
  list(
    root = root,
    state = state,
    graph = file.path(state, "graph.json"),
    outputs = file.path(root, "outputs"),
    programs = file.path(root, "programs"),
    attempts = file.path(root, "attempts"),
    selected = file.path(state, "selected.json"),
    usage = file.path(state, "usage.json"),
    report_json = file.path(state, "report.json"),
    report_md = file.path(root, "report.md")
  )
}

#' Initialize on-disk migration directories
#'
#' Creates `.sas2r/`, `programs/`, `attempts/`, and `outputs/` under `out_dir`.
#'
#' @param out_dir Output root directory.
#' @return Named list of migration paths.
#' @noRd
init_migration_paths <- function(out_dir) {
  paths <- migration_paths(out_dir)
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$state, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$programs, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$attempts, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$outputs, recursive = TRUE, showWarnings = FALSE)
  paths
}

#' Create a new migration run record
#'
#' @param run_id Unique run identifier.
#' @param path Input SAS file or directory path.
#' @param out_dir Output root directory path.
#' @param status Initial or final bundle status (must be in BUNDLE_STATUSES).
#' @param agent_evidence Agent evidence policy ("code_only" or "bounded").
#' @param execute Logical indicating if execution is enabled.
#' @param max_program_repair_rounds Integer repair budget for programs.
#' @param max_bundle_repair_rounds Integer repair budget for bundles.
#' @param schema_version Schema version string.
#' @param created_at ISO 8601 creation timestamp.
#' @param ... Additional metadata fields.
#' @return A named list representing the migration run record.
#' @noRd
new_migration_run_record <- function(
  run_id,
  path = character(),
  out_dir = character(),
  status = "blocked",
  agent_evidence = "code_only",
  execute = TRUE,
  max_program_repair_rounds = 1L,
  max_bundle_repair_rounds = 2L,
  schema_version = MIGRATION_SCHEMA_VERSION,
  created_at = NULL,
  ...
) {
  if (!is.character(run_id) || length(run_id) != 1L || !nzchar(run_id)) {
    cli::cli_abort("run_id must be a non-empty string", class = "sas2r_invalid_argument")
  }
  if (!is.null(status) && (!is.character(status) || length(status) != 1L || !status %in% BUNDLE_STATUSES)) {
    cli::cli_abort(
      "invalid bundle status: {.val {status}}; must be one of {.val {BUNDLE_STATUSES}}",
      class = "sas2r_invalid_status"
    )
  }
  if (!is.character(agent_evidence) || length(agent_evidence) != 1L || !agent_evidence %in% AGENT_EVIDENCE_POLICIES) {
    cli::cli_abort(
      "invalid agent evidence policy: {.val {agent_evidence}}; must be one of {.val {AGENT_EVIDENCE_POLICIES}}",
      class = "sas2r_invalid_evidence_policy"
    )
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  record <- list(
    schema_version = schema_version,
    run_id = run_id,
    path = path,
    out_dir = out_dir,
    status = status,
    agent_evidence = agent_evidence,
    execute = isTRUE(execute),
    max_program_repair_rounds = as.integer(max_program_repair_rounds),
    max_bundle_repair_rounds = as.integer(max_bundle_repair_rounds),
    created_at = created_at
  )
  extra <- list(...)
  if (length(extra) > 0) {
    record <- utils::modifyList(record, extra)
  }
  record
}

#' Create a new bundle status record
#'
#' @param status Bundle status (must be in BUNDLE_STATUSES).
#' @param reason Explanatory reason for status.
#' @param attempt_id Identifier of the attempt evaluated (if any).
#' @param assessed_targets Optional list or vector of assessed target summary records.
#' @param schema_version Schema version string.
#' @param created_at ISO 8601 creation timestamp.
#' @param ... Additional metadata fields.
#' @return A named list representing the bundle status record.
#' @noRd
new_bundle_status_record <- function(
  status,
  reason = character(),
  attempt_id = NULL,
  assessed_targets = list(),
  schema_version = MIGRATION_SCHEMA_VERSION,
  created_at = NULL,
  ...
) {
  if (!is.character(status) || length(status) != 1L || !status %in% BUNDLE_STATUSES) {
    cli::cli_abort(
      "invalid bundle status: {.val {status}}; must be one of {.val {BUNDLE_STATUSES}}",
      class = "sas2r_invalid_status"
    )
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  record <- list(
    schema_version = schema_version,
    status = status,
    reason = reason,
    attempt_id = attempt_id,
    assessed_targets = assessed_targets,
    created_at = created_at
  )
  extra <- list(...)
  if (length(extra) > 0) {
    record <- utils::modifyList(record, extra)
  }
  record
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

