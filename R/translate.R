#' Translate SAS programs or project to R with dependency-aware orchestration
#'
#' Exposes the single authoritative SAS-to-R migration pipeline. SAS source is
#' the only required input. Operates on a single SAS file (wrapped as a one-module
#' project) or a directory of SAS files with identical internal sequencing:
#' initializes/reconciles run -> discovers dependency graph and output contracts ->
#' baseline generation and immediate program repair -> full bundle attempt execution
#' and output-driven repair -> deterministic gate selection and reporting.
#'
#' @param path Path to a SAS file or directory containing SAS files, or a `sas2r_project`.
#' @param out_dir Output directory path for generated R bundle, attempts, and reports. Defaults to a temporary directory.
#' @param config Optional project configuration list or `sas2r_config` object.
#' @param execute Logical; whether to run meaningful program smoke and full bundle execution. Defaults to TRUE.
#' @param max_program_repair_rounds Maximum repair rounds per component in immediate loop. Defaults to 1L.
#' @param max_bundle_repair_rounds Maximum repair rounds for full bundle repair loop. Defaults to 2L.
#' @param outputs Optional character vector or list specifying output contract overrides.
#' @param agent_evidence Agent evidence policy ("code_only" or "bounded"). Defaults to "code_only".
#' @param llm Optional `sas2r_llm` instance for agent-assisted translation, review, and repair.
#' @param budget_usd Dollar threshold for LLM spend. Defaults to Inf (unlimited).
#' @param budget_mode Dollar enforcement mode: "stop", "observe", "soft", or "strict". Defaults to "stop".
#' @param pricing_source Cost provenance source ("catalog", "adapter", "organization", "external"). Defaults to "catalog".
#' @param pricing_rates Optional organization pricing table.
#' @param usage_limits Optional named list of non-dollar request limits:
#'   `max_calls`, `max_retries`, `max_tool_calls`, `max_wall_time` (seconds),
#'   `max_request_bytes`, `max_request_chars`, `max_input_tokens`, and
#'   `max_output_tokens`. Each defaults to Inf. Zero prevents the corresponding
#'   request activity; unknown names and invalid values raise a configuration error.
#' @param recursive Logical; whether to scan subdirectories recursively. Defaults to FALSE.
#' @param resume Logical; reuse saved translation revisions and completed reviews
#'   when source, input data, configuration, helpers, and worker prompts still
#'   match. Execution and output checks always run again in a fresh attempt.
#'   Missing or edited revision files trigger regeneration. Defaults to FALSE.
#' @param keep_raw_attempts Logical; whether to retain raw outputs from unselected attempts. Defaults to FALSE.
#' @return An object of S3 class `"sas2r_translation"` containing `$run_id`, `$out_dir`,
#'   `$bundle_dir`, `$outputs_dir`, `$status`, `$status_reason`, `$graph_path`,
#'   `$output_contracts_path`, `$report_path`, `$report_json_path`, `$component_evidence`,
#'   `$output_assessments`, `$diagnostics`, `$repair_history`, `$usage`, and `$project`.
#'   `$outputs_dir` contains all selected generated files with their library and
#'   relative-path layout (for example `work/out.rds`, `adam/adsl.rds`,
#'   `outputs/table.html`); it is NULL when execution is disabled. The JSON
#'   report includes separate target/reference/review coverage and effective limits.
#' @examples
#' # Translate a small SAS program with the deterministic rule-based engine.
#' # Neither an LLM nor a SAS installation is required.
#' sas_dir <- file.path(tempdir(), "sas2r-translate-example")
#' dir.create(sas_dir, showWarnings = FALSE)
#' writeLines(c(
#'   "data work.adsl;",
#'   "  set adam.dm;",
#'   "  where age >= 18;",
#'   "  bmi = weight / (height * height);",
#'   "run;",
#'   "",
#'   "proc sort data=work.adsl out=work.adsl_sorted;",
#'   "  by usubjid;",
#'   "run;"
#' ), file.path(sas_dir, "adsl.sas"))
#'
#' # execute = FALSE translates without running the generated bundle.
#' res <- sas_translate(sas_dir, out_dir = tempfile("sas2r-out"), execute = FALSE)
#' res$status
#' cat(sas_code(res))
#'
#' \dontrun{
#' # Run the generated bundle and let the agents translate, review, and repair
#' # the patterns the deterministic rules cannot prove.
#' llm <- sas_llm(list(provider = "anthropic", model = "claude-sonnet-4-6"))
#' res <- sas_translate("path/to/sas/project", llm = llm, recursive = TRUE)
#' sas_write(res, "path/to/r/bundle")
#' }
#' @export
sas_translate <- function(
  path,
  out_dir = NULL,
  config = NULL,
  execute = TRUE,
  max_program_repair_rounds = 1L,
  max_bundle_repair_rounds = 2L,
  outputs = NULL,
  agent_evidence = c("code_only", "bounded"),
  llm = NULL,
  budget_usd = Inf,
  budget_mode = "stop",
  pricing_source = "catalog",
  pricing_rates = NULL,
  usage_limits = NULL,
  recursive = FALSE,
  resume = FALSE,
  keep_raw_attempts = FALSE
) {
  agent_evidence <- if (is.character(agent_evidence)) match.arg(agent_evidence, c("code_only", "bounded")) else "code_only"

  # A mistyped mode must refuse, not fall through to a weaker default: budget
  # enforcement and cost provenance are safety knobs on a paid path.
  if (!is.character(budget_mode) || length(budget_mode) != 1L ||
      !budget_mode %in% c("stop", "strict", "soft", "observe")) {
    cli::cli_abort(
      "budget_mode must be one of \"stop\", \"strict\", \"soft\", or \"observe\", not {.val {budget_mode}}",
      class = "sas2r_invalid_argument"
    )
  }
  if (!is.character(pricing_source) || length(pricing_source) != 1L ||
      !pricing_source %in% c("catalog", "adapter", "organization", "external")) {
    cli::cli_abort(
      "pricing_source must be one of \"catalog\", \"adapter\", \"organization\", or \"external\", not {.val {pricing_source}}",
      class = "sas2r_invalid_argument"
    )
  }

  # 1. Output directory setup
  if (is.null(out_dir)) {
    out_dir <- tempfile(pattern = "sas2r_out_")
  }
  paths <- migration_paths(out_dir)
  # Only the root and state directories are needed before the run identifier
  # exists; the run-scoped directories (runs/<run_id>/...) are created by
  # new_migration_state() once the budget has minted the run id.
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$state, recursive = TRUE, showWarnings = FALSE)

  # 2. Configuration resolution
  cfg <- if (inherits(config, "sas2r_config")) {
    config
  } else if (is.character(config) && length(config) == 1L && file.exists(config)) {
    sas_config(path = config)
  } else if (is.list(config)) {
    structure(config, class = "sas2r_config")
  } else {
    start_dir <- if (inherits(path, "sas2r_project")) {
      path$project_dir
    } else if (is.character(path) && length(path) == 1L) {
      if (dir.exists(path)) path else dirname(path)
    } else {
      "."
    }
    sas_config(start = start_dir)
  }

  # 3. Project normalization (single file wrapped into 1-module project or multi-module directory)
  project <- if (inherits(path, "sas2r_project")) {
    path
  } else {
    sas_project(path, config = cfg, cache = TRUE, recursive = recursive)
  }

  # 4. Output contracts discovery and override merge
  output_overrides <- outputs %||% cfg$outputs
  output_contracts <- infer_output_contracts(project, overrides = output_overrides)
  output_contracts_path <- file.path(paths$state, "output-contracts.json")
  write_output_contracts(output_contracts, output_contracts_path)

  # 5. Dependency graph and stable schedule
  graph <- build_dependency_graph(project, output_contracts = output_contracts)
  atomic_write_json(graph, paths$graph)
  schedule <- stable_dependency_schedule(graph)

  # 6. Usage budget and accounting
  budget_mode_norm <- if (!is.finite(budget_usd)) {
    if (identical(budget_mode, "soft")) "soft" else "observe"
  } else if (identical(budget_mode, "stop") || identical(budget_mode, "strict")) {
    "strict"
  } else if (identical(budget_mode, "soft")) {
    "soft"
  } else {
    "observe"
  }

  pricing_source_norm <- if (identical(pricing_source, "catalog")) "adapter" else pricing_source

  usage_limits_map <- usage_limits %||% list()
  limit_names <- setdiff(names(formals(new_usage_budget)),
                         c("mode", "max_usd", "rates", "pricing_source", "ledger_path", "run_id", "resume"))
  if (!is.list(usage_limits_map) ||
      (length(usage_limits_map) && (is.null(names(usage_limits_map)) ||
       any(!names(usage_limits_map) %in% limit_names) || anyDuplicated(names(usage_limits_map))))) {
    cli::cli_abort("usage_limits must be a named list of request limits: {.val {limit_names}}",
                   class = "sas2r_budget_config_error")
  }
  budget <- do.call(new_usage_budget, c(list(
    mode = budget_mode_norm,
    max_usd = budget_usd,
    pricing_source = pricing_source_norm,
    rates = pricing_rates,
    ledger_path = file.path(paths$state, "usage.jsonl"),
    resume = isTRUE(resume)
  ), usage_limits_map))

  # 7. Resolve LLM adapter from argument or config
  resolved_llm <- if (!is.null(llm)) {
    llm
  } else if (!is.null(cfg$llm)) {
    sas_llm(cfg$llm)
  } else {
    NULL
  }

  # 8. Initialize migration state
  state <- new_migration_state(
    project = project,
    out_dir = out_dir,
    llm = resolved_llm,
    config = cfg,
    execute = isTRUE(execute),
    max_program_repair_rounds = as.integer(max_program_repair_rounds),
    max_bundle_repair_rounds = as.integer(max_bundle_repair_rounds),
    usage_budget = budget
  )
  # Adopt the state's run-scoped paths: attempts (and thus pruning and the
  # selected-bundle fallbacks below) live under attempts/<run_id>/.
  paths <- state$paths
  state$graph <- graph
  state$schedule <- schedule
  state$output_contracts <- output_contracts
  state$agent_evidence <- agent_evidence
  state$keep_raw_attempts <- isTRUE(keep_raw_attempts)

  # Resume exact revision records only when all relevant inputs still match.
  resume_fingerprint <- migration_resume_fingerprint(state)
  if (isTRUE(resume)) state <- restore_migration_checkpoint(state, resume_fingerprint)

  # 9./10. Program pipeline (baseline translation, mechanical checks, review,
  # immediate repair) then bundle pipeline (full execution attempt, output
  # assessment, bundle repair, selection). Both signal sas2r_progress
  # conditions, and this is the one place the console renderer is installed --
  # without it a long metered run prints nothing.
  state <- with_sas2r_progress({
    prog_state <- run_program_pipeline(
      state = state,
      max_program_repair_rounds = as.integer(max_program_repair_rounds),
      execute = isTRUE(execute)
    )
    run_bundle_pipeline(
      state = prog_state,
      max_bundle_repair_rounds = as.integer(max_bundle_repair_rounds),
      execute = isTRUE(execute)
    )
  })

  # 11. Determine selected bundle and outputs directories
  selected_att <- state$selected_attempt
  bundle_dir <- if (!is.null(selected_att) && !is.null(selected_att$attempt_dir)) {
    file.path(selected_att$attempt_dir, "bundle")
  } else if (!is.null(state$attempt) && !is.null(state$attempt$attempt_dir)) {
    file.path(state$attempt$attempt_dir, "bundle")
  } else {
    file.path(paths$state, "selected", "bundle")
  }

  # If bundle_dir doesn't exist, create snapshot
  if (!dir.exists(bundle_dir)) {
    dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
    snapshot_selected_bundle(state, dirname(bundle_dir))
  }

  outputs_dir <- if (isTRUE(execute) && !is.null(selected_att) && !is.null(selected_att$attempt_dir)) {
    destination <- file.path(paths$attempts, "generated-outputs")
    copy_output_inventory(selected_att$attempt_dir, destination, selected_att$output_hashes)
    destination
  } else {
    NULL
  }

  state$bundle_dir <- bundle_dir
  state$outputs_dir <- outputs_dir

  # Status adjustments for execute = FALSE
  if (isFALSE(execute)) {
    state$outputs_dir <- NULL
    outputs_dir <- NULL
    # If there are check failures, status is blocked; otherwise needs_review
    has_check_failure <- any(vapply(state$selected_revisions, function(r) {
      identical(r$status, "check_failed")
    }, logical(1)))
    if (has_check_failure) {
      state$status <- "blocked"
      state$status_reason <- "Mechanical check failed for one or more components"
    } else {
      state$status <- "needs_review"
      state$status_reason <- "Execution disabled (execute = FALSE); outputs unverified"
    }
  }

  # A run whose LLM calls failed must not read like a successful deterministic
  # run: name the components that kept the baseline because the agent was
  # unreachable.
  degraded <- state$diagnostics$agent_degraded %||% list()
  if (length(degraded)) {
    note <- sprintf(
      "LLM agent unavailable for %d component(s) (%s); deterministic baseline kept",
      length(degraded),
      paste(unique(unlist(degraded)), collapse = ", ")
    )
    state$status_reason <- if (is.null(state$status_reason) ||
                               !nzchar(state$status_reason %||% "")) {
      note
    } else {
      paste0(state$status_reason, "; ", note)
    }
  }

  # 12. Prune unselected raw attempts if requested
  if (!isTRUE(keep_raw_attempts)) {
    prune_rejected_attempt_outputs(paths, keep_raw = FALSE)
  }

  # 12b. Surface the selected translation at the top of the run folder, next
  # to its report; the attempt bundle underneath stays canonical.
  if (identical(basename(paths$attempts), budget$run_id)) {
    materialize_run_translation(bundle_dir, paths$attempts, project = state$project)
  }

  # 13. Write authoritative machine and markdown reports
  budget$end_time <- Sys.time()
  write_migration_report(state)
  write_migration_checkpoint(state, resume_fingerprint)
  with_sas2r_progress(signal_bundle_event(
    "migration_summary", summary = migration_usage_lines(migration_usage_summary(budget))
  ))

  # 14. Return canonical sas2r_translation object
  structure(
    list(
      run_id = budget$run_id %||% new_usage_run_id(),
      out_dir = paths$root,
      bundle_dir = bundle_dir,
      outputs_dir = outputs_dir,
      status = state$status,
      status_reason = state$status_reason,
      graph_path = paths$graph,
      output_contracts_path = output_contracts_path,
      report_path = paths$report_md,
      report_json_path = paths$report_json,
      component_evidence = state$histories,
      output_assessments = state$assessment$targets %||% state$assessment,
      diagnostics = state$diagnostics %||% list(),
      repair_history = state$repairs %||% list(),
      usage = state$usage_budget,
      project = state$project
    ),
    class = c("sas2r_translation", "list")
  )
}

#' Print a sas2r translation summary
#'
#' @param x A `sas2r_translation` object.
#' @param ... Additional arguments (ignored).
#' @return The input `x`, invisibly.
#' @export
print.sas2r_translation <- function(x, ...) {
  cli::cat_line(cli::rule(left = "sas2r migration"))
  cli::cat_line("status: ", x$status)
  if (!is.null(x$status_reason) && nzchar(x$status_reason)) {
    cli::cat_line("reason: ", x$status_reason)
  }
  if (!is.null(x$bundle_dir)) {
    cli::cat_line("bundle: ", x$bundle_dir)
  }
  if (!is.null(x$outputs_dir)) {
    cli::cat_line("outputs: ", x$outputs_dir)
  }
  if (!is.null(x$report_path)) {
    cli::cat_line("report: ", x$report_path)
  }

  if (length(x$component_evidence) > 0L) {
    levels <- vapply(x$component_evidence, function(h) {
      ev <- tryCatch(current_component_evidence(h), error = function(e) NULL)
      ev$level %||% ev$evidence_level %||% "pending"
    }, character(1))
    tab <- table(levels)
    for (lvl in names(tab)) {
      cli::cat_line(sprintf("  %s: %d", lvl, tab[[lvl]]))
    }
  }

  cli::cat_line(migration_coverage_lines(migration_coverage(x$output_assessments, x$component_evidence)))
  cli::cat_line(migration_usage_lines(migration_usage_summary(x$usage)))
  invisible(x)
}

#' Write translated code artifacts and outputs to a destination directory
#'
#' Copies the selected programs, runtime, all generated files, and migration
#' reports. Rebuilds `autoexec.R` with destination-relative output paths and
#' includes a dependency-ordered `run.R`, an output manifest, and a README.
#' Input libraries remain external dependencies documented in the README.
#' Run `Rscript run.R` from the exported folder, or `source("run.R", chdir = TRUE)`.
#' Warns if status is `blocked` or `needs_review`.
#'
#' @param x A `sas2r_translation` object.
#' @param dir Target directory path to write translated files.
#' @return The target directory path, invisibly.
#' @examples
#' sas_dir <- file.path(tempdir(), "sas2r-write-example")
#' dir.create(sas_dir, showWarnings = FALSE)
#' writeLines(c("proc sort data=work.dm out=work.dm_sorted;",
#'              "  by usubjid;",
#'              "run;"), file.path(sas_dir, "sort.sas"))
#' res <- sas_translate(sas_dir, out_dir = tempfile("sas2r-out"), execute = FALSE)
#'
#' # Writing an unverified translation warns; the bundle is still written.
#' dest <- tempfile("sas2r-bundle")
#' suppressWarnings(sas_write(res, dest))
#' list.files(dest, recursive = TRUE)
#' @export
sas_write <- function(x, dir) {
  stopifnot(inherits(x, "sas2r_translation"))
  if (x$status %in% c("blocked", "needs_review")) {
    cli::cli_warn(
      sprintf("writing code with status '%s' (%s)", x$status, x$status_reason %||% "review before use"),
      class = "sas2r_unverified_write"
    )
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  materialize_run_translation(x$bundle_dir, dir, project = x$project)
  inventory <- if (!is.null(x$outputs_dir)) attempt_output_hashes(x$outputs_dir) else list()
  if (length(inventory)) copy_output_inventory(x$outputs_dir, dir, inventory)
  write_bundle_guide(x$project, dir, inventory)

  if (!is.null(x$report_path) && file.exists(x$report_path)) {
    file.copy(x$report_path, file.path(dir, basename(x$report_path)), overwrite = TRUE)
  }

  if (!is.null(x$report_json_path) && file.exists(x$report_json_path)) {
    state_dir <- file.path(dir, ".sas2r")
    dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(x$report_json_path, file.path(state_dir, "report.json"), overwrite = TRUE)
  }

  invisible(dir)
}

#' Retrieve staged code for a translated program from the selected bundle
#'
#' Recursively inspects the selected bundle directory and resolves by component ID,
#' relative file path, basename, or integer index.
#'
#' @param x A `sas2r_translation` object.
#' @param file Staged file index or name/component ID.
#' @return Character string containing the program code.
#' @examples
#' sas_dir <- file.path(tempdir(), "sas2r-code-example")
#' dir.create(sas_dir, showWarnings = FALSE)
#' writeLines(c("proc sort data=work.dm out=work.dm_sorted;",
#'              "  by usubjid;",
#'              "run;"), file.path(sas_dir, "sort.sas"))
#' res <- sas_translate(sas_dir, out_dir = tempfile("sas2r-out"), execute = FALSE)
#'
#' # Resolve by index, or by file name / component id.
#' cat(sas_code(res, 1L))
#' @export
sas_code <- function(x, file = 1L) {
  stopifnot(inherits(x, "sas2r_translation"))
  b_dir <- x$bundle_dir
  if (is.null(b_dir) || !dir.exists(b_dir)) {
    cli::cli_abort("Bundle directory not found: {.file {b_dir}}", class = "sas2r_file_not_found")
  }

  all_files <- list.files(b_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  run_order_path <- file.path(b_dir, "run-order.json")
  entrypoint <- if (file.exists(run_order_path)) read_json_record(run_order_path)$entrypoint else NULL
  prog_files <- all_files[!basename(all_files) %in% SAS2R_BUNDLE_FILES &
                           !all_files %in% file.path(b_dir, entrypoint)]
  if (length(prog_files) == 0L) {
    prog_files <- all_files
  }
  if (length(all_files) == 0L) {
    cli::cli_abort("No R files found in bundle directory {.file {b_dir}}", class = "sas2r_file_not_found")
  }

  target <- NULL
  if (is.numeric(file)) {
    idx <- as.integer(file)
    if (idx < 1L || idx > length(prog_files)) {
      cli::cli_abort("File index {file} out of range (1..{length(prog_files)})", class = "sas2r_file_not_found")
    }
    target <- prog_files[[idx]]
  } else if (is.character(file)) {
    rel_paths <- substring(all_files, nchar(b_dir) + 2L)
    base_names <- basename(all_files)
    cids <- tools::file_path_sans_ext(base_names)

    matched <- if (file %in% all_files) {
      file
    } else if (file %in% rel_paths) {
      all_files[rel_paths == file]
    } else if (file %in% base_names) {
      all_files[base_names == file]
    } else if (file %in% cids) {
      all_files[cids == file]
    } else if (paste0(file, ".R") %in% base_names) {
      all_files[base_names == paste0(file, ".R")]
    } else {
      character(0)
    }

    if (length(matched) == 0L) {
      cli::cli_abort("File {.val {file}} not found in bundle directory {.file {b_dir}}", class = "sas2r_file_not_found")
    }
    target <- matched[[1L]]
  } else {
    cli::cli_abort("file must be an integer index or character file/component name", class = "sas2r_invalid_argument")
  }

  paste(readLines(target, warn = FALSE), collapse = "\n")
}
