#' Translate SAS programs or project to R with dependency-aware orchestration
#'
#' Exposes the single authoritative SAS-to-R migration pipeline. SAS source is
#' the only required input. Operates on a single SAS file (wrapped as a one-module
#' project) or a directory of SAS files with identical internal sequencing:
#' initializes/reconciles run -> discovers dependency graph and output contracts ->
#' baseline generation and immediate program repair -> full bundle attempt execution
#' and output-driven repair -> deterministic gate selection and reporting.
#'
#' Called macro dependencies must resolve during offline dependency mapping.
#' Missing definitions or unsupported dynamic macro calls raise a
#' `sas2r_macro_dependency_error` before provider setup or model requests, with
#' source locations and configuration guidance. Use [sas_preflight()] to inspect
#' the findings, and configure `macros.search_path` in `_sas2r.yml` when definitions
#' live outside the scanned sources.
#'
#' @param path Path to a SAS file or directory containing SAS files, or a `sas2r_project`.
#' @param out_dir Output directory path for generated R bundle, attempts, and reports. Defaults to a temporary directory.
#'   Relative paths resolve from the calling working directory before workers start.
#' @param config Optional configuration list, YAML path, or `sas2r_config` object.
#'   With a reused project, a plain list replaces only its supplied top-level
#'   fields; omitted fields inherit, and explicit `NULL` resets a field. A YAML
#'   path or `sas2r_config` object supplies a complete configuration, as does a
#'   plain list with a source path. Changing library or source-search settings
#'   requires rescanning the source path. Reference paths in YAML resolve from
#'   its directory; paths in R lists resolve from the project directory.
#' @param execute Logical; whether to run meaningful program smoke and full bundle execution. Defaults to TRUE.
#' @param max_program_repair_rounds Maximum repair rounds per component in immediate loop. Defaults to 1L.
#' @param max_bundle_repair_rounds Optional overall cap on bundle fixer calls.
#'   Defaults to NULL, allowing each component its own bounded repair allowance.
#'   An explicit zero disables bundle repair.
#' @param max_bundle_repairs_per_component Maximum bundle fixer calls per component
#'   across the run. Defaults to 2L; separate from immediate program repairs.
#' @param outputs Optional character vector or list specifying output contract overrides.
#'   Reference paths supplied here resolve from the calling working directory.
#'   Output overrides are retained in the returned project for reuse.
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
#'   match. Full-bundle execution and output checks use fresh attempts; passing
#'   or deferred smoke results can be reused when their full context matches.
#'   Changing only the parallel translation limit does not invalidate reuse.
#'   Missing or edited revision files trigger regeneration. Compatible version-8
#'   checkpoints are imported; other incompatible checkpoints regenerate with an
#'   explanation before new provider calls. Defaults to FALSE.
#' @param max_parallel_translations Maximum concurrent SAS program or macro translation workflows. Overrides
#'   `migration.max_parallel_translations` in `_sas2r.yml`; defaults to 1. Model waiting can
#'   overlap on fewer CPUs. Local execution and repair remain serial.
#'   Values above 1 cannot be combined with `llm.max_tries` above 1; preflight
#'   and translation stop with a configuration error before provider calls.
#' @param keep_raw_attempts Logical; retain raw outputs from unselected attempts,
#'   including isolated component smoke outputs and their replay scripts. These
#'   are partial debugging artifacts, not validated final outputs. Defaults to FALSE.
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
  max_bundle_repair_rounds = NULL,
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
  keep_raw_attempts = FALSE,
  max_bundle_repairs_per_component = 2L,
  max_parallel_translations = NULL
) {
  if (!is.null(max_parallel_translations)) max_parallel_translations <- normalize_max_parallel_translations(max_parallel_translations)
  max_bundle_repair_rounds <- bundle_repair_limit(max_bundle_repair_rounds,
    "max_bundle_repair_rounds", allow_null = TRUE)
  max_bundle_repairs_per_component <- bundle_repair_limit(max_bundle_repairs_per_component,
    "max_bundle_repairs_per_component")
  agent_evidence <- if (is.character(agent_evidence)) match.arg(agent_evidence, c("code_only", "bounded")) else "code_only"

  # Resolve budget and configuration before output/cache writes or providers.
  if (is.null(out_dir)) out_dir <- tempfile(pattern = "sas2r_out_")
  paths <- migration_paths(out_dir)
  budget <- translation_budget(
    budget_usd, budget_mode, pricing_source, pricing_rates, usage_limits,
    ledger_path = file.path(paths$state, "usage.jsonl"), resume = resume
  )
  paths <- migration_paths(out_dir, budget$run_id)
  state <- list(paths = paths, usage_budget = budget, execute = isTRUE(execute))
  stage <- "preflight"
  tryCatch({
  setup <- translation_setup(path, config, outputs, recursive, cache = TRUE,
                             max_parallel_translations = max_parallel_translations, llm = llm)
  translation_limit <- setup$max_parallel_translations
  paths <- init_migration_paths(out_dir, budget$run_id)
  state$paths <- paths
  state$project <- setup$project
  state$graph <- setup$plan$graph
  state$schedule <- setup$plan$schedule
  state$output_contracts <- setup$plan$contracts
  state$diagnostics$pipeline <- setup$plan$pipeline
  if (sas2r_progress_enabled()) {
    cli::cat_line(pipeline_coverage_lines(setup$plan$pipeline), file = stderr())
    flush(stderr())
  }
  require_resolved_macros(setup$project)
  require_complete_pipeline(setup$plan$pipeline)
  cfg <- setup$config
  project <- setup$project
  plan <- setup$plan
  output_contracts <- plan$contracts
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$state, recursive = TRUE, showWarnings = FALSE)
  output_contracts_path <- file.path(paths$state, "output-contracts.json")
  write_output_contracts(output_contracts, output_contracts_path)

  # 5. Dependency graph and stable schedule
  graph <- plan$graph
  atomic_write_json(graph, paths$graph)

  # 7. Resolve LLM adapter from argument or config
  resolved_llm <- if (!is.null(llm)) {
    llm
  } else if (!is.null(cfg$llm)) {
    sas_llm(cfg$llm)
  } else {
    NULL
  }

  stage <- "initialize"
  # 8. Initialize migration state
  state <- new_migration_state(
    project = project,
    out_dir = out_dir,
    llm = resolved_llm,
    config = cfg,
    execute = isTRUE(execute),
    max_program_repair_rounds = as.integer(max_program_repair_rounds),
    max_bundle_repair_rounds = max_bundle_repair_rounds,
    max_bundle_repairs_per_component = max_bundle_repairs_per_component,
    usage_budget = budget,
    plan = plan
  )
  # Adopt the state's run-scoped paths for code, execution evidence, and reports.
  paths <- state$paths
  state$diagnostics$pipeline <- plan$pipeline
  state$output_contracts <- output_contracts
  state$parallel <- resolve_parallel_execution(state, translation_limit)
  state$diagnostics$parallel <- state$parallel
  state$agent_evidence <- agent_evidence
  state$keep_raw_attempts <- isTRUE(keep_raw_attempts)

  # Resume exact revision records only when all relevant inputs still match.
  resume_fingerprint <- migration_resume_fingerprint(state)
  state$resume_fingerprint <- resume_fingerprint
  if (isTRUE(resume)) state <- restore_migration_checkpoint(state, resume_fingerprint)

  # 9./10. Program pipeline (baseline translation, mechanical checks, review,
  # immediate repair) then bundle pipeline (full execution attempt, output
  # assessment, bundle repair, selection). Both signal sas2r_progress
  # conditions, and this is the one place the console renderer is installed --
  # without it a long metered run prints nothing.
  stage <- "translation"
  state <- with_sas2r_progress({
    state$environment <- migration_environment(state)
    append_usage_record(budget, list(record_type = "run_environment",
                                    run_id = state$run_id, environment = state$environment))
    signalCondition(structure(list(event = "run_environment", phase = "environment",
                                   environment = state$environment),
                              class = c("sas2r_progress", "condition")))
    prepare_migration_llm_settings(state)
    state <- run_program_pipeline(
      state = state,
      max_program_repair_rounds = as.integer(max_program_repair_rounds),
      execute = isTRUE(execute)
    )
    if (!length(state$diagnostics$parallel_deferred)) {
      stage <- "bundle execution and repair"
      state <- run_bundle_pipeline(
        state = state,
        max_bundle_repair_rounds = max_bundle_repair_rounds,
        max_bundle_repairs_per_component = max_bundle_repairs_per_component,
        execute = isTRUE(execute)
      )
    }
    state
  })

  stage <- "finalization"
  selected_att <- state$selected_attempt
  executed_bundle <- if (!is.null(selected_att$attempt_dir)) {
    file.path(selected_att$attempt_dir, "bundle")
  } else snapshot_selected_bundle(state, file.path(paths$diagnostics, "partial_bundle"))
  materialize_user_bundle(executed_bundle, paths$bundle, state$project)
  bundle_dir <- paths$bundle
  state$bundle_dir <- bundle_dir
  state$saved_outputs <- materialize_run_outputs(state)
  outputs_dir <- if (length(state$saved_outputs)) paths$outputs else NULL
  state$outputs_dir <- outputs_dir

  # Status adjustments for execute = FALSE
  if (isFALSE(execute)) {
    state$outputs_dir <- NULL
    outputs_dir <- NULL
    # If there are check failures, status is blocked; otherwise needs_review
    has_check_failure <- any(vapply(state$selected_revisions, function(r) {
      identical(r$status, "check_failed")
    }, logical(1)))
    if (length(state$diagnostics$parallel_deferred)) {
      state$status <- "blocked"
    } else if (has_check_failure) {
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

  # 13. Write authoritative machine and markdown reports
  budget$end_time <- Sys.time()
  write_migration_report(state, emit_outcome = TRUE)
  write_migration_checkpoint(state, resume_fingerprint)
  with_sas2r_progress(signal_bundle_event(
    "migration_summary", summary = migration_usage_lines(migration_usage_summary(budget))
  ))

  cli::cat_line("Start here: ", paths$start_here)
  cli::cat_line("Bundle: ", bundle_dir)
  if (!is.null(outputs_dir)) cli::cat_line("Saved outputs: ", outputs_dir)

  # 14. Return canonical sas2r_translation object
  structure(
    list(
      run_id = budget$run_id %||% new_usage_run_id(),
      out_dir = paths$root,
      bundle_dir = bundle_dir,
      outputs_dir = outputs_dir,
      status = state$status,
      status_reason = state$status_reason,
      graph_path = file.path(paths$report_dir, "graph.json"),
      output_contracts_path = file.path(paths$report_dir, "output-contracts.json"),
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
  }, error = function(error) {
    if (identical(stage, "preflight") && inherits(error, c("sas2r_invalid_argument",
        "sas2r_config_error", "sas2r_llm_config_error", "sas2r_output_contract_error", "sas2r_budget_config_error"))) stop(error)
    finalize_usage_run(budget, terminal_status = "failed")
    # Evidence writing must never hide the original failure, including an
    # unwritable output location. No additional success state is invented.
    tryCatch({
      paths <- init_migration_paths(out_dir, budget$run_id)
      state$paths <- paths
      state$status <- "blocked"
      state$status_reason <- conditionMessage(error)
      state$diagnostics$failure <- list(stage = stage, message = conditionMessage(error))
      if (length(state$selected_revisions) && !dir.exists(paths$bundle)) {
        partial <- snapshot_selected_bundle(state, file.path(paths$diagnostics, "partial_bundle"))
        materialize_user_bundle(partial, paths$bundle, state$project)
      }
      state$bundle_dir <- if (dir.exists(paths$bundle)) paths$bundle else NULL
      write_migration_report(state, emit_outcome = TRUE)
      cli::cat_line("Blocked run: ", paths$start_here)
    }, error = function(report_error) {
      cli::cat_line("Could not write run report: ", conditionMessage(report_error))
    })
    stop(error)
  })
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
#' Copies the selected editable bundle (including failed code), with its
#' `programs/`, `macros/`, `runtime/`, `autoexec.R`, `run.R`, and README.
#' Saved automated deliverables go to `saved-outputs/` and reports to `report/`.
#' Existing manual `output/` files are not copied; configuration edits are kept.
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

  materialize_user_bundle(x$bundle_dir, dir, project = x$project)
  inventory <- if (!is.null(x$outputs_dir)) attempt_output_hashes(x$outputs_dir) else list()
  if (length(inventory)) copy_output_inventory(x$outputs_dir, file.path(dir, "saved-outputs"), inventory)

  if (!is.null(x$report_path) && file.exists(x$report_path)) {
    dir.create(file.path(dir, "report"), showWarnings = FALSE)
    file.copy(x$report_path, file.path(dir, "report", basename(x$report_path)), overwrite = TRUE)
  }

  if (!is.null(x$report_json_path) && file.exists(x$report_json_path)) {
    state_dir <- file.path(dir, "report")
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
  run_order <- if (file.exists(run_order_path)) read_json_record(run_order_path) else list()
  entrypoint <- run_order$entrypoint
  prog_files <- all_files[!basename(all_files) %in% SAS2R_BUNDLE_FILES &
                           !all_files %in% file.path(b_dir, entrypoint)]
  if (identical(run_order$layout, "organized")) {
    prog_files <- all_files[startsWith(all_files, paste0(b_dir, "/programs/")) |
                              startsWith(all_files, paste0(b_dir, "/macros/"))]
    all_files <- prog_files
  }
  if (!length(prog_files)) {
    cli::cli_abort("No program code generated in {.file {b_dir}}", class = "sas2r_file_not_found")
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
