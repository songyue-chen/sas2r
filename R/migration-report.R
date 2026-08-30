#' SAS to R Migration Report Generation
#'
#' Implements machine-readable JSON and human-readable Markdown reporting for
#' dependency-aware migrations, capturing graph facts, component evidence history,
#' output assessments, repair iterations, diagnostics, and usage accounting.

#' Helper to redact actual secrets and API credentials from diagnostic strings
#'
#' @param x Object or character vector to redact.
#' @return Redacted object or character vector.
#' @noRd
redact_secrets <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) {
    # Redact common LLM API keys and bearer tokens
    x <- gsub("sk-[A-Za-z0-9_-]{20,}", "[REDACTED_API_KEY]", x)
    x <- gsub("AIza[A-Za-z0-9_-]{30,}", "[REDACTED_API_KEY]", x)
    x <- gsub("Bearer\\s+[A-Za-z0-9._~+/-]{20,}", "Bearer [REDACTED_TOKEN]", x, ignore.case = TRUE)
    return(x)
  }
  if (is.list(x)) {
    return(lapply(x, redact_secrets))
  }
  x
}

#' Convert a data frame or tibble to a markdown table string
#'
#' @param df Data frame or tibble.
#' @return Markdown formatted table string.
#' @noRd
migration_md_table <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
    return("_(none)_")
  }
  clean_cell <- function(col) {
    s <- as.character(col)
    s[is.na(s)] <- "NA"
    s <- gsub("\\|", "\\\\|", s)
    s <- gsub("[\r\n]+", " ", s)
    s
  }
  clean_df <- as.data.frame(lapply(df, clean_cell), stringsAsFactors = FALSE)
  hdr <- paste0("| ", paste(names(clean_df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(clean_df)), collapse = "|"), "|")
  rows <- apply(clean_df, 1L, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(hdr, sep, rows), collapse = "\n")
}

#' Write authoritative migration report in JSON and Markdown formats
#'
#' Produces `.sas2r/report.json` and `report.md` capturing graph, execution order,
#' unresolved edges, output inventory, component evidence revisions, review/smoke
#' records, attempts, output assessments, status reasons, repair history, input hashes,
#' selected paths, and usage/cost accounting.
#'
#' @param state Migration state object, bundle pipeline result, or translation object.
#' @return The path to the written markdown report, invisibly.
#' @noRd
write_migration_report <- function(state) {
  if (is.null(state)) {
    cli::cli_abort("{.arg state} must be provided", class = "sas2r_invalid_argument")
  }

  paths <- if (is.list(state$paths)) {
    state$paths
  } else if (!is.null(state$out_dir)) {
    migration_paths(state$out_dir)
  } else {
    cli::cli_abort("Unable to determine migration paths from state", class = "sas2r_invalid_argument")
  }

  dir.create(paths$state, recursive = TRUE, showWarnings = FALSE)

  run_id <- state$run_id %||% paths$run_id %||% state$usage_budget$run_id %||% new_usage_run_id()
  status <- state$status %||% "blocked"
  status_reason <- state$status_reason %||% NULL

  bundle_dir <- state$bundle_dir %||% (
    if (!is.null(state$selected_attempt)) {
      file.path(state$selected_attempt$attempt_dir, "bundle")
    } else {
      file.path(paths$state, "selected", "bundle")
    }
  )
  outputs_dir <- state$outputs_dir %||% NULL

  # Graph facts
  graph_nodes <- if (!is.null(state$graph$nodes)) state$graph$nodes else tibble::tibble()
  graph_edges <- if (!is.null(state$graph$edges)) state$graph$edges else tibble::tibble()
  unresolved_edges <- if (nrow(graph_edges) > 0L) {
    graph_edges[graph_edges$resolution != "resolved", , drop = FALSE]
  } else {
    tibble::tibble()
  }
  execution_order <- if (!is.null(state$schedule$component_id)) {
    state$schedule$component_id
  } else {
    character()
  }

  # Output contracts & assessments
  output_contracts <- state$output_contracts %||% empty_output_contracts()
  output_contracts_path <- file.path(paths$state, "output-contracts.json")
  if (nrow(output_contracts) > 0L) {
    write_output_contracts(output_contracts, output_contracts_path)
  }

  output_assessments <- if (!is.null(state$assessment$targets)) {
    state$assessment$targets
  } else if (!is.null(state$output_assessments)) {
    state$output_assessments
  } else {
    list()
  }

  # Component evidence summary
  component_evidence_list <- list()
  comp_table_rows <- list()
  histories <- state$histories %||% state$component_evidence %||% list()

  for (cid in names(histories)) {
    h <- histories[[cid]]
    curr <- tryCatch(current_component_evidence(h), error = function(e) NULL)
    level <- curr$level %||% "reviewed_only"
    rev_status <- curr$review_status %||% "review_unavailable"
    smoke_status <- if (!is.null(curr$runtime_deferred)) {
      paste0("deferred (", curr$runtime_deferred, ")")
    } else if ("smoke_failed" %in% curr$blockers) {
      "failed"
    } else if (level %in% c("runtime_verified", "output_verified", "reference_validated")) {
      "passed"
    } else {
      "unexecuted"
    }
    blockers <- paste(curr$blockers %||% character(), collapse = ", ")
    if (!nzchar(blockers)) blockers <- "(none)"

    component_evidence_list[[cid]] <- list(
      component_id = cid,
      evidence_level = level,
      review_status = rev_status,
      smoke_status = smoke_status,
      blockers = curr$blockers %||% character(),
      revisions = h$revisions %||% list()
    )

    comp_table_rows[[length(comp_table_rows) + 1L]] <- list(
      component_id = cid,
      evidence_level = level,
      review_status = rev_status,
      smoke_status = smoke_status,
      blockers = blockers
    )
  }

  comp_df <- if (length(comp_table_rows) > 0L) {
    tibble::tibble(
      component = vapply(comp_table_rows, `[[`, character(1), "component_id"),
      level = vapply(comp_table_rows, `[[`, character(1), "evidence_level"),
      review = vapply(comp_table_rows, `[[`, character(1), "review_status"),
      smoke = vapply(comp_table_rows, `[[`, character(1), "smoke_status"),
      blockers = vapply(comp_table_rows, `[[`, character(1), "blockers")
    )
  } else {
    tibble::tibble()
  }

  # Attempts summary
  attempts_data <- if (!is.null(state$attempts)) {
    state$attempts
  } else {
    tibble::tibble()
  }

  # Repair history
  repair_history <- state$repairs %||% state$repair_history %||% list()

  # Input hashes
  input_hashes <- tryCatch(
    input_hash_manifest(state$project %||% state),
    error = function(e) list()
  )

  # Usage and cost
  usage_obj <- state$usage_budget %||% state$usage
  usage_summary <- if (!is.null(usage_obj) && is.list(usage_obj)) {
    list(
      known_amount = usage_obj$known_amount %||% 0,
      calls = usage_obj$calls %||% 0L,
      input_tokens = usage_obj$input_tokens %||% 0L,
      output_tokens = usage_obj$output_tokens %||% 0L,
      pricing_source = usage_obj$pricing_source %||% "catalog"
    )
  } else {
    list(
      known_amount = 0,
      calls = 0L,
      input_tokens = 0L,
      output_tokens = 0L,
      pricing_source = "catalog"
    )
  }

  # Diagnostics
  diagnostics <- redact_secrets(state$diagnostics %||% list(
    stop_reason = state$diagnostics$stop_reason %||% NULL,
    latest_diagnosis = state$diagnostics$latest_diagnosis %||% NULL
  ))

  # 1. Construct JSON report payload
  report_payload <- list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    run_id = run_id,
    status = status,
    status_reason = status_reason,
    selected_paths = list(
      bundle_dir = bundle_dir,
      outputs_dir = outputs_dir,
      graph_path = paths$graph,
      output_contracts_path = output_contracts_path,
      report_path = paths$report_md,
      report_json_path = paths$report_json
    ),
    graph = list(
      nodes = graph_nodes,
      edges = graph_edges,
      unresolved_edges = unresolved_edges,
      execution_order = execution_order
    ),
    output_inventory = list(
      contracts = output_contracts
    ),
    output_assessments = output_assessments,
    component_evidence = component_evidence_list,
    attempts = attempts_data,
    repair_history = repair_history,
    input_hashes = input_hashes,
    usage = usage_summary,
    diagnostics = diagnostics,
    created_at = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  # Write JSON report
  atomic_write_json(report_payload, paths$report_json)

  # When paths are run-scoped, the run's own folder gets a copy of the machine
  # report so runs/<run_id>/ is self-contained evidence; the .sas2r/ copy
  # stays authoritative for the latest run (resume reads it).
  run_dir <- if (identical(basename(paths$attempts %||% ""), run_id)) paths$attempts else NULL
  if (!is.null(run_dir) && dir.exists(run_dir)) {
    atomic_write_json(report_payload, file.path(run_dir, "report.json"))
  }

  # 2. Construct Markdown report lines
  md_lines <- c(
    "# SAS to R Migration Report",
    "",
    paste0("- **Status:** `", status, "`"),
    if (!is.null(status_reason) && nzchar(status_reason)) paste0("- **Status Reason:** ", status_reason) else NULL,
    paste0("- **Run ID:** `", run_id, "`"),
    paste0("- **Timestamp:** `", report_payload$created_at, "`"),
    "",
    "## Selected Artifacts",
    "",
    paste0("- **Generated R Bundle:** `", bundle_dir %||% "(none)", "`"),
    paste0("- **Outputs Directory:** `", outputs_dir %||% "(none - execution disabled or no outputs generated)", "`"),
    paste0("- **Dependency Graph:** `", paths$graph, "`"),
    paste0("- **Output Contracts:** `", output_contracts_path, "`"),
    paste0("- **Machine Report:** `", paths$report_json, "`"),
    "",
    "## Component Evidence & Verification",
    "",
    migration_md_table(comp_df),
    ""
  )

  # Output target table
  if (nrow(output_contracts) > 0L) {
    target_rows <- list()
    for (i in seq_len(nrow(output_contracts))) {
      t_key <- output_contracts$target_key[i]
      t_kind <- output_contracts$kind[i]
      t_req <- output_contracts$required[i]
      ass <- if (!is.null(output_assessments[[t_key]])) output_assessments[[t_key]] else NULL
      t_stat <- ass$status %||% "unverified"
      t_rsn <- ass$reason %||% "(none)"
      target_rows[[length(target_rows) + 1L]] <- list(
        target = t_key,
        kind = t_kind,
        required = if (isTRUE(t_req)) "yes" else "no",
        status = t_stat,
        reason = t_rsn
      )
    }
    targets_df <- tibble::tibble(
      target = vapply(target_rows, `[[`, character(1), "target"),
      kind = vapply(target_rows, `[[`, character(1), "kind"),
      required = vapply(target_rows, `[[`, character(1), "required"),
      status = vapply(target_rows, `[[`, character(1), "status"),
      reason = vapply(target_rows, `[[`, character(1), "reason")
    )
    md_lines <- c(
      md_lines,
      "## Output Targets & Assessments",
      "",
      migration_md_table(targets_df),
      ""
    )
  }

  # Dependency Schedule
  if (length(execution_order) > 0L) {
    sched_df <- tibble::tibble(
      order = seq_along(execution_order),
      component = execution_order
    )
    md_lines <- c(
      md_lines,
      "## Execution Schedule",
      "",
      migration_md_table(sched_df),
      ""
    )
  }

  # Unresolved dependencies
  if (nrow(unresolved_edges) > 0L) {
    unres_clean <- tibble::tibble(
      from = unresolved_edges$from %||% character(),
      to = unresolved_edges$to %||% character(),
      type = unresolved_edges$type %||% character(),
      detail = unresolved_edges$detail %||% character()
    )
    md_lines <- c(
      md_lines,
      "## Unresolved Dependencies",
      "",
      migration_md_table(unres_clean),
      ""
    )
  }

  # Repair history
  if (length(repair_history) > 0L) {
    rep_rows <- list()
    for (r in repair_history) {
      rep_rows[[length(rep_rows) + 1L]] <- list(
        round = as.character(r$round %||% 1L),
        component = as.character(r$component_id %||% "unknown"),
        diagnosis = as.character(r$diagnosis %||% "patch applied"),
        summary = as.character(r$summary %||% ""),
        cost = sprintf("$%.4f", r$spend_usd %||% 0)
      )
    }
    rep_df <- tibble::tibble(
      round = vapply(rep_rows, `[[`, character(1), "round"),
      component = vapply(rep_rows, `[[`, character(1), "component"),
      diagnosis = vapply(rep_rows, `[[`, character(1), "diagnosis"),
      summary = vapply(rep_rows, `[[`, character(1), "summary"),
      cost = vapply(rep_rows, `[[`, character(1), "cost")
    )
    md_lines <- c(
      md_lines,
      "## Repair History",
      "",
      migration_md_table(rep_df),
      ""
    )
  }

  # Usage & Budget
  md_lines <- c(
    md_lines,
    "## Resource Usage & Spend",
    "",
    paste0("- **Total LLM Calls:** ", usage_summary$calls),
    paste0("- **Total Input Tokens:** ", usage_summary$input_tokens),
    paste0("- **Total Output Tokens:** ", usage_summary$output_tokens),
    paste0("- **Total Spend USD:** $", sprintf("%.4f", usage_summary$known_amount)),
    ""
  )

  # Write Markdown report
  writeLines(md_lines, paths$report_md)

  # Per-run markdown report alongside the run's attempts; the out_dir root
  # report.md always reflects the latest run.
  if (!is.null(run_dir) && dir.exists(run_dir)) {
    writeLines(md_lines, file.path(run_dir, "report.md"))
  }

  invisible(paths$report_md)
}

#' Read a migration report JSON record from disk
#'
#' @param path Path to `report.json` or migration directory.
#' @return Named list representing the parsed migration report.
#' @noRd
read_migration_report <- function(path) {
  json_file <- if (dir.exists(path)) {
    f1 <- file.path(path, ".sas2r", "report.json")
    if (file.exists(f1)) f1 else file.path(path, "report.json")
  } else {
    path
  }
  if (!file.exists(json_file)) {
    cli::cli_abort("Migration report file not found: {.file {path}}", class = "sas2r_file_not_found")
  }
  jsonlite::fromJSON(json_file, simplifyVector = TRUE, simplifyDataFrame = FALSE)
}
