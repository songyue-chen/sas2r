#' Repair a program revision using structured review, smoke, bundle, or output evidence
#'
#' @param revision The component revision list/object to fix.
#' @param review Optional program review record from `review_program_revision()`.
#' @param smoke Optional smoke execution record.
#' @param bundle Optional bundle execution record.
#' @param outputs Optional output differences record or list.
#' @param mode Mode of repair: "program" (single component) or "bundle" (cross-component).
#' @param llm Optional `sas2r_llm` instance.
#' @param usage Optional usage budget.
#' @param limits Optional limits list.
#' @param paths Optional migration paths list.
#' @param project Optional `sas2r_project` object.
#' @param config Configuration list.
#' @param project_dir Optional project directory.
#' @param round Integer repair iteration round (default 1L).
#' @param attempt_id Optional attempt identifier.
#' @param ... Additional arguments.
#' @return A new `sas2r_program_revision` object.
#' @noRd
fix_program_revision <- function(
  revision,
  review = NULL,
  smoke = NULL,
  bundle = NULL,
  outputs = NULL,
  mode = c("program", "bundle"),
  llm = NULL,
  usage = NULL,
  limits = NULL,
  paths = NULL,
  project = NULL,
  config = list(),
  project_dir = NULL,
  round = 1L,
  attempt_id = NULL,
  report_registry = NULL,
  ...
) {
  mode <- match.arg(mode)

  # 1. Collect evidence IDs
  evidence_ids <- character()
  if (!is.null(review)) {
    rev_id <- review$review_id %||% review$id %||% review$basis_id
    if (!is.null(rev_id)) evidence_ids <- c(evidence_ids, as.character(rev_id))
  }
  if (!is.null(smoke)) {
    smoke_id <- smoke$execution_id %||% smoke$run_id %||% smoke$id
    if (!is.null(smoke_id)) evidence_ids <- c(evidence_ids, as.character(smoke_id))
  }
  if (!is.null(bundle)) {
    bundle_id <- bundle$bundle_id %||% bundle$execution_id %||% bundle$id
    if (!is.null(bundle_id)) evidence_ids <- c(evidence_ids, as.character(bundle_id))
  }
  if (!is.null(outputs)) {
    if (is.list(outputs) && !is.data.frame(outputs)) {
      out_ids <- unlist(lapply(outputs, function(o) o$output_id %||% o$evidence_id %||% o$id))
      evidence_ids <- c(evidence_ids, as.character(out_ids))
    } else if (is.character(outputs)) {
      evidence_ids <- c(evidence_ids, outputs)
    }
  }
  extra_args <- list(...)
  if (!is.null(extra_args$evidence_ids)) {
    evidence_ids <- c(evidence_ids, as.character(extra_args$evidence_ids))
  }
  evidence_ids <- unique(evidence_ids[!is.na(evidence_ids) & nzchar(evidence_ids)])

  if (length(evidence_ids) == 0L) {
    cli::cli_abort(
      "fix_program_revision requires at least one material evidence ID",
      class = "sas2r_fixer_missing_evidence"
    )
  }

  # 2. Extract component details and prior revision
  component_id <- revision$component_id %||% "unknown"
  prior_revision_id <- revision$revision_id %||% "r1"
  contract <- revision$contract %||% NULL

  r_code <- revision$r_code %||% NULL
  if (is.null(r_code) && !is.null(revision$r_path) && file.exists(revision$r_path)) {
    r_code <- paste(readLines(revision$r_path, warn = FALSE), collapse = "\n")
  }
  if (is.null(r_code)) r_code <- ""

  sas_text <- revision$sas_text %||% revision$sas_source %||% contract$sas_text %||% ""
  if (!nzchar(sas_text) && !is.null(project) && !is.null(project$statements)) {
    stmts <- project$statements[project$statements$file == paste0(component_id, ".sas") |
                                tools::file_path_sans_ext(basename(project$statements$file)) == component_id, , drop = FALSE]
    if (nrow(stmts) > 0L) {
      sas_text <- format_sas_statements(stmts$text)
    }
  }

  comments_text <- revision$comments %||% revision$comments_text %||% "(none attached)"

  # 3. Format evidence description
  evidence_sections <- character()
  if (!is.null(review)) {
    f_text <- if (length(review$findings) > 0L) {
      paste(vapply(review$findings, function(f) {
        sprintf("- [%s] SAS: %s | R: %s | outputs: %s",
                f$severity %||% "material",
                f$sas_evidence %||% "",
                f$r_evidence %||% "",
                paste(f$affected_outputs %||% character(), collapse = ", "))
      }, character(1)), collapse = "\n")
    } else {
      "(no structured findings)"
    }
    evidence_sections <- c(evidence_sections, sprintf(
      "Static Review (verdict: %s, runnability: %s, ID: %s):\n%s",
      review$verdict %||% "repair_required",
      review$static_runnability %||% "unknown",
      review$review_id %||% "unknown",
      f_text
    ))
  }
  if (!is.null(smoke)) {
    evidence_sections <- c(evidence_sections, sprintf(
      "Smoke Execution Failure (ID: %s, exit: %s):\nError: %s\nLog:\n%s",
      smoke$execution_id %||% smoke$id %||% "unknown",
      smoke$exit_code %||% 1,
      smoke$error %||% smoke$message %||% "(none)",
      smoke$log %||% "(none)"
    ))
  }
  if (!is.null(bundle)) {
    evidence_sections <- c(evidence_sections, sprintf(
      "Bundle Execution Failure (ID: %s):\nFailing outputs: %s\nError: %s\nLog:\n%s",
      bundle$bundle_id %||% bundle$execution_id %||% "unknown",
      paste(bundle$failing_outputs %||% character(), collapse = ", "),
      bundle$error %||% bundle$message %||% "(none)",
      bundle$log %||% "(none)"
    ))
  }
  if (!is.null(outputs)) {
    evidence_sections <- c(evidence_sections, sprintf(
      "Output Differences:\n%s",
      if (is.character(outputs)) paste(outputs, collapse = "\n") else jsonlite::toJSON(outputs, auto_unbox = TRUE)
    ))
  }
  evidence_text <- paste(evidence_sections, collapse = "\n\n")

  # 4. Route skills from what actually failed: the component's PROCs and
  # source-derived ordering flags, plus comparison reasons mapped from the
  # difference evidence -- so the alignment and ordering skills reach the
  # fixer exactly when a failed comparison shows their failure modes.
  catalog <- agent_skill_catalog()
  comp_stmts <- component_statements(project, component_id)
  procs <- if (!is.null(comp_stmts)) unit_proc_names(comp_stmts) else character()
  routing_ctx <- list(
    agent = "fixer",
    unit_type = "program",
    procs = procs,
    flags = skill_flags_from_sas(sas_text),
    macros = character(0),
    semantic_rules = if (length(procs)) paste0("procs.", procs) else character(0),
    comparison_reasons = comparison_reasons_from_outputs(outputs),
    functions = character(0)
  )
  routed <- route_agent_skills(routing_ctx, catalog = catalog)
  rendered_skills <- render_agent_skills(routed)

  specs <- load_agent_specs(project_dir = project_dir)
  spec <- as.list(specs$fixer)
  spec$output_schema <- "program_fix_v1"

  usage_budget <- usage %||% new_usage_budget()

  audit_context <- list(
    role = "fixer",
    component_id = component_id,
    revision_id = prior_revision_id,
    round = round,
    attempt_id = attempt_id,
    mode = mode,
    evidence_ids = evidence_ids,
    purpose = "program_fix"
  )

  prompt_vars <- list(
    unit = sas_text,
    comments = comments_text,
    staged_r = r_code,
    evidence = evidence_text,
    skills = rendered_skills,
    allowlist = config$allowlist %||% "dplyr, tidyr, haven"
  )

  # The fixer's prompt deliberately carries no context packet; these tools are
  # its route to the unit's statements, schemas, configured macros, and the
  # bounded comparison report -- so they get the real objects, not emptiness.
  tools <- build_tools(spec, list(
    project = project,
    unit_stmts = comp_stmts,
    schemas = tryCatch(infer_schemas(project), error = function(e) list()),
    config = config,
    macro_index = project_macro_index(project, config),
    report_registry = report_registry
  ))

  agent_res <- run_agent(
    spec = spec,
    llm = llm,
    tools = tools,
    user_content = "Repair the program using the failing evidence.",
    log_dir = if (!is.null(paths)) paths$state else ".sas2r",
    prompt_vars = prompt_vars,
    audit_context = audit_context,
    usage_budget = usage_budget
  )

  if (!identical(agent_res$status, "ok") || is.null(agent_res$data)) {
    cli::cli_abort(
      paste0("Fixer agent failed: ", agent_res$status %||% "unknown"),
      class = "sas2r_fixer_agent_error"
    )
  }

  fix_data <- agent_res$data

  # 5. Check for forbidden mutations
  if (!is.null(fix_data$bundle_helper_patch)) {
    patch_path <- fix_data$bundle_helper_patch$path %||% ""
    # In program or bundle mode, permit only helper patches snapshotting bundle helpers (not .sas, not system/package)
    is_sas <- grepl("\\.sas$", patch_path, ignore.case = TRUE)
    is_absolute <- grepl("^(/|\\\\|[A-Za-z]:)", patch_path)
    is_data <- grepl("(\\.rds|\\.csv|\\.parquet|\\.sas7bdat)$", patch_path, ignore.case = TRUE)
    is_library <- grepl("^(input|data|lib|packages)/", patch_path, ignore.case = TRUE)
    if (is_sas || is_absolute || is_data || is_library) {
      cli::cli_abort(
        "Fixer attempted forbidden mutation on {.path {patch_path}}",
        class = "sas2r_fixer_forbidden_mutation"
      )
    }
  }

  # 6. Create new revision leaving prior revision immutable
  new_rev_id <- paste0("rev_", substr(migration_hash(list(component_id, prior_revision_id, fix_data$r_code, fix_data$diagnosis, evidence_ids, Sys.time())), 1L, 16L))
  patch_h <- migration_hash(list(old_code = r_code, new_code = fix_data$r_code, helper_patch = fix_data$bundle_helper_patch))

  new_r_hash <- migration_hash(fix_data$r_code)
  old_binding <- contract$binding %||% revision$binding %||% list()
  new_binding <- new_component_binding(
    source_hash = old_binding$source_hash %||% migration_hash(sas_text),
    r_hash = new_r_hash,
    helper_hash = old_binding$helper_hash %||% migration_hash(""),
    prompt_skill_hash = old_binding$prompt_skill_hash %||% migration_hash("fixer"),
    dependency_closure_hash = old_binding$dependency_closure_hash %||% migration_hash("closure")
  )

  new_contract <- contract %||% new_behavioral_contract(
    component_id = component_id,
    binding = new_binding
  )
  new_contract$binding <- new_binding
  new_contract$diagnosis <- fix_data$diagnosis
  new_contract$patch_hash <- patch_h
  new_contract$evidence_ids <- unique(c(evidence_ids, unlist(fix_data$evidence_ids %||% character())))

  new_r_path <- NULL
  new_contract_path <- NULL

  if (!is.null(paths)) {
    new_rev_dir <- file.path(paths$programs %||% paths$root, component_id, "revisions", new_rev_id)
    dir.create(new_rev_dir, recursive = TRUE, showWarnings = FALSE)
    new_r_path <- file.path(new_rev_dir, "program.R")
    new_contract_path <- file.path(new_rev_dir, "contract.json")
    writeLines(fix_data$r_code, new_r_path)
    atomic_write_json(new_contract, new_contract_path)
    if (!is.null(fix_data$bundle_helper_patch)) {
      hp_file <- file.path(new_rev_dir, fix_data$bundle_helper_patch$path)
      dir.create(dirname(hp_file), recursive = TRUE, showWarnings = FALSE)
      writeLines(fix_data$bundle_helper_patch$content, hp_file)
    }
  } else {
    new_r_path <- tempfile(fileext = ".R")
    writeLines(fix_data$r_code, new_r_path)
  }

  checks <- check_program_revision(new_r_path, contract = new_contract)

  structure(
    list(
      component_id = component_id,
      revision_id = new_rev_id,
      prior_revision_id = prior_revision_id,
      mode = mode,
      r_code = fix_data$r_code,
      r_path = new_r_path,
      contract = new_contract,
      contract_path = new_contract_path,
      diagnosis = fix_data$diagnosis,
      summary = fix_data$summary,
      evidence_ids = unique(c(evidence_ids, unlist(fix_data$evidence_ids %||% character()))),
      patch_hash = patch_h,
      bundle_helper_patch = fix_data$bundle_helper_patch,
      changed_interfaces = unlist(fix_data$changed_interfaces %||% character()),
      affected_outputs = unlist(fix_data$affected_outputs %||% character()),
      remaining_uncertainty = unlist(fix_data$remaining_uncertainty %||% character()),
      status = if (isTRUE(checks$pass)) "ok" else "check_failed",
      checks = checks,
      spend_usd = agent_res$spend_usd %||% 0
    ),
    class = c("sas2r_program_revision", "list")
  )
}

