#' Repair a program revision using structured review, smoke, bundle, or output evidence
#'
#' @param revision The component revision list/object to fix.
#' @param review Optional program review record from `review_program_revision()`.
#' @param smoke Optional smoke execution record.
#' @param bundle Optional bundle execution record.
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
  checks = NULL,
  selected_revisions = list(),
  ...
) {
  mode <- match.arg(mode)

  # 1. Collect evidence IDs
  evidence_ids <- checks$check_id %||% character()
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
    stmts <- component_statements(project, component_id)
    if (!is.null(stmts) && nrow(stmts) > 0L) {
      sas_text <- format_sas_statements(stmts$text)
    }
  }

  comments_text <- revision$comments %||% revision$comments_text %||% "(none attached)"

  # 3. Format evidence description
  evidence_sections <- character()
  if (!is.null(review)) {
    review$findings <- actionable_review_findings(review)
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
  if (!is.null(checks)) {
    evidence_sections <- c(evidence_sections, sprintf(
      "Mechanical Check Failure (ID: %s):\n%s", checks$check_id,
      paste(checks$errors, collapse = "\n")))
  }
  if (!is.null(smoke)) {
    evidence_sections <- c(evidence_sections, sprintf(
      "Smoke Execution Failure (ID: %s):\n%s",
      smoke$execution_id %||% smoke$id %||% "unknown",
      jsonlite::toJSON(bounded_agent_diagnostics(smoke), auto_unbox = TRUE, pretty = TRUE)
    ))
  }
  if (!is.null(bundle)) {
    evidence_sections <- c(evidence_sections, sprintf(
      "Bundle Execution Evidence (ID: %s):\nFailing outputs: %s\n%s",
      bundle$bundle_id %||% bundle$execution_id %||% "unknown",
      paste(bundle$failing_outputs %||% character(), collapse = ", "),
      jsonlite::toJSON(bounded_agent_diagnostics(bundle), auto_unbox = TRUE, pretty = TRUE)
    ))
  }
  evidence_text <- paste(evidence_sections, collapse = "\n\n")

  # 4. Route skills from what actually failed: the component's PROCs and
  # source-derived ordering flags. Reference comparisons never route repairs.
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
    comparison_reasons = character(),
    functions = character(0)
  )
  routed <- route_agent_skills(routing_ctx, catalog = catalog)
  rendered_skills <- render_agent_skills(routed)

  specs <- load_agent_specs(project_dir = project_dir %||% project$project_dir)
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
    skills = paste(rendered_skills,
      render_component_libraries(project, component_id),
      "Declared component interface (preserve names and defaults):",
      render_macro_interface(contract$macro_contract),
      "Upstream macro interfaces (loaded by autoexec.R):",
      render_dependency_interfaces(project, component_id),
      "Resolved project functions (call by name; do not redefine):",
      paste(contract$dependency_functions %||% character(), collapse = ", "),
      build_agent_guidance(project, component_id, contract, selected_revisions, config = config)$text, sep = "\n"),
    allowlist = paste(normalize_package_allowlist(config$allowlist), collapse = ", ")
  )

  # Existing role tools retain their scope; deterministic guidance adds no tools.
  tools <- build_tools(spec, list(
    agent_role = "fixer",
    project = project,
    unit_stmts = comp_stmts,
    schemas = tryCatch(infer_schemas(project), error = function(e) list()),
    config = config,
    macro_index = project_macro_index(project, config)
  ))

  agent_res <- run_agent(
    spec = spec,
    llm = llm,
    tools = tools,
    user_content = "Repair the program using the failing evidence.",
    log_dir = if (!is.null(paths)) paths$logs else ".sas2r",
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
  spend_usd <- agent_res$spend_usd %||% 0

  # One correction opportunity for a mechanically broken answer, using the
  # same checker as persisted revisions. This is an ordinary budgeted request.
  candidate_path <- tempfile(fileext = ".R")
  on.exit(unlink(candidate_path), add = TRUE)
  writeLines(fix_data$r_code, candidate_path)
  candidate_checks <- check_program_revision(candidate_path, contract = contract,
    helper_patch = fix_data$bundle_helper_patch, allowlist = config$allowlist)
  retry_errors <- candidate_checks$errors[grepl("^(parse_error|lint_error)", candidate_checks$errors)]
  retry_record <- NULL
  if (length(retry_errors) && usage_budget_allows_future(usage_budget)) {
    retry_record <- list(errors = retry_errors, dynamic_code = any(grepl(
      "banned_function.*(parse|eval)", retry_errors)), prior_revision_id = prior_revision_id)
    retry_res <- run_agent(
      spec = spec, llm = llm, tools = tools,
      user_content = paste("The proposed repair failed mechanical checks. Correct these errors while preserving the source behavior and addressing the original evidence:",
        paste(retry_errors, collapse = "\n"),
        "Use supported operations and the helper contracts. Do not replace banned parse/eval with a handwritten general interpreter. A banned implementation is not proof the source feature is impossible; report remaining unsupported behavior explicitly.",
        "Proposed R code:", fix_data$r_code,
        if (!is.null(fix_data$bundle_helper_patch)) paste("Proposed helper patch:", fix_data$bundle_helper_patch$content),
        sep = "\n\n"),
      log_dir = if (!is.null(paths)) paths$logs else ".sas2r",
      prompt_vars = prompt_vars,
      audit_context = utils::modifyList(audit_context, list(purpose = "mechanical_retry")),
      usage_budget = usage_budget
    )
    spend_usd <- spend_usd + (retry_res$spend_usd %||% 0)
    if (identical(retry_res$status, "ok") && !is.null(retry_res$data)) fix_data <- retry_res$data
  }

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
  new_contract$helper_use <- reconcile_helper_use(fix_data$r_code,
    new_contract$helper_use, new_contract$dependency_functions, refresh = TRUE,
    allowlist = config$allowlist)
  new_contract$binding <- new_binding
  new_contract$diagnosis <- fix_data$diagnosis
  if (isTRUE(new_contract$macro_contract$standalone)) {
    new_contract$flags <- unique(c(setdiff(new_contract$flags, "macro_deferred"),
                                   "llm_authored", "macro_semantics_unverified"))
  }
  new_contract$patch_hash <- patch_h
  new_contract$evidence_ids <- unique(c(evidence_ids, unlist(fix_data$evidence_ids %||% character())))

  new_r_path <- NULL
  new_contract_path <- NULL

  if (!is.null(paths)) {
    new_rev_dir <- file.path(paths$component_revisions %||% paths$root, component_id, "revisions", new_rev_id)
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

  checks <- check_program_revision(new_r_path, contract = new_contract,
    helper_patch = fix_data$bundle_helper_patch, allowlist = config$allowlist)

  structure(
    list(
      component_id = component_id,
      revision_id = new_rev_id,
      staged_file = revision$staged_file,
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
      mechanical_retry = retry_record,
      dependency_notices = dependency_symbol_notices(r_code, fix_data$r_code,
        contract$dependency_functions %||% character()),
      changed_interfaces = unlist(fix_data$changed_interfaces %||% character()),
      affected_outputs = unlist(fix_data$affected_outputs %||% character()),
      remaining_uncertainty = unlist(fix_data$remaining_uncertainty %||% character()),
      status = if (isTRUE(checks$pass)) "ok" else "check_failed",
      checks = checks,
      spend_usd = spend_usd
    ),
    class = c("sas2r_program_revision", "list")
  )
}
