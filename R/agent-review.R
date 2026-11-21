REVIEW_STATUSES <- c("no_issue", "issue_found", "uncertain")
REVIEW_ACTIONS <- c("keep", "repair", "human_review")
REVIEW_ISSUE_CODES <- c(
  "missing_value_semantics", "sort_order_semantics", "by_group_semantics",
  "lineage_mismatch", "sas_default_mismatch", "unsupported_assumption",
  "generated_r_parse_risk", "unit_context_missing", "other_source_grounded"
)

#' The PROCs a translation unit runs, as skill routing sees them
#'
#' Skill triggers key off the proc name, and there is exactly one place to read
#' it from: the statement text. `first_token` for a PROC statement is the bare
#' token `"proc"`, so stripping the keyword off it yields `""` and routes
#' nothing; the extractor's `proc` column lands on `project$lineage`, not on
#' `project$statements`, so it is absent from the rows every agent holds. This
#' is the single spelling for all four routing sites -- translator, both fixer
#' entry points, and the reviewer -- so a PROC unit cannot route one set of
#' skills to one agent and a different set to another.
#'
#' @param unit_stmts Statements data frame for one translation unit.
#' @return A lowercase character vector of proc names, possibly empty.
#' @noRd
unit_proc_names <- function(unit_stmts) {
  if (is.null(unit_stmts) || !is.data.frame(unit_stmts) ||
      nrow(unit_stmts) == 0L) {
    return(character(0))
  }
  if ("proc" %in% names(unit_stmts)) {
    vals <- trimws(as.character(unit_stmts$proc))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals)) return(unique(tolower(vals)))
  }
  if (!"text" %in% names(unit_stmts)) return(character(0))
  txt <- trimws(as.character(unit_stmts$text))
  txt <- txt[!is.na(txt)]
  if (!length(txt)) return(character(0))
  matched <- regmatches(
    txt, regexec("^proc\\s+([A-Za-z_]\\w*)", txt, ignore.case = TRUE))
  names_found <- vapply(matched, function(m) {
    if (length(m) >= 2L) tolower(m[2]) else NA_character_
  }, character(1))
  unique(names_found[!is.na(names_found)])
}

#' Perform independent, read-only program review on a generated revision
#'
#' Evaluates SAS component source and assembled R program for semantic equivalence
#' without receiving any translator chain-of-thought, self-score, or claimed verdict.
#'
#' @param revision Generated program revision list or object.
#' @param context Review context list containing SAS source, contract, interfaces, helper guarantees, etc.
#' @param llm `sas2r_llm` instance.
#' @param usage Optional usage budget.
#' @param limits Optional limits list.
#' @param history Optional component evidence history object.
#' @param paths Optional migration paths list.
#' @param project_dir Optional project directory.
#' @param round Integer review iteration round (default 0L).
#' @param attempt_id Optional attempt identifier.
#' @param reuse_only Return a matching saved review or NULL, without calling an agent.
#' @param ... Additional arguments.
#' @return A `sas2r_program_review` record list.
#' @noRd
review_program_revision <- function(
  revision,
  context,
  llm = NULL,
  usage = NULL,
  limits = NULL,
  history = NULL,
  paths = NULL,
  project_dir = NULL,
  round = 0L,
  attempt_id = NULL,
  reuse_only = FALSE,
  identity_only = FALSE,
  ...
) {
  component_id <- revision$component_id %||% context$component_id %||% "unknown"
  revision_id <- revision$revision_id %||% context$revision_id %||% "r1"
  contract <- revision$contract %||% context$contract %||% context$behavioral_contract %||% NULL
  binding <- revision$binding %||% contract$binding %||% context$binding %||% NULL
  binding_hash <- if (!is.null(binding)) binding$binding_hash %||% binding$hash else NULL

  # Assembled R code
  r_code <- revision$r_code %||% context$r_code %||% context$staged_r %||% NULL
  if (is.null(r_code) && !is.null(revision$r_path) && file.exists(revision$r_path)) {
    r_code <- paste(readLines(revision$r_path, warn = FALSE), collapse = "\n")
  }
  if (is.null(r_code)) r_code <- ""

  # SAS source text
  sas_source <- context$sas_source %||% context$sas_text %||% context$component %||%
    context$unit %||% revision$sas_text %||% revision$sas_source %||%
    contract$sas_text %||% ""
  if (is.data.frame(sas_source) && "text" %in% names(sas_source)) {
    sas_source <- format_sas_statements(sas_source$text)
  }
  sas_text <- as.character(sas_source)[1L] %||% ""
  if (is.na(sas_text) || !nzchar(trimws(sas_text))) {
    source_stmts <- component_statements(context$project, component_id)
    sas_text <- if (!is.null(source_stmts) && nrow(source_stmts)) {
      format_sas_statements(source_stmts$text)
    } else ""
  }

  # Ensure NO translator reasoning or self-score reaches the reviewer
  comments_text <- context$comments %||% context$comments_text %||% "(none attached)"

  # Context packet assembling deterministic facts only
  interfaces <- context$resolved_interfaces %||% context$call_sites %||% contract$known_call_sites %||% list()
  if_txt <- if (length(interfaces) > 0L) {
    paste(vapply(interfaces, function(intf) {
      if (is.list(intf)) {
        sprintf("%s: %s (resolution: %s)", intf$type %||% "call", intf$detail %||% "", intf$resolution %||% "resolved")
      } else {
        as.character(intf)
      }
    }, character(1)), collapse = "\n")
  } else {
    "(none)"
  }

  helpers <- context$helper_guarantees %||% contract$helper_use %||% character()
  helpers_txt <- if (length(helpers) > 0L) paste(helpers, collapse = ", ") else "(none)"

  unresolved_facts <- context$unresolved_graph_facts %||% contract$suspected_dependencies %||% character()
  unres_txt <- if (length(unresolved_facts) > 0L) paste(unresolved_facts, collapse = ", ") else "(none)"

  lineage <- context$output_lineage %||% contract$affected_outputs %||% character()
  lineage_txt <- if (length(lineage) > 0L) paste(lineage, collapse = ", ") else "(none)"

  guidance <- build_agent_guidance(context$project, component_id, contract,
    context$selected_revisions %||% list(), config = context$config %||% context$project$config %||% list(),
    include_consumers = identical(context$phase, "bundle"),
    priority_dependencies = unique(c(context$execution$failed_component_id %||% context$execution$condition$component_id,
      context$priority_dependencies %||% review_context_dependencies(history %||% context$history))))
  review_scope <- if (length(context$focus_outputs) && !isTRUE(context$full_review)) "focused" else "full"
  phase <- context$phase %||% "program"
  diagnostics <- if (!is.null(context$execution)) bounded_agent_diagnostics(context$execution, policy = context$config$agent_evidence %||% "code_only", source_code = r_code) else NULL
  if (!is.null(diagnostics)) {
    # Static review uses observations, not their per-attempt storage locations.
    # Retain the original records and fixer diagnostics for navigation/debugging.
    diagnostics[c("execution_id", "stdout_path", "stderr_path")] <- NULL
  }
  context_packet <- paste(c(
    "Component:", component_id,
    render_component_libraries(context$project, component_id),
    "Source-owned macro interface:", render_macro_interface(contract$macro_contract),
    "Upstream macro interfaces (loaded by autoexec.R):",
    render_dependency_interfaces(context$project, component_id),
    "Resolved interfaces:", if_txt,
    "Helper guarantees:", helpers_txt,
    "Unresolved graph facts:", unres_txt,
    "Output lineage:", lineage_txt,
    guidance$text,
    if (length(context$discrepancy_summary)) c(
      "Investigation-only comparison patterns (not desired values and not proof of a translation defect):",
      jsonlite::toJSON(context$discrepancy_summary, auto_unbox = TRUE, null = "null", na = "null"),
      "Trace these columns through the SAS and selected R. Return only independently source-grounded SAS/R contradictions, not comparison counts or patterns. The reference may be inconsistent with the source. Do not invent source changes to fit it."),
    revision$dependency_notices,
    if (!is.null(diagnostics)) c("Bounded executor observations (not reviewer execution):",
      jsonlite::toJSON(diagnostics, auto_unbox = TRUE, null = "null")),
    if (!is.null(context$helper_code)) c("Candidate shared R helpers:", context$helper_code)
  ), collapse = "\n")

  # Route skills from the component's actual content: its PROC statements and
  # source-derived ordering flags, so the ordering skills reach the reviewer
  # for the sorts and by-group logic they were written for.
  catalog <- agent_skill_catalog()
  comp_stmts <- component_statements(context$project, component_id)
  procs <- if (!is.null(comp_stmts)) unit_proc_names(comp_stmts) else character()
  routing_ctx <- list(
    agent = "reviewer",
    unit_type = "program",
    procs = procs,
    flags = skill_flags_from_sas(sas_text),
    macros = character(0),
    semantic_rules = if (length(procs)) paste0("procs.", procs) else character(0),
    comparison_reasons = character(0),
    functions = character(0)
  )
  routed <- route_agent_skills(routing_ctx, catalog = catalog)
  rendered_skills <- render_agent_skills(routed)

  specs <- load_agent_specs(project_dir = project_dir %||% context$project$project_dir)
  spec <- as.list(specs$reviewer)
  spec$output_schema <- "program_review_v1"

  usage_budget <- usage %||% new_usage_budget()

  audit_context <- list(
    role = "reviewer",
    component_id = component_id,
    revision_id = revision_id,
    round = round,
    attempt_id = attempt_id,
    review_scope = review_scope,
    phase = phase,
    purpose = if (length(context$focus_outputs)) "source_mismatch_review" else "program_review"
  )

  prompt_vars <- list(
    phase = agent_phase_guidance(phase),
    unit = sas_text,
    comments = comments_text,
    staged_r = r_code,
    context = context_packet,
    skills = rendered_skills
  )

  # Completed reviews live in the existing evidence history. All entry paths
  # (component, helper consumers, bundle) use the same exact-context identity.
  # Never restore an old history snapshot when reusing a review.
  history_obj <- history %||% context$history %||% revision$history %||% NULL
  review_key <- migration_hash(list(
    prompt = prompt_vars, scope = review_scope, phase = phase,
    focus = sort(unique(context$focus_outputs %||% character())), binding = binding,
    contract = contract, config = source_review_config(context$config %||% list()),
    dependencies = lapply((context$selected_revisions %||% list())[
      dependency_closure(context$project$graph, component_id)], function(x)
        list(code = revision_code(x), binding = x$binding %||% x$contract$binding)),
    inputs = context$source_input_identity,
    helper_reference = helper_reference(), rulebook = load_rulebook(),
    worker = worker_binding_hash("reviewer", catalog,
      project_dir = project_dir %||% context$project$project_dir, schema = "program_review_v1"),
    reviewer = llm[c("provider", "model", "model_parameters", "endpoint", "api_version")]
  ))
  if (isTRUE(identity_only)) return(review_key)
  events <- current_component_evidence(history_obj)$events %||% list()
  # A later review may add evidence or fail to complete. Never reach back past
  # it for an older clean verdict, even if the old request key matches again.
  events <- utils::tail(Filter(function(e) e$type %in%
    c("review_completed", "review_unavailable"), events), 1L)
  reusable <- Filter(function(e) identical(e$review_key, review_key) &&
    e$review_record$verdict %in% c("reviewed_no_material_finding", "repair_required"), events)
  if (length(reusable)) {
    cached <- reusable[[length(reusable)]]$review_record
    cached$history <- history_obj
    cached$reused <- TRUE
    cached$spend_usd <- 0
    return(cached)
  }
  if (isTRUE(reuse_only)) return(NULL)
  if (length(events) && identical(events[[1L]]$type, "review_completed") &&
      is.null(events[[1L]]$review_key)) {
    signal_immediate_coordinator_event("review_refresh", component_id, revision_id,
      reason = "saved review predates request identity; refreshing")
  }

  # The reviewer's tools answer from the project, not from an empty context:
  # query_project_graph needs the lineage, and read_skill/search_skills keep
  # their catalog fallback either way.
  tools <- build_tools(spec, list(
    agent_role = "reviewer",
    component_id = component_id, selected_revisions = context$selected_revisions,
    project = context$project,
    unit_stmts = comp_stmts,
    schemas = infer_schemas(context$project),
    config = context$config %||% list()
  ))

  agent_res <- run_agent(
    spec = spec,
    llm = llm,
    tools = tools,
    user_content = if (length(context$focus_outputs)) paste(
      if (identical(review_scope, "full")) "Full component review with additional focus on source outputs:" else
        "Focused source review. Investigate source outputs:",
      paste(context$focus_outputs, collapse = ", "),
      "Trace their row selection, joins, derivations and required effects through producer and consumer SAS and R code.",
      if (identical(review_scope, "full")) "Complete the entire component review, including behavior outside this focus. A focused-only check cannot resolve an unavailable full review.",
      "Identify a concrete conflicting source/R operation, or report no established defect.",
      "The comparison may be inconsistent with the SAS. Do not infer desired values or change source rules."
    ) else "Review this SAS component and assembled R program.",
    log_dir = if (!is.null(paths)) paths$logs else tempdir(),
    prompt_vars = prompt_vars,
    audit_context = audit_context,
    usage_budget = usage_budget
  )

  verdict <- NULL
  static_runnability <- "unknown"
  unresolved_deps <- character()
  findings <- list()
  reason <- NULL

  if (identical(agent_res$status, "ok") && !is.null(agent_res$data)) {
    data <- agent_res$data
    verdict <- data$verdict %||% "reviewed_no_material_finding"
    static_runnability <- data$static_runnability %||% "looks_runnable"
    unresolved_deps <- unique(as.character(unlist(data$unresolved_dependencies %||% character())))
    findings <- classify_review_findings(data$findings %||% list(), guidance, r_code, contract,
      available = names(context$selected_revisions %||% list()))
    if (identical(verdict, "review_unavailable")) {
      reason <- if (length(unresolved_deps)) {
        paste("Unresolved review dependencies:", paste(unresolved_deps, collapse = ", "))
      } else if (length(findings)) {
        paste(vapply(findings, function(f) f$sas_evidence %||% "", character(1)), collapse = "; ")
      } else "Reviewer could not complete the semantic review."
    }
  } else {
    # Convert exhausted failure into coordinator-authored review_unavailable
    verdict <- "review_unavailable"
    static_runnability <- "unknown"
    err_msg <- paste(as.character(if (is.list(agent_res$error)) agent_res$error$message else (agent_res$error %||% "")), collapse = "; ")
    reason <- if (nzchar(err_msg)) err_msg else as.character(agent_res$status %||% "review_unavailable")[1L]
  }

  review_id <- paste0("revw_", substr(migration_hash(list(component_id, revision_id, verdict, Sys.time(), tempfile("review-"))), 1L, 16L))

  if (!is.null(history_obj)) {
    if (identical(verdict, "review_unavailable")) {
      history_obj <- record_review_unavailable(history_obj, reason = reason)
    } else {
      history_obj <- record_completed_review(
        history_obj,
        verdict = verdict,
        basis_id = review_id,
        findings = findings
      )
    }
  }

  if (!is.null(history_obj)) {
    idx <- match(history_obj$active_revision_id, vapply(history_obj$revisions, `[[`, "", "revision_id"))
    last <- length(history_obj$revisions[[idx]]$events)
    history_obj$revisions[[idx]]$events[[last]]$unresolved_dependencies <- unresolved_deps
    history_obj$revisions[[idx]]$events[[last]]$review_scope <- review_scope
    history_obj$revisions[[idx]]$events[[last]]$context_identity <- guidance$identity
  }
  review_record <- structure(
    list(
      review_id = review_id,
      component_id = component_id,
      revision_id = revision_id,
      binding_hash = binding_hash,
      binding = binding,
      verdict = verdict,
      static_runnability = static_runnability,
      unresolved_dependencies = unresolved_deps,
      findings = findings,
      context_identity = guidance$identity,
      review_scope = review_scope,
      phase = phase,
      review_key = review_key,
      status = if (identical(verdict, "review_unavailable")) "review_unavailable" else "ok",
      reason = if (identical(verdict, "review_unavailable")) reason else NULL,
      spend_usd = agent_res$spend_usd %||% 0,
      history = history_obj,
      created_at = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    class = c("sas2r_program_review", "list")
  )

  if (!is.null(history_obj) && !identical(verdict, "review_unavailable")) {
    saved <- review_record
    saved$history <- NULL
    history_obj$revisions[[idx]]$events[[last]]$review_key <- review_key
    history_obj$revisions[[idx]]$events[[last]]$review_record <- saved
    review_record$history <- history_obj
  }

  if (!is.null(paths)) {
    rev_dir <- file.path(paths$component_revisions %||% paths$root, component_id, "reviews")
    dir.create(rev_dir, recursive = TRUE, showWarnings = FALSE)
    atomic_write_json(review_record, file.path(rev_dir, paste0(review_id, ".json")))
  }

  review_record
}
