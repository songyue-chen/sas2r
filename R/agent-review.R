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

#' Build skill routing context for an agent
#'
#' @param agent_name Character agent name (e.g., "reviewer").
#' @param project A `sas2r_project` object.
#' @param unit_id Integer unit ID.
#' @param comparison_reasons Optional character vector of comparison reasons.
#' @return Named list routing context.
#' @noRd
build_skill_context <- function(agent_name, project, unit_id, comparison_reasons = character()) {
  unit <- project$statements[project$statements$unit_id == unit_id, ]
  u_type <- if (length(unit$unit_type) > 0L) unit$unit_type[1] else "data_step"
  procs <- unit_proc_names(unit)
  is_macro <- length(unit$unit_type) > 0L && unit$unit_type[1] == "macro_def"
  flags <- if ("flags" %in% names(unit)) {
    unlist(strsplit(unit$flags %||% "", "[, ]+"))
  } else character(0)
  flags <- flags[nzchar(flags)]
  if (is.list(comparison_reasons)) comparison_reasons <- unlist(comparison_reasons)
  comparison_reasons <- as.character(comparison_reasons)
  comparison_reasons <- comparison_reasons[!is.na(comparison_reasons) & nzchar(comparison_reasons)]

  list(
    agent = agent_name,
    unit_type = u_type,
    procs = procs,
    flags = flags,
    macros = if (is_macro) "macro_def" else character(0),
    semantic_rules = if (length(procs)) paste0("procs.", procs) else character(0),
    comparison_reasons = comparison_reasons,
    functions = character(0)
  )
}

#' Build execution context for an agent
#'
#' @param project A `sas2r_project` object.
#' @param unit_stmts Data frame of unit statements.
#' @param transpilation A `sas2r_transpilation` object.
#' @param skill_catalog Optional skill catalog list.
#' @param config Project configuration list.
#' @return Named list execution context.
#' @noRd
build_agent_context <- function(project, unit_stmts, transpilation,
                                skill_catalog = agent_skill_catalog(),
                                config = list()) {
  list(
    project = project,
    unit_stmts = unit_stmts,
    schemas = infer_schemas(project),
    transpilation = transpilation,
    config = config,
    skill_catalog = skill_catalog
  )
}

#' Execute reviewer agent loop for a translation unit
#'
#' @param spec Loaded reviewer agent spec.
#' @param llm `sas2r_llm` instance.
#' @param ctx Execution context.
#' @param unit Statements data frame for the unit.
#' @param staged_code Character string of staged R code for the unit.
#' @param routed List of routed skills.
#' @param usage_budget Shared usage budget instance.
#' @param log_dir Audit log directory.
#' @return List with status, issue_codes, explanation, recommended_action, confidence, unit_id, and provenance.
#' @noRd
run_review_agent <- function(spec, llm, ctx, unit, staged_code,
                             routed, usage_budget = NULL, log_dir = ".sas2r") {
  if (is.null(usage_budget)) usage_budget <- new_usage_budget()
  unit_id <- unit$unit_id[1]
  staged_txt <- if (!is.null(staged_code) && length(staged_code) > 0L && !is.na(staged_code[1])) {
    staged_code[1]
  } else ""
  rendered_skills <- render_agent_skills(routed)
  skill_provenance <- unname(lapply(routed, function(x) x[c(
    "skill_id", "version", "content_hash", "activation_reason"
  )]))
  ctxp <- build_context_packet(unit_id, ctx$project, ctx$config %||% list())
  prompt_vars <- list(
    unit = format_sas_statements(unit$text),
    staged_r = staged_txt,
    context = ctxp$packet,
    comments = ctxp$comments,
    skills = rendered_skills
  )
  tools <- build_tools(spec, ctx)
  r <- run_agent(
    spec, llm, tools,
    user_content = "Review this translation unit.",
    log_dir = log_dir,
    prompt_vars = prompt_vars,
    usage_budget = usage_budget,
    audit_context = list(
      purpose = "reviewer",
      unit_id = unit_id,
      skill_provenance = skill_provenance
    )
  )

  if (identical(r$status, "ok")) {
    if (!is.null(r$data$verdict)) {
      status_val <- switch(r$data$verdict,
        reviewed_no_material_finding = "no_issue",
        repair_required = "issue_found",
        review_unavailable = "uncertain",
        "uncertain"
      )
      issue_codes <- character(0)
      explanation_val <- r$data$explanation %||% ""
      conf_val <- 0.9
      if (length(r$data$findings) > 0L) {
        issue_codes <- unique(vapply(r$data$findings, function(f) f$r_evidence %||% f$severity %||% "material", character(1)))
        if (!nzchar(explanation_val)) {
          explanation_val <- paste(vapply(r$data$findings, function(f) f$sas_evidence %||% "", character(1)), collapse = "; ")
        }
        conf_val <- max(vapply(r$data$findings, function(f) f$confidence %||% 0.9, numeric(1)))
      }
      action_val <- switch(r$data$verdict,
        reviewed_no_material_finding = "keep",
        repair_required = if (conf_val < 0.5) "human_review" else "repair",
        review_unavailable = "human_review",
        "human_review"
      )
      return(list(
        status = status_val,
        issue_codes = issue_codes,
        explanation = explanation_val,
        recommended_action = action_val,
        confidence = conf_val,
        unit_id = unit_id,
        skill_provenance = skill_provenance,
        spend_usd = r$spend_usd %||% 0,
        known_cost_usd = r$known_cost_usd %||% 0,
        estimated_cost_usd = r$estimated_cost_usd %||% 0,
        cost_unknown = isTRUE(r$cost_unknown),
        tool_calls = r$tool_calls %||% 0L
      ))
    }
    raw_codes <- r$data$issue_codes %||% character(0)
    if (is.list(raw_codes)) raw_codes <- unlist(raw_codes)
    codes <- as.character(raw_codes)
    return(list(
      status = r$data$status,
      issue_codes = codes,
      explanation = r$data$explanation %||% "",
      recommended_action = r$data$recommended_action,
      confidence = r$data$confidence,
      unit_id = unit_id,
      skill_provenance = skill_provenance,
      spend_usd = r$spend_usd %||% 0,
      known_cost_usd = r$known_cost_usd %||% 0,
      estimated_cost_usd = r$estimated_cost_usd %||% 0,
      cost_unknown = isTRUE(r$cost_unknown),
      tool_calls = r$tool_calls %||% 0L
    ))
  }

  explanation <- switch(r$status %||% "",
    refused = "Reviewer model refused request.",
    incomplete = paste0("Reviewer model response incomplete (", r$finish_reason %||% "unknown", ")."),
    invalid_output = paste0("Reviewer output failed schema validation: ", paste(r$errors %||% character(), collapse = "; ")),
    budget_exhausted = "Review halted: usage budget exhausted.",
    agent_tool_limit_reached =
      "Review halted: the reviewer used its tool_call_limit without answering.",
    agent_budget_exhausted = "Review halted: agent tool budget exhausted.",
    paste0("Review halted due to runner status: ", r$status)
  )
  list(
    status = "uncertain",
    issue_codes = "other_source_grounded",
    explanation = explanation,
    recommended_action = "human_review",
    confidence = 0,
    unit_id = unit_id,
    skill_provenance = skill_provenance,
    spend_usd = r$spend_usd %||% 0,
    known_cost_usd = r$known_cost_usd %||% 0,
    estimated_cost_usd = r$estimated_cost_usd %||% 0,
    cost_unknown = isTRUE(r$cost_unknown),
    tool_calls = r$tool_calls %||% 0L
  )
}

#' Review a single translation unit using the structured reviewer agent
#'
#' @param unit_id Integer unit ID.
#' @param project A `sas2r_project` object.
#' @param transpilation A `sas2r_transpilation` object.
#' @param specs Loaded agent specs.
#' @param llm `sas2r_llm` instance.
#' @param usage_budget Shared usage budget instance.
#' @param log_dir Audit log directory.
#' @param comparison_reasons Optional character vector of comparison reasons.
#' @return List with status, issue_codes, explanation, recommended_action, confidence, unit_id, and provenance.
#' @noRd
review_translation_unit <- function(unit_id, project, transpilation, specs, llm,
                                    usage_budget = NULL, log_dir = ".sas2r",
                                    comparison_reasons = character()) {
  spec <- specs$reviewer
  unit <- project$statements[project$statements$unit_id == unit_id, ]
  manifest <- transpilation$manifest[
    transpilation$manifest$unit_id == unit_id, , drop = FALSE]
  if (nrow(unit) == 0L || nrow(manifest) != 1L) {
    return(list(
      status = "uncertain",
      issue_codes = "unit_context_missing",
      explanation = "The unit could not be resolved uniquely.",
      recommended_action = "human_review",
      confidence = 0,
      unit_id = unit_id,
      skill_provenance = list()
    ))
  }
  routed <- route_agent_skills(build_skill_context(
    "reviewer", project, unit_id, comparison_reasons))
  ctx <- build_agent_context(project, unit, transpilation,
                             skill_catalog = agent_skill_catalog())
  run_review_agent(spec, llm, ctx, unit, manifest$code,
                   routed, usage_budget, log_dir)
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

  context_packet <- paste(c(
    "Component:", component_id,
    "Resolved interfaces:", if_txt,
    "Helper guarantees:", helpers_txt,
    "Unresolved graph facts:", unres_txt,
    "Output lineage:", lineage_txt
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

  specs <- load_agent_specs(project_dir = project_dir)
  spec <- as.list(specs$reviewer)
  spec$output_schema <- "program_review_v1"

  usage_budget <- usage %||% new_usage_budget()

  audit_context <- list(
    role = "reviewer",
    component_id = component_id,
    revision_id = revision_id,
    round = round,
    attempt_id = attempt_id,
    purpose = "program_review"
  )

  prompt_vars <- list(
    unit = sas_text,
    comments = comments_text,
    staged_r = r_code,
    context = context_packet,
    skills = rendered_skills
  )

  # The reviewer's tools answer from the project, not from an empty context:
  # query_project_graph needs the lineage, and read_skill/search_skills keep
  # their catalog fallback either way.
  tools <- build_tools(spec, list(
    project = context$project,
    unit_stmts = comp_stmts,
    schemas = tryCatch(infer_schemas(context$project), error = function(e) list()),
    config = context$config %||% list()
  ))

  agent_res <- run_agent(
    spec = spec,
    llm = llm,
    tools = tools,
    user_content = "Review this SAS component and assembled R program.",
    log_dir = if (!is.null(paths)) paths$state else ".sas2r",
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
    findings <- data$findings %||% list()
  } else {
    # Convert exhausted failure into coordinator-authored review_unavailable
    verdict <- "review_unavailable"
    static_runnability <- "unknown"
    err_msg <- paste(as.character(if (is.list(agent_res$error)) agent_res$error$message else (agent_res$error %||% "")), collapse = "; ")
    reason <- if (nzchar(err_msg)) err_msg else as.character(agent_res$status %||% "review_unavailable")[1L]
  }

  review_id <- paste0("revw_", substr(migration_hash(list(component_id, revision_id, verdict, Sys.time(), stats::runif(1))), 1L, 16L))

  history_obj <- history %||% context$history %||% revision$history %||% NULL
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
      status = if (identical(verdict, "review_unavailable")) "review_unavailable" else "ok",
      reason = if (identical(verdict, "review_unavailable")) reason else NULL,
      spend_usd = agent_res$spend_usd %||% 0,
      history = history_obj,
      created_at = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    class = c("sas2r_program_review", "list")
  )

  if (!is.null(paths)) {
    rev_dir <- file.path(paths$programs %||% paths$root, component_id, "reviews")
    dir.create(rev_dir, recursive = TRUE, showWarnings = FALSE)
    atomic_write_json(review_record, file.path(rev_dir, paste0(review_id, ".json")))
  }

  review_record
}

