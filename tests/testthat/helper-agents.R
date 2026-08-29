good_translation <- function(code) {
  valid_program_translation_response(code = code, summary = "translated unit")
}

valid_translation_response <- function(code, assumptions = list("repaired"),
                                       confidence = 0.9, flags = list()) {
  summary_txt <- if (length(assumptions)) paste(unlist(assumptions), collapse = "; ") else "translated unit"
  unc_list <- if (length(flags)) {
    lapply(flags, function(flg) {
      list(
        severity = "low",
        claim = as.character(flg),
        evidence = "model flag",
        affected_outputs = list("work.b")
      )
    })
  } else {
    list()
  }
  valid_program_translation_response(
    code = code,
    summary = summary_txt,
    uncertainty = unc_list
  )
}

good_review <- function(status = "no_issue", action = "keep",
                        codes = character(),
                        explanation = "Source and generated R agree for the reviewed behavior.",
                        confidence = 0.9) {
  verdict <- switch(status,
    no_issue = "reviewed_no_material_finding",
    issue_found = "repair_required",
    uncertain = "review_unavailable",
    status
  )
  static_runnability <- if (identical(verdict, "reviewed_no_material_finding")) "looks_runnable" else "material_issue"
  findings <- if (identical(verdict, "repair_required") || length(codes) > 0L) {
    if (length(codes) > 0L) {
      lapply(codes, function(cd) {
        list(
          severity = "material",
          sas_evidence = explanation,
          r_evidence = cd,
          affected_outputs = list("work.b"),
          confidence = confidence,
          unresolved_dependencies = list()
        )
      })
    } else {
      list(list(
        severity = "material",
        sas_evidence = explanation,
        r_evidence = "semantic difference",
        affected_outputs = list("work.b"),
        confidence = confidence,
        unresolved_dependencies = list()
      ))
    }
  } else {
    list()
  }
  res_data <- list(
    verdict = verdict,
    static_runnability = static_runnability,
    unresolved_dependencies = list(),
    findings = findings
  )
  attr(res_data, "mock_explanation") <- explanation
  attr(res_data, "mock_confidence") <- confidence
  attr(res_data, "mock_action") <- action
  list(
    status = "completed",
    action = "final",
    data = res_data
  )
}

valid_program_translation_response <- function(code = "x <- 1",
                                               summary = "translated program",
                                               parameters = list(),
                                               defaults = list(),
                                               reads = character(),
                                               writes = character(),
                                               side_effects = character(),
                                               helper_use = character(),
                                               discovered_dependencies = character(),
                                               suspected_dependencies = character(),
                                               affected_outputs = character(),
                                               uncertainty = list()) {
  list(
    status = "completed",
    action = "final",
    data = list(
      r_code = code,
      summary = summary,
      parameters = as.list(parameters),
      defaults = if (length(defaults) == 0 && is.list(defaults)) structure(list(), names = character(0)) else defaults,
      reads = as.list(reads),
      writes = as.list(writes),
      side_effects = as.list(side_effects),
      helper_use = as.list(helper_use),
      discovered_dependencies = as.list(discovered_dependencies),
      suspected_dependencies = as.list(suspected_dependencies),
      affected_outputs = as.list(affected_outputs),
      uncertainty = as.list(uncertainty)
    )
  )
}

valid_program_review_response <- function(verdict = "reviewed_no_material_finding",
                                          static_runnability = "looks_runnable",
                                          findings = list(),
                                          unresolved_dependencies = character()) {
  list(
    status = "completed",
    action = "final",
    data = list(
      verdict = verdict,
      static_runnability = static_runnability,
      findings = as.list(findings),
      unresolved_dependencies = as.list(unresolved_dependencies)
    )
  )
}

valid_program_fix_response <- function(code = "x <- 1",
                                       diagnosis = "fixed issue",
                                       summary = "applied patch",
                                       evidence_ids = character(),
                                       changed_interfaces = character(),
                                       affected_outputs = character(),
                                       remaining_uncertainty = character(),
                                       bundle_helper_patch = NULL) {
  list(
    status = "completed",
    action = "final",
    data = list(
      r_code = code,
      diagnosis = diagnosis,
      summary = summary,
      evidence_ids = as.list(evidence_ids),
      changed_interfaces = as.list(changed_interfaces),
      affected_outputs = as.list(affected_outputs),
      remaining_uncertainty = as.list(remaining_uncertainty),
      bundle_helper_patch = bundle_helper_patch
    )
  )
}

material_review_response <- function(sas_evidence = "SAS branch dropped",
                                     r_evidence = "Missing handling",
                                     affected_outputs = "work.target",
                                     unresolved_dependencies = character()) {
  valid_program_review_response(
    verdict = "repair_required",
    static_runnability = "material_issue",
    findings = list(
      list(
        severity = "material",
        sas_evidence = sas_evidence,
        r_evidence = r_evidence,
        affected_outputs = as.list(affected_outputs),
        confidence = 0.95,
        unresolved_dependencies = as.list(unresolved_dependencies)
      )
    ),
    unresolved_dependencies = unresolved_dependencies
  )
}

valid_fix_response <- function(code = "x <- 1", diagnosis = "fixed issue", evidence_ids = character()) {
  valid_program_fix_response(
    code = code,
    diagnosis = diagnosis,
    summary = "repaired program",
    evidence_ids = evidence_ids
  )
}

recording_reviewer <- function(handler, budget = new_usage_budget()) {
  captured_requests <- list()
  llm <- new_llm(function(request, audit_context = list()) {
    captured_requests[[length(captured_requests) + 1L]] <<- request
    req_context <- utils::modifyList(audit_context, list(
      request = request,
      messages = request$messages
    ))
    resp <- if (is.function(handler)) handler(req_context) else handler
    normalize_provider_response(resp, request = request, provider = "mock")
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))
  llm$requests <- function() captured_requests
  llm$budget <- budget
  llm
}

recording_fixer <- function(handler, budget = new_usage_budget()) {
  captured_requests <- list()
  llm <- new_llm(function(request, audit_context = list()) {
    captured_requests[[length(captured_requests) + 1L]] <<- request
    req_context <- utils::modifyList(audit_context, list(
      request = request,
      messages = request$messages,
      evidence_ids = audit_context$evidence_ids %||% character()
    ))
    resp <- if (is.function(handler)) handler(req_context) else handler
    normalize_provider_response(resp, request = request, provider = "mock")
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))
  llm$requests <- function() captured_requests
  llm$budget <- budget
  llm
}

review_fix_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(c(
    "data work.target; set work.source;",
    "if missing(x) then y = 0;",
    "else y = x * 2;",
    "run;"
  ), file.path(dir, "transform.sas"))
  project <- sas_project(dir)
  paths <- init_migration_paths(withr::local_tempdir(.local_envir = parent.frame()))
  baseline <- sas_transpile(project, paths$root)
  outputs <- infer_output_contracts(project)
  graph <- build_dependency_graph(project, output_contracts = outputs)
  schedule <- stable_dependency_schedule(graph)

  binding <- new_component_binding(
    source_hash = migration_hash("data work.target; set work.source; run;"),
    r_hash = migration_hash("target <- source"),
    helper_hash = migration_hash(""),
    prompt_skill_hash = migration_hash("translator"),
    dependency_closure_hash = migration_hash("closure")
  )
  history <- new_component_evidence_history("transform", binding = binding)

  contract <- new_behavioral_contract(
    component_id = "transform",
    reads = "work.source",
    writes = "work.target",
    affected_outputs = "work.target",
    binding = binding
  )

  revision <- list(
    component_id = "transform",
    revision_id = "r1",
    r_code = "target <- source |> dplyr::mutate(y = x * 2)",
    r_path = file.path(paths$programs, "transform", "revisions", "r1", "program.R"),
    contract = contract,
    binding = binding
  )
  dir.create(dirname(revision$r_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(revision$r_code, revision$r_path)

  context <- list(
    component_id = "transform",
    revision_id = "r1",
    sas_source = "data work.target; set work.source; if missing(x) then y = 0; else y = x * 2; run;",
    contract = contract,
    resolved_interfaces = list(),
    helper_guarantees = list(),
    unresolved_graph_facts = character(),
    output_lineage = "work.target",
    history = history,
    translator_reasoning = "I assumed missing values do not occur in x"
  )

  failed_smoke <- list(
    execution_id = "exec_smoke_9988",
    status = "failed",
    exit_code = 1L,
    error = "missing value in if condition",
    log = "Error: object 'x' contains NA values"
  )

  list(
    project = project,
    paths = paths,
    revision = revision,
    context = context,
    history = history,
    failed_smoke = failed_smoke
  )
}

reviewer_fixture <- function(sas_code) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(sas_code, file.path(dir, "prog.sas"))
  p <- sas_project(dir)
  out_dir <- withr::local_tempdir(.local_envir = parent.frame())
  tr <- sas_transpile(p, out_dir)
  unit_id <- p$statements$unit_id[1]
  list(unit_id = unit_id, project = p, transpilation = tr, out_dir = out_dir)
}

review_repair_fixture <- function(two_units = TRUE) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  code <- if (two_units) {
    c("data work.b; set w.root; where x > 0; x = round(x); run;",
      "data work.c; set work.b; y = 1; run;")
  } else {
    "data work.b; set w.root; where x > 0; x = round(x); run;"
  }
  writeLines(code, file.path(dir, "p.sas"))
  p <- sas_project(dir)
  out_dir <- withr::local_tempdir(.local_envir = parent.frame())
  tr <- sas_transpile(p, out_dir)
  list(path = dir, project = p, transpilation = tr, out_dir = out_dir)
}

frozen_review_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines("data work.b; set work.a; x = round(x); run;", file.path(dir, "p.sas"))
  p <- sas_project(dir)
  out_dir <- withr::local_tempdir(.local_envir = parent.frame())
  tr <- sas_transpile(p, out_dir)
  tr$manifest$flags[1] <- "frozen"
  staged_rel <- tr$manifest$staged_file[1]
  staged_file <- file.path(out_dir, staged_rel)
  list(project = p, transpilation = tr, out_dir = out_dir, staged_file = staged_file)
}

capturing_normalized_llm <- function(response, budget = new_usage_budget()) {
  captured_requests <- list()
  llm <- new_llm(function(request) {
    captured_requests[[length(captured_requests) + 1L]] <<- request
    resp <- if (is.function(response)) response(request) else response
    normalize_provider_response(resp, request = request, provider = "mock")
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))

  text <- function() {
    all_texts <- character(0)
    for (req in captured_requests) {
      if (is.list(req$messages)) {
        for (m in req$messages) {
          if (!is.null(m$content) && nzchar(m$content)) {
            all_texts <- c(all_texts, m$content)
          }
        }
      }
    }
    paste(all_texts, collapse = "\n\n")
  }

  list(
    llm = llm,
    budget = budget,
    text = text,
    requests = function() captured_requests
  )
}

sequence_normalized_llm <- function(responses, budget = new_usage_budget()) {
  captured_requests <- list()
  captured_purposes <- character(0)
  idx <- 0L
  # The audit context arrives alongside the request, never on it: the request
  # travels to the provider and comes back on the response, so nothing that
  # closes over credentials may ride on it.
  llm <- new_llm(function(request, audit_context = list()) {
    idx <<- idx + 1L
    captured_requests[[length(captured_requests) + 1L]] <<- request
    p <- audit_context$purpose %||% "unknown"
    captured_purposes <<- c(captured_purposes, p)
    if (idx > length(responses)) {
      cli::cli_abort("sequence_normalized_llm exhausted after {length(responses)} response(s)",
                     class = "sas2r_llm_error")
    }
    resp <- responses[[idx]]
    if (is.function(resp)) resp <- resp(request)
    normalize_provider_response(resp, request = request, provider = "mock")
  }, provider = "mock", capabilities = llm_capabilities(
    structured_output = "native",
    tool_calling = "native",
    tools_with_structured_output = "supported"
  ))

  llm$purposes <- function() captured_purposes
  llm$requests <- function() captured_requests
  llm$budget <- budget
  llm
}


# A double that answers by output schema rather than by call order, so a test
# can assert on end state without pinning the exact number of agent calls.
schema_routed_llm <- function(translation = valid_translation_response("x <- 1"),
                              review = good_review(),
                              annotation = NULL) {
  capturing_normalized_llm(function(request) {
    schema <- request$schema_name %||% ""
    if (identical(schema, "reviewer_output_v1") || identical(schema, "program_review_v1")) {
      review
    } else if (identical(schema, "annotation") && !is.null(annotation)) {
      annotation
    } else {
      translation
    }
  })
}
