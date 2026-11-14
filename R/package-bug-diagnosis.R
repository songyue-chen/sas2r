# Terminal advice. This path never runs generated examples or changes run status.
migration_package_bug_diagnosis <- function(state, llm = state$reviewer_llm, failure = NULL, stage = NULL) {
  if (!isTRUE(state$config$migration$bug_reports)) return(NULL)
  result <- list(schema_version = 2L, status = "unavailable", findings = list())

  tryCatch({
    evidence <- package_bug_evidence(state, failure, stage)
    if (!length(evidence)) {
      result$status <- "not_needed"
      result$reason <- "No observed run failures were eligible for AI bug triage."
      return(result)
    }

    budget <- state$usage_budget
    if (is.null(llm)) {
      result$status <- "not_configured"
      result$reason <- "AI bug triage requires a configured reviewer."
      return(result)
    }
    if (is.null(budget) || !usage_budget_allows_future(budget)) {
      result$status <- "budget_unavailable"
      result$reason <- "No request budget remains for AI bug triage."
      return(result)
    }

    # Send only the fixed evidence projection and bounded code. Local binding
    # metadata is retained for the draft and does not need to go to the model.
    facts <- lapply(evidence, function(item) item$facts)
    ids <- unique(unlist(lapply(evidence, function(item) item$code_ids), use.names = FALSE))
    calls <- unique(unlist(lapply(state$selected_revisions[ids], function(revision) {
      r_call_names(revision_code(revision))
    }), use.names = FALSE))
    helpers <- utils::head(intersect(c(calls,
      unlist(lapply(facts, function(fact) fact$runtime_helpers))), SAS2R_HELPER_NAMES), 4L)
    docs <- if (length(helpers)) helper_documentation() else list()
    contracts <- lapply(docs[intersect(helpers, names(docs))], function(doc) substr(doc$text, 1L, 1800L))
    packet <- report_advice_context(state, list(evidence = facts, helper_contracts = contracts,
      execution_order = state$graph$execution_order %||% state$schedule$component_id), ids)
    redact <- llm_audit_redactor(llm)
    result$provider <- llm$provider
    result$context_truncated <- packet$truncated
    cli::cli_inform("AI bug triage may send bounded SAS/R code and structured run evidence to {llm$provider}, subject to the run budget. Use migration.bug_reports: false to disable it.")
    advice <- request_report_advice(llm, budget, packet, "package-bug-triage",
      "package_bug_triage_v1", "package_bug_triage")
    response <- advice$response
    result$model <- redact(response$resolved_model %||% llm$model)
    result$response_status <- response$status
    result$finish_reason <- redact(response$finish_reason)
    if (!advice$valid) {
      result$status <- if (identical(response$status, "budget_exhausted")) "budget_unavailable" else "unavailable"
      result$validation_errors <- advice$validation_errors
      result$reason <- if (length(advice$validation_errors)) {
        paste0("AI bug triage rejected the completed response: ",
          paste(utils::head(advice$validation_errors, 3L), collapse = "; "))
      } else if (identical(result$status, "budget_unavailable")) {
        "The run budget did not admit the AI bug triage request."
      } else {
        paste0("AI bug triage did not complete (", response$status, ").")
      }
      return(result)
    }

    seen <- character()
    for (finding in redact(response$data$findings)) {
      id <- finding$evidence_id
      item <- evidence[[id]]
      if (is.null(item) || id %in% seen) next
      seen <- c(seen, id)
      code <- trimws(finding$repro_r)
      parsed <- if (nzchar(code)) tryCatch(parse(text = code), error = function(e) NULL)
      finding$repro_r <- if (length(parsed)) code else NULL
      finding$reproduction <- if (length(parsed)) "not_run" else "no_example"
      finding$observed <- item$observed
      finding$binding <- item$binding
      result$findings[[length(result$findings) + 1L]] <- finding
    }
    result$status <- "completed"
    result$reason <- if (length(result$findings)) "AI identified possible sas2r bugs for review." else
      "AI triage did not identify a grounded package-bug suspicion in the supplied evidence."
    if (length(result$findings)) {
      description <- file.path(getNamespaceInfo(asNamespace("sas2r"), "path"), "DESCRIPTION")
      revision <- if (file.exists(description)) read.dcf(description, fields = "RemoteSha")[1L, 1L] else NA_character_
      result$environment <- list(
        sas2r = state$environment$versions$sas2r %||% as.character(getNamespaceVersion("sas2r")),
        R = state$environment$versions$R %||% as.character(getRversion()),
        platform = R.version$platform,
        revision = if (!is.na(revision)) revision,
        workers = state$environment$parallel$effective %||% state$parallel$effective)
    }
    result
  }, error = function(error) {
    result$status <- "unavailable"
    result$findings <- list()
    result$reason <- paste("AI bug triage unavailable:", redact_secrets(conditionMessage(error)))
    result
  })
}

package_bug_evidence <- function(state, failure, stage) {
  items <- list()
  revisions <- state$selected_revisions
  condition_item <- function(scope, condition, cid = NULL, binding = NULL, code_ids = cid) {
    facts <- agent_error_facts(condition, revision_code(revisions[[code_ids %||% ""]]))
    # Package condition classes and code-confirmed helper names provide hints,
    # while the original message, paths and call arguments stay local.
    known_classes <- union(facts$condition_class, intersect(condition$class,
      c("sas2r_pipeline_coverage_error", "sas2r_mechanical_check_failure")))
    helpers <- intersect(condition$call_names, SAS2R_HELPER_NAMES)
    list(facts = list(scope = scope, component = cid, kind = facts$kind,
      condition_classes = known_classes, runtime_helpers = helpers,
      identifiers = facts$identifiers),
      observed = paste0(scope, " - ", gsub("_", " ", facts$kind, fixed = TRUE),
        if (length(known_classes)) paste0(" (", paste(known_classes, collapse = ", "), ")")),
      binding = binding, code_ids = code_ids)
  }
  if (!is.null(failure)) {
    terminal <- condition_item(stage, list(class = class(failure), message = conditionMessage(failure)))
    items <- list(terminal)
  }

  selected <- state$selected_attempt
  attempt <- state$attempt
  if (!is.null(attempt$condition) && identical(attempt$kind, "bundle") &&
      isTRUE(attempt$completed) && !isTRUE(attempt$deferred)) {
    cid <- attempt$condition$component_id
    if (!is_scalar_character(cid)) cid <- NULL
    recorded <- attempt$revision_manifest[[cid %||% ""]]
    code_ids <- if (!is.null(cid) && !is.null(recorded$r_hash) &&
      identical(recorded$revision_id, revisions[[cid]]$revision_id) &&
      identical(recorded$r_hash, migration_hash(revision_code(revisions[[cid]])))) cid
    items <- c(items, list(condition_item("Bundle execution failed", attempt$condition, cid,
      list(attempt_id = attempt$attempt_id, revision_id = recorded$revision_id,
        helper_hash = attempt$helper_hash), code_ids)))
  }

  component_failures <- state$diagnostics$component_failures %||% list()
  for (cid in utils::head(names(component_failures), 4L)) {
    item <- component_failures[[cid]]
    items <- c(items, list(condition_item("Component processing failed",
      list(message = item$reason), cid)))
  }

  # A scheduler decision can prevent any runtime error or candidate output.
  # Keep the reason and source/library evidence ahead of output discrepancies.
  dependencies <- Filter(function(item) length(item$findings) > 0L,
    state$diagnostics$dependency_findings)
  for (cid in utils::head(names(dependencies), max(0L, 6L - length(items)))) {
    item <- dependencies[[cid]]
    reported <- utils::head(item$findings, 8L)
    availability <- late_dataset_availability(state$project, cid, reported)
    items <- c(items, list(list(
      facts = list(scope = "Execution deferred by dependency reconciliation",
        component = cid, reason = item$reason, dependencies = reported,
        affected_components = utils::head(item$affected, 8L),
        member_availability = as.list(availability)),
      observed = "Execution deferred by dependency reconciliation",
      binding = list(revision_id = revisions[[cid]]$revision_id), code_ids = cid)))
  }

  if (length(items) < 6L) {
    for (cid in names(revisions)) {
      smoke <- revisions[[cid]]$smoke
      if (is.null(smoke$condition) || !isFALSE(smoke$passed) ||
          isTRUE(smoke$deferred) || !is.null(smoke$blocked_by) ||
          cid %in% attempt$executed_component_ids) next
      items <- c(items, list(condition_item("Program smoke test failed", smoke$condition, cid,
        list(revision_id = revisions[[cid]]$revision_id))))
      if (length(items) >= 6L) break
    }
  }

  targets <- Filter(function(target) target$status %in% c("failed", "unreadable", "missing_candidate"),
    state$assessment$targets %||% list())
  targets <- utils::head(targets, max(0L, 6L - length(items)))
  selected_record <- if (length(targets) && !is.null(selected$attempt_dir)) {
    if (identical(attempt$attempt_dir, selected$attempt_dir)) attempt else read_attempt_record(selected$attempt_dir)
  }
  for (target in targets) {
    key <- target$target_key
    dataset <- identical(target$kind, "dataset")
    ids <- state$assessment$lineage_by_target[[key]]$upstream_components %||%
      graph_output_writers(state$graph, key)
    summary <- if (dataset) reviewer_discrepancy_summary(target, key)
    if (dataset) {
      summary$variables <- utils::head(summary$variables, 5L)
      summary$type_differences <- utils::head(summary$type_differences, 5L)
    }
    failed_checks <- names(Filter(function(check) isFALSE(check$passed), target$checks))
    items <- c(items, list(list(facts = list(scope = "Output assessment", target = key,
      kind = target$kind, status = target$status, discrepancy = summary,
      failed_checks = failed_checks),
      observed = paste0(target$kind %||% "Dataset", " output assessment - ",
        gsub("_", " ", target$status, fixed = TRUE),
        if (length(failed_checks)) paste0(" (", paste(failed_checks, collapse = ", "), ")")),
      binding = list(attempt_id = selected$attempt_id, helper_hash = selected_record$helper_hash),
      code_ids = ids)))
  }
  stats::setNames(items, paste0(rep("E", length(items)), seq_along(items)))
}

package_bug_reproduction_label <- function(finding) {
  if (identical(finding$reproduction, "not_run")) "Reproduction unverified (draft R example not run)" else
    "Reproduction unverified (no runnable example supplied)"
}

package_bug_finding_summary <- function(finding) {
  paste0("AI suspicion [", finding$evidence_id, "]: ", finding$explanation,
    " ", package_bug_reproduction_label(finding), ".")
}

package_bug_report_lines <- function(diagnosis) {
  if (is.null(diagnosis)) return(character())
  if (!length(diagnosis$findings)) return(c("## AI bug triage (beta)", "", diagnosis$reason, ""))
  c("## Possible sas2r bugs (AI beta)", "",
    unlist(lapply(diagnosis$findings, function(finding) c(
      paste("-", package_bug_finding_summary(finding)),
      paste("  - Recorded symptom:", finding$observed),
      paste("  - Expected behavior:", finding$expected),
      paste("  - Uncertainty:", finding$uncertainty),
      paste("  - Next step:", finding$next_step))), use.names = FALSE), "",
    "Create an issue draft with `sas2r::sas_bug_report()` using this run's `report.json`.", "")
}
