# Terminal advice. This path never runs generated examples or changes run status.
migration_package_bug_diagnosis <- function(state, llm = state$reviewer_llm, failure = NULL, stage = NULL) {
  if (!isTRUE(state$config$migration$bug_reports)) return(NULL)
  result <- list(schema_version = 2L, status = "unavailable", findings = list())

  tryCatch({
    withheld <- new.env(parent = emptyenv())
    budget <- state$usage_budget
    # Attribution reads the compared datasets again on this machine. That is
    # worth doing only when a request can follow.
    can_ask <- !is.null(llm) && !is.null(budget) && usage_budget_allows_future(budget)
    evidence <- package_bug_evidence(state, failure, stage, omitted = withheld, attribute = can_ask)
    if (length(withheld$items)) result$discrepancy_summary_omitted <- withheld$items
    if (!length(evidence)) {
      result$status <- "not_needed"
      result$reason <- "No observed run failures were eligible for AI bug triage."
      return(result)
    }
    recognized <- Filter(function(item) isTRUE(item$recognized), evidence)
    if (length(recognized)) {
      result$observed_failures <- unname(vapply(recognized, function(item) item$observed, ""))
    }

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
    ids <- package_bug_program_ids(state, unique(c(ids, unlist(lapply(ids, function(cid)
      context_component_dependencies(state$graph, cid)), use.names = FALSE))))
    calls <- unique(unlist(lapply(state$selected_revisions[ids], function(revision) {
      r_call_names(revision_code(revision))
    }), use.names = FALSE))
    # A helper that handled a differing column is documented before one that
    # is merely called somewhere.
    helpers <- utils::head(intersect(unique(c(
      unlist(lapply(facts, function(fact) lapply(fact$columns, `[[`, "helpers"))),
      unlist(lapply(facts, function(fact) fact$runtime_helpers)), calls)), SAS2R_HELPER_NAMES), 4L)
    docs <- if (length(helpers)) helper_documentation() else list()
    contracts <- lapply(docs[intersect(helpers, names(docs))], function(doc) substr(doc$text, 1L, 1800L))
    targets <- unlist(lapply(names(facts), function(id)
      if (is_scalar_character(facts[[id]]$target)) stats::setNames(id, facts[[id]]$target)))
    run_facts <- package_bug_run_facts(state, targets %||% character(), ids, withheld)
    if (length(withheld$items)) result$discrepancy_summary_omitted <- withheld$items
    packet <- package_bug_request_context(state, c(
      if (length(run_facts$shared_patterns)) run_facts["shared_patterns"],
      list(evidence = facts), run_facts[intersect(c("programs", "runtime_helpers"), names(run_facts))],
      list(execution_order = state$graph$execution_order %||% state$schedule$component_id)),
      contracts, ids)
    redact <- llm_audit_redactor(llm)
    result$provider <- llm$provider
    result$context_truncated <- packet$truncated
    if (packet$truncated) result$context_truncated_sections <- packet$truncated_sections
    cli::cli_inform("AI bug triage may send bounded SAS/R code and structured run evidence to {llm$provider}, subject to the run budget. Use migration.bug_reports: false to disable it.")
    advice <- request_report_advice(llm, budget, packet, "package-bug-triage",
      "package_bug_triage_v2", "package_bug_triage", schema_version = "2")
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

# The request text: facts first, then the documentation of the runtime helpers
# involved, then SAS and R code. Facts are bounded where they are built. Code
# keeps a reserved share of the limit, so facts and documentation cannot crowd
# it out, and every section that was cut is named.
PACKAGE_BUG_CONTEXT_LIMIT <- 24000L
PACKAGE_BUG_CODE_RESERVE <- 8000L

package_bug_request_context <- function(state, facts, helper_contracts, ids,
                                        limit = PACKAGE_BUG_CONTEXT_LIMIT) {
  blocks <- vapply(ids, function(cid) paste0("\nComponent: ", cid, "\nSAS:\n",
    component_source_text(state$graph, cid), "\nSelected R:\n",
    state$selected_revisions[[cid]]$r_code %||% "Unavailable"), "")
  if (length(blocks)) blocks[[1L]] <- paste0("\nCode of the programs involved:", blocks[[1L]])
  reserve <- min(PACKAGE_BUG_CODE_RESERVE, sum(nchar(blocks)), limit)
  cut <- character()
  text <- paste0("Facts:\n", jsonlite::toJSON(facts, auto_unbox = TRUE, null = "null"))
  if (nchar(text) > limit - reserve) {
    text <- substr(text, 1L, limit - reserve)
    cut <- c(cut, "facts")
  }
  if (length(helper_contracts)) {
    documentation <- paste0("\nRuntime helper documentation (behavior sas2r owns):",
      paste0("\n", names(helper_contracts), ": ", unlist(helper_contracts), collapse = ""))
    room <- max(0L, limit - reserve - nchar(text))
    if (nchar(documentation) > room) cut <- c(cut, "helper_documentation")
    text <- paste0(text, substr(documentation, 1L, room))
  }
  for (block in blocks) {
    remaining <- max(0L, limit - nchar(text))
    text <- paste0(text, substr(block, 1L, remaining))
    if (nchar(block) > remaining) { cut <- c(cut, "code"); break }
  }
  list(text = paste0(text, if (length(cut)) paste0("\n[Context truncated (", paste(cut, collapse = ", "),
    "); omitted content is unknown.]")), truncated = length(cut) > 0L, truncated_sections = cut)
}

# `attribute` adds, for each failed dataset target, the attribution facts and
# the package's own decisions. They need the compared datasets read again.
package_bug_evidence <- function(state, failure, stage, omitted = NULL, attribute = FALSE) {
  items <- list()
  revisions <- state$selected_revisions
  condition_item <- function(scope, condition, cid = NULL, binding = NULL, code_ids = cid) {
    facts <- agent_error_facts(condition, revision_code(revisions[[code_ids %||% ""]]))
    # Package condition classes and code-confirmed helper names provide hints,
    # while the original message, paths and call arguments stay local.
    known_classes <- union(facts$condition_class, intersect(condition$class,
      c("sas2r_pipeline_coverage_error", "sas2r_mechanical_check_failure")))
    helpers <- intersect(condition$call_names, SAS2R_HELPER_NAMES)
    projected <- list(scope = scope, component = cid, kind = facts$kind,
      condition_classes = known_classes, runtime_helpers = helpers,
      identifiers = facts$identifiers)
    symptom <- paste0(gsub("_", " ", facts$kind, fixed = TRUE),
      if (length(known_classes)) paste0(" (", paste(known_classes, collapse = ", "), ")"))
    if (!is.null(facts$called_function)) {
      projected$called_function <- facts$called_function
      projected$condition_message <- facts$condition_message
    }
    # A recognized failure is recorded with its program and its fixed label,
    # which carries only code-confirmed names. The request is unchanged: it
    # already has the kind and the names.
    recognized <- !is.null(facts$called_function) ||
      !facts$kind %in% c("none", "unclassified_error")
    if (recognized) symptom <- paste0(if (!is.null(cid)) paste0(cid, " - "), facts$condition_message)
    list(facts = projected, observed = paste0(scope, " - ", symptom),
      recognized = recognized, binding = binding, code_ids = code_ids)
  }
  if (!is.null(failure)) {
    terminal <- condition_item(stage, list(class = class(failure), message = conditionMessage(failure)))
    items <- list(terminal)
  }

  selected <- state$selected_attempt
  attempt <- state$attempt
  if (!is.null(attempt$condition) && identical(attempt$kind, "bundle") &&
      isTRUE(attempt$completed) && !isTRUE(attempt$deferred)) {
    matches_attempt <- function(id) {
      recorded <- attempt$revision_manifest[[id]]
      revision <- revisions[[id]]
      !is.null(recorded$r_hash) && identical(recorded$revision_id, revision$revision_id) &&
        identical(recorded$r_hash, migration_hash(revision_code(revision)))
    }
    cid <- attempt$condition$component_id
    if (!is_scalar_character(cid)) cid <- NULL
    recorded <- attempt$revision_manifest[[cid %||% ""]]
    code_ids <- if (!is.null(cid) && matches_attempt(cid)) cid
    item <- condition_item("Bundle execution failed", attempt$condition, cid,
      list(attempt_id = attempt$attempt_id, revision_id = recorded$revision_id,
        helper_hash = attempt$helper_hash), code_ids)
    if (!is.null(item$facts$called_function)) {
      macro_id <- paste0("macro__", tolower(item$facts$called_function))
      if (matches_attempt(macro_id) && macro_id %in% context_component_dependencies(state$graph, cid)) {
        # Read the recorded caller and callee before unrelated output context
        # consumes the request's code budget.
        item$code_ids <- c(code_ids, macro_id)
      }
    }
    items <- c(items, list(item))
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
  # The more targets share the request, the fewer columns each may describe.
  described <- sum(vapply(targets, function(target)
    identical(target$kind, "dataset") && identical(target$status, "failed"), logical(1)))
  column_limit <- max(2L, min(8L, 12L %/% max(1L, described)))
  for (target in targets) {
    key <- target$target_key
    dataset <- identical(target$kind, "dataset")
    ids <- state$assessment$lineage_by_target[[key]]$upstream_components %||%
      graph_output_writers(state$graph, key)
    summary <- if (dataset) reviewer_discrepancy_summary(target, key, omitted = omitted)
    attribution <- if (dataset && isTRUE(attribute) && identical(target$status, "failed")) {
      package_bug_target_facts(state, target, key, ids, summary, column_limit, omitted)
    }
    if (dataset && !is.null(summary)) {
      # Each described column carries its own types, extent and patterns.
      if (length(attribution$columns)) summary$variables <- summary$type_differences <- NULL
      summary$variables <- utils::head(summary$variables, 5L)
      summary$type_differences <- utils::head(summary$type_differences, 5L)
    }
    failed_checks <- names(Filter(function(check) isFALSE(check$passed), target$checks))
    items <- c(items, list(list(facts = c(list(scope = "Output assessment", target = key,
      kind = target$kind, status = target$status, discrepancy = summary,
      failed_checks = failed_checks), attribution),
      observed = paste0(target$kind %||% "Dataset", " output assessment - ",
        gsub("_", " ", target$status, fixed = TRUE),
        if (length(failed_checks)) paste0(" (", paste(failed_checks, collapse = ", "), ")")),
      binding = list(attempt_id = selected$attempt_id, helper_hash = selected_record$helper_hash),
      code_ids = ids)))
  }
  stats::setNames(items, paste0(rep("E", length(items)), seq_along(items)))
}

# Attribution and decisions for one target, held to the closed format. A
# failure to build them never stops triage: the target keeps its other facts.
package_bug_target_facts <- function(state, target, key, ids, summary, limit, omitted = NULL) {
  build <- function() {
    attribution <- package_bug_attribution(state, target, key, ids, summary, limit)
    described <- c(vapply(attribution$columns, `[[`, "", "name"), attribution$other_differing_columns)
    c(attribution, list(decisions = package_bug_decisions(state, target, key, described)))
  }
  strict <- isTRUE(getOption("sas2r.strict_discrepancy_summary", FALSE))
  facts <- if (strict) build() else tryCatch(build(), error = function(e) NULL)
  if (is.null(facts)) {
    return(withhold_package_bug_facts(key, "the facts could not be built", omitted))
  }
  checked_package_bug_facts(facts, key, c(target$candidate_path, target$reference_path), omitted)
}

# Facts about the run as a whole: difference classes that repeat across
# outputs, who wrote each program in the request, and whose runtime helpers
# the bundle ran with. Held to the closed format like the per-target facts.
package_bug_run_facts <- function(state, evidence_ids, ids, omitted = NULL) {
  build <- function() list(
    shared_patterns = package_bug_shared_patterns(state, evidence_ids),
    programs = package_bug_program_facts(state, ids),
    runtime_helpers = runtime_helper_origin(state))
  strict <- isTRUE(getOption("sas2r.strict_discrepancy_summary", FALSE))
  facts <- if (strict) build() else tryCatch(build(), error = function(e) NULL)
  if (is.null(facts)) {
    return(withhold_package_bug_facts("the run", "the facts could not be built", omitted))
  }
  paths <- unlist(lapply(state$assessment$targets, function(target)
    c(target$candidate_path, target$reference_path)), use.names = FALSE)
  checked_package_bug_facts(facts, "the run", paths, omitted)
}

package_bug_step_label <- function(step) {
  if (!is_scalar_character(step) || !step %in% PACKAGE_BUG_STEPS) return(NULL)
  gsub("_", " ", step, fixed = TRUE)
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
  if (!length(diagnosis$findings)) return(c("## AI bug triage (beta)", "", diagnosis$reason, "",
    if (length(diagnosis$observed_failures)) c(paste("- Recorded symptom:", diagnosis$observed_failures), "")))
  c("## Possible sas2r bugs (AI beta)", "",
    unlist(lapply(diagnosis$findings, function(finding) c(
      paste("-", package_bug_finding_summary(finding)),
      if (!is.null(package_bug_step_label(finding$responsible_step)))
        paste("  - Package step:", package_bug_step_label(finding$responsible_step)),
      paste("  - Recorded symptom:", finding$observed),
      paste("  - Expected behavior:", finding$expected),
      paste("  - Uncertainty:", finding$uncertainty),
      paste("  - Next step:", finding$next_step))), use.names = FALSE), "",
    "Create an issue draft with `sas2r::sas_bug_report()` using this run's `report.json`.", "")
}
