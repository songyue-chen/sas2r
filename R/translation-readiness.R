# One offline readiness assessment feeds preflight, agent context and execution.
# Findings describe unavailable evidence; they do not prevent drafting source.
translation_readiness <- function(project, plan) {
  inputs <- preflight_inputs(project, effective_librefs(project))
  contracts <- plan$contracts
  paths <- vapply(seq_len(nrow(contracts)), function(i)
    output_reference_path(contracts[i, ], project$config$comparison_rules), "")
  keep <- which(!is.na(paths) & nzchar(paths))
  references <- tibble::tibble(target_key = contracts$target_key[keep], path = paths[keep],
    status = c("missing", "available")[1L + as.integer(file.exists(paths[keep]) & !dir.exists(paths[keep]))])
  warnings <- list()
  ids <- plan$schedule$component_id
  add <- function(kind, detail, components = ids, file = NA_character_, line = NA_integer_,
                  blocks_execution = TRUE, action = "Supply or resolve the dependency before execution.") {
    affected <- unique(unlist(lapply(components, function(cid)
      parallel_affected_components(plan$graph, cid, ids)), use.names = FALSE))
    warnings[[length(warnings) + 1L]] <<- list(kind = kind, detail = detail,
      file = file, line = line, affected = affected, blocks_execution = blocks_execution, action = action)
  }
  nodes <- plan$graph$nodes
  for (i in which(inputs$status %in% c("missing", "unresolved", "no_producer", "backward_dependency"))) {
    row <- inputs[i, ]
    components <- unique(nodes$component_id[nodes$source_file %in% row$file & nodes$component_id %in% ids])
    unavailable <- row$status %in% c("missing", "unresolved")
    add(paste0("input_", row$status), paste0(row$dataset, " (", row$status, ")"),
      components, row$file, row$line, blocks_execution = unavailable,
      action = if (unavailable) paste0("Supply the input or correct its binding/producer.",
        if (length(row$searched_paths[[1L]])) paste0(" Searched: ", paste(row$searched_paths[[1L]], collapse = ", ")))
      else "Static analysis could not establish the producer/order. Review the source; execution must assess whether the translated program supplies this input.")
  }
  for (i in seq_len(nrow(plan$schedule))) {
    cid <- ids[i]
    unresolved <- filter_dependency_resources(plan$schedule$unresolved_dependencies[[i]], project, cid)
    for (dependency in unresolved) {
      source <- nodes[nodes$type == "unresolved_dependency" & nodes$component_id == dependency, , drop = FALSE]
      if (!nrow(source)) source <- nodes[nodes$component_id == cid, , drop = FALSE]
      add("source_dependency", dependency, cid, source$source_file[1L], source$line[1L],
        action = paste0("Supply the missing source or resolve the call/include; configure macros.search_path for external macros.",
          if (length(project$config$macro_search_path)) paste0(" Macro search paths: ", paste(project$config$macro_search_path, collapse = ", ")),
          " Available code will be translated; missing logic must not be invented."))
    }
    if (plan$schedule$group_kind[i] == "cycle" &&
        !plan$schedule$group_id[i] %in% plan$schedule$group_id[seq_len(i - 1L)]) {
      members <- ids[plan$schedule$group_id == plan$schedule$group_id[i]]
      add("dependency_cycle", paste("Dependency cycle involving", paste(members, collapse = ", ")), members,
        action = "Resolve the execution order. Drafts use a stable source order within the cycle; that order is not validated for execution.")
    }
  }
  # These scanner findings cannot always be attached to a schedule edge. Keep
  # their scope explicit instead of pretending the scanner resolved the source.
  global <- c("autoexec_missing", "include_depth_exceeded", "include_cycle",
    "libref_context_truncated", "macro_dependency_analysis_deferred",
    "macro_library_initialization_unsupported", "macro_include_requires_expansion",
    "macro_nested_definition_unsupported")
  flag_components <- function(detail) {
    files <- project$files$file[startsWith(detail, paste0(project$files$file, ":"))]
    if (length(files)) unique(nodes$component_id[nodes$source_file %in% files & nodes$component_id %in% ids]) else ids
  }
  for (i in which(project$flags$kind %in% global)) add(project$flags$kind[i], project$flags$detail[i],
    flag_components(project$flags$detail[i]))
  for (i in which(project$flags$kind %in% c("dynamic_dataset_reference", "macro_data_flow_deferred",
      "dataset_statement_deferred", "libref_engine_unsupported")))
    add(project$flags$kind[i], project$flags$detail[i], flag_components(project$flags$detail[i]), blocks_execution = FALSE,
      action = "Static analysis could not establish this behavior. Translation may implement it; review and execution checks must still assess it.")
  for (i in which(references$status == "missing")) add("reference_missing",
    paste(references$target_key[i], references$path[i], sep = ": "), blocks_execution = FALSE,
    action = "Supply the configured reference before comparison. Translation and execution can continue.")
  list(inputs = inputs, references = references, warnings = warnings)
}

readiness_warning_lines <- function(readiness) {
  vapply(readiness$warnings, function(x) paste0(x$kind, ": ", x$detail,
    if (!is.na(x$file)) paste0(" at ", x$file, if (!is.na(x$line)) paste0(":", x$line)),
    if (length(x$affected)) paste0(" [", paste(x$affected, collapse = ", "), "]"),
    ". ", x$action), "")
}

component_readiness_context <- function(project, component_id) {
  warnings <- Filter(function(x) component_id %in% x$affected, project$readiness$warnings)
  findings <- Filter(function(x) component_id %in% x$affected, project$dependency_findings)
  blocking <- Filter(function(x) length(x$findings), findings)
  advisory <- unique(unlist(lapply(findings, `[[`, "advisory"), use.names = FALSE))
  if (!length(warnings) && !length(blocking) && !length(advisory)) return(character())
  c("Translation readiness findings (static findings may be resolved by faithful translation):",
    readiness_warning_lines(list(warnings = warnings)),
    vapply(names(blocking), function(cid) paste0(cid, ": ",
      paste(blocking[[cid]]$findings, collapse = ", ")), ""),
    if (length(advisory)) paste0("Reported names without a scanned producer (not blocking execution; ",
      "keep any unresolved behavior visible in uncertainty): ", paste(advisory, collapse = ", ")),
    "Translate only the available source. Do not invent missing macro/include bodies, dataset schemas, values or execution order. Preserve unresolved behavior explicitly in the code and contract uncertainty. An upstream draft is not verified behavior; review the available source and keep affected downstream assumptions visible.")
}

component_execution_reasons <- function(state, component_id = NULL) {
  warnings <- Filter(function(x) isTRUE(x$blocks_execution) &&
    (is.null(component_id) || component_id %in% x$affected), state$project$readiness$warnings)
  findings <- Filter(function(x) length(x$findings) &&
    (is.null(component_id) || component_id %in% x$affected), state$diagnostics$dependency_findings)
  failures <- Filter(function(x) is.null(component_id) || component_id %in% x$affected,
    state$diagnostics$component_failures)
  unique(c(vapply(warnings, `[[`, "", "detail"),
    vapply(names(findings), function(cid) paste0(cid, " (",
      paste(findings[[cid]]$findings, collapse = ", "), ")"), ""),
    vapply(failures, `[[`, "", "reason")))
}

record_dependency_finding <- function(state, cid, finding) {
  known <- Filter(function(x) cid %in% x$affected && x$kind == "source_dependency",
    state$project$readiness$warnings)
  # SAS macro identifiers are case-insensitive and agents may prefix them with
  # %. Preserve exact file/include spellings rather than normalizing paths.
  normalize <- function(x) {
    key <- tolower(sub("^%", "", trimws(x)))
    macro <- key %in% state$project$macros$resolution$name
    x[macro] <- key[macro]
    x
  }
  finding <- unique(trimws(as.character(finding %||% character())))
  finding <- finding[nzchar(finding)]
  finding <- finding[!normalize(finding) %in% normalize(vapply(known, `[[`, "", "detail"))]
  finding <- finding[!duplicated(normalize(finding))]
  # The recorded set is what the selected revision reports now. A revision
  # that no longer reports a name retracts it; nothing accumulates.
  blocking <- finding[dependency_finding_blocks_execution(finding)]
  prior <- state$diagnostics$dependency_findings[[cid]]
  if (!length(finding) && is.null(prior)) return(state)
  affected <- parallel_affected_components(state$graph, cid, state$schedule$component_id)
  state$diagnostics$dependency_findings[[cid]] <- if (length(finding)) list(findings = blocking,
    advisory = setdiff(finding, blocking), affected = affected, reason = "source_reconciliation_required")
  state <- sync_dependency_context(state)
  if (length(blocking) && !identical(prior$findings, blocking))
    signal_immediate_coordinator_event("dependency_warning", cid, severity = "warning",
      reason = paste(blocking, collapse = ", "), affected = affected)
  state
}

# SAS stops on a macro it cannot resolve and on a dataset it cannot open. An
# unresolved macro variable, or a bare name the scanner cannot classify, is a
# warning in SAS; execution remains the check on that translation rather than
# the agent's own report exempting its code from execution.
dependency_finding_blocks_execution <- function(x) {
  grepl("^%", x) | grepl("^[A-Za-z_][A-Za-z0-9_]*[.][A-Za-z_][A-Za-z0-9_]*$", x)
}

sync_dependency_context <- function(state) {
  findings <- state$diagnostics$dependency_findings %||% list()
  for (cid in names(state$diagnostics$component_failures)) {
    failure <- state$diagnostics$component_failures[[cid]]
    findings[[cid]] <- list(affected = union(findings[[cid]]$affected, failure$affected),
      findings = union(findings[[cid]]$findings,
        paste("Component", cid, "could not finish:", failure$reason)),
      advisory = findings[[cid]]$advisory)
  }
  state$project$dependency_findings <- findings
  state
}

critical_translation_error <- function(error) {
  # Configuration, provider access, accounting and artifact persistence failures
  # stop the run; ordinary component errors can leave other work useful.
  if (inherits(error, c("sas2r_llm_settings_error", "sas2r_llm_access_error",
      "sas2r_budget_error", "sas2r_usage_ledger_error", "sas2r_config_error",
      "sas2r_parallel_config_error", "sas2r_llm_config_error",
      "sas2r_write_failed", "sas2r_record_exists"))) return(TRUE)
  if (inherits(error$parent, "condition")) return(critical_translation_error(error$parent))
  FALSE
}

record_component_failure <- function(state, cid, error, phase = "translation", logs = NULL) {
  if (critical_translation_error(error)) {
    # Only the coordinator may attach live state to an in-memory condition.
    # Worker errors cross callr's serialization boundary without adapters.
    if (is.null(.parallel_worker$client)) error$migration_state <- state
    stop(error)
  }
  affected <- parallel_affected_components(state$graph, cid, state$schedule$component_id)
  reason <- redact_secrets(conditionMessage(error))
  state$diagnostics$component_failures[[cid]] <- list(component_id = cid, phase = phase,
    reason = reason, affected = affected, logs = logs)
  state$component_stage[[cid]] <- "failed"
  state <- sync_dependency_context(state)
  signal_immediate_coordinator_event("component_failed", cid, severity = "warning",
    reason = reason, path = logs)
  state
}

# Scheduling waits for available providers. Cycles still get drafts in the
# scanner's stable order; their execution remains deferred by readiness.
translation_providers <- function(graph, schedule, ids) {
  stats::setNames(lapply(ids, function(cid) {
    deps <- intersect(dependency_closure(graph, cid), ids)
    row <- match(cid, schedule$component_id)
    if (!is.na(row) && schedule$group_kind[row] == "cycle") {
      group <- schedule$component_id[schedule$group_id == schedule$group_id[row]]
      deps <- setdiff(deps, group[match(group, schedule$component_id) >= row])
    }
    deps
  }), ids)
}
