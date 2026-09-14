# Coverage comes from target checks, independently of a bundle-level label.
migration_coverage <- function(targets = list(), histories = list()) {
  detail <- lapply(targets, function(t) {
    compared <- !is.null(t$checks$reference_comparison) &&
      !is.null(t$checks$reference_comparison$passed) && !is.na(t$checks$reference_comparison$passed)
    list(
      target = t$target_key,
      produced = isTRUE(t$checks$candidate_exists$passed),
      reference_compared = compared,
      passed = isTRUE(t$passed),
      reference_passed = isTRUE(t$reference_passed),
      contributes_to_validated = isTRUE(t$required) && isTRUE(t$passed) &&
        isTRUE(t$has_reference) && isTRUE(t$reference_passed)
    )
  })
  count <- function(field) sum(vapply(detail, function(t) isTRUE(t[[field]]), logical(1)))
  reviewed <- vapply(histories, component_review_verdict, character(1)) %in%
    c("reviewed_no_material_finding", "repair_required")
  list(
    outputs_total = length(targets),
    outputs_produced = count("produced"),
    outputs_reference_compared = count("reference_compared"),
    outputs_passed = count("passed"),
    outputs_reference_passed = count("reference_passed"),
    components_total = length(histories),
    components_independently_reviewed = sum(reviewed),
    validated_targets = names(detail)[vapply(detail, function(t) t$contributes_to_validated, logical(1))],
    unreferenced_targets = names(detail)[!vapply(detail, function(t) t$reference_compared, logical(1))],
    targets = detail
  )
}

migration_usage_summary <- function(budget) {
  limits <- usage_limit_names()
  list(
    known_amount = budget$known_amount %||% 0,
    billed_amount = budget$billed_amount %||% 0,
    estimated_amount = budget$estimated_amount %||% 0,
    unknown_cost_calls = budget$unknown_count %||% 0L,
    calls = budget$request_count %||% budget$calls %||% 0L,
    input_tokens = budget$input_tokens %||% 0L,
    output_tokens = budget$output_tokens %||% 0L,
    pricing_source = budget$pricing_source %||% "unavailable",
    elapsed_seconds = if (is.null(budget$start_time)) NA_real_ else
      as.numeric(difftime(budget$end_time %||% Sys.time(), budget$start_time, units = "secs")),
    mode = budget$mode %||% "unavailable",
    limits = stats::setNames(lapply(limits, function(nm) {
      value <- budget[[nm]]
      if (is.null(value)) "unavailable" else if (is.finite(value)) value else "unlimited"
    }), limits)
  )
}

migration_coverage_lines <- function(coverage) {
  shown <- function(x) if (length(x)) paste(x, collapse = ", ") else "(none)"
  c(sprintf("Outputs: %d produced / %d targets; %d reference-compared; %d passed (%d reference comparisons passed).",
            coverage$outputs_produced, coverage$outputs_total, coverage$outputs_reference_compared,
            coverage$outputs_passed, coverage$outputs_reference_passed),
    sprintf("Independent reviews: %d / %d components.", coverage$components_independently_reviewed, coverage$components_total),
    paste0("Targets contributing validation evidence: ", shown(coverage$validated_targets)),
    paste0("Targets without a completed reference comparison: ", shown(coverage$unreferenced_targets)))
}

migration_usage_lines <- function(usage) {
  c(sprintf("Elapsed: %.1f seconds. Provider calls: %d. Known spend: $%.4f (billed $%.4f; estimated $%.4f; unknown cost calls: %d).",
            usage$elapsed_seconds, usage$calls, usage$known_amount, usage$billed_amount, usage$estimated_amount, usage$unknown_cost_calls),
    paste0("Effective limits (", usage$mode, "): ", paste(names(usage$limits), unlist(usage$limits), sep = "=", collapse = ", ")))
}

# Versions come from this process's namespaces, not the installed DESCRIPTION
# that another R session (or an install while this session runs) might see.
migration_environment <- function(state) {
  loaded_version <- function(package) {
    if (!package %in% loadedNamespaces()) return("not_loaded")
    as.character(getNamespaceVersion(package))
  }
  specs <- load_agent_specs(project_dir = state$project$project_dir)
  specs <- specs[migration_agent_names()]
  agents <- lapply(specs, function(spec) {
    narrower <- Filter(function(tool) tool$max_calls != spec$tool_call_limit, spec$tools)
    list(tool_call_limit = spec$tool_call_limit,
         tool_overrides = lapply(narrower, function(tool) tool$max_calls))
  })
  list(
    versions = list(sas2r = loaded_version("sas2r"), ellmer = loaded_version("ellmer"),
                    R = as.character(getRversion())),
    execution_root = state$project$project_dir,
    agents = agents,
    repairs = list(immediate_per_component = state$max_program_repair_rounds,
                   bundle_per_component = state$max_bundle_repairs_per_component,
                   bundle_overall_cap = state$max_bundle_repair_rounds %||% "not_set",
                   bundle_derived_ceiling = as.double(state$max_bundle_repairs_per_component) *
                     length(unique(state$schedule$component_id)))
  )
}

migration_environment_lines <- function(info) {
  if (is.null(info)) return(character())
  c(paste("Loaded versions:", paste(names(info$versions), unlist(info$versions),
                                    sep = "=", collapse = ", ")),
    paste0("Execution root: ", info$execution_root),
    vapply(names(info$agents), function(role) {
      agent <- info$agents[[role]]
      paste0(role, ": ", agent$tool_call_limit, " tool calls per invocation",
             if (length(agent$tool_overrides)) paste0("; tool overrides: ", paste(
               names(agent$tool_overrides), unlist(agent$tool_overrides), sep = "=", collapse = ", ")))
    }, character(1)),
    paste("Repair limits:", paste(names(info$repairs), unlist(info$repairs), sep = "=", collapse = ", ")))
}
