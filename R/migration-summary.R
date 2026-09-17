# Coverage comes from target checks, independently of a bundle-level label.
migration_coverage <- function(targets = list(), histories = list()) {
  is_unresolved <- vapply(targets, function(t) identical(t$status, "unresolved_target"), logical(1))
  unresolved <- names(targets)[is_unresolved]
  targets <- targets[!is_unresolved]
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
    unresolved_output_expressions = unresolved,
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
  completed <- last_usage_records_by_id(budget$records %||% list(), "request_completed")
  unknown <- function(field) sum(vapply(completed, function(r)
    is.na(nonnegative_number_or_na(r[[field]])), logical(1)))
  reported_tokens <- function(field) {
    values <- vapply(completed, function(r) nonnegative_number_or_na(r[[field]]), numeric(1))
    if (length(values) && all(is.na(values))) NA_real_ else budget[[field]] %||% NA_real_
  }
  list(
    known_amount = budget$known_amount %||% 0,
    billed_amount = budget$billed_amount %||% 0,
    estimated_amount = budget$estimated_amount %||% 0,
    unknown_cost_calls = budget$unknown_count %||% 0L,
    calls = budget$request_count %||% budget$calls %||% 0L,
    input_tokens = reported_tokens("total_input_tokens"),
    output_tokens = reported_tokens("total_output_tokens"),
    input_token_category = reported_tokens("input_tokens"),
    output_token_category = reported_tokens("output_tokens"),
    cached_input_tokens = reported_tokens("cached_input_tokens"),
    cache_write_tokens = reported_tokens("cache_write_tokens"),
    reasoning_tokens = reported_tokens("reasoning_tokens"),
    unknown_input_token_calls = unknown("total_input_tokens"),
    unknown_output_token_calls = unknown("total_output_tokens"),
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
    paste0("Unresolved output expressions (not counted as concrete targets): ", shown(coverage$unresolved_output_expressions)),
    sprintf("Independent reviews: %d / %d components.", coverage$components_independently_reviewed, coverage$components_total),
    paste0("Targets contributing validation evidence: ", shown(coverage$validated_targets)),
    paste0("Targets without a completed reference comparison: ", shown(coverage$unreferenced_targets)))
}

migration_usage_lines <- function(usage) {
  c(sprintf("Elapsed: %.1f seconds. Provider calls: %d. Known spend: $%.4f (billed $%.4f; estimated $%.4f; unknown cost calls: %d).",
            usage$elapsed_seconds, usage$calls, usage$known_amount, usage$billed_amount, usage$estimated_amount, usage$unknown_cost_calls),
    sprintf("Known token totals: input %s; output %s (includes reasoning %s). Cached input %s; cache creation %s. Unknown input usage calls: %s; unknown output usage calls: %s.",
      usage$input_tokens, usage$output_tokens, usage$reasoning_tokens,
      usage$cached_input_tokens, usage$cache_write_tokens,
      usage$unknown_input_token_calls, usage$unknown_output_token_calls),
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
