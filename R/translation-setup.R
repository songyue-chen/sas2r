# Shared offline configuration and budget resolution for migration and preflight.
project_input_root <- function(path) {
  if (inherits(path, "sas2r_project")) return(include_normalize_path(path$project_dir))
  if (!is_scalar_character(path) || (!dir.exists(path) && !file.exists(path))) {
    cli::cli_abort("Path does not exist or is invalid: {.val {path}}", class = "sas2r_invalid_argument")
  }
  include_normalize_path(if (dir.exists(path)) path else dirname(path))
}

# Config provenance is kept for reports; scan decisions use normalized values.
scan_config_fields <- function(config) {
  fields <- config[c("libraries", "macro_search_path", "include_roots", "autoexec")]
  fields$libraries <- fields$libraries[sort(names(fields$libraries))]
  fields
}

translation_config <- function(path, config) {
  root <- project_input_root(path)
  cfg <- if (inherits(config, "sas2r_config")) config else if (is.character(config)) {
    if (!is_scalar_character(config) || !file.exists(config) || dir.exists(config)) {
      cli::cli_abort("Configuration file not found: {.file {config}}", class = "sas2r_config_error")
    }
    sas_config(path = config)
  } else if (is.list(config)) {
    # Plain lists update a reused project's whole top-level fields. Omission
    # inherits; an explicit NULL or empty value remains an intentional override.
    assert_exact_names(config, PROJECT_CONFIG_KEYS)
    if (inherits(path, "sas2r_project")) {
      updated <- path$config
      updated[names(config)] <- config
      if (length(config)) {
        updated$raw <- NULL
        updated$source <- NA_character_
      }
      updated
    } else config
  } else if (is.null(config)) {
    if (inherits(path, "sas2r_project")) path$config else sas_config(start = root)
  } else cli::cli_abort("config must be a mapping or configuration file", class = "sas2r_config_error")
  cfg <- normalize_project_config(cfg, root)
  if (inherits(path, "sas2r_project") && !is.null(config) &&
      !identical(scan_config_fields(cfg), scan_config_fields(normalize_project_config(path$config, root)))) {
    cli::cli_abort(c("Configuration changes the supplied project's source or library bindings.",
      "i" = "Run sas_preflight() or sas_translate() on the source path with the new config to rescan."),
      class = "sas2r_config_error")
  }
  cfg
}

translation_setup <- function(path, config, outputs, recursive, cache = FALSE,
                              max_parallel_translations = NULL, llm = NULL) {
  cfg <- translation_config(path, config)
  translation_limit <- normalize_max_parallel_translations(max_parallel_translations %||% cfg$migration$max_parallel_translations)
  # An explicit adapter replaces the YAML LLM settings. Validate its actual
  # transport settings, before source scanning, cache writes or provider setup.
  llm_config <- if (is.null(llm)) cfg$llm else attr(llm, "parallel_config", exact = TRUE)
  validate_parallel_retry_settings(translation_limit, llm_config$max_tries)
  overrides <- if (is.null(outputs)) cfg$outputs else validate_output_overrides(outputs)
  if (!is.null(outputs) && is.list(overrides) && length(overrides$references)) {
    overrides$references <- lapply(overrides$references, config_anchor_paths, base = getwd())
  }
  cfg$outputs <- overrides
  project <- if (inherits(path, "sas2r_project")) path else
    scan_project(path, config = cfg, recursive = recursive, cache = cache)
  if (!inherits(path, "sas2r_project")) cfg <- project$config
  plan <- translation_plan(project, overrides, cfg$comparison_rules)
  plan$pipeline <- translation_pipeline_coverage(project, plan$graph, plan$schedule)
  if (inherits(path, "sas2r_project")) validate_effective_qc(overrides, cfg$comparison_rules, plan$contracts)
  # The returned project represents this complete plan, including explicit
  # output overrides, so reusing it does not silently lose preflight settings.
  cfg$outputs <- overrides
  project$config <- cfg
  project$output_contracts <- plan$contracts
  project$graph <- plan$graph
  project$schedule <- plan$schedule
  project$readiness <- translation_readiness(project, plan)
  project$dependency_findings <- NULL
  list(config = cfg, project = project, plan = plan, max_parallel_translations = translation_limit)
}

usage_limit_names <- function() grep("^max_", names(formals(new_usage_budget)), value = TRUE)

translation_budget <- function(budget_usd, budget_mode, pricing_source,
                               pricing_rates, usage_limits,
                               ledger_path = NULL, resume = FALSE) {
  # A mistyped mode must refuse, not fall through to a weaker default: budget
  # enforcement and cost provenance are safety knobs on a paid path.
  if (!is.character(budget_mode) || length(budget_mode) != 1L ||
      !budget_mode %in% c("stop", "strict", "soft", "observe")) {
    cli::cli_abort(
      "budget_mode must be one of \"stop\", \"strict\", \"soft\", or \"observe\", not {.val {budget_mode}}",
      class = "sas2r_invalid_argument"
    )
  }
  if (!is.character(pricing_source) || length(pricing_source) != 1L ||
      !pricing_source %in% c("catalog", "adapter", "organization", "external")) {
    cli::cli_abort(
      "pricing_source must be one of \"catalog\", \"adapter\", \"organization\", or \"external\", not {.val {pricing_source}}",
      class = "sas2r_invalid_argument"
    )
  }

  budget_mode_norm <- if (!is.finite(budget_usd)) {
    if (identical(budget_mode, "soft")) "soft" else "observe"
  } else if (identical(budget_mode, "stop") || identical(budget_mode, "strict")) {
    "strict"
  } else if (identical(budget_mode, "soft")) {
    "soft"
  } else {
    "observe"
  }

  pricing_source_norm <- if (identical(pricing_source, "catalog")) "adapter" else pricing_source
  if (identical(budget_mode_norm, "strict") && is.finite(budget_usd) && identical(pricing_source_norm, "adapter"))
    cli::cli_abort("A strict dollar budget requires pricing_source = 'organization' and pricing_rates; use budget_mode = 'soft' for catalog estimates", class = "sas2r_budget_config_error")

  usage_limits_map <- usage_limits %||% list()
  limit_names <- setdiff(usage_limit_names(), "max_usd")
  if (!is.list(usage_limits_map) ||
      (length(usage_limits_map) && (is.null(names(usage_limits_map)) ||
       any(!names(usage_limits_map) %in% limit_names) || anyDuplicated(names(usage_limits_map))))) {
    cli::cli_abort("usage_limits must be a named list of request limits: {.val {limit_names}}",
                   class = "sas2r_budget_config_error")
  }
  do.call(new_usage_budget, c(list(
    mode = budget_mode_norm,
    max_usd = budget_usd,
    pricing_source = pricing_source_norm,
    rates = pricing_rates,
    ledger_path = ledger_path,
    resume = isTRUE(resume)
  ), usage_limits_map))

}

translation_plan <- function(project, overrides, rules = project$config$comparison_rules) {
  same_references <- identical(rules[c("references", "reference_path")],
    project$config$comparison_rules[c("references", "reference_path")])
  contracts <- if (identical(overrides, project$config$outputs) && same_references) project$output_contracts else {
    updated <- project
    updated$config$comparison_rules <- rules
    infer_output_contracts(updated, overrides)
  }
  if (!is.null(project$graph) && identical(contracts, project$output_contracts)) {
    return(list(contracts = contracts, graph = project$graph, schedule = project$schedule))
  }
  graph <- build_dependency_graph(project, output_contracts = contracts)
  list(contracts = contracts, graph = graph, schedule = stable_dependency_schedule(graph))
}
