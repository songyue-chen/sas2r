# Shared offline configuration and budget resolution for migration and preflight.
translation_config <- function(path, config) {
  if (inherits(config, "sas2r_config")) {
    config
  } else if (is.character(config) && length(config) == 1L) {
    if (is.na(config) || !file.exists(config) || dir.exists(config)) {
      cli::cli_abort("Configuration file not found: {.file {config}}", class = "sas2r_config_error")
    }
    sas_config(path = config)
  } else if (is.list(config)) {
    structure(config, class = "sas2r_config")
  } else {
    start_dir <- if (inherits(path, "sas2r_project")) {
      path$project_dir
    } else if (is.character(path) && length(path) == 1L) {
      if (dir.exists(path)) path else dirname(path)
    } else {
      "."
    }
    sas_config(start = start_dir)
  }

}

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

  usage_limits_map <- usage_limits %||% list()
  limit_names <- setdiff(names(formals(new_usage_budget)),
                         c("mode", "max_usd", "rates", "pricing_source", "ledger_path", "run_id", "resume"))
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
