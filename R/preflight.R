#' Inspect a migration and optionally diagnose findings
#'
#' Scans sources and resolves library bindings with the same project scanner
#' used by [sas_translate()]. Reads directory listings, not dataset contents.
#' Does not execute programs, read dataset contents, or create output directories,
#' scan caches, or usage ledgers. With a configured LLM, actionable findings can
#' receive one bounded advisory diagnosis. Use `diagnose = "off"` for offline
#' inspection without constructing an adapter or making a model call.
#' Deterministic support findings are advisory: an AI translation may handle
#' deferred constructs, and runtime checks can still defer supported shapes.
#'
#' @inheritParams sas_translate
#' @param max_parallel_translations Concurrent component workflow limit used
#'   when planning translation. Overrides `migration.max_parallel_translations`.
#'   Invalid combinations are reported by static checks; automatic diagnosis
#'   can explain these errors when a valid diagnosis budget is available.
#' @param diagnose `"auto"` (default) diagnoses actionable findings when an LLM
#'   is configured; `"off"` always stays offline. Advice never clears findings.
#' @param llm Optional `sas2r_llm` adapter for diagnosis, overriding `llm:`
#'   configuration. Missing configuration is reported, not treated as a failure.
#' @param out_dir Planned migration output root. `NULL` reports a temporary
#'   output root, as used by `sas_translate()`, without creating it.
#' @return A `sas2r_preflight` list with `sources`, point-of-use `libraries`,
#'   `inputs` (available, missing, unresolved, no_producer, deferred, backward_dependency,
#'   created_if_missing, generated, or environment), `references`, `unsupported`,
#'   scanner `findings`, shared `readiness` warnings, `outputs`, `destinations`, `budget`, `next_actions`, `diagnosis`, and actual `model_calls`.
#'   Diagnosis failures preserve the static result. Inspection errors retain
#'   their original condition with optional advice attached as `$diagnosis`.
#'   `called_macros` lists reachable search-path macro definitions and their
#'   standalone R and interface-test paths.
#'   `pipeline` reconciles every scanned file with the translation schedule and
#'   bundle execution roles, listing intentional exclusions and blocking gaps.
#'   Also includes `status`, `configured_libraries`, `schedule`, explanatory `notes`,
#'   `max_parallel_translations`, and the scanned `project`, reusable by `sas_translate()` while sources are unchanged.
#'   `ready_for_translation` means no known missing inputs or unresolved scan
#'   findings; it does not mean execution or reference validation has passed.
#' @examples
#' source <- tempfile(fileext = ".sas")
#' writeLines("data out; set raw.dm; run;", source)
#' check <- sas_preflight(source, usage_limits = list(max_calls = 20))
#' check$inputs
#' check$model_calls
#' unlink(source)
#' @export
sas_preflight <- function(path, out_dir = NULL, config = NULL, outputs = NULL,
                          budget_usd = Inf, budget_mode = "stop",
                          pricing_source = "catalog", pricing_rates = NULL,
                          usage_limits = NULL, recursive = FALSE, max_parallel_translations = NULL,
                          diagnose = c("auto", "off"), llm = NULL) {
  diagnose <- match.arg(diagnose)
  cfg <- budget <- NULL
  tryCatch({
    root <- if (is.null(out_dir)) "<temporary output root>" else migration_paths(out_dir)$root
    if (!is.null(out_dir)) root <- config_anchor_paths(root, getwd())
    cfg <- translation_config(path, config)
    configured_budget <- cfg$budget %||% list()
    if (missing(budget_usd)) budget_usd <- configured_budget$max_usd %||% budget_usd
    if (missing(budget_mode)) budget_mode <- configured_budget$mode %||% budget_mode
    if (missing(pricing_source)) pricing_source <- configured_budget$pricing_source %||% pricing_source
    if (missing(pricing_rates)) pricing_rates <- configured_budget$rates %||% pricing_rates
    configured_limits <- configured_budget[intersect(names(configured_budget), setdiff(usage_limit_names(), "max_usd"))]
    usage_limits <- utils::modifyList(configured_limits, usage_limits %||% list())
    config <- cfg
    budget <- translation_budget(budget_usd, budget_mode, pricing_source,
                                 pricing_rates, usage_limits)
    setup <- translation_setup(path, config, outputs, recursive,
                               max_parallel_translations = max_parallel_translations)
    cfg <- setup$config
    project <- setup$project
    plan <- setup$plan
    contracts <- plan$contracts
    effective <- effective_librefs(project)
    inputs <- project$readiness$inputs
    unsupported <- preflight_unsupported(project)
    limits <- c("mode", usage_limit_names(), "pricing_source")
    references <- project$readiness$references
    findings <- project$flags
    unresolved <- findings$kind %in% preflight_blocking_findings()
    for (lib in c("sashelp", "dictionary")) {
      reads <- startsWith(tolower(inputs$dataset), paste0(lib, "."))
      if (any(reads) && all(inputs$status[reads] == "environment"))
        unresolved[findings$kind == "libref_undeclared" & tolower(findings$detail) == lib] <- FALSE
    }
    needs_attention <- any(inputs$status %in% c("missing", "unresolved", "no_producer", "deferred", "backward_dependency")) || any(unresolved) || any(references$status == "missing") || length(plan$pipeline$issues) > 0L
    paths <- migration_paths(root, "<run_id>")
    destinations <- list(root = root, run = paths$run_root, state = paths$state,
                         generated_outputs = paths$outputs,
                         bundle = paths$bundle,
                         report_json = paths$report_json,
                         report_md = paths$report_md)
    sources <- project$files
    result <- structure(list(
      status = if (needs_attention) "needs_attention" else "ready_for_translation",
      sources = sources, called_macros = called_macro_units(project), libraries = effective$bindings,
      configured_libraries = effective$seed, inputs = inputs, references = references,
      unsupported = unsupported, findings = findings, outputs = contracts,
      schedule = plan$schedule, pipeline = plan$pipeline, readiness = project$readiness,
      project = project, destinations = destinations,
      budget = as.list(budget)[limits], model_calls = 0L,
      max_parallel_translations = setup$max_parallel_translations,
      next_actions = c(
        if (length(plan$pipeline$issues)) "Resolve the pipeline coverage gaps; inspect $pipeline$sources and $pipeline$issues.",
        if (any(inputs$status == "missing")) "Supply missing input members or correct their library paths; inspect $inputs$searched_paths.",
        if (any(inputs$status == "backward_dependency")) "Move the producer before its read in the same source file; an existing output does not establish correct execution order.",
        if (any(inputs$status == "no_producer")) "Identify or supply the earlier step or macro that creates the current input; inspect $inputs source locations.",
        if (any(inputs$status == "deferred")) "Review macro or other unmodeled effects on dataset state; the current producer is unknown.",
        if (any(inputs$status == "unresolved")) "Resolve input library bindings at the reported source locations; inspect $libraries.",
        if (any(references$status == "missing")) "Supply the configured SAS reference files or correct their paths; inspect $references.",
        if (any(unresolved)) "Resolve the reported include, macro, or dependency findings before execution.",
        if (nrow(unsupported)) "Review deferred constructs; they require AI translation or manual implementation."
      ),
      notes = c("Static inspection only; datasets and generated programs were not executed.",
                "Missing resources normally allow translation with warnings; affected execution remains unavailable.",
                "Budget includes project configuration; explicit arguments take precedence.",
                "Run and attempt identifiers are assigned when translation starts.")
    ), class = "sas2r_preflight")
    result$diagnosis <- preflight_diagnosis(result, cfg, budget, llm, diagnose)
    result$model_calls <- result$diagnosis$model_calls
    result
  }, error = function(error) {
    error$diagnosis <- preflight_diagnosis(config = cfg, budget = budget,
      llm = llm, diagnose = diagnose, error = error)
    for (line in preflight_diagnosis_lines(error$diagnosis)) cli::cli_inform("{line}")
    stop(error)
  })
}

preflight_inputs <- function(project, effective) {
  lineage <- project$lineage
  producers <- dataset_producers(project, effective)
  paths <- producers$path
  reads <- which(lineage$role == "reads")
  n <- length(reads)
  status <- rep("unresolved", n)
  found_path <- rep(NA_character_, n)
  searched <- rep(list(character()), n)
  append_targets <- lineage$role == "creates" & lineage$proc == "append"
  keys <- paste(lineage$unit_id, lineage$dataset)
  create_if_missing <- keys %in% keys[append_targets]
  for (i in seq_along(reads)) {
    row <- reads[i]
    if (tolower(lineage$dataset[row]) %in% SAS_METADATA_RESOURCES) { status[i] <- "environment"; next }
    path <- paths[row]
    if (is.na(path) || !nzchar(path)) next
    if (producers$backward[row]) { status[i] <- "backward_dependency"; next }
    if (isTRUE(producers$deferred[row])) { status[i] <- "deferred"; next }
    if (isTRUE(producers$generated[row])) { status[i] <- "generated"; next }
    if (!is.na(producers$writer[row])) { status[i] <- "generated"; next }
    if (path == "<session work>") {
      status[i] <- if (create_if_missing[row]) "created_if_missing" else "no_producer"
      next
    }
    member <- sub("^[^.]+\\.", "", lineage$dataset[row])
    candidates <- file.path(path, paste0(member, c(".rds", ".sas7bdat", ".xpt")))
    searched[[i]] <- candidates
    found <- candidates[file.exists(candidates) & !dir.exists(candidates)]
    status[i] <- if (length(found)) "available" else if (create_if_missing[row]) "created_if_missing" else "missing"
    if (length(found)) found_path[i] <- found[1L]
  }
  tibble::tibble(dataset = lineage$dataset[reads], file = lineage$file[reads],
    line = lineage$line[reads], library_path = paths[reads], status = status,
    path = found_path, searched_paths = searched)
}

preflight_unsupported <- function(project) {
  rb <- load_rulebook()
  units <- project$statements
  rows <- split(seq_len(nrow(units)), units$unit_id)
  rows <- rows[vapply(rows, function(i) units$unit_type[i[1L]] %in%
    c("data_step", "proc_step", "macro_def"), logical(1))]
  reasons <- vapply(rows, function(i) {
    deterministic_unit_translation(units[i, ], rb)$reason %||% NA_character_
  }, character(1))
  first <- vapply(rows, `[`, integer(1), 1L)
  keep <- !is.na(reasons)
  first <- first[keep]
  tibble::tibble(unit_id = units$unit_id[first], file = units$file[first],
    line = units$line_start[first], unit_type = units$unit_type[first],
    reason = unname(reasons[keep]))
}

#' @export
print.sas2r_preflight <- function(x, ...) {
  cli::cli_h1("sas2r preflight: {x$status}")
  cli::cli_text("{nrow(x$sources)} source files; {nrow(x$outputs)} output targets; {x$model_calls} model calls")
  for (line in pipeline_coverage_lines(x$pipeline)) cli::cli_text("{line}")
  for (issue in x$pipeline$issues) cli::cli_text("ERROR: {issue}")
  for (line in readiness_warning_lines(x$readiness)) cli::cli_text("WARNING: {line}")
  if (nrow(x$called_macros)) {
    cli::cli_text("{nrow(x$called_macros)} called macro dependencies:")
    print(x$called_macros[c("name", "file", "staged_file")])
  }
  if (nrow(x$inputs)) print(x$inputs)
  if (nrow(x$references)) print(x$references)
  if (nrow(x$unsupported)) print(x$unsupported)
  if (nrow(x$findings)) print(x$findings)
  for (action in x$next_actions) cli::cli_text("{action}")
  for (line in preflight_diagnosis_lines(x$diagnosis)) cli::cli_text("{line}")
  cli::cli_text("Output root: {x$destinations$root}")
  cli::cli_text("Budget: {x$budget$mode}; max_usd={x$budget$max_usd}; max_calls={x$budget$max_calls}")
  cli::cli_text("Concurrent translation limit: {x$max_parallel_translations}; local execution and repair remain serial.")
  cli::cli_text("Static inspection only; inspect $libraries, $outputs, $budget for full details.")
  invisible(x)
}

preflight_blocking_findings <- function() c(
    "autoexec_missing", "unresolved_include", "dynamic_include", "include_cycle",
    "include_depth_exceeded", "unresolved_macro", "dependency_cycle",
    "libref_context_truncated", "libref_undeclared", "libref_engine_unsupported",
    "dynamic_dataset_reference", "backward_dependency", "macro_data_flow_deferred",
    "dataset_statement_deferred", "macro_definition_missing",
    "macro_library_initialization_unsupported", "macro_include_requires_expansion",
    "macro_nested_definition_unsupported", "macro_dependency_analysis_deferred")

preflight_advisory_findings <- function() c("autoexec_autodiscovered", "macro_shadowing",
  "sasautos_from_environment", "sasautos_from_program", "macro_expansion_unverified")
