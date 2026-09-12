#' Inspect a migration before making model calls or running programs
#'
#' Scans sources and resolves library bindings with the same project scanner
#' used by [sas_translate()]. Reads directory listings, not dataset contents.
#' Does not create output directories, scan caches, adapters, or usage ledgers.
#' Deterministic support findings are advisory: an AI translation may handle
#' deferred constructs, and runtime checks can still defer supported shapes.
#'
#' @inheritParams sas_translate
#' @param out_dir Planned migration output root. `NULL` reports a temporary
#'   output root, as used by `sas_translate()`, without creating it.
#' @return A `sas2r_preflight` list with `sources`, point-of-use `libraries`,
#'   `inputs` (available, missing, unresolved, no_producer, or generated), `references`, `unsupported`,
#'   scanner `findings`, `outputs`, `destinations`, `budget`, `next_actions`, and `model_calls`.
#'   Also includes `status`, `configured_libraries`, `schedule`, explanatory `notes`,
#'   and the scanned `project`, reusable by `sas_translate()` while sources are unchanged.
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
                          usage_limits = NULL, recursive = FALSE) {
  cfg <- translation_config(path, config)
  budget <- translation_budget(budget_usd, budget_mode, pricing_source,
                               pricing_rates, usage_limits)
  project <- if (inherits(path, "sas2r_project")) path else
    sas_project(path, config = cfg, cache = FALSE, recursive = recursive)
  overrides <- validate_output_overrides(outputs %||% cfg$outputs)
  validate_effective_qc(overrides, cfg$comparison_rules)
  plan <- translation_plan(project, overrides)
  contracts <- plan$contracts
  effective <- effective_librefs(project)
  inputs <- preflight_inputs(project, effective)
  unsupported <- preflight_unsupported(project)
  limits <- c("mode", usage_limit_names(), "pricing_source")
  reference_paths <- contracts$reference_path
  keep_refs <- which(!is.na(reference_paths) & nzchar(reference_paths))
  references <- tibble::tibble(
    target_key = contracts$target_key[keep_refs],
    path = config_resolve_paths(reference_paths[keep_refs], getwd()),
    status = c("missing", "available")[1L + as.integer(
      file.exists(reference_paths[keep_refs]) & !dir.exists(reference_paths[keep_refs]))]
  )
  findings <- project$flags
  unresolved <- findings$kind %in% c(
    "autoexec_missing", "unresolved_include", "dynamic_include", "include_cycle",
    "include_depth_exceeded", "unresolved_macro", "dependency_cycle",
    "libref_context_truncated", "libref_undeclared", "libref_engine_unsupported",
    "dynamic_dataset_reference")
  needs_attention <- any(inputs$status %in% c("missing", "unresolved", "no_producer")) || any(unresolved) || any(references$status == "missing")
  root <- if (is.null(out_dir)) "<temporary output root>" else
    config_resolve_paths(out_dir, getwd())
  paths <- migration_paths(root, "<run_id>")
  destinations <- list(root = root, run = paths$attempts, state = paths$state,
                       generated_outputs = paths$generated_outputs,
                       bundle = file.path(paths$attempts, "<bundle_attempt_id>", "bundle"),
                       report = paths$report_json, report_json = paths$report_json,
                       report_md = paths$report_md)
  sources <- project$files
  sources$file <- config_resolve_paths(sources$file, getwd())
  structure(list(
    status = if (needs_attention) "needs_attention" else "ready_for_translation",
    sources = sources, libraries = effective$bindings,
    configured_libraries = effective$seed, inputs = inputs, references = references,
    unsupported = unsupported, findings = findings, outputs = contracts,
    schedule = plan$schedule, project = project, destinations = destinations,
    budget = as.list(budget)[limits], model_calls = 0L,
    next_actions = c(
      if (any(inputs$status == "missing")) "Supply missing input members or correct their library paths; inspect $inputs$searched_paths.",
      if (any(inputs$status == "no_producer")) "Identify or supply the earlier step that creates the WORK input; inspect $inputs source locations.",
      if (any(inputs$status == "unresolved")) "Resolve input library bindings at the reported source locations; inspect $libraries.",
      if (any(references$status == "missing")) "Supply the configured SAS reference files or correct their paths; inspect $references.",
      if (any(unresolved)) "Resolve the reported include, macro, or dependency findings before execution.",
      if (nrow(unsupported)) "Review deferred constructs; they require AI translation or manual implementation."
    ),
    notes = c("Static inspection only; datasets and generated programs were not executed.",
              "Budget matches sas_translate arguments; config$budget is not used by that entry point.",
              "Run and attempt identifiers are assigned when translation starts.")
  ), class = "sas2r_preflight")
}

preflight_inputs <- function(project, effective) {
  lineage <- project$lineage
  bindings <- effective$bindings
  # Binding identity is already resolved by the scanner, including conflicting
  # include contexts. Resolve each lineage row once, then index producers.
  idx <- match(lineage$binding_id, bindings$binding_id)
  paths <- bindings$selected_path[idx]
  paths[is.na(lineage$binding_status) | lineage$binding_status != "bound"] <- NA_character_
  paths[startsWith(lineage$dataset, "work.")] <- "<session work>"
  identities <- paste(lineage$dataset, paths, sep = "\r")
  writers <- which(lineage$role == "creates" & !is.na(paths))
  producers <- split(writers, identities[writers])
  reads <- which(lineage$role == "reads")
  n <- length(reads)
  status <- rep("unresolved", n)
  found_path <- rep(NA_character_, n)
  searched <- rep(list(character()), n)
  for (i in seq_along(reads)) {
    row <- reads[i]
    path <- paths[row]
    if (is.na(path) || !nzchar(path)) next
    candidates <- producers[[identities[row]]]
    # A later statement in the same program cannot supply an earlier read.
    generated <- any(lineage$unit_id[candidates] != lineage$unit_id[row] &
      (lineage$file[candidates] != lineage$file[row] |
       lineage$unit_id[candidates] < lineage$unit_id[row]))
    if (generated) { status[i] <- "generated"; next }
    if (path == "<session work>") { status[i] <- "no_producer"; next }
    member <- sub("^[^.]+\\.", "", lineage$dataset[row])
    candidates <- file.path(path, paste0(member, c(".rds", ".sas7bdat", ".xpt")))
    searched[[i]] <- candidates
    found <- candidates[file.exists(candidates) & !dir.exists(candidates)]
    status[i] <- if (length(found)) "available" else "missing"
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
  cli::cli_h1("sas2r offline preflight: {x$status}")
  cli::cli_text("{nrow(x$sources)} source files; {nrow(x$outputs)} output targets; 0 model calls")
  if (nrow(x$inputs)) print(x$inputs)
  if (nrow(x$references)) print(x$references)
  if (nrow(x$unsupported)) print(x$unsupported)
  if (nrow(x$findings)) print(x$findings)
  for (action in x$next_actions) cli::cli_text("{action}")
  cli::cli_text("Output root: {x$destinations$root}")
  cli::cli_text("Budget: {x$budget$mode}; max_usd={x$budget$max_usd}; max_calls={x$budget$max_calls}")
  cli::cli_text("Static inspection only; inspect $libraries, $outputs, $budget for full details.")
  invisible(x)
}
