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
#'   `inputs` (available, missing, unresolved, or generated), `references`, `unsupported`,
#'   scanner `findings`, `outputs`, `destinations`, `budget`, `next_actions`, and `model_calls`.
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
  contracts <- infer_output_contracts(project, outputs %||% cfg$outputs)
  effective <- effective_librefs(project)
  inputs <- preflight_inputs(project, effective)
  unsupported <- preflight_unsupported(project)
  graph <- build_dependency_graph(project, output_contracts = contracts)
  limits <- c("mode", "max_usd", "max_calls", "max_retries", "max_tool_calls",
              "max_wall_time", "max_request_bytes", "max_request_chars",
              "max_input_tokens", "max_output_tokens", "pricing_source")
  reference_paths <- contracts$reference_path
  keep_refs <- which(!is.na(reference_paths) & nzchar(reference_paths))
  references <- tibble::tibble(
    target_key = contracts$target_key[keep_refs],
    path = config_resolve_paths(reference_paths[keep_refs], getwd()),
    status = ifelse(file.exists(reference_paths[keep_refs]) & !dir.exists(reference_paths[keep_refs]),
                    "available", "missing")
  )
  findings <- project$flags
  unresolved <- grepl("missing|unresolved|ambiguous|cycle|truncat", findings$kind)
  needs_attention <- any(inputs$status %in% c("missing", "unresolved")) || any(unresolved) || any(references$status == "missing")
  root <- if (is.null(out_dir)) "<temporary output root>" else
    config_resolve_paths(out_dir, getwd())
  paths <- migration_paths(root, "<run_id>")
  destinations <- list(root = root, run = paths$attempts, state = paths$state,
                       generated_outputs = file.path(paths$attempts, "generated-outputs"),
                       bundle = file.path(paths$attempts, "<bundle_attempt_id>", "bundle"),
                       report = file.path(paths$attempts, "report.json"))
  sources <- project$files
  sources$file <- config_resolve_paths(sources$file, getwd())
  structure(list(
    status = if (needs_attention) "needs_attention" else "ready_for_translation",
    sources = sources, libraries = effective$bindings,
    configured_libraries = effective$seed, inputs = inputs, references = references,
    unsupported = unsupported, findings = findings, outputs = contracts,
    schedule = stable_dependency_schedule(graph), destinations = destinations,
    budget = as.list(budget)[limits], model_calls = 0L,
    next_actions = c(
      if (any(inputs$status == "missing")) "Supply missing input members or correct their library paths; inspect $inputs$searched_paths.",
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
  reads <- lineage[lineage$role == "reads", ]
  writes <- lineage[lineage$role == "creates", ]
  bindings <- effective$bindings
  paths_for <- function(row) {
    if (startsWith(row$dataset, "work.")) return("<session work>")
    hit <- bindings$kind == "reference" & bindings$use_file == row$file &
      bindings$use_line == row$line & bindings$libref == sub("\\..*$", "", row$dataset)
    selected <- bindings[which(hit), ]
    if (!nrow(selected) || any(is.na(selected$status) | selected$status != "bound")) return(NA_character_)
    unique(selected$selected_path)
  }
  result <- lapply(seq_len(nrow(reads)), function(i) {
    row <- reads[i, ]
    paths <- paths_for(row)
    path <- if (length(paths) == 1L) paths else NA_character_
    producers <- writes[which(writes$dataset == row$dataset & writes$unit_id != row$unit_id), ]
    generated <- any(vapply(seq_len(nrow(producers)), function(j) {
      p <- paths_for(producers[j, ])
      length(p) == 1L && !is.na(path) && !is.na(p) && identical(p, path)
    }, logical(1)))
    status <- if (generated) "generated" else if (is.na(path) || !nzchar(path)) "unresolved" else "missing"
    candidates <- character()
    if (!generated && !is.na(path) && path != "<session work>") {
      member <- sub("^[^.]+\\.", "", row$dataset)
      candidates <- file.path(path, paste0(member, c(".rds", ".sas7bdat", ".xpt")))
      found <- candidates[file.exists(candidates) & !dir.exists(candidates)]
      if (length(found)) status <- "available"
    } else found <- character()
    tibble::tibble(dataset = row$dataset, file = row$file, line = row$line,
                   library_path = path, status = status,
                   path = if (length(found)) found[1L] else NA_character_,
                   searched_paths = list(candidates))
  })
  if (!length(result)) return(tibble::tibble(
    dataset = character(), file = character(), line = integer(), library_path = character(),
    status = character(), path = character(), searched_paths = list()))
  do.call(rbind, result)
}

preflight_unsupported <- function(project) {
  rb <- load_rulebook()
  units <- project$statements
  result <- lapply(unique(units$unit_id), function(uid) {
    us <- units[units$unit_id == uid, ]
    type <- us$unit_type[1L]
    if (!type %in% c("data_step", "proc_step", "macro_def")) return(NULL)
    detail <- tryCatch({
      if (type == "macro_def") "macro_deferred" else {
        out <- deterministic_unit_translation(us, rb)
        if (is.null(out$em)) out$reason else if (is.na(out$em$code[1L]))
          paste(out$em$flags, collapse = ", ") else NULL
      }
    }, error = function(e) conditionMessage(e))
    if (is.null(detail)) return(NULL)
    tibble::tibble(unit_id = uid, file = us$file[1L], line = us$line_start[1L],
                   unit_type = type, reason = detail)
  })
  result <- Filter(Negate(is.null), result)
  if (!length(result)) return(tibble::tibble(unit_id = integer(), file = character(),
                                             line = integer(), unit_type = character(), reason = character()))
  do.call(rbind, result)
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
