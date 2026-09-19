# Reconcile scanner units/files with the existing translation and bundle plans.
# This is static coverage; it never claims the translated code will pass QC.
translation_pipeline_coverage <- function(project, graph, schedule) {
  bundle <- build_bundle_execution_plan(graph)
  nodes <- graph$nodes[graph$nodes$type %in% c("source_unit", "setup", "macro"), , drop = FALSE]
  units <- project$units
  included <- graph$nodes$component_id[graph$nodes$node_id %in%
    graph$edges$from[graph$edges$type == "includes" & graph$edges$resolution == "resolved"]]
  issues <- character()
  rows <- lapply(unique(project$files$file), function(file) {
    u <- units[units$file == file, , drop = FALSE]
    n <- nodes[nodes$source_file == file, , drop = FALSE]
    ids <- unique(n$component_id)
    missing_units <- setdiff(u$unit_id, n$original_index)
    missing_schedule <- setdiff(ids, schedule$component_id)
    role <- vapply(ids, function(id) {
      if (id %in% bundle$execution_order) "main program" else
        if (any(n$type[n$component_id == id] == "setup")) "startup" else
          if (any(n$type[n$component_id == id] == "macro")) "called macro" else
            if (id %in% included) "included by caller" else "unplanned"
    }, "")
    missing <- length(missing_units) || length(missing_schedule) || any(role == "unplanned")
    reason <- if (!nrow(u)) "No active source units (empty/comment-only file or inactive macro definitions)." else
      if (missing) paste0("Missing from pipeline: ", paste(c(
        if (length(missing_units)) paste("source units", paste(missing_units, collapse = ", ")),
        if (length(missing_schedule)) paste("translation components", paste(missing_schedule, collapse = ", ")),
        ids[role == "unplanned"]), collapse = "; ")) else
          "All active source units are scheduled and have an execution role."
    if (missing) issues <<- c(issues, paste(file, reason, sep = ": "))
    tibble::tibble(file = file, components = list(ids), execution_roles = list(unname(role)),
      translation_positions = list(match(ids, schedule$component_id)),
      bundle_positions = list(match(ids, bundle$execution_order)),
      status = if (missing) "unplanned" else if (!nrow(u)) "excluded" else "covered", reason = reason)
  })
  extra <- setdiff(schedule$component_id, nodes$component_id)
  duplicate <- unique(schedule$component_id[duplicated(schedule$component_id)])
  if (length(extra)) issues <- c(issues, paste("Scheduled components without scanned source:", paste(extra, collapse = ", ")))
  if (length(duplicate)) issues <- c(issues, paste("Components scheduled more than once:", paste(duplicate, collapse = ", ")))
  for (id in unique(nodes$component_id)) {
    component <- nodes[nodes$component_id == id, , drop = FALSE]
    files <- unique(component$source_file)
    # Startup files intentionally share the setup component. Other components
    # must not silently combine unrelated physical scripts under one name.
    if (length(files) > 1L && !all(component$type == "setup")) issues <- c(issues,
      paste0("Component ", id, " combines different source files: ", paste(files, collapse = ", "),
        ". Rename the conflicting source file and rescan."))
  }
  cycles <- schedule$component_id[schedule$group_kind == "cycle"]
  if (length(cycles)) issues <- c(issues, paste("Dependency cycle:", paste(cycles, collapse = ", ")))
  sources <- if (length(rows)) do.call(rbind, rows) else tibble::tibble(file = character(),
    components = list(), execution_roles = list(), translation_positions = list(),
    bundle_positions = list(), status = character(), reason = character())
  list(status = if (length(issues)) "invalid" else "complete", sources = sources,
    execution_order = bundle$execution_order, issues = issues)
}

require_complete_pipeline <- function(pipeline) {
  if (!length(pipeline$issues)) return(invisible(NULL))
  cli::cli_abort(c("Cannot translate: preflight found an incomplete or invalid source pipeline.",
    stats::setNames(pipeline$issues, rep("x", length(pipeline$issues))),
    "i" = "Inspect sas_preflight()$pipeline and resolve these findings before translation. No model calls were made."),
    class = "sas2r_pipeline_coverage_error")
}

pipeline_coverage_lines <- function(pipeline) {
  c(sprintf("Preflight pipeline: %s; %d covered, %d intentionally excluded, %d unplanned files.",
    pipeline$status, sum(pipeline$sources$status == "covered"),
    sum(pipeline$sources$status == "excluded"), sum(pipeline$sources$status == "unplanned")),
    paste("Main programs in dependency order:", if (length(pipeline$execution_order))
      paste(pipeline$execution_order, collapse = " -> ") else "(none)"))
}
