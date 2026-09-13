# Discover the static call closure using the existing project/autocall precedence.
# Reading a library file does not make every definition in it a translation unit.
discover_called_macros <- function(statements, defs, config, root) {
  source <- statements[statements$origin != "macro_search_path", , drop = FALSE]
  local_defs <- defs[defs$file %in% source$file, , drop = FALSE]
  pending <- extract_macro_calls(source)
  visited <- character()
  parsed <- list()
  selected <- list()
  findings <- list()
  while (nrow(pending)) {
    pending <- pending[!pending$name %in% visited & !duplicated(pending$name), , drop = FALSE]
    if (!nrow(pending)) break
    visited <- c(visited, pending$name)
    resolved <- resolve_macro_calls(pending, local_defs, config, root)
    pending <- empty_macro_calls()
    for (i in which(resolved$status %in% c("resolved_path", "resolved_content"))) {
      file <- include_normalize_path(resolved$source[i])
      name <- resolved$name[i]
      if (is.null(parsed[[file]])) {
        units <- sas_units(sas_statements(paste(readLines(file, warn = FALSE), collapse = "\n")))
        units$file <- rep(file, nrow(units))
        parsed[[file]] <- list(units = units, defs = extract_macro_defs(units))
      }
      contents <- parsed[[file]]
      definition <- contents$defs[contents$defs$name == name, , drop = FALSE]
      body <- contents$units[contents$units$unit_id %in% definition$unit_id, , drop = FALSE]
      reason <- if (nrow(definition) != 1L) {
        "macro_definition_missing"
      } else if (sum(contents$defs$unit_id == definition$unit_id) != 1L) {
        "macro_nested_definition_unsupported"
      } else if (any(contents$units$type == "code" & contents$units$unit_type != "macro_def")) {
        "macro_library_initialization_unsupported"
      } else if (nrow(extract_includes(body))) {
        "macro_include_requires_expansion"
      } else NULL
      if (!is.null(reason)) {
        findings[[length(findings) + 1L]] <- tibble::tibble(
          kind = reason, detail = paste0(name, ": ", file), name = name)
        next
      }
      selected[[length(selected) + 1L]] <- definition
      pending <- fast_bind(list(pending, extract_macro_calls(body)), empty_macro_calls())
    }
  }
  list(defs = fast_bind(selected, extract_macro_defs(statements[0L, ])),
       findings = fast_bind(findings, tibble::tibble(kind = character(), detail = character(), name = character())))
}

# One mapping for scheduling, agent context, artifacts and preflight reporting.
called_macro_units <- function(project) {
  defs <- project$macros$defs %||% project$dependency_facts$macros$defs %||%
    tibble::tibble(name = character(), params = character(), line_start = integer(),
                   line_end = integer(), file = character(), unit_id = integer())
  units <- project$units
  defs <- defs[defs$unit_id %in% units$unit_id[units$origin == "macro_search_path"], , drop = FALSE]
  defs$component_id <- if (nrow(defs)) paste0("macro__", defs$name) else character()
  defs$staged_file <- if (nrow(defs)) paste0("R/macros/", defs$name, ".R") else character()
  defs$test_file <- if (nrow(defs)) paste0("tests_macros/test-", defs$name, ".R") else character()
  defs
}
