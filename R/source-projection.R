# Narrow negative source facts, not a schema inference engine. Unsupported
# syntax or binding ambiguity leaves the fact unknown. No datasets are read.
source_output_projection <- function(project, dataset) {
  stmts <- project$statements
  lineage <- project$lineage
  graph <- project$graph
  if (is.null(stmts) || is.null(lineage) || is.null(graph)) return(NULL)
  dataset <- tolower(dataset)
  code <- stmts[stmts$type == "code", ]
  writers <- unique(lineage$unit_id[lineage$role == "creates" & lineage$dataset == dataset])
  if (length(writers) != 1L) return(NULL)
  unit <- code[code$unit_id == writers, ]
  if (!nrow(unit) || !all(unit$unit_type == "data_step")) return(NULL)
  # Literal macro setup outside the writer cannot generate another DATA step.
  # Still reject expansion anywhere: e.g. DATA &target could overwrite this
  # output without appearing under its literal name in the lineage.
  text <- vapply(code$text, mask_strings, "", keep_double = TRUE)
  setup <- code$first_token %in% c("%let", "%put", "%global", "%local", "%symdel") &
    code$unit_id != writers
  text[setup] <- sub("^\\s*%(let|put|global|local|symdel)\\b", "", text[setup],
    ignore.case = TRUE, perl = TRUE)
  if (any(code$first_token == "rename") || any(grepl("[%&]", text))) return(NULL)
  edges <- graph$edges
  writes <- edges[edges$type == "writes_dataset" & edges$detail == dataset, ]
  if (nrow(writes) != 1L || !identical(writes$resolution, "resolved")) return(NULL)
  lib <- split_ds(dataset)[["lib"]]
  # Reuse the library parser; only unrelated input-library declarations are
  # harmless here. Output assignments/clears and unsupported forms stay unknown.
  libraries <- extract_librefs(code)
  written_libs <- vapply(lineage$dataset[lineage$role == "creates"],
    function(x) split_ds(x)[["lib"]], "")
  if (nrow(libraries) != sum(code$first_token == "libname") ||
      any(libraries$libref %in% c(lib, "_all_", written_libs))) return(NULL)
  binding <- lineage[lineage$unit_id == writers & lineage$role == "creates", ]
  if (lib != "work" && (!"binding_status" %in% names(binding) ||
      any(is.na(binding$binding_status) | binding$binding_status != "resolved"))) return(NULL)
  ir <- parse_data_step(unit)
  kinds <- vapply(ir$steps, `[[`, "", "kind")
  if (nrow(ir$blockers) || length(ir$outputs) != 1L ||
      dataset %in% ir$inputs || any(kinds == "rename")) return(NULL)
  projections <- Filter(function(s) s$kind %in% c("keep", "drop"), ir$steps)
  if (length(projections) != 1L) return(NULL)
  projection <- projections[[1L]]
  vars <- projection$vars
  if (!length(vars) || length(vars) > 32L || any(nchar(vars) > 32L) || any(!grepl("^[a-z_][a-z0-9_]*$", vars)) ||
      any(vars %in% c("_all_", "_numeric_", "_character_"))) return(NULL)
  cid <- graph$nodes$component_id[match(writes$from, graph$nodes$node_id)]
  list(kind = "source_output_excludes_column", component_id = cid,
    unit_id = writers, stmt_id = projection$stmt_id, dataset = dataset,
    declaration = unit$text[unit$stmt_id == projection$stmt_id],
    operation = projection$kind, columns = vars)
}

source_projection_context <- function(project, component_id) {
  statements <- component_statements(project, component_id)
  lineage <- project$lineage
  reads <- unique(lineage$dataset[lineage$role == "reads" & lineage$unit_id %in% statements$unit_id])
  facts <- lapply(utils::head(sort(reads), 8L), function(key) source_output_projection(project, key))
  Filter(Negate(is.null), facts)
}

render_source_projections <- function(facts) {
  if (!length(facts)) return(character())
  c("Conservative source output exclusions (not proof of a runtime cause or populated columns):",
    vapply(facts, function(f) paste(f$kind, f$dataset, "producer", f$component_id,
      "unit", f$unit_id, "statement", f$stmt_id, paste0("[", f$declaration, "]"),
      if (f$operation == "keep") "excludes every ordinary column outside:" else "excludes these columns:",
      paste(f$columns, collapse = ", ")), ""),
    "Check whether the consumer SAS actually requires the reported column. An R typo or invented guard remains repairable.",
    "KEEP inclusion does not establish that a column is created or populated. Unknown projections prove nothing.",
    "These facts do not clear failures or authorize changed filters, fabricated values, or input substitution.")
}
