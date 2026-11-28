# SAS defines descriptor order by first occurrence. Support only literal LENGTH
# groups followed by one plain SET; defer options, other declarations and rewrites.
# https://service.sas.com/csm/en/how-to-reorder-the-variables-in-a-sas-data-set?id=kb_article_view&sysparm_article=KB0036123
source_column_order_specs <- function(project, component_id) {
  stmts <- component_statements(project, component_id)
  if (is.null(stmts) || !nrow(stmts)) return(list())
  if (any(stmts$first_token == "libname")) return(list())
  units <- split(stmts, factor(stmts$unit_id, levels = unique(stmts$unit_id)))
  lineage <- project$lineage
  writes <- lineage$dataset[lineage$role == "creates"]
  repeated <- writes[duplicated(writes)]
  rules <- lapply(units, function(unit) {
    if (unit$unit_type[[1L]] != "data_step") return(NULL)
    text <- tolower(trimws(sub(";\\s*$", "", unit$text)))
    set <- which(grepl("^set\\s", text))
    if (length(set) != 1L || set < 3L) return(NULL)
    prefix <- text[seq.int(2L, set - 1L)]
    if (!all(grepl("^length\\s", prefix))) return(NULL)
    if (!grepl("^set\\s+[a-z_][a-z0-9_]*(\\.[a-z_][a-z0-9_]*)?$", text[set])) return(NULL)
    # Output options and later declarations/projections can change the schema.
    if (!grepl("^data\\s+[a-z_][a-z0-9_]*(\\.[a-z_][a-z0-9_]*)?$", text[1L]) ||
        any(grepl("^(keep|drop|rename|attrib|retain|array|format|informat|length)\\b", text[-seq_len(set)])) ||
        any(grepl("[&%]", text))) return(NULL)
    columns <- character()
    group <- "^([a-z_][a-z0-9_]*(?:\\s+[a-z_][a-z0-9_]*)*)\\s+\\$?\\s*[0-9]+(?:\\s+|$)"
    for (declaration in prefix) {
      rest <- sub("^length\\s+", "", declaration)
      while (nzchar(rest)) {
        match <- regexec(group, rest, perl = TRUE)
        parts <- regmatches(rest, match)[[1L]]
        if (!length(parts)) return(NULL)
        columns <- c(columns, strsplit(parts[2L], "\\s+")[[1L]])
        rest <- trimws(substring(rest, nchar(parts[1L]) + 1L))
      }
    }
    ir <- parse_data_step(unit)
    if (length(ir$outputs) != 1L || length(ir$inputs) != 1L ||
        any(ir$outputs %in% c(repeated, ir$inputs))) return(NULL)
    list(status = "supported", reason = "source_column_order", route = "column_order",
      unit_id = unit$unit_id[[1L]], outputs = ir$outputs, inputs = ir$inputs,
      prefix = unique(columns))
  })
  Filter(Negate(is.null), rules)
}
