# Scanner facts, never observations of runtime records. An unresolved WORK read
# can have a dynamic writer in the declared prefix; this is a possibility only.
component_read_context <- function(graph, component_id) {
  if (is.null(graph$edges) || is.null(graph$nodes)) return(list(reads = list(), possible = character()))
  nodes <- graph$nodes
  edges <- graph$edges
  reads <- edges[edges$type == "reads_dataset" &
    edges$to %in% nodes$node_id[nodes$component_id == component_id], , drop = FALSE]
  order <- graph$execution_order %||% character()
  prefix <- if (component_id %in% order) utils::head(order, match(component_id, order) - 1L) else character()
  possible <- character()
  facts <- lapply(seq_len(nrow(reads)), function(i) {
    read <- reads[i, ]
    writer <- nodes$component_id[match(read$from, nodes$node_id)]
    known <- identical(read$resolution[[1L]], "resolved") && !is.na(writer)
    uncertain_work <- !known && startsWith(tolower(read$detail), "work.")
    if (uncertain_work) possible <<- union(possible, prefix)
    list(dataset = read$detail, source_file = read$source_file, line = read$line,
      writer_status = if (known) "selected_by_source_order" else "unknown",
      writer = if (known) writer else NULL,
      possible_preceding_programs = if (uncertain_work) prefix else character())
  })
  list(reads = facts, possible = possible)
}

source_comparison_context <- function(project, component_id) {
  statements <- component_statements(project, component_id)
  if (is.null(statements)) return(character())
  comparisons <- c("<", "<=", ">", ">=", "=", "^=", "~=", "lt", "le", "gt", "ge", "eq", "ne")
  hits <- vapply(statements$text, function(text) {
    # Only identify statements for source inspection; do not infer grouping
    # across SAS statement syntax or override explicit parentheses.
    tokens <- tryCatch(tokenize_expr(sub(";[[:space:]]*$", "", text)), error = function(e) character())
    sum(tolower(tokens) %in% comparisons) >= 2L
  }, logical(1))
  rows <- statements[hits, , drop = FALSE]
  if (!nrow(rows)) return(character())
  c("Source statements with multiple comparisons: check for SAS implied AND. A < B < C means (A < B) AND (B < C); explicit parentheses may specify different grouping.",
    paste0(rows$file, ":", rows$line_start, ": ", rows$text))
}
