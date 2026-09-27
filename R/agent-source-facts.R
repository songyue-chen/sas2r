# Scanner facts, never observations of runtime records. An unresolved WORK read
# can have a dynamic writer in the declared prefix; this is a possibility only.
component_read_context <- function(graph, component_id) {
  if (!is.null(graph$dataset_reads)) {
    facts <- Filter(function(x) identical(x$component_id, component_id), graph$dataset_reads)
    facts <- lapply(facts, function(x) { x$component_id <- NULL; x })
    return(list(reads = facts, possible = unique(unlist(lapply(facts, function(x)
      x$possible_preceding_programs), use.names = FALSE)) %||% character()))
  }
  if (is.null(graph$edges) || is.null(graph$nodes)) return(list(reads = list(), possible = character()))
  nodes <- graph$nodes
  edges <- graph$edges
  reads <- edges[edges$type == "reads_dataset" &
    edges$to %in% nodes$node_id[nodes$component_id == component_id], , drop = FALSE]
  possible <- character()
  facts <- lapply(seq_len(nrow(reads)), function(i) {
    read <- reads[i, ]
    writer <- nodes$component_id[match(read$from, nodes$node_id)]
    known <- identical(read$resolution[[1L]], "resolved") && !is.na(writer)
    list(dataset = read$detail, source_file = read$source_file, line = read$line,
      writer_status = if (known) "selected_by_source_order" else "unknown",
      writer = if (known) writer else NULL,
      possible_preceding_programs = character())
  })
  list(reads = facts, possible = possible)
}

source_comparison_context <- function(project, component_id) {
  statements <- component_statements(project, component_id)
  if (is.null(statements)) return(character())
  operators <- load_rulebook()$operators
  hits <- vapply(statements$text, function(text) {
    tokens <- tryCatch(tokenize_expr(sub(";[[:space:]]*$", "", text)), error = function(e) character())
    # Separate conditions/actions and remove assignment LHSs. Use the same
    # grouping and chain detector as translation, including deferred chains.
    pieces <- split(tokens, cumsum(tolower(tokens) %in% c("then", "else")))
    found <- FALSE
    for (part in pieces) {
      condition <- any(tolower(utils::head(part, 2L)) %in% c("if", "where"))
      while (length(part) && tolower(part[1L]) %in% c("if", "where", "then", "else"))
        part <- part[-1L]
      if (!condition && length(part) > 2L && part[2L] == "=" && grepl("^[A-Za-z_][A-Za-z0-9_]*$", part[1L]))
        part <- part[-c(1L, 2L)]
      replace <- tolower(part) %in% names(operators)
      part[replace] <- unlist(operators[tolower(part[replace])], use.names = FALSE)
      tryCatch(expand_comparison_chains(part, function() { found <<- TRUE }),
        error = function(e) NULL)
    }
    found
  }, logical(1))
  rows <- statements[hits, , drop = FALSE]
  if (!nrow(rows)) return(character())
  c("Source comparison chains: check SAS implied AND. A < B < C means (A < B) AND (B < C); unsupported chain forms need source review.",
    paste0(rows$file, ":", rows$line_start, ": ", rows$text))
}
