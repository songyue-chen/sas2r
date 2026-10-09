# Deterministic source context and paged code access, shared by authoring and
# static review. Neither reads datasets, comparisons, or prior opinions.
agent_guidance_policy <- function() {
  paste(readLines(system.file("prompts", "translation-policy.md", package = "sas2r"),
    warn = FALSE), collapse = "\n")
}

# Phase changes the investigation's emphasis, never the semantic standard or
# the reviewer's read-only role. Only the executor supplies runtime evidence.
agent_phase_guidance <- function(phase = "program") {
  if (identical(phase, "bundle")) {
    paste("Bundle integration focus. Trace the reported failure through the selected caller/callee or producer/consumer:",
      "argument and return shapes, names, lookup, state lifetime, execution order, intermediates and required artifact content.",
      "Use only the supplied bounded executor diagnostics and source/code facts; do not execute or open runtime data.",
      "A changed candidate still requires full semantic review unless this request explicitly says focused review.",
      "An input mismatch or source-required failure is not permission to change source rules.")
  } else {
    paste("Component translation focus. Check source calculations, filters, merges, return values and side effects.",
      "Identify the causal defect in this component; affected outputs may include downstream consumers, even for return-only macros.",
      "Preserve source-visible effects while accepting harmless SAS/R representation differences under the shared policy.")
  }
}

agent_package_facts <- function(allowlist = NULL) {
  allowed <- normalize_package_allowlist(allowlist)
  versions <- vapply(allowed, function(pkg) tryCatch(
    as.character(utils::packageVersion(pkg)), error = function(e) "unknown"), character(1))
  list(r_version = as.character(getRversion()), allowed = allowed, versions = versions)
}

direct_component_dependencies <- function(graph, component_id, downstream = FALSE) {
  if (is.null(graph$nodes) || is.null(graph$edges)) return(character())
  nodes <- graph$nodes
  eligible <- nodes[!nodes$type %in% c("external_input", "final_output", "unresolved_dependency"), ]
  from <- if (downstream) "to" else "from"
  to <- if (downstream) "from" else "to"
  incoming <- graph$edges[graph$edges$type != "execution_before" &
    graph$edges[[to]] %in% eligible$node_id[eligible$component_id == component_id], ]
  result <- eligible$component_id[match(incoming[[from]], eligible$node_id)]
  sort(setdiff(unique(result[!is.na(result)]), component_id), method = "radix")
}

# Follow semantic relationships in one direction. Execution-order edges only
# schedule programs; they do not make every earlier program relevant code.
context_component_dependencies <- function(graph, component_id, downstream = FALSE) {
  seen <- component_id
  pending <- component_id
  while (length(pending)) {
    next_ids <- unique(unlist(lapply(pending, function(cid)
      direct_component_dependencies(graph, cid, downstream)), use.names = FALSE))
    pending <- setdiff(next_ids, seen)
    seen <- c(seen, pending)
  }
  sort(setdiff(seen, component_id), method = "radix")
}

# Code-only context shared by all three roles. Neighbour identifiers come from
# the graph; R bodies come from the selected revision snapshot, never outputs.
agent_dependency_bodies <- function(project, component_id, selected_revisions = list(),
                                    graph = project$graph, requested = NULL) {
  ids <- unique(c(context_component_dependencies(graph, component_id),
    context_component_dependencies(graph, component_id, downstream = TRUE)))
  possible <- component_read_context(graph, component_id)$possible
  ids <- unique(c(ids, possible, unlist(lapply(possible, function(cid)
    context_component_dependencies(graph, cid)), use.names = FALSE)))
  ids <- setdiff(ids, component_id)
  if (!is.null(requested)) ids <- intersect(ids, requested)
  stats::setNames(lapply(ids, function(cid) list(
    sas = component_source_text(graph, cid),
    r = selected_revisions[[cid]]$r_code %||% "",
    revision = selected_revisions[[cid]]$revision_id %||% "unavailable",
    execution_note = if (any(graph$nodes$component_id == cid & graph$nodes$type == "setup")) paste(
      "Startup source context only: selected setup R code is not executed as a program.",
      "Startup library bindings and compiled .sas2r_formats are loaded by autoexec.R.",
      "Use sas_put(x, 'name.') for compiled formats; bindings invented in setup R are unavailable."),
    symbol = selected_revisions[[cid]]$contract$macro_contract$name %||% sub("^macro__", "", cid))), ids)
}

read_dependency_context <- function(ctx, component_id, language, offset = 1L) {
  bodies <- agent_dependency_bodies(ctx$project, ctx$component_id,
    ctx$selected_revisions %||% list(), ctx$graph %||% ctx$project$graph,
    requested = component_id)
  body <- bodies[[component_id]]
  if (is.null(body)) return(list(error = "not_a_related_dependency_or_consumer"))
  code <- body[[language]]
  size <- nchar(code)
  end <- min(size, offset + 11999L)
  list(component_id = component_id, revision_id = body$revision, language = language,
    execution_note = body$execution_note,
    status = if (size) "available" else "unavailable", total_characters = size,
    offset = offset, next_offset = if (end < size) end + 1L else NULL,
    code = if (offset <= size) substr(code, offset, end) else "")
}

build_agent_guidance <- function(project, component_id, contract = NULL,
                                 selected_revisions = list(), graph = project$graph,
                                 body_limit = 6000L, packet_limit = 24000L,
                                 config = project$config %||% list(), priority_dependencies = character(),
                                 include_consumers = FALSE) {
  deps <- context_component_dependencies(graph, component_id)
  read_context <- component_read_context(graph, component_id)
  source_comparisons <- source_comparison_context(project, component_id)
  consumers <- if (isTRUE(include_consumers)) context_component_dependencies(graph, component_id, downstream = TRUE) else character()
  deps <- unique(c(deps, consumers, read_context$possible))
  environment <- agent_package_facts(config$allowlist)
  projections <- source_projection_context(project, component_id)
  while (length(projections) && nchar(paste(render_source_projections(projections), collapse = "\n")) >
      min(6000L, floor(packet_limit / 3L))) projections <- utils::head(projections, -1L)
  macro <- contract$macro_contract %||% component_macro_contract(project, graph, component_id)
  available_bodies <- agent_dependency_bodies(project, component_id, selected_revisions, graph)
  additional_consumers <- setdiff(names(available_bodies), deps)
  bodies <- available_bodies[deps]
  calls <- r_call_names(selected_revisions[[component_id]]$r_code %||% "")
  called <- vapply(bodies, function(b) b$symbol %in% calls, logical(1))
  cited <- deps %in% priority_dependencies | vapply(bodies, function(b)
    b$symbol %in% priority_dependencies, logical(1))
  deps <- deps[order(!cited, !called, !deps %in% read_context$possible,
    match(deps, read_context$possible, nomatch = length(deps) + 1L), seq_along(deps))]
  bodies <- bodies[deps]
  scope <- migration_hash(list(component_id, macro, available_bodies, environment, projections, consumers,
    reads = read_context, source_comparisons = source_comparisons,
    execution_order = graph$execution_order, policy = agent_guidance_policy(), body_limit = body_limit, packet_limit = packet_limit))
  facts <- list()
  add_fact <- function(kind, subject, value) {
    id <- paste0("fact_", substr(migration_hash(list(scope, kind, subject)), 1L, 16L))
    facts[[id]] <<- list(id = id, scope = scope, component_id = component_id,
      kind = kind, subject = subject, value = value)
    id
  }
  versions <- environment$versions
  text <- c(paste("Source context identity:", scope),
    paste0("Allowlisted packages with observed versions (lint allows them; installation is not semantic support): R ",
      environment$r_version, "; ", paste(names(versions),
        ifelse(versions == "unknown", "not installed", versions), collapse = ", "), "."),
    "Runtime helper signatures, behavior and limits are in the shared authoritative helper reference.",
    "For truncated code, use read_dependency_context(component_id, language = sas or r, offset = 1), then next_offset. Omitted code is not missing source. Related transitive dependencies and consumers are readable; execution-only predecessors are excluded.",
    if (length(additional_consumers)) paste("Additional related code IDs:", paste(utils::head(additional_consumers, 32L), collapse = ", ")),
    render_source_projections(projections),
    "Source/order summaries below are bounded excerpts; omitted facts remain unknown.",
    paste("Declared execution order:", substr(paste(graph$execution_order %||% character(), collapse = " -> "), 1L, 2000L)),
    "WORK reads use the latest preceding write. Selected writers below are static source/order facts; unknown writers and possible preceding programs do not establish runtime provenance.",
    substr(as.character(jsonlite::toJSON(read_context$reads, auto_unbox = TRUE, null = "null")), 1L, 4000L),
    substr(paste(source_comparisons, collapse = "\n"), 1L, 2000L))
  params <- macro$parameters
  if (!is.null(params) && nrow(params)) for (i in utils::head(seq_len(nrow(params)), 32L)) {
    if (!identical(params$default_status[i], "unresolved")) next
    id <- add_fact("macro_default", params$name[i], "unresolved_source_expansion")
    text <- c(text, paste(id, "macro_default", params$name[i],
      "needs source expansion/context; an omitted argument is not an established literal default."))
  }
  # New source/order facts share the existing packet ceiling. Reserve room for
  # dependency labels and the omission notice even for a small requested packet.
  intro <- paste(text, collapse = "\n")
  intro_limit <- max(0L, packet_limit - 200L)
  if (nchar(intro) > intro_limit) {
    notice <- "\n[Source context truncated; omitted facts remain unknown. Use paged code retrieval.]"
    text <- substr(paste0(substr(intro, 1L, max(0L, intro_limit - nchar(notice))), notice), 1L, intro_limit)
    facts <- Filter(function(f) grepl(f$id, text, fixed = TRUE), facts)
  }
  # Reserve all labels before allocating body text, including labels for
  # dependencies whose bodies no longer fit. They still need explicit status.
  headers <- vapply(deps, function(cid) {
    body <- bodies[[cid]]
    id <- add_fact("dependency_body", body$symbol, "missing_or_truncated")
    paste(id, "dependency_body", body$symbol, "component", cid,
      "selected revision", body$revision,
      "available characters: sas", nchar(body$sas), "r", nchar(body$r),
      if (cid %in% read_context$possible) "possible preceding writer (unconfirmed)" else
        if (cid %in% consumers) "downstream caller/consumer" else "upstream dependency",
      body$execution_note %||% "")
  }, character(1))
  label_budget <- max(0L, packet_limit - nchar(paste(text, collapse = "\n")) - 150L)
  included <- deps[cumsum(nchar(headers) + 80L) <= label_budget]
  headers <- headers[included]
  visible_symbols <- vapply(bodies[included], `[[`, "", "symbol")
  facts <- Filter(function(f) f$kind != "dependency_body" || f$subject %in% visible_symbols, facts)
  remaining <- max(0L, label_budget - sum(nchar(headers)) - 80L * length(included))
  # The existing closure signatures remain available in ordinary role context.
  for (cid in included) {
    body <- bodies[[cid]]
    pieces <- character()
    complete <- TRUE
    need <- pmin(vapply(body[c("sas", "r")], nchar, integer(1)), body_limit)
    allocation <- pmin(need, floor(remaining / 2L))
    extra <- max(0L, remaining - sum(allocation))
    for (language in c("sas", "r")) {
      add <- min(extra, need[[language]] - allocation[[language]])
      allocation[[language]] <- allocation[[language]] + add
      extra <- extra - add
    }
    for (language in c("sas", "r")) {
      original <- body[[language]]
      size <- allocation[[language]]
      status <- if (!nzchar(original)) "missing" else if (nchar(original) > size) "truncated" else "complete"
      if (language == "r") complete <- identical(status, "complete")
      pieces <- c(pieces, paste(language, status, "\n", substr(original, 1L, size)))
      remaining <- max(0L, remaining - min(nchar(original), size))
    }
    add_fact("dependency_body", body$symbol, if (complete) "complete" else "missing_or_truncated")
    text <- c(text, headers[[cid]], pieces)
  }
  if (length(deps) > length(included)) text <- c(text, "Additional dependencies omitted by packet limit; no behavior is implied.")
  identity <- migration_hash(list(scope, allocation_policy = "paired-v1", selected = included))
  text <- sub(scope, identity, text, fixed = TRUE)
  facts <- lapply(facts, function(f) { f$scope <- identity; f })
  list(text = paste(text, collapse = "\n"), facts = facts, identity = identity,
    selected_dependencies = included, allocation_policy = "paired-v1")
}

# A disappearance is an observation about source-code symbols, including aliases
# used as values. It is never a gate or an assertion that behavior was lost.
dependency_symbol_notices <- function(before, after, dependencies) {
  symbols <- function(code) tryCatch(all.names(parse(text = code), unique = TRUE),
    error = function(e) character())
  missing <- setdiff(intersect(symbols(before), dependencies), symbols(after))
  if (!length(missing)) return(character())
  utils::head(paste("Dependency symbol reference no longer seen:", missing,
    "(advisory only; dynamic/equivalent use is not resolved and no defect is established)."), 10L)
}

# Only two narrow missing-context requests are recognized. The R evidence must
# be exactly the cited parameter symbol or one direct call, present in this R.
# A label or an unrelated fact identifier cannot excuse a filter/derivation.
classify_review_findings <- function(findings, guidance, r_code, contract, available = character()) {
  exprs <- tryCatch(parse(text = r_code), error = function(e) expression())
  available <- tolower(available)
  available <- unique(c(available, sub("^macro__", "", available[startsWith(available, "macro__")])))
  contains <- function(target, x) {
    if (identical(target, x)) return(TRUE)
    if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return(FALSE)
    any(vapply(seq_along(x), function(i) {
      if (identical(x[[i]], quote(expr = ))) FALSE else contains(target, x[[i]])
    }, logical(1)))
  }
  lapply(findings, function(f) {
    f$category <- f$category %||% "unknown"
    f$repair_disposition <- "unverified"
    if (identical(f$category, "source_syntax_claim")) {
      f$repair_disposition <- "source_syntax_claim_only"
      return(f)
    }
    if (!identical(f$category, "missing_context")) return(f)
    # Context that only names components translated in this run is available:
    # the reviewer can retrieve it and no fixer round for this component can
    # supply it. The verdict stands for human review; it is not a repair item.
    named <- tolower(sub("^%", "", trimws(as.character(unlist(f$unresolved_dependencies %||% character())))))
    named <- named[nzchar(named)]
    context_available <- length(named) > 0L && all(named %in% available)
    fact <- guidance$facts[[f$context_fact_id %||% ""]]
    evidence <- tryCatch(parse(text = f$r_evidence), error = function(e) expression())
    if (is.null(fact) || !identical(fact$scope, guidance$identity) || length(evidence) != 1L ||
        !contains(evidence[[1L]], exprs)) {
      if (context_available) f$repair_disposition <- "context_available"
      return(f)
    }
    e <- evidence[[1L]]
    relevant <- identical(fact$kind, "dependency_body") &&
      identical(fact$value, "missing_or_truncated") && is.call(e) &&
      identical(e[[1L]], as.name(fact$subject))
    params <- contract$macro_contract$parameters
    if (identical(fact$kind, "macro_default") && is.name(e) &&
        identical(as.character(e), fact$subject) && !is.null(params)) {
      relevant <- any(params$name == fact$subject & params$default_status == "unresolved")
    }
    if (relevant) f$repair_disposition <- "awaiting_context" else
      if (context_available) f$repair_disposition <- "context_available"
    f
  })
}

actionable_review_findings <- function(review) {
  Filter(function(f) !((f$repair_disposition %||% "unverified") %in%
    c("awaiting_context", "source_syntax_claim_only", "context_available")), review$findings %||% list())
}

program_review_needs_repair <- function(review) {
  identical(review$verdict, "repair_required") &&
    (!length(review$findings) || length(actionable_review_findings(review)) > 0L)
}

# Only exact dependency identifiers requested by the last review; no free-text
# resolver or inference from a model's prose.
review_context_dependencies <- function(history) {
  events <- current_component_evidence(history)$events %||% list()
  reviews <- Filter(function(e) e$type %in% c("review_completed", "review_unavailable"), events)
  if (!length(reviews)) return(character())
  as.character(unlist(utils::tail(reviews, 1L)[[1L]]$unresolved_dependencies %||% character()))
}
