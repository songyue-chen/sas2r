# Deterministic source context, shared by authoring and static review. This is
# not an agent tool and never reads datasets, comparisons, or prior opinions.
agent_guidance_policy <- function() {
  paste(readLines(system.file("prompts", "translation-policy.md", package = "sas2r"),
    warn = FALSE), collapse = "\n")
}

agent_package_facts <- function(allowlist = NULL) {
  allowed <- normalize_package_allowlist(allowlist)
  versions <- vapply(allowed, function(pkg) tryCatch(
    as.character(utils::packageVersion(pkg)), error = function(e) "unknown"), character(1))
  list(r_version = as.character(getRversion()), allowed = allowed, versions = versions)
}

direct_component_dependencies <- function(graph, component_id) {
  if (is.null(graph$nodes) || is.null(graph$edges)) return(character())
  nodes <- graph$nodes
  eligible <- nodes[!nodes$type %in% c("external_input", "final_output", "unresolved_dependency"), ]
  incoming <- graph$edges[graph$edges$to %in% eligible$node_id[eligible$component_id == component_id], ]
  result <- eligible$component_id[match(incoming$from, eligible$node_id)]
  sort(setdiff(unique(result[!is.na(result)]), component_id), method = "radix")
}

build_agent_guidance <- function(project, component_id, contract = NULL,
                                 selected_revisions = list(), graph = project$graph,
                                 body_limit = 6000L, packet_limit = 24000L,
                                 config = project$config %||% list(), priority_dependencies = character()) {
  deps <- direct_component_dependencies(graph, component_id)
  environment <- agent_package_facts(config$allowlist)
  macro <- contract$macro_contract %||% component_macro_contract(project, graph, component_id)
  bodies <- lapply(deps, function(cid) list(
    sas = component_source_text(graph, cid),
    r = selected_revisions[[cid]]$r_code %||% "",
    revision = selected_revisions[[cid]]$revision_id %||% "unavailable",
    symbol = selected_revisions[[cid]]$contract$macro_contract$name %||% sub("^macro__", "", cid)))
  names(bodies) <- deps
  calls <- r_call_names(selected_revisions[[component_id]]$r_code %||% "")
  called <- vapply(bodies, function(b) b$symbol %in% calls, logical(1))
  cited <- deps %in% priority_dependencies | vapply(bodies, function(b)
    b$symbol %in% priority_dependencies, logical(1))
  deps <- deps[order(!cited, !called, seq_along(deps))]
  bodies <- bodies[deps]
  scope <- migration_hash(list(component_id, macro, bodies, environment,
    policy = agent_guidance_policy(), body_limit = body_limit, packet_limit = packet_limit))
  facts <- list()
  add_fact <- function(kind, subject, value) {
    id <- paste0("fact_", substr(migration_hash(list(scope, kind, subject)), 1L, 16L))
    facts[[id]] <<- list(id = id, scope = scope, component_id = component_id,
      kind = kind, subject = subject, value = value)
    id
  }
  text <- c(paste("Source context identity:", scope),
    "Package facts observed in the local execution environment (installation is not semantic support):",
    paste("R:", environment$r_version),
    paste(names(environment$versions), "allowed by mechanical lint; installed version:", environment$versions),
    "Runtime helper signatures, behavior and limits are in the shared authoritative helper reference.")
  params <- macro$parameters
  if (!is.null(params) && nrow(params)) for (i in utils::head(seq_len(nrow(params)), 32L)) {
    if (!identical(params$default_status[i], "unresolved")) next
    id <- add_fact("macro_default", params$name[i], "unresolved_source_expansion")
    text <- c(text, paste(id, "macro_default", params$name[i],
      "needs source expansion/context; an omitted argument is not an established literal default."))
  }
  # Reserve all labels before allocating body text, including labels for
  # dependencies whose bodies no longer fit. They still need explicit status.
  headers <- vapply(deps, function(cid) {
    body <- bodies[[cid]]
    id <- add_fact("dependency_body", body$symbol, "missing_or_truncated")
    paste(id, "dependency_body", body$symbol, "component", cid,
      "selected revision", body$revision)
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
  if (length(deps) > length(included)) text <- c(text, "Additional direct dependencies omitted by packet limit; no behavior is implied.")
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
classify_review_findings <- function(findings, guidance, r_code, contract) {
  exprs <- tryCatch(parse(text = r_code), error = function(e) expression())
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
    fact <- guidance$facts[[f$context_fact_id %||% ""]]
    evidence <- tryCatch(parse(text = f$r_evidence), error = function(e) expression())
    if (!identical(f$category, "missing_context") || is.null(fact) ||
        !identical(fact$scope, guidance$identity) || length(evidence) != 1L ||
        !contains(evidence[[1L]], exprs)) return(f)
    e <- evidence[[1L]]
    relevant <- identical(fact$kind, "dependency_body") &&
      identical(fact$value, "missing_or_truncated") && is.call(e) &&
      identical(e[[1L]], as.name(fact$subject))
    params <- contract$macro_contract$parameters
    if (identical(fact$kind, "macro_default") && is.name(e) &&
        identical(as.character(e), fact$subject) && !is.null(params)) {
      relevant <- any(params$name == fact$subject & params$default_status == "unresolved")
    }
    if (relevant) f$repair_disposition <- "awaiting_context"
    f
  })
}

actionable_review_findings <- function(review) {
  Filter(function(f) !((f$repair_disposition %||% "unverified") %in%
    c("awaiting_context", "source_syntax_claim_only")), review$findings %||% list())
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
