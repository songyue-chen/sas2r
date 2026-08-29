.semantic_registry_env <- new.env(parent = emptyenv())

semantic_registry_abort <- function(message) {
  cli::cli_abort(message, class = "sas2r_semantic_registry_error")
}

semantic_registry_scalar <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

semantic_registry_date <- function(x) {
  if (!semantic_registry_scalar(x) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    return(FALSE)
  }
  parsed <- suppressWarnings(tryCatch(as.Date(x), error = function(e) NA))
  length(parsed) == 1L && !is.na(parsed) && identical(format(parsed), x)
}

semantic_executable_rule_ids <- function() {
  if (!is.null(.semantic_registry_env$executable_ids)) {
    return(.semantic_registry_env$executable_ids)
  }
  dir <- system.file("rulebook", package = "sas2r")
  functions <- yaml::read_yaml(
    file.path(dir, "functions.yml"), eval.expr = FALSE
  )
  operators <- yaml::read_yaml(
    file.path(dir, "operators.yml"), eval.expr = FALSE
  )
  procs <- yaml::read_yaml(file.path(dir, "procs.yml"), eval.expr = FALSE)
  proc_names <- names(Filter(
    function(entry) entry$status %in% c("supported", "partial"),
    procs
  ))
  ids <- c(
    paste0("functions.", names(functions)),
    paste0("operators.", names(operators)),
    paste0("procs.", proc_names)
  )
  .semantic_registry_env$executable_ids <- ids
  ids
}

validate_semantic_source <- function(source, owner, index) {
  label <- sprintf("%s source %d", owner, index)
  if (!is.list(source) || is.null(names(source)) || any(!nzchar(names(source)))) {
    semantic_registry_abort(sprintf("%s must be a named mapping.", label))
  }
  if (anyDuplicated(names(source))) {
    semantic_registry_abort(sprintf("%s has a duplicate field.", label))
  }
  required <- c("url", "accessed")
  allowed <- c(required, "version")
  missing <- setdiff(required, names(source))
  unknown <- setdiff(names(source), allowed)
  if (length(missing)) {
    semantic_registry_abort(sprintf(
      "%s is missing field(s): %s.", label, paste(missing, collapse = ", ")
    ))
  }
  if (length(unknown)) {
    semantic_registry_abort(sprintf(
      "%s has unknown field(s): %s.", label, paste(unknown, collapse = ", ")
    ))
  }
  if (!semantic_registry_scalar(source$url) ||
      !grepl("^https://[^[:space:]]+$", source$url)) {
    semantic_registry_abort(sprintf("%s url must be a non-empty HTTPS URL.", label))
  }
  if (!semantic_registry_date(source$accessed)) {
    semantic_registry_abort(sprintf(
      "%s accessed must be a valid YYYY-MM-DD date.", label
    ))
  }
  if (!is.null(source$version) && !semantic_registry_scalar(source$version)) {
    semantic_registry_abort(sprintf("%s version must be a non-empty string.", label))
  }
  invisible(source)
}

validate_semantic_entry <- function(entry, id) {
  if (!is.list(entry) || is.null(names(entry)) || any(!nzchar(names(entry)))) {
    semantic_registry_abort(sprintf("Semantic entry %s must be a named mapping.", id))
  }
  if (anyDuplicated(names(entry))) {
    semantic_registry_abort(sprintf("Semantic entry %s has a duplicate field.", id))
  }
  required <- c(
    "classification", "sas_default", "r_default", "strategy",
    "implementation_status", "risk", "scope", "sources", "edge_cases"
  )
  missing <- setdiff(required, names(entry))
  unknown <- setdiff(names(entry), required)
  if (length(missing)) {
    semantic_registry_abort(sprintf(
      "Semantic entry %s is missing field(s): %s.",
      id, paste(missing, collapse = ", ")
    ))
  }
  if (length(unknown)) {
    semantic_registry_abort(sprintf(
      "Semantic entry %s has unknown field(s): %s.",
      id, paste(unknown, collapse = ", ")
    ))
  }

  enums <- list(
    classification = c("equivalent", "adapter_required", "known_divergence", "unsupported"),
    implementation_status = c("verified", "implemented_unverified", "deferred"),
    risk = c("low", "medium", "high")
  )
  for (field in names(enums)) {
    if (!semantic_registry_scalar(entry[[field]]) ||
        !entry[[field]] %in% enums[[field]]) {
      semantic_registry_abort(sprintf(
        "Semantic entry %s has invalid %s.", id, field
      ))
    }
  }
  if (identical(entry$implementation_status, "verified")) {
    semantic_registry_abort(sprintf(
      paste0(
        "Semantic entry %s cannot be verified in schema version 1 ",
        "without machine-checkable fixture provenance."
      ),
      id
    ))
  }
  for (field in c("sas_default", "r_default", "strategy", "scope")) {
    if (!semantic_registry_scalar(entry[[field]])) {
      semantic_registry_abort(sprintf(
        "Semantic entry %s field %s must be a non-empty string.", id, field
      ))
    }
  }
  if (!is.list(entry$sources) || length(entry$sources) == 0L) {
    semantic_registry_abort(sprintf(
      "Semantic entry %s sources must be a non-empty list.", id
    ))
  }
  for (index in seq_along(entry$sources)) {
    validate_semantic_source(entry$sources[[index]], id, index)
  }
  if (!is.character(entry$edge_cases) || length(entry$edge_cases) == 0L ||
      anyNA(entry$edge_cases) || any(!nzchar(entry$edge_cases)) ||
      any(!grepl("^[a-z][a-z0-9_]*$", entry$edge_cases)) ||
      anyDuplicated(entry$edge_cases)) {
    semantic_registry_abort(sprintf(
      "Semantic entry %s edge_cases must be unique non-empty identifiers.", id
    ))
  }
  if (identical(entry$classification, "unsupported") &&
      !identical(entry$implementation_status, "deferred")) {
    semantic_registry_abort(sprintf(
      "Unsupported semantic entry %s must be deferred.", id
    ))
  }
  invisible(entry)
}

validate_semantic_registry <- function(registry, enforce_surface = TRUE) {
  if (!is.list(registry) || is.null(names(registry))) {
    semantic_registry_abort("Semantic registry must be a named mapping.")
  }
  if (anyDuplicated(names(registry))) {
    semantic_registry_abort("Semantic registry has a duplicate field.")
  }
  required <- c("schema_version", "rules", "known_domains")
  missing <- setdiff(required, names(registry))
  unknown <- setdiff(names(registry), required)
  if (length(missing)) {
    semantic_registry_abort(sprintf(
      "Semantic registry is missing field(s): %s.", paste(missing, collapse = ", ")
    ))
  }
  if (length(unknown)) {
    semantic_registry_abort(sprintf(
      "Semantic registry has unknown field(s): %s.", paste(unknown, collapse = ", ")
    ))
  }
  if (!is.integer(registry$schema_version) ||
      !identical(registry$schema_version, 1L)) {
    semantic_registry_abort("Semantic registry schema_version must be integer 1.")
  }
  for (field in c("rules", "known_domains")) {
    entries <- registry[[field]]
    if (!is.list(entries) || length(entries) == 0L || is.null(names(entries)) ||
        any(!nzchar(names(entries))) || anyDuplicated(names(entries))) {
      semantic_registry_abort(sprintf(
        "Semantic registry %s must be a non-empty uniquely named mapping.", field
      ))
    }
  }
  rule_ids <- names(registry$rules)
  valid_rule_ids <- vapply(rule_ids, function(id) {
    if (!identical(id, tolower(id))) return(FALSE)
    if (startsWith(id, "functions.")) {
      return(grepl("^[a-z][a-z0-9_]*$", sub("^functions\\.", "", id)))
    }
    if (startsWith(id, "operators.")) {
      name <- sub("^operators\\.", "", id)
      return(grepl("^[a-z][a-z0-9_]*$", name) ||
               name %in% c("=", "^=", "~=", "||", "**"))
    }
    if (startsWith(id, "procs.")) {
      return(grepl("^[a-z][a-z0-9_]*$", sub("^procs\\.", "", id)))
    }
    FALSE
  }, logical(1))
  if (!all(valid_rule_ids)) {
    semantic_registry_abort(sprintf(
      "Semantic registry has invalid rule id(s): %s.",
      paste(rule_ids[!valid_rule_ids], collapse = ", ")
    ))
  }
  if (isTRUE(enforce_surface)) {
    absent <- setdiff(rule_ids, semantic_executable_rule_ids())
    if (length(absent)) {
      semantic_registry_abort(sprintf(
        "Semantic rule id(s) absent from executable surface: %s.",
        paste(absent, collapse = ", ")
      ))
    }
  }
  domain_ids <- names(registry$known_domains)
  if (any(!grepl("^[a-z][a-z0-9_]*$", domain_ids))) {
    semantic_registry_abort(sprintf(
      "Semantic registry has invalid known domain id(s): %s.",
      paste(domain_ids[!grepl("^[a-z][a-z0-9_]*$", domain_ids)], collapse = ", ")
    ))
  }
  for (id in rule_ids) validate_semantic_entry(registry$rules[[id]], id)
  for (id in domain_ids) {
    validate_semantic_entry(registry$known_domains[[id]], paste0("known_domains.", id))
    if (!identical(registry$known_domains[[id]]$implementation_status, "deferred")) {
      semantic_registry_abort(sprintf(
        "Known domain %s must have implementation_status deferred.", id
      ))
    }
  }
  registry
}

load_semantic_registry <- function(path = NULL) {
  use_cache <- is.null(path)
  if (use_cache && !is.null(.semantic_registry_env$registry)) {
    return(.semantic_registry_env$registry)
  }
  if (is.null(path)) {
    path <- file.path(system.file("rulebook", package = "sas2r"), "semantics.yml")
  }
  if (!semantic_registry_scalar(path) || !file.exists(path)) {
    semantic_registry_abort(sprintf(
      "Semantic registry file does not exist: %s.", paste(path, collapse = ", ")
    ))
  }
  registry <- tryCatch(
    yaml::read_yaml(path, eval.expr = FALSE),
    error = function(e) semantic_registry_abort(sprintf(
      "Cannot parse semantic registry: %s", conditionMessage(e)
    ))
  )
  registry <- validate_semantic_registry(registry)
  if (use_cache) .semantic_registry_env$registry <- registry
  registry
}

semantic_rule <- function(id, registry = load_semantic_registry()) {
  if (!semantic_registry_scalar(id)) {
    semantic_registry_abort("Semantic rule id must be a non-empty string.")
  }
  if (startsWith(id, "known_domains.")) {
    return(registry$known_domains[[sub("^known_domains\\.", "", id)]])
  }
  registry$rules[[id]]
}

semantic_coverage <- function(rulebook, registry = load_semantic_registry()) {
  registry <- validate_semantic_registry(registry, enforce_surface = FALSE)
  proc_names <- names(Filter(
    function(x) x$status %in% c("supported", "partial"),
    rulebook$procs
  ))
  required <- c(
    paste0("functions.", names(rulebook$functions)),
    paste0("operators.", names(rulebook$operators)),
    paste0("procs.", proc_names)
  )
  required <- unique(required)
  targets <- c(
    stats::setNames(
      unname(rulebook$functions), paste0("functions.", names(rulebook$functions))
    ),
    stats::setNames(
      unname(rulebook$operators), paste0("operators.", names(rulebook$operators))
    ),
    stats::setNames(
      vapply(rulebook$procs[proc_names], `[[`, "", "emitter"),
      paste0("procs.", proc_names)
    )
  )
  present <- intersect(required, names(registry$rules))
  strategy_mismatches <- present[vapply(
    present,
    function(id) !identical(registry$rules[[id]]$strategy, targets[[id]]),
    logical(1)
  )]
  invalid_dispositions <- present[vapply(
    present,
    function(id) {
      entry <- registry$rules[[id]]
      identical(entry$classification, "unsupported") ||
        identical(entry$implementation_status, "deferred")
    },
    logical(1)
  )]
  missing <- setdiff(required, names(registry$rules))
  unexpected <- setdiff(names(registry$rules), required)
  ok <- !length(c(
    missing, unexpected, strategy_mismatches, invalid_dispositions
  ))
  list(
    required = required,
    covered = present,
    missing = missing,
    unexpected = unexpected,
    strategy_mismatches = strategy_mismatches,
    invalid_dispositions = invalid_dispositions,
    ok = ok
  )
}

semantic_data_step_expressions <- function(project) {
  ids <- unique(project$statements$unit_id[
    project$statements$unit_type == "data_step"
  ])
  expressions <- character()
  for (id in ids[!is.na(ids)]) {
    unit <- project$statements[project$statements$unit_id == id, ]
    ir <- tryCatch(parse_data_step(unit), error = function(e) NULL)
    if (is.null(ir) || !length(ir$steps)) next
    for (step in ir$steps) {
      for (field in intersect(c("cond", "expr"), names(step))) {
        value <- step[[field]]
        if (semantic_registry_scalar(value)) expressions <- c(expressions, value)
      }
    }
  }
  expressions
}

semantic_expression_rules <- function(expression, rulebook) {
  tokens <- tryCatch(tokenize_expr(expression), error = function(e) character())
  if (!length(tokens)) return(character())
  lower <- tolower(tokens)
  function_indices <- which(
    lower %in% names(rulebook$functions) &
      c(lower[-1L], "") == "("
  )
  function_hits <- unique(lower[function_indices])

  is_operand_end <- function(token) {
    grepl("^[a-z_][a-z0-9_]*$", token) ||
      grepl("^[0-9.]", token) ||
      grepl("^['\"]", token) || identical(token, ")")
  }
  is_operand_start <- function(token) {
    is_operand_end(token) || identical(token, "(")
  }
  operator_hits <- character()
  symbolic <- c("=", "^=", "~=", "||")
  binary_words <- c("eq", "ne", "lt", "gt", "le", "ge", "and", "or", "in")
  for (index in seq_along(lower)) {
    token <- lower[index]
    if (!token %in% names(rulebook$operators)) next
    if (token %in% symbolic) {
      operator_hits <- c(operator_hits, token)
      next
    }
    previous <- if (index > 1L) lower[index - 1L] else ""
    following <- if (index < length(lower)) lower[index + 1L] else ""
    binary_position <- is_operand_end(previous) && is_operand_start(following)
    if (identical(token, "in") && identical(previous, "not")) {
      binary_position <- index > 2L && is_operand_end(lower[index - 2L]) &&
        is_operand_start(following)
    }
    composite_not_in <- identical(token, "not") &&
      is_operand_end(previous) && identical(following, "in") &&
      index + 2L <= length(lower) && is_operand_start(lower[index + 2L])
    unary_not <- identical(token, "not") && !identical(following, "in") &&
      (index == 1L || previous %in% c("(", ",", names(rulebook$operators))) &&
      is_operand_start(following)
    if (token %in% binary_words && binary_position) {
      operator_hits <- c(operator_hits, token)
    } else if (composite_not_in || unary_not) {
      operator_hits <- c(operator_hits, token)
    }
  }
  unique(c(
    paste0("functions.", function_hits),
    paste0("operators.", operator_hits)
  ))
}

semantic_exercised_rules <- function(project, procs, rulebook = load_rulebook()) {
  expressions <- semantic_data_step_expressions(project)
  expression_ids <- unique(unlist(lapply(
    expressions,
    semantic_expression_rules,
    rulebook = rulebook
  ), use.names = FALSE))

  proc_hits <- intersect(unique(tolower(procs)), names(rulebook$procs))
  ids <- unique(c(
    expression_ids,
    paste0("procs.", proc_hits)
  ))
  ids[ids %in% names(rulebook$semantics$rules)]
}

semantic_usage_table <- function(ids, registry = load_semantic_registry()) {
  if (!length(ids)) {
    return(tibble::tibble(
      rule_id = character(), classification = character(),
      implementation_status = character(), strategy = character(),
      risk = character()
    ))
  }
  tibble::tibble(
    rule_id = ids,
    classification = vapply(ids, function(id) registry$rules[[id]]$classification,
                            character(1)),
    implementation_status = vapply(
      ids, function(id) registry$rules[[id]]$implementation_status, character(1)
    ),
    strategy = vapply(ids, function(id) registry$rules[[id]]$strategy,
                      character(1)),
    risk = vapply(ids, function(id) registry$rules[[id]]$risk, character(1))
  )
}
