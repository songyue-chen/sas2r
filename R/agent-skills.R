SKILL_NAME_PATTERN <- "^[a-z0-9]+(?:-[a-z0-9]+)*$"
SKILL_ALLOWED_AGENTS <- c("translator", "reviewer", "fixer")
SKILL_ALLOWED_TRIGGER_KEYS <- c(
  "semantic_rules", "procs", "unit_types", "flags",
  "comparison_reasons", "macros", "functions"
)

skill_abort <- function(message) {
  cli::cli_abort(message, class = "sas2r_skill_schema_error")
}

skill_allowed_tools <- function() {
  if (exists("TOOL_ARGUMENT_SCHEMAS", mode = "list")) {
    names(TOOL_ARGUMENT_SCHEMAS)
  } else {
    c("read_unit_context", "query_project_graph", "lookup_rulebook",
      "find_macro", "get_macro_source", "list_macro_files", "search_docs",
      "search_skills", "read_skill")
  }
}

valid_semantic_rule_ids <- function() {
  registry <- tryCatch(load_semantic_registry(), error = function(e) NULL)
  if (is.null(registry)) return(character(0))
  rules <- names(registry$rules %||% list())
  domains <- paste0("known_domains.", names(registry$known_domains %||% list()))
  exec_ids <- tryCatch(semantic_executable_rule_ids(), error = function(e) character(0))
  unique(c(rules, domains, exec_ids))
}

check_duplicate_yaml_keys <- function(lines) {
  stack <- list(list(indent = -1L, key = "", seen = character()))
  for (line in lines) {
    clean <- sub("#.*$", "", line)
    if (!nzchar(trimws(clean))) next
    if (grepl("^[ ]*([a-zA-Z0-9_-]+):", clean)) {
      indent <- nchar(sub("^( *).*", "\\1", clean))
      key_name <- sub("^[ ]*([a-zA-Z0-9_-]+):.*", "\\1", clean)
      while (length(stack) > 1L && stack[[length(stack)]]$indent >= indent) {
        stack <- stack[-length(stack)]
      }
      parent_idx <- length(stack)
      if (key_name %in% stack[[parent_idx]]$seen) {
        skill_abort(sprintf("duplicate key in frontmatter: '%s'", key_name))
      }
      stack[[parent_idx]]$seen <- c(stack[[parent_idx]]$seen, key_name)
      stack <- c(stack, list(list(indent = indent, key = key_name, seen = character())))
    }
  }
}

read_agent_skill <- function(folder_path) {
  if (!is_scalar_character(folder_path) || !dir.exists(folder_path)) {
    skill_abort("skill folder path does not exist")
  }
  folder_name <- basename(folder_path)
  if (!grepl(SKILL_NAME_PATTERN, folder_name)) {
    skill_abort(sprintf("invalid skill folder name '%s'", folder_name))
  }
  skill_file <- file.path(folder_path, "SKILL.md")
  if (!file.exists(skill_file) || dir.exists(skill_file)) {
    skill_abort(sprintf("SKILL.md missing in folder '%s'", folder_name))
  }
  content_hash <- as.character(cli::hash_file_sha256(skill_file))

  lines <- readLines(skill_file, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 2L || lines[1] != "---") {
    skill_abort("SKILL.md must start with '---'")
  }
  delims <- which(lines == "---")
  if (length(delims) < 2L || delims[1] != 1L) {
    skill_abort("SKILL.md must have opening and closing '---' frontmatter delimiters")
  }
  end_yaml <- delims[2]
  yaml_lines <- if (end_yaml > 2L) lines[2:(end_yaml - 1)] else character(0)
  body_lines <- if (end_yaml < length(lines)) lines[(end_yaml + 1):length(lines)] else character(0)
  body <- paste(body_lines, collapse = "\n")
  yaml_text <- paste(yaml_lines, collapse = "\n")

  check_duplicate_yaml_keys(yaml_lines)

  parsed <- tryCatch(
    yaml::yaml.load(yaml_text, eval.expr = FALSE),
    error = function(e) skill_abort(sprintf("failed to parse YAML frontmatter: %s", conditionMessage(e)))
  )
  if (!is.list(parsed)) {
    skill_abort("frontmatter must be a YAML mapping")
  }

  allowed_top <- c("name", "description", "metadata")
  unknown_top <- setdiff(names(parsed), allowed_top)
  if (length(unknown_top) > 0L) {
    skill_abort(sprintf("unknown top-level frontmatter key(s): %s", paste(unknown_top, collapse = ", ")))
  }
  missing_top <- setdiff(allowed_top, names(parsed))
  if (length(missing_top) > 0L) {
    skill_abort(sprintf("missing required frontmatter key(s): %s", paste(missing_top, collapse = ", ")))
  }

  name <- parsed$name
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    skill_abort("skill name must be a non-empty string")
  }
  if (!grepl(SKILL_NAME_PATTERN, name)) {
    skill_abort(sprintf("invalid skill name '%s'", name))
  }
  if (!identical(name, folder_name)) {
    skill_abort(sprintf("skill name '%s' does not match folder name '%s'", name, folder_name))
  }

  description <- parsed$description
  if (!is.character(description) || length(description) != 1L || is.na(description) || !nzchar(trimws(description))) {
    skill_abort("skill description must be a non-empty string")
  }

  metadata <- parsed$metadata
  if (!is.list(metadata) || is.null(metadata$sas2r) || !is.list(metadata$sas2r)) {
    skill_abort("metadata must contain a sas2r mapping")
  }
  sas2r_meta <- metadata$sas2r
  allowed_meta <- c("version", "agents", "priority", "triggers", "tools")
  unknown_meta <- setdiff(names(sas2r_meta), allowed_meta)
  if (length(unknown_meta) > 0L) {
    skill_abort(sprintf("unknown metadata.sas2r key(s): %s", paste(unknown_meta, collapse = ", ")))
  }
  missing_meta <- setdiff(allowed_meta, names(sas2r_meta))
  if (length(missing_meta) > 0L) {
    skill_abort(sprintf("missing metadata.sas2r key(s): %s", paste(missing_meta, collapse = ", ")))
  }

  version <- sas2r_meta$version
  if (!is_positive_integer(version)) {
    skill_abort("metadata.sas2r.version must be a positive integer")
  }
  version <- as.integer(version)

  priority <- sas2r_meta$priority
  if (!is.numeric(priority) || length(priority) != 1L || is.na(priority) || priority < 0 || priority != floor(priority)) {
    skill_abort("metadata.sas2r.priority must be a non-negative integer")
  }
  priority <- as.integer(priority)

  agents <- sas2r_meta$agents
  if (is.list(agents)) agents <- unlist(agents)
  if (!is.character(agents) || length(agents) == 0L || anyNA(agents) || any(!nzchar(agents))) {
    skill_abort("metadata.sas2r.agents must be a non-empty vector of strings")
  }
  if (anyDuplicated(agents)) {
    skill_abort("metadata.sas2r.agents cannot contain duplicates")
  }
  unsupported_agents <- setdiff(agents, SKILL_ALLOWED_AGENTS)
  if (length(unsupported_agents) > 0L) {
    skill_abort(sprintf("unsupported agent(s): %s", paste(unsupported_agents, collapse = ", ")))
  }

  tools <- sas2r_meta$tools
  if (is.list(tools)) tools <- unlist(tools)
  if (!is.character(tools) || length(tools) == 0L || anyNA(tools) || any(!nzchar(tools))) {
    skill_abort("metadata.sas2r.tools must be a non-empty vector of strings")
  }
  if (anyDuplicated(tools)) {
    skill_abort("metadata.sas2r.tools cannot contain duplicates")
  }
  allowed_tools <- skill_allowed_tools()
  unknown_tools <- setdiff(tools, allowed_tools)
  if (length(unknown_tools) > 0L) {
    skill_abort(sprintf("unknown tool(s): %s", paste(unknown_tools, collapse = ", ")))
  }

  triggers <- sas2r_meta$triggers
  if (!is.list(triggers) || length(triggers) == 0L || is.null(names(triggers)) || any(!nzchar(names(triggers)))) {
    skill_abort("metadata.sas2r.triggers must be a non-empty named list")
  }
  if (anyDuplicated(names(triggers))) {
    skill_abort("metadata.sas2r.triggers has duplicate keys")
  }
  unknown_trig_keys <- setdiff(names(triggers), SKILL_ALLOWED_TRIGGER_KEYS)
  if (length(unknown_trig_keys) > 0L) {
    skill_abort(sprintf("unknown trigger key(s): %s", paste(unknown_trig_keys, collapse = ", ")))
  }
  norm_triggers <- list()
  for (key in names(triggers)) {
    val <- triggers[[key]]
    if (is.list(val)) val <- unlist(val)
    if (!is.character(val) || length(val) == 0L || anyNA(val) || any(!nzchar(val))) {
      skill_abort(sprintf("triggers.%s must be a non-empty vector of strings", key))
    }
    if (anyDuplicated(val)) {
      skill_abort(sprintf("triggers.%s cannot contain duplicates", key))
    }
    norm_triggers[[key]] <- as.character(val)
  }

  if (!is.null(norm_triggers$semantic_rules)) {
    valid_rules <- valid_semantic_rule_ids()
    nonexistent <- setdiff(norm_triggers$semantic_rules, valid_rules)
    if (length(nonexistent) > 0L) {
      skill_abort(sprintf("nonexistent semantic_rules trigger(s): %s", paste(nonexistent, collapse = ", ")))
    }
  }

  record <- structure(
    list(
      skill_id = name,
      folder_name = folder_name,
      version = version,
      description = description,
      body = body,
      content_hash = content_hash,
      agents = as.character(agents),
      priority = priority,
      triggers = norm_triggers,
      tools = as.character(tools)
    ),
    class = c("sas2r_agent_skill", "list")
  )

  record
}

validate_agent_skill <- function(skill) {
  if (!is.list(skill)) {
    skill_abort("skill must be a list record")
  }
  required_fields <- c(
    "skill_id", "folder_name", "version", "description", "body",
    "content_hash", "agents", "priority", "triggers", "tools"
  )
  missing <- setdiff(required_fields, names(skill))
  if (length(missing) > 0L) {
    skill_abort(sprintf("skill record missing field(s): %s", paste(missing, collapse = ", ")))
  }
  if (!is_scalar_character(skill$skill_id) || !grepl(SKILL_NAME_PATTERN, skill$skill_id)) {
    skill_abort("invalid skill_id")
  }
  if (!is_scalar_character(skill$folder_name) || !identical(skill$skill_id, skill$folder_name)) {
    skill_abort("skill_id does not match folder_name")
  }
  if (!is_positive_integer(skill$version)) {
    skill_abort("skill version must be a positive integer")
  }
  if (!is_scalar_character(skill$description) || !nzchar(trimws(skill$description))) {
    skill_abort("skill description must be a non-empty string")
  }
  if (!is_scalar_character(skill$body, allow_empty = TRUE)) {
    skill_abort("skill body must be a character string")
  }
  if (!is_scalar_character(skill$content_hash) || !grepl("^[a-f0-9]{64}$", skill$content_hash)) {
    skill_abort("skill content_hash must be a 64-character lowercase hex sha256 digest")
  }
  if (!is.character(skill$agents) || length(skill$agents) == 0L || anyNA(skill$agents) || any(!nzchar(skill$agents))) {
    skill_abort("skill agents must be a non-empty character vector")
  }
  if (anyDuplicated(skill$agents)) {
    skill_abort("skill agents cannot contain duplicates")
  }
  unsupported_agents <- setdiff(skill$agents, SKILL_ALLOWED_AGENTS)
  if (length(unsupported_agents) > 0L) {
    skill_abort(sprintf("unsupported agent(s): %s", paste(unsupported_agents, collapse = ", ")))
  }
  if (!is.numeric(skill$priority) || length(skill$priority) != 1L || is.na(skill$priority) || skill$priority < 0) {
    skill_abort("skill priority must be a non-negative number")
  }
  if (!is.character(skill$tools) || length(skill$tools) == 0L || anyNA(skill$tools) || any(!nzchar(skill$tools))) {
    skill_abort("skill tools must be a non-empty character vector")
  }
  if (anyDuplicated(skill$tools)) {
    skill_abort("skill tools cannot contain duplicates")
  }
  allowed_tools <- skill_allowed_tools()
  unknown_tools <- setdiff(skill$tools, allowed_tools)
  if (length(unknown_tools) > 0L) {
    skill_abort(sprintf("unknown tool(s): %s", paste(unknown_tools, collapse = ", ")))
  }
  if (!is.list(skill$triggers) || length(skill$triggers) == 0L || is.null(names(skill$triggers)) || any(!nzchar(names(skill$triggers)))) {
    skill_abort("skill triggers must be a non-empty named list")
  }
  if (anyDuplicated(names(skill$triggers))) {
    skill_abort("skill triggers has duplicate keys")
  }
  unknown_trig_keys <- setdiff(names(skill$triggers), SKILL_ALLOWED_TRIGGER_KEYS)
  if (length(unknown_trig_keys) > 0L) {
    skill_abort(sprintf("unknown trigger key(s): %s", paste(unknown_trig_keys, collapse = ", ")))
  }
  for (k in names(skill$triggers)) {
    val <- skill$triggers[[k]]
    if (!is.character(val) || length(val) == 0L || anyNA(val) || any(!nzchar(val))) {
      skill_abort(sprintf("triggers.%s must be a non-empty character vector", k))
    }
    if (anyDuplicated(val)) {
      skill_abort(sprintf("triggers.%s cannot contain duplicates", k))
    }
  }
  if (!is.null(skill$triggers$semantic_rules)) {
    valid_rules <- valid_semantic_rule_ids()
    nonexistent <- setdiff(skill$triggers$semantic_rules, valid_rules)
    if (length(nonexistent) > 0L) {
      skill_abort(sprintf("nonexistent semantic_rules trigger(s): %s", paste(nonexistent, collapse = ", ")))
    }
  }
  invisible(TRUE)
}

load_agent_skills <- function(root) {
  if (!is_scalar_character(root) || !dir.exists(root)) {
    cli::cli_abort("package skill root is unavailable",
                   class = "sas2r_skill_schema_error")
  }
  folders <- sort(list.dirs(root, recursive = FALSE, full.names = TRUE),
                  method = "radix")
  records <- lapply(folders, read_agent_skill)
  names(records) <- vapply(records, `[[`, character(1), "skill_id")
  if (anyDuplicated(names(records))) {
    cli::cli_abort("package skill names must be unique",
                   class = "sas2r_skill_schema_error")
  }
  invisible(lapply(records, validate_agent_skill))
  records
}

agent_skill_catalog <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      root <- system.file("skills", package = "sas2r")
      if (!nzchar(root) || !dir.exists(root)) {
        dev_inst <- file.path(getwd(), "inst", "skills")
        if (dir.exists(dev_inst)) root <- dev_inst
      }
      cached <<- if (nzchar(root) && dir.exists(root)) load_agent_skills(root) else list()
    }
    cached
  }
})

get_context_trigger_values <- function(context, key) {
  val <- switch(key,
    semantic_rules = context$semantic_rules %||% context$semantic_rule,
    procs = context$procs %||% context$proc %||% context$proc_name,
    unit_types = context$unit_types %||% context$unit_type,
    flags = context$flags %||% context$flag,
    comparison_reasons = context$comparison_reasons %||% context$comparison_reason %||% context$reasons,
    macros = context$macros %||% context$macro %||% context$macro_name,
    functions = context$functions %||% context[["function"]] %||% context$func,
    context[[key]]
  )
  if (is.null(val)) return(character(0))
  if (is.list(val)) val <- unlist(val)
  val <- as.character(val)
  val <- val[!is.na(val) & nzchar(trimws(val))]
  unique(tolower(trimws(val)))
}

score_skill_triggers <- function(skill, context) {
  if (!is.list(skill$triggers) || !length(skill$triggers)) {
    return(list(score = 0L, matches = character(0)))
  }
  matches <- character(0)
  for (key in names(skill$triggers)) {
    trig_vals <- skill$triggers[[key]]
    if (is.list(trig_vals)) trig_vals <- unlist(trig_vals)
    trig_vals <- unique(tolower(trimws(as.character(trig_vals))))
    ctx_vals <- get_context_trigger_values(context, key)
    hits <- intersect(trig_vals, ctx_vals)
    if (length(hits) > 0L) {
      matches <- c(matches, hits)
    }
  }
  matches <- unique(matches)
  list(score = as.integer(length(matches)), matches = matches)
}

route_agent_skills <- function(context, catalog = agent_skill_catalog()) {
  eligible <- Filter(function(skill) {
    if (is.null(context$agent)) return(TRUE)
    context$agent %in% skill$agents
  }, catalog)
  if (!length(eligible)) return(list())
  scored <- lapply(eligible, function(skill) score_skill_triggers(skill, context))
  keep <- vapply(scored, function(x) x$score > 0L, logical(1))
  selected <- eligible[keep]
  selected_scores <- scored[keep]
  if (!length(selected)) return(list())
  ord <- order(
    -vapply(selected, `[[`, integer(1), "priority"),
    -vapply(selected_scores, `[[`, integer(1), "score"),
    names(selected), method = "radix"
  )
  unname(Map(function(skill, score) {
    list(skill_id = skill$skill_id, version = skill$version,
         content_hash = skill$content_hash,
         activation_reason = paste(score$matches, collapse = ","),
         body = skill$body)
  }, selected[ord], selected_scores[ord]))
}

render_agent_skills <- function(records) {
  if (!length(records)) return("No routed package skills.")
  paste(vapply(records, function(x) paste0(
    "## Skill: ", x$skill_id, " v", x$version, "\n",
    "Activation: ", x$activation_reason, "\n\n", x$body
  ), character(1)), collapse = "\n\n")
}

search_registered_skills <- function(query, catalog = agent_skill_catalog()) {
  if (!is.character(query) || length(query) != 1L || is.na(query)) {
    return(list())
  }
  q <- trimws(tolower(query))
  if (!nzchar(q)) return(list())
  terms <- unlist(strsplit(q, "\\s+"))
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(list())

  matches <- list()
  scores <- integer()
  priorities <- integer()
  names_vec <- character()

  for (skill in catalog) {
    id_lower <- tolower(skill$skill_id)
    desc_lower <- tolower(skill$description)
    trig_lower <- tolower(paste(
      paste(names(skill$triggers), collapse = " "),
      paste(unlist(skill$triggers), collapse = " ")
    ))
    target_text <- paste(id_lower, desc_lower, trig_lower)

    term_hits <- vapply(terms, function(t) grepl(t, target_text, fixed = TRUE), logical(1))
    if (all(term_hits)) {
      score <- 0L
      if (grepl(q, id_lower, fixed = TRUE)) score <- score + 100L
      if (grepl(q, desc_lower, fixed = TRUE)) score <- score + 50L
      for (t in terms) {
        if (grepl(t, id_lower, fixed = TRUE)) score <- score + 20L
        if (grepl(t, desc_lower, fixed = TRUE)) score <- score + 10L
        if (grepl(t, trig_lower, fixed = TRUE)) score <- score + 1L
      }
      matches[[length(matches) + 1L]] <- list(
        name = skill$skill_id,
        description = skill$description
      )
      scores <- c(scores, score)
      priorities <- c(priorities, as.integer(skill$priority %||% 0L))
      names_vec <- c(names_vec, skill$skill_id)
    }
  }
  if (!length(matches)) return(list())
  ord <- order(-scores, -priorities, names_vec, method = "radix")
  matches <- matches[ord]
  if (length(matches) > 10L) {
    matches <- matches[1:10]
  }
  matches
}

read_registered_skill <- function(name, catalog = agent_skill_catalog()) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    cli::cli_abort("skill name must be a non-empty string",
                   class = "sas2r_skill_not_registered")
  }
  # Path-shaped input is rejected on its own terms, with its own condition
  # subclass, so the traversal guard stays observable and cannot be mistaken
  # for -- or silently replaced by -- the ordinary not-in-catalogue branch.
  # The subclass keeps `sas2r_skill_not_registered` so existing handlers hold.
  if (grepl("[/\\\\]", name) || grepl("..", name, fixed = TRUE)) {
    cli::cli_abort(
      "skill {.val {name}} looks like a path; only catalogue skill names are accepted",
      class = c("sas2r_skill_path_rejected", "sas2r_skill_not_registered")
    )
  }
  if (!name %in% names(catalog)) {
    cli::cli_abort("skill {.val {name}} is not registered in the package catalogue",
                   class = "sas2r_skill_not_registered")
  }
  skill <- catalog[[name]]
  list(
    name = skill$skill_id,
    skill_id = skill$skill_id,
    version = skill$version,
    content_hash = skill$content_hash,
    description = skill$description,
    body = skill$body
  )
}

#' Compute deterministic skill hash for routed skills
#'
#' @param skills List of routed skill records or character vector.
#' @return 64-character hex SHA-256 string.
#' @noRd
worker_skill_hash <- function(skills = list()) {
  if (is.null(skills) || length(skills) == 0L) {
    return(migration_hash(list()))
  }
  norm_skills <- lapply(skills, function(s) {
    if (is.list(s)) {
      list(
        skill_id = s$skill_id %||% s$name %||% "",
        version = s$version %||% 1L,
        content_hash = s$content_hash %||% "",
        body = s$body %||% ""
      )
    } else {
      list(skill_id = as.character(s))
    }
  })
  ord <- order(vapply(norm_skills, `[[`, character(1), "skill_id"), method = "radix")
  migration_hash(norm_skills[ord])
}



#' Skill-trigger flags derived from a unit's SAS source
#'
#' The routing context's `flags` vocabulary (`by_group`, `order_dependent`)
#' used to be populated nowhere, so the ordering skills never routed for the
#' RETAIN / FIRST. / LAST. components that are the agent's commonest workload.
#' Deriving the flags from the source itself works at every call site that has
#' the unit's SAS, with no manifest threading.
#'
#' @param sas_text The unit's SAS source text.
#' @return A character vector drawn from the skill-trigger flag vocabulary.
#' @noRd
skill_flags_from_sas <- function(sas_text) {
  x <- tolower(paste(as.character(sas_text %||% ""), collapse = "\n"))
  flags <- character()
  if (grepl("\\b(first|last)\\.", x)) flags <- c(flags, "by_group", "order_dependent")
  if (grepl("\\bretain\\b", x)) flags <- c(flags, "order_dependent")
  if (grepl("\\bproc\\s+sort\\b", x)) flags <- c(flags, "order_dependent")
  unique(flags)
}

#' Skill-trigger comparison reasons derived from repair difference evidence
#'
#' Maps the redacted digest's pattern hints and the gate's alignment
#' provenance onto the `comparison_reasons` trigger vocabulary, so the
#' alignment and ordering skills route to the fixer exactly when the failed
#' comparison shows their failure modes.
#'
#' @param outputs The per-target difference evidence list handed to the fixer.
#' @return A character vector drawn from the comparison-reason vocabulary.
#' @noRd
comparison_reasons_from_outputs <- function(outputs) {
  if (!is.list(outputs) || !length(outputs)) return(character())
  reasons <- character()
  for (t in outputs) {
    d <- t$differences
    if (!is.list(d)) next
    hints <- tryCatch(as.character(d$digest$pattern_hints$hint),
                      error = function(e) character())
    if (any(hints == "DUPLICATE_KEYS")) reasons <- c(reasons, "duplicate_key_order")
    if (any(hints == "NA_PATTERN_DIFF")) reasons <- c(reasons, "missing_order_difference")
    st <- d$structure
    if (is.list(st)) {
      if (isTRUE(st$dup_fanout)) reasons <- c(reasons, "duplicate_key_order")
      if (identical(st$alignment_method, "keyless_multiset")) {
        reasons <- c(reasons, "keyless_reorder")
      }
    }
  }
  unique(reasons)
}

#' Statements belonging to one component, by source file identity
#'
#' The reviewer and fixer receive a component id and the project; this is the
#' shared route from that pair to the unit statements their tools need.
#'
#' @param project A `sas2r_project`, or `NULL`.
#' @param component_id The component identifier.
#' @return A statements tibble, or `NULL` when it cannot be derived.
#' @noRd
component_statements <- function(project, component_id) {
  if (is.null(project) || is.null(project$statements) ||
      !is.data.frame(project$statements) || !nrow(project$statements)) {
    return(NULL)
  }
  stmts <- project$statements
  base <- basename(as.character(stmts$file))
  hit <- base == paste0(component_id, ".sas") |
    tools::file_path_sans_ext(base) == component_id
  rows <- stmts[!is.na(hit) & hit, , drop = FALSE]
  if (nrow(rows)) rows else NULL
}
