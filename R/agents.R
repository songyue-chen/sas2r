AGENT_REQUIRED <- c("name", "description", "prompt", "tier", "tools",
                    "tool_call_limit", "retry_limit",
                    "output_schema", "on_budget_exhausted")

deep_merge <- function(base, over) {
  for (k in names(over)) {
    base[[k]] <- if (is.list(base[[k]]) && is.list(over[[k]]))
      deep_merge(base[[k]], over[[k]]) else over[[k]]
  }
  base
}

is_abs_prompt_path <- function(p) {
  grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", p)
}

#' Load declarative agent specifications with project overrides
#'
#' @param project_dir Optional path to project root with `.sas2r/agents` overrides.
#' @return Named list of validated agent specifications.
#' @noRd
load_agent_specs <- function(project_dir = NULL) {
  if (!is.null(project_dir)) {
    project_dir <- normalizePath(project_dir, mustWork = FALSE)
  }
  dir <- system.file("agents", package = "sas2r")
  files <- list.files(dir, pattern = "\\.yml$", full.names = TRUE)
  specs <- lapply(files, yaml::read_yaml)
  names(specs) <- vapply(specs, `[[`, character(1), "name")
  if (!is.null(project_dir)) {
    for (nm in names(specs)) {
      ov_file <- file.path(project_dir, ".sas2r", "agents", paste0(nm, ".yml"))
      if (file.exists(ov_file)) {
        ov <- yaml::read_yaml(ov_file)
        if (!is.null(ov$on_budget_exhausted) &&
            !identical(ov$on_budget_exhausted, "downgrade"))
          cli::cli_abort("on_budget_exhausted is immutable (always 'downgrade')",
                         class = "sas2r_agent_spec_error")
        if (isTRUE(ov$tools$search_docs$enabled))
          cli::cli_abort("search_docs can only be enabled via project config, not agent override",
                         class = "sas2r_agent_spec_error")
        specs[[nm]] <- deep_merge(specs[[nm]], ov)
      }
      # Check project_dir/.sas2r/prompts for prompt override
      if (!grepl("[/\\]", specs[[nm]]$prompt)) {
        proj_prompt <- file.path(project_dir, ".sas2r", "prompts", specs[[nm]]$prompt)
        if (file.exists(proj_prompt)) {
          specs[[nm]]$prompt <- proj_prompt
        }
      }
    }
    if (!is.null(specs$translator)) {
      curr_macro <- specs$translator$prompt_macro
      if (is.null(curr_macro) || !grepl("[/\\]", curr_macro)) {
        proj_macro <- file.path(project_dir, ".sas2r", "prompts", curr_macro %||% "translator-macro.md")
        if (file.exists(proj_macro)) {
          specs$translator$prompt_macro <- proj_macro
        }
      }
    }
  }
  for (nm in names(specs)) {
    sp <- specs[[nm]]
    miss <- setdiff(AGENT_REQUIRED, names(sp))
    if (length(miss))
      cli::cli_abort("agent {.val {nm}} missing field{?s} {.field {miss}}",
                     class = "sas2r_agent_spec_error")
    if (grepl("[/\\]", sp$prompt) && !is_abs_prompt_path(sp$prompt)) {
      cli::cli_abort("prompt {.val {sp$prompt}} in agent {.val {nm}} must be a bare filename or absolute path",
                     class = "sas2r_agent_spec_error")
    }
    if (!is.null(sp$prompt_macro) && grepl("[/\\]", sp$prompt_macro) && !is_abs_prompt_path(sp$prompt_macro)) {
      cli::cli_abort("prompt_macro {.val {sp$prompt_macro}} in agent {.val {nm}} must be a bare filename or absolute path",
                     class = "sas2r_agent_spec_error")
    }
    keep <- vapply(names(sp$tools), function(t_name) {
      t <- sp$tools[[t_name]]
      !isFALSE(t$enabled) || t_name == "search_docs"
    }, logical(1))
    specs[[nm]]$tools <- sp$tools[keep]
    specs[[nm]]$tool_call_limit <- as.integer(sp$tool_call_limit)
    specs[[nm]]$retry_limit <- as.integer(sp$retry_limit)
    if (!is.null(sp$temperature)) {
      specs[[nm]]$temperature <- as.numeric(sp$temperature)
    }
    if (!is.null(sp$max_repair_iterations))
      specs[[nm]]$max_repair_iterations <- as.integer(sp$max_repair_iterations)
  }
  specs
}

#' Allowed migration worker role names
#'
#' @return Character vector of migration agent names.
#' @noRd
migration_agent_names <- function() {
  c("translator", "reviewer", "fixer")
}

#' Compute deterministic prompt hash for a migration worker role
#'
#' Covers the role YAML definition, prompt template markdown, and JSON response schema.
#'
#' @param role Worker role name (e.g., "translator", "reviewer", "fixer").
#' @param project_dir Optional project directory for local overrides.
#' @param prompt Optional explicit prompt template filename or path.
#' @param schema Optional explicit response schema name.
#' @return 64-character hex SHA-256 string.
#' @noRd
worker_prompt_hash <- function(role, project_dir = NULL, prompt = NULL, schema = NULL) {
  specs <- load_agent_specs(project_dir = project_dir)
  if (!role %in% names(specs)) {
    cli::cli_abort("unknown worker role {.val {role}}", class = "sas2r_agent_spec_error")
  }
  spec <- specs[[role]]

  # 1. Role YAML content
  yaml_file <- if (!is.null(project_dir)) {
    ov <- file.path(project_dir, ".sas2r", "agents", paste0(role, ".yml"))
    if (file.exists(ov)) ov else system.file("agents", paste0(role, ".yml"), package = "sas2r")
  } else {
    system.file("agents", paste0(role, ".yml"), package = "sas2r")
  }
  yaml_text <- if (nzchar(yaml_file) && file.exists(yaml_file)) {
    paste(readLines(yaml_file, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  # 2. Prompt template markdown
  prompt_file <- prompt %||% spec$prompt
  prompt_path <- if (grepl("[/\\]", prompt_file)) {
    prompt_file
  } else {
    system.file("prompts", prompt_file, package = "sas2r")
  }
  prompt_text <- if (nzchar(prompt_path) && file.exists(prompt_path)) {
    paste(readLines(prompt_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  # 3. Response schema
  schema_name <- schema %||% spec$output_schema
  schema_obj <- tryCatch(agent_output_schema(schema_name), error = function(e) list())

  migration_hash(list(
    role = role,
    yaml = yaml_text,
    prompt = prompt_text,
    schema = schema_obj
  ))
}

#' Compute combined prompt and skill binding hash for a migration worker
#'
#' Covers role YAML, prompt template, response schema, and selected skill bytes.
#'
#' @param role Worker role name.
#' @param skills List of routed skill records.
#' @param project_dir Optional project directory for local overrides.
#' @param prompt Optional explicit prompt template filename.
#' @param schema Optional explicit response schema name.
#' @return 64-character hex SHA-256 string.
#' @noRd
worker_binding_hash <- function(role, skills = list(), project_dir = NULL, prompt = NULL, schema = NULL) {
  p_hash <- worker_prompt_hash(role, project_dir = project_dir, prompt = prompt, schema = schema)
  s_hash <- worker_skill_hash(skills)
  migration_hash(list(
    prompt_hash = p_hash,
    skill_hash = s_hash
  ))
}

