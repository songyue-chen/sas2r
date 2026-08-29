#' Migration program smoke execution and diagnostics
#'
#' Implements dependency-aware program smoke planning, isolated callr subprocess
#' execution, log/hash capture, and bounded agent diagnostics.

#' Build a program smoke execution plan
#'
#' Resolves upstream dependency prefix and identifies real call sites to smoke-test
#' a generated program component without inventing synthetic arguments.
#' Returns exact deferred reasons when execution is not possible.
#'
#' @param graph A dependency graph from `build_dependency_graph()`.
#' @param component_id Component identifier to test.
#' @param selected_revisions Named list of generated R code or revision records.
#' @param execute Logical indicating if execution is globally enabled (default TRUE).
#' @return A named list representing the smoke plan.
#' @noRd
build_program_smoke_plan <- function(
  graph,
  component_id,
  selected_revisions,
  execute = TRUE
) {
  if (!is.character(component_id) || length(component_id) != 1L || !nzchar(component_id)) {
    cli::cli_abort(
      "{.arg component_id} must be a non-empty string",
      class = "sas2r_invalid_argument"
    )
  }

  if (!isTRUE(execute)) {
    return(list(
      status = "deferred",
      reason = "execute_disabled",
      component_id = component_id
    ))
  }

  nodes <- if (is.list(graph) && !is.null(graph$nodes)) graph$nodes else tibble::tibble()
  edges <- if (is.list(graph) && !is.null(graph$edges)) graph$edges else tibble::tibble()

  # Check for dynamic or unresolved dependencies
  if (nrow(edges) > 0L && nrow(nodes) > 0L) {
    comp_node_ids <- nodes$node_id[nodes$component_id == component_id]
    comp_edges <- edges[edges$to %in% comp_node_ids, ]
    if (nrow(comp_edges) > 0L) {
      has_dynamic <- any(comp_edges$resolution %in% c("dynamic", "unresolved") |
                         comp_edges$from %in% nodes$node_id[nodes$type == "unresolved_dependency"])
      if (has_dynamic) {
        return(list(
          status = "deferred",
          reason = "dynamic_call_unresolved",
          component_id = component_id
        ))
      }
    }
  }

  # Check if component is a macro/function without any callable path in graph
  is_macro_name <- grepl("(?i)macro", component_id)
  is_macro_node <- nrow(nodes) > 0L && any(nodes$component_id == component_id & nodes$type %in% c("macro", "function"))
  if (is_macro_name || is_macro_node) {
    comp_node_ids <- if (nrow(nodes) > 0L) nodes$node_id[nodes$component_id == component_id] else character()
    call_edges <- if (nrow(edges) > 0L) {
      edges[edges$type == "calls_macro" & (edges$from %in% comp_node_ids | edges$detail == component_id), ]
    } else {
      tibble::tibble()
    }
    if (nrow(call_edges) == 0L) {
      return(list(
        status = "deferred",
        reason = "no_callable_path",
        component_id = component_id
      ))
    }
  }

  # Check if target component exists in selected revisions
  if (!component_id %in% names(selected_revisions)) {
    return(list(
      status = "deferred",
      reason = "missing_dependency",
      component_id = component_id
    ))
  }

  # Check upstream dependency closure
  deps <- dependency_closure(graph, component_id)
  if (length(deps) > 0L) {
    missing_deps <- setdiff(deps, names(selected_revisions))
    if (length(missing_deps) > 0L) {
      return(list(
        status = "deferred",
        reason = "missing_dependency",
        component_id = component_id,
        missing = missing_deps
      ))
    }
  }

  # Extract target code
  target_entry <- selected_revisions[[component_id]]
  target_code <- if (is.character(target_entry)) {
    target_entry
  } else if (is.list(target_entry)) {
    target_entry$assembled_r %||% target_entry$code %||% target_entry$r_code %||% ""
  } else {
    as.character(target_entry)
  }

  # Determine if component is a macro/function definition requiring a call site
  is_callable_def <- FALSE
  if (nrow(edges) > 0L && nrow(nodes) > 0L) {
    comp_node_ids <- nodes$node_id[nodes$component_id == component_id]
    macro_provider_edges <- edges[edges$from %in% comp_node_ids & edges$type == "calls_macro", ]
    if (nrow(macro_provider_edges) > 0L) {
      is_callable_def <- TRUE
    }
  }

  # Also inspect parsed code: if it only defines functions and has no top-level execution
  parsed_exprs <- tryCatch(parse(text = target_code), error = function(e) NULL)
  if (!is.null(parsed_exprs) && length(parsed_exprs) > 0L) {
    all_fn_assigns <- TRUE
    for (i in seq_along(parsed_exprs)) {
      expr <- parsed_exprs[[i]]
      if (is.call(expr) && (identical(expr[[1L]], as.name("<-")) || identical(expr[[1L]], as.name("=")))) {
        rhs <- expr[[3L]]
        if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) {
          next
        }
      }
      all_fn_assigns <- FALSE
      break
    }
    if (all_fn_assigns) {
      is_callable_def <- TRUE
    }
  } else if (grepl("function\\s*\\(", target_code) && !grepl("([a-zA-Z0-9_]+)\\s*\\(", sub(".*function\\s*\\([^)]*\\)\\s*\\{.*\\}", "", target_code))) {
    is_callable_def <- TRUE
  }

  call_site <- NULL
  if (is_callable_def) {
    # Search for real call site in graph edges
    found_call <- FALSE
    if (nrow(edges) > 0L && nrow(nodes) > 0L) {
      comp_node_ids <- nodes$node_id[nodes$component_id == component_id]
      macro_calls <- edges[edges$from %in% comp_node_ids & edges$type == "calls_macro", ]
      if (nrow(macro_calls) > 0L) {
        for (j in seq_len(nrow(macro_calls))) {
          to_node <- macro_calls$to[j]
          caller_cid <- nodes$component_id[nodes$node_id == to_node]
          if (length(caller_cid) > 0L && caller_cid[1L] %in% names(selected_revisions)) {
            caller_entry <- selected_revisions[[caller_cid[1L]]]
            caller_code <- if (is.character(caller_entry)) caller_entry else caller_entry$code %||% ""
            m_name <- macro_calls$detail[j]
            # Look for call expression in caller code
            call_pattern <- paste0("(?m)^\\s*(", m_name, "\\s*\\([^)]*\\))")
            if (grepl(m_name, caller_code)) {
              lines <- strsplit(caller_code, "\n", fixed = TRUE)[[1L]]
              matching_lines <- grep(paste0("\\b", m_name, "\\s*\\("), lines, value = TRUE)
              if (length(matching_lines) > 0L) {
                call_site <- trimws(matching_lines[1L])
                found_call <- TRUE
                break
              }
            }
          }
        }
      }
    }

    if (!found_call) {
      return(list(
        status = "deferred",
        reason = "no_callable_path",
        component_id = component_id
      ))
    }
  }

  list(
    status = "runnable",
    component_id = component_id,
    dependency_prefix = deps,
    call_site = call_site,
    selected_revisions = selected_revisions
  )
}

#' Run a program smoke test in a fresh callr subprocess
#'
#' Sources resolved dependency prefix and target R component in an isolated
#' callr subprocess, capturing exit status, logs, timing, and error conditions.
#'
#' @param plan A smoke plan from `build_program_smoke_plan()`.
#' @param runtime List or path specifying registry and helpers runtime files.
#' @param attempt_dir Directory path of the attempt.
#' @param timeout Subprocess execution timeout in seconds (default 60).
#' @return A named list representing the smoke execution result.
#' @noRd
run_program_smoke <- function(
  plan,
  runtime,
  attempt_dir,
  timeout = 60
) {
  if (!is.list(plan) || is.null(plan$status)) {
    cli::cli_abort(
      "{.arg plan} must be a valid smoke plan from build_program_smoke_plan()",
      class = "sas2r_invalid_argument"
    )
  }

  component_id <- plan$component_id %||% "unknown"

  if (identical(plan$status, "deferred")) {
    signal_program_smoke_event(
      "program_smoke_deferred",
      component_id = component_id,
      path = attempt_dir,
      reason = plan$reason
    )
    return(list(
      schema_version = MIGRATION_SCHEMA_VERSION,
      execution_id = paste0("exec_", substr(migration_hash(list(comp = component_id, time = Sys.time())), 1L, 16L)),
      component_id = component_id,
      attempt_dir = attempt_dir,
      passed = FALSE,
      deferred = TRUE,
      reason = plan$reason,
      exit_status = NA_integer_,
      elapsed_sec = 0,
      condition = NULL,
      executed_component_ids = character(),
      executed_call_ids = character(),
      stdout_path = NA_character_,
      stderr_path = NA_character_,
      input_hashes = list(),
      output_hashes = list()
    ))
  }

  execution_id <- paste0("exec_", substr(migration_hash(list(comp = component_id, time = Sys.time(), plan = plan)), 1L, 16L))
  attempt_id <- if (is.character(attempt_dir)) basename(attempt_dir) else NULL

  signal_program_smoke_event(
    "program_smoke_started",
    component_id = component_id,
    attempt_id = attempt_id,
    execution_id = execution_id,
    path = attempt_dir
  )

  logs_dir <- file.path(attempt_dir, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

  stdout_path <- normalizePath(file.path(logs_dir, paste0("smoke_", component_id, "_stdout.log")), winslash = "/", mustWork = FALSE)
  stderr_path <- normalizePath(file.path(logs_dir, paste0("smoke_", component_id, "_stderr.log")), winslash = "/", mustWork = FALSE)

  # Resolve runtime files
  registry_file <- NULL
  helpers_file <- NULL
  formats_file <- NULL

  if (is.list(runtime)) {
    registry_file <- runtime$registry
    helpers_file <- runtime$helpers
    formats_file <- runtime$formats
  } else if (is.character(runtime) && length(runtime) == 1L) {
    if (file.exists(file.path(runtime, "_sas2r_registry.R"))) {
      registry_file <- file.path(runtime, "_sas2r_registry.R")
    }
    if (file.exists(file.path(runtime, "sas2r-helpers.R"))) {
      helpers_file <- file.path(runtime, "sas2r-helpers.R")
    }
    if (file.exists(file.path(runtime, "_sas2r_formats.R"))) {
      formats_file <- file.path(runtime, "_sas2r_formats.R")
    }
  }

  if (is.null(helpers_file) || !file.exists(helpers_file)) {
    helpers_file <- system.file("templates", "sas2r-helpers.R", package = "sas2r")
  }

  # Build code chunks for dependencies and target
  dep_codes <- list()
  if (length(plan$dependency_prefix) > 0L) {
    for (d in plan$dependency_prefix) {
      entry <- plan$selected_revisions[[d]]
      code_str <- if (is.character(entry)) {
        entry
      } else if (is.list(entry)) {
        entry$assembled_r %||% entry$r_code %||% entry$code %||% (if (!is.null(entry$r_path) && file.exists(entry$r_path)) paste(readLines(entry$r_path, warn = FALSE), collapse = "\n") else "")
      } else {
        as.character(entry %||% "")
      }
      dep_codes[[d]] <- code_str
    }
  }

  target_entry <- plan$selected_revisions[[component_id]]
  target_code <- if (is.character(target_entry)) {
    target_entry
  } else if (is.list(target_entry)) {
    target_entry$assembled_r %||% target_entry$r_code %||% target_entry$code %||% (if (!is.null(target_entry$r_path) && file.exists(target_entry$r_path)) paste(readLines(target_entry$r_path, warn = FALSE), collapse = "\n") else "")
  } else {
    as.character(target_entry %||% "")
  }

  call_site_str <- if (is.character(plan$call_site)) {
    plan$call_site
  } else if (is.list(plan$call_site)) {
    plan$call_site$text %||% plan$call_site$expression %||% ""
  } else {
    ""
  }

  smoke_runner_fn <- function(registry_file, helpers_file, formats_file, dep_codes, target_code, call_site) {
    # Initialize fresh environment
    rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())

    if (!is.null(registry_file) && nzchar(registry_file) && file.exists(registry_file)) {
      sys.source(registry_file, envir = globalenv())
    }
    if (!is.null(helpers_file) && nzchar(helpers_file) && file.exists(helpers_file)) {
      sys.source(helpers_file, envir = globalenv())
    }
    if (!is.null(formats_file) && nzchar(formats_file) && file.exists(formats_file)) {
      sys.source(formats_file, envir = globalenv())
    }

    executed_components <- character()
    executed_calls <- character()

    if (length(dep_codes) > 0L) {
      for (nm in names(dep_codes)) {
        eval(parse(text = dep_codes[[nm]]), envir = globalenv())
        executed_components <- c(executed_components, nm)
      }
    }

    eval(parse(text = target_code), envir = globalenv())
    executed_components <- c(executed_components, "target")

    if (!is.null(call_site) && nzchar(call_site)) {
      eval(parse(text = call_site), envir = globalenv())
      executed_calls <- c(executed_calls, "call_site_1")
    }

    list(
      success = TRUE,
      executed_components = executed_components,
      executed_calls = executed_calls
    )
  }

  t_start <- Sys.time()
  res <- tryCatch(
    callr::r(
      smoke_runner_fn,
      args = list(
        registry_file = registry_file,
        helpers_file = helpers_file,
        formats_file = formats_file,
        dep_codes = dep_codes,
        target_code = target_code,
        call_site = call_site_str
      ),
      stdout = stdout_path,
      stderr = stderr_path,
      wd = attempt_dir,
      timeout = timeout,
      env = callr::rcmd_safe_env()
    ),
    error = function(e) e
  )
  t_end <- Sys.time()
  elapsed_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))

  passed <- !inherits(res, "error") && isTRUE(res$success)
  condition <- NULL

  if (!passed) {
    err_msg <- conditionMessage(res)
    err_class <- class(res)
    # Check if stderr has more specific error message from the child process
    if (file.exists(stderr_path)) {
      stderr_lines <- readLines(stderr_path, warn = FALSE)
      if (length(stderr_lines) > 0L) {
        first_err <- stderr_lines[grep("(Error|stop):", stderr_lines)]
        if (length(first_err) > 0L) {
          err_msg <- trimws(sub(".*(Error|stop):", "", first_err[1L]))
        }
      }
    }
    condition <- list(
      message = err_msg,
      class = err_class,
      call = conditionCall(res)
    )
    signal_program_smoke_event(
      "program_smoke_failed",
      component_id = component_id,
      attempt_id = attempt_id,
      execution_id = execution_id,
      path = stderr_path,
      reason = err_msg
    )
  } else {
    signal_program_smoke_event(
      "program_smoke_passed",
      component_id = component_id,
      attempt_id = attempt_id,
      execution_id = execution_id,
      path = stdout_path
    )
  }

  # Hash outputs in attempt_dir/work
  output_hashes <- list()
  work_dir <- file.path(attempt_dir, "work")
  if (dir.exists(work_dir)) {
    out_files <- list.files(work_dir, full.names = TRUE)
    if (length(out_files) > 0L) {
      for (f in out_files) {
        output_hashes[[basename(f)]] <- unname(cli::hash_sha256(f))
      }
    }
  }

  list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    execution_id = execution_id,
    component_id = component_id,
    attempt_dir = normalizePath(attempt_dir, winslash = "/", mustWork = FALSE),
    passed = passed,
    exit_status = if (passed) 0L else 1L,
    elapsed_sec = elapsed_sec,
    condition = condition,
    executed_component_ids = if (passed) c(names(dep_codes), component_id) else (res$executed_components %||% character()),
    executed_call_ids = if (passed && nzchar(call_site_str)) "call_site_1" else character(),
    stdout_path = stdout_path,
    stderr_path = stderr_path,
    input_hashes = list(),
    output_hashes = output_hashes,
    created_at = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Produce bounded diagnostics for an agent worker
#'
#' Formats execution diagnostics strictly adhering to the specified policy.
#' `code_only` policy sends condition class, message, mapped location, stack trace,
#' affected identifiers, and capped log excerpts with zero dataset rows or TLF bytes.
#' `bounded` adds configured capped metadata and previews.
#'
#' @param execution Smoke or bundle execution result record.
#' @param policy Evidence policy ("code_only", "bounded", or "full").
#' @return A named list representing bounded diagnostics.
#' @noRd
bounded_agent_diagnostics <- function(
  execution,
  policy = c("code_only", "bounded", "full")
) {
  if (is.character(policy) && length(policy) > 1L) {
    policy <- policy[1L]
  }
  if (!is.character(policy) || length(policy) != 1L || !policy %in% c("code_only", "bounded", "full")) {
    policy <- "code_only"
  }

  cond_msg <- execution$condition$message
  if (is.null(cond_msg) || !nzchar(cond_msg)) {
    if (!is.null(execution$stderr_path) && file.exists(execution$stderr_path)) {
      lines <- readLines(execution$stderr_path, warn = FALSE)
      if (length(lines) > 0L) {
        cond_msg <- paste(lines, collapse = "\n")
      }
    }
  }

  log_excerpt <- character()
  if (!is.null(execution$stderr_path) && file.exists(execution$stderr_path)) {
    err_lines <- readLines(execution$stderr_path, warn = FALSE)
    if (length(err_lines) > 0L) {
      # Take last 50 lines max
      n_lines <- length(err_lines)
      start_line <- max(1L, n_lines - 49L)
      log_excerpt <- paste(err_lines[start_line:n_lines], collapse = "\n")
    }
  }

  affected_ids <- unique(c(
    execution$component_id,
    execution$executed_component_ids
  ))
  affected_ids <- affected_ids[!is.na(affected_ids) & nzchar(affected_ids)]

  if (identical(policy, "code_only")) {
    return(list(
      policy = "code_only",
      execution_id = execution$execution_id,
      component_id = execution$component_id,
      passed = execution$passed,
      condition_message = cond_msg,
      condition_class = execution$condition$class %||% character(),
      source_location = execution$condition$call %||% NA_character_,
      stack_frames = execution$stack_frames %||% character(),
      affected_identifiers = affected_ids,
      log_excerpt = log_excerpt,
      dataset_rows = NULL,
      output_previews = NULL
    ))
  }

  # Bounded policy
  output_meta <- list()
  output_prev <- list()

  if (!is.null(execution$attempt_dir)) {
    work_dir <- file.path(execution$attempt_dir, "work")
    if (dir.exists(work_dir)) {
      rds_files <- list.files(work_dir, pattern = "\\.rds$", full.names = TRUE)
      for (rf in rds_files) {
        ds_name <- sub("\\.rds$", "", basename(rf))
        ds_data <- tryCatch(readRDS(rf), error = function(e) NULL)
        if (is.data.frame(ds_data)) {
          output_meta[[ds_name]] <- list(
            columns = names(ds_data),
            row_count = nrow(ds_data),
            col_count = ncol(ds_data)
          )
          # Capped preview (first 5 rows max)
          output_prev[[ds_name]] <- utils::head(ds_data, 5L)
        }
      }
    }
  }

  if (identical(policy, "bounded")) {
    return(list(
      policy = "bounded",
      execution_id = execution$execution_id,
      component_id = execution$component_id,
      passed = execution$passed,
      condition_message = cond_msg,
      condition_class = execution$condition$class %||% character(),
      source_location = execution$condition$call %||% NA_character_,
      stack_frames = execution$stack_frames %||% character(),
      affected_identifiers = affected_ids,
      log_excerpt = log_excerpt,
      output_metadata = output_meta,
      output_previews = output_prev
    ))
  }

  # Full policy
  list(
    policy = "full",
    execution_id = execution$execution_id,
    component_id = execution$component_id,
    attempt_dir = execution$attempt_dir,
    passed = execution$passed,
    exit_status = execution$exit_status,
    elapsed_sec = execution$elapsed_sec,
    condition = execution$condition,
    condition_message = cond_msg,
    condition_class = execution$condition$class %||% character(),
    executed_component_ids = execution$executed_component_ids,
    executed_call_ids = execution$executed_call_ids,
    stdout_path = execution$stdout_path,
    stderr_path = execution$stderr_path,
    log_excerpt = log_excerpt,
    output_metadata = output_meta,
    output_previews = output_prev,
    input_hashes = execution$input_hashes,
    output_hashes = execution$output_hashes
  )
}

#' Build a bundle execution plan
#'
#' Schedules root entry-point programs in stable dependency order.
#' Included modules and functions are loaded through generated interfaces
#' and are not independently double-run.
#'
#' @param graph A dependency graph from `build_dependency_graph()` or `sas2r_project`.
#' @return A named list representing the bundle execution plan with `$execution_order`,
#'   `$root_programs`, and `$included_modules`.
#' @noRd
build_bundle_execution_plan <- function(graph) {
  g <- if (inherits(graph, "sas2r_project")) {
    build_dependency_graph(graph)
  } else if (is.list(graph) && !is.null(graph$nodes) && !is.null(graph$edges)) {
    graph
  } else {
    cli::cli_abort("{.arg graph} must be a dependency graph or project", class = "sas2r_invalid_argument")
  }

  nodes <- g$nodes
  edges <- g$edges

  schedule <- tryCatch(stable_dependency_schedule(g), error = function(e) tibble::tibble())

  program_nodes <- nodes[nodes$type == "source_unit", ]
  candidate_roots <- unique(program_nodes$component_id)

  inc_edges <- if (nrow(edges) > 0L) edges[edges$type == "includes_file", ] else tibble::tibble()
  if (nrow(inc_edges) > 0L) {
    inc_nodes <- nodes$component_id[nodes$node_id %in% inc_edges$to]
    # If all nodes of a component are targets of includes_file and not from top-level origin,
    # it is strictly an included module
    purely_included <- character()
    for (cid in unique(inc_nodes)) {
      cid_nodes <- nodes[nodes$component_id == cid, ]
      if (all(cid_nodes$node_id %in% inc_edges$to)) {
        purely_included <- c(purely_included, cid)
      }
    }
    candidate_roots <- setdiff(candidate_roots, purely_included)
  }

  ordered_roots <- character()
  if (nrow(schedule) > 0L) {
    sched_roots <- schedule$component_id[schedule$component_id %in% candidate_roots]
    rem_roots <- setdiff(candidate_roots, sched_roots)
    ordered_roots <- unique(c(sched_roots, rem_roots))
  } else {
    ordered_roots <- candidate_roots
  }

  included_modules <- setdiff(unique(nodes$component_id), ordered_roots)

  list(
    execution_order = ordered_roots,
    root_programs = ordered_roots,
    included_modules = included_modules,
    all_components = unique(nodes$component_id)
  )
}

#' Run a complete migration bundle attempt
#'
#' Executes root programs in graph dependency order from an immutable snapshot
#' in a fresh callr subprocess, capturing logs, timing, before/after input hashes,
#' output inventory, and atomically finalizes record.json.
#'
#' @param state Migration state object.
#' @param sequence Optional explicit sequence number.
#' @param parent_attempt_id Optional parent attempt identifier.
#' @param timeout Subprocess execution timeout in seconds (default 120).
#' @return Completed attempt record.
#' @noRd
run_bundle_attempt <- function(
  state,
  sequence = NULL,
  parent_attempt_id = NULL,
  timeout = 120
) {
  state <- normalize_migration_state(state)
  paths <- state$paths %||% migration_paths(state$out_dir %||% tempfile())

  attempt <- init_attempt(
    paths,
    kind = "bundle",
    parent_attempt_id = parent_attempt_id,
    sequence = sequence
  )

  before_hashes <- input_hash_manifest(state$project %||% state)
  bundle_dir <- snapshot_selected_bundle(state, attempt)
  plan <- build_bundle_execution_plan(state$graph)
  exec_order <- plan$execution_order

  logs_dir <- attempt$logs_dir
  stdout_path <- normalizePath(file.path(logs_dir, "bundle_stdout.log"), winslash = "/", mustWork = FALSE)
  stderr_path <- normalizePath(file.path(logs_dir, "bundle_stderr.log"), winslash = "/", mustWork = FALSE)

  bundle_runner_fn <- function(bundle_dir, execution_order) {
    rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())

    reg_file <- file.path(bundle_dir, "_sas2r_registry.R")
    helpers_file <- file.path(bundle_dir, "sas2r-helpers.R")
    formats_file <- file.path(bundle_dir, "_sas2r_formats.R")

    if (file.exists(reg_file)) sys.source(reg_file, envir = globalenv())
    if (file.exists(helpers_file)) sys.source(helpers_file, envir = globalenv())
    if (file.exists(formats_file)) sys.source(formats_file, envir = globalenv())

    status_file <- file.path(bundle_dir, "_sas2r_bundle_progress.json")
    executed <- character()
    for (item in execution_order) {
      writeLines(jsonlite::toJSON(list(current = item, executed = executed), auto_unbox = TRUE), status_file)
      candidates <- c(
        file.path(bundle_dir, item),
        file.path(bundle_dir, paste0(item, ".R")),
        file.path(bundle_dir, paste0(item, ".r"))
      )
      target_file <- candidates[file.exists(candidates)][1L]
      if (is.na(target_file) || !file.exists(target_file)) {
        all_r <- list.files(bundle_dir, pattern = "\\.[rR]$", full.names = TRUE)
        matching <- all_r[tools::file_path_sans_ext(basename(all_r)) == item]
        if (length(matching) > 0L) target_file <- matching[1L]
      }

      if (!is.na(target_file) && file.exists(target_file)) {
        sys.source(target_file, envir = globalenv())
        executed <- c(executed, item)
        writeLines(jsonlite::toJSON(list(current = NA_character_, executed = executed), auto_unbox = TRUE), status_file)
      } else {
        stop(sprintf("Target program %s not found in bundle", item))
      }
    }
    list(success = TRUE, executed = executed)
  }

  t_start <- Sys.time()
  res <- tryCatch(
    callr::r(
      bundle_runner_fn,
      args = list(
        bundle_dir = bundle_dir,
        execution_order = exec_order
      ),
      wd = attempt$attempt_dir,
      stdout = stdout_path,
      stderr = stderr_path,
      timeout = timeout,
      env = callr::rcmd_safe_env()
    ),
    error = function(e) e
  )
  t_end <- Sys.time()
  elapsed_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))

  passed <- !inherits(res, "error") && isTRUE(res$success)
  after_hashes <- input_hash_manifest(state$project %||% state)

  status_file <- file.path(bundle_dir, "_sas2r_bundle_progress.json")
  prog_info <- if (file.exists(status_file)) {
    tryCatch(jsonlite::fromJSON(status_file), error = function(e) NULL)
  } else NULL

  executed_ids <- if (passed) {
    exec_order
  } else if (!is.null(prog_info$executed)) {
    as.character(prog_info$executed)
  } else {
    res$executed %||% character()
  }

  failed_cid <- if (!passed) {
    if (!is.null(prog_info$current) && !is.na(prog_info$current)) {
      as.character(prog_info$current)
    } else {
      setdiff(exec_order, executed_ids)[1L]
    }
  } else {
    NULL
  }

  condition <- NULL
  if (!passed) {
    err_msg <- conditionMessage(res)
    err_class <- class(res)
    if (file.exists(stderr_path)) {
      stderr_lines <- readLines(stderr_path, warn = FALSE)
      if (length(stderr_lines) > 0L) {
        first_err <- stderr_lines[grep("(Error|stop):", stderr_lines)]
        if (length(first_err) > 0L) {
          err_msg <- trimws(sub(".*(Error|stop):", "", first_err[1L]))
        }
      }
    }
    condition <- list(
      message = err_msg,
      class = err_class,
      call = conditionCall(res),
      component_id = failed_cid
    )
  }

  # Hash outputs in attempt directories
  output_hashes <- list()
  cand_dirs <- list.dirs(attempt$attempt_dir, recursive = FALSE, full.names = TRUE)
  cand_dirs <- cand_dirs[!basename(cand_dirs) %in% c("bundle", "logs")]
  for (cd in cand_dirs) {
    c_files <- list.files(cd, recursive = TRUE, full.names = TRUE)
    for (cf in c_files) {
      if (file.info(cf)$isdir) next
      rel_k <- substring(cf, nchar(attempt$attempt_dir) + 2L)
      output_hashes[[rel_k]] <- tryCatch(as.character(cli::hash_file_sha256(cf)), error = function(e) "")
    }
  }

  completed_rec <- complete_attempt(
    attempt,
    passed = passed,
    exit_status = if (passed) 0L else 1L,
    elapsed_sec = elapsed_sec,
    execution_order = exec_order,
    executed_component_ids = executed_ids,
    condition = condition,
    stdout_path = stdout_path,
    stderr_path = stderr_path,
    input_hashes_before = before_hashes,
    input_hashes_after = after_hashes,
    output_hashes = output_hashes,
    run_binding = state$binding %||% state$run_binding %||% list()
  )

  completed_rec
}

