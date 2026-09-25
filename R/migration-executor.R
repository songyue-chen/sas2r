# Generated R does not need provider credentials. It is still executable local
# code, not an OS sandbox; private input access must be constrained externally.
execution_process_env <- function(startup_file) {
  env <- callr::rcmd_safe_env()
  names <- unique(unlist(lapply(llm_provider_ids(), function(id) llm_provider_spec(id)$credential_envs)))
  current <- Sys.getenv()
  secrets <- llm_registered_secret_values()
  names <- unique(c(names, names(current)[current %in% secrets & nzchar(current)]))
  env[names] <- NA_character_
  env[c("R_ENVIRON_USER", "R_PROFILE_USER")] <- startup_file
  env
}

#' Migration program smoke execution and diagnostics
#'
#' Implements dependency-aware program smoke planning, isolated callr subprocess
#' execution, log/hash capture, and bounded agent diagnostics.

#' @noRd
# The revision record, rather than the caller's choice of field, owns code.
revision_code <- function(entry) {
  if (is.character(entry)) return(paste(entry, collapse = "\n"))
  if (!is.list(entry)) return("")
  code <- entry$assembled_r %||% entry$r_code %||% entry$code
  if (is.null(code) && !is.null(entry$r_path) && file.exists(entry$r_path)) {
    code <- readLines(entry$r_path, warn = FALSE)
  }
  paste(code %||% "", collapse = "\n")
}

# Use the same executable body for target classification and caller lookup.
smoke_program_expressions <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) expression())
  # The emitted startup block is already performed by the smoke runtime.
  # Skip only that exact block; arbitrary caller setup still needs its context.
  boot <- parse(text = module_bootstrap(), keep.source = FALSE)
  if (length(exprs) && identical(exprs[[1L]], boot[[1L]])) exprs <- exprs[-1L]
  exprs
}

# Only a direct, top-level call with literal arguments is independent of its
# caller's scope and control flow. Everything else runs with the real caller;
# do not pull a line out of a function/loop or invent argument values.
standalone_smoke_call <- function(code, name) {
  exprs <- smoke_program_expressions(code)
  literal <- function(e) {
    is.atomic(e) || is.null(e) ||
      (is.call(e) && as.character(e[[1L]])[1L] %in% c("+", "-") &&
         length(e) == 2L && is.numeric(e[[2L]]))
  }
  # Earlier caller statements may create datasets, set options, or bind names
  # used indirectly by the macro, even when its explicit arguments are literal.
  for (e in utils::head(exprs, 1L)) {
    if (is.call(e) && as.character(e[[1L]])[1L] %in% c("<-", "=")) e <- e[[3L]]
    if (!is.call(e) || !identical(e[[1L]], as.name(name))) next
    args <- as.list(e)[-1L]
    safe <- vapply(seq_along(args), function(i) {
      !identical(args[[i]], quote(expr = )) && literal(args[[i]])
    }, logical(1))
    if (all(safe)) return(paste(deparse(e, width.cutoff = 500L), collapse = "\n"))
  }
  NULL
}

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

  target_code <- revision_code(selected_revisions[[component_id]])
  is_callable_def <- FALSE

  # Also inspect parsed code: if it only defines functions and has no top-level execution
  parsed_exprs <- smoke_program_expressions(target_code)
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
  }

  call_site <- NULL
  waiting_on <- character()
  if (is_callable_def) {
    comp_node_ids <- nodes$node_id[nodes$component_id == component_id]
    macro_calls <- if (nrow(edges)) edges[edges$from %in% comp_node_ids &
      edges$type == "calls_macro" & edges$resolution == "resolved", , drop = FALSE] else edges
    reason <- "no_callable_path"
    if (nrow(macro_calls)) {
      reason <- "caller_context_required"
      for (j in seq_len(nrow(macro_calls))) {
        caller <- nodes$component_id[match(macro_calls$to[j], nodes$node_id)]
        if (is.na(caller) || identical(caller, component_id)) next
        if (!caller %in% names(selected_revisions)) {
          waiting_on <- c(waiting_on, caller)
          next
        }
        call_site <- standalone_smoke_call(revision_code(selected_revisions[[caller]]),
                                           macro_calls$detail[j])
        if (!is.null(call_site)) break
      }
    }
    if (is.null(call_site)) return(list(
      status = "deferred", reason = if (length(waiting_on)) "caller_not_generated" else reason,
      component_id = component_id, waiting_on = unique(waiting_on)))
  }

  list(
    status = "runnable",
    component_id = component_id,
    dependency_prefix = deps,
    call_site = call_site,
    selected_revisions = selected_revisions
  )
}

# Every smoke execution gets fresh writable libraries. Optional raw retention
# uses the existing keep_raw_attempts policy, rather than copying all datasets.
prepare_program_smoke <- function(state, plan, attempt_dir) {
  executions <- file.path(attempt_dir, "executions")
  dir.create(executions, recursive = TRUE, showWarnings = FALSE)
  dir <- tempfile(paste0(plan$component_id, "_"), tmpdir = executions)
  dir.create(dir)
  helpers <- file.path(dir, "sas2r-helpers.R")
  file.copy(state$runtime$helpers, helpers)
  formats <- state$runtime$formats %||% file.path(dirname(state$runtime$helpers), "_sas2r_formats.R")
  if (file.exists(formats)) file.copy(formats, file.path(dir, "_sas2r_formats.R"))
  libraries <- build_attempt_library_map(state$project, dir)
  write_autoexec(state$project, dir, library_map = libraries)
  ids <- c(plan$dependency_prefix, plan$component_id)
  programs <- file.path(dir, "programs")
  dir.create(programs)
  files <- file.path(programs, paste0(ids, ".R"))
  for (i in seq_along(ids)) writeLines(revision_code(plan$selected_revisions[[ids[i]]]), files[i])
  replay <- file.path(dir, "run.R")
  writeLines(c(
    "# Partial component smoke replay; not a validated final bundle.",
    sprintf("setwd(%s)", encodeString(normalizePath(dir, winslash = "/"), quote = '\"')),
    'source("autoexec.R", chdir = TRUE)',
    vapply(files, function(f) sprintf("source(%s)", encodeString(f, quote = '\"')), character(1)),
    plan$call_site %||% character()
  ), replay)
  plan$input_project <- state$project
  plan$input_hashes <- state$input_manifest %||% list()
  plan$input_metadata <- state$input_metadata %||% input_hash_manifest(state$project, metadata_only = TRUE)
  plan$code_hashes <- stats::setNames(lapply(files, function(f) unname(cli::hash_file_sha256(f))), ids)
  plan$replay_script <- replay
  plan$raw_output_retention <- if (isTRUE(state$keep_raw_attempts)) "keep_raw_attempts" else "prune_unselected"
  plan$record_dir <- file.path(attempt_dir, "logs")
  list(plan = plan, attempt_dir = dir, runtime = list(
    autoexec = file.path(dir, "autoexec.R"), helpers = helpers,
    output_dirs = c(vapply(libraries, function(entry) entry$write_path, character(1)),
                    libraries = file.path(dir, "libraries"))))
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

  stdout_path <- normalizePath(file.path(logs_dir, paste0("smoke_", component_id, "_", execution_id, "_stdout.log")), winslash = "/", mustWork = FALSE)
  stderr_path <- normalizePath(file.path(logs_dir, paste0("smoke_", component_id, "_", execution_id, "_stderr.log")), winslash = "/", mustWork = FALSE)

  # Resolve runtime files: a bundle's own autoexec.R when there is one (it
  # loads the helpers and formats beside it), else the files named one by one.
  autoexec_file <- NULL
  registry_file <- NULL
  helpers_file <- NULL
  formats_file <- NULL

  if (is.list(runtime)) {
    autoexec_file <- runtime$autoexec
    registry_file <- runtime$registry
    helpers_file <- runtime$helpers
    formats_file <- runtime$formats
  } else if (is.character(runtime) && length(runtime) == 1L) {
    if (file.exists(file.path(runtime, "autoexec.R"))) {
      autoexec_file <- file.path(runtime, "autoexec.R")
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
      code_str <- revision_code(entry)
      dep_codes[[d]] <- code_str
    }
  }

  target_code <- revision_code(plan$selected_revisions[[component_id]])

  call_site_str <- if (is.character(plan$call_site)) {
    plan$call_site
  } else if (is.list(plan$call_site)) {
    plan$call_site$text %||% plan$call_site$expression %||% ""
  } else {
    ""
  }

  smoke_runner_fn <- function(autoexec_file, registry_file, helpers_file, formats_file, dep_codes, target_code, call_site, component_id, population_specs, observe_population, format_call) {
    # Initialize fresh environment
    execution_env <- new.env(parent = globalenv())

    if (!is.null(autoexec_file) && nzchar(autoexec_file) && file.exists(autoexec_file)) {
      sys.source(autoexec_file, envir = execution_env, chdir = TRUE)
    } else {
      if (!is.null(registry_file) && nzchar(registry_file) && file.exists(registry_file)) {
        sys.source(registry_file, envir = execution_env)
      }
      if (!is.null(helpers_file) && nzchar(helpers_file) && file.exists(helpers_file)) {
        sys.source(helpers_file, envir = execution_env)
      }
      if (!is.null(formats_file) && nzchar(formats_file) && file.exists(formats_file)) {
        sys.source(formats_file, envir = execution_env)
      }
    }

    registry_seed <- get0(".sas2r_registry", envir = execution_env, inherits = FALSE)
    executed_components <- character()
    executed_calls <- character()
    current <- component_id
    population_checks <- list()
    execute_component <- function(id, code) {
      current <<- id
      if (!is.null(registry_seed)) assign(".sas2r_registry", registry_seed, envir = execution_env)
      observer <- observe_population(population_specs[[id]], execution_env)
      on.exit({
        population_checks[[id]] <<- observer$finish()
        observer$restore()
      })
      eval(parse(text = code), envir = execution_env)
    }

    tryCatch({

    if (length(dep_codes) > 0L) {
      for (nm in names(dep_codes)) {
        execute_component(nm, dep_codes[[nm]])
        executed_components <- c(executed_components, nm)
      }
    }

    # Keep the observer active for callable programs through their call site.
    execute_component(component_id, paste(target_code, call_site, sep = "\n"))
    executed_components <- c(executed_components, component_id)

    if (!is.null(call_site) && nzchar(call_site)) {
      executed_calls <- c(executed_calls, "call_site_1")
    }

    list(
      success = TRUE,
      executed_components = executed_components,
      executed_calls = executed_calls,
      population_checks = population_checks
    )
    }, error = function(e) {
      cat("Error: ", conditionMessage(e), "\n", sep = "", file = stderr())
      list(
      success = FALSE, executed_components = executed_components,
      executed_calls = executed_calls, failed_component_id = current,
      population_checks = population_checks,
      condition = list(message = conditionMessage(e), class = class(e),
                       call = format_call(conditionCall(e)),
                       component_id = current, population_check = e$population_check)
    )})
  }

  startup_file <- tempfile("sas2r-empty-startup-")
  file.create(startup_file)
  on.exit(unlink(startup_file), add = TRUE)
  t_start <- Sys.time()
  res <- tryCatch(
    callr::r(
      smoke_runner_fn,
      args = list(
        autoexec_file = autoexec_file,
        registry_file = registry_file,
        helpers_file = helpers_file,
        formats_file = formats_file,
        dep_codes = dep_codes,
        target_code = target_code,
        call_site = call_site_str,
        component_id = component_id,
        population_specs = plan$population_specs %||% list(),
        observe_population = observe_source_population,
        format_call = execution_call_text
      ),
      stdout = stdout_path,
      stderr = stderr_path,
      wd = attempt_dir,
      timeout = timeout,
      env = execution_process_env(startup_file), user_profile = FALSE, system_profile = FALSE
    ),
    error = function(e) e
  )
  t_end <- Sys.time()
  elapsed_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))

  passed <- !inherits(res, "error") && isTRUE(res$success)
  condition <- NULL
  if (!is.null(plan$input_project) && !identical(plan$input_metadata, input_hash_manifest(plan$input_project, metadata_only = TRUE))) {
    passed <- FALSE
    res <- simpleError("Source input files changed during program execution")
  }

  if (!passed) {
    condition <- if (inherits(res, "error")) execution_condition(res) else res$condition
    if (any(grepl("timeout", condition$class, fixed = TRUE))) {
      condition$timeout_setting <- "migration.smoke_timeout"
      condition$timeout_seconds <- timeout
      condition$message <- paste0(condition$message, "; migration.smoke_timeout = ", timeout,
        " seconds. Increase this setting for a longer smoke check; no translation defect established.")
    }
    err_msg <- condition$message
    signal_program_smoke_event(
      "program_smoke_failed",
      component_id = component_id,
      attempt_id = attempt_id,
      execution_id = execution_id,
      path = stderr_path,
      reason = if (!is.null(condition$component_id) && !identical(condition$component_id, component_id))
        paste0("blocked by ", condition$component_id, ": ", err_msg) else err_msg
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

  # Include all writable libraries, not just WORK. Hashes and paths remain in
  # the permanent record even when the default retention policy prunes data.
  output_hashes <- list()
  output_files <- list()
  output_dirs <- if (is.list(runtime)) runtime$output_dirs else NULL
  output_dirs <- output_dirs %||% c(work = file.path(attempt_dir, "work"))
  for (libref in names(output_dirs)) {
    files <- list.files(output_dirs[[libref]], full.names = TRUE, recursive = TRUE)
    for (f in files[!dir.exists(files)]) {
      relative <- substring(f, nchar(output_dirs[[libref]]) + 2L)
      key <- if (identical(libref, "work")) relative else paste(libref, relative, sep = "/")
      output_hashes[[key]] <- unname(cli::hash_file_sha256(f))
      output_files[[key]] <- normalizePath(f, winslash = "/", mustWork = TRUE)
    }
  }

  result <- list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    execution_id = execution_id,
    component_id = component_id,
    attempt_dir = normalizePath(attempt_dir, winslash = "/", mustWork = FALSE),
    passed = passed,
    exit_status = if (passed) 0L else 1L,
    elapsed_sec = elapsed_sec,
    condition = condition,
    failed_component_id = condition$component_id %||% NULL,
    blocked_by = if (!is.null(condition$component_id) && !identical(condition$component_id, component_id)) condition$component_id else NULL,
    population_checks = res$population_checks %||% list(),
    executed_component_ids = if (passed) c(names(dep_codes), component_id) else (res$executed_components %||% character()),
    executed_call_ids = if (passed && nzchar(call_site_str)) "call_site_1" else character(),
    stdout_path = stdout_path,
    stderr_path = stderr_path,
    scope = "program_smoke",
    reference_compared = FALSE,
    raw_output_retention = plan$raw_output_retention %||% "caller_managed",
    replay_script = plan$replay_script,
    input_hashes = plan$input_hashes %||% list(),
    input_metadata = plan$input_metadata %||% list(),
    code_hashes = plan$code_hashes %||% list(),
    output_files = output_files,
    output_hashes = output_hashes,
    created_at = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  record_dir <- plan$record_dir %||% logs_dir
  dir.create(record_dir, recursive = TRUE, showWarnings = FALSE)
  # Logs must survive pruning of the execution's writable directories too.
  if (!identical(record_dir, logs_dir)) {
    file.copy(c(stdout_path, stderr_path), record_dir, overwrite = TRUE)
    result$stdout_path <- file.path(record_dir, basename(stdout_path))
    result$stderr_path <- file.path(record_dir, basename(stderr_path))
  }
  result$record_path <- file.path(record_dir, paste0(execution_id, "_record.json"))
  atomic_write_json(result, result$record_path)
  result
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
  if (!identical(policy, "code_only") && (is.null(cond_msg) || !nzchar(cond_msg))) {
    if (!is.null(execution$stderr_path) && file.exists(execution$stderr_path)) {
      lines <- readLines(execution$stderr_path, warn = FALSE)
      if (length(lines) > 0L) {
        cond_msg <- paste(lines, collapse = "\n")
      }
    }
  }

  log_excerpt <- character()
  if (!identical(policy, "code_only") && !is.null(execution$stderr_path) && file.exists(execution$stderr_path)) {
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
  source_location <- execution_call_text(execution$condition$call) %||% NA_character_

  if (identical(policy, "code_only")) {
    return(list(
      policy = "code_only",
      execution_id = execution$execution_id,
      component_id = execution$component_id,
      passed = execution$passed,
      exit_status = execution$exit_status,
      stdout_path = execution$stdout_path,
      stderr_path = execution$stderr_path,
      failed_component_id = execution$failed_component_id %||% execution$condition$component_id,
      blocked_by = execution$blocked_by,
      population_checks = execution$population_checks,
      condition_message = substr(redact_llm_secrets(cond_msg %||% ""), 1L, 2000L),
      condition_class = execution$condition$class %||% character(),
      source_location = source_location,
      stack_frames = if (identical(policy, "code_only")) character() else execution$stack_frames %||% character(),
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
      exit_status = execution$exit_status,
      stdout_path = execution$stdout_path,
      stderr_path = execution$stderr_path,
      failed_component_id = execution$failed_component_id %||% execution$condition$component_id,
      blocked_by = execution$blocked_by,
      population_checks = execution$population_checks,
      condition_message = substr(redact_llm_secrets(cond_msg %||% ""), 1L, 2000L),
      condition_class = execution$condition$class %||% character(),
      source_location = source_location,
      stack_frames = if (identical(policy, "code_only")) character() else execution$stack_frames %||% character(),
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
    condition_message = substr(redact_llm_secrets(cond_msg %||% ""), 1L, 2000L),
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

  inc_edges <- if (nrow(edges) > 0L) edges[edges$type == "includes" & edges$resolution == "resolved", ] else tibble::tibble()
  if (nrow(inc_edges) > 0L) {
    # Includes point from the included file's first unit to the call site.
    included <- nodes$component_id[nodes$node_id %in% inc_edges$from]
    candidate_roots <- setdiff(candidate_roots, included)
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

  before_hashes <- state$input_manifest %||% input_hash_manifest(state$project %||% state)
  bundle_dir <- snapshot_selected_bundle(state, attempt)
  attempt$helper_hash <- unname(cli::hash_file_sha256(file.path(bundle_dir, "sas2r-helpers.R")))
  attempt$revision_manifest <- lapply(state$selected_revisions, function(rev) list(
    revision_id = rev$revision_id, r_hash = migration_hash(revision_code(rev)),
    source_hash = rev$contract$binding$source_hash %||% rev$binding$source_hash,
    affected_outputs = rev$affected_outputs %||% rev$contract$affected_outputs))
  plan <- build_bundle_execution_plan(state$graph)
  # Root programs with unresolved blocking findings are not executed. Their
  # outputs are assessed as not executed, never as produced or as missing.
  deferred <- state$diagnostics$deferred_components %||% list()
  deferred <- deferred[names(deferred) %in% plan$execution_order]
  exec_order <- setdiff(plan$execution_order, names(deferred))
  scope <- list(execution_scope = if (length(deferred)) "partial" else "complete",
    deferred_component_ids = names(deferred) %||% character(), deferred_reasons = deferred)
  failed_checks <- Filter(function(id) identical(state$selected_revisions[[id]]$checks$pass, FALSE),
    unique(c(exec_order, unlist(lapply(exec_order, function(id) dependency_closure(state$graph, id))))))
  if (length(failed_checks)) {
    id <- failed_checks[[1L]]
    failures <- stats::setNames(lapply(failed_checks, function(cid) list(
      component_id = cid, class = "sas2r_mechanical_check_failure",
      message = paste(state$selected_revisions[[cid]]$checks$errors, collapse = "; ")
    )), failed_checks)
    return(do.call(complete_attempt, c(list(attempt, passed = FALSE, deferred = TRUE,
      reason = "mechanical_checks_failed", exit_status = NA_integer_,
      execution_order = exec_order, executed_component_ids = character(),
      condition = failures[[id]], mechanical_failures = failures,
      input_hashes_before = before_hashes, input_hashes_after = before_hashes,
      output_hashes = list()), scope)))
  }
  program_files <- vapply(exec_order, function(cid) {
    rev <- state$selected_revisions[[cid]]
    rev$staged_file %||% rev$contract$staged_file %||% paste0(cid, ".R")
  }, character(1))

  logs_dir <- attempt$logs_dir
  stdout_path <- normalizePath(file.path(logs_dir, "bundle_stdout.log"), winslash = "/", mustWork = FALSE)
  stderr_path <- normalizePath(file.path(logs_dir, "bundle_stderr.log"), winslash = "/", mustWork = FALSE)

  bundle_runner_fn <- function(bundle_dir, execution_order, program_files, population_specs, observe_population) {
    execution_env <- new.env(parent = globalenv())

    # The bundle's own autoexec.R loads the runtime, exactly as a program
    # launched by a person would; an older bundle without one is loaded by hand.
    autoexec_file <- file.path(bundle_dir, "autoexec.R")
    reg_file <- file.path(bundle_dir, "_sas2r_registry.R")
    helpers_file <- file.path(bundle_dir, "sas2r-helpers.R")
    formats_file <- file.path(bundle_dir, "_sas2r_formats.R")

    if (file.exists(autoexec_file)) {
      sys.source(autoexec_file, envir = execution_env, chdir = TRUE)
    } else {
      if (file.exists(reg_file)) sys.source(reg_file, envir = execution_env)
      if (file.exists(helpers_file)) sys.source(helpers_file, envir = execution_env)
      if (file.exists(formats_file)) sys.source(formats_file, envir = execution_env)
    }

    registry_seed <- get(".sas2r_registry", envir = execution_env)
    output_dirs <- lapply(registry_seed, function(binding) binding$write_path)
    status_file <- file.path(bundle_dir, "_sas2r_bundle_progress.json")
    executed <- character()
    population_checks <- list()
    for (item in execution_order) {
      assign(".sas2r_registry", registry_seed, envir = execution_env)
      writeLines(jsonlite::toJSON(list(current = item, executed = executed, population_checks = population_checks), auto_unbox = TRUE), status_file)
      candidates <- c(
        file.path(bundle_dir, program_files[[item]]),
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
        observer <- observe_population(population_specs[[item]], execution_env)
        tryCatch({
          sys.source(target_file, envir = execution_env)
        }, finally = {
          population_checks[[item]] <- observer$finish()
          observer$restore()
          writeLines(jsonlite::toJSON(list(current = item, executed = executed,
            population_checks = population_checks), auto_unbox = TRUE), status_file)
        })
        bindings <- get(".sas2r_registry", envir = execution_env)
        for (lib in names(bindings)) output_dirs[[lib]] <- unique(c(output_dirs[[lib]], bindings[[lib]]$write_path))
        executed <- c(executed, item)
        writeLines(jsonlite::toJSON(list(current = NA_character_, executed = executed, population_checks = population_checks), auto_unbox = TRUE), status_file)
      } else {
        stop(sprintf("Target program %s not found in bundle", item))
      }
    }
    list(success = TRUE, executed = executed, population_checks = population_checks,
      output_dirs = output_dirs)
  }

  startup_file <- tempfile("sas2r-empty-startup-")
  file.create(startup_file)
  on.exit(unlink(startup_file), add = TRUE)
  t_start <- Sys.time()
  res <- tryCatch(
    callr::r(
      bundle_runner_fn,
      args = list(
        bundle_dir = bundle_dir,
        execution_order = exec_order,
        program_files = program_files,
        population_specs = source_population_specs(state$project, exec_order),
        observe_population = observe_source_population
      ),
      wd = attempt$attempt_dir,
      stdout = stdout_path,
      stderr = stderr_path,
      timeout = timeout,
      env = execution_process_env(startup_file), user_profile = FALSE, system_profile = FALSE
    ),
    error = function(e) e
  )
  t_end <- Sys.time()
  elapsed_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))

  passed <- !inherits(res, "error") && isTRUE(res$success)
  after_hashes <- input_hash_manifest(state$project %||% state)
  input_changed <- !identical(before_hashes, after_hashes)
  if (input_changed) {
    # Preserve an existing runtime diagnosis (for example a removed input).
    # Integrity evidence is additional context, never a replacement for it.
    if (passed) res <- simpleError("Source input files changed since run initialization")
    passed <- FALSE
  }

  status_file <- file.path(bundle_dir, "_sas2r_bundle_progress.json")
  prog_info <- if (file.exists(status_file)) {
    tryCatch(jsonlite::fromJSON(status_file, simplifyVector = FALSE), error = function(e) NULL)
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
    condition <- execution_condition(res)
    condition$input_changed <- input_changed
    if (any(grepl("timeout", condition$class, fixed = TRUE))) {
      condition$timeout_setting <- "migration.bundle_timeout"
      condition$timeout_seconds <- timeout
      condition$message <- paste0(condition$message, "; migration.bundle_timeout = ", timeout,
        " seconds. Increase this setting for a longer study run; no translation defect established.")
    }
    condition$component_id <- failed_cid
  }

  output_hashes <- attempt_output_hashes(attempt$attempt_dir)

  completed_rec <- complete_attempt(
    attempt,
    passed = passed,
    exit_status = if (passed) 0L else 1L,
    elapsed_sec = elapsed_sec,
    execution_order = exec_order,
    executed_component_ids = executed_ids,
    execution_scope = scope$execution_scope,
    deferred_component_ids = scope$deferred_component_ids,
    deferred_reasons = scope$deferred_reasons,
    population_checks = res$population_checks %||% prog_info$population_checks %||% list(),
    condition = condition,
    stdout_path = stdout_path,
    stderr_path = stderr_path,
    input_hashes_before = before_hashes,
    input_hashes_after = after_hashes,
    output_dirs = res$output_dirs %||% list(),
    output_hashes = output_hashes,
    execution_context = list(
      sources = lapply(state$selected_revisions, function(rev) rev$contract$binding$source_hash %||% rev$binding$source_hash),
      source_configuration = scan_config_fields(state$config),
      environment = agent_package_facts(state$config$allowlist), locale = Sys.getlocale()),
    run_binding = state$binding %||% state$run_binding %||% list()
  )

  completed_rec
}

# callr wraps the useful child condition in parent; preserve the deepest cause.
execution_condition <- function(error) {
  while (inherits(error$parent, "condition")) error <- error$parent
  list(message = conditionMessage(error), class = class(error),
       call = execution_call_text(conditionCall(error)),
       population_check = error$population_check)
}

# Calls in persisted execution records may already be formatted. Keep absent
# calls absent, including the legacy deparse(NULL) representation.
execution_call_text <- function(call) {
  if (is.null(call)) return(NULL)
  if (is.character(call)) {
    if (!length(call) || all(is.na(call))) return(NULL)
    text <- paste(call[!is.na(call)], collapse = " ")
  } else {
    text <- paste(deparse(call), collapse = " ")
  }
  if (!nzchar(text) || identical(text, "NULL")) NULL else text
}
