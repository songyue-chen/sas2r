# Process workers retain the synchronous role/tool conversation. The parent is
# the sole writer of canonical state and sole authority for paid admissions.
.parallel_worker <- new.env(parent = emptyenv())
.parallel_worker$client <- NULL

is_parallel_budget <- function(budget) {
  !is.null(budget) && isTRUE(attr(budget, "parallel_client", exact = TRUE))
}

parallel_rpc <- function(operation, args = list(), allow_error = FALSE) {
  client <- .parallel_worker$client
  started <- Sys.time()
  client$sequence <- client$sequence + 1L
  stem <- file.path(client$dir, sprintf("%08d", client$sequence))
  atomic_write_file(function(path) saveRDS(list(operation = operation, args = args), path),
                    paste0(stem, ".request"))
  reply <- paste0(stem, ".reply")
  while (!file.exists(reply)) Sys.sleep(0.02)
  result <- readRDS(reply)
  if (length(result$rejected_capabilities))
    list2env(result$rejected_capabilities, .llm_rejected_capabilities)
  client$rpc_ms <- c(client$rpc_ms, stats::setNames(as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000, operation))
  unlink(c(paste0(stem, ".request"), reply))
  if (!is.null(result$error) && !allow_error) stop(structure(result$error, class = result$error_class))
  result
}

parallel_budget_rpc <- function(budget, operation, args) {
  # Executable tools and redactors close over live adapter credentials. Only
  # their public contracts are needed by the parent's existing admission code.
  if (!is.null(args$audit_context)) args$audit_context$.usage_redactor <- NULL
  if (!is.null(args$request)) {
    if (!is.null(args$request$native_history)) {
      # The trusted worker measures the complete request before discarding
      # accounting-only native history from the on-disk RPC payload.
      args$request$text_metrics <- request_text_metrics(args$request)
      args$request$native_history <- NULL
    }
    args$request$tools <- lapply(args$request$tools, function(tool)
      tool[intersect(names(tool), c("name", "description", "schema"))])
  }
  if (!is.null(args$response)) args$response <- args$response[intersect(names(args$response),
    c("usage", "cost", "status", "error", "action", "tool_name", "resolved_model"))]
  result <- parallel_rpc(operation, args, allow_error = TRUE)
  for (field in names(result$increments)) budget[[field]] <- budget[[field]] + result$increments[[field]]
  if (!is.null(result$error)) stop(structure(result$error, class = result$error_class))
  result$value
}

parallel_budget_policy <- function(budget) {
  as.list(budget)[c("mode", "max_usd", "rates", "pricing_source", "run_id",
    "max_calls", "max_retries", "max_tool_calls", "max_wall_time", "max_request_bytes",
    "max_request_chars", "max_input_tokens", "max_output_tokens")]
}

parallel_adapter_recipe <- function(llm) {
  if (is.null(llm)) return(list(kind = "none"))
  cfg <- attr(llm, "parallel_config", exact = TRUE)
  factory <- attr(llm, "parallel_factory", exact = TRUE)
  if (is.null(cfg) && !is.function(factory)) return(NULL)
  # Source references can carry an entire parsed source file into this small
  # startup recipe and exceed Linux's per-environment-string size limit.
  # Keep the executable closure and captured values, not its source metadata.
  if (is.function(factory)) factory <- utils::removeSource(factory)
  list(kind = if (!is.null(cfg)) "ellmer" else "factory", config = cfg,
    factory = factory, verified = as.list(llm$verified_settings))
}

resolve_parallel_execution <- function(state, requested) {
  roles <- c("translator_llm", "reviewer_llm", "fixer_llm")
  for (llm in state[roles]) {
    cfg <- attr(llm, "parallel_config", exact = TRUE)
    validate_parallel_retry_settings(requested, cfg$max_tries)
  }
  supported <- all(vapply(state[roles], function(llm) !is.null(parallel_adapter_recipe(llm)), logical(1)))
  missing_callbacks <- any(vapply(state[roles], function(llm)
    isTRUE(attr(llm, "is_ellmer", exact = TRUE)), logical(1))) && !ellmer_has_request_callbacks()
  effective <- if (supported && !missing_callbacks) requested else 1L
  reason <- if (requested <= 1L) NULL else if (!supported) "adapter_has_no_process_factory" else
      if (missing_callbacks) "ellmer_request_callbacks_unavailable" else NULL
  if (identical(reason, "adapter_has_no_process_factory")) cli::cli_inform(
    "Parallel translation uses one worker: the custom adapter cannot be reconstructed in a fresh process.")
  if (identical(reason, "ellmer_request_callbacks_unavailable")) cli::cli_inform(
    "Parallel translation uses one workflow: install ellmer 0.5.0 or newer for native per-request accounting.")
  list(requested = requested, effective = effective,
    backend = if (effective > 1L) "callr" else "sequential", reason = reason,
    local_execution_slots = 1L, repair_slots = 1L, cpu_allocation = "unknown")
}

parallel_worker_state <- function(state) {
  # Keep full role-equivalent configuration in memory, while moving the LLM
  # config out of callr's on-disk argument serialization (it may contain keys).
  configs <- list()
  clean <- function(x) {
    if (inherits(x, "sas2r_llm") || is.environment(x)) return(NULL)
    if (!is.list(x) || is.data.frame(x)) return(x)
    for (i in seq_along(x)) {
      if (identical(names(x)[i], "llm") && !is.null(x[[i]])) {
        configs[[length(configs) + 1L]] <<- x[[i]]
        x[i] <- list(structure(list(index = length(configs)), class = "sas2r_worker_config"))
      } else x[i] <- list(clean(x[[i]]))
    }
    x
  }
  state$usage_budget <- NULL
  state[c("translator_llm", "reviewer_llm", "fixer_llm")] <- NULL
  snapshot <- clean(state)
  list(state = snapshot, configs = configs)
}

parallel_restore_configs <- function(x, configs) {
  if (inherits(x, "sas2r_worker_config")) return(configs[[x$index]])
  if (is.list(x) && !is.data.frame(x)) x[] <- lapply(x, parallel_restore_configs, configs = configs)
  x
}

parallel_start_job <- function(pool, state, component_id, kind, execute, repair_cap) {
  pool$sequence <- pool$sequence + 1L
  id <- sprintf("job_%s_%06d", pool$id, pool$sequence)
  dir <- file.path(state$paths$diagnostics, "workers", id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  roles <- c("translator_llm", "reviewer_llm", "fixer_llm")
  recipes <- lapply(state[roles], parallel_adapter_recipe)
  # This variable belongs only to this child. It is unset immediately at boot.
  packet <- parallel_worker_state(state)
  env <- c(SAS2R_WORKER_ADAPTERS = jsonlite::base64_enc(serialize(
    list(recipes = recipes, configs = packet$configs,
      tested_capabilities = as.list(.llm_tested_capability_registry),
      rejected_capabilities = as.list(.llm_rejected_capabilities)), NULL)))
  snapshot <- packet$state
  snapshot$paths$component_revisions <- file.path(dir, "components")
  # Attempt execution remains in one lane, using the established smoke layout.
  package_path <- find.package("sas2r")
  process <- callr::r_bg(function(package_path, libpath, snapshot, policy, dir, cid, kind, execute, repair_cap) {
    .libPaths(libpath)
    if (file.exists(file.path(package_path, "Meta", "package.rds"))) {
      loadNamespace("sas2r", lib.loc = dirname(package_path))
    } else pkgload::load_all(package_path, quiet = TRUE)
    get("parallel_worker_main", asNamespace("sas2r"))(
      snapshot, policy, dir, cid, kind, execute, repair_cap)
  }, args = list(package_path, .libPaths(), snapshot,
    parallel_budget_policy(state$usage_budget), dir, component_id, kind, execute, repair_cap),
    env = env, supervise = TRUE, stdout = file.path(dir, "stdout.log"),
    stderr = file.path(dir, "stderr.log"), wd = state$project$project_dir)
  job <- list(id = id, component_id = component_id, kind = kind, dir = dir,
    process = process, redactor = llm_audit_redactor(state$translator_llm),
    requests = character(), tools = list(), last_reply = 0L, started = Sys.time())
  pool$jobs[[id]] <- job
  pool$peak_workers <- max(pool$peak_workers, length(pool$jobs))
  parallel_job_record(job, "running")
  job
}

parallel_worker_main <- function(state, policy, dir, cid, kind, execute, repair_cap) {
  encoded <- Sys.getenv("SAS2R_WORKER_ADAPTERS")
  Sys.unsetenv("SAS2R_WORKER_ADAPTERS")
  private <- unserialize(jsonlite::base64_dec(encoded))
  list2env(private$tested_capabilities, .llm_tested_capability_registry)
  list2env(private$rejected_capabilities, .llm_rejected_capabilities)
  state <- parallel_restore_configs(state, private$configs)
  recipes <- private$recipes
  for (role in names(recipes)) {
    recipe <- recipes[[role]]
    adapter <- switch(recipe$kind, none = NULL, ellmer = ellmer_llm(recipe$config), factory = recipe$factory())
    if (!is.null(adapter)) list2env(recipe$verified, adapter$verified_settings)
    state[role] <- list(adapter)
  }
  state$usage_budget <- do.call(new_usage_budget, policy)
  attr(state$usage_budget, "parallel_client") <- TRUE
  client <- new.env(parent = emptyenv())
  client$dir <- dir
  client$sequence <- 0L
  client$rpc_ms <- numeric()
  .parallel_worker$client <- client
  on.exit(.parallel_worker$client <- NULL, add = TRUE)
  before <- state
  review <- initial_review <- NULL
  state <- withCallingHandlers({
    if (kind == "draft") {
      state <- initialize_program_component(state, cid)
      state <- check_component_revision(state, cid)
      reviewed <- review_component_revision(state, cid, state$repair_counts[[cid]] %||% 0L)
      state <- reviewed$state
      initial_review <- reviewed$review[setdiff(names(reviewed$review), "history")]
    } else if (kind == "review") {
      reviewed <- review_component_revision(state, cid, state$repair_counts[[cid]] %||% 0L)
      review <- reviewed$review[c("verdict", "reused", "reason")]
      state <- promote_reviewed_smoke(reviewed$state, cid)
    } else if (kind == "revisit") {
      state <- revisit_component_runtime(state, cid, execute)
    } else {
      state <- refresh_component_runtime_binding(state, cid)
      state <- process_program_component(state, cid, execute, repair_cap, initial_review = state$initial_reviews[[cid]])
    }
    state
  }, sas2r_progress = function(event) parallel_rpc("progress", list(event = unclass(event), classes = class(event))))
  # Results are scoped changes, never a replacement parent state/budget.
  changed <- function(field) {
    ids <- names(state[[field]])
    ids[!vapply(ids, function(id) identical(before[[field]][[id]], state[[field]][[id]]), logical(1))]
  }
  list(component_id = cid, kind = kind,
    revisions = state$selected_revisions[changed("selected_revisions")],
    histories = state$histories[changed("histories")],
    runtime = if (!identical(before$runtime, state$runtime)) state$runtime else NULL,
    repair_counts = state$repair_counts,
    events = state$events[seq_along(state$events) > length(before$events)],
    diagnostics = state$diagnostics[changed("diagnostics")], review = review,
    dependency_findings = parallel_dependency_findings(state, cid), rpc_ms = client$rpc_ms,
    initial_review = initial_review)
}

parallel_job_record <- function(job, status, reason = NULL) {
  atomic_write_json(list(assignment_id = job$id, component_id = job$component_id,
    phase = job$kind, status = status, started_at = usage_timestamp(job$started),
    updated_at = usage_timestamp(Sys.time()), reason = reason,
    stdout = file.path(job$dir, "stdout.log"), stderr = file.path(job$dir, "stderr.log")),
    file.path(job$dir, "job.json"))
}

parallel_new_pool <- function(state) {
  pool <- new.env(parent = emptyenv())
  pool$state <- state
  pool$jobs <- list()
  pool$failures <- list()
  pool$sequence <- 0L
  pool$id <- new_request_id()
  pool$peak_workers <- 0L
  pool$peak_rss <- NA_real_
  pool$last_sample <- Sys.time() - 1
  pool$rpc_ms <- numeric()
  pool$ps_available <- requireNamespace("ps", quietly = TRUE)
  pool
}

parallel_sample_resources <- function(pool) {
  if (!pool$ps_available || as.numeric(difftime(Sys.time(), pool$last_sample, units = "secs")) < 0.25)
    return(invisible(NULL))
  pool$last_sample <- Sys.time()
  handles <- list(ps::ps_handle())
  for (job in pool$jobs) {
    branch <- tryCatch({
      handle <- ps::ps_handle(job$process$get_pid())
      c(list(handle), ps::ps_children(handle, recursive = TRUE))
    }, error = function(error) list())
    handles <- c(handles, branch)
  }
  rss <- vapply(handles, function(handle) tryCatch(unname(ps::ps_memory_info(handle)["rss"]),
    error = function(error) NA_real_), numeric(1))
  if (any(!is.na(rss))) pool$peak_rss <- max(c(pool$peak_rss, sum(rss, na.rm = TRUE)), na.rm = TRUE)
  invisible(NULL)
}

parallel_observations <- function(pool) {
  admissions <- pool$rpc_ms[names(pool$rpc_ms) %in% c("reserve_usage_request", "reserve_usage_tool_call")]
  list(peak_workers = pool$peak_workers, sampled_peak_process_tree_rss_bytes = pool$peak_rss,
    memory_measurement = if (pool$ps_available) "sampled RSS sum; includes coordinator and nested execution; shared pages may be counted twice" else "unavailable (ps not installed)",
    admission_roundtrip_ms_p95 = if (length(admissions)) unname(stats::quantile(admissions, 0.95)) else NA_real_,
    admission_roundtrip_ms_max = if (length(admissions)) max(admissions) else NA_real_)
}

parallel_counter_values <- function(budget) {
  fields <- c("known_amount", "billed_amount", "estimated_amount", "unknown_count",
    "request_count", "retry_count", "tool_request_count", "tool_count",
    "tool_completed_count", "tool_refused_count", "tool_failed_count", USAGE_TOKEN_FIELDS,
    USAGE_TOTAL_TOKEN_FIELDS)
  unlist(as.list(budget)[unique(fields)])
}

parallel_reservation_wait <- function(budget, request = NULL, context = list()) {
  live_holds <- sum(vapply(budget$reservations, function(reservation)
    if (isTRUE(reservation$abandoned) || isTRUE(reservation$recovered)) 0 else reservation$amount, numeric(1)))
  if (!identical(budget$mode, "strict") || live_holds <= 0) return(FALSE)
  # Compare the same authoritative policy with and without temporary holds.
  available <- if (is.null(request)) usage_budget_allows_future(budget) else
    isTRUE(usage_reservation_quote(budget, request, context)$ok)
  if (available) return(FALSE)
  held <- budget$reserved_amount
  budget$reserved_amount <- held - live_holds
  update_usage_remaining(budget)
  on.exit({ budget$reserved_amount <- held; update_usage_remaining(budget) }, add = TRUE)
  if (is.null(request)) usage_budget_allows_future(budget) else
    isTRUE(usage_reservation_quote(budget, request, context)$ok)
}

parallel_service_job <- function(pool, id) {
  job <- pool$jobs[[id]]
  budget <- pool$state$usage_budget
  for (path in sort(list.files(job$dir, pattern = "[.]request$", full.names = TRUE))) {
    sequence <- as.integer(sub("[.]request$", "", basename(path)))
    if (sequence <= job$last_reply) next
    reply <- sub("[.]request$", ".reply", path)
    message <- readRDS(path)
    op <- message$operation
    args <- message$args
    if (op %in% c("reserve_usage_request", "usage_budget_allows_future") &&
        parallel_reservation_wait(budget, args$request, args$audit_context %||% list())) next
    before <- parallel_counter_values(budget)
    if (op == "progress") signalCondition(structure(args$event, class = args$classes))
    value <- tryCatch({
      if (op == "progress") NULL else if (op == "log") {
        llm_log(args$entry, args$dir, redactor = job$redactor)
        NULL
      } else if (op == "capability_rejection") {
        record_capability_rejection(list(cache_key = args$key), args$parameter)
        NULL
      } else if (op == "checkpoint") {
        for (cid in names(args$repair_counts)) pool$state$repair_counts[[cid]] <-
          max(pool$state$repair_counts[[cid]] %||% 0L, args$repair_counts[[cid]])
        if (!is.null(pool$state$resume_fingerprint))
          write_migration_checkpoint(pool$state, pool$state$resume_fingerprint)
        NULL
      } else {
        if (!is.null(args$audit_context)) args$audit_context$.usage_redactor <- job$redactor
        budget$active_redactor <- job$redactor
        if (op == "reconcile_usage_request") args$reservation <- budget$reservations[[args$reservation$request_id]]
        result <- do.call(get(op, envir = asNamespace("sas2r")), c(list(budget = budget), args))
        if (op == "reserve_usage_request") {
          job$requests <- c(job$requests, result$request_id)
          result <- result[c("request_id")]
        }
        if (op == "reserve_usage_tool_call") job$tools[[result$tool_event_id]] <- result
        if (op == "complete_usage_tool_call") job$tools[[args$reservation$tool_event_id]] <- NULL
        result
      }
    }, error = identity)
    result <- list(increments = parallel_counter_values(budget) - before,
      rejected_capabilities = as.list(.llm_rejected_capabilities))
    if (inherits(value, "condition")) {
      result$error <- list(message = conditionMessage(value), call = NULL, reason = value$reason)
      result$error_class <- class(value)
    } else result["value"] <- list(value)
    job$last_reply <- sequence
    atomic_write_file(function(p) saveRDS(result, p), reply)
  }
  pool$jobs[[id]] <- job
  invisible(NULL)
}

parallel_abandon_job <- function(pool, job) {
  budget <- pool$state$usage_budget
  for (id in intersect(job$requests, names(budget$reservations))) {
    abandon_usage_request(budget, id)
  }
  for (reservation in job$tools) complete_usage_tool_call(budget, reservation, "failed", "unknown")
}

parallel_stop_pool <- function(pool) {
  for (job in pool$jobs) {
    if (job$process$is_alive()) job$process$kill_tree()
    # An admitted request may already have reached the provider; no refund.
    parallel_abandon_job(pool, job)
    parallel_job_record(job, "interrupted")
  }
  invisible(NULL)
}

parallel_poll <- function(pool) {
  parallel_sample_resources(pool)
  completed <- list()
  for (id in names(pool$jobs)) {
    parallel_service_job(pool, id)
    job <- pool$jobs[[id]]
    if (job$process$is_alive()) next
    result <- tryCatch(job$process$get_result(), error = identity)
    if (inherits(result, "condition")) {
      reason <- redact_secrets(conditionMessage(result))
      parallel_abandon_job(pool, job)
      parallel_job_record(job, "failed", reason)
      pool$jobs[[id]] <- NULL
      pool$failures[[id]] <- list(component_id = job$component_id, phase = job$kind,
        job_id = id, reason = reason, critical = critical_translation_error(result),
        stdout = file.path(job$dir, "stdout.log"), stderr = file.path(job$dir, "stderr.log"))
      if (isTRUE(pool$failures[[id]]$critical)) signal_immediate_coordinator_event("worker_failed", job$component_id,
        severity = "error", reason = reason, path = job$dir)
      pool$state$component_stage[[job$component_id]] <- "interrupted"
      next
    }
    pool$rpc_ms <- c(pool$rpc_ms, result$rpc_ms)
    parallel_job_record(job, "completed")
    result$assignment <- job[c("id", "component_id", "kind", "dir", "started")]
    completed[[id]] <- list(job = job, result = result)
    pool$jobs[[id]] <- NULL
  }
  completed
}

parallel_collect_component_failures <- function(pool) {
  handled <- character()
  for (id in names(pool$failures)) {
    failure <- pool$failures[[id]]
    if (isTRUE(failure$critical)) next
    pool$state <- record_component_failure(pool$state, failure$component_id,
      simpleError(failure$reason), failure$phase, dirname(failure$stderr))
    pool$state$diagnostics$worker_failures[[id]] <- failure
    handled <- union(handled, failure$component_id)
    pool$failures[[id]] <- NULL
  }
  handled
}

parallel_abort_failure <- function(pool) {
  if (!length(pool$failures)) return(invisible(NULL))
  pool$state$diagnostics$worker_failures <- c(
    pool$state$diagnostics$worker_failures, pool$failures)
  parallel_save_checkpoint(pool)
  failure <- pool$failures[[1L]]
  cli::cli_abort(c("Parallel worker failed for {failure$component_id} ({failure$phase}); completed sibling work was checkpointed.",
    "i" = "Worker logs: {.path {dirname(failure$stderr)}}"),
    class = "sas2r_parallel_worker_error")
}

parallel_apply_result <- function(state, result) {
  for (field in c(revisions = "selected_revisions", histories = "histories")) {
    source <- if (field == "selected_revisions") result$revisions else result$histories
    state[[field]][names(source)] <- source
  }
  if (!is.null(result$runtime)) state$runtime <- result$runtime
  if (!is.null(result$initial_review)) state$initial_reviews[[result$component_id]] <- result$initial_review
  if (result$kind == "settle") state$initial_reviews[[result$component_id]] <- NULL
  for (cid in names(result$repair_counts)) state$repair_counts[[cid]] <-
    max(state$repair_counts[[cid]] %||% 0L, result$repair_counts[[cid]])
  state$events <- c(state$events, result$events)
  if (length(result$diagnostics)) state$diagnostics <- utils::modifyList(state$diagnostics %||% list(), result$diagnostics)
  state <- sync_dependency_context(state)
  if (!is.null(result$assignment)) parallel_job_record(result$assignment, "accepted")
  state$active_revision <- state$selected_revisions[[result$component_id]]$revision_id
  state
}
