# Test support that no package code calls: oracles, ledger readers, signal and
# error constructors, and drivers the tests use to exercise package code.

# Base-R oracle for PROC MEANS
#
# @param input Input data frame / tibble.
# @param class Grouping column name (character), or character() / NULL / NA for ungrouped.
# @param var Analysis variable column name (character).
# @param stats Named character vector of stats (names = output column names, values = stat IDs).
# @param missing Whether missing class values form a group (logical).
# @return A tibble with computed summary statistics.
means_oracle <- function(input, class = character(), var, stats, missing = FALSE) {
  has_class <- length(class) > 0L && !is.na(class[1]) && nzchar(class[1])
  if (has_class && !missing) {
    valid_class <- !is.na(input[[class[1]]]) & (!is.character(input[[class[1]]]) | trimws(input[[class[1]]]) != "")
    input <- input[valid_class, , drop = FALSE]
  }
  groups <- if (has_class) unique(input[[class[1]]]) else NA
  rows <- lapply(groups, function(g) {
    subset_mask <- if (has_class) {
      if (is.na(g)) is.na(input[[class[1]]]) else !is.na(input[[class[1]]]) & input[[class[1]]] == g
    } else {
      rep(TRUE, nrow(input))
    }
    x <- input[[var]][subset_mask]
    xr <- x[!is.na(x)]
    r <- list()
    if (has_class) r[[class[1]]] <- g
    for (k in seq_along(stats)) {
      stat_name <- if (!is.null(names(stats)) && nzchar(names(stats)[k])) names(stats)[k] else stats[[k]]
      r[[stat_name]] <- switch(tolower(stats[[k]]),
        n = length(xr),
        mean = if (length(xr)) mean(xr) else NA_real_,
        sum = if (length(xr)) sum(xr) else NA_real_,
        std = if (length(xr) >= 2) stats::sd(xr) else NA_real_,
        min = if (length(xr)) min(xr) else NA_real_,
        max = if (length(xr)) max(xr) else NA_real_,
        median = if (length(xr)) stats::median(xr) else NA_real_)
    }
    r[["_freq_"]] <- as.integer(sum(subset_mask))
    tibble::as_tibble(r)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0L) {
    empty_list <- list()
    if (has_class) empty_list[[class[1]]] <- input[[class[1]]][0]
    for (k in seq_along(stats)) {
      stat_name <- if (!is.null(names(stats)) && nzchar(names(stats)[k])) names(stats)[k] else stats[[k]]
      empty_list[[stat_name]] <- if (tolower(stats[[k]]) == "n") integer(0) else numeric(0)
    }
    empty_list[["_freq_"]] <- integer(0)
    out <- tibble::as_tibble(empty_list)
  }
  for (k in seq_along(stats)) {
    if (tolower(stats[[k]]) == "n") {
      stat_name <- if (!is.null(names(stats)) && nzchar(names(stats)[k])) names(stats)[k] else stats[[k]]
      if (stat_name %in% names(out)) {
        out[[stat_name]] <- as.integer(out[[stat_name]])
      }
    }
  }
  out
}

# Base-R oracle for PROC FREQ
#
# @param input Input data frame / tibble.
# @param var Column name for one-way frequency table.
# @param missing Logical indicating whether NA values should be included in counts and percent base.
# @return A tibble with columns `var`, `count`, `percent`.
freq_oracle <- function(input, var, missing) {
  x <- input[[var]]
  if (!missing) {
    valid <- !is.na(x) & (!is.character(x) | trimws(x) != "")
    x <- x[valid]
  }
  tab <- table(x, useNA = if (missing) "ifany" else "no")
  levels_vec <- names(tab)
  if (is.numeric(input[[var]])) {
    levels_vec <- as.numeric(levels_vec)
  }
  out <- tibble::tibble(level = levels_vec, count = as.integer(tab),
                        percent = 100 * as.integer(tab) / sum(tab))
  names(out)[1] <- var
  out
}

semantic_coverage <- function(rulebook, registry = load_semantic_registry()) {
  registry <- validate_semantic_registry(registry, enforce_surface = FALSE)
  proc_names <- names(Filter(
    function(x) x$status %in% c("supported", "partial"),
    rulebook$procs
  ))
  required <- c(
    names(SEMANTIC_MANUAL_ADAPTERS),
    paste0("functions.", names(rulebook$functions)),
    paste0("operators.", names(rulebook$operators)),
    paste0("procs.", proc_names)
  )
  required <- unique(required)
  targets <- c(
    SEMANTIC_MANUAL_ADAPTERS,
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
  targets[c("operators.and", "operators.or", "operators.not")] <- "sas_true"
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

# Validate agent output against a named schema
#
# @param data Extracted output data list.
# @param schema_name Name of the schema in `OUTPUT_SCHEMAS`.
# @return The validated `data` invisibly, or aborts with class `sas2r_schema_error`.
validate_agent_output <- function(data, schema_name) {
  v <- validate_output(data, schema_name)
  if (!isTRUE(v$ok)) {
    cli::cli_abort(
      c("Output failed validation against schema {.val {schema_name}}:",
        v$errors),
      class = "sas2r_schema_error"
    )
  }
  invisible(data)
}

usage_scalar_column <- function(values) {
  scalar <- vapply(values, function(value) {
    is.null(value) || (is.atomic(value) && length(value) <= 1L)
  }, logical(1))
  if (!all(scalar)) return(I(values))
  present <- Filter(function(value) !is.null(value) && length(value), values)
  if (!length(present)) return(rep(NA, length(values)))
  if (any(vapply(present, is.character, logical(1)))) {
    return(vapply(values, function(value) {
      if (is.null(value) || !length(value)) NA_character_ else as.character(value)[1]
    }, character(1)))
  }
  if (any(vapply(present, is.numeric, logical(1)))) {
    return(vapply(values, function(value) {
      if (is.null(value) || !length(value)) NA_real_ else as.numeric(value)[1]
    }, numeric(1)))
  }
  vapply(values, function(value) {
    if (is.null(value) || !length(value)) NA else as.logical(value)[1]
  }, logical(1))
}

read_usage_ledger <- function(path) {
  records <- read_usage_records(path)
  if (!length(records)) return(data.frame())
  fields <- unique(unlist(lapply(records, names), use.names = FALSE))
  columns <- lapply(fields, function(field) {
    usage_scalar_column(lapply(records, function(record) record[[field]]))
  })
  names(columns) <- fields
  as.data.frame(columns, stringsAsFactors = FALSE, optional = TRUE)
}

load_usage_budget <- function(path, mode = "observe", max_usd = Inf,
                              rates = NULL, pricing_source = "adapter",
                              run_id = NULL, ...) {
  new_usage_budget(
    mode = mode, max_usd = max_usd, rates = rates,
    pricing_source = pricing_source, ledger_path = path,
    run_id = run_id, resume = TRUE, ...
  )
}

can_reserve_request <- function(budget, request, audit_context = list()) {
  isTRUE(usage_reservation_quote(budget, request, audit_context)$ok)
}

llm_optional_parameter_error <- function(parameter, message = NULL) {
  if (!parameter %in% LLM_OPTIONAL_PARAMETERS) {
    cli::cli_abort("unknown optional LLM parameter {.val {parameter}}",
                   class = "sas2r_llm_capability_error")
  }
  structure(
    list(
      message = message %||% paste0("optional parameter not supported: ", parameter),
      call = NULL, parameter = parameter, status_code = 400L
    ),
    class = c("sas2r_llm_optional_parameter_error", "error", "condition")
  )
}

clear_llm_registered_secrets <- function() {
  .llm_secret_state$values <- character()
  invisible(NULL)
}

# Signal one progress condition of class `sas2r_progress`, so a test can drive
# the console renderer in with_sas2r_progress() without running an agent.
signal_sas2r_progress <- function(phase, index, total, unit_id = NA_integer_,
                                  status = NA_character_) {
  condition <- structure(
    class = c("sas2r_progress", "condition"),
    list(
      message = "", call = NULL,
      phase = phase,
      index = as.integer(index),
      total = as.integer(total),
      unit_id = as.integer(unit_id),
      status = as.character(status)
    )
  )
  signalCondition(condition)
  invisible(NULL)
}

# Generate all program revisions in stable dependency order
#
# @param project A `sas2r_project` object.
# @param baseline Optional `sas2r_transpilation` baseline from `sas_transpile()`.
# @param graph Optional dependency graph from `build_dependency_graph()`.
# @param schedule Optional stable schedule from `stable_dependency_schedule()`.
# @param outputs Optional output contracts from `infer_output_contracts()`.
# @param llm Optional `sas2r_llm` instance for agent-assisted translation.
# @param paths Optional named list of migration paths from `migration_paths()`.
# @param config Configuration list.
# @param usage_budget Optional shared usage budget.
# @param ... Additional arguments.
# @return A tibble with columns `component_id`, `revision_id`, `r_path`, `contract_path`, `contract`, `status`, `checks`.
generate_program_revisions <- function(
  project,
  baseline = NULL,
  graph = NULL,
  schedule = NULL,
  outputs = NULL,
  llm = NULL,
  paths = NULL,
  config = list(),
  usage_budget = NULL,
  ...
) {
  if (is.null(paths)) {
    paths <- init_migration_paths(tempfile("sas2r-revisions-"))
  } else if (is.character(paths)) {
    paths <- init_migration_paths(paths)
  } else if (is.list(paths)) {
    init_migration_paths(paths$root)
  }

  if (is.null(baseline)) {
    baseline <- sas_transpile(project, paths$staging %||% paths$root)
  }
  if (is.null(outputs)) {
    outputs <- infer_output_contracts(project, overrides = config$outputs)
  }
  if (is.null(graph)) {
    graph <- build_dependency_graph(project, output_contracts = outputs)
  }
  if (is.null(schedule)) {
    schedule <- stable_dependency_schedule(graph)
  }

  resolved_contracts <- list()
  rows <- list()

  if (nrow(schedule) > 0L) {
    for (cid in schedule$component_id) {
      rev_res <- generate_program_revision(
        component_id = cid,
        project = project,
        baseline = baseline,
        graph = graph,
        schedule = schedule,
        outputs = outputs,
        llm = llm,
        paths = paths,
        resolved_contracts = resolved_contracts,
        revision_id = "r1",
        config = config,
        usage_budget = usage_budget,
        ...
      )
      resolved_contracts[[cid]] <- rev_res$contract
      rows[[length(rows) + 1L]] <- rev_res
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      component_id = character(),
      revision_id = character(),
      r_path = character(),
      contract_path = character(),
      contract = list(),
      status = character(),
      checks = list(),
      agent_status = character()
    ))
  }

  tibble::tibble(
    component_id = vapply(rows, `[[`, character(1), "component_id"),
    revision_id = vapply(rows, `[[`, character(1), "revision_id"),
    r_path = vapply(rows, `[[`, character(1), "r_path"),
    contract_path = vapply(rows, `[[`, character(1), "contract_path"),
    contract = lapply(rows, `[[`, "contract"),
    status = vapply(rows, function(r) r$status %||% "ok", character(1)),
    checks = lapply(rows, `[[`, "checks"),
    agent_status = vapply(rows, function(r) r$agent_status %||% NA_character_, character(1))
  )
}

# Locate unit block boundaries in staged R code
#
# @param txt Character vector of file lines.
# @param unit_id Integer unit ID.
# @return Named integer vector c(start = ..., end = ...) or integer(0) if not found.
locate_unit_block <- function(txt, unit_id) {
  uid <- as.integer(unit_id)
  anchor_pat <- sprintf("^# --- sas2r:unit %d ---$", uid)
  anchors <- grep(anchor_pat, txt)
  if (length(anchors) > 0L) {
    start <- anchors[1]
    next_anchors <- grep("^# --- sas2r:unit [0-9]+ ---$", txt)
    next_anchors <- next_anchors[next_anchors > start]
    block_limit <- if (length(next_anchors) > 0L) next_anchors[1] - 1L else length(txt)
    llm_ends <- grep(sprintf("^# sas2r:end unit=%d$", uid), txt)
    llm_ends <- llm_ends[llm_ends >= start & llm_ends <= block_limit]
    if (length(llm_ends) > 0L) return(c(start = start, end = llm_ends[1]))
    stubs <- grep(sprintf("^# sas2r:untranslated unit=%d\\b", uid), txt)
    stubs <- stubs[stubs >= start & stubs <= block_limit]
    if (length(stubs) > 0L) {
      fences <- grep("^# --------------------$", txt)
      fences <- fences[fences >= stubs[1] & fences <= block_limit]
      if (length(fences) > 0L) return(c(start = start, end = fences[1]))
    }
    while (block_limit > start && !nzchar(trimws(txt[block_limit]))) {
      block_limit <- block_limit - 1L
    }
    return(c(start = start, end = block_limit))
  }
  llm_starts <- grep(sprintf("^# sas2r:llm_authored unit=%d\\b", uid), txt)
  if (length(llm_starts) > 0L) {
    start <- llm_starts[1]
    ends <- grep(sprintf("^# sas2r:end unit=%d$", uid), txt)
    ends <- ends[ends >= start]
    end <- if (length(ends) > 0L) ends[1] else start
    return(c(start = start, end = end))
  }
  stub_starts <- grep(sprintf("^# sas2r:untranslated unit=%d\\b", uid), txt)
  if (length(stub_starts) > 0L) {
    start <- stub_starts[1]
    fences <- grep("^# --------------------$", txt)
    fences <- fences[fences >= start]
    end <- if (length(fences) > 0L) fences[1] else start
    return(c(start = start, end = end))
  }
  integer(0)
}
