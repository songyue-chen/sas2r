#' Component evidence binding and revision history
#'
#' Implements hash-bound component revisions, completed review evidence,
#' coverage, blockers, monotonic promotion ladder, and output-lineage evidence evaluation.

#' Create a new component binding record
#'
#' Derives a canonical binding hash strictly from five canonical components:
#' source_hash, r_hash, helper_hash, prompt_skill_hash, and dependency_closure_hash.
#'
#' @param source_hash Hash of the SAS source unit/file.
#' @param r_hash Hash of the generated R program.
#' @param helper_hash Hash of the helper functions snapshot.
#' @param prompt_skill_hash Combined hash of worker prompts, schemas, and routed skills.
#' @param dependency_closure_hash Hash of the upstream dependency closure.
#' @return A named list representing the component binding.
#' @noRd
new_component_binding <- function(
  source_hash,
  r_hash,
  helper_hash,
  prompt_skill_hash,
  dependency_closure_hash
) {
  args <- list(
    source_hash = source_hash,
    r_hash = r_hash,
    helper_hash = helper_hash,
    prompt_skill_hash = prompt_skill_hash,
    dependency_closure_hash = dependency_closure_hash
  )
  for (nm in names(args)) {
    val <- args[[nm]]
    if (!is.character(val) || length(val) != 1L || !nzchar(val)) {
      cli::cli_abort(
        "{.arg {nm}} must be a non-empty string",
        class = "sas2r_invalid_argument"
      )
    }
  }

  payload <- list(
    source_hash = as.character(source_hash),
    r_hash = as.character(r_hash),
    helper_hash = as.character(helper_hash),
    prompt_skill_hash = as.character(prompt_skill_hash),
    dependency_closure_hash = as.character(dependency_closure_hash)
  )
  b_hash <- migration_hash(payload)

  structure(
    list(
      source_hash = as.character(source_hash),
      r_hash = as.character(r_hash),
      helper_hash = as.character(helper_hash),
      prompt_skill_hash = as.character(prompt_skill_hash),
      dependency_closure_hash = as.character(dependency_closure_hash),
      binding_hash = b_hash,
      hash = b_hash
    ),
    class = "sas2r_component_binding"
  )
}

#' Create a new component evidence history record
#'
#' @param component_id Unique component identifier.
#' @param binding Optional component binding object from `new_component_binding()`.
#' @param schema_version Migration schema version string.
#' @param created_at ISO 8601 creation timestamp.
#' @return A named list representing the component evidence history.
#' @noRd
new_component_evidence_history <- function(
  component_id,
  binding = NULL,
  schema_version = MIGRATION_SCHEMA_VERSION,
  created_at = NULL
) {
  if (!is.character(component_id) || length(component_id) != 1L || !nzchar(component_id)) {
    cli::cli_abort(
      "{.arg component_id} must be a non-empty string",
      class = "sas2r_invalid_argument"
    )
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  revisions <- list()
  active_revision_id <- NULL

  if (!is.null(binding)) {
    if (!inherits(binding, "sas2r_component_binding") && !is.list(binding)) {
      cli::cli_abort(
        "{.arg binding} must be a component binding object",
        class = "sas2r_invalid_argument"
      )
    }
    b_hash <- binding$binding_hash %||% binding$hash
    if (is.null(b_hash) || !nzchar(b_hash)) {
      cli::cli_abort(
        "{.arg binding} must contain a valid binding_hash",
        class = "sas2r_invalid_argument"
      )
    }
    rev_id <- paste0("rev_", substr(b_hash, 1L, 16L))
    initial_rev <- list(
      component_id = component_id,
      revision_id = rev_id,
      binding = binding,
      level = NULL,
      coverage = character(),
      basis_ids = character(),
      blockers = character(),
      runtime_deferred = NULL,
      review_unavailable = FALSE,
      created_at = created_at,
      events = list(
        list(
          type = "binding_activated",
          revision_id = rev_id,
          binding_hash = b_hash,
          created_at = created_at
        )
      )
    )
    revisions <- list(initial_rev)
    active_revision_id <- rev_id
  }

  structure(
    list(
      schema_version = schema_version,
      component_id = component_id,
      active_revision_id = active_revision_id,
      revisions = revisions,
      created_at = created_at
    ),
    class = "sas2r_component_evidence_history"
  )
}

#' Get the active revision evidence record for a component
#'
#' @param history A component evidence history object.
#' @return The active revision record list, or NULL if no revisions exist.
#' @noRd
current_component_evidence <- function(history) {
  if (is.null(history) || length(history$revisions) == 0L) {
    return(NULL)
  }
  if (!is.null(history$active_revision_id)) {
    for (rev in history$revisions) {
      if (identical(rev$revision_id, history$active_revision_id)) {
        return(rev)
      }
    }
  }
  history$revisions[[length(history$revisions)]]
}

#' Record review unavailable on the active component revision
#'
#' Adds a blocker to the active revision without granting evidence level,
#' sets review_unavailable to TRUE, and records the event in append-only history.
#'
#' @param history A component evidence history object.
#' @param reason Explanatory reason (e.g. "provider_timeout", "llm_rate_limit").
#' @param created_at Optional ISO 8601 timestamp.
#' @return Updated component evidence history object.
#' @noRd
record_review_unavailable <- function(history, reason = "review_unavailable", created_at = NULL) {
  if (is.null(history) || length(history$revisions) == 0L) {
    cli::cli_abort("History has no active revision to record review unavailable",
                   class = "sas2r_invalid_state")
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  reason_str <- as.character(reason)
  if (!length(reason_str) || !nzchar(reason_str[1L])) {
    reason_str <- "review_unavailable"
  }

  active_idx <- which(vapply(history$revisions, function(r) {
    identical(r$revision_id, history$active_revision_id)
  }, logical(1)))
  if (length(active_idx) == 0L) active_idx <- length(history$revisions)

  rev <- history$revisions[[active_idx]]
  rev$review_unavailable <- TRUE
  rev$blockers <- unique(c(rev$blockers, reason_str))
  rev$events <- c(rev$events, list(list(
    type = "review_unavailable",
    reason = reason_str,
    created_at = created_at
  )))

  history$revisions[[active_idx]] <- rev
  history
}

#' Record runtime deferred on the active component revision
#'
#' Sets runtime_deferred reason on the active revision and records event in append-only history.
#'
#' @param history A component evidence history object.
#' @param reason Explanatory reason (e.g. "execute_disabled", "no_callable_path", "missing_dependency").
#' @param created_at Optional ISO 8601 timestamp.
#' @return Updated component evidence history object.
#' @noRd
record_runtime_deferred <- function(history, reason = "execute_disabled", created_at = NULL) {
  if (is.null(history) || length(history$revisions) == 0L) {
    return(history)
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  reason_str <- as.character(reason)
  if (!length(reason_str) || !nzchar(reason_str[1L])) {
    reason_str <- "execute_disabled"
  }

  active_idx <- which(vapply(history$revisions, function(r) {
    identical(r$revision_id, history$active_revision_id)
  }, logical(1)))
  if (length(active_idx) == 0L) active_idx <- length(history$revisions)

  rev <- history$revisions[[active_idx]]
  rev$runtime_deferred <- reason_str
  rev$events <- c(rev$events, list(list(
    type = "runtime_deferred",
    reason = reason_str,
    created_at = created_at
  )))

  history$revisions[[active_idx]] <- rev
  history
}

#' Record completed review on the active component revision
#'
#' The only valid path to the `reviewed_only` evidence level.
#' Resolves active review_unavailable blockers while preserving historical events.
#'
#' @param history A component evidence history object.
#' @param verdict Review verdict enum: "reviewed_no_material_finding", "repair_required", or "review_unavailable".
#' @param runtime_deferred Optional precise reason why runtime verification is deferred (e.g. "no_callable_path").
#' @param basis_id Optional evidence basis identifier.
#' @param basis_ids Optional character vector of evidence basis identifiers.
#' @param findings Optional list of reviewer findings.
#' @param created_at Optional ISO 8601 timestamp.
#' @return Updated component evidence history object.
#' @noRd
record_completed_review <- function(
  history,
  verdict = "reviewed_no_material_finding",
  runtime_deferred = NULL,
  basis_id = NULL,
  basis_ids = character(),
  findings = list(),
  created_at = NULL
) {
  if (is.null(history) || length(history$revisions) == 0L) {
    cli::cli_abort("History has no active revision to record review",
                   class = "sas2r_invalid_state")
  }
  if (!is.character(verdict) || length(verdict) != 1L) {
    cli::cli_abort("verdict must be a single string", class = "sas2r_invalid_argument")
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  if (identical(verdict, "review_unavailable")) {
    return(record_review_unavailable(history, reason = "review_unavailable", created_at = created_at))
  }

  active_idx <- which(vapply(history$revisions, function(r) {
    identical(r$revision_id, history$active_revision_id)
  }, logical(1)))
  if (length(active_idx) == 0L) active_idx <- length(history$revisions)

  rev <- history$revisions[[active_idx]]
  all_basis <- unique(c(rev$basis_ids, basis_id, basis_ids))
  all_basis <- all_basis[!is.na(all_basis) & nzchar(all_basis)]

  if (identical(verdict, "repair_required")) {
    rev$blockers <- unique(c(rev$blockers, "repair_required"))
    rev$basis_ids <- all_basis
    rev$events <- c(rev$events, list(list(
      type = "review_completed",
      verdict = verdict,
      findings = as.list(findings),
      created_at = created_at
    )))
    history$revisions[[active_idx]] <- rev
    return(history)
  }

  if (identical(verdict, "reviewed_no_material_finding")) {
    rev$level <- "reviewed_only"
    rev$review_unavailable <- FALSE
    # Clear review-unavailable and repair-required blockers
    review_blockers <- c("provider_timeout", "llm_rate_limit", "provider_crash",
                         "review_unavailable", "repair_required")
    rev$blockers <- setdiff(rev$blockers, review_blockers)
    if (!is.null(runtime_deferred) && nzchar(runtime_deferred)) {
      rev$runtime_deferred <- as.character(runtime_deferred)
    }
    rev$basis_ids <- all_basis
    rev$events <- c(rev$events, list(list(
      type = "review_completed",
      verdict = verdict,
      runtime_deferred = rev$runtime_deferred,
      basis_ids = rev$basis_ids,
      findings = as.list(findings),
      created_at = created_at
    )))
    history$revisions[[active_idx]] <- rev
    return(history)
  }

  cli::cli_abort(
    "invalid review verdict: {.val {verdict}}; must be one of {.val {c('reviewed_no_material_finding', 'repair_required', 'review_unavailable')}}",
    class = "sas2r_invalid_verdict"
  )
}

#' Promote evidence level on the active component revision
#'
#' Enforces monotonic ladder: reviewed_only -> runtime_verified -> output_verified -> reference_validated.
#' Rejects invalid skips, demotions, and missing coverage/basis.
#'
#' @param history A component evidence history object.
#' @param target_level Target evidence level from `COMPONENT_EVIDENCE_LEVELS`.
#' @param coverage Optional coverage string or character vector (e.g. "call:macro(x=1)", "dataset:work.ds").
#' @param basis_ids Optional character vector of basis IDs.
#' @param basis_id Optional single basis ID.
#' @param created_at Optional ISO 8601 timestamp.
#' @return Updated component evidence history object.
#' @noRd
promote_component_evidence <- function(
  history,
  target_level,
  coverage = NULL,
  basis_ids = character(),
  basis_id = NULL,
  created_at = NULL
) {
  if (is.null(history) || length(history$revisions) == 0L) {
    cli::cli_abort("History has no active revision to promote",
                   class = "sas2r_invalid_state")
  }
  if (!is.character(target_level) || length(target_level) != 1L || !target_level %in% COMPONENT_EVIDENCE_LEVELS) {
    cli::cli_abort(
      "invalid target evidence level: {.val {target_level}}; must be one of {.val {COMPONENT_EVIDENCE_LEVELS}}",
      class = "sas2r_invalid_evidence_level"
    )
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  active_idx <- which(vapply(history$revisions, function(r) {
    identical(r$revision_id, history$active_revision_id)
  }, logical(1)))
  if (length(active_idx) == 0L) active_idx <- length(history$revisions)

  rev <- history$revisions[[active_idx]]
  curr_level <- rev$level

  if (isTRUE(rev$review_unavailable)) {
    cli::cli_abort(
      "Cannot promote component evidence while review is unavailable",
      class = "sas2r_invalid_evidence_promotion"
    )
  }
  if (length(rev$blockers) > 0L) {
    cli::cli_abort(
      "Cannot promote component evidence with active blockers: {.val {rev$blockers}}",
      class = "sas2r_invalid_evidence_promotion"
    )
  }

  curr_rank <- if (!is.null(curr_level)) match(curr_level, COMPONENT_EVIDENCE_LEVELS) else 0L
  target_rank <- match(target_level, COMPONENT_EVIDENCE_LEVELS)

  if (target_rank < curr_rank) {
    cli::cli_abort(
      "Cannot demote component evidence from {.val {curr_level}} to {.val {target_level}}",
      class = "sas2r_invalid_evidence_promotion"
    )
  }

  supplied_basis <- unique(c(basis_id, basis_ids))
  supplied_basis <- supplied_basis[!is.na(supplied_basis) & nzchar(supplied_basis)]
  cov_vec <- if (!is.null(coverage)) unique(as.character(coverage)) else character()
  cov_vec <- cov_vec[!is.na(cov_vec) & nzchar(cov_vec)]

  is_advancement <- is.null(curr_level) || !identical(curr_level, target_level)

  # Monotonic ladder verification
  if (identical(target_level, "reviewed_only")) {
    if (is.null(curr_level)) {
      cli::cli_abort(
        "reviewed_only requires completed review via record_completed_review()",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
  } else if (identical(target_level, "runtime_verified")) {
    if (is.null(curr_level) || !curr_level %in% c("reviewed_only", "runtime_verified", "output_verified", "reference_validated")) {
      cli::cli_abort(
        "runtime_verified requires completed review (reviewed_only); current level is {.val {curr_level %||% 'none'}}",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
    if (is_advancement && length(cov_vec) == 0L && length(supplied_basis) == 0L) {
      cli::cli_abort(
        "runtime_verified requires non-empty execution coverage",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
  } else if (identical(target_level, "output_verified")) {
    if (is.null(curr_level) || !curr_level %in% c("runtime_verified", "output_verified", "reference_validated")) {
      cli::cli_abort(
        "output_verified requires prior runtime_verified level; current level is {.val {curr_level %||% 'none'}}",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
    if (is_advancement && length(cov_vec) == 0L && length(supplied_basis) == 0L) {
      cli::cli_abort(
        "output_verified requires output coverage or basis identifier",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
  } else if (identical(target_level, "reference_validated")) {
    if (is.null(curr_level) || !curr_level %in% c("output_verified", "reference_validated")) {
      cli::cli_abort(
        "reference_validated requires prior output_verified level; current level is {.val {curr_level %||% 'none'}}",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
    if (is_advancement && length(supplied_basis) == 0L) {
      cli::cli_abort(
        "reference_validated requires reference comparison basis identifiers",
        class = "sas2r_invalid_evidence_promotion"
      )
    }
  }

  rev$level <- target_level
  rev$coverage <- unique(c(rev$coverage, cov_vec))
  rev$basis_ids <- unique(c(rev$basis_ids, supplied_basis))
  rev$events <- c(rev$events, list(list(
    type = "evidence_promoted",
    from_level = curr_level,
    to_level = target_level,
    coverage = rev$coverage,
    basis_ids = rev$basis_ids,
    created_at = created_at
  )))

  history$revisions[[active_idx]] <- rev
  history
}

#' Activate a new component binding on an evidence history
#'
#' Creates a new active revision with no evidence level (resetting level, coverage,
#' basis, and blockers) while preserving all prior revisions immutably.
#'
#' @param history A component evidence history object.
#' @param binding A component binding object from `new_component_binding()`.
#' @param created_at Optional ISO 8601 timestamp.
#' @return Updated component evidence history object with new active revision.
#' @noRd
activate_component_binding <- function(history, binding, created_at = NULL) {
  if (is.null(history) || !inherits(history, "sas2r_component_evidence_history") && !is.list(history)) {
    cli::cli_abort("{.arg history} must be a component evidence history object",
                   class = "sas2r_invalid_argument")
  }
  if (!inherits(binding, "sas2r_component_binding") && !is.list(binding)) {
    cli::cli_abort("{.arg binding} must be a component binding object",
                   class = "sas2r_invalid_argument")
  }
  b_hash <- binding$binding_hash %||% binding$hash
  if (is.null(b_hash) || !nzchar(b_hash)) {
    cli::cli_abort("{.arg binding} must contain a valid binding_hash",
                   class = "sas2r_invalid_argument")
  }
  if (is.null(created_at)) {
    created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  rev_num <- length(history$revisions) + 1L
  base_rev_id <- paste0("rev_", substr(b_hash, 1L, 16L))
  existing_ids <- vapply(history$revisions, function(r) r$revision_id, character(1))
  rev_id <- base_rev_id
  if (rev_id %in% existing_ids) {
    rev_id <- paste0(base_rev_id, "_", rev_num)
  }

  new_rev <- list(
    component_id = history$component_id,
    revision_id = rev_id,
    binding = binding,
    level = NULL,
    coverage = character(),
    basis_ids = character(),
    blockers = character(),
    runtime_deferred = NULL,
    review_unavailable = FALSE,
    created_at = created_at,
    events = list(
      list(
        type = "binding_activated",
        revision_id = rev_id,
        binding_hash = b_hash,
        created_at = created_at
      )
    )
  )

  history$revisions <- c(history$revisions, list(new_rev))
  history$active_revision_id <- rev_id
  history
}

#' Evaluate evidence across the upstream lineage of a target output
#'
#' Traverses the dependency graph upstream from a target output node/key, evaluates
#' current evidence across all contributing components, and reports summary evidence.
#' Explicitly ignores unrelated low evidence components that are outside the target's lineage.
#'
#' @param graph Dependency graph with `nodes` and `edges`.
#' @param histories Named list of component evidence histories keyed by component_id,
#'   or a list of component evidence history objects.
#' @param target_id Final output target key or identifier.
#' @return Named list representing the lineage evidence assessment:
#'   \describe{
#'     \item{target_id}{The target identifier evaluated.}
#'     \item{upstream_components}{Character vector of upstream component IDs.}
#'     \item{min_level}{Lowest evidence level achieved across upstream components, or NULL.}
#'     \item{review_unavailable}{Logical indicating if any upstream component has review unavailable.}
#'     \item{blockers}{Character vector of all active blockers in upstream lineage.}
#'     \item{runtime_deferred}{Named list of runtime deferred reasons for upstream components.}
#'     \item{is_blocked}{Logical indicating if the lineage is currently blocked.}
#'     \item{is_ready}{Logical indicating if all upstream components have verified evidence and no blockers.}
#'     \item{component_evidence}{Named list of active revision evidence per upstream component.}
#'   }
#' @noRd
evidence_for_output_lineage <- function(graph, histories, target_id) {
  if (is.null(target_id) || !is.character(target_id) || length(target_id) != 1L || !nzchar(target_id)) {
    cli::cli_abort("{.arg target_id} must be a non-empty string",
                   class = "sas2r_invalid_argument")
  }

  nodes <- if (is.list(graph) && !is.null(graph$nodes)) graph$nodes else tibble::tibble()
  edges <- if (is.list(graph) && !is.null(graph$edges)) graph$edges else tibble::tibble()

  # Normalize histories map (component_id -> history)
  hist_map <- list()
  if (is.list(histories)) {
    if (!is.null(names(histories)) && all(nzchar(names(histories)))) {
      hist_map <- histories
    } else {
      for (h in histories) {
        if (!is.null(h$component_id)) {
          hist_map[[h$component_id]] <- h
        }
      }
    }
  }

  # Find target nodes in graph
  norm_tgt <- tolower(target_id)
  target_node_ids <- character()

  if (nrow(nodes) > 0L) {
    match_idx <- which(
      tolower(nodes$component_id) == norm_tgt |
      tolower(nodes$node_id) == norm_tgt
    )
    if (length(match_idx) > 0L) {
      target_node_ids <- nodes$node_id[match_idx]
    }
  }

  if (length(target_node_ids) == 0L && nrow(edges) > 0L) {
    edge_match <- which(tolower(edges$detail) == norm_tgt)
    if (length(edge_match) > 0L) {
      target_node_ids <- unique(c(edges$from[edge_match], edges$to[edge_match]))
    }
  }

  # Backward BFS along incoming edges (to -> from)
  visited_nodes <- character()
  queue <- target_node_ids
  visited_nodes <- queue

  if (nrow(edges) > 0L && length(queue) > 0L) {
    while (length(queue) > 0L) {
      curr <- queue[1L]
      queue <- queue[-1L]
      incoming_from <- edges$from[edges$to == curr]
      new_nodes <- setdiff(incoming_from, visited_nodes)
      if (length(new_nodes) > 0L) {
        visited_nodes <- c(visited_nodes, new_nodes)
        queue <- c(queue, new_nodes)
      }
    }
  }

  # Extract upstream component IDs from visited nodes
  upstream_cids <- character()
  if (nrow(nodes) > 0L && length(visited_nodes) > 0L) {
    v_nodes <- nodes[nodes$node_id %in% visited_nodes, ]
    sched_nodes <- v_nodes[!v_nodes$type %in% c("final_output", "unresolved_dependency"), ]
    if (nrow(sched_nodes) > 0L) {
      upstream_cids <- unique(sched_nodes$component_id)
    }
  }

  # Also include transitive dependency closures for all discovered components
  if (length(upstream_cids) > 0L && is.list(graph)) {
    for (cid in upstream_cids) {
      cls <- dependency_closure(graph, cid)
      if (length(cls) > 0L) {
        upstream_cids <- unique(c(upstream_cids, cls))
      }
    }
  }

  # If graph had no nodes matching target, fallback to target as component
  if (length(upstream_cids) == 0L) {
    if (target_id %in% names(hist_map)) {
      upstream_cids <- target_id
      if (is.list(graph)) {
        cls <- dependency_closure(graph, target_id)
        upstream_cids <- unique(c(upstream_cids, cls))
      }
    }
  }

  # Stable order if graph schedule is available
  if (is.list(graph) && !is.null(graph$nodes) && nrow(graph$nodes) > 0L) {
    sched <- tryCatch(stable_dependency_schedule(graph), error = function(e) NULL)
    if (!is.null(sched) && nrow(sched) > 0L) {
      ordered_cids <- sched$component_id[sched$component_id %in% upstream_cids]
      remaining <- setdiff(upstream_cids, ordered_cids)
      upstream_cids <- c(ordered_cids, remaining)
    }
  }

  # Evaluate evidence for each upstream component
  comp_ev_map <- list()
  all_blockers <- character()
  all_deferred <- list()
  has_review_unavail <- FALSE
  levels <- character()
  has_unreviewed <- FALSE

  for (cid in upstream_cids) {
    h <- hist_map[[cid]]
    if (is.null(h)) {
      ev <- list(
        component_id = cid,
        revision_id = NA_character_,
        level = NULL,
        coverage = character(),
        basis_ids = character(),
        blockers = "missing_evidence_history",
        runtime_deferred = NULL,
        review_unavailable = TRUE
      )
      has_review_unavail <- TRUE
      has_unreviewed <- TRUE
      all_blockers <- unique(c(all_blockers, "missing_evidence_history"))
    } else {
      ev <- current_component_evidence(h)
      if (is.null(ev)) {
        ev <- list(
          component_id = cid,
          revision_id = NA_character_,
          level = NULL,
          coverage = character(),
          basis_ids = character(),
          blockers = "no_revisions",
          runtime_deferred = NULL,
          review_unavailable = TRUE
        )
        has_review_unavail <- TRUE
        has_unreviewed <- TRUE
        all_blockers <- unique(c(all_blockers, "no_revisions"))
      } else {
        if (isTRUE(ev$review_unavailable)) {
          has_review_unavail <- TRUE
        }
        if (length(ev$blockers) > 0L) {
          all_blockers <- unique(c(all_blockers, ev$blockers))
        }
        if (!is.null(ev$runtime_deferred) && nzchar(ev$runtime_deferred)) {
          all_deferred[[cid]] <- ev$runtime_deferred
        }
        if (is.null(ev$level)) {
          has_unreviewed <- TRUE
        } else {
          levels <- c(levels, ev$level)
        }
      }
    }
    comp_ev_map[[cid]] <- ev
  }

  # Compute min_level
  min_level <- NULL
  if (!has_unreviewed && length(levels) > 0L && length(levels) == length(upstream_cids)) {
    level_ranks <- match(levels, COMPONENT_EVIDENCE_LEVELS)
    if (!anyNA(level_ranks) && length(level_ranks) > 0L) {
      min_rank <- min(level_ranks)
      min_level <- COMPONENT_EVIDENCE_LEVELS[min_rank]
    }
  }

  is_blocked <- isTRUE(has_review_unavail) || length(all_blockers) > 0L
  is_ready <- !is_blocked && !is.null(min_level)

  list(
    target_id = target_id,
    upstream_components = upstream_cids,
    min_level = min_level,
    review_unavailable = has_review_unavail,
    blockers = all_blockers,
    runtime_deferred = all_deferred,
    is_blocked = is_blocked,
    is_ready = is_ready,
    component_evidence = comp_ev_map
  )
}
