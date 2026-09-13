#' Build the typed SAS dependency graph
#'
#' Projects facts from a `sas2r_project` into a typed graph of nodes and
#' dependency edges.
#'
#' @param project A `sas2r_project` object.
#' @param output_contracts Optional tibble or list of output target contracts.
#' @return A named list representing the dependency graph:
#' \describe{
#'   \item{schema_version}{Schema version string ("1").}
#'   \item{nodes}{Tibble of graph nodes with columns `node_id`, `component_id`,
#'     `type`, `source_file`, `line`, `original_index`, `content_hash`.}
#'   \item{edges}{Tibble of graph edges with columns `edge_id`, `from`, `to`,
#'     `type`, `resolution`, `source_file`, `line`, `detail`.}
#' }
#' @noRd
build_dependency_graph <- function(project, output_contracts = NULL, producers = dataset_producers(project)) {
  if (!inherits(project, "sas2r_project")) {
    cli::cli_abort("{.arg project} must be a sas2r_project object.",
                   class = "sas2r_invalid_argument")
  }

  units <- project$units
  stmts <- project$statements
  facts <- project$dependency_facts %||% list()
  lineage <- project$lineage %||% facts$lineage %||% tibble::tibble()
  macros <- project$macros %||% facts$macros %||% list()
  defs <- macros$defs %||% tibble::tibble()
  calls <- macros$calls %||% tibble::tibble()
  resolution <- macros$resolution %||% tibble::tibble()
  inc_occ <- if (!is.null(project$include_graph)) {
    project$include_graph$occurrences %||% tibble::tibble()
  } else if (!is.null(facts$include_graph)) {
    facts$include_graph$occurrences %||% tibble::tibble()
  } else {
    tibble::tibble()
  }
  fmt_facts <- facts$formats %||% list(defs = tibble::tibble(), uses = tibble::tibble())
  fn_facts <- facts$functions %||% list(defs = tibble::tibble(), uses = tibble::tibble())

  node_list <- list()
  edge_list <- list()

  # Helper: unit text content hash
  unit_hash <- function(uid) {
    u_stmts <- stmts[stmts$unit_id == uid, ]
    if (nrow(u_stmts) == 0L) return(migration_hash(""))
    migration_hash(paste(u_stmts$text, collapse = ";"))
  }

  component_ids <- project_component_ids(units$file, units$origin, project$project_dir)

  macro_units <- called_macro_units(project)
  collisions <- intersect(macro_units$component_id,
                          component_ids[units$origin != "macro_search_path"])
  if (length(collisions)) {
    cli::cli_abort("Source program names collide with called macro component IDs: {.val {collisions}}. Rename the source programs.",
                   class = "sas2r_component_id_collision")
  }
  for (i in seq_len(nrow(macro_units))) {
    component_ids[units$unit_id == macro_units$unit_id[i]] <- macro_units$component_id[i]
  }

  # 1. Setup and Source Unit nodes
  unit_node_map <- list() # maps unit_id -> node_id
  file_first_unit_map <- list() # maps file -> node_id

  if (nrow(units) > 0L) {
    for (i in seq_len(nrow(units))) {
      u_id <- as.integer(units$unit_id[i])
      f <- as.character(units$file[i])
      orig <- as.character(units$origin[i])
      l_start <- as.integer(units$line_start[i])
      is_env <- identical(orig, "environment") || tolower(basename(f)) == "autoexec.sas"
      n_type <- if (is_env) "setup" else if (orig == "macro_search_path") "macro" else "source_unit"
      c_id <- component_ids[i]

      n_id <- paste0("node_", if (is_env) "setup_" else "unit_",
                     substr(migration_hash(list(file = f, line = l_start, unit_id = u_id, type = n_type)), 1L, 16L))
      c_hash <- unit_hash(u_id)

      unit_node_map[[as.character(u_id)]] <- n_id
      if (is.null(file_first_unit_map[[f]])) {
        file_first_unit_map[[f]] <- n_id
      }

      node_list[[length(node_list) + 1L]] <- list(
        node_id = n_id,
        component_id = c_id,
        type = n_type,
        source_file = f,
        line = l_start,
        original_index = u_id,
        content_hash = c_hash
      )
    }
  }

  # Helper: register or get external input node
  external_nodes <- list()
  get_external_node <- function(ds_name, identity = ds_name) {
    norm_name <- tolower(ds_name)
    n_id <- paste0("node_input_", substr(migration_hash(list(type = "external_input", name = identity)), 1L, 16L))
    if (is.null(external_nodes[[identity]])) {
      external_nodes[[identity]] <<- n_id
      node_list[[length(node_list) + 1L]] <<- list(
        node_id = n_id,
        component_id = norm_name,
        type = "external_input",
        source_file = NA_character_,
        line = NA_integer_,
        original_index = NA_integer_,
        content_hash = migration_hash(list(type = "external_input", name = identity))
      )
    }
    n_id
  }

  # Helper: register or get unresolved dependency node
  unresolved_nodes <- list()
  get_unresolved_node <- function(detail, source_file, line, type_detail = "unresolved") {
    key <- paste(detail, source_file, line, type_detail, sep = "\u001f")
    n_id <- paste0("node_unresolved_", substr(migration_hash(list(type = "unresolved_dependency", detail = detail, file = source_file, line = line)), 1L, 16L))
    if (is.null(unresolved_nodes[[key]])) {
      unresolved_nodes[[key]] <<- n_id
      node_list[[length(node_list) + 1L]] <<- list(
        node_id = n_id,
        component_id = detail,
        type = "unresolved_dependency",
        source_file = as.character(source_file),
        line = as.integer(line),
        original_index = NA_integer_,
        content_hash = migration_hash(list(type = "unresolved_dependency", detail = detail, file = source_file, line = line))
      )
    }
    n_id
  }

  # Helper: register or get final output node
  output_nodes <- list()
  get_output_node <- function(target_key, source_file = NA_character_, line = NA_integer_) {
    norm_key <- tolower(target_key)
    n_id <- paste0("node_output_", substr(migration_hash(list(type = "final_output", target = norm_key)), 1L, 16L))
    if (is.null(output_nodes[[norm_key]])) {
      output_nodes[[norm_key]] <<- n_id
      node_list[[length(node_list) + 1L]] <<- list(
        node_id = n_id,
        component_id = norm_key,
        type = "final_output",
        source_file = as.character(source_file),
        line = as.integer(line),
        original_index = NA_integer_,
        content_hash = migration_hash(list(type = "final_output", target = norm_key))
      )
    }
    n_id
  }

  # Find unit node by file and line
  find_unit_node_at <- function(file, line) {
    if (is.null(file) || is.na(file) || !nzchar(file)) return(NULL)
    f_units <- units[units$file == file, ]
    if (nrow(f_units) == 0L) return(NULL)
    if (is.na(line)) return(unit_node_map[[as.character(f_units$unit_id[1])]])
    match_u <- f_units[f_units$line_start <= line & f_units$line_end >= line, ]
    if (nrow(match_u) > 0L) {
      return(unit_node_map[[as.character(match_u$unit_id[1])]])
    }
    # Closest unit
    dists <- abs(f_units$line_start - line)
    best_idx <- which.min(dists)
    unit_node_map[[as.character(f_units$unit_id[best_idx])]]
  }

  # 2. Add edges: setup_before
  setup_nodes <- vapply(node_list[vapply(node_list, function(n) n$type == "setup", logical(1))],
                        `[[`, character(1), "node_id")
  source_unit_nodes <- vapply(node_list[vapply(node_list, function(n) n$type == "source_unit", logical(1))],
                              `[[`, character(1), "node_id")

  if (length(setup_nodes) > 0L && length(source_unit_nodes) > 0L) {
    # Distinct program files
    prog_files <- unique(units$file[units$origin != "environment"])
    for (s_id in setup_nodes) {
      s_node <- node_list[[which(vapply(node_list, function(n) n$node_id == s_id, logical(1)))[1]]]
      for (pf in prog_files) {
        first_n_id <- file_first_unit_map[[pf]]
        if (!is.null(first_n_id)) {
          edge_list[[length(edge_list) + 1L]] <- list(
            from = s_id,
            to = first_n_id,
            type = "setup_before",
            resolution = "resolved",
            source_file = s_node$source_file,
            line = s_node$line,
            detail = "setup_before"
          )
        }
      }
    }
  }

  # 3. Add edges: includes
  if (nrow(inc_occ) > 0L) {
    for (k in seq_len(nrow(inc_occ))) {
      p_file <- inc_occ$parent_file[k]
      p_uid <- inc_occ$parent_unit_id[k]
      tgt_expr <- inc_occ$target_expression[k]
      tgt_file <- inc_occ$target_file[k]
      st <- inc_occ$status[k]
      l_val <- inc_occ$line[k]

      consumer_id <- unit_node_map[[as.character(p_uid)]] %||% find_unit_node_at(p_file, l_val)
      if (is.null(consumer_id)) next

      if (identical(st, "resolved") && !is.na(tgt_file) && nzchar(tgt_file)) {
        provider_id <- file_first_unit_map[[tgt_file]] %||% find_unit_node_at(tgt_file, 1L)
        if (!is.null(provider_id)) {
          edge_list[[length(edge_list) + 1L]] <- list(
            from = provider_id,
            to = consumer_id,
            type = "includes",
            resolution = "resolved",
            source_file = p_file,
            line = l_val,
            detail = tgt_expr
          )
        }
      } else if (identical(st, "dynamic")) {
        unres_id <- get_unresolved_node(tgt_expr, p_file, l_val, "dynamic_include")
        edge_list[[length(edge_list) + 1L]] <- list(
          from = unres_id,
          to = consumer_id,
          type = "includes",
          resolution = "dynamic",
          source_file = p_file,
          line = l_val,
          detail = tgt_expr
        )
      } else {
        unres_id <- get_unresolved_node(tgt_expr, p_file, l_val, "unresolved_include")
        edge_list[[length(edge_list) + 1L]] <- list(
          from = unres_id,
          to = consumer_id,
          type = "includes",
          resolution = "unresolved",
          source_file = p_file,
          line = l_val,
          detail = tgt_expr
        )
      }
    }
  }

  # 4. Add edges: calls_macro
  if (nrow(resolution) > 0L) {
    for (k in seq_len(nrow(resolution))) {
      m_name <- resolution$name[k]
      m_st <- resolution$status[k]
      src_file <- resolution$source_file[k]
      l_val <- resolution$line[k]
      def_src <- resolution$source[k]

      consumer_id <- find_unit_node_at(src_file, l_val)
      if (is.null(consumer_id)) next

      if (identical(m_st, "resolved_project")) {
        def_matches <- defs[defs$name == m_name, ]
        if (nrow(def_matches) > 0L) {
          def_row <- def_matches[1, ]
          provider_id <- unit_node_map[[as.character(def_row$unit_id)]] %||%
            find_unit_node_at(def_row$file, def_row$line_start)
          if (!is.null(provider_id)) {
            edge_list[[length(edge_list) + 1L]] <- list(
              from = provider_id,
              to = consumer_id,
              type = "calls_macro",
              resolution = "resolved",
              source_file = src_file,
              line = l_val,
              detail = m_name
            )
          }
        }
      } else if (identical(m_st, "resolved_path") || identical(m_st, "resolved_content")) {
        definition <- defs[include_scan_key(defs$file) == include_scan_key(def_src) & defs$name == m_name, , drop = FALSE]
        provider_id <- if (nrow(definition)) unit_node_map[[as.character(definition$unit_id[1L])]] else NULL
        if (is.null(provider_id)) {
          # External macro definition file
          provider_id <- get_external_node(paste0("macro:", m_name))
        }
        edge_list[[length(edge_list) + 1L]] <- list(
          from = provider_id,
          to = consumer_id,
          type = "calls_macro",
          resolution = "resolved",
          source_file = src_file,
          line = l_val,
          detail = m_name
        )
      } else if (identical(m_st, "dynamic")) {
        unres_id <- get_unresolved_node(m_name, src_file, l_val, "dynamic_macro")
        edge_list[[length(edge_list) + 1L]] <- list(
          from = unres_id,
          to = consumer_id,
          type = "calls_macro",
          resolution = "dynamic",
          source_file = src_file,
          line = l_val,
          detail = m_name
        )
      } else {
        unres_id <- get_unresolved_node(m_name, src_file, l_val, "unresolved_macro")
        edge_list[[length(edge_list) + 1L]] <- list(
          from = unres_id,
          to = consumer_id,
          type = "calls_macro",
          resolution = "unresolved",
          source_file = src_file,
          line = l_val,
          detail = m_name
        )
      }
    }
  }

  # 5. Each read depends on its selected producer, using the same bound
  # dataset identity as preflight. Superseded writers are not dependencies.
  node_ids <- unlist(unit_node_map, use.names = FALSE)[match(lineage$unit_id, units$unit_id)]
  used_writers <- integer()
  for (row in which(lineage$role == "reads")) {
    consumer_id <- node_ids[row]
    if (is.na(consumer_id)) next
    writer <- producers$writer[row]
    ds <- lineage$dataset[row]
    if (!is.na(writer)) {
      provider_id <- node_ids[writer]
      if (is.na(provider_id)) next
      used_writers <- c(used_writers, writer)
      edge_list[[length(edge_list) + 1L]] <- list(
        from = provider_id, to = consumer_id, type = "writes_dataset",
        resolution = "resolved", source_file = lineage$file[writer],
        line = lineage$line[writer], detail = ds)
    } else {
      provider_id <- get_external_node(ds, producers$identity[row])
    }
    edge_list[[length(edge_list) + 1L]] <- list(
      from = provider_id, to = consumer_id, type = "reads_dataset",
      resolution = if (is.na(writer)) "external" else "resolved",
      source_file = lineage$file[row], line = lineage$line[row], detail = ds)
  }
  for (row in setdiff(which(lineage$role == "creates"), used_writers)) {
    provider_id <- node_ids[row]
    if (is.na(provider_id)) next
    ds <- lineage$dataset[row]
    edge_list[[length(edge_list) + 1L]] <- list(
      from = provider_id, to = get_output_node(ds, lineage$file[row], lineage$line[row]),
      type = "writes_dataset", resolution = "resolved", source_file = lineage$file[row],
      line = lineage$line[row], detail = ds)
  }

  # 6. Add edges: uses_format
  fmt_defs <- fmt_facts$defs %||% tibble::tibble()
  fmt_uses <- fmt_facts$uses %||% tibble::tibble()
  if (nrow(fmt_uses) > 0L) {
    for (f_idx in seq_len(nrow(fmt_uses))) {
      f_name <- fmt_uses$name[f_idx]
      f_file <- fmt_uses$file[f_idx]
      f_line <- fmt_uses$line[f_idx]
      f_uid <- fmt_uses$unit_id[f_idx]

      consumer_id <- unit_node_map[[as.character(f_uid)]] %||% find_unit_node_at(f_file, f_line)
      if (is.null(consumer_id)) next

      # Check if defined in project
      match_defs <- fmt_defs[fmt_defs$name == f_name, ]
      if (nrow(match_defs) > 0L) {
        provider_id <- unit_node_map[[as.character(match_defs$unit_id[1])]] %||%
          find_unit_node_at(match_defs$file[1], match_defs$line[1])
        if (!is.null(provider_id)) {
          edge_list[[length(edge_list) + 1L]] <- list(
            from = provider_id,
            to = consumer_id,
            type = "uses_format",
            resolution = "resolved",
            source_file = f_file,
            line = f_line,
            detail = f_name
          )
        }
      } else if (grepl("&", f_name)) {
        unres_id <- get_unresolved_node(f_name, f_file, f_line, "dynamic_format")
        edge_list[[length(edge_list) + 1L]] <- list(
          from = unres_id,
          to = consumer_id,
          type = "uses_format",
          resolution = "dynamic",
          source_file = f_file,
          line = f_line,
          detail = f_name
        )
      } else {
        # External format
        ext_id <- get_external_node(paste0("format.", f_name))
        edge_list[[length(edge_list) + 1L]] <- list(
          from = ext_id,
          to = consumer_id,
          type = "uses_format",
          resolution = "external",
          source_file = f_file,
          line = f_line,
          detail = f_name
        )
      }
    }
  }

  # 7. Add edges: uses_function
  fn_defs <- fn_facts$defs %||% tibble::tibble()
  fn_uses <- fn_facts$uses %||% tibble::tibble()
  if (nrow(fn_uses) > 0L) {
    for (fn_idx in seq_len(nrow(fn_uses))) {
      fn_name <- fn_uses$name[fn_idx]
      fn_file <- fn_uses$file[fn_idx]
      fn_line <- fn_uses$line[fn_idx]
      fn_uid <- fn_uses$unit_id[fn_idx]

      consumer_id <- unit_node_map[[as.character(fn_uid)]] %||% find_unit_node_at(fn_file, fn_line)
      if (is.null(consumer_id)) next

      match_defs <- fn_defs[fn_defs$name == fn_name, ]
      if (nrow(match_defs) > 0L) {
        provider_id <- unit_node_map[[as.character(match_defs$unit_id[1])]] %||%
          find_unit_node_at(match_defs$file[1], match_defs$line[1])
        if (!is.null(provider_id)) {
          edge_list[[length(edge_list) + 1L]] <- list(
            from = provider_id,
            to = consumer_id,
            type = "uses_function",
            resolution = "resolved",
            source_file = fn_file,
            line = fn_line,
            detail = fn_name
          )
        }
      } else {
        unres_id <- get_unresolved_node(fn_name, fn_file, fn_line, "unresolved_function")
        edge_list[[length(edge_list) + 1L]] <- list(
          from = unres_id,
          to = consumer_id,
          type = "uses_function",
          resolution = "unresolved",
          source_file = fn_file,
          line = fn_line,
          detail = fn_name
        )
      }
    }
  }

  # 8. Output contracts (if provided or present on project)
  if (is.null(output_contracts) && !is.null(project$output_contracts)) {
    output_contracts <- project$output_contracts
  }
  if (!is.null(output_contracts) && nrow(output_contracts) > 0L) {
    for (k_idx in seq_len(nrow(output_contracts))) {
      t_key <- output_contracts$target_key[k_idx]
      t_kind <- output_contracts$kind[k_idx]
      t_file <- output_contracts$source_file[k_idx]
      t_line <- output_contracts$line[k_idx]
      t_res <- output_contracts$resolution[k_idx]
      t_prod_node <- output_contracts$producer_node_id[k_idx]

      out_node_id <- get_output_node(t_key, t_file, t_line)

      provider_id <- if (!is.na(t_prod_node) && nzchar(t_prod_node) && t_prod_node %in% unlist(unit_node_map)) {
        t_prod_node
      } else if (!is.na(t_file) && nzchar(t_file)) {
        find_unit_node_at(t_file, t_line)
      } else if (t_kind == "dataset" && nrow(lineage) > 0L) {
        c_rows <- lineage[lineage$role == "creates" & lineage$dataset == t_key, ]
        if (nrow(c_rows) > 0L) {
          unit_node_map[[as.character(c_rows$unit_id[nrow(c_rows)])]]
        } else {
          NULL
        }
      } else {
        NULL
      }

      if (!is.null(provider_id)) {
        edge_list[[length(edge_list) + 1L]] <- list(
          from = provider_id,
          to = out_node_id,
          type = if (t_kind == "dataset") "writes_dataset" else "writes_output",
          resolution = t_res,
          source_file = t_file,
          line = t_line,
          detail = t_key
        )
      }
    }
  }

  # Build tibbles
  nodes_df <- if (length(node_list) > 0L) {
    tibble::tibble(
      node_id = vapply(node_list, `[[`, character(1), "node_id"),
      component_id = vapply(node_list, `[[`, character(1), "component_id"),
      type = vapply(node_list, `[[`, character(1), "type"),
      source_file = vapply(node_list, function(n) as.character(n$source_file %||% NA_character_), character(1)),
      line = vapply(node_list, function(n) as.integer(n$line %||% NA_integer_), integer(1)),
      original_index = vapply(node_list, function(n) as.integer(n$original_index %||% NA_integer_), integer(1)),
      content_hash = vapply(node_list, `[[`, character(1), "content_hash")
    )
  } else {
    tibble::tibble(
      node_id = character(), component_id = character(), type = character(),
      source_file = character(), line = integer(), original_index = integer(),
      content_hash = character()
    )
  }

  # Deduplicate nodes by node_id
  if (nrow(nodes_df) > 0L) {
    nodes_df <- nodes_df[!duplicated(nodes_df$node_id), ]
  }

  edges_df <- if (length(edge_list) > 0L) {
    from_vec <- vapply(edge_list, `[[`, character(1), "from")
    to_vec <- vapply(edge_list, `[[`, character(1), "to")
    type_vec <- vapply(edge_list, `[[`, character(1), "type")
    res_vec <- vapply(edge_list, `[[`, character(1), "resolution")
    src_vec <- vapply(edge_list, function(e) as.character(e$source_file %||% ""), character(1))
    line_vec <- vapply(edge_list, function(e) as.integer(e$line %||% 0L), integer(1))
    detail_vec <- vapply(edge_list, function(e) as.character(e$detail %||% ""), character(1))

    edge_ids <- vapply(seq_along(edge_list), function(idx) {
      paste0("edge_", substr(migration_hash(list(
        from = from_vec[idx], to = to_vec[idx], type = type_vec[idx],
        res = res_vec[idx], file = src_vec[idx], line = line_vec[idx], detail = detail_vec[idx]
      )), 1L, 16L))
    }, character(1))

    raw_edges <- tibble::tibble(
      edge_id = edge_ids,
      from = from_vec,
      to = to_vec,
      type = type_vec,
      resolution = res_vec,
      source_file = src_vec,
      line = line_vec,
      detail = detail_vec
    )
    raw_edges[!duplicated(raw_edges$edge_id), ]
  } else {
    tibble::tibble(
      edge_id = character(), from = character(), to = character(),
      type = character(), resolution = character(), source_file = character(),
      line = integer(), detail = character()
    )
  }

  list(
    schema_version = "1",
    nodes = nodes_df,
    edges = edges_df
  )
}

#' Compute stable dependency schedule
#'
#' Schedules graph components in stable dependency order using deterministic
#' Tarjan SCC grouping and Kahn topological sorting with original_index as tie breaker.
#'
#' @param graph A dependency graph from `build_dependency_graph()`.
#' @return A tibble with columns `component_id`, `group_id`, `group_kind`,
#'   `sequence`, `original_index`, and `unresolved_dependencies`.
#' @noRd
stable_dependency_schedule <- function(graph) {
  if (!is.list(graph) || is.null(graph$nodes) || is.null(graph$edges)) {
    cli::cli_abort("{.arg graph} must be a dependency graph.",
                   class = "sas2r_invalid_argument")
  }

  nodes <- graph$nodes
  edges <- graph$edges

  empty_schedule <- function() {
    tibble::tibble(
      component_id = character(),
      group_id = character(),
      group_kind = character(),
      sequence = integer(),
      original_index = integer(),
      unresolved_dependencies = list()
    )
  }

  if (nrow(nodes) == 0L) return(empty_schedule())

  # Filter to schedulable component nodes
  sched_nodes <- nodes[!nodes$type %in% c("external_input", "final_output", "unresolved_dependency"), ]
  if (nrow(sched_nodes) == 0L) return(empty_schedule())

  cids <- unique(sched_nodes$component_id)
  comp_orig_idx <- stats::setNames(vector("integer", length(cids)), cids)
  for (cid in cids) {
    sub_nodes <- sched_nodes[sched_nodes$component_id == cid, ]
    min_idx <- suppressWarnings(min(sub_nodes$original_index, na.rm = TRUE))
    comp_orig_idx[[cid]] <- if (is.finite(min_idx)) as.integer(min_idx) else as.integer(match(cid, cids))
  }

  # Sort components by original index
  cids <- cids[order(unname(comp_orig_idx[cids]))]

  # Map node_id -> component_id
  node_to_comp <- stats::setNames(sched_nodes$component_id, sched_nodes$node_id)

  # Build adjacency list (provider -> consumer)
  adj <- stats::setNames(vector("list", length(cids)), cids)
  for (cid in cids) adj[[cid]] <- character()

  if (nrow(edges) > 0L) {
    valid_edges <- edges[edges$from %in% names(node_to_comp) & edges$to %in% names(node_to_comp), ]
    if (nrow(valid_edges) > 0L) {
      from_c <- unname(node_to_comp[valid_edges$from])
      to_c <- unname(node_to_comp[valid_edges$to])
      for (i in seq_len(nrow(valid_edges))) {
        u <- from_c[i]
        v <- to_c[i]
        if (!identical(u, v)) {
          adj[[u]] <- unique(c(adj[[u]], v))
        }
      }
    }
  }

  # Sort neighbors deterministically by original_index
  for (cid in cids) {
    if (length(adj[[cid]]) > 0L) {
      n_idxs <- vapply(adj[[cid]], function(m) comp_orig_idx[[m]], integer(1))
      adj[[cid]] <- adj[[cid]][order(n_idxs)]
    }
  }

  # Deterministic Tarjan SCC algorithm
  index_counter <- 0L
  indices <- list()
  lowlink <- list()
  on_stack <- list()
  stack <- character()
  scc_list <- list()

  strongconnect <- function(v) {
    index_counter <<- index_counter + 1L
    indices[[v]] <<- index_counter
    lowlink[[v]] <<- index_counter
    stack <<- c(stack, v)
    on_stack[[v]] <<- TRUE

    neighbors <- adj[[v]] %||% character()
    for (w in neighbors) {
      if (is.null(indices[[w]])) {
        strongconnect(w)
        lowlink[[v]] <<- min(lowlink[[v]], lowlink[[w]])
      } else if (isTRUE(on_stack[[w]])) {
        lowlink[[v]] <<- min(lowlink[[v]], indices[[w]])
      }
    }

    if (lowlink[[v]] == indices[[v]]) {
      scc <- character()
      repeat {
        w <- stack[length(stack)]
        stack <<- stack[-length(stack)]
        on_stack[[w]] <<- FALSE
        scc <- c(scc, w)
        if (identical(w, v)) break
      }
      member_idxs <- vapply(scc, function(m) comp_orig_idx[[m]], integer(1))
      scc_sorted <- scc[order(member_idxs)]
      scc_list[[length(scc_list) + 1L]] <<- scc_sorted
    }
  }

  for (cid in cids) {
    if (is.null(indices[[cid]])) {
      strongconnect(cid)
    }
  }

  # Condensation DAG and Kahn's algorithm
  n_sccs <- length(scc_list)
  scc_grp_id <- character(n_sccs)
  scc_grp_kind <- character(n_sccs)
  scc_orig_idx <- integer(n_sccs)
  comp_to_scc <- list()

  for (k in seq_len(n_sccs)) {
    members <- scc_list[[k]]
    is_cycle <- length(members) > 1L
    if (is_cycle) {
      scc_grp_kind[k] <- "cycle"
    } else {
      m <- members[1L]
      has_outgoing <- length(adj[[m]]) > 0L
      has_incoming <- any(vapply(cids, function(other) m %in% adj[[other]], logical(1)))
      scc_grp_kind[k] <- if (has_outgoing || has_incoming) "singleton" else "independent"
    }
    scc_grp_id[k] <- paste0("group_", paste(members, collapse = "_"))
    scc_orig_idx[k] <- min(vapply(members, function(m) comp_orig_idx[[m]], integer(1)))
    for (m in members) {
      comp_to_scc[[m]] <- k
    }
  }

  scc_adj <- vector("list", n_sccs)
  scc_indegree <- integer(n_sccs)
  for (k in seq_len(n_sccs)) scc_adj[[k]] <- integer()

  for (u in cids) {
    u_scc <- comp_to_scc[[u]]
    for (v in adj[[u]]) {
      v_scc <- comp_to_scc[[v]]
      if (u_scc != v_scc && !v_scc %in% scc_adj[[u_scc]]) {
        scc_adj[[u_scc]] <- c(scc_adj[[u_scc]], v_scc)
        scc_indegree[v_scc] <- scc_indegree[v_scc] + 1L
      }
    }
  }

  ready_sccs <- which(scc_indegree == 0L)
  ordered_sccs <- integer()

  while (length(ready_sccs) > 0L) {
    ready_orig_idxs <- vapply(ready_sccs, function(idx) scc_orig_idx[idx], integer(1))
    best_pos <- which.min(ready_orig_idxs)
    chosen_scc <- ready_sccs[best_pos]
    ready_sccs <- ready_sccs[-best_pos]
    ordered_sccs <- c(ordered_sccs, chosen_scc)

    for (downstream_scc in scc_adj[[chosen_scc]]) {
      scc_indegree[downstream_scc] <- scc_indegree[downstream_scc] - 1L
      if (scc_indegree[downstream_scc] == 0L) {
        ready_sccs <- c(ready_sccs, downstream_scc)
      }
    }
  }

  # Build scheduled components
  out_cids <- character()
  out_grp_ids <- character()
  out_grp_kinds <- character()
  out_orig_idxs <- integer()

  for (k in ordered_sccs) {
    members <- scc_list[[k]]
    grp_id <- scc_grp_id[k]
    grp_kind <- scc_grp_kind[k]
    for (m in members) {
      out_cids <- c(out_cids, m)
      out_grp_ids <- c(out_grp_ids, grp_id)
      out_grp_kinds <- c(out_grp_kinds, grp_kind)
      out_orig_idxs <- c(out_orig_idxs, comp_orig_idx[[m]])
    }
  }

  # Compute unresolved dependencies per component
  unres_list <- stats::setNames(vector("list", length(out_cids)), out_cids)
  for (m in out_cids) {
    m_node_ids <- sched_nodes$node_id[sched_nodes$component_id == m]
    m_edges <- edges[edges$to %in% m_node_ids, ]
    if (nrow(m_edges) > 0L) {
      unres_edges <- m_edges[m_edges$resolution %in% c("unresolved", "dynamic", "ambiguous") |
                               m_edges$from %in% nodes$node_id[nodes$type == "unresolved_dependency"], ]
      details <- unres_edges$detail[nzchar(unres_edges$detail)]
      node_details <- nodes$component_id[nodes$node_id %in% unres_edges$from & nodes$type == "unresolved_dependency"]
      all_unres <- unique(c(details, node_details))
      all_unres <- all_unres[!is.na(all_unres) & nzchar(all_unres)]
      unres_list[[m]] <- if (length(all_unres) > 0L) all_unres else character()
    } else {
      unres_list[[m]] <- character()
    }
  }

  tibble::tibble(
    component_id = out_cids,
    group_id = out_grp_ids,
    group_kind = out_grp_kinds,
    sequence = seq_along(out_cids),
    original_index = out_orig_idxs,
    unresolved_dependencies = unres_list[out_cids]
  )
}

#' Compute complete transitive upstream dependency closure for a component
#'
#' Traverses upstream provider dependencies across all multi-hop chains
#' until convergence.
#'
#' @param graph A dependency graph object or list with `nodes` and `edges`.
#' @param component_id The component ID to compute dependencies for.
#' @return A character vector of all upstream component IDs that this component depends on.
#' @noRd
dependency_closure <- function(graph, component_id) {
  if (is.null(graph) || is.null(graph$nodes) || is.null(graph$edges) || nrow(graph$nodes) == 0L) {
    return(character())
  }
  sched_nodes <- graph$nodes[!graph$nodes$type %in% c("external_input", "final_output", "unresolved_dependency"), ]
  if (nrow(sched_nodes) == 0L) return(character())
  node_to_comp <- stats::setNames(sched_nodes$component_id, sched_nodes$node_id)
  cids <- unique(sched_nodes$component_id)

  if (!component_id %in% cids) return(character())

  # Build direct provider map for all components
  direct_providers <- stats::setNames(vector("list", length(cids)), cids)
  for (cid in cids) direct_providers[[cid]] <- character()

  if (nrow(graph$edges) > 0L) {
    valid_edges <- graph$edges[graph$edges$from %in% names(node_to_comp) & graph$edges$to %in% names(node_to_comp), ]
    if (nrow(valid_edges) > 0L) {
      from_c <- unname(node_to_comp[valid_edges$from])
      to_c <- unname(node_to_comp[valid_edges$to])
      for (i in seq_len(nrow(valid_edges))) {
        u <- from_c[i]
        v <- to_c[i]
        if (!identical(u, v)) {
          direct_providers[[v]] <- unique(c(direct_providers[[v]], u))
        }
      }
    }
  }

  # Breadth-first / fixpoint traversal of all upstream ancestors
  visited <- character()
  queue <- direct_providers[[component_id]] %||% character()
  visited <- queue

  while (length(queue) > 0L) {
    curr <- queue[1L]
    queue <- queue[-1L]
    curr_provs <- direct_providers[[curr]] %||% character()
    new_provs <- setdiff(curr_provs, c(visited, component_id))
    if (length(new_provs) > 0L) {
      visited <- c(visited, new_provs)
      queue <- c(queue, new_provs)
    }
  }

  if (length(visited) == 0L) return(character())

  sched <- tryCatch(stable_dependency_schedule(graph), error = function(e) NULL)
  if (!is.null(sched) && nrow(sched) > 0L) {
    ordered_cids <- sched$component_id[sched$component_id %in% visited]
    return(unique(ordered_cids))
  }

  # Fallback ordering by original index
  comp_orig_idx <- stats::setNames(vector("integer", length(cids)), cids)
  for (cid in cids) {
    sub_nodes <- sched_nodes[sched_nodes$component_id == cid, ]
    min_idx <- suppressWarnings(min(sub_nodes$original_index, na.rm = TRUE))
    comp_orig_idx[[cid]] <- if (is.finite(min_idx)) as.integer(min_idx) else as.integer(match(cid, cids))
  }
  visited[order(unname(comp_orig_idx[visited]))]
}

#' Compute closure hashes for components
#'
#' Computes a hash binding for each component's dependency closure, including
#' its own revision hash, hashes of its dependencies, helper_hash, and prompt_skill_hash.
#'
#' @param graph A dependency graph.
#' @param selected_revision_hashes Named character vector of component revision hashes.
#' @param helper_hash Optional hash of helper functions.
#' @param prompt_skill_hash Optional hash of prompt/skills.
#' @return A named character vector of closure hashes.
#' @noRd
dependency_closure_hashes <- function(graph, selected_revision_hashes, helper_hash = NULL, prompt_skill_hash = NULL) {
  sched <- stable_dependency_schedule(graph)
  cids <- if (nrow(sched) > 0L) sched$component_id else names(selected_revision_hashes) %||% character()

  res <- vapply(cids, function(cid) {
    deps <- dependency_closure(graph, cid)
    dep_hashes <- as.list(selected_revision_hashes[intersect(deps, names(selected_revision_hashes))])
    self_hash <- if (cid %in% names(selected_revision_hashes)) selected_revision_hashes[[cid]] else ""
    payload <- list(
      component_id = cid,
      revision = self_hash,
      dependencies = dep_hashes,
      helper_hash = helper_hash %||% "",
      prompt_skill_hash = prompt_skill_hash %||% ""
    )
    migration_hash(payload)
  }, character(1))

  stats::setNames(res, cids)
}

#' Determine components that need to be requeued
#'
#' Evaluates which components should be requeued based on changed revision hashes,
#' changed dependency closures, or availability of runtime_deferred prerequisites.
#'
#' @param graph A dependency graph.
#' @param old_hashes Named character vector of previous revision hashes.
#' @param new_hashes Named character vector of new revision hashes.
#' @param runtime_deferred Optional character vector of component IDs that were deferred.
#' @return Character vector of component IDs to requeue, in schedule order.
#' @noRd
requeue_components <- function(graph, old_hashes, new_hashes, runtime_deferred = character()) {
  sched <- stable_dependency_schedule(graph)
  if (nrow(sched) == 0L) return(character())

  cids <- sched$component_id
  if (length(old_hashes) == 0L) {
    active_cids <- intersect(cids, names(new_hashes))
    return(if (length(active_cids) > 0L) active_cids else cids)
  }

  requeue <- character()
  for (cid in cids) {
    old_self <- if (cid %in% names(old_hashes)) old_hashes[[cid]] else NA_character_
    new_self <- if (cid %in% names(new_hashes)) new_hashes[[cid]] else NA_character_
    self_changed <- !identical(old_self, new_self) && !is.na(new_self)

    deps <- dependency_closure(graph, cid)
    deps_changed <- FALSE
    if (length(deps) > 0L) {
      for (d in deps) {
        old_d <- if (d %in% names(old_hashes)) old_hashes[[d]] else NA_character_
        new_d <- if (d %in% names(new_hashes)) new_hashes[[d]] else NA_character_
        if (!identical(old_d, new_d)) {
          deps_changed <- TRUE
          break
        }
      }
    }

    deferred_ready <- cid %in% runtime_deferred && (length(deps) == 0L || all(deps %in% names(new_hashes)))

    if (self_changed || deps_changed || deferred_ready) {
      requeue <- c(requeue, cid)
    }
  }

  intersect(cids, requeue)
}


# Basenames remain readable IDs unless two physical files would collide.
project_component_ids <- function(files, origins, root = ".") {
  ids <- tools::file_path_sans_ext(basename(files))
  unique_files <- unique(files)
  basenames <- tools::file_path_sans_ext(basename(unique_files))
  collision_files <- unique_files[duplicated(basenames) | duplicated(basenames, fromLast = TRUE)]
  for (file in collision_files) ids[files == file] <- paste0(
    tools::file_path_sans_ext(basename(file)), "__", substr(cli::hash_sha256(
      canonical_include_staged_path(file, root)), 1L, 8L))
  ids[origins == "environment" | tolower(basename(files)) == "autoexec.sas"] <- "setup"
  ids
}

# One producer policy for graph and preflight. WORK is shared by the bundle;
# permanent libraries additionally require the same point-of-use directory.
dataset_producers <- function(project, effective = effective_librefs(project)) {
  lineage <- project$lineage
  n <- nrow(lineage)
  paths <- rep(NA_character_, n)
  if ("binding_id" %in% names(lineage)) {
    paths <- effective$bindings$selected_path[match(lineage$binding_id, effective$bindings$binding_id)]
    paths[is.na(lineage$binding_status) | lineage$binding_status != "bound"] <- NA_character_
  }
  paths[startsWith(lineage$dataset, "work.")] <- "<session work>"
  identity <- paste(lineage$dataset, paths, sep = "\r")
  # Unknown bindings retain name-based ordering, but no usable path. Preflight
  # still reports them as unresolved; a guessed dependency is not availability.
  candidates <- which(lineage$role == "creates")
  index <- split(candidates, identity[candidates])
  writer <- rep(NA_integer_, n)
  backward <- rep(FALSE, n)
  groups <- match(identity, names(index))
  unit_ids <- lineage$unit_id
  files <- lineage$file
  for (row in which(lineage$role == "reads")) {
    if (is.na(groups[row])) next
    matches <- index[[groups[row]]]
    matches <- matches[unit_ids[matches] != unit_ids[row]]
    same_file <- files[matches] == files[row]
    prior <- matches[same_file & unit_ids[matches] < unit_ids[row]]
    other <- matches[!same_file]
    if (length(prior)) writer[row] <- utils::tail(prior, 1L)
    else if (length(other)) {
      before <- other[unit_ids[other] < unit_ids[row]]
      writer[row] <- if (length(before)) utils::tail(before, 1L) else other[1L]
    } else if (length(matches)) {
      writer[row] <- matches[1L]
      backward[row] <- TRUE
    }
  }
  list(path = paths, identity = identity, writer = writer, backward = backward)
}
