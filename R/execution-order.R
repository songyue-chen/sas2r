# Explicit order names executable roots, not every scanned include or macro.
execution_root_files <- function(project) {
  units <- project$units
  roots <- unique(units$file[units$origin == "program" &
    tolower(basename(units$file)) != "autoexec.sas"])
  occurrences <- project$include_graph$occurrences
  included <- occurrences$target_file[occurrences$status == "resolved"]
  roots[!include_scan_key(roots) %in% include_scan_key(included)]
}

configured_execution_files <- function(project) {
  declared <- project$config$migration$execution_order
  if (is.null(declared)) return(NULL)
  roots <- execution_root_files(project)
  keys <- include_scan_key(declared)
  root_keys <- include_scan_key(roots)
  missing <- roots[!root_keys %in% keys]
  unknown <- declared[!keys %in% root_keys]
  repeated <- declared[duplicated(keys)]
  if (length(missing) || length(unknown) || length(repeated)) {
    describe <- function(paths) paste0(paste(utils::head(paths, 5L), collapse = ", "),
      if (length(paths) > 5L) paste0(" (and ", length(paths) - 5L, " more)"))
    cli::cli_abort(c(
      "migration.execution_order must name each executable root program exactly once.",
      if (length(missing)) c("x" = paste("Missing:", describe(missing))),
      if (length(unknown)) c("x" = paste("Not an executable root in this source scope:", describe(unknown))),
      if (length(repeated)) c("x" = paste("Repeated:", describe(repeated))),
      "i" = "Paths are relative to the YAML configuration file, or to the scanned project directory for an in-memory configuration.",
      "i" = paste("Scanned project directory:", project$project_dir),
      "i" = "List root program paths; setup, called macros, and included modules execute through their existing roles.",
      "i" = "Full paths are retained in the error's missing_roots, unknown_roots and repeated_roots fields."
    ), class = "sas2r_config_error", missing_roots = missing,
       unknown_roots = unknown, repeated_roots = repeated)
  }
  roots[match(keys, root_keys)]
}

# Recognize only simple variable-setting macros called with literal arguments.
# This is a syntax whitelist, not macro expansion or a side-effect denylist.
# Any emitted text, indirect value, nested call or control flow remains unknown.
macro_preserves_datasets <- function(project, call) {
  resolution <- project$macros$resolution
  resolved <- resolution[resolution$call_id == call$call_id, ]
  if (nrow(resolved) != 1L || !resolved$status %in%
      c("resolved_project", "resolved_path", "resolved_content")) return(FALSE)
  defs <- project$macros$defs
  def <- defs[defs$name == call$name & defs$file == resolved$source, ]
  if (nrow(def) != 1L) return(FALSE)
  comments <- project$comments
  if (any(comments$unit_id %in% def$unit_id & comments$kind == "statement" &
          grepl("%[A-Za-z_&]", comments$text))) return(FALSE)
  literal <- "[A-Za-z0-9_,= .+-]*"
  if (!grepl(paste0("^%[A-Za-z_][A-Za-z0-9_]*(\\(", literal, "\\))?$"),
             trimws(call$call_text)) ||
      !grepl(paste0("^", literal, "$"), def$params)) return(FALSE)
  contract <- tryCatch(parse_macro_contract(def$name, def$params), error = function(e) NULL)
  if (is.null(contract)) return(FALSE)
  body <- project$statements[project$statements$unit_id == def$unit_id, ]
  if (sum(body$first_token == "%macro") != 1L || sum(body$first_token == "%mend") != 1L)
    return(FALSE)
  for (i in seq_len(nrow(body))) {
    text <- trimws(body$text[i])
    token <- body$first_token[i]
    if (token == "%macro") {
      if (!grepl(paste0("^%macro\\s+[A-Za-z_][A-Za-z0-9_]*(\\(", literal, "\\))?$"),
                 text, ignore.case = TRUE)) return(FALSE)
    } else if (token == "%mend") {
      if (!grepl("^%mend(\\s+[A-Za-z_][A-Za-z0-9_]*)?$", text, ignore.case = TRUE)) return(FALSE)
    } else if (token %in% c("%global", "%local")) {
      if (!grepl("^%(global|local)\\s+[A-Za-z_][A-Za-z0-9_]*(\\s+[A-Za-z_][A-Za-z0-9_]*)*$",
                 text, ignore.case = TRUE)) return(FALSE)
    } else if (token == "%let") {
      for (param in contract$parameters$name)
        text <- gsub(paste0("&", param, "\\b\\.?"), "VALUE", text, ignore.case = TRUE)
      if (!grepl("^%let\\s+[A-Za-z_][A-Za-z0-9_]*\\s*=\\s*[A-Za-z0-9_ .+-]*$",
                 text, ignore.case = TRUE)) return(FALSE)
    } else return(FALSE)
  }
  TRUE
}

# Literal outputs of reachable resolved macros are possible writes, not known
# producers. Resolve their libraries at the outer call site, where the macro
# runs; do not promote the macro body into ordinary executable lineage. Keep
# results by source statement so the ordered walk admits them only at that site.
macro_possible_dataset_writes <- function(project, include_work = FALSE) {
  statements <- project$statements
  resolution <- project$macros$resolution
  defs <- project$macros$defs
  if (!nrow(resolution) || !any(statements$unit_type == "macro_def" &
      statements$first_token %in% dataset_candidate_tokens())) return(list())
  keys <- paste(defs$file, defs$name, sep = "\r")
  target <- match(paste(resolution$source, resolution$name, sep = "\r"), keys)
  ambiguous <- which(duplicated(keys) | duplicated(keys, fromLast = TRUE))
  target[target %in% ambiguous | !resolution$status %in%
    c("resolved_project", "resolved_path", "resolved_content") |
    duplicated(resolution$call_id) | duplicated(resolution$call_id, fromLast = TRUE)] <- NA_integer_
  targets <- function(calls) unique(stats::na.omit(target[match(calls$call_id, resolution$call_id)]))
  rows_by_unit <- split(seq_len(nrow(statements)), statements$unit_id)
  # Parse each definition once, including its resolved nested-call targets.
  bodies <- lapply(seq_len(nrow(defs)), function(d) {
    body <- statements[rows_by_unit[[as.character(defs$unit_id[d])]], ]
    datasets <- unique(unlist(lapply(seq_len(nrow(body)), function(i) {
      token <- body$first_token[i]
      refs <- dataset_statement_refs(body$text[i], token,
        if (token == "data") "data_step" else "proc_step")
      norm_ds(static_dataset_names(refs$creates))
    }), use.names = FALSE))
    list(outputs = if (include_work) datasets else datasets[!startsWith(datasets, "work.")],
      calls = targets(extract_macro_calls(body)))
  })
  output_cache <- vector("list", nrow(defs))
  outputs <- function(d) {
    if (is.null(output_cache[[d]])) {
      pending <- d
      visited <- integer()
      while (length(pending)) {
        next_defs <- setdiff(pending, visited)
        if (!length(next_defs)) break
        visited <- c(visited, next_defs)
        pending <- unique(unlist(lapply(bodies[next_defs], `[[`, "calls"), use.names = FALSE))
      }
      output_cache[[d]] <<- unique(unlist(lapply(bodies[visited], `[[`, "outputs"),
                                         use.names = FALSE)) %||% character()
    }
    output_cache[[d]]
  }
  writing_defs <- Filter(function(d) length(outputs(d)) > 0L, unique(stats::na.omit(target)))
  sites <- resolution[target %in% writing_defs, ]
  # Most setup calls have no dataset effects to collect. Use their resolved
  # summaries to skip them; parse candidate statements to distinguish calls
  # sharing a source line before assigning results to statement IDs.
  site_keys <- paste(sites$source_file, sites$line, sep = "\r")
  rows <- which(statements$unit_type != "macro_def" &
    paste(statements$file, statements$line_start, sep = "\r") %in% site_keys)
  writes <- lapply(rows, function(row) {
    call <- statements[row, ]
    datasets <- unique(unlist(lapply(targets(extract_macro_calls(call)), outputs), use.names = FALSE))
    if (!length(datasets)) return(character())
    bindings <- libref_point_of_use_records(project$libref_registry,
      sub("\\..*$", "", datasets), rep(call$file, length(datasets)), rep(call$line_start, length(datasets)))
    unique(unlist(lapply(seq_along(datasets), function(i) {
      if (startsWith(datasets[i], "work.")) return(paste(datasets[i], "<session work>", sep = "\r"))
      binding <- bindings$records[[bindings$slot[i]]]
      if (identical(binding$status, "bound")) paste(datasets[i], binding$selected_path, sep = "\r")
    }), use.names = FALSE))
  })
  names(writes) <- paste(statements$file[rows], statements$stmt_id[rows], sep = "\r")
  writes[lengths(writes) > 0L]
}

# Context candidates only: this does not promote macro outputs to known writes.
# Resolved, non-writing setup code is not a possible WORK producer. Unknown
# emitted text, dataset names, includes or nested calls remain possible effects.
macro_unknown_dataset_effects <- function(project) {
  defs <- project$macros$defs
  resolution <- project$macros$resolution
  target <- function(call) {
    rows <- resolution[resolution$call_id == call$call_id, ]
    if (nrow(rows) != 1L || !rows$status %in% c("resolved_project", "resolved_path", "resolved_content"))
      return(NA_integer_)
    ids <- which(defs$file == rows$source & defs$name == rows$name)
    if (length(ids) == 1L) ids else NA_integer_
  }
  cache <- vector("list", nrow(defs))
  unknown <- function(id, visited = integer()) {
    if (is.na(id) || id %in% visited) return(TRUE)
    if (!is.null(cache[[id]])) return(cache[[id]])
    body <- project$statements[project$statements$unit_id == defs$unit_id[id], ]
    nested <- extract_macro_calls(body)
    value <- any(body$macro_control %in% TRUE) ||
      any(body$first_token == "%include") ||
      any(grepl("\\bcall\\s+execute\\s*\\(|^proc\\s+datasets\\b", body$text, ignore.case = TRUE)) ||
      any(grepl("^[&]", trimws(body$text)))
    if (!value) for (i in seq_len(nrow(body))) {
      refs <- dataset_statement_refs(body$text[i], body$first_token[i],
        if (body$first_token[i] == "data") "data_step" else "proc_step")
      if (any(grepl("[&%]", refs$creates))) { value <- TRUE; break }
    }
    if (!value && nrow(nested)) value <- any(vapply(seq_len(nrow(nested)), function(i)
      unknown(target(nested[i, ]), c(visited, id)), logical(1)))
    cache[[id]] <<- value
    value
  }
  calls <- project$macros$calls
  stats::setNames(vapply(seq_len(nrow(calls)), function(i) unknown(target(calls[i, ])), logical(1)), calls$call_id)
}

# Walk existing statements and resolved include sites in execution order.
# No macro expansion: an unmodeled effect invalidates previous producer claims.
# Occurrences remain separate so a reused include can consume different states.
ordered_dataset_producers <- function(project, paths, identity) {
  roots <- configured_execution_files(project)
  lineage <- project$lineage
  statements <- project$statements
  sites <- project$include_graph$occurrences
  calls <- project$macros$calls
  if (nrow(calls)) calls <- calls[!vapply(seq_len(nrow(calls)), function(i)
    macro_preserves_datasets(project, calls[i, ]), logical(1)), ]
  is_work <- startsWith(lineage$dataset, "work.")
  possible <- macro_possible_dataset_writes(project)
  context_writes <- macro_possible_dataset_writes(project, include_work = TRUE)
  unknown_calls <- macro_unknown_dataset_effects(project)
  possible_owners <- list()
  unknown_owners <- character()
  remember <- function(keys, owner) {
    for (key in keys) possible_owners[[key]] <<- unique(c(owner, possible_owners[[key]]))
  }
  possible_before <- function(key) {
    owners <- union(possible_owners[[key]], unknown_owners)
    ordered <- c(project$dependency_facts$env_files, roots)
    owners[order(match(owners, ordered), decreasing = TRUE)]
  }
  seen <- character()
  lineage_rows <- split(seq_len(nrow(lineage)), lineage$unit_id)
  current <- list()
  uncertain <- FALSE
  events <- list()
  invalidate <- function() {
    current <<- list()
    uncertain <<- TRUE
  }
  read_rows <- function(rows, owner) {
    for (row in rows) {
      value <- current[[identity[row]]]
      events[[length(events) + 1L]] <<- list(row = row,
        writer = if (is.null(value)) NA_integer_ else value$row,
        reader_root = owner, writer_root = if (is.null(value)) NA_character_ else value$owner,
        deferred = is.null(value) && uncertain && (is_work[row] || identity[row] %in% seen),
        possible_writers = if (is.null(value) && uncertain && is_work[row])
          setdiff(possible_before(identity[row]), owner) else character())
    }
  }
  write_rows <- function(rows, owner) {
    for (row in rows) {
      if (!is.na(paths[row])) current[[identity[row]]] <<- list(row = row, owner = owner)
    }
  }
  walk <- function(file, owner, chain = character()) {
    key <- include_scan_key(file)
    if (key %in% chain || length(chain) >= INCLUDE_MAX_DEPTH) {
      unknown_owners <<- union(owner, unknown_owners)
      invalidate()
      return(invisible(NULL))
    }
    chain <- c(chain, key)
    code <- statements[statements$file == file & statements$unit_type != "macro_def", ]
    units <- split(seq_len(nrow(code)), factor(code$unit_id, levels = unique(code$unit_id)))
    for (indices in units) {
      unit <- code[indices, ]
      uid <- unit$unit_id[1L]
      rows <- lineage_rows[[as.character(uid)]] %||% integer()
      is_data <- identical(unit$unit_type[1L], "data_step")
      # DATA-step outputs become visible only after all input reads.
      # Other multi-statement procedures are deferred as a class: the lineage
      # extractor does not retain statement IDs for same-line references.
      is_sql <- any(unit$first_token == "proc" & grepl("^proc\\s+sql\\b", unit$text, ignore.case = TRUE)) &&
        (sum(!unit$first_token %in% c("proc", "run", "quit")) > 1L ||
         any(grepl("^(update|delete|insert|alter|drop)\\b", unit$text, ignore.case = TRUE)))
      mutation <- any(grepl("^proc\\s+datasets\\b", unit$text, ignore.case = TRUE))
      dynamic <- any(unit$macro_control %in% TRUE) ||
        any(calls$source_file == file & calls$line %in% unit$line_start) ||
        any(grepl("\\bcall\\s+execute\\s*\\(", unit$text, ignore.case = TRUE)) ||
        (is_data && any(unit$first_token == "%include"))
      unit_calls <- calls[calls$source_file == file & calls$line %in% unit$line_start, ]
      if (any(unknown_calls[unit_calls$call_id] %in% TRUE) ||
          any(unit$macro_control %in% TRUE) || mutation ||
          any(grepl("\\bcall\\s+execute\\s*\\(", unit$text, ignore.case = TRUE)) ||
          (is_data && any(unit$first_token == "%include")))
        unknown_owners <<- union(owner, unknown_owners)
      if (mutation || is_sql || dynamic) invalidate()
      read_rows(rows[lineage$role[rows] == "reads"], owner)
      for (s in seq_len(nrow(unit))) {
        at <- unit$line_start[s]
        include <- sites[sites$parent_unit_id == uid & sites$line == at, ]
        if (unit$first_token[s] == "%include") {
          if (!nrow(include)) { invalidate(); unknown_owners <<- union(owner, unknown_owners) }
          for (i in seq_len(nrow(include))) {
            if (include$status[i] == "resolved" && !is.na(include$target_file[i]))
              walk(include$target_file[i], owner, chain)
            else { invalidate(); unknown_owners <<- union(owner, unknown_owners) }
          }
        }
        if (any(calls$source_file == file & calls$line == at) ||
            grepl("\\bcall\\s+execute\\s*\\(", unit$text[s], ignore.case = TRUE)) invalidate()
        refs <- dataset_statement_refs(unit$text[s], unit$first_token[s], unit$unit_type[s])
        if (length(refs$creates) && any(grepl("[&%]", refs$creates))) {
          invalidate(); unknown_owners <<- union(owner, unknown_owners)
        }
      }
      # SQL with several writes and catalog mutations need statement-level
      # semantics; do not resurrect a possibly deleted/intermediate dataset.
      if (mutation || is_sql || dynamic) invalidate()
      else write_rows(rows[lineage$role[rows] == "creates"], owner)
      # Only the completed prefix can explain an uncertain permanent read.
      # Even an invalidating unit can contain a possible (not known) write.
      created <- rows[lineage$role[rows] == "creates" & !is.na(paths[rows])]
      keys <- paste(file, unit$stmt_id, sep = "\r")
      remember(c(identity[created], unlist(context_writes[intersect(keys, names(context_writes))],
        use.names = FALSE)), owner)
      seen <<- c(seen, identity[created], unlist(possible[intersect(keys, names(possible))], use.names = FALSE))
    }
    invisible(NULL)
  }
  for (file in project$dependency_facts$env_files %||% character()) walk(file, file)
  for (file in roots) walk(file, file)
  n <- nrow(lineage)
  writer <- rep(NA_integer_, n)
  generated <- deferred <- rep(FALSE, n)
  events_by_row <- split(events, factor(vapply(events, `[[`, integer(1), "row"),
                                       levels = seq_len(n)))
  for (row in which(lineage$role == "reads")) {
    occurrences <- events_by_row[[row]]
    if (!length(occurrences)) {
      events[[length(events) + 1L]] <- list(row = row, writer = NA_integer_,
        reader_root = NA_character_, writer_root = NA_character_, deferred = TRUE)
      deferred[row] <- TRUE
      next
    }
    writers <- vapply(occurrences, `[[`, integer(1), "writer")
    generated[row] <- all(!is.na(writers))
    if (length(unique(writers)) == 1L) writer[row] <- writers[1L]
    deferred[row] <- any(vapply(occurrences, `[[`, logical(1), "deferred"))
  }
  list(path = paths, identity = identity, writer = writer,
    backward = rep(FALSE, n), generated = generated, deferred = deferred,
    events = events, execution_files = roots)
}
