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
  stateful <- startsWith(lineage$dataset, "work.") |
    identity %in% identity[lineage$role == "creates"]
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
        deferred = is.null(value) && uncertain && stateful[row])
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
      if (mutation || is_sql || dynamic) invalidate()
      read_rows(rows[lineage$role[rows] == "reads"], owner)
      for (s in seq_len(nrow(unit))) {
        at <- unit$line_start[s]
        include <- sites[sites$parent_unit_id == uid & sites$line == at, ]
        if (unit$first_token[s] == "%include") {
          if (!nrow(include)) invalidate()
          for (i in seq_len(nrow(include))) {
            if (include$status[i] == "resolved" && !is.na(include$target_file[i]))
              walk(include$target_file[i], owner, chain)
            else invalidate()
          }
        }
        if (any(calls$source_file == file & calls$line == at) ||
            grepl("\\bcall\\s+execute\\s*\\(", unit$text[s], ignore.case = TRUE)) invalidate()
        refs <- dataset_statement_refs(unit$text[s], unit$first_token[s], unit$unit_type[s])
        if (length(refs$creates) && any(grepl("[&%]", refs$creates))) invalidate()
      }
      # SQL with several writes and catalog mutations need statement-level
      # semantics; do not resurrect a possibly deleted/intermediate dataset.
      if (mutation || is_sql || dynamic) invalidate()
      else write_rows(rows[lineage$role[rows] == "creates"], owner)
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
