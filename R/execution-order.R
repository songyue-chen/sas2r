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
    cli::cli_abort(c(
      "migration.execution_order must name each executable root program exactly once.",
      if (length(missing)) c("x" = paste("Missing:", paste(missing, collapse = ", "))),
      if (length(unknown)) c("x" = paste("Not an executable root in this source scope:", paste(unknown, collapse = ", "))),
      if (length(repeated)) c("x" = paste("Repeated:", paste(repeated, collapse = ", "))),
      "i" = "List root program paths; setup, called macros, and included modules execute through their existing roles."
    ), class = "sas2r_config_error")
  }
  roots[match(keys, root_keys)]
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
        deferred = is.null(value) && uncertain)
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
    for (uid in unique(code$unit_id)) {
      unit <- code[code$unit_id == uid, ]
      rows <- which(lineage$unit_id == uid)
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
  for (row in which(lineage$role == "reads")) {
    occurrences <- Filter(function(e) e$row == row, events)
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
