# Identify literal startup block boundaries, without evaluating conditions or
# expanding macros. Keep the scanner's file-wide macro_control flag unchanged:
# other consumers deliberately defer whole translation units on that evidence.
startup_library_scope <- function(statements) {
  local_unit <- match(statements$unit_id, unique(statements$unit_id))
  definition <- statements$unit_type == "macro_def"
  control <- rep(FALSE, nrow(statements))
  depth <- 0L
  supported <- TRUE
  for (i in which(!definition)) {
    control[i] <- depth > 0L
    if (!statements$first_token[i] %in% c("%if", "%else", "%do", "%end", "%goto", "%return", "%abort")) next
    text <- tolower(mask_strings(statements$text[i]))
    if (grepl("%(?:goto|return|abort)\\b", text, perl = TRUE)) supported <- FALSE
    tokens <- regmatches(text, gregexpr("%(?:do|end)\\b", text, perl = TRUE))[[1L]]
    # Inline conditional statements are not separate LIBNAME/include events in
    # the scanner. Defer that class instead of overlooking a possible write.
    if (statements$first_token[i] %in% c("%if", "%else") && !"%do" %in% tokens)
      supported <- FALSE
    for (token in tokens) {
      depth <- depth + if (token == "%do") 1L else -1L
      if (depth < 0L) supported <- FALSE
    }
  }
  list(definition = unique(local_unit[definition]),
    control = unique(local_unit[control]), supported = supported && depth == 0L)
}

defer_startup_bindings <- function(registry, statements, env_files) {
  frames <- registry$frames
  startup <- which(vapply(frames$key_prefix,
    function(key) key[1L] <= length(env_files), logical(1)))
  if (!length(startup)) return(registry)
  files <- unique(frames$file[startup])
  scopes <- stats::setNames(lapply(files, function(file)
    startup_library_scope(statements[statements$file == file, ])), files)
  events <- registry$events
  rows <- which(events$frame_index %in% frames$frame_index[startup])
  if (!all(vapply(scopes, `[[`, logical(1), "supported"))) {
    # Inline control, cross-file/unbalanced blocks and jumps are unsupported.
    # Defer the prologue as a class rather than interpreting control flow.
    events$conditional[rows] <- TRUE
    registry$startup_control_rows <- rows
  } else {
    definitions <- controls <- rep(FALSE, nrow(frames))
    control_rows <- integer()
    for (frame in startup) {
      parent <- frames$parent_frame[frame]
      if (!is.na(parent)) {
        site <- utils::tail(frames$key_prefix[[frame]], 1L)
        parent_scope <- scopes[[frames$file[parent]]]
        definitions[frame] <- definitions[parent] || site %in% parent_scope$definition
        controls[frame] <- !definitions[frame] &&
          (controls[parent] || site %in% parent_scope$control)
      }
      scope <- scopes[[frames$file[frame]]]
      own <- rows[events$frame_index[rows] == frames$frame_index[frame]]
      definition <- definitions[frame] | events$file_unit_id[own] %in% scope$definition
      control <- !definition & (controls[frame] | events$file_unit_id[own] %in% scope$control)
      events$conditional[own] <- events$conditional[own] | definition | control
      control_rows <- c(control_rows, own[control])
    }
    if (length(control_rows)) registry$startup_control_rows <- control_rows
  }
  registry$events <- events
  registry
}

# Possible library uses omitted from static lineage. Do not infer macro call
# order or expand names: literal prefixes identify a library, while a dynamic
# prefix can name any startup library. Dataset options are excluded by the
# shared dataset-position parser.
nonlineage_library_uses <- function(statements) {
  librefs <- character()
  dynamic <- FALSE
  rows <- which(statements$type == "code" &
    statements$first_token %in% dataset_candidate_tokens() &
    (statements$unit_type == "macro_def" | statements$first_token == "proc" |
      grepl("[&%]", statements$text)))
  for (row in rows) {
    token <- statements$first_token[row]
    macro <- statements$unit_type[row] == "macro_def"
    unit_type <- if (macro) {
      if (token %in% c("data", "set", "merge", "update")) "data_step" else "proc_step"
    } else statements$unit_type[row]
    refs <- dataset_statement_refs(statements$text[row], token, unit_type)
    datasets <- c(refs$creates, refs$reads)
    if (!macro) datasets <- datasets[grepl("[&%]", datasets)]
    # An explicit prefix remains known when only the member is dynamic.
    literal <- grepl("^[A-Za-z_]\\w*\\.", datasets)
    librefs <- c(librefs, sub("\\..*$", "", datasets[literal]))
    dynamic <- dynamic || any(grepl("[&%]", datasets[!literal]))
    if (token == "proc" && refs$proc %in% c("copy", "datasets")) {
      flat <- dataset_tokens(statements$text[row])$flat
      keys <- if (refs$proc == "copy") c("in", "out") else c("library", "lib")
      libraries <- unlist(lapply(keys, function(key) dataset_option_values(flat, key)),
        use.names = FALSE)
      librefs <- c(librefs, libraries[grepl("^[A-Za-z_]\\w*$", libraries)])
      dynamic <- dynamic || any(grepl("[&%]", libraries))
    }
  }
  list(librefs = setdiff(unique(tolower(librefs)), "work"), dynamic = dynamic)
}

# Reuse exact point-of-use records for static lineage. Other possible reads or
# writes use the startup projection, since their execution position is unknown.
startup_library_findings <- function(project, records) {
  findings <- tibble::tibble(kind = character(), detail = character())
  env_files <- project$dependency_facts$env_files
  if (!length(env_files)) return(findings)
  registry <- project$libref_registry
  extra <- nonlineage_library_uses(project$statements)
  if (extra$dynamic || length(extra$librefs)) {
    startup_records <- startup_libref_bindings(project)
    if (!extra$dynamic) startup_records <- startup_records[names(startup_records) %in% extra$librefs]
    records <- c(records, startup_records)
  }
  if (!length(records)) return(findings)
  frames <- registry$frames
  startup <- vapply(frames$key_prefix,
    function(key) key[1L] <= length(env_files), logical(1))
  occurrences <- stats::na.omit(frames$include_occurrence_id[startup])
  used <- unique(vapply(records, `[[`, character(1), "libref"))
  events <- registry$events
  conditional <- which(events$frame_index %in% frames$frame_index[startup] & events$conditional)
  # Conditional events outside open control are macro-definition events,
  # including inherited definition scope in includes. Macro calls are not
  # evaluated, so keep review for a definition that can change a used library.
  definitions <- conditional[!conditional %in% registry$startup_control_rows &
    events$libref[conditional] %in% c(used, "_all_")]
  deferred <- if (length(definitions))
    paste0(events$libref[definitions], " in ", events$file[definitions]) else character()
  for (record in records) {
    from_startup <- record$file %in% env_files ||
      record$include_occurrence_id %in% occurrences
    if (!from_startup) next
    # The selected event can be conditional even when the resolver reports a
    # more specific fallback reason, such as CLEAR or an unavailable path.
    uncertain <- any(events$file[conditional] %in% record$file &
      events$line[conditional] %in% record$line &
      events$libref[conditional] %in% c(record$libref, "_all_") &
      events$action[conditional] %in% record$action &
      events$path_expression[conditional] %in% record$source_path_expression &
      events$include_occurrence_id[conditional] %in% record$include_occurrence_id)
    if (uncertain) {
      deferred <- c(deferred, paste0(record$libref, " in ", record$file))
    }
    if (identical(record$status, "bound") && !isTRUE(record$context_truncated) &&
        identical(record$selection_origin, "source") &&
        !is.na(record$configured_path) &&
        !identical(include_scan_key(record$selected_path), include_scan_key(record$configured_path))) {
      findings <- rbind(findings, tibble::tibble(
        kind = "autoexec_library_shadows_config",
        detail = paste0(record$libref, ": startup path ", record$selected_path,
          " (", record$file, ":", record$line, ") takes precedence over configured fallback ",
          record$configured_path)))
    }
  }
  if (length(deferred)) findings <- rbind(findings, tibble::tibble(
    kind = "autoexec_bindings_deferred",
    detail = paste("Conditional startup bindings used or possibly used by dataset reads or writes require review;",
      "configured library fallbacks can supply them:", paste(unique(deferred), collapse = ", "))))
  unique(findings)
}
