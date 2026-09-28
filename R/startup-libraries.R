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
