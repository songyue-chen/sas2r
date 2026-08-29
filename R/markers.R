#' Locate unit block boundaries in staged R code
#'
#' @param txt Character vector of file lines.
#' @param unit_id Integer unit ID.
#' @return Named integer vector c(start = ..., end = ...) or integer(0) if not found.
#' @noRd
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

#' Extract unit code block from staged R file
#'
#' @param staged_file Path to staged .R file.
#' @param unit_id Integer unit ID.
#' @return Character vector of lines for the unit.
#' @noRd
extract_unit_block <- function(staged_file, unit_id) {
  if (is.null(staged_file) || is.na(staged_file) || !file.exists(staged_file)) {
    cli::cli_abort("staged file not found for unit {.val {unit_id}}",
                   class = "sas2r_unit_not_found")
  }
  txt <- readLines(staged_file, warn = FALSE)
  loc <- locate_unit_block(txt, unit_id)
  if (!length(loc)) {
    cli::cli_abort("no staged block found for unit {.val {unit_id}} in {.file {staged_file}}",
                   class = "sas2r_unit_not_found")
  }
  txt[loc["start"]:loc["end"]]
}

#' Renumber or normalize unit ID markers in an archived code block
#'
#' Rewrites unit ID references in anchor and boundary comments to `new_uid`.
#' If the block is an anchor-less legacy archive, prepends the anchor.
#'
#' @param lines Character vector of code block lines.
#' @param new_uid Integer target unit ID.
#' @return Character vector of normalized block lines.
#' @noRd
renumber_unit_block <- function(lines, new_uid) {
  uid <- as.integer(new_uid)
  if (length(lines) == 0L) return(sprintf("# --- sas2r:unit %d ---", uid))
  out <- lines
  # 1. Update existing anchor if present, otherwise prepend anchor
  has_anchor <- any(grepl("^# --- sas2r:unit [0-9]+ ---$", out))
  if (has_anchor) {
    out <- sub("^# --- sas2r:unit [0-9]+ ---$", sprintf("# --- sas2r:unit %d ---", uid), out)
  } else {
    out <- c(sprintf("# --- sas2r:unit %d ---", uid), out)
  }
  # 2. Update llm_authored, untranslated, and end marker unit IDs
  out <- sub("^# sas2r:llm_authored unit=[0-9]+", sprintf("# sas2r:llm_authored unit=%d", uid), out)
  out <- sub("^# sas2r:untranslated unit=[0-9]+", sprintf("# sas2r:untranslated unit=%d", uid), out)
  out <- sub("^# sas2r:end unit=[0-9]+$", sprintf("# sas2r:end unit=%d", uid), out)
  out
}

#' Splice unit code block into staged R file
#'
#' @param staged_file Path to staged .R file.
#' @param unit_id Integer unit ID.
#' @param code Translated R code string or replacement block lines.
#' @param kind Marker kind: 'llm' (adds llm_authored/end), 'verbatim' (uses code as-is).
#' @return Invisible staged file path.
#' @noRd
splice_unit_code <- function(staged_file, unit_id, code, kind = c("llm", "verbatim")) {
  kind <- match.arg(kind)
  if (!file.exists(staged_file)) {
    cli::cli_abort("staged file {.file {staged_file}} does not exist",
                   class = "sas2r_splice_error")
  }
  txt <- readLines(staged_file, warn = FALSE)
  loc <- locate_unit_block(txt, unit_id)
  if (!length(loc)) {
    cli::cli_abort("no block for unit {unit_id} in {.file {staged_file}}",
                   class = "sas2r_splice_error")
  }
  # Split only a single string. unlist(strsplit(<vector>)) silently drops
  # empty elements, which would make a verbatim restore lose blank lines and
  # break the approved-artifact digest.
  code_lines <- if (is.character(code) && length(code) == 1L) {
    strsplit(code, "\r?\n")[[1]]
  } else {
    as.character(code)
  }
  new_block <- if (kind == "llm") {
    c(sprintf("# --- sas2r:unit %d ---", as.integer(unit_id)),
      sprintf("# sas2r:llm_authored unit=%d -- REVIEW REQUIRED", as.integer(unit_id)),
      code_lines,
      sprintf("# sas2r:end unit=%d", as.integer(unit_id)))
  } else {
    renumber_unit_block(code_lines, as.integer(unit_id))
  }
  txt <- append(txt[-(loc["start"]:loc["end"])], new_block, after = loc["start"] - 1L)
  writeLines(txt, staged_file)
  invisible(staged_file)
}

#' @noRd
splice_agent_code <- function(staged_file, unit_id, code,
                              kind = c("llm", "verbatim")) {
  kind <- match.arg(kind)
  if (identical(kind, "verbatim")) {
    splice_unit_code(staged_file, unit_id, code, kind = kind)
    return(invisible(character()))
  }
  preserved <- character()
  if (file.exists(staged_file)) {
    txt <- readLines(staged_file, warn = FALSE)
    loc <- locate_unit_block(txt, unit_id)
    if (length(loc)) {
      block <- txt[loc[["start"]]:loc[["end"]]]
      preserved <- block[grepl("^# sas2r:untranslated include\\b", block)]
    }
  }
  code_lines <- if (is.character(code) && length(code) == 1L) {
    strsplit(code, "\r?\n")[[1]]
  } else {
    as.character(code)
  }
  keep <- preserved[!preserved %in% code_lines]
  splice_unit_code(staged_file, unit_id, c(keep, code_lines), kind = kind)
  invisible(preserved)
}

