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
