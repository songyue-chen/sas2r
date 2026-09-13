doc_validate_config <- function(file) {
  llm <- yaml::read_yaml(file)$llm
  credential_envs <- if (!is.null(llm$provider)) {
    sas2r:::llm_provider_spec(llm$provider)$credential_envs
  } else character()
  if (!length(credential_envs)) return(sas2r::sas_config(file))
  # These are configuration checks, not authentication or provider calls. Supply
  # fixture credentials from the registry so developer keys cannot mask a missing
  # test prerequisite. Keep the published YAML unchanged and restore the caller's
  # environment after the check, including when configuration validation fails.
  withr::with_envvar(stats::setNames(
    rep("sas2r-offline-docs-placeholder", length(credential_envs)), credential_envs
  ), sas2r::sas_config(file))
}

# One fence grammar for the documentation contract tests and executable runner.
doc_code_blocks <- function(lines, file = "<text>", all_languages = FALSE) {
  lines <- strsplit(paste(lines, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
  blocks <- list()
  i <- 1L
  while (i <= length(lines)) {
    opener <- regmatches(lines[i], regexec("^\\s*(`{3,})\\s*(.*)$", lines[i]))[[1L]]
    if (!length(opener)) { i <- i + 1L; next }
    close <- paste0("^\\s*`{", nchar(opener[2L]), ",}\\s*$")
    ends <- which(seq_along(lines) > i & grepl(close, lines))
    if (!length(ends)) stop("Unclosed code fence in ", file, ":", i)
    end <- ends[1L]
    header <- trimws(opener[3L])
    language <- if (grepl("^(\\{\\s*[rR][ ,}]|[rR]$)", header)) "r" else tolower(header)
    if (all_languages || language %in% c("r", "yaml")) {
      code <- if (end == i + 1L) "" else paste(lines[seq.int(i + 1L, end - 1L)], collapse = "\n")
      blocks[[length(blocks) + 1L]] <- list(file = file, line = i, end_line = end, language = language,
        marker = if (i > 1L) lines[i - 1L] else "", code = code)
    }
    i <- end + 1L
  }
  blocks
}

doc_r_chunks <- function(lines) {
  blocks <- Filter(function(b) b$language == "r", doc_code_blocks(lines))
  vapply(blocks, `[[`, character(1), "code")
}

doc_prose_calls <- function(lines) {
  code <- unlist(lapply(doc_code_blocks(lines, all_languages = TRUE),
    function(block) seq.int(block$line, block$end_line)))
  prose <- paste(lines[setdiff(seq_along(lines), code)], collapse = "\n")
  hits <- unlist(regmatches(
    prose, gregexpr("`[A-Za-z._][A-Za-z0-9._]*\\(\\)`", prose)))
  unique(gsub("[`()]", "", hits))
}
