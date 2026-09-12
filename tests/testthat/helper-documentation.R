# One fence grammar for the documentation contract tests and executable runner.
doc_code_blocks <- function(lines, file = "<text>") {
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
    if (language %in% c("r", "yaml")) {
      code <- if (end == i + 1L) "" else paste(lines[seq.int(i + 1L, end - 1L)], collapse = "\n")
      blocks[[length(blocks) + 1L]] <- list(file = file, line = i, language = language,
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
