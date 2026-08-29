INJECTION_PATTERNS <- c("ignore (all |previous |prior )?instructions",
                        "system\\s*\\(", "download\\.file", "unlink\\s*\\(")

#' Implementation for search_docs tool
#'
#' @param ctx Execution context containing config.
#' @return A function(args) implementing the search_docs tool.
#' @noRd
search_docs_impl <- function(ctx) {
  cfg <- ctx$config$search_docs
  function(args) {
    if (!isTRUE(cfg$enabled)) return(list(error = "search_docs_disabled"))
    # structured query: only these named fields, each sanitized to word chars
    terms <- unlist(args[c("construct", "package", "topic")], use.names = FALSE)
    terms <- tolower(gsub("[^A-Za-z0-9_. ]", "", terms))
    terms <- unlist(strsplit(terms[nzchar(terms)], "\\s+"))
    if (!length(terms)) return(list(error = "empty_query"))
    if (!identical(cfg$backend, "mirror"))
      return(list(error = "backend_unavailable"))
    files <- list.files(cfg$mirror_dir, pattern = "\\.(md|txt)$",
                        full.names = TRUE)
    scored <- lapply(files, function(f) {
      txt <- tolower(paste(readLines(f, warn = FALSE), collapse = " "))
      score <- sum(vapply(terms, function(t)
        lengths(regmatches(txt, gregexpr(t, txt, fixed = TRUE))), integer(1)))
      list(file = f, score = score,
           text = paste(readLines(f, warn = FALSE), collapse = " "))
    })
    scored <- Filter(function(s) s$score > 0, scored)
    scored <- scored[order(-vapply(scored, `[[`, numeric(1), "score"))]
    quarantined <- 0L
    keep <- list()
    for (s in scored[seq_len(min(3L, length(scored)))]) {
      bad <- any(vapply(INJECTION_PATTERNS, function(p)
        grepl(p, tolower(s$text), perl = TRUE), logical(1)))
      if (bad) quarantined <- quarantined + 1L
      else keep[[length(keep) + 1L]] <- list(file = basename(s$file),
                                             text = s$text)
    }
    list(untrusted = TRUE, snippets = keep, quarantined = quarantined)
  }
}
