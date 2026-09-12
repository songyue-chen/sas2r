semantic_frame <- function(path) {
  frame <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                          na.strings = "", blank.lines.skip = FALSE)
  # All-missing numeric columns otherwise get inferred as logical by read.csv.
  for (nm in names(frame)) if (is.logical(frame[[nm]]) && all(is.na(frame[[nm]]))) frame[[nm]] <- as.numeric(frame[[nm]])
  frame
}

# Used by the offline tests and the explicit SAS collection verifier. Synthetic
# files in verifier tests test the verifier only, never establish SAS evidence.
semantic_reference_audit <- function(root, generated = file.path(root, "sas-generated")) {
  cases <- jsonlite::read_json(file.path(root, "manifest.json"))
  rows <- lapply(cases, function(case) {
    path <- file.path(generated, paste0(case$id, ".csv"))
    if (!file.exists(path)) return(data.frame(id = case$id, status = "missing", detail = path))
    difference <- tryCatch({
      semantic_reference_difference(path, file.path(root, case$id, "expected.csv"))
    }, error = function(e) conditionMessage(e))
    data.frame(id = case$id, status = if (isTRUE(difference)) "passed" else "failed",
               detail = if (isTRUE(difference)) path else paste(difference, collapse = "; "))
  })
  coverage <- do.call(rbind, rows)
  provenance <- file.path(generated, "provenance.txt")
  log <- file.path(generated, "sas.log")
  read_evidence <- function(path) if (file.exists(path)) readLines(path, warn = FALSE) else character()
  provenance_text <- read_evidence(provenance)
  log_text <- read_evidence(log)
  evidence_missing <- character()
  if (!length(provenance_text) || !any(nzchar(trimws(provenance_text)))) evidence_missing <- c(evidence_missing, provenance)
  if (!length(log_text) || !any(nzchar(trimws(log_text)))) evidence_missing <- c(evidence_missing, log)
  errors <- grep("^\\s*ERROR( [0-9]+-[0-9]+)?:", log_text, value = TRUE, perl = TRUE)
  status <- if (any(coverage$status == "failed") || length(errors)) "failed" else
    if (any(coverage$status == "missing") || length(evidence_missing)) "incomplete" else "passed"
  list(status = status, coverage = coverage, missing_evidence = evidence_missing,
       sas_errors = errors, provenance = provenance_text,
       scope = "Fixture output agreement only; inspect SAS log and provenance before treating files as SAS evidence.")
}

semantic_reference_difference <- function(path, expected_path) {
  actual <- semantic_frame(path)
  expected <- semantic_frame(expected_path)
  names(actual) <- tolower(names(actual))
  if (!identical(names(actual), names(expected))) return("Column names or order differ")
  all.equal(actual, expected, check.attributes = FALSE, tolerance = 1e-8)
}
