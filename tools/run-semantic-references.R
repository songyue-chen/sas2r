#!/usr/bin/env Rscript
# Run from the repository root. No package installation or provider is needed.
args <- commandArgs(trailingOnly = TRUE)
if (any(args == "--help")) {
  cat("Rscript tools/run-semantic-references.R --sas=/path/to/sas [--output=/path/to/new-directory]\n",
      "Rscript tools/run-semantic-references.R --verify-only [--output=/path/to/collected-references]\n")
  quit(status = 0)
}
if (any(!grepl("^(--sas=|--output=|--verify-only$)", args))) stop("Unknown argument; use --help")
value <- function(prefix, default) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit) > 1L) stop("Repeated argument: ", prefix)
  if (length(hit)) substring(hit, nchar(prefix) + 1L) else default
}
root <- "tests/testthat/fixtures/semantic-reference"
if (!file.exists(file.path(root, "manifest.json"))) stop("Run this script from the repository root")
output <- value("--output=", file.path(root, "sas-generated"))
source("tests/testthat/helper-semantic-reference.R")
if (!"--verify-only" %in% args) {
  sas <- value("--sas=", Sys.which("sas"))
  if (!nzchar(sas)) stop("No SAS executable found. Collection remains pending; --verify-only audits existing evidence.")
  # Never mix an interrupted collection with a later run. Use a new directory.
  if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory is not empty. Choose a new --output directory for this collection: ", output)
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output, winslash = "/", mustWork = TRUE)
  Sys.setenv(SAS2R_REFERENCE_OUT = output)
  status <- system2(sas, c("-sysin", shQuote("tools/generate-semantic-references.sas"),
                           "-log", shQuote(file.path(output, "sas.log")),
                           "-print", shQuote(file.path(output, "sas.lst"))))
  if (status != 0L) stop("SAS exited with status ", status, "; inspect ", file.path(output, "sas.log"))
}
audit <- semantic_reference_audit(root, output)
print(audit$coverage, row.names = FALSE)
cat("\nCollection status:", audit$status, "\n")
if (length(audit$missing_evidence)) cat("Missing evidence:", paste(audit$missing_evidence, collapse = ", "), "\n")
if (length(audit$sas_errors)) cat(paste(audit$sas_errors, collapse = "\n"), "\n")
cat(audit$scope, "\n")
quit(status = switch(audit$status, passed = 0L, failed = 1L, incomplete = 2L))
