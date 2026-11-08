# Tutorial expectations derived from the corrected SAS and synthetic input.
# These are NOT independent SAS-generated reference results.
verify_demo_output <- function(result, project_dir) {
  if (!identical(result$status, "migration_ready")) {
    stop("Run is not migration_ready. Inspect START_HERE.html before accepting output.")
  }
  if (is.null(result$outputs_dir)) stop("No executed output directory is available.")
  dataset <- file.path(result$outputs_dir, "datasets", "adam", "final_ds.rds")
  pdf <- file.path(result$outputs_dir, "tlf", "outputs", "table1.pdf")
  stopifnot(file.exists(dataset), file.exists(pdf))
  actual <- readRDS(dataset)
  expected <- read.csv(file.path(project_dir, "example", "expected-final.csv"),
                       colClasses = c("character", "integer", "numeric", "character", "character", "integer"),
                       na.strings = "", check.names = FALSE)
  # SAS names are case-insensitive; retain the actual output's original names.
  position <- match(tolower(names(expected)), tolower(names(actual)))
  stopifnot(nrow(actual) == nrow(expected), !anyNA(position),
            ncol(actual) == ncol(expected))
  for (i in seq_along(position)) {
    x <- actual[[position[i]]]
    y <- expected[[i]]
    # SAS character values compare equal after trailing blank padding is removed.
    # Preserve leading/internal spaces and the actual generated output.
    if (is.character(x) && is.character(y)) {
      x <- sub(" +$", "", x)
      y <- sub(" +$", "", y)
    }
    if (!isTRUE(all.equal(as.vector(x), as.vector(y), check.attributes = FALSE))) {
      stop("Output mismatch in ", names(expected)[i], ". Inspect the generated code and report.")
    }
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Dataset checks passed. Install pdftools, then rerun verification for PDF content.")
  }
  text <- paste(pdftools::pdf_text(pdf), collapse = "\n")
  required <- c(names(expected), "MISSING", "RECORDED", "12.5", "14.2", "9.8")
  missing <- required[!vapply(required, function(x) grepl(tolower(x), tolower(text), fixed = TRUE), logical(1))]
  if (length(missing)) stop("PDF content missing: ", paste(missing, collapse = ", "))
  message("PASS: 5 rows, values, missingness, flags, row order, and PDF text. These are tutorial checks, not SAS-reference equivalence.")
  invisible(actual)
}
