# Generate synthetic RDS input in an explicitly supplied directory.
# Source this script, then call make_demo_input(dir), or use:
# Rscript make-input.R /path/to/migration-demo/data
make_demo_input <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  input_data <- data.frame(
    USUBJID = c("01", "02", "03", "04", "05"),
    AVISITN = c(1L, 1L, 2L, 2L, 1L),
    AVAL = c(12.5, NA_real_, 15.0, 9.8, 14.2),
    TRTP = c("TRT A", "TRT B", "TRT A", "TRT B", "TRT A"),
    stringsAsFactors = FALSE
  )
  file <- file.path(dir, "input_ds.rds")
  saveRDS(input_data, file)
  message("Generated input dataset: ", file)
  invisible(file)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) {
    stop("Pass the destination data directory as the only argument.", call. = FALSE)
  }
  make_demo_input(args[[1L]])
}
