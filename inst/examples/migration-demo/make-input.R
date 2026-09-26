# Generate synthetic RDS input for a writable copy of the migration demo.
# Usage from any directory: Rscript make-input.R /path/to/migration-demo/data
# Or run/source this script with the copied demo as your working directory.
args <- if (sys.nframe() == 0L) commandArgs(trailingOnly = TRUE) else character()
if (!length(args) && !file.exists("demo.sas")) {
  stop("Run from the copied migration-demo directory or pass its data directory as the first argument.",
       call. = FALSE)
}

target_dir <- if (length(args) > 0L) {
  args[[1L]]
} else {
  file.path(getwd(), "data")
}

dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

input_data <- data.frame(
  USUBJID = c("01", "02", "03", "04", "05"),
  AVISITN = c(1L, 1L, 2L, 2L, 1L),
  AVAL = c(12.5, NA_real_, 15.0, 9.8, 14.2),
  TRTP = c("TRT A", "TRT B", "TRT A", "TRT B", "TRT A"),
  stringsAsFactors = FALSE
)
saveRDS(input_data, file.path(target_dir, "input_ds.rds"))
message("Generated input dataset: ", file.path(target_dir, "input_ds.rds"))
