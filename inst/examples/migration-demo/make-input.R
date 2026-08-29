# Generate local RDS input dataset(s) for the migration demo
args <- commandArgs(trailingOnly = TRUE)

script_dir <- tryCatch({
  frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  if (length(frame_files) > 0L) dirname(normalizePath(frame_files[[1L]], winslash = "/", mustWork = FALSE)) else NA_character_
}, error = function(e) NA_character_)

if (is.na(script_dir) || !nzchar(script_dir)) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg) > 0L) {
    script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/", mustWork = FALSE))
  } else {
    script_dir <- if (file.exists("demo.sas")) "." else file.path("inst", "examples", "migration-demo")
  }
}

target_dir <- if (length(args) > 0L) {
  args[[1L]]
} else {
  file.path(script_dir, "data")
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
