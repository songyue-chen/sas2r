#!/usr/bin/env Rscript
# Maintainer benchmark of the internal scanner, not a public package API.
# Run from the repository to load development code, or from elsewhere to use
# the installed sas2r package. The fixture is located relative to this script.
if (file.exists("DESCRIPTION") && any(grepl("^Package:\\s*sas2r", readLines("DESCRIPTION", warn = FALSE)))) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(quiet = TRUE)
  } else {
    library(sas2r)
  }
} else if (!requireNamespace("sas2r", quietly = TRUE)) {
  stop("sas2r package not found")
} else {
  library(sas2r)
}

# 1. Setup benchmark directory and generate ~1 MB corpus of SAS code
bench_dir <- tempfile(pattern = "bench_scan_")
dir.create(bench_dir, recursive = TRUE, showWarnings = FALSE)

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
repo_root <- dirname(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)))
demo_files <- list.files(file.path(repo_root, "tests", "testthat", "fixtures", "scanner-project"),
                         pattern = "\\.sas$", full.names = TRUE)

# Read demo templates
templates <- lapply(demo_files, function(f) readLines(f, warn = FALSE))
combined_template <- paste(unlist(templates), collapse = "\n\n")

# Generate ~15 program files of ~70 KB each (~1 MB total corpus)
n_files <- 15L
target_total_bytes <- 1024 * 1024
bytes_per_file <- ceiling(target_total_bytes / n_files)
repeats_per_file <- ceiling(bytes_per_file / nchar(combined_template, type = "bytes"))

total_bytes <- 0L
for (i in seq_len(n_files)) {
  chunks <- character(repeats_per_file)
  for (r in seq_len(repeats_per_file)) {
    chunks[r] <- sprintf("/* File %02d Block %03d */\n%s\n", i, r, combined_template)
  }
  content <- paste(chunks, collapse = "\n")
  fpath <- file.path(bench_dir, sprintf("prog_%02d.sas", i))
  writeLines(content, fpath)
  total_bytes <- total_bytes + nchar(content, type = "bytes")
}

actual_size_mb <- total_bytes / (1024 * 1024)

# 2. Cold scan (cache = TRUE, cache starts empty)
t_cold <- system.time({
  p_cold <- sas2r:::sas_project(bench_dir, cache = TRUE)
})

# 3. Warm scan (cache = TRUE, cache already populated in session-temporary storage)
t_warm <- system.time({
  p_warm <- sas2r:::sas_project(bench_dir, cache = TRUE)
})

cold_elapsed <- t_cold[["elapsed"]]
warm_elapsed <- t_warm[["elapsed"]]
speedup <- cold_elapsed / warm_elapsed
cold_rate <- (actual_size_mb / cold_elapsed)
warm_rate <- (actual_size_mb / warm_elapsed)

# 4. Print benchmark table
cat("\n")
cat("========================================================================\n")
cat("                  sas2r Scan Benchmark (1 MB Corpus)                    \n")
cat("========================================================================\n")
cat(sprintf("Corpus Size  : %.2f MB (%d files, %d statements)\n",
            actual_size_mb, n_files, nrow(p_cold$statements)))
cat("------------------------------------------------------------------------\n")
cat(sprintf("| %-12s | %-12s | %-12s | %-12s |\n",
            "Mode", "Elapsed (s)", "Throughput", "Speedup"))
cat("------------------------------------------------------------------------\n")
cat(sprintf("| %-12s | %-12.3f | %-8.2f MB/s | %-12s |\n",
            "Cold Scan", cold_elapsed, cold_rate, "1.0x (base)"))
cat(sprintf("| %-12s | %-12.3f | %-8.2f MB/s | %-11.1fx |\n",
            "Warm Scan", warm_elapsed, warm_rate, speedup))
cat("Timing is descriptive; it is not a release gate or a parser-selection rule.\n")
cat("========================================================================\n\n")
unlink(bench_dir, recursive = TRUE)
