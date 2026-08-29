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
on.exit(unlink(bench_dir, recursive = TRUE), add = TRUE)

demo_files <- list.files("inst/examples/demo_project", pattern = "\\.sas$", full.names = TRUE)
if (!length(demo_files)) {
  demo_files <- list.files(system.file("examples/demo_project", package = "sas2r"),
                           pattern = "\\.sas$", full.names = TRUE)
}

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
  p_cold <- sas2r::sas_project(bench_dir, cache = TRUE)
})

# 3. Warm scan (cache = TRUE, cache already populated in .sas2r/scan_cache.rds)
t_warm <- system.time({
  p_warm <- sas2r::sas_project(bench_dir, cache = TRUE)
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
status <- if (warm_elapsed < 2.0) "DEFERRED" else "REVISIT TRIGGERED"
cmp <- if (warm_elapsed < 2.0) "<" else ">="
cat(sprintf("Decision Rule: Warm scan (%.3fs) %s 2.0s threshold => Tree-sitter %s\n", warm_elapsed, cmp, status))
cat("========================================================================\n\n")
