#!/usr/bin/env Rscript
# Execute the published code, not separately maintained copies of examples.
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %in% c("--installed", "--source"))) stop("Use --installed or --source")
repo <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo, "DESCRIPTION"))) stop("Run from the repository root")
if ("--source" %in% args) pkgload::load_all(repo, quiet = TRUE) else library(sas2r)

files <- c("README.md", "docs/output-evidence.md", "docs/clinical-qc-preflight.md",
           "vignettes/dependency-aware-migration.Rmd", "vignettes/runtime-helpers.Rmd")
source(file.path(repo, "tests/testthat/helper-documentation.R"))
blocks <- unlist(lapply(files, function(file) {
  doc_code_blocks(readLines(file.path(repo, file), warn = FALSE), file)
}), recursive = FALSE)
for (i in seq_along(blocks)) {
  block <- blocks[[i]]
  if (block$language == "r" && !grepl("^<!-- sas2r-example: (offline|network) [a-z0-9-]+ -->$", block$marker)) {
    stop("Classify the R example as offline or network: ", block$file, ":", block$line)
  }
  blocks[[i]]$offline <- grepl(": offline ", block$marker)
}

workspace <- tempfile("sas2r-doc-examples-")
dir.create(workspace)
old <- setwd(workspace)
for (dir in c("programs", "data/sdtm", "data/adam", "data/reference", "saved/generated-outputs/adam")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
reference <- data.frame(STUDYID = c("DOCS", "DOCS"), USUBJID = c("01", "02"), AVAL = c(10, 20))
haven::write_xpt(reference, "data/sdtm/dm.xpt")
haven::write_xpt(reference, "data/reference/adsl.xpt")
haven::write_xpt(reference, "data/reference/adae.xpt")
saveRDS(reference[c(2, 1), ], "saved/generated-outputs/adam/adsl.rds")
writeLines("data adam.adsl; set sdtm.dm; run;", "programs/adsl.sas")
# The study configuration is also read verbatim from the README.
quickstart <- Filter(function(b) b$file == "README.md" && b$language == "yaml", blocks)[[1L]]
writeLines(quickstart$code, "_sas2r.yml")

# Runtime vignette examples need a real portable bundle. Build and export one
# from the synthetic input using the public workflow with no configured adapter.
fixture_result <- sas_translate(
  "programs/adsl.sas", out_dir = "fixture-migration",
  config = list(libraries = list(sdtm = "../data/sdtm", adam = "../data/adam")),
  outputs = list(
    profiles = list(subject = qc_profile(keys = "USUBJID", unique_keys = TRUE, row_count = 2)),
    assertions = list("adam.adsl" = list(profile = "subject"))
  ),
  usage_limits = list(max_calls = 0), max_program_repair_rounds = 0,
  max_bundle_repair_rounds = 0
)
stopifnot(file.exists(file.path(fixture_result$outputs_dir, "adam", "adsl.rds")))
sas_write(fixture_result, "run_20260907T120000Z_1a2b3c4d")
env <- new.env(parent = globalenv())
env$result <- list(outputs_dir = normalizePath("saved/generated-outputs"))
counts <- c(executed = 0L, parsed_network = 0L, configurations = 0L)
for (block in blocks) {
  cat(block$file, ":", block$line, " ", block$language, "\n", sep = "")
  if (block$language == "r") {
    code <- parse(text = block$code, keep.source = TRUE)
    if (block$offline) {
      setwd(workspace)
      eval(code, envir = env)
      setwd(workspace)
      counts["executed"] <- counts["executed"] + 1L
    } else counts["parsed_network"] <- counts["parsed_network"] + 1L
  } else {
    # README's provider menu shows alternative mappings separated by blank
    # lines. Validate each independently rather than accepting duplicate keys.
    parts <- if (length(grep("^provider:", strsplit(block$code, "\n")[[1L]])) > 1L)
      strsplit(block$code, "\n\n")[[1L]] else block$code
    for (part in parts) {
      config <- yaml::yaml.load(part)
      if (!is.null(config$provider)) config <- list(llm = config)
      file <- tempfile(fileext = ".yml")
      yaml::write_yaml(config, file)
      sas_config(file)
      counts["configurations"] <- counts["configurations"] + 1L
    }
  }
}
stopifnot(counts["executed"] >= 6L, counts["configurations"] >= 4L,
          file.exists("adsl-comparison.md"), file.exists("aligned-comparison.json"),
          identical(env$check$model_calls, 0L))
cat("\nDocumentation checks passed; synthetic study and saved-output fixtures.\n")
print(counts)
setwd(old)
unlink(workspace, recursive = TRUE)
