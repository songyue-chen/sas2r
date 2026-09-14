# Inventory the files produced by an isolated execution, excluding its code and
# evidence. This same relative-path inventory drives selection, reports and export.
attempt_output_hashes <- function(attempt_dir) {
  files <- list.files(attempt_dir, recursive = TRUE)
  files <- files[!grepl("^(bundle|logs)/", files) & files != "record.json"]
  stats::setNames(lapply(files, function(rel) {
    as.character(cli::hash_file_sha256(file.path(attempt_dir, rel)))
  }), files)
}

copy_output_inventory <- function(source_dir, destination, inventory) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  for (rel in names(inventory)) {
    target <- file.path(destination, rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(source_dir, rel), target, overwrite = TRUE)) {
      cli::cli_abort("Could not copy generated output {.file {rel}} to {.file {destination}}")
    }
  }
  invisible(destination)
}

write_bundle_entrypoint <- function(state, bundle_dir) {
  order <- if (!is.null(state$graph)) build_bundle_execution_plan(state$graph)$execution_order else names(state$selected_revisions)
  files <- vapply(order, function(cid) {
    rev <- state$selected_revisions[[cid]]
    rev$staged_file %||% rev$contract$staged_file %||% paste0(cid, ".R")
  }, character(1))
  occupied <- vapply(state$selected_revisions, function(rev) {
    rev$staged_file %||% rev$contract$staged_file %||% paste0(rev$component_id, ".R")
  }, character(1))
  entrypoint <- "run.R"
  while (tolower(entrypoint) %in% tolower(occupied)) entrypoint <- paste0("_", entrypoint)
  atomic_write_json(list(entrypoint = entrypoint, programs = unname(files)),
                    file.path(bundle_dir, "run-order.json"))
  writeLines(c(
    sprintf('# Run with Rscript %s, or source("%s", chdir = TRUE).', entrypoint, entrypoint),
    "local({",
    "  env <- new.env(parent = globalenv())",
    '  sys.source("autoexec.R", envir = env, chdir = TRUE)',
    paste0("  programs <- ", paste(deparse(unname(files)), collapse = "\n")),
    "  for (program in programs) sys.source(program, envir = env)",
    "})"
  ), file.path(bundle_dir, entrypoint))
  invisible(files)
}

# The page and README share these same instructions.
bundle_instructions <- function(project, dir) {
  order <- read_json_record(file.path(dir, "run-order.json"))
  programs <- as.character(unlist(order$programs))
  code_files <- list.files(dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- paste(unlist(lapply(code_files, readLines, warn = FALSE)), collapse = "\n")
  packages <- unique(sub("::.*$", "", regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*::", code))[[1L]]))
  packages <- sort(setdiff(packages, c("base", "utils", "stats", "tools", "methods", "grDevices", "graphics")))
  inputs <- build_attempt_library_map(project, dir)
  c(
    "# Use and edit your translated scripts", "",
    "All available selected code is included, even when it failed checks or execution. Missing code is listed in the run manifest; no empty replacements are supplied.", "",
    "## Prepare R", "",
    "Requires R 4.1 or later. The bundled runtime needs no sas2r installation, LLM account, or translation call.",
    if (length(packages)) paste0("Packages referenced by this bundle (including optional runtime features): ", paste(packages, collapse = ", "), "."),
    if (length(packages)) paste0("Install required packages with install.packages(c(", paste(sprintf('"%s"', packages), collapse = ", "), "))."),
    "Observed package versions are in the migration report; they are not minimum requirements.", "",
    "## Choose data locations", "",
    "Edit read_path in autoexec.R for each input library; data can stay external or be copied beside this bundle and given a relative path. Keep engine consistent with the input format (for example rds or sas7bdat). Reference datasets used only for comparison are separate from program inputs.",
    vapply(setdiff(names(inputs), "work"), function(lib) paste0("- ", lib, ": original input ", inputs[[lib]]$read_path), character(1)),
    "Manual outputs use .sas2r_output_root in autoexec.R, initially bundle/output/: datasets/<library>/, work/, and tlf/ for relative report files. New source LIBNAME bindings use libraries/ under the same write root. Change the root to keep separate manual iterations.",
    "Relative translated LIBNAME calls use .sas2r_execution_root (initially the source project directory), whereas relative registry seed paths use the bundle directory. Explicit script-level paths and external resources may also need editing.", "",
    "## Run here", "",
    "Start a fresh R session, set the working directory to this bundle folder, and run:", "",
    "```r", 'source("run.R")', "```", "",
    "Or run Rscript run.R from this folder. Failed code can stop execution until repaired.",
    paste0("Program order: ", paste(programs, collapse = " -> "), ". Macros are loaded as function definitions, not executed as separate jobs."), "",
    "## Fix one script or supply existing upstream datasets", "",
    "Use the starting page's component table to find the code, upstream dependencies, and error. From a fresh session in this bundle folder:", "",
    "```r", 'source("autoexec.R")',
    'dir.create(file.path(.sas2r_output_root, "tlf"), recursive = TRUE, showWarnings = FALSE)',
    'setwd(file.path(.sas2r_output_root, "tlf"))',
    if (length(programs)) paste0('source(file.path(.sas2r_bundle_root, ', vapply(programs, deparse, character(1)), '))'),
    "```", "",
    "Run the needed upstream lines followed by the edited script; rerun affected downstream scripts afterwards. To use supplied upstream datasets, configure their input libraries and run only the downstream lines. Use a fresh write root: generated members take precedence over input members, so running the full derivation order or keeping old manual outputs can replace the data you meant to test.",
    "The runner uses output/tlf/ as its working directory for relative report files. Review other relative file reads/writes in hand-edited code; arbitrary hard-coded paths are not relocated automatically.", "",
    "## Move the bundle", "",
    "Copy this entire folder, including programs, macros, runtime, formats, autoexec.R, run.R, and support files. Install required R packages, copy input data or set accessible external paths, update autoexec.R and explicit paths in scripts, then run from the new folder.", "",
    "## Check edited results", "",
    "The saved migration outputs and report describe the original automated run. Manual runs do not replace them or update their validation status. Compare edited results against your reference datasets and review unresolved findings before use. The run's report/comparison-details folder contains available original comparison evidence; there is no automatic revalidation command in this bundle.",
    "Repeated manual runs may overwrite earlier manual outputs; select a fresh .sas2r_output_root to keep them."
  )
}

write_bundle_guide <- function(project, dir, inventory = list()) {
  writeLines(bundle_instructions(project, dir), file.path(dir, "README.md"))
  invisible(dir)
}
