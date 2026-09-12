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

write_bundle_guide <- function(project, dir, inventory = list()) {
  entrypoint <- read_json_record(file.path(dir, "run-order.json"))$entrypoint
  code_files <- list.files(dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- paste(unlist(lapply(code_files, readLines, warn = FALSE)), collapse = "\n")
  packages <- unique(sub("::.*$", "", regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*::", code))[[1L]]))
  packages <- sort(setdiff(packages, c("base", "utils", "stats", "tools", "methods", "grDevices", "graphics")))
  inputs <- build_attempt_library_map(project, dir)
  input_lines <- vapply(setdiff(names(inputs), "work"), function(lib) {
    paste0("- `", lib, "`: reads `", inputs[[lib]]$read_path, "`; writes `", lib, "/` in this folder.")
  }, character(1))
  writeLines(c(
    "# Translated SAS program folder", "",
    sprintf("Run `Rscript %s` from this folder. In R, use `source(\"%s\", chdir = TRUE)`.", entrypoint, entrypoint),
    "The entry point runs root programs in dependency order; included programs are called by their parents.", "",
    "## Dependencies", "", "Requires R 4.1 or later. The generated runtime is included.",
    if (length(packages)) paste0("Install the packages used by this bundle: `install.packages(c(", paste(sprintf('"%s"', packages), collapse = ", "), "))`."),
    "Packages used by optional runtime readers/formatters are listed too; load only the features you need.", "",
    "## Input and output paths", "",
    "Edit library paths in `autoexec.R` when inputs move. Input datasets are external dependencies and are not copied.",
    input_lines,
    "WORK reads and writes `work/` in this folder. Other generated files retain their relative paths.",
    "Explicit LIBNAME assignments inside programs still apply at their source positions; inspect those when moving input libraries.",
    "`outputs-manifest.json` lists the generated files copied from the selected execution.", "",
    "## Verification", "",
    "The report describes the selected execution before export. Rerunning this folder creates new outputs but does not update that report.",
    "Check the report's separate output and independent-review coverage before using these results."
  ), file.path(dir, "README.md"))
  atomic_write_json(inventory, file.path(dir, "outputs-manifest.json"))
  invisible(dir)
}
