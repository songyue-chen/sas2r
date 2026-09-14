# User-facing copies are distinct from the immutable, flat execution snapshots.
user_bundle_path <- function(path) {
  if (startsWith(path, "R/macros/")) return(sub("^R/macros/", "macros/", path))
  file.path("programs", path)
}

materialize_user_bundle <- function(source_dir, destination, project = NULL) {
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  if (identical(source_dir, destination)) return(invisible(destination))
  order_path <- file.path(source_dir, "run-order.json")
  order <- if (file.exists(order_path)) read_json_record(order_path) else list()
  organized <- identical(order$layout, "organized")
  files <- list.files(source_dir, recursive = TRUE)
  if (organized) {
    # Export the editable bundle, including configuration edits, but not an old
    # manual run. Saved automated deliverables are copied separately by sas_write.
    files <- files[!startsWith(files, "output/")]
    mapping <- stats::setNames(files, files)
  } else {
    code <- files[grepl("\\.R$", files) & !basename(files) %in% SAS2R_BUNDLE_FILES &
                    !files %in% order$entrypoint & !startsWith(files, "tests_macros/")]
    mapping <- stats::setNames(vapply(code, user_bundle_path, character(1)), code)
    support <- files[startsWith(files, "tests_macros/")]
    mapping <- c(mapping, stats::setNames(support, support))
    runtime <- intersect(files, setdiff(SAS2R_BUNDLE_FILES, "autoexec.R"))
    mapping <- c(mapping, stats::setNames(file.path("runtime", runtime), runtime))
  }
  for (file in names(mapping)) {
    target <- file.path(destination, mapping[[file]])
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(source_dir, file), target, overwrite = TRUE)) {
      cli::cli_abort("Could not copy bundle file {.file {file}}")
    }
  }
  if (!organized) {
    for (dir in c("programs", "macros", "runtime")) {
      dir.create(file.path(destination, dir), recursive = TRUE, showWarnings = FALSE)
    }
    write_autoexec(project, destination,
      library_map = build_attempt_library_map(project, destination), organized = TRUE)
    programs <- vapply(as.character(unlist(order$programs)), user_bundle_path, character(1))
    atomic_write_json(list(layout = "organized", entrypoint = "run.R", programs = unname(programs),
                           source_paths = as.list(mapping)), file.path(destination, "run-order.json"))
    writeLines(c(
      '# Run from this folder with Rscript run.R or source("run.R").',
      'local({',
      '  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)',
      '  env <- new.env(parent = globalenv())',
      '  sys.source(file.path(root, "autoexec.R"), envir = env, chdir = TRUE)',
      '  tlf_dir <- file.path(env$.sas2r_output_root, "tlf")',
      '  dir.create(tlf_dir, recursive = TRUE, showWarnings = FALSE)',
      '  old_dir <- setwd(tlf_dir)',
      '  on.exit(setwd(old_dir), add = TRUE)',
      paste0('  programs <- ', paste(deparse(unname(programs)), collapse = "\n")),
      '  for (program in programs) sys.source(file.path(root, program), envir = env)',
      '})'
    ), file.path(destination, "run.R"))
    write_bundle_guide(project, destination)
  }
  invisible(destination)
}

# Use the existing output resolver and the actual attempt inventory. A requested
# WORK member is a deliverable; its library name alone does not make it scratch.
materialize_run_outputs <- function(state) {
  attempt <- state$selected_attempt
  if (is.null(attempt$attempt_dir) || !isTRUE(state$execute)) return(list())
  inventory <- attempt$output_hashes %||% list()
  source_files <- normalizePath(file.path(attempt$attempt_dir, names(inventory)),
                                winslash = "/", mustWork = FALSE)
  contracts <- state$output_contracts %||% empty_output_contracts()
  outputs <- list()
  used <- character()
  for (i in seq_len(nrow(contracts))) {
    contract <- contracts[i, , drop = FALSE]
    candidate <- find_attempt_candidate_file(contract, attempt)
    index <- match(candidate, source_files)
    if (is.na(index)) next
    original <- names(inventory)[index]
    relative <- if (identical(contract$kind[[1]], "dataset")) {
      parts <- split_ds(contract$logical_name[[1]])
      file.path("datasets", parts[["lib"]], paste0(parts[["member"]], ".", tools::file_ext(original)))
    } else file.path("tlf", original)
    target <- file.path(state$paths$outputs, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(candidate, target, overwrite = TRUE)) cli::cli_abort("Could not save {.file {candidate}}")
    outputs[[contract$target_key[[1]]]] <- list(path = file.path("outputs", relative),
      attempt_path = original, sha256 = inventory[[original]], kind = contract$kind[[1]])
    used <- c(used, original)
  }
  scratch <- inventory[setdiff(names(inventory), used)]
  if (length(scratch)) copy_output_inventory(attempt$attempt_dir, state$paths$work, scratch)
  outputs
}
