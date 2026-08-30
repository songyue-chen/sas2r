#' Migration attempts lifecycle and immutability management
#'
#' Manages isolated attempt directories, snapshots, attempt IDs, status records,
#' and immutability guarantees.

#' Generate a canonical attempt identifier
#'
#' @param kind Attempt kind (e.g. "smoke", "bundle", "program").
#' @param sequence Integer sequence number.
#' @return Formatted attempt identifier string.
#' @noRd
new_attempt_id <- function(kind, sequence) {
  if (!is.character(kind) || length(kind) != 1L || !nzchar(kind)) {
    cli::cli_abort(
      "{.arg kind} must be a non-empty string",
      class = "sas2r_invalid_argument"
    )
  }
  if (is.null(sequence) || length(sequence) != 1L || is.na(sequence) || as.integer(sequence) < 1L) {
    cli::cli_abort(
      "{.arg sequence} must be a positive integer",
      class = "sas2r_invalid_argument"
    )
  }
  sprintf("%s_attempt_%03d", kind, as.integer(sequence))
}

#' Initialize an isolated attempt directory and record
#'
#' Creates subdirectories for `bundle/`, `work/`, `outputs/`, `logs/`, and writes
#' an initial incomplete `record.json`.
#'
#' @param paths Migration paths object or output directory path.
#' @param kind Attempt kind (default "smoke").
#' @param parent_attempt_id Optional parent attempt identifier.
#' @param sequence Optional explicit sequence number.
#' @return A named list representing the initialized attempt record.
#' @noRd
init_attempt <- function(paths, kind = "smoke", parent_attempt_id = NULL, sequence = NULL) {
  if (!is.character(kind) || length(kind) != 1L || !nzchar(kind)) {
    cli::cli_abort(
      "{.arg kind} must be a non-empty string",
      class = "sas2r_invalid_argument"
    )
  }

  attempts_root <- if (is.list(paths) && !is.null(paths$attempts)) {
    paths$attempts
  } else if (is.character(paths) && length(paths) == 1L) {
    if (basename(paths) %in% c("runs", "attempts")) {
      paths
    } else {
      file.path(paths, "runs")
    }
  } else {
    cli::cli_abort(
      "{.arg paths} must be a migration_paths object or directory path",
      class = "sas2r_invalid_argument"
    )
  }

  dir.create(attempts_root, recursive = TRUE, showWarnings = FALSE)

  if (is.null(sequence)) {
    existing <- list.dirs(attempts_root, full.names = FALSE, recursive = FALSE)
    pattern <- paste0("^", kind, "_attempt_([0-9]+)$")
    matching <- existing[grepl(pattern, existing)]
    if (length(matching) == 0L) {
      sequence <- 1L
    } else {
      nums <- as.integer(sub(pattern, "\\1", matching))
      nums <- nums[!is.na(nums)]
      sequence <- if (length(nums) > 0L) max(nums) + 1L else 1L
    }
  } else {
    if (is.na(sequence) || as.integer(sequence) < 1L) {
      cli::cli_abort(
        "{.arg sequence} must be a positive integer",
        class = "sas2r_invalid_argument"
      )
    }
    sequence <- as.integer(sequence)
  }

  attempt_id <- new_attempt_id(kind, sequence)
  attempt_dir <- file.path(attempts_root, attempt_id)
  bundle_dir <- file.path(attempt_dir, "bundle")
  work_dir <- file.path(attempt_dir, "work")
  outputs_dir <- file.path(attempt_dir, "outputs")
  logs_dir <- file.path(attempt_dir, "logs")

  dir.create(attempt_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

  created_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  record <- list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    attempt_id = attempt_id,
    kind = kind,
    sequence = sequence,
    parent_attempt_id = parent_attempt_id,
    attempt_dir = normalizePath(attempt_dir, winslash = "/", mustWork = FALSE),
    bundle_dir = normalizePath(bundle_dir, winslash = "/", mustWork = FALSE),
    work_dir = normalizePath(work_dir, winslash = "/", mustWork = FALSE),
    outputs_dir = normalizePath(outputs_dir, winslash = "/", mustWork = FALSE),
    logs_dir = normalizePath(logs_dir, winslash = "/", mustWork = FALSE),
    completed = FALSE,
    created_at = created_at
  )

  rec_path <- file.path(attempt_dir, "record.json")
  jsonlite::write_json(
    unclass(record),
    rec_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  structure(record, class = "sas2r_migration_attempt")
}

#' Complete an attempt record and mark it immutable
#'
#' Updates an attempt record with final completion status, timing, and hashes,
#' and persists it to `record.json`. A completed record cannot be modified again.
#'
#' @param record Attempt record from `init_attempt()`.
#' @param ... Additional metadata fields to merge into record.
#' @return Completed attempt record.
#' @noRd
complete_attempt <- function(record, ...) {
  if (!is.list(record) || is.null(record$attempt_id)) {
    cli::cli_abort(
      "{.arg record} must be a valid attempt record",
      class = "sas2r_invalid_argument"
    )
  }

  if (isTRUE(record$completed)) {
    cli::cli_abort(
      "Attempt {.val {record$attempt_id}} is already completed and immutable.",
      class = "sas2r_immutable_attempt"
    )
  }

  record$completed <- TRUE
  record$completed_at <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  extra <- list(...)
  if (length(extra) > 0L) {
    for (nm in names(extra)) {
      record[[nm]] <- extra[[nm]]
    }
  }

  rec_path <- file.path(record$attempt_dir, "record.json")
  tmp_path <- file.path(record$attempt_dir, paste0("record_", substr(migration_hash(Sys.time()), 1L, 8L), ".tmp"))

  jsonlite::write_json(
    unclass(record),
    tmp_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  file.rename(tmp_path, rec_path)

  structure(record, class = "sas2r_migration_attempt")
}

#' Read an attempt record from disk
#'
#' @param attempt_dir Directory path of the attempt.
#' @return Named list representing the attempt record, or NULL if not found.
#' @noRd
read_attempt_record <- function(attempt_dir) {
  if (is.null(attempt_dir) || !is.character(attempt_dir) || length(attempt_dir) != 1L) {
    return(NULL)
  }
  rec_file <- file.path(attempt_dir, "record.json")
  if (!file.exists(rec_file)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(rec_file, simplifyVector = TRUE, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
}

#' Build a hash manifest of configured inputs
#'
#' @param project A `sas2r_project`, `sas2r_migration_state`, or directory/list.
#' @return A named list of file hashes for all files in configured input libraries.
#' @noRd
input_hash_manifest <- function(project) {
  if (is.null(project)) return(list())

  # Extract project if state was passed
  p <- if (is.list(project) && !is.null(project$project)) project$project else project

  # Extract library roots
  lib_roots <- list()
  if (inherits(p, "sas2r_project") && !is.null(p$libref_registry)) {
    eff <- tryCatch(effective_librefs(p), error = function(e) NULL)
    if (!is.null(eff) && !is.null(eff$seed)) {
      for (nm in names(eff$seed)) {
        if (tolower(nm) == "work") next
        entry <- eff$seed[[nm]]
        pth <- entry$path %||% entry$read_path
        if (!is.null(pth) && nzchar(pth) && dir.exists(pth)) {
          lib_roots[[tolower(nm)]] <- normalizePath(pth, winslash = "/", mustWork = FALSE)
        }
      }
    }
  }

  if (length(lib_roots) == 0L && is.list(p)) {
    cfg_libs <- p$config$libraries %||% p$libraries %||% list()
    for (nm in names(cfg_libs)) {
      if (tolower(nm) == "work") next
      entry <- cfg_libs[[nm]]
      pth <- if (is.character(entry)) entry else (entry$path %||% entry$read_path)
      if (!is.null(pth) && nzchar(pth) && dir.exists(pth)) {
        lib_roots[[tolower(nm)]] <- normalizePath(pth, winslash = "/", mustWork = FALSE)
      }
    }
  }

  manifest <- list()
  for (lib in names(lib_roots)) {
    root <- lib_roots[[lib]]
    files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    files <- sort(files, method = "radix")
    for (f in files) {
      if (file.info(f)$isdir) next
      rel <- substring(f, nchar(root) + 2L)
      key <- paste0(lib, "/", rel)
      h <- tryCatch(as.character(cli::hash_file_sha256(f)), error = function(e) "")
      manifest[[key]] <- h
    }
  }

  manifest
}

#' Snapshot active selected bundle into an immutable attempt directory
#'
#' Copies active generated programs, contracts, format catalogs, and runtime
#' helpers into `attempt/bundle/`, and emits the attempt-specific read/write
#' registry template. Never executes mutable files outside the snapshot.
#'
#' Materialize the selected translation at the run folder root
#'
#' Copies the selected attempt's bundle files to the top of the run folder so
#' users find the translated programs (and the runtime files they need)
#' without descending into bundle_attempt_NNN/bundle/. Machine metadata
#' (*.contract.json, _sas2r_bundle_progress.json) stays behind in the attempt
#' bundle, which remains the canonical, complete artifact that
#' `sas_code()`/`sas_write()` and `$bundle_dir` refer to. Written only after
#' selection is final, so unlike a pre-selection staging copy it cannot
#' diverge from the selected result.
#'
#' @param bundle_dir The selected attempt's bundle directory.
#' @param run_dir The run folder root.
#' @return The run directory, invisibly (NULL when either side is missing).
#' @noRd
materialize_run_translation <- function(bundle_dir, run_dir) {
  if (is.null(bundle_dir) || length(bundle_dir) != 1L || !dir.exists(bundle_dir)) {
    return(invisible(NULL))
  }
  if (is.null(run_dir) || length(run_dir) != 1L || !dir.exists(run_dir)) {
    return(invisible(NULL))
  }
  rels <- list.files(bundle_dir, recursive = TRUE)
  keep <- !grepl("\\.contract\\.json$", rels) &
    rels != "_sas2r_bundle_progress.json"
  for (rel in rels[keep]) {
    dest <- file.path(run_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(bundle_dir, rel), dest, overwrite = TRUE)
  }
  invisible(run_dir)
}

#' @param state Migration state object.
#' @param attempt Initialized attempt record or attempt directory path.
#' @return The canonical path to the populated attempt bundle directory.
#' @noRd
snapshot_selected_bundle <- function(state, attempt) {
  if (is.null(state)) {
    cli::cli_abort("{.arg state} must be provided", class = "sas2r_invalid_argument")
  }
  if (is.null(attempt)) {
    cli::cli_abort("{.arg attempt} must be provided", class = "sas2r_invalid_argument")
  }

  attempt_dir <- if (is.list(attempt)) attempt$attempt_dir else normalizePath(attempt, winslash = "/", mustWork = FALSE)
  bundle_dir <- file.path(attempt_dir, "bundle")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Runtime helpers
  if (!is.null(state$runtime) && !is.null(state$runtime$helpers) && file.exists(state$runtime$helpers)) {
    file.copy(state$runtime$helpers, file.path(bundle_dir, "sas2r-helpers.R"), overwrite = TRUE)
  } else if (!is.null(state$paths) && file.exists(file.path(state$paths$state, "sas2r-helpers.R"))) {
    file.copy(file.path(state$paths$state, "sas2r-helpers.R"), file.path(bundle_dir, "sas2r-helpers.R"), overwrite = TRUE)
  } else {
    write_helpers(bundle_dir)
  }

  # 2. Formats
  if (!is.null(state$project) && inherits(state$project, "sas2r_project")) {
    fcat <- tryCatch(compile_format_catalog(state$project), error = function(e) list(catalog = list()))
    if (!is.null(fcat$catalog)) {
      write_formats(fcat$catalog, bundle_dir)
    } else {
      writeLines(c("# sas2r: no format catalog definitions", ""), file.path(bundle_dir, "_sas2r_formats.R"))
    }
  } else {
    writeLines(c("# sas2r: no format catalog definitions", ""), file.path(bundle_dir, "_sas2r_formats.R"))
  }

  # 3. Attempt registry with copy-on-write mapping
  if (!is.null(state$project)) {
    lib_map <- build_attempt_library_map(state$project, attempt_dir)
    write_registry(state$project, bundle_dir, library_map = lib_map)
    for (libref in names(lib_map)) {
      w_dir <- lib_map[[libref]]$write_path
      if (!is.null(w_dir) && nzchar(w_dir)) {
        dir.create(w_dir, recursive = TRUE, showWarnings = FALSE)
      }
    }
  } else {
    reg_lines <- c(
      ".sas2r_registry <- list(",
      sprintf("  work = list(read_path = %s, write_path = %s, engine = 'rds', write = 'rds')",
              deparse(file.path(attempt_dir, "work")), deparse(file.path(attempt_dir, "work"))),
      ")",
      "dir.create(.sas2r_registry$work$write_path, showWarnings = FALSE, recursive = TRUE)"
    )
    writeLines(reg_lines, file.path(bundle_dir, "_sas2r_registry.R"))
  }

  # 4. Active generated programs and contracts
  if (!is.null(state$selected_revisions) && length(state$selected_revisions) > 0L) {
    for (cid in names(state$selected_revisions)) {
      rev <- state$selected_revisions[[cid]]
      rel_name <- rev$staged_file %||% rev$contract$staged_file %||% paste0(cid, ".R")
      dest_path <- file.path(bundle_dir, rel_name)
      dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

      code_str <- rev$r_code %||% rev$assembled_r %||% rev$code
      if (is.null(code_str) && !is.null(rev$r_path) && file.exists(rev$r_path)) {
        code_str <- paste(readLines(rev$r_path, warn = FALSE), collapse = "\n")
      }
      if (!is.null(code_str)) {
        writeLines(code_str, dest_path)
      }

      if (!is.null(rev$contract)) {
        contract_dest <- file.path(bundle_dir, paste0(cid, ".contract.json"))
        atomic_write_json(rev$contract, contract_dest)
      }
    }
  } else if (!is.null(state$paths$programs) && dir.exists(state$paths$programs)) {
    prog_files <- list.files(state$paths$programs, pattern = "\\.[rR]$", recursive = TRUE, full.names = TRUE)
    for (pf in prog_files) {
      rel <- substring(pf, nchar(state$paths$programs) + 2L)
      dest <- file.path(bundle_dir, rel)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(pf, dest, overwrite = TRUE)
    }
  }

  normalizePath(bundle_dir, winslash = "/", mustWork = FALSE)
}

#' Resume migration attempts from on-disk attempt records
#'
#' Discovers and reuses only completed attempt records whose bindings match
#' the current run binding. Interrupted or incomplete attempts are excluded.
#'
#' @param paths Migration paths object or output directory path.
#' @param run_binding Current run binding definition or hash list.
#' @return A named list containing `paths`, `reusable_attempt_ids`, `incomplete_attempt_ids`,
#'   `completed_attempts`, and `latest_completed`.
#' @noRd
resume_migration_attempts <- function(paths, run_binding = NULL) {
  attempts_root <- if (is.list(paths) && !is.null(paths$attempts)) {
    paths$attempts
  } else if (is.character(paths) && length(paths) == 1L) {
    if (basename(paths) %in% c("runs", "attempts")) paths else file.path(paths, "runs")
  } else {
    cli::cli_abort("{.arg paths} must be a migration_paths object or directory path", class = "sas2r_invalid_argument")
  }

  if (!dir.exists(attempts_root)) {
    return(list(
      paths = paths,
      reusable_attempt_ids = character(),
      incomplete_attempt_ids = character(),
      completed_attempts = list(),
      latest_completed = NULL
    ))
  }

  attempt_dirs <- list.dirs(attempts_root, full.names = TRUE, recursive = FALSE)
  # The run folder holds more than attempts (programs/ evidence, report
  # copies); only <kind>_attempt_<NNN> directories are attempt records.
  attempt_dirs <- attempt_dirs[grepl("^[a-z]+_attempt_[0-9]+$", basename(attempt_dirs))]
  reusable_ids <- character()
  incomplete_ids <- character()
  completed_list <- list()
  latest_completed <- NULL
  max_seq <- -1L

  for (adir in attempt_dirs) {
    aid <- basename(adir)
    rec <- read_attempt_record(adir)
    if (is.null(rec) || !isTRUE(rec$completed)) {
      incomplete_ids <- c(incomplete_ids, aid)
      next
    }

    binding_matches <- TRUE
    if (!is.null(run_binding) && !is.null(rec$run_binding) && length(rec$run_binding) > 0L) {
      if (is.character(run_binding) && is.character(rec$run_binding)) {
        binding_matches <- identical(run_binding, rec$run_binding)
      } else if (is.list(run_binding) && is.list(rec$run_binding)) {
        h1 <- migration_hash(run_binding)
        h2 <- migration_hash(rec$run_binding)
        binding_matches <- identical(h1, h2)
      }
    }

    if (!binding_matches) {
      next
    }

    reusable_ids <- c(reusable_ids, aid)
    completed_list[[aid]] <- rec

    seq_num <- as.integer(rec$sequence %||% 0L)
    if (seq_num > max_seq) {
      max_seq <- seq_num
      latest_completed <- rec
    }
  }

  list(
    paths = paths,
    reusable_attempt_ids = reusable_ids,
    incomplete_attempt_ids = incomplete_ids,
    completed_attempts = completed_list,
    latest_completed = latest_completed
  )
}

#' Select a non-regressive candidate attempt as authoritative
#'
#' Checks status ranking and target-by-target assessment to ensure candidate does not
#' regress compared to any previously selected attempt, then atomically writes
#' `selected.json`.
#'
#' @param paths Migration paths object or output directory path.
#' @param candidate Completed attempt record.
#' @param assessment Assessment or bundle status record.
#' @param previous Optional previous selected attempt record.
#' @return Selection record.
#' @noRd
select_attempt <- function(paths, candidate, assessment, previous = NULL) {
  if (!is.list(candidate) || is.null(candidate$attempt_id)) {
    cli::cli_abort("{.arg candidate} must be a valid attempt record", class = "sas2r_invalid_argument")
  }
  if (!isTRUE(candidate$completed)) {
    cli::cli_abort(
      "Cannot select incomplete attempt {.val {candidate$attempt_id}}",
      class = "sas2r_incomplete_attempt"
    )
  }

  status_ranks <- c(blocked = 1L, needs_review = 2L, migration_ready = 3L, validated = 4L)
  status_str <- if (is.list(assessment)) (assessment$status %||% "migration_ready") else as.character(assessment)
  cand_rank <- status_ranks[[status_str]] %||% 1L

  sel_path <- if (is.list(paths) && !is.null(paths$selected)) {
    paths$selected
  } else if (is.character(paths) && length(paths) == 1L) {
    file.path(paths, ".sas2r", "selected.json")
  } else {
    cli::cli_abort("{.arg paths} must be a migration_paths object or directory path", class = "sas2r_invalid_argument")
  }

  prev_rec <- previous
  if (is.null(prev_rec) && file.exists(sel_path)) {
    prev_rec <- tryCatch(
      jsonlite::fromJSON(sel_path, simplifyVector = TRUE, simplifyDataFrame = FALSE),
      error = function(e) NULL
    )
  }

  if (!is.null(prev_rec)) {
    prev_status <- prev_rec$status %||% prev_rec$assessment$status %||% "migration_ready"
    prev_rank <- status_ranks[[prev_status]] %||% 1L
    if (cand_rank < prev_rank) {
      cli::cli_abort(
        "Candidate attempt has regressed in status from {.val {prev_status}} to {.val {status_str}}",
        class = "sas2r_regressive_selection"
      )
    }

    cand_passing <- length(assessment$passing_targets %||% character())
    prev_passing <- length(prev_rec$assessment$passing_targets %||% prev_rec$passing_targets %||% character())
    if (prev_passing > 0L && cand_passing < prev_passing) {
      cli::cli_abort(
        "Candidate attempt has fewer passing required outputs ({cand_passing}) than previous selected attempt ({prev_passing})",
        class = "sas2r_regressive_selection"
      )
    }
  }

  sel_rec <- list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    attempt_id = candidate$attempt_id,
    selected_at = strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = status_str,
    assessment = assessment,
    attempt_dir = candidate$attempt_dir,
    outputs_dir = candidate$outputs_dir,
    execution_order = candidate$execution_order
  )

  dir.create(dirname(sel_path), recursive = TRUE, showWarnings = FALSE)
  atomic_write_json(sel_rec, sel_path)

  structure(sel_rec, class = "sas2r_selected_attempt")
}

#' Prune work and outputs from rejected attempts
#'
#' Removes `work/` and `outputs/` directories of non-selected attempts while
#' preserving code snapshots, logs, patches, assessments, hashes, and usage.
#'
#' @param paths Migration paths object or output directory path.
#' @param keep_raw Logical indicating if raw candidate outputs should be kept.
#' @return A named list reporting `selected_attempt_id`, `pruned_attempts`, and `removed_paths`.
#' @noRd
prune_rejected_attempt_outputs <- function(paths, keep_raw = FALSE) {
  sel_path <- if (is.list(paths) && !is.null(paths$selected)) {
    paths$selected
  } else if (is.character(paths) && length(paths) == 1L) {
    file.path(paths, ".sas2r", "selected.json")
  } else {
    cli::cli_abort("{.arg paths} must be a migration_paths object or directory path", class = "sas2r_invalid_argument")
  }

  if (!file.exists(sel_path)) {
    return(list(
      selected_attempt_id = NULL,
      pruned_attempts = character(),
      removed_paths = character()
    ))
  }

  sel <- tryCatch(
    jsonlite::fromJSON(sel_path, simplifyVector = TRUE, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  selected_id <- sel$attempt_id

  attempts_root <- if (is.list(paths) && !is.null(paths$attempts)) {
    paths$attempts
  } else if (is.character(paths) && length(paths) == 1L) {
    if (basename(paths) %in% c("runs", "attempts")) paths else file.path(paths, "runs")
  } else {
    file.path(dirname(dirname(sel_path)), "runs")
  }

  if (!dir.exists(attempts_root)) {
    return(list(
      selected_attempt_id = selected_id,
      pruned_attempts = character(),
      removed_paths = character()
    ))
  }

  attempt_dirs <- list.dirs(attempts_root, full.names = TRUE, recursive = FALSE)
  # Prune must only ever touch attempt directories: the run folder also holds
  # the run's programs/ evidence and report copies, which are permanent.
  attempt_dirs <- attempt_dirs[grepl("^[a-z]+_attempt_[0-9]+$", basename(attempt_dirs))]
  pruned_attempts <- character()
  removed_paths <- character()

  for (adir in attempt_dirs) {
    aid <- basename(adir)
    if (identical(aid, selected_id)) next

    subdirs <- list.dirs(adir, full.names = TRUE, recursive = FALSE)
    for (sd in subdirs) {
      s_name <- basename(sd)
      if (s_name %in% c("bundle", "logs")) next
      if (isTRUE(keep_raw) && !s_name %in% c("work", "outputs")) next

      unlink(sd, recursive = TRUE, force = TRUE)
      removed_paths <- c(removed_paths, sd)
    }
    pruned_attempts <- c(pruned_attempts, aid)
  }

  list(
    selected_attempt_id = selected_id,
    pruned_attempts = pruned_attempts,
    removed_paths = removed_paths
  )
}

