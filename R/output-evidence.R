# Output review configuration, canonical root path confinement, private inventory, and fixed dataset readers

OUTPUT_EVIDENCE_MAX_ROOTS <- 50L
OUTPUT_EVIDENCE_MAX_DEPTH <- 10L
OUTPUT_EVIDENCE_MAX_FILES_PER_ROOT <- 500L
OUTPUT_EVIDENCE_MAX_FILE_BYTES <- 50 * 1024 * 1024L # 50 MB
OUTPUT_EVIDENCE_MAX_ROWS <- 50000L
OUTPUT_EVIDENCE_MAX_COLS <- 500L
OUTPUT_EVIDENCE_MAX_EXAMPLES <- 20L

# What each side of the comparison may be read from. The reference side is the
# SAS truth, so it admits only the two formats SAS itself writes: an `.rds`
# under a reference root is R output, and admitting it would let a candidate be
# compared against a copy of itself and pass. The candidate side is R output,
# read as `.rds` or exchanged as `.xpt`. [read_output_candidate()] dispatches
# on these, and docs/output-evidence.md documents them.
OUTPUT_REFERENCE_FORMATS <- c("sas7bdat", "xpt")
OUTPUT_CANDIDATE_FORMATS <- c("rds", "xpt")

OUTPUT_INVENTORY_COLUMNS <- c(
  "candidate_id", "side", "root_id", "libref", "relative_name",
  "logical_name", "format", "size_bytes", "file_hash", "status",
  "reason", "nrow", "ncol", "column_signature"
)

#' Return default or custom output evidence limits
#'
#' @param max_roots Maximum number of library roots to inventory.
#' @param max_depth Maximum directory recursion depth.
#' @param max_files_per_root Maximum number of candidates inventoried per root.
#' @param max_file_bytes Maximum file size in bytes.
#' @param max_rows Maximum allowed rows in a candidate dataset.
#' @param max_cols Maximum allowed columns in a candidate dataset.
#' @param max_examples Maximum discrepancy examples in a comparison report.
#' @return A named list of limit constants.
#' @noRd
output_evidence_limits <- function(max_roots = OUTPUT_EVIDENCE_MAX_ROOTS,
                                   max_depth = OUTPUT_EVIDENCE_MAX_DEPTH,
                                   max_files_per_root = OUTPUT_EVIDENCE_MAX_FILES_PER_ROOT,
                                   max_file_bytes = OUTPUT_EVIDENCE_MAX_FILE_BYTES,
                                   max_rows = OUTPUT_EVIDENCE_MAX_ROWS,
                                   max_cols = OUTPUT_EVIDENCE_MAX_COLS,
                                   max_examples = OUTPUT_EVIDENCE_MAX_EXAMPLES) {
  list(
    max_roots = as.integer(max_roots),
    max_depth = as.integer(max_depth),
    max_files_per_root = as.integer(max_files_per_root),
    max_file_bytes = as.numeric(max_file_bytes),
    max_rows = as.integer(max_rows),
    max_cols = as.integer(max_cols),
    max_examples = as.integer(max_examples)
  )
}

#' Canonicalize and confine an evidence path within an authorized root
#'
#' Resolves symlinks and ensures the target path is inside or identical to the
#' authorized canonical root directory.
#'
#' @param path Path to verify.
#' @param root Authorized root directory.
#' @return Canonical path string with forward slashes.
#' @noRd
confine_evidence_path <- function(path, root) {
  if (!is_scalar_character(root) || !nzchar(root)) {
    cli::cli_abort("Evidence root must be a single non-empty string",
                   class = "sas2r_evidence_path_error")
  }
  if (!dir.exists(root) && !file.exists(root)) {
    cli::cli_abort("Evidence root {.file {root}} does not exist",
                   class = "sas2r_evidence_path_error")
  }
  if (!is_scalar_character(path) || !nzchar(path)) {
    cli::cli_abort("Evidence path must be a single non-empty string",
                   class = "sas2r_evidence_path_error")
  }
  if (!file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("Evidence path {.file {path}} does not exist",
                   class = "sas2r_evidence_path_error")
  }

  canon_root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  canon_path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  is_within <- identical(canon_path, canon_root) || startsWith(canon_path, paste0(canon_root, "/"))
  if (!is_within) {
    cli::cli_abort(
      c("Evidence path escapes authorized root.",
        "x" = "Path {.file {path}} resolves to {.file {canon_path}}.",
        "i" = "Authorized root is {.file {canon_root}}."),
      class = "sas2r_evidence_path_escape"
    )
  }
  canon_path
}

#' Test whether a path names a readable regular file
#'
#' A FIFO, socket, or device node called `adsl.xpt` inside an authorized root is
#' enumerated by [list.files()] like any other candidate, and both
#' `cli::hash_file_sha256()` and `haven::read_xpt()` open it -- which blocks
#' forever on a FIFO with no writer. base R exposes no `st_mode` file type
#' (`file.info()$mode` is masked to the permission bits), so the type is decided
#' from the two facts `stat` does report: a directory is `isdir`, and every
#' non-regular type [list.files()] can hand back -- FIFO, socket, character
#' device, block device -- reports `st_size == 0`. A zero-byte *regular* file is
#' refused along with them, which costs nothing: `sas7bdat`, `xpt`, and `rds`
#' each carry a mandatory non-empty header, so an empty candidate was never a
#' readable dataset.
#'
#' @param path A path to test.
#' @return `TRUE` when the path names a non-empty, non-directory file.
#' @noRd
is_regular_evidence_file <- function(path) {
  if (!is_scalar_character(path)) return(FALSE)
  info <- file.info(path)
  isdir <- info$isdir[1]
  size <- info$size[1]
  !is.na(isdir) && !isTRUE(isdir) && !is.na(size) && size > 0
}

#' Check evidence file size against configured limit
#'
#' @param path Canonical file path.
#' @param max_bytes Maximum allowed file size in bytes.
#' @return File size in bytes.
#' @noRd
check_evidence_file_size <- function(path, max_bytes) {
  info <- file.info(path)
  size <- info$size
  if (is.na(size) || size > max_bytes) {
    cli::cli_abort(
      c("Evidence file exceeds size limit.",
        "x" = "File {.file {path}} is {size} bytes (limit: {max_bytes} bytes)."),
      class = "sas2r_evidence_resource_limit"
    )
  }
  size
}

#' Enforce dataset dimension limits
#'
#' @param value A data frame object.
#' @param limits Evidence limits list.
#' @return The data frame if valid.
#' @noRd
enforce_evidence_shape <- function(value, limits = output_evidence_limits()) {
  nr <- nrow(value)
  nc <- ncol(value)
  if (nr > limits$max_rows || nc > limits$max_cols) {
    cli::cli_abort(
      c("Dataset exceeds dimension limits.",
        "x" = "Dimensions are {nr} x {nc} (limit: {limits$max_rows} rows x {limits$max_cols} cols)."),
      class = "sas2r_evidence_resource_limit"
    )
  }
  value
}

#' Build a confined local output evidence inventory
#'
#' When output review is disabled, returns an empty inventory. When enabled,
#' walks authorized reference and candidate library roots within fixed limits,
#' assigning sequential opaque IDs and storing canonical paths only in a private
#' resolver environment.
#'
#' @param project A `sas2r_project` object, or `NULL`.
#' @param config A `sas2r_config` object, or `NULL`.
#' @param limits Resource limits list from [output_evidence_limits()].
#' @return A `sas2r_output_inventory` object.
#' @noRd
build_output_inventory <- function(project = NULL, config = NULL,
                                   limits = output_evidence_limits()) {
  if (is.null(config) || !isTRUE(config$output_review$enabled)) {
    empty_env <- new.env(parent = emptyenv())
    return(structure(
      list(inventory = tibble::tibble(), resolver = empty_env),
      class = "sas2r_output_inventory",
      resolver = empty_env
    ))
  }

  # Collect reference library roots
  ref_roots <- list()
  project_has_registry <- !is.null(project) && inherits(project, "sas2r_project") &&
    !is.null(project$libref_registry)
  if (project_has_registry) {
    # effective_librefs() is the single projection of which directories the
    # emitted R actually binds: statement-level libname overrides, configured
    # fallbacks, resolution status, and the `work` exclusion are all decided
    # there. Swallowing a failure here and re-deriving roots from
    # config$libraries would compare against a different set of directories
    # than the translation writes to, with nothing raised.
    eff <- tryCatch(
      effective_librefs(project),
      error = function(e) {
        cli::cli_abort(
          c("Cannot project effective librefs for output evidence.",
            "x" = conditionMessage(e),
            "i" = "Reference roots come only from {.fn effective_librefs}; there is no second derivation to fall back to."),
          class = "sas2r_output_evidence_libref_error",
          parent = e
        )
      }
    )
    if (!is.null(eff$bindings) && nrow(eff$bindings) > 0L) {
      for (i in seq_len(nrow(eff$bindings))) {
        b_libref <- tolower(eff$bindings$libref[i])
        b_path <- eff$bindings$selected_path[i]
        if (!is.na(b_libref) && b_libref != "work" && !is.na(b_path) &&
            nzchar(b_path) && dir.exists(b_path)) {
          ref_roots[[b_libref]] <- normalizePath(b_path, winslash = "/", mustWork = FALSE)
        }
      }
    }
  }

  # Only when no project projection exists at all. A project that carries a
  # registry has already had every one of these decisions made for it.
  if (!project_has_registry && length(ref_roots) == 0L &&
      !is.null(config$libraries) && length(config$libraries) > 0L) {
    for (nm in names(config$libraries)) {
      lib_entry <- config$libraries[[nm]]
      p <- lib_entry$path
      if (is.character(p) && length(p) == 1L && !is.na(p) && nzchar(p) && dir.exists(p)) {
        ref_roots[[tolower(nm)]] <- normalizePath(p, winslash = "/", mustWork = FALSE)
      }
    }
  }

  # Collect candidate library roots
  cand_roots <- list()
  if (!is.null(config$output_review$r_libraries) && length(config$output_review$r_libraries) > 0L) {
    for (nm in names(config$output_review$r_libraries)) {
      p <- config$output_review$r_libraries[[nm]]
      if (is.character(p) && length(p) == 1L && !is.na(p) && nzchar(p) && dir.exists(p)) {
        cand_roots[[tolower(nm)]] <- normalizePath(p, winslash = "/", mustWork = FALSE)
      }
    }
  }

  records <- list()

  # Helper to scan a single root
  scan_root <- function(root_path, libref, side, allowed_exts) {
    if (!dir.exists(root_path)) return(list())
    canon_root <- tryCatch(confine_evidence_path(root_path, root_path), error = function(e) NULL)
    if (is.null(canon_root)) return(list())

    all_files <- list.files(canon_root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    if (!length(all_files)) return(list())

    # Radix, not locale collation: opaque candidate IDs are assigned
    # positionally from this order, so a locale-dependent sort would make
    # candidate_000001 name a different file on a different machine.
    all_files <- sort(all_files, method = "radix")

    # Filter depth
    rel_paths <- substring(all_files, nchar(canon_root) + 2L)
    depths <- vapply(strsplit(rel_paths, "[/\\\\]"), length, integer(1))
    within_depth <- depths <= limits$max_depth
    all_files <- all_files[within_depth]
    rel_paths <- rel_paths[within_depth]

    # Filter extension
    ext_pattern <- paste0("\\.(", paste(allowed_exts, collapse = "|"), ")$")
    matching <- grepl(ext_pattern, all_files, ignore.case = TRUE)
    all_files <- all_files[matching]
    rel_paths <- rel_paths[matching]

    if (!length(all_files)) return(list())

    # A cap that still emits a full record per file is not a cap: a root with
    # 200,000 candidates produced 200,000 rows and 200,000 resolver bindings.
    # Stop at the limit and record the truncation once.
    n_matching <- length(all_files)
    root_truncated <- n_matching > limits$max_files_per_root
    if (root_truncated) {
      keep <- seq_len(max(limits$max_files_per_root, 0L))
      all_files <- all_files[keep]
      rel_paths <- rel_paths[keep]
    }

    root_records <- list()
    for (idx in seq_along(all_files)) {
      f <- all_files[idx]
      rel <- rel_paths[idx]
      canon_file <- tryCatch(confine_evidence_path(f, canon_root), error = function(e) NULL)
      if (is.null(canon_file)) {
        # symlink escaped root
        root_records[[length(root_records) + 1L]] <- list(
          side = side,
          libref = tolower(libref),
          relative_name = rel,
          logical_name = paste0(tolower(libref), ".", tolower(tools::file_path_sans_ext(basename(rel)))),
          format = tolower(tools::file_ext(rel)),
          size_bytes = 0,
          file_hash = "",
          status = "resource_limit",
          reason = "symlink_escape",
          path = f,
          root = canon_root
        )
        next
      }

      # Before anything opens the file. cli::hash_file_sha256() below is a read,
      # and a read of a FIFO blocks forever.
      if (!is_regular_evidence_file(canon_file)) {
        root_records[[length(root_records) + 1L]] <- list(
          side = side,
          libref = tolower(libref),
          relative_name = rel,
          logical_name = paste0(tolower(libref), ".", tolower(tools::file_path_sans_ext(basename(rel)))),
          format = tolower(tools::file_ext(rel)),
          size_bytes = 0,
          file_hash = "",
          status = "unsupported",
          reason = "not_a_regular_file",
          path = canon_file,
          root = canon_root
        )
        next
      }

      info <- file.info(canon_file)
      size <- as.numeric(info$size)
      too_large <- size > limits$max_file_bytes
      hash <- if (too_large) "" else as.character(cli::hash_file_sha256(canon_file))
      st <- if (too_large) "resource_limit" else "available"
      rs <- if (too_large) "file_size_limit_exceeded" else NA_character_

      stem <- tolower(tools::file_path_sans_ext(basename(rel)))
      root_records[[length(root_records) + 1L]] <- list(
        side = side,
        libref = tolower(libref),
        relative_name = rel,
        logical_name = paste0(tolower(libref), ".", stem),
        format = tolower(tools::file_ext(rel)),
        size_bytes = size,
        file_hash = hash,
        status = st,
        reason = rs,
        path = canon_file,
        root = canon_root
      )
    }

    if (root_truncated) {
      # One marker, not one per omitted file. The name cannot collide with a
      # dataset stem, so pairing never resolves a target onto it.
      root_records[[length(root_records) + 1L]] <- list(
        side = side,
        libref = tolower(libref),
        relative_name = "<max_files_per_root_exceeded>",
        logical_name = paste0(tolower(libref), ".<max_files_per_root_exceeded>"),
        format = "",
        size_bytes = 0,
        file_hash = "",
        status = "resource_limit",
        reason = "max_files_per_root_exceeded",
        path = canon_root,
        root = canon_root
      )
    }

    root_records
  }

  # Inventory reference roots
  for (libref in names(ref_roots)) {
    rec <- scan_root(ref_roots[[libref]], libref, "reference", OUTPUT_REFERENCE_FORMATS)
    records <- c(records, rec)
  }

  # Inventory candidate roots
  for (libref in names(cand_roots)) {
    rec <- scan_root(cand_roots[[libref]], libref, "candidate", OUTPUT_CANDIDATE_FORMATS)
    records <- c(records, rec)
  }

  if (length(records) == 0L) {
    empty_inv <- tibble::tibble(
      candidate_id = character(),
      side = character(),
      root_id = character(),
      libref = character(),
      relative_name = character(),
      logical_name = character(),
      format = character(),
      size_bytes = numeric(),
      file_hash = character(),
      status = character(),
      reason = character(),
      nrow = integer(),
      ncol = integer(),
      column_signature = character()
    )
    empty_env <- new.env(parent = emptyenv())
    return(structure(
      list(inventory = empty_inv, resolver = empty_env),
      class = "sas2r_output_inventory",
      resolver = empty_env
    ))
  }

  # Stable sort
  sides <- vapply(records, `[[`, character(1), "side")
  librefs <- vapply(records, `[[`, character(1), "libref")
  rel_names <- vapply(records, `[[`, character(1), "relative_name")
  formats <- vapply(records, `[[`, character(1), "format")

  ord <- order(sides, librefs, rel_names, formats, method = "radix")
  records <- records[ord]

  # Assign root IDs
  unique_roots <- unique(vapply(records, function(r) paste0(r$side, "::", r$root), character(1)))
  root_map <- stats::setNames(sprintf("root_%04d", seq_along(unique_roots)), unique_roots)

  # Assign candidate IDs
  candidate_ids <- sprintf("candidate_%06d", seq_along(records))

  resolver_env <- new.env(parent = emptyenv())

  root_id_vec <- character(length(records))
  side_vec <- character(length(records))
  libref_vec <- character(length(records))
  rel_name_vec <- character(length(records))
  logical_name_vec <- character(length(records))
  format_vec <- character(length(records))
  size_bytes_vec <- numeric(length(records))
  file_hash_vec <- character(length(records))
  status_vec <- character(length(records))
  reason_vec <- character(length(records))

  for (i in seq_along(records)) {
    cid <- candidate_ids[i]
    r <- records[[i]]
    rid <- root_map[[paste0(r$side, "::", r$root)]]

    root_id_vec[i] <- rid
    side_vec[i] <- r$side
    libref_vec[i] <- r$libref
    rel_name_vec[i] <- r$relative_name
    logical_name_vec[i] <- r$logical_name
    format_vec[i] <- r$format
    size_bytes_vec[i] <- r$size_bytes
    file_hash_vec[i] <- r$file_hash
    status_vec[i] <- r$status
    reason_vec[i] <- r$reason

    resolver_env[[cid]] <- list(
      candidate_id = cid,
      side = r$side,
      root_id = rid,
      libref = r$libref,
      relative_name = r$relative_name,
      logical_name = r$logical_name,
      format = r$format,
      size_bytes = r$size_bytes,
      file_hash = r$file_hash,
      status = r$status,
      reason = r$reason,
      path = r$path,
      root = r$root
    )
  }

  inv_tbl <- tibble::tibble(
    candidate_id = candidate_ids,
    side = side_vec,
    root_id = root_id_vec,
    libref = libref_vec,
    relative_name = rel_name_vec,
    logical_name = logical_name_vec,
    format = format_vec,
    size_bytes = size_bytes_vec,
    file_hash = file_hash_vec,
    status = status_vec,
    reason = reason_vec,
    nrow = rep(NA_integer_, length(records)),
    ncol = rep(NA_integer_, length(records)),
    column_signature = rep(NA_character_, length(records))
  )

  # Attribute-only: as a list element the private path map is a public
  # accessor, and saveRDS()/as.list() would embed absolute paths in a
  # durable artifact.
  structure(
    list(inventory = inv_tbl),
    class = "sas2r_output_inventory",
    resolver = resolver_env
  )
}

#' Validate a resolved candidate pair before it may be marked `paired`
#'
#' Confinement, same-physical-file, and hash-freshness are properties of the
#' pair, not of how the pair was chosen, so the deterministic and the
#' agent-resolved paths must both clear them. A pair that points both sides at
#' one physical file would otherwise compare a file with itself and report a
#' manufactured clean result.
#'
#' @param ref_id,cand_id Opaque candidate IDs.
#' @param inventory A `sas2r_output_inventory`.
#' @return `list(ok = TRUE)`, or `list(ok = FALSE, reason = <character>)`.
#' @noRd
validate_output_pair <- function(ref_id, cand_id, inventory) {
  ref_meta <- tryCatch(resolve_inventory_candidate(ref_id, inventory), error = function(e) NULL)
  cand_meta <- tryCatch(resolve_inventory_candidate(cand_id, inventory), error = function(e) NULL)
  if (is.null(ref_meta) || is.null(cand_meta)) {
    return(list(ok = FALSE, reason = "candidate_metadata_resolution_failed"))
  }
  ref_canon <- tryCatch(confine_evidence_path(ref_meta$path, ref_meta$root), error = function(e) NULL)
  cand_canon <- tryCatch(confine_evidence_path(cand_meta$path, cand_meta$root), error = function(e) NULL)
  if (is.null(ref_canon) || is.null(cand_canon)) {
    return(list(ok = FALSE, reason = "confinement_verification_failed"))
  }
  if (identical(ref_canon, cand_canon)) {
    return(list(ok = FALSE, reason = "same_physical_file"))
  }
  # Before the hashes below, which are reads: a read of a device file blocks.
  if (!is_regular_evidence_file(ref_canon) || !is_regular_evidence_file(cand_canon)) {
    return(list(ok = FALSE, reason = "not_a_regular_file"))
  }
  ref_hash_cur <- tryCatch(as.character(cli::hash_file_sha256(ref_canon)), error = function(e) "")
  cand_hash_cur <- tryCatch(as.character(cli::hash_file_sha256(cand_canon)), error = function(e) "")
  if (nzchar(ref_meta$file_hash) && ref_hash_cur != ref_meta$file_hash) {
    return(list(ok = FALSE, reason = "stale_reference_file_hash"))
  }
  if (nzchar(cand_meta$file_hash) && cand_hash_cur != cand_meta$file_hash) {
    return(list(ok = FALSE, reason = "stale_candidate_file_hash"))
  }
  list(ok = TRUE, reason = NA_character_)
}

#' Resolve candidate metadata from inventory resolver map
#'
#' @param candidate_id Opaque candidate ID string.
#' @param inventory A `sas2r_output_inventory` object.
#' @return Candidate metadata list including canonical path and root.
#' @noRd
resolve_inventory_candidate <- function(candidate_id, inventory) {
  if (!is_scalar_character(candidate_id) || !nzchar(candidate_id)) {
    cli::cli_abort("candidate_id must be a single non-empty string",
                   class = "sas2r_candidate_not_found")
  }
  resolver <- if (inherits(inventory, "sas2r_output_inventory")) {
    inventory$resolver %||% attr(inventory, "resolver")
  } else if (inherits(inventory, "sas2r_output_target_plan")) {
    inventory$resolver %||% attr(inventory, "resolver")
  } else if (is.environment(inventory)) {
    inventory
  } else {
    NULL
  }

  if (is.null(resolver) || !exists(candidate_id, envir = resolver, inherits = FALSE)) {
    cli::cli_abort("candidate {.val {candidate_id}} not found in output inventory",
                   class = "sas2r_candidate_not_found")
  }
  get(candidate_id, envir = resolver)
}

#' Read an output candidate dataset through fixed package readers
#'
#' @param candidate_id Opaque candidate ID string.
#' @param inventory A `sas2r_output_inventory` object.
#' @param limits Resource limits list from [output_evidence_limits()].
#' @return A data frame object.
#' @noRd
read_output_candidate <- function(candidate_id, inventory,
                                  limits = output_evidence_limits()) {
  resolved <- resolve_inventory_candidate(candidate_id, inventory)
  path <- confine_evidence_path(resolved$path, resolved$root)
  # Ahead of every reader below: haven::read_sas()/read_xpt() and readRDS() all
  # open the path, and opening a FIFO or device node never returns.
  if (!is_regular_evidence_file(path)) {
    cli::cli_abort(
      c("Evidence candidate is not a readable regular file.",
        "x" = "{.file {path}} is a directory, an empty file, or a device file."),
      class = "sas2r_evidence_shape_error"
    )
  }
  check_evidence_file_size(path, limits$max_file_bytes)
  value <- switch(
    resolved$format,
    sas7bdat = haven::read_sas(path),
    xpt = haven::read_xpt(path),
    rds = readRDS(path),
    cli::cli_abort("unsupported evidence format {.val {resolved$format}}",
                   class = "sas2r_evidence_format_error")
  )
  if (!inherits(value, "data.frame")) {
    cli::cli_abort("candidate is not a rectangular dataset",
                   class = "sas2r_evidence_shape_error")
  }
  enforce_evidence_shape(value, limits)
}

#' @export
print.sas2r_output_inventory <- function(x, ...) {
  n <- nrow(x$inventory)
  cli::cat_line(cli::rule(left = "sas2r output inventory"))
  cli::cat_line(paste0("Total candidates: ", n))
  if (n > 0L) {
    ref_n <- sum(x$inventory$side == "reference")
    cand_n <- sum(x$inventory$side == "candidate")
    cli::cat_line(paste0("  Reference: ", ref_n, ", Candidate: ", cand_n))
  }
  invisible(x)
}

OUTPUT_TLF_PROCS <- c("sgplot", "sgpanel", "sgrender", "report", "tabulate", "print")

OUTPUT_TARGET_ROLES <- c("final_dataset", "tlf_preparation_data", "input_only_nonprobative")

OUTPUT_TARGET_STATUSES <- c(
  "disabled", "paired", "missing_reference", "missing_candidate",
  "ambiguous", "resource_limit", "unsupported", "input_only_nonprobative",
  # A pair that was resolved but whose comparison could not be produced.
  "comparison_failed"
)

OUTPUT_TARGET_PLAN_COLUMNS <- c(
  "target_id", "logical_dataset", "role", "program", "consumer_proc",
  "contributing_unit_ids", "reference_candidate_id", "r_candidate_id",
  "reference_options", "r_options", "pairing_method", "status", "reason"
)

#' Discover final output datasets and TLF preparation data from project lineage
#'
#' @param project A `sas2r_project` object.
#' @return A tibble of discovered target datasets.
#' @noRd
discover_output_targets <- function(project) {
  empty_targets <- tibble::tibble(
    target_id = character(),
    logical_dataset = character(),
    role = character(),
    program = character(),
    consumer_proc = character(),
    contributing_unit_ids = list()
  )

  if (is.null(project) || !inherits(project, "sas2r_project") ||
      is.null(project$units) || nrow(project$units) == 0L ||
      is.null(project$lineage) || nrow(project$lineage) == 0L) {
    return(empty_targets)
  }

  programs <- unique(project$units$file)
  if (!length(programs)) return(empty_targets)

  contracts <- infer_output_contracts(project)
  creates <- project$lineage[project$lineage$role == "creates", ]
  reads <- project$lineage[project$lineage$role == "reads", ]

  find_contributing_units <- function(ds, creates_df, reads_df) {
    direct <- unique(creates_df$unit_id[creates_df$dataset == ds])
    if (!length(direct)) return(integer(0))
    visited <- integer(0)
    queue <- direct
    while (length(queue) > 0L) {
      u <- queue[1]
      queue <- queue[-1]
      if (u %in% visited) next
      visited <- c(visited, u)
      u_reads <- unique(reads_df$dataset[reads_df$unit_id == u])
      for (r_ds in u_reads) {
        r_creators <- unique(creates_df$unit_id[creates_df$dataset == r_ds & creates_df$unit_id < u])
        for (rc in r_creators) {
          if (!rc %in% visited && !rc %in% queue) {
            queue <- c(queue, rc)
          }
        }
      }
    }
    sort(unique(visited))
  }

  records <- list()

  for (prog_file in programs) {
    prog_name <- basename(prog_file)
    prog_units <- project$units[project$units$file == prog_file, ]
    p_unit_ids <- prog_units$unit_id
    p_lineage <- project$lineage[project$lineage$unit_id %in% p_unit_ids, ]
    if (!nrow(p_lineage)) next

    p_creates <- p_lineage[p_lineage$role == "creates", ]
    p_reads <- p_lineage[p_lineage$role == "reads", ]

    tlf_handled_datasets <- character()

    # 1. Output PROC steps
    if ("proc" %in% names(p_reads) || !is.null(p_reads$proc)) {
      proc_col <- tolower(p_reads$proc %||% character())
      tlf_idx <- which(proc_col %in% OUTPUT_TLF_PROCS)
      for (idx in tlf_idx) {
        ds <- p_reads$dataset[idx]
        proc_nm <- proc_col[idx]
        u_id <- p_reads$unit_id[idx]

        # Check if created in same program prior to consumer
        created_before <- any(p_creates$dataset == ds & p_creates$unit_id < u_id)
        if (created_before) {
          is_persistent <- sub("\\..*$", "", ds) != "work"
          if (is_persistent) {
            tlf_handled_datasets <- c(tlf_handled_datasets, ds)
            records[[length(records) + 1L]] <- list(
              logical_dataset = ds,
              role = "tlf_preparation_data",
              program = prog_name,
              consumer_proc = proc_nm,
              contributing_unit_ids = find_contributing_units(ds, creates, reads)
            )
          }
        } else {
          records[[length(records) + 1L]] <- list(
            logical_dataset = ds,
            role = "input_only_nonprobative",
            program = prog_name,
            consumer_proc = proc_nm,
            contributing_unit_ids = as.integer(u_id)
          )
        }
      }
    }

    # 2. Final datasets from inferred contracts
    prog_contracts <- contracts[contracts$kind == "dataset" & (contracts$source_file == prog_file | is.na(contracts$source_file)), ]
    if (nrow(prog_contracts) > 0L) {
      for (c_idx in seq_len(nrow(prog_contracts))) {
        ds <- prog_contracts$logical_name[c_idx]
        if (ds %in% tlf_handled_datasets) next
        if (any(p_creates$dataset == ds)) {
          records[[length(records) + 1L]] <- list(
            logical_dataset = ds,
            role = "final_dataset",
            program = prog_name,
            consumer_proc = NA_character_,
            contributing_unit_ids = find_contributing_units(ds, creates, reads)
          )
        }
      }
    }
  }

  if (!length(records)) return(empty_targets)

  target_ids <- sprintf("target_%04d", seq_along(records))
  log_ds <- vapply(records, `[[`, character(1), "logical_dataset")
  roles <- vapply(records, `[[`, character(1), "role")
  progs <- vapply(records, `[[`, character(1), "program")
  c_procs <- vapply(records, function(r) r$consumer_proc %||% NA_character_, character(1))
  c_units <- lapply(records, function(r) as.integer(r$contributing_unit_ids))

  tibble::tibble(
    target_id = target_ids,
    logical_dataset = log_ds,
    role = roles,
    program = progs,
    consumer_proc = c_procs,
    contributing_unit_ids = c_units
  )
}

#' Pair output candidates deterministically by exact normalized names
#'
#' @param targets Discovered output targets tibble.
#' @param inventory A `sas2r_output_inventory` object.
#' @return A tibble with all 13 target plan columns.
#' @noRd
pair_output_candidates <- function(targets, inventory) {
  empty_plan <- tibble::tibble(
    target_id = character(),
    logical_dataset = character(),
    role = character(),
    program = character(),
    consumer_proc = character(),
    contributing_unit_ids = list(),
    reference_candidate_id = character(),
    r_candidate_id = character(),
    reference_options = list(),
    r_options = list(),
    pairing_method = character(),
    status = character(),
    reason = character()
  )

  if (is.null(targets) || nrow(targets) == 0L) {
    return(empty_plan)
  }

  inv_df <- if (inherits(inventory, "sas2r_output_inventory")) {
    inventory$inventory
  } else if (inherits(inventory, "data.frame")) {
    inventory
  } else {
    tibble::tibble()
  }

  n <- nrow(targets)
  target_ids <- targets$target_id
  log_ds <- targets$logical_dataset
  roles <- targets$role
  progs <- targets$program
  c_procs <- targets$consumer_proc
  c_units <- targets$contributing_unit_ids

  ref_cand_id_vec <- character(n)
  r_cand_id_vec <- character(n)
  ref_options_list <- vector("list", n)
  r_options_list <- vector("list", n)
  pairing_method_vec <- character(n)
  status_vec <- character(n)
  reason_vec <- character(n)

  for (i in seq_len(n)) {
    cur_role <- roles[i]
    cur_ds <- tolower(log_ds[i])
    cur_libref <- sub("\\..*$", "", cur_ds)
    cur_stem <- sub("^.*\\.", "", cur_ds)

    if (cur_role == "input_only_nonprobative") {
      ref_cand_id_vec[i] <- NA_character_
      r_cand_id_vec[i] <- NA_character_
      ref_options_list[[i]] <- character(0)
      r_options_list[[i]] <- character(0)
      pairing_method_vec[i] <- "input_only_nonprobative"
      status_vec[i] <- "input_only_nonprobative"
      reason_vec[i] <- "upstream_input_only"
      next
    }

    # Reference matching
    ref_rows <- inv_df[inv_df$side == "reference", ]
    exact_ref <- ref_rows[tolower(ref_rows$logical_name) == cur_ds, ]
    ref_matches <- if (nrow(exact_ref) > 0L) {
      exact_ref
    } else if (nrow(ref_rows) > 0L) {
      ref_rows[tolower(tools::file_path_sans_ext(basename(ref_rows$relative_name))) == cur_stem, ]
    } else {
      ref_rows[0, ]
    }
    ref_ids <- ref_matches$candidate_id

    # Candidate matching
    cand_rows <- inv_df[inv_df$side == "candidate", ]
    exact_cand <- cand_rows[tolower(cand_rows$logical_name) == cur_ds, ]
    cand_matches <- if (nrow(exact_cand) > 0L) {
      exact_cand
    } else if (nrow(cand_rows) > 0L) {
      cand_rows[tolower(tools::file_path_sans_ext(basename(cand_rows$relative_name))) == cur_stem, ]
    } else {
      cand_rows[0, ]
    }
    cand_ids <- cand_matches$candidate_id

    ref_options_list[[i]] <- ref_ids
    r_options_list[[i]] <- cand_ids

    if (length(ref_ids) == 0L) {
      ref_cand_id_vec[i] <- NA_character_
      r_cand_id_vec[i] <- if (length(cand_ids) == 1L) cand_ids[1] else NA_character_
      pairing_method_vec[i] <- "missing_reference"
      status_vec[i] <- "missing_reference"
      reason_vec[i] <- "reference_dataset_not_found"
    } else if (length(cand_ids) == 0L) {
      ref_cand_id_vec[i] <- if (length(ref_ids) == 1L) ref_ids[1] else NA_character_
      r_cand_id_vec[i] <- NA_character_
      pairing_method_vec[i] <- "missing_candidate"
      status_vec[i] <- "missing_candidate"
      reason_vec[i] <- "r_candidate_not_found"
    } else if (length(ref_ids) > 1L) {
      ref_cand_id_vec[i] <- NA_character_
      r_cand_id_vec[i] <- if (length(cand_ids) == 1L) cand_ids[1] else NA_character_
      pairing_method_vec[i] <- "ambiguous"
      status_vec[i] <- "ambiguous"
      reason_vec[i] <- "multiple_reference_options"
    } else if (length(cand_ids) > 1L) {
      ref_cand_id_vec[i] <- ref_ids[1]
      r_cand_id_vec[i] <- NA_character_
      pairing_method_vec[i] <- "ambiguous"
      status_vec[i] <- "ambiguous"
      reason_vec[i] <- "multiple_candidate_options"
    } else {
      # Exactly 1 on each side
      ref_id <- ref_ids[1]
      cand_id <- cand_ids[1]
      ref_cand_id_vec[i] <- ref_id
      r_cand_id_vec[i] <- cand_id

      # Resource limit checks from inventory
      ref_row <- ref_matches[1, ]
      cand_row <- cand_matches[1, ]

      if (ref_row$status == "resource_limit") {
        pairing_method_vec[i] <- "exact_logical_name"
        status_vec[i] <- "resource_limit"
        reason_vec[i] <- paste0("reference_", ref_row$reason)
      } else if (cand_row$status == "resource_limit") {
        pairing_method_vec[i] <- "exact_logical_name"
        status_vec[i] <- "resource_limit"
        reason_vec[i] <- paste0("candidate_", cand_row$reason)
      } else {
        # Confinement, same physical file, and hash freshness validation
        chk <- validate_output_pair(ref_id, cand_id, inventory)
        pairing_method_vec[i] <- "exact_logical_name"
        status_vec[i] <- if (chk$ok) "paired" else "unsupported"
        reason_vec[i] <- chk$reason
      }
    }
  }

  tibble::tibble(
    target_id = target_ids,
    logical_dataset = log_ds,
    role = roles,
    program = progs,
    consumer_proc = c_procs,
    contributing_unit_ids = c_units,
    reference_candidate_id = ref_cand_id_vec,
    r_candidate_id = r_cand_id_vec,
    reference_options = ref_options_list,
    r_options = r_options_list,
    pairing_method = pairing_method_vec,
    status = status_vec,
    reason = reason_vec
  )
}

#' Build a read-only output target plan
#'
#' @param project A `sas2r_project` object, or `NULL`.
#' @param config A `sas2r_config` object, or `NULL`.
#' @param limits Resource limits list from [output_evidence_limits()].
#' @return A `sas2r_output_target_plan` object.
#' @noRd
build_output_target_plan <- function(project = NULL, config = NULL,
                                    limits = output_evidence_limits()) {
  if (is.null(config) && !is.null(project) && inherits(project, "sas2r_project")) {
    config <- project$config
  }

  if (is.null(config) || !isTRUE(config$output_review$enabled)) {
    sentinel_plan <- tibble::tibble(
      target_id = "target_disabled",
      logical_dataset = "",
      role = "final_dataset",
      program = "",
      consumer_proc = NA_character_,
      contributing_unit_ids = list(integer(0)),
      reference_candidate_id = NA_character_,
      r_candidate_id = NA_character_,
      reference_options = list(character(0)),
      r_options = list(character(0)),
      pairing_method = "disabled",
      status = "disabled",
      reason = "output_review_disabled"
    )
    empty_env <- new.env(parent = emptyenv())
    return(structure(
      list(plan = sentinel_plan, inventory = tibble::tibble()),
      class = "sas2r_output_target_plan",
      resolver = empty_env
    ))
  }

  inventory <- build_output_inventory(project, config, limits = limits)
  targets <- discover_output_targets(project)
  paired_plan <- pair_output_candidates(targets, inventory)

  structure(
    list(
      plan = paired_plan,
      inventory = inventory$inventory
    ),
    class = "sas2r_output_target_plan",
    resolver = attr(inventory, "resolver", exact = TRUE)
  )
}

#' @export
print.sas2r_output_target_plan <- function(x, ...) {
  cli::cat_line(cli::rule(left = "sas2r output target plan"))
  if (nrow(x$plan) == 0L) {
    cli::cat_line("No output targets discovered.")
    return(invisible(x))
  }
  if (identical(x$plan$status[1], "disabled")) {
    cli::cat_line("Output review is disabled.")
    return(invisible(x))
  }
  cli::cat_line(paste0("Total targets: ", nrow(x$plan)))
  status_counts <- table(x$plan$status)
  for (st in names(status_counts)) {
    cli::cat_line(sprintf("  %s: %d", st, status_counts[[st]]))
  }
  invisible(x)
}

#' @export
as.data.frame.sas2r_output_target_plan <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$plan, row.names = row.names, optional = optional, ...)
}

#' @export
`$.sas2r_output_target_plan` <- function(x, name) {
  if (name %in% names(x)) return(x[[name]])
  if ("plan" %in% names(x) && is.data.frame(x$plan) && name %in% names(x$plan)) {
    return(x$plan[[name]])
  }
  NULL
}

#' Resolve ambiguous candidate pairs using structured agent choice
#'
#' @param plan A `sas2r_output_target_plan` object or data frame.
#' @param llm `sas2r_llm` instance.
#' @param specs Loaded agent specs list.
#' @param usage_budget Shared usage budget instance.
#' @param log_dir Audit log directory.
#' @return The updated `sas2r_output_target_plan` or data frame.
#' @noRd
resolve_ambiguous_output_pairs <- function(plan, llm, specs = load_agent_specs(),
                                          usage_budget = NULL, log_dir = ".sas2r") {
  if (is.null(plan)) return(NULL)
  is_plan_obj <- inherits(plan, "sas2r_output_target_plan")
  plan_tbl <- if (is_plan_obj) plan$plan else plan

  if (!is.data.frame(plan_tbl) || nrow(plan_tbl) == 0L || is.null(llm)) {
    return(plan)
  }

  ambig_idx <- which(plan_tbl$status == "ambiguous")
  if (length(ambig_idx) == 0L) {
    return(plan)
  }

  if (is.null(usage_budget)) usage_budget <- new_usage_budget()

  for (i in ambig_idx) {
    ref_opts <- unlist(plan_tbl$reference_options[[i]])
    cand_opts <- unlist(plan_tbl$r_options[[i]])
    if (!length(ref_opts)) ref_opts <- plan_tbl$reference_candidate_id[i]
    if (!length(cand_opts)) cand_opts <- plan_tbl$r_candidate_id[i]
    ref_opts <- ref_opts[!is.na(ref_opts) & nzchar(ref_opts)]
    cand_opts <- cand_opts[!is.na(cand_opts) & nzchar(cand_opts)]

    if (length(ref_opts) == 0L || length(cand_opts) == 0L) next

    spec <- as.list(specs$reviewer)
    spec$prompt <- "reviewer-output-pairing.md"
    spec$output_schema <- "output_pairing_v1"

    prompt_vars <- list(
      target = sprintf(
        "target_id: %s\nlogical_dataset: %s\nrole: %s\nprogram: %s\nconsumer_proc: %s",
        plan_tbl$target_id[i], plan_tbl$logical_dataset[i], plan_tbl$role[i],
        plan_tbl$program[i], plan_tbl$consumer_proc[i] %||% ""
      ),
      reference_options = paste(ref_opts, collapse = ", "),
      r_options = paste(cand_opts, collapse = ", "),
      lineage = sprintf(
        "dataset: %s\ncontributing_units: %s",
        plan_tbl$logical_dataset[i],
        paste(unlist(plan_tbl$contributing_unit_ids[[i]]), collapse = ", ")
      ),
      skills = ""
    )

    ctx <- list(project = NULL, unit_stmts = NULL, schemas = list(), config = list())
    tools <- build_tools(spec, ctx)

    r <- run_agent(
      spec, llm, tools,
      user_content = paste0("Resolve candidate pairing for target ", plan_tbl$target_id[i]),
      log_dir = log_dir,
      prompt_vars = prompt_vars,
      usage_budget = usage_budget,
      audit_context = list(
        purpose = "output_pairing",
        target_id = plan_tbl$target_id[i]
      )
    )

    if (identical(r$status, "ok")) {
      chosen_ref <- r$data$reference_candidate_id
      chosen_cand <- r$data$r_candidate_id
      conf <- as.numeric(r$data$confidence %||% 0)

      is_valid_ref <- is.character(chosen_ref) && length(chosen_ref) == 1L &&
        chosen_ref %in% ref_opts && !grepl("[/\\\\]|\\.\\.", chosen_ref)
      is_valid_cand <- is.character(chosen_cand) && length(chosen_cand) == 1L &&
        chosen_cand %in% cand_opts && !grepl("[/\\\\]|\\.\\.", chosen_cand)
      is_confident <- !is.na(conf) && conf >= 0.7

      if (is_valid_ref && is_valid_cand && is_confident) {
        # An enumerated ID is not enough: the pair must still clear the same
        # confinement, same-physical-file, and hash-freshness guards the
        # deterministic path applies, or the model could pick two IDs that
        # resolve to one file and manufacture a clean result.
        chk <- validate_output_pair(chosen_ref, chosen_cand, plan)
        if (isTRUE(chk$ok)) {
          plan_tbl$reference_candidate_id[i] <- chosen_ref
          plan_tbl$r_candidate_id[i] <- chosen_cand
          plan_tbl$pairing_method[i] <- "agent_enumerated_choice"
          plan_tbl$status[i] <- "paired"
          plan_tbl$reason[i] <- NA_character_
        } else {
          plan_tbl$reason[i] <- chk$reason
        }
      } else if (!is_confident) {
        plan_tbl$reason[i] <- "agent_low_confidence"
      } else {
        plan_tbl$reason[i] <- "agent_selection_invalid"
      }
    } else {
      plan_tbl$reason[i] <- paste0("agent_", r$status)
    }
  }

  if (is_plan_obj) {
    plan$plan <- plan_tbl
    plan
  } else {
    plan_tbl
  }
}

