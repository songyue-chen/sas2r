# Limits, root path confinement, and fixed readers for saved output datasets

OUTPUT_EVIDENCE_MAX_FILES_PER_ROOT <- 500L
OUTPUT_EVIDENCE_MAX_FILE_BYTES <- 50 * 1024 * 1024L # 50 MB
OUTPUT_EVIDENCE_MAX_ROWS <- 50000L
OUTPUT_EVIDENCE_MAX_COLS <- 500L
OUTPUT_EVIDENCE_MAX_EXAMPLES <- 20L

# Formats a saved R output dataset is read from: `.rds`, or `.xpt` when it is
# exchanged. docs/output-evidence.md documents them.
OUTPUT_CANDIDATE_FORMATS <- c("rds", "xpt")

#' Return default or custom output evidence limits
#'
#' @param max_files_per_root Maximum number of candidate files read per attempt.
#' @param max_file_bytes Maximum file size in bytes.
#' @param max_rows Maximum allowed rows in a candidate dataset.
#' @param max_cols Maximum allowed columns in a candidate dataset.
#' @param max_examples Maximum discrepancy examples in a comparison report.
#' @return A named list of limit constants.
#' @noRd
output_evidence_limits <- function(max_files_per_root = OUTPUT_EVIDENCE_MAX_FILES_PER_ROOT,
                                   max_file_bytes = OUTPUT_EVIDENCE_MAX_FILE_BYTES,
                                   max_rows = OUTPUT_EVIDENCE_MAX_ROWS,
                                   max_cols = OUTPUT_EVIDENCE_MAX_COLS,
                                   max_examples = OUTPUT_EVIDENCE_MAX_EXAMPLES) {
  list(
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

# Fixed readers and the same resource limits for a known candidate path.
read_evidence_dataset <- function(path, format, limits = output_evidence_limits()) {
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
    format,
    sas7bdat = haven::read_sas(path),
    xpt = haven::read_xpt(path),
    rds = readRDS(path),
    cli::cli_abort("unsupported evidence format {.val {format}}",
                   class = "sas2r_evidence_format_error")
  )
  if (!inherits(value, "data.frame")) {
    cli::cli_abort("candidate is not a rectangular dataset",
                   class = "sas2r_evidence_shape_error")
  }
  enforce_evidence_shape(value, limits)
}
