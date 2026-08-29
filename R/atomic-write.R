#' Safely write a file atomically via a temporary file and rename
#'
#' Writes to a temporary file in the same directory and renames over the destination.
#' On platforms or filesystems where rename cannot overwrite an existing destination,
#' safely falls back to copy with overwrite.
#'
#' @param write_fn Function taking a file path argument to execute the write.
#' @param target_file Target destination path.
#' @param pattern Tempfile prefix pattern.
#' @return Logical TRUE on success.
#' @noRd
atomic_write_file <- function(write_fn, target_file, pattern = "atomic_") {
  dir.create(dirname(target_file), showWarnings = FALSE, recursive = TRUE)
  tf <- tempfile(pattern = pattern, tmpdir = dirname(target_file))
  on.exit(if (file.exists(tf)) unlink(tf), add = TRUE)
  write_fn(tf)
  ok <- tryCatch(file.rename(tf, target_file), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    copy_ok <- tryCatch(file.copy(tf, target_file, overwrite = TRUE), error = function(e) FALSE)
    if (!isTRUE(copy_ok)) {
      cli::cli_abort("failed to atomically write {.file {target_file}}",
                     class = "sas2r_write_failed")
    }
  }
  invisible(TRUE)
}

#' Safely write an R object as JSON atomically
#'
#' @param x Object to serialize to JSON.
#' @param path Destination file path.
#' @param overwrite Logical; if FALSE, abort if destination already exists.
#' @param pretty Logical; whether to format JSON nicely.
#' @param auto_unbox Logical; whether to unbox length-1 vectors.
#' @return The destination path invisibly.
#' @noRd
atomic_write_json <- function(x,
                              path,
                              overwrite = TRUE,
                              pretty = TRUE,
                              auto_unbox = TRUE) {
  if (!isTRUE(overwrite) && file.exists(path)) {
    cli::cli_abort(
      "target record already exists: {.file {path}}",
      class = "sas2r_record_exists"
    )
  }
  atomic_write_file(
    function(tf) {
      payload <- jsonlite::toJSON(
        x,
        auto_unbox = auto_unbox,
        null = "null",
        digits = NA,
        dataframe = "rows",
        pretty = pretty,
        force = TRUE
      )
      writeLines(as.character(payload), tf, useBytes = TRUE)
    },
    target_file = path
  )
  invisible(path)
}

#' Read a JSON record from disk
#'
#' @param path Path to JSON file.
#' @param simplifyVector Logical; whether to simplify JSON arrays to vectors.
#' @return Parsed R object.
#' @noRd
read_json_record <- function(path, simplifyVector = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "record file does not exist: {.file {path}}",
      class = "sas2r_record_not_found"
    )
  }
  jsonlite::fromJSON(path, simplifyVector = simplifyVector)
}
