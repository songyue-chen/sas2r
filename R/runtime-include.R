# Runtime helpers: %INCLUDE. Part of the runtime every translated program
# carries; see ?sas2r_runtime.

#' Run an included module where the %INCLUDE stood
#'
#' Each included SAS file is translated once into its own staged module; this
#' executes that module at one include site, into the calling program's own
#' environment, exactly where the `%INCLUDE` stood.
#'
#' The path is always relative to the staged bundle root -- the nearest
#' ancestor of the running script holding `autoexec.R` -- and never to
#' the working directory, so the bundle can be run from anywhere. Absolute
#' paths and `.` or `..` components are rejected, and the file the joined path
#' actually resolves to is then required to lie under the root: rejecting the
#' components alone would still let a symlinked directory inside the bundle
#' read outside it, so the confinement is checked on the resolved path, where
#' a hand-edited bundle cannot talk its way out.
#'
#' @param relative_path The module's path relative to the bundle root.
#' @param envir The environment to run it in; the caller's by default.
#' @return The resolved path of the module, invisibly.
#' @family runtime helpers
#' @examples
#' \dontrun{
#' # inside a translated program, where the SAS source had %include 'inc/prep.sas';
#' sas2r_source_include("inc/prep.R")
#' }
#' @export
sas2r_source_include <- function(relative_path, envir = parent.frame()) {
  fail <- function(cls, msg) {
    stop(structure(
      class = c(cls, "sas2r_include_error", "error", "condition"),
      list(message = paste0("sas2r_source_include: ", msg), call = NULL)
    ))
  }
  if (!is.character(relative_path) || length(relative_path) != 1L ||
      is.na(relative_path) || !nzchar(relative_path)) {
    fail("sas2r_include_path_error",
         "relative_path must be one non-empty string")
  }
  if (!is.environment(envir)) {
    fail("sas2r_include_envir_error", "envir must be an environment")
  }
  path <- gsub("\\\\", "/", relative_path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (grepl("^(/|[A-Za-z]:/|//)", path) || !length(parts) ||
      any(!nzchar(parts)) || any(parts %in% c(".", ".."))) {
    fail("sas2r_include_path_error",
         paste0("unsafe staged include path: ", relative_path))
  }

  # Where the file now being sourced lives. source() and sys.source() each keep
  # the path in a frame variable, so the innermost of those frames names the
  # staged file that reached this call -- including a module that was itself
  # reached through an include. A driver started some other way (Rscript, say)
  # has no such frame, and the working directory is the documented fallback.
  start <- NULL
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    fn <- tryCatch(sys.function(i), error = function(e) NULL)
    slot <- if (identical(fn, base::sys.source)) "file" else
            if (identical(fn, base::source)) "ofile" else NULL
    if (is.null(slot)) next
    found <- tryCatch(get(slot, envir = frames[[i]], inherits = FALSE),
                      error = function(e) NULL)
    if (is.character(found) && length(found) == 1L && !is.na(found) &&
        nzchar(found)) {
      start <- dirname(found)
      break
    }
  }
  if (is.null(start)) start <- getwd()

  # Bounded walk upward: it stops at the filesystem root (where dirname() is a
  # fixed point) and after a fixed number of levels, so a bundle with no
  # registry fails with this error instead of looping.
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  root <- NULL
  for (level in seq_len(64L)) {
    if (file.exists(file.path(dir, "autoexec.R"))) {
      root <- dir
      break
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  if (is.null(root)) {
    fail("sas2r_include_root_error",
         paste0("no staged bundle root (autoexec.R) at or above ", start))
  }

  target <- file.path(root, path)
  if (!file.exists(target) || dir.exists(target)) {
    fail("sas2r_include_missing_error",
         paste0("staged include module not found: ", path))
  }
  # Re-confine after resolution: every component was ..-free, but a symlinked
  # directory inside the bundle resolves outside it all the same.
  real <- normalizePath(target, winslash = "/", mustWork = FALSE)
  real_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!startsWith(real, paste0(real_root, "/"))) {
    fail("sas2r_include_path_error",
         paste0("staged include module resolves outside the bundle: ", path))
  }
  sys.source(real, envir = envir)
  invisible(real)
}
