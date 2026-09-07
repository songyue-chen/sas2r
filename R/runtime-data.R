# Runtime helpers: librefs and data access. Part of the runtime every
# translated program carries; see ?sas2r_runtime.

# The libref registry is the `.sas2r_registry` list the bundle's
# _sas2r_registry.R defines. The runtime finds it where it was loaded:
# lexically first -- the environment the runtime was sourced into, which is
# how a bundle program and a test harness both work -- then, for the package
# form of the runtime, out through the global environment, and finally in any
# active frame, so a registry defined inside a function is honoured too.
sas2r_registry_env <- function() {
  env <- parent.env(environment())
  while (!identical(env, emptyenv())) {
    if (exists(".sas2r_registry", envir = env, inherits = FALSE)) return(env)
    env <- parent.env(env)
  }
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (exists(".sas2r_registry", envir = frames[[i]], inherits = FALSE)) {
      return(frames[[i]])
    }
  }
  sas2r_libref_stop(
    "sas2r_no_registry",
    "No libref registry is loaded: source the bundle's _sas2r_registry.R first"
  )
}

#' Bind or clear a libref at its point of use
#'
#' The registry a bundle starts with is only a seed -- the libraries the
#' configuration declares, plus `work`. Every `LIBNAME` the SAS program
#' executes changes the registry where that statement stood, so a libref that
#' is rebound or cleared part-way through behaves at run time the way it does
#' in SAS. `sas2r_libname_assign()` is `LIBNAME libref '<path>'`;
#' `sas2r_libname_clear()` is `LIBNAME libref CLEAR`.
#'
#' @param libref The libref (case-insensitive).
#' @param read_path,write_path Directories the libref reads from and writes to.
#' @param engine Format of members read: `"sas7bdat"`, `"xpt"`, or `"rds"`.
#' @param write Format members are written in: `"rds"` or `"xpt"`.
#' @return `sas2r_libname_assign()` returns the new registry entry, invisibly;
#'   `sas2r_libname_clear()` returns `NULL`, invisibly.
#' @family runtime helpers
#' @examples
#' .sas2r_registry <- list()
#' sas2r_libname_assign("raw", tempdir(), engine = "rds")
#' names(.sas2r_registry)
#' sas2r_libname_clear("raw")
#' names(.sas2r_registry)
#' rm(.sas2r_registry)
#' @export
sas2r_libname_assign <- function(libref, read_path, write_path = read_path,
                                 engine = "sas7bdat", write = "rds") {
  env <- sas2r_registry_env()
  registry <- get(".sas2r_registry", envir = env, inherits = FALSE)
  key <- tolower(libref)
  registry[[key]] <- list(
    read_path = read_path,
    write_path = write_path,
    engine = engine,
    write = write
  )
  assign(".sas2r_registry", registry, envir = env)
  invisible(registry[[key]])
}

#' @rdname sas2r_libname_assign
#' @export
sas2r_libname_clear <- function(libref) {
  env <- sas2r_registry_env()
  registry <- get(".sas2r_registry", envir = env, inherits = FALSE)
  registry[[tolower(libref)]] <- NULL
  assign(".sas2r_registry", registry, envir = env)
  invisible(NULL)
}

# Internal: raise a classed libref error (sas2r_libref_error plus `cls`).
sas2r_libref_stop <- function(cls, msg) {
  stop(structure(
    class = c(cls, "sas2r_libref_error", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}

# The library a libref names is chosen from SAS source or from configuration
# and is deliberately *not* held inside the project -- a study library lives
# where the study keeps it. The confinement that does apply is this one: a
# member is a SAS dataset name, never a path, so the file it resolves to has to
# sit in the library directory itself and cannot walk out of it.
sas2r_lib_entry <- function(libref) {
  registry <- get(".sas2r_registry", envir = sas2r_registry_env(), inherits = FALSE)
  reg <- registry[[tolower(libref)]]
  if (is.null(reg)) {
    sas2r_libref_stop("sas2r_unknown_libref",
                      paste0("Unknown libref: ", libref))
  }
  reg
}

# Internal: the file a dataset member resolves to inside its library; refuses
# any member name that is not a plain name.
sas2r_lib_member_path <- function(dir, member, ext) {
  if (!is.character(member) || length(member) != 1L || is.na(member) ||
      !nzchar(member) || grepl("[/\\\\]", member) ||
      member %in% c(".", "..")) {
    sas2r_libref_stop("sas2r_libref_member_error",
                      paste0("Unsafe dataset member name: ", member))
  }
  file.path(dir, paste0(member, ext))
}

#' Fold column names to lower case
#'
#' SAS resolves variable names case-insensitively; R does not. Deterministic
#' translations fold every frame to lower-case names at entry so lower-case
#' source references (`aval`) find upper-case data columns (`AVAL`) the way
#' SAS would.
#'
#' @param df A data frame.
#' @return `df` with lower-case column names.
#' @family runtime helpers
#' @examples
#' names(sas2r_fold_names(data.frame(USUBJID = 1, AVAL = 2)))
#' @export
sas2r_fold_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

# Frames read by lib_read() carry class sas2r_dataset so `df$AVAL` and
# `df[["aval"]]` resolve case-insensitively, as SAS variable names do.
#' @export
#' @noRd
`$.sas2r_dataset` <- function(x, name) {
  idx <- match(tolower(name), tolower(names(x)))
  if (!is.na(idx)) x[[idx]] else NULL
}

#' @export
#' @noRd
`[[.sas2r_dataset` <- function(x, i, exact = TRUE) {
  if (is.character(i) && length(i) == 1L) {
    idx <- match(tolower(i), tolower(names(x)))
    if (!is.na(idx)) return(x[[idx]])
  }
  NextMethod()
}

#' Read and write datasets through librefs
#'
#' `lib_read("lib", "member")` reads a dataset from a libref as the registry
#' configures it: the libref's write path is tried first (so a dataset the
#' bundle produced earlier wins), then its read path, for `.rds`,
#' `.sas7bdat`, and `.xpt` in that order. The result carries class
#' `sas2r_dataset`, whose `$` and `[[` methods resolve column names
#' case-insensitively, as SAS variable names do. `lib_write(df, "lib",
#' "member")` writes to the libref's write path in its configured format
#' (`rds` or `xpt`), creating the directory on demand.
#'
#' Exactly one call form is accepted for each. Combined `"lib.member"`
#' strings, single-argument calls, and `dataset=`/`table=` aliases fail with
#' the canonical form in the message, so a wrong call in an agent translation
#' becomes repair feedback instead of being silently reinterpreted.
#'
#' The registry is the `.sas2r_registry` list a bundle's `_sas2r_registry.R`
#' defines; in a translated program it is loaded by the bootstrap header. To
#' use these helpers interactively, source that file first.
#'
#' @param libref The libref, as a single string.
#' @param member The dataset member name, as a single string.
#' @param df The data frame to write.
#' @param ... Not used; present so a rejected alias fails with a clear message.
#' @return `lib_read()` returns the dataset with class `sas2r_dataset`;
#'   `lib_write()` returns `df`, invisibly.
#' @family runtime helpers
#' @examples
#' lib <- tempfile("lib"); dir.create(lib)
#' .sas2r_registry <- list(
#'   raw = list(read_path = lib, write_path = lib, engine = "rds", write = "rds")
#' )
#' lib_write(data.frame(USUBJID = c("01", "02"), AVAL = c(1, 2)), "raw", "dm")
#' dm <- lib_read("raw", "dm")
#' dm$usubjid          # case-insensitive column access
#' rm(.sas2r_registry)
#' @export
lib_read <- function(libref, member, ...) {
  # Exactly one call form: lib_read("lib", "member"). The rejected forms --
  # combined "lib.member" strings, single-argument calls that used to default
  # to work, and dataset=/table= aliases -- fail here with the canonical form
  # in the message, so a wrong call in an agent translation becomes repair
  # feedback instead of being silently reinterpreted.
  canonical <- 'lib_read("lib", "member")'
  if (length(list(...)) > 0L) {
    stop("lib_read(): dataset=/table= aliases are not accepted; use ",
         canonical, call. = FALSE)
  }
  if (missing(libref) || !is.character(libref) || length(libref) != 1L ||
      is.na(libref) || !nzchar(libref)) {
    stop("lib_read() takes a libref and a member as two separate strings; use ",
         canonical, call. = FALSE)
  }
  # The combined form lives in the libref position only. The member is left
  # to sas2r_lib_member_path(), whose path-traversal refusal must keep its
  # own classed condition rather than be pre-empted here.
  if (grepl(".", libref, fixed = TRUE)) {
    stop('lib_read() does not accept combined "lib.member" references; use ',
         canonical, call. = FALSE)
  }
  if (missing(member)) {
    stop("lib_read() needs both a libref and a member; use ", canonical,
         ' -- a single-argument call no longer defaults to "work".', call. = FALSE)
  }
  if (!is.character(member) || length(member) != 1L || is.na(member) || !nzchar(member)) {
    stop("lib_read() takes a libref and a member as two separate strings; use ",
         canonical, call. = FALSE)
  }
  reg <- sas2r_lib_entry(libref)
  w_dir <- if (!is.null(reg$write_path)) reg$write_path else reg$path
  r_dir <- if (!is.null(reg$read_path)) reg$read_path else reg$path
  find_file <- function(dir) {
    if (is.null(dir) || is.na(dir) || !nzchar(dir)) return(NULL)
    f <- function(ext) sas2r_lib_member_path(dir, member, ext)
    if (file.exists(f(".rds"))) list(type = "rds", path = f(".rds"))
    else if (file.exists(f(".sas7bdat"))) list(type = "sas7bdat", path = f(".sas7bdat"))
    else if (file.exists(f(".xpt"))) list(type = "xpt", path = f(".xpt"))
    else NULL
  }
  target <- find_file(w_dir)
  if (is.null(target)) target <- find_file(r_dir)
  if (is.null(target)) {
    stop("Dataset not found: ", libref, ".", member, call. = FALSE)
  }
  df <- if (target$type == "rds") readRDS(target$path)
  else if (target$type == "sas7bdat") haven::read_sas(target$path)
  else if (target$type == "xpt") haven::read_xpt(target$path)
  class(df) <- unique(c("sas2r_dataset", class(df)))
  df
}

#' @rdname lib_read
#' @export
lib_write <- function(df, libref, member, ...) {
  # Exactly one call form: lib_write(df, "lib", "member"), data frame first.
  # See lib_read() for why the other forms are rejected rather than absorbed.
  canonical <- 'lib_write(df, "lib", "member")'
  if (length(list(...)) > 0L) {
    stop("lib_write(): dataset=/table= aliases are not accepted; use ",
         canonical, call. = FALSE)
  }
  if (missing(df)) {
    stop("lib_write() takes the data frame first; use ", canonical, call. = FALSE)
  }
  if (is.character(df)) {
    stop("lib_write() takes the data frame first; use ", canonical,
         ' -- the first argument was the string "', df[1], '".', call. = FALSE)
  }
  if (missing(libref) || !is.character(libref) || length(libref) != 1L ||
      is.na(libref) || !nzchar(libref)) {
    stop("lib_write() takes the libref and member as two separate strings after ",
         "the data frame; use ", canonical, call. = FALSE)
  }
  if (grepl(".", libref, fixed = TRUE)) {
    stop('lib_write() does not accept combined "lib.member" references; use ',
         canonical, call. = FALSE)
  }
  if (missing(member)) {
    stop("lib_write() needs both a libref and a member; use ", canonical,
         ' -- a two-argument call no longer defaults to "work".', call. = FALSE)
  }
  if (!is.character(member) || length(member) != 1L || is.na(member) || !nzchar(member)) {
    stop("lib_write() takes the libref and member as two separate strings after ",
         "the data frame; use ", canonical, call. = FALSE)
  }
  reg <- sas2r_lib_entry(libref)
  w_dir <- if (!is.null(reg$write_path)) reg$write_path else reg$path
  if (is.null(w_dir) || is.na(w_dir) || !nzchar(w_dir)) {
    stop("No writable path configured for libref: ", libref, call. = FALSE)
  }
  dir.create(w_dir, showWarnings = FALSE, recursive = TRUE)
  fmt <- if (is.null(reg$write)) "rds" else reg$write
  if (fmt == "rds") saveRDS(df, sas2r_lib_member_path(w_dir, member, ".rds"))
  else if (fmt == "xpt") haven::write_xpt(df, sas2r_lib_member_path(w_dir, member, ".xpt"))
  else stop("Unsupported write format: ", fmt, call. = FALSE)
  invisible(df)
}
