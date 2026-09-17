# Runtime helpers: librefs and data access. Part of the runtime every
# translated program carries; see ?sas2r_runtime.

# The libref registry is the `.sas2r_registry` list the bundle's autoexec.R
# defines. The runtime finds it where it was loaded:
# lexically first -- the environment the runtime was sourced into, which is
# how a bundle program and a test harness both work -- then, for the package
# form of the runtime, out through the global environment, and finally in any
# active frame, so a registry defined inside a function is honoured too.
#' Find the environment holding the runtime library registry
#'
#' Looks first along the runtime's enclosing environments, then active call
#' frames. Load `autoexec.R` before calling data-access helpers.
#' @return An environment containing `.sas2r_registry`, not the registry list.
#'   Signals `sas2r_no_registry` if no registry is loaded.
#' @examples
#' registry_env <- getFromNamespace("sas2r_registry_env", "sas2r")
#' local({
#'   .sas2r_registry <- list()
#'   is.environment(registry_env())
#' })
#' @keywords internal
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
    "No libref registry is loaded: source the bundle's autoexec.R first (with chdir = TRUE)"
  )
}

#' Resolve a bundle's libref registry against its folder
#'
#' `autoexec.R` lists each libref's directories the way a person writes them:
#' an absolute path is used as is, and a relative path means inside the bundle
#' folder. This turns the relative ones into absolute paths against `root`,
#' creates every write directory, and returns the registry ready for
#' [lib_read()] and [lib_write()]. `autoexec.R` calls it once, after the
#' helpers are loaded; call it yourself only when building a registry by hand.
#'
#' A `<FILL>` placeholder left in a path -- the commented entries `autoexec.R`
#' carries for a library nothing binds -- is refused with the libref named,
#' rather than becoming a directory called `<FILL...>`.
#'
#' @param registry The `.sas2r_registry` list as written in `autoexec.R`.
#' @param root The bundle folder, as an absolute path.
#' @return The registry, with every `read_path` and `write_path` absolute.
#' @family runtime helpers
#' @examples
#' root <- tempfile("bundle"); dir.create(root)
#' reg <- sas2r_resolve_registry(
#'   list(work = list(read_path = "work", write_path = "work",
#'                    engine = "rds", write = "rds")),
#'   root)
#' reg$work$write_path      # <root>/work, and the directory now exists
#' @export
sas2r_resolve_registry <- function(registry, root) {
  if (!is.list(registry)) {
    sas2r_libref_stop("sas2r_registry_error", "registry must be a list of libref entries")
  }
  if (!is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)) {
    sas2r_libref_stop("sas2r_registry_error", "root must be one directory path")
  }
  absolute <- function(p) grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\|~)", p)
  for (libref in names(registry)) {
    entry <- registry[[libref]]
    for (field in intersect(c("read_path", "write_path", "path"), names(entry))) {
      p <- entry[[field]]
      if (!is.character(p) || length(p) != 1L || is.na(p) || !nzchar(p)) next
      if (startsWith(p, "<FILL")) {
        sas2r_libref_stop(
          "sas2r_registry_fill",
          paste0("autoexec.R: libref ", libref, " still has a <FILL> placeholder for ",
                 field, "; set the path"))
      }
      entry[[field]] <- if (absolute(p)) path.expand(p) else file.path(root, p)
    }
    w <- if (!is.null(entry$write_path)) entry$write_path else entry$path
    if (is.character(w) && length(w) == 1L && !is.na(w) && nzchar(w)) {
      dir.create(w, showWarnings = FALSE, recursive = TRUE)
    }
    registry[[libref]] <- entry
  }
  registry
}

#' Bind or clear a libref at its point of use
#'
#' The registry a bundle starts with is only a seed -- the libraries the
#' configuration declares, plus `work`. Every `LIBNAME` the SAS program
#' executes changes the registry where that statement stood, so a libref that
#' is rebound or cleared part-way through behaves at run time the way it does
#' in SAS. `sas2r_libname_assign()` is `LIBNAME libref '<path>'`;
#' `sas2r_libname_clear()` is `LIBNAME libref CLEAR`.
#' Relative assignment paths use `.sas2r_execution_root` from `autoexec.R`,
#' initially the source project directory. Registry seed paths still use the
#' bundle folder. Reassigning a known physical library preserves its separate
#' write directory. Newly assigned libraries write under the execution output
#' `libraries/` folder unless `write_path` is explicitly supplied.
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
  read_path <- sas2r_assignment_path(read_path, env)
  bindings <- get0(".sas2r_library_bindings", envir = env, inherits = FALSE)
  if (missing(write_path) && !is.null(bindings)) {
    # A repeated LIBNAME (including an alias or a bind after CLEAR) names the
    # same physical data. Keep the generated members in that library visible.
    paths <- vapply(bindings, function(entry) {
      sas2r_assignment_path(entry$read_path, env)
    }, character(1))
    idx <- match(read_path, paths)
    if (!is.na(idx)) {
      write_path <- bindings[[idx]]$write_path
    } else {
      root <- get(".sas2r_output_root", envir = env, inherits = FALSE)
      write_path <- file.path(root, "libraries", paste0(key, "_", length(bindings) + 1L))
    }
  } else {
    write_path <- sas2r_assignment_path(write_path, env)
  }
  registry[[key]] <- list(
    read_path = read_path,
    write_path = write_path,
    engine = engine,
    write = write
  )
  assign(".sas2r_registry", registry, envir = env)
  if (!is.null(bindings)) {
    existing <- which(vapply(bindings, function(entry) {
      identical(sas2r_assignment_path(entry$read_path, env), read_path)
    }, logical(1)))
    idx <- if (length(existing)) existing[1L] else length(bindings) + 1L
    bindings[[idx]] <- registry[[key]]
    assign(".sas2r_library_bindings", bindings, envir = env)
  }
  invisible(registry[[key]])
}

#' Resolve a translated LIBNAME path against the execution root
#'
#' Registry seed paths are resolved separately by [sas2r_resolve_registry()].
#' @param path Character scalar path, absolute or relative.
#' @param env Runtime environment containing `.sas2r_execution_root`.
#' @return A normalized character scalar path. Relative paths use the execution
#'   root, or the working directory when no root is set. The target need not exist.
#' @examples
#' resolve <- getFromNamespace("sas2r_assignment_path", "sas2r")
#' env <- new.env()
#' env$.sas2r_execution_root <- tempdir()
#' resolve("inputs", env)
#' @keywords internal
sas2r_assignment_path <- function(path, env) {
  root <- get0(".sas2r_execution_root", envir = env, inherits = FALSE,
               ifnotfound = getwd())
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\|~)", path)) path <- file.path(root, path)
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
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

#' Signal a runtime library error
#' @param cls Character scalar giving the specific error class.
#' @param msg Character scalar error message.
#' @return Does not return normally. Signals an error with classes `cls`,
#'   `sas2r_libref_error`, `error`, and `condition`, and fields `message` and `call`.
#' @examples
#' fail <- getFromNamespace("sas2r_libref_stop", "sas2r")
#' tryCatch(fail("example_library_error", "Example failure"),
#'          sas2r_libref_error = function(e) conditionMessage(e))
#' @keywords internal
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
#' Read one library entry from the loaded registry
#' @param libref Character scalar library name, matched case-insensitively.
#' @return The registry entry as a list, normally with `read_path`, `write_path`,
#'   `engine`, and `write` fields. Signals `sas2r_unknown_libref` when absent;
#'   does not read a dataset or return a data frame.
#' @examples
#' entry <- getFromNamespace("sas2r_lib_entry", "sas2r")
#' local({
#'   .sas2r_registry <- list(work = list(read_path = tempdir(),
#'     write_path = tempdir(), engine = "rds", write = "rds"))
#'   entry("WORK")$write_path
#' })
#' @keywords internal
sas2r_lib_entry <- function(libref) {
  registry <- get(".sas2r_registry", envir = sas2r_registry_env(), inherits = FALSE)
  reg <- registry[[tolower(libref)]]
  if (is.null(reg)) {
    sas2r_libref_stop("sas2r_unknown_libref",
                      paste0("Unknown libref: ", libref))
  }
  reg
}

#' Construct a member filename inside a library directory
#' @param dir Character scalar library directory.
#' @param member Character scalar dataset member name, without path separators.
#' @param ext File extension including its dot, for example `".rds"`.
#' @return A character scalar filename. Does not read or create the file.
#'   Invalid member names signal `sas2r_libref_member_error`.
#' @examples
#' member_path <- getFromNamespace("sas2r_lib_member_path", "sas2r")
#' member_path(tempdir(), "measurements", ".rds")
#' @keywords internal
sas2r_lib_member_path <- function(dir, member, ext) {
  if (!is.character(member) || length(member) != 1L || is.na(member) ||
      !nzchar(member) || grepl("[/\\\\]", member) ||
      member %in% c(".", "..")) {
    sas2r_libref_stop("sas2r_libref_member_error",
                      paste0("Unsafe dataset member name: ", member))
  }
  file.path(dir, paste0(member, ext))
}

#' Find a dataset member without reading its contents
#' @param reg A resolved library registry entry.
#' @param member A dataset member name, without a path or extension.
#' @return A list containing `type` and `path`, or `NULL` for an absent member.
#'   The write directory precedes the read directory; formats are tried in
#'   RDS, SAS7BDAT, XPT order. Directories are not dataset member files.
#' @examples
#' find_member <- getFromNamespace("sas2r_lib_member_file", "sas2r")
#' find_member(list(path = tempdir()), "not_created")
#' @keywords internal
sas2r_lib_member_file <- function(reg, member) {
  sas2r_lib_member_path("", member, "")
  dirs <- unique(c(if (!is.null(reg$write_path)) reg$write_path else reg$path,
                   if (!is.null(reg$read_path)) reg$read_path else reg$path))
  for (dir in dirs[!is.na(dirs) & nzchar(dirs)]) {
    for (type in c("rds", "sas7bdat", "xpt")) {
      path <- sas2r_lib_member_path(dir, member, paste0(".", type))
      if (file.exists(path) && !dir.exists(path)) return(list(type = type, path = path))
    }
  }
  NULL
}

#' Test whether a library dataset member exists
#'
#' Uses the same registry and write/read search order as [lib_read()], without
#' reading or parsing data. A present unreadable or zero-row dataset exists.
#' Invalid arguments and an unknown library remain errors, not `FALSE`.
#' Supports ordinary RDS, SAS7BDAT and XPT member files, not SAS views or
#' arbitrary external paths. Existence does not establish readability.
#' @param libref A single library name, without a member or path.
#' @param member A single dataset member name, without a path or extension.
#' @return A single logical value.
#' @family runtime helpers
#' @examples
#' local({
#'   .sas2r_registry <- list(work = list(path = tempdir()))
#'   lib_exists("work", "not_created")
#' })
#' @export
lib_exists <- function(libref, member) {
  if (missing(libref) || !is.character(libref) || length(libref) != 1L ||
      is.na(libref) || !nzchar(libref) || grepl(".", libref, fixed = TRUE) ||
      missing(member) || !is.character(member) || length(member) != 1L ||
      is.na(member) || !nzchar(member)) {
    stop('Use lib_exists("lib", "member") with two separate strings.', call. = FALSE)
  }
  !is.null(sas2r_lib_member_file(sas2r_lib_entry(libref), member))
}

#' List ordinary dataset members in a configured library
#'
#' Lists names without reading dataset rows. Uses the same member resolver and
#' write/read precedence as [lib_exists()] and [lib_read()]. Duplicate storage
#' formats yield one name. Names and extensions must be resolvable on the local
#' filesystem; case is preserved. Unsupported views, name literals and ranges
#' are not expanded. An unknown library or inaccessible configured directory is
#' an error, distinct from an empty library. A not-yet-created write directory
#' is empty; a missing separate read directory remains a configuration error.
#'
#' Listing does not authorize deletion of protected inputs: [lib_delete()]
#' retains its separate-input restriction. This is a runtime helper, not an
#' agent tool or permission to send library contents to a model.
#' @param libref A single registered library name, without a member or path.
#' @return A sorted character vector of ordinary resolvable member names.
#' @family runtime helpers
#' @examples
#' local({
#'   folder <- tempfile()
#'   dir.create(folder)
#'   .sas2r_registry <- list(work = list(path = folder))
#'   lib_write(data.frame(id = 1), "work", "scratch")
#'   members <- lib_members("work")
#'   lib_delete("work", members)
#'   unlink(folder, recursive = TRUE)
#' })
#' @export
lib_members <- function(libref) {
  if (missing(libref) || !is.character(libref) || length(libref) != 1L ||
      is.na(libref) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", libref)) {
    stop('Use lib_members("lib") with one registered library name.', call. = FALSE)
  }
  reg <- sas2r_lib_entry(libref)
  write_dir <- if (!is.null(reg$write_path)) reg$write_path else reg$path
  read_dir <- if (!is.null(reg$read_path)) reg$read_path else reg$path
  dirs <- unique(c(write_dir, read_dir))
  if (!length(dirs) || anyNA(dirs) || any(!nzchar(dirs))) {
    stop("No directory configured for libref: ", libref, call. = FALSE)
  }
  names <- character()
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      if (identical(dir, write_dir) && !file.exists(dir)) next
      stop("Library directory is unavailable: ", dir, call. = FALSE)
    }
    if (file.access(dir, 4L) != 0L) stop("Library directory is unreadable: ", dir, call. = FALSE)
    files <- list.files(dir, pattern = "\\.(rds|sas7bdat|xpt)$", full.names = TRUE, ignore.case = TRUE)
    names <- c(names, sub("\\.[^.]+$", "", basename(files[!dir.exists(files)])))
  }
  names <- sort(unique(names[grepl("^[A-Za-z_][A-Za-z0-9_]*$", names)]), method = "radix")
  names[vapply(names, function(member) !is.null(sas2r_lib_member_file(reg, member)), logical(1))]
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
#' The registry is the `.sas2r_registry` list a bundle's `autoexec.R`
#' defines; in a translated program it is loaded by the bootstrap header. To
#' use these helpers interactively, source that file first, with
#' `chdir = TRUE`.
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
  target <- sas2r_lib_member_file(reg, member)
  if (is.null(target)) {
    dirs <- unique(c(w_dir, r_dir))
    dirs <- dirs[!is.na(dirs) & nzchar(dirs)]
    searched <- unlist(lapply(dirs, function(dir) {
      vapply(c(".rds", ".sas7bdat", ".xpt"), function(ext) {
        sas2r_lib_member_path(dir, member, ext)
      }, character(1))
    }))
    env <- sas2r_registry_env()
    root <- get0(".sas2r_execution_root", envir = env, inherits = FALSE,
                 ifnotfound = getwd())
    stop("Dataset not found: ", libref, ".", member,
         "\nSearched: ", paste(searched, collapse = ", "),
         "\nExecution root: ", root, call. = FALSE)
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

#' Delete explicitly named datasets from a writable library
#'
#' Removes the stored dataset, rather than just an R object. Missing members
#' produce a message and do not stop deletion of other members. Names are
#' matched case-insensitively across rds, xpt and sas7bdat files.
#'
#' Only an explicit vector of ordinary SAS member names is supported. Expand
#' a known list into names before calling; prefix lists, ranges and `_ALL_`
#' are not interpreted. Dynamic selection that cannot be resolved must remain
#' an explicit unsupported operation, never a silent no-op.
#'
#' This helper deletes from the configured write directory. If a requested
#' member also exists in a different read directory, deletion is unsupported:
#' removing the written copy would expose that original again to [lib_read()].
#' The whole request is checked before any file is removed, and input library
#' files in a separate read directory are never deleted.
#'
#' @param libref A configured library name, such as `"work"`.
#' @param members Character vector of explicit dataset names, without librefs
#'   or file extensions. An empty vector does nothing.
#' @return The names of removed members, invisibly.
#' @family runtime helpers
#' @examples
#' .sas2r_registry <- list(work = list(path = tempfile("work"), write = "rds"))
#' lib_write(data.frame(x = 1), "work", "scratch")
#' lib_delete("work", "scratch")
#' rm(.sas2r_registry)
#' @export
lib_delete <- function(libref, members) {
  if (!is.character(members) || anyNA(members) ||
      any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", members)) ||
      any(toupper(members) == "_ALL_")) {
    sas2r_libref_stop("sas2r_unsupported_dataset_delete",
      "lib_delete() supports explicit member names only; expand dataset lists before calling")
  }
  reg <- sas2r_lib_entry(libref)
  write_dir <- if (!is.null(reg$write_path)) reg$write_path else reg$path
  read_dir <- if (!is.null(reg$read_path)) reg$read_path else reg$path
  if (is.null(write_dir) || !nzchar(write_dir)) {
    sas2r_libref_stop("sas2r_unsupported_dataset_delete",
      paste0("No writable path configured for libref: ", libref))
  }
  files_in <- function(dir) {
    if (is.null(dir) || !dir.exists(dir)) return(character())
    files <- list.files(dir, pattern = "\\.(rds|xpt|sas7bdat)$", full.names = TRUE,
                        ignore.case = TRUE)
    files[!dir.exists(files)]
  }
  stem <- function(files) tolower(sub("\\.[^.]+$", "", basename(files)))
  members <- unique(tolower(members))
  separate_input <- !is.null(read_dir) &&
    !identical(normalizePath(read_dir, winslash = "/", mustWork = FALSE),
               normalizePath(write_dir, winslash = "/", mustWork = FALSE))
  if (separate_input && any(members %in% stem(files_in(read_dir)))) {
    sas2r_libref_stop("sas2r_unsupported_dataset_delete",
      paste0("Cannot delete ", libref,
        " members backed by a separate input directory; input files were preserved"))
  }
  files <- files_in(write_dir)
  targets <- files[stem(files) %in% members]
  absent <- setdiff(members, stem(targets))
  for (member in absent) message("Dataset not found: ", libref, ".", member)
  if (length(targets) && !all(file.remove(targets))) {
    sas2r_libref_stop("sas2r_dataset_delete_failed",
      paste0("Could not delete all requested datasets in ", write_dir))
  }
  invisible(unique(stem(targets)))
}
