# Runtime helpers: librefs and data access. Part of the runtime every
# translated program carries; see ?sas2r_runtime.

# The libref registry is the `.sas2r_registry` list the bundle's autoexec.R
# defines. The runtime finds it where it was loaded:
# lexically first -- the environment the runtime was sourced into, which is
# how a bundle program and a test harness both work -- then, for the package
# form of the runtime, in active local call frames. Package helpers must not
# fall back to the user's workspace: LIBNAME updates the registry in place.
# A standalone runtime explicitly sourced into a script's top-level environment
# uses that chosen environment, preserving direct Rscript execution.
#' Find the environment holding the runtime library registry
#'
#' Looks first along the runtime's enclosing environments, then active local
#' call frames. Package helpers exclude the global workspace; standalone
#' helpers use the environment they were explicitly sourced into. Load
#' `autoexec.R` into a local environment for interactive use.
#' @return An environment containing `.sas2r_registry`, not the registry list.
#'   Signals `sas2r_no_registry` if no registry is loaded.
#' @keywords internal
sas2r_registry_env <- function() {
  env <- parent.env(environment())
  runtime_env <- env
  while (!identical(env, emptyenv())) {
    if ((!identical(env, globalenv()) || identical(env, runtime_env)) &&
        exists(".sas2r_registry", envir = env, inherits = FALSE)) return(env)
    env <- parent.env(env)
  }
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (!identical(frames[[i]], globalenv()) &&
        exists(".sas2r_registry", envir = frames[[i]], inherits = FALSE)) {
      return(frames[[i]])
    }
  }
  sas2r_libref_stop(
    "sas2r_no_registry",
    "No libref registry is loaded in a local environment: source the bundle's autoexec.R with local = runtime, chdir = TRUE, where runtime <- new.env()"
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
#' local({
#'   .sas2r_registry <- list()
#'   sas2r_libname_assign("raw", tempdir(), engine = "rds")
#'   names(.sas2r_registry)
#'   sas2r_libname_clear("raw")
#'   names(.sas2r_registry)
#' })
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
      write_path <- file.path(root, "libraries", paste0(key, "_", substr(cli::hash_sha256(read_path), 1L, 16L)))
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
#' @keywords internal
sas2r_assignment_path <- function(path, env) {
  root <- get0(".sas2r_execution_root", envir = env, inherits = FALSE,
               ifnotfound = getwd())
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\|~)", path)) path <- file.path(root, path)
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

#' Relative location of a report within the TLF directory
#' @param path A relative logical report path.
#' @return The normalized relative path, including source subdirectories.
#' @keywords internal
sas2r_tlf_relative_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("Report path must be a non-empty relative path", call. = FALSE)
  path <- gsub("\\\\", "/", path)
  path <- sub("^(\\./)+", "", path)
  if (grepl("^(/|[A-Za-z]:|~)", path))
    stop("Use a relative logical report path within the output root", call. = FALSE)
  parts <- character()
  for (part in strsplit(path, "/", fixed = TRUE)[[1L]]) {
    if (part %in% c("", ".")) next
    if (part == "..") {
      if (!length(parts)) stop("Report path escapes the logical output root", call. = FALSE)
      parts <- utils::head(parts, -1L)
    } else parts <- c(parts, part)
  }
  path <- paste(parts, collapse = "/")
  if (!nzchar(path)) stop("Report path must name a file", call. = FALSE)
  path
}

#' Resolve a report destination within the current run
#'
#' Use this helper to locate ODS and other report files.
#' The full relative source path is retained under the run's TLF directory.
#' For example, `outputs/table.pdf` and `table.pdf` have distinct destinations.
#' The parent directory is created. Absolute source paths require an explicit
#' relative logical destination in the translated program.
#'
#' @param path Relative logical report path from the SAS output contract.
#' @param env Calling environment or an explicit runtime environment. Nested
#'   functions use the output root from their enclosing bundle runtime.
#' @return Character scalar destination path. No working-directory fallback
#'   is used when the runtime output root is absent.
#' @export
sas_output_path <- function(path, env = parent.frame()) {
  # Follow the translated function's lexical scope, stopping before the
  # workspace unless the standalone helper was itself sourced there.
  while (!identical(env, emptyenv()) && !identical(env, globalenv()) &&
         !exists(".sas2r_output_root", envir = env, inherits = FALSE)) {
    env <- parent.env(env)
  }
  root <- if (!identical(env, globalenv()) || identical(environment(sas_output_path), env))
    get0(".sas2r_output_root", envir = env, inherits = FALSE)
  if (!is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root))
    stop("A runtime output root is required; source autoexec.R into a local environment", call. = FALSE)
  dest <- file.path(root, "tlf", sas2r_tlf_relative_path(path))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  dest
}

#' @rdname sas2r_libname_assign
#' @export
sas2r_libname_clear <- function(libref) {
  env <- sas2r_registry_env()
  registry <- get(".sas2r_registry", envir = env, inherits = FALSE)
  if (identical(tolower(libref), "_all_")) registry <- list() else registry[[tolower(libref)]] <- NULL
  assign(".sas2r_registry", registry, envir = env)
  invisible(NULL)
}

#' Signal a runtime library error
#' @param cls Character scalar giving the specific error class.
#' @param msg Character scalar error message.
#' @param identifiers Technical names for structured diagnostics, checked against code before agent use.
#' @return Does not return normally. Signals an error with classes `cls`,
#'   `sas2r_libref_error`, `error`, and `condition`, and fields `message`, `call` and `identifiers`.
#' @keywords internal
sas2r_libref_stop <- function(cls, msg, identifiers = character()) {
  stop(structure(
    class = c(cls, "sas2r_libref_error", "error", "condition"),
    list(message = msg, call = NULL, identifiers = identifiers)
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
#' @keywords internal
sas2r_lib_entry <- function(libref) {
  registry <- get(".sas2r_registry", envir = sas2r_registry_env(), inherits = FALSE)
  reg <- registry[[tolower(libref)]]
  if (is.null(reg)) {
    sas2r_libref_stop("sas2r_unknown_libref",
                      paste0("Unknown libref: ", libref), identifiers = libref)
  }
  reg
}

#' Construct a member filename inside a library directory
#' @param dir Character scalar library directory.
#' @param member Character scalar dataset member name, without path separators.
#' @param ext File extension including its dot, for example `".rds"`.
#' @return A character scalar filename. Does not read or create the file.
#'   Invalid member names signal `sas2r_libref_member_error`.
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
#' @keywords internal
sas2r_lib_member_file <- function(reg, member) {
  sas2r_lib_member_path("", member, "")
  write_dir <- if (!is.null(reg$write_path)) reg$write_path else reg$path
  read_dir <- if (!is.null(reg$read_path)) reg$read_path else reg$path
  locations <- list(list(path = write_dir, types = unique(c(reg$write, "rds", "sas7bdat", "xpt"))),
                    list(path = read_dir, types = unique(c(reg$engine, "rds", "sas7bdat", "xpt"))))
  for (location in locations) {
    if (is.null(location$path) || is.na(location$path) || !nzchar(location$path)) next
    files <- list.files(location$path, full.names = TRUE)
    files <- files[!dir.exists(files)]
    for (type in location$types) {
      hits <- files[tolower(basename(files)) == paste0(tolower(member), ".", type)]
      if (length(hits) > 1L) stop("Ambiguous dataset member: ", member, call. = FALSE)
      if (length(hits)) return(list(type = type, path = hits[[1L]]))
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

#' @export
#' @noRd
`$<-.sas2r_dataset` <- function(x, name, value) {
  idx <- match(tolower(name), tolower(names(x)))
  if (!is.na(idx)) name <- names(x)[idx]
  NextMethod()
}

#' @export
#' @noRd
`[[<-.sas2r_dataset` <- function(x, i, value) {
  if (is.character(i) && length(i) == 1L) {
    idx <- match(tolower(i), tolower(names(x)))
    if (!is.na(idx)) i <- names(x)[idx]
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
#' `lib_write()` also takes `formats`, the SAS formats the program gives the
#' dataset's variables, as a `FORMAT` statement does:
#' `lib_write(df, "lib", "member", formats = c(visitdt = "date9."))`. A SAS
#' date is a day number from 1 January 1960 with a format that shows it as a
#' date, and generated R keeps that number. A column given a date format here
#' is stored as an R `Date`, and `lib_read()` hands a later program the SAS day
#' number again, so date arithmetic is unchanged. Only a plain numeric column
#' whose values convert back exactly is stored that way; a format that is not
#' a date format changes nothing. Datetime and time formats are not covered.
#'
#' The registry is the `.sas2r_registry` list a bundle's `autoexec.R`
#' defines; in a translated program it is loaded by the bootstrap header. To
#' use these helpers interactively, create `runtime <- new.env()` and source
#' that file with `local = runtime, chdir = TRUE`, then call
#' `runtime$lib_read()` or `runtime$lib_write()`.
#'
#' @param libref The libref, as a single string.
#' @param member The dataset member name, as a single string.
#' @param df The data frame to write.
#' @param formats A named character vector, column name to SAS format, written
#'   as it is in SAS (`"yymmdd10."`). `NULL` when the program gives no format.
#' @param ... Not used; present so a rejected alias fails with a clear message.
#' @return `lib_read()` returns the dataset with class `sas2r_dataset`;
#'   `lib_write()` returns `df`, invisibly.
#' @family runtime helpers
#' @examples
#' local({
#'   lib <- tempfile("lib"); dir.create(lib)
#'   .sas2r_registry <- list(
#'     raw = list(read_path = lib, write_path = lib, engine = "rds", write = "rds")
#'   )
#'   lib_write(data.frame(USUBJID = c("01", "02"), AVAL = c(1, 2)), "raw", "dm")
#'   dm <- lib_read("raw", "dm")
#'   dm$usubjid          # case-insensitive column access
#'   unlink(lib, recursive = TRUE)
#' })
#' @export
lib_read <- function(libref, member, ...) {
  # Exactly one call form: lib_read("lib", "member"). The rejected forms --
  # combined "lib.member" strings, single-argument calls that used to default
  # to work, and dataset=/table= aliases -- fail here with the canonical form
  # in the message, so a wrong call in an agent translation becomes repair
  # feedback instead of being silently reinterpreted.
  canonical <- 'lib_read("lib", "member")'
  if (length(list(...)) > 0L) {
    sas2r_libref_stop("sas2r_lib_read_arguments", paste0("lib_read(): dataset=/table= aliases are not accepted; use ",
         canonical))
  }
  if (missing(libref) || !is.character(libref) || length(libref) != 1L ||
      is.na(libref) || !nzchar(libref)) {
    sas2r_libref_stop("sas2r_lib_read_arguments", paste0("lib_read() takes a libref and a member as two separate strings; use ",
         canonical))
  }
  # The combined form lives in the libref position only. The member is left
  # to sas2r_lib_member_path(), whose path-traversal refusal must keep its
  # own classed condition rather than be pre-empted here.
  if (grepl(".", libref, fixed = TRUE)) {
    sas2r_libref_stop("sas2r_lib_read_arguments", paste0('lib_read() does not accept combined "lib.member" references; use ',
         canonical))
  }
  if (missing(member)) {
    sas2r_libref_stop("sas2r_lib_read_arguments", paste0("lib_read() needs both a libref and a member; use ", canonical,
         ' -- a single-argument call no longer defaults to "work".'))
  }
  if (!is.character(member) || length(member) != 1L || is.na(member) || !nzchar(member)) {
    sas2r_libref_stop("sas2r_lib_read_arguments", paste0("lib_read() takes a libref and a member as two separate strings; use ",
         canonical))
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
    sas2r_libref_stop("sas2r_dataset_not_found",
      paste0("Dataset not found: ", libref, ".", member,
        "\nSearched: ", paste(searched, collapse = ", "), "\nExecution root: ", root),
      identifiers = paste(libref, member, sep = "."))
  }
  df <- if (target$type == "rds") readRDS(target$path)
  else if (target$type == "sas7bdat") haven::read_sas(target$path)
  else if (target$type == "xpt") haven::read_xpt(target$path)
  # SAS keeps a date as a number of days from 1960-01-01 and a format that says
  # to show it as a date. A column a program gave a date format when it wrote
  # the dataset is stored as an R Date (see lib_write()) and handed back here
  # as the SAS number, so
  # date arithmetic in a later program is the SAS arithmetic. The format
  # attribute goes with the class: R arithmetic copies attributes, and a
  # duration computed from two dates must not look like a date.
  dates <- get0(".sas2r_output_dates", envir = sas2r_registry_env(), inherits = TRUE)
  dates <- if (is.list(dates)) dates[[tolower(paste(libref, member, sep = "."))]]
  if (is.character(dates) && !is.null(names(dates))) {
    origin <- -as.numeric(as.Date("1960-01-01"))
    for (name in names(dates)) {
      at <- which(tolower(names(df)) == tolower(name))
      if (length(at) != 1L || !inherits(df[[at]], "Date")) next
      x <- unclass(df[[at]])
      storage.mode(x) <- "double"
      present <- !is.na(x)
      x[present] <- x[present] + origin
      attr(x, "format.sas") <- NULL
      df[[at]] <- x
    }
  }
  class(df) <- unique(c("sas2r_dataset", class(df)))
  df
}

#' @rdname lib_read
#' @export
lib_write <- function(df, libref, member, formats = NULL, ...) {
  # Exactly one call form: lib_write(df, "lib", "member"), data frame first,
  # with the optional formats = c(column = "format.") after it.
  # See lib_read() for why the other forms are rejected rather than absorbed.
  canonical <- 'lib_write(df, "lib", "member")'
  if (length(list(...)) > 0L) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write(): dataset=/table= aliases are not accepted; use ",
         canonical))
  }
  if (!is.null(formats) && (!is.character(formats) || is.null(names(formats)) || anyNA(formats) ||
      anyNA(names(formats)) || !all(nzchar(names(formats))) || anyDuplicated(tolower(names(formats))) > 0L)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write(): formats names each column once with its SAS format; use ",
         canonical, ' with formats = c(column = "date9.")'))
  }
  if (missing(df)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write() takes the data frame first; use ", canonical))
  }
  if (is.character(df)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write() takes the data frame first; use ", canonical,
         ' -- the first argument was the string "', df[1], '".'))
  }
  if (missing(libref) || !is.character(libref) || length(libref) != 1L ||
      is.na(libref) || !nzchar(libref)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write() takes the libref and member as two separate strings after ",
         "the data frame; use ", canonical))
  }
  if (grepl(".", libref, fixed = TRUE)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0('lib_write() does not accept combined "lib.member" references; use ',
         canonical))
  }
  if (missing(member)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write() needs both a libref and a member; use ", canonical,
         ' -- a two-argument call no longer defaults to "work".'))
  }
  if (!is.character(member) || length(member) != 1L || is.na(member) || !nzchar(member)) {
    sas2r_libref_stop("sas2r_lib_write_arguments", paste0("lib_write() takes the libref and member as two separate strings after ",
         "the data frame; use ", canonical))
  }
  reg <- sas2r_lib_entry(libref)
  w_dir <- if (!is.null(reg$write_path)) reg$write_path else reg$path
  if (is.null(w_dir) || is.na(w_dir) || !nzchar(w_dir)) {
    sas2r_libref_stop("sas2r_library_unavailable", paste0("No writable path configured for libref: ", libref), identifiers = libref)
  }
  dir.create(w_dir, showWarnings = FALSE, recursive = TRUE)
  fmt <- if (is.null(reg$write)) "rds" else reg$write
  df <- as.data.frame(df)
  if (!fmt %in% c("rds", "xpt")) sas2r_libref_stop("sas2r_write_format", paste0("Unsupported write format: ", fmt))
  # A SAS date is a day number with a format that shows it as a date. A column
  # the program gives a date format, in `formats`, is stored as an R Date. Only
  # what is stored changes: the program keeps, and is returned, its own data,
  # and lib_read() gives a later program the SAS day number again.
  # A column is converted only when it is a plain number and every value comes
  # back exactly, so reading returns the numbers written; one value that would
  # not, such as some fractional day counts, leaves the whole column as it is.
  # A missing value is never computed on, which keeps a special missing
  # value's tag. A column that is already a Date is left.
  # The date formats are the ones haven reads back as a date.
  stored <- df
  dates <- character()
  if (length(formats)) {
    declared <- toupper(sub("[.][0-9]*$", "", trimws(formats)))
    is_date <- grepl("^(DATE|WEEKDATE|(YYMMDD|MMDDYY|DDMMYY)[BCDNPS]?|E8601DA|B8601DA|IS8601DA)[0-9]*$", declared)
    dates <- declared[is_date]
    names(dates) <- tolower(names(formats)[is_date])
  }
  as_dates <- character()
  origin <- -as.numeric(as.Date("1960-01-01"))
  for (name in names(dates)) {
    at <- which(tolower(names(stored)) == name)
    if (length(at) != 1L) next
    x <- stored[[at]]
    # A declared column that is a Date already is stored as it is, and is
    # read back as a day number like any other declared date.
    if (inherits(x, "Date")) { as_dates[[name]] <- dates[[name]]; next }
    if (!is.numeric(x) || !is.null(oldClass(x))) next
    present <- !is.na(x)
    shifted <- as.double(x[present]) - origin
    if (!identical(shifted + origin, as.double(x[present]))) next
    storage.mode(x) <- "double"
    x[present] <- shifted
    if (is.null(attr(x, "format.sas", exact = TRUE))) attr(x, "format.sas") <- dates[[name]]
    class(x) <- "Date"
    stored[[at]] <- x
    as_dates[[name]] <- dates[[name]]
  }
  target <- sas2r_lib_member_path(w_dir, member, paste0(".", fmt))
  pending <- tempfile(".sas2r-write-", tmpdir = w_dir, fileext = paste0(".", fmt))
  on.exit(unlink(pending), add = TRUE)
  # haven names an XPT member after its file, and the temporary file's name is
  # not a SAS name: name the member after the file it becomes.
  if (fmt == "rds") saveRDS(stored, pending) else
    haven::write_xpt(stored, pending, name = sub("[.]xpt$", "", basename(target), ignore.case = TRUE))
  if (!file.rename(pending, target)) sas2r_libref_stop("sas2r_library_unavailable", paste0("Could not replace dataset member: ", target))
  # lib_read() returns the day number for the columns stored as dates here. A
  # bundle's `_sas2r_formats.R` lists the same columns for a program run in
  # another session; this keeps the list right within one, also for a member
  # written again without the format.
  env <- sas2r_registry_env()
  known <- get0(".sas2r_output_dates", envir = env, inherits = TRUE)
  key <- tolower(paste(libref, member, sep = "."))
  if (length(as_dates) || (is.list(known) && !is.null(known[[key]]))) {
    if (!is.list(known)) known <- list()
    known[[key]] <- if (length(as_dates)) as_dates else NULL
    assign(".sas2r_output_dates", known, envir = env)
  }
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
#' local({
#'   folder <- tempfile("work")
#'   .sas2r_registry <- list(work = list(path = folder, write = "rds"))
#'   lib_write(data.frame(x = 1), "work", "scratch")
#'   lib_delete("work", "scratch")
#'   unlink(folder, recursive = TRUE)
#' })
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

#' Read variables no input supplied as missing, as a SAS DATA step does
#'
#' A SAS DATA step may read a variable that none of its inputs carries and
#' that it never assigns. SAS does not stop: it creates the variable, leaves
#' it missing in every row, and writes `NOTE: Variable X is uninitialized.` to
#' the log. `sas_uninitialized()` does the same for a data frame. Each named
#' variable that is absent is added as a missing column, and the note is
#' printed and recorded, so a migration run lists it in its report. A
#' variable that is present, in any letter case, is left as it is and gives
#' no note.
#'
#' Use it where a step reads a variable in an expression (an assignment, an
#' `IF` condition, a function or macro argument) and its inputs may not carry
#' that variable. Apply it to the frame the step computes on, after its
#' inputs are read and merged, never to one input of a merge. Write no
#' separate test for the column, no fallback value and no `stop()` for that
#' case. Name only variables the SAS source reads.
#'
#' SAS itself stops, and so must the R, when the absent variable is named in
#' `BY`, in `WHERE`, in `KEEP=`, `DROP=` or `RENAME=` on an input dataset, or
#' in the variable list of a procedure. Do not use this helper there.
#'
#' @param df A data frame.
#' @param vars Character vector naming the variables the step reads.
#' @param type `"numeric"`, the SAS type of a variable first met in an
#'   expression, or `"character"` for a variable the step declares character
#'   with `LENGTH`, `ATTRIB` or a `$` format.
#' @return `df`, with a missing column for each absent variable, named as
#'   given in `vars`.
#' @seealso [sas2r_runtime]
#' @examples
#' dm <- data.frame(usubjid = c("01", "02"))
#' sas_uninitialized(dm, c("USUBJID", "randdtc"))
#' @export
sas_uninitialized <- function(df, vars, type = "numeric") {
  if (!is.data.frame(df)) stop("sas_uninitialized(): df must be a data frame", call. = FALSE)
  if (!is.character(vars) || anyNA(vars) || !all(grepl("^[A-Za-z_][A-Za-z0-9_]*$", vars))) {
    stop("sas_uninitialized(): vars names each variable as a string", call. = FALSE)
  }
  if (!is.character(type) || length(type) != 1L || !type %in% c("numeric", "character")) {
    stop('sas_uninitialized(): type is "numeric" or "character"', call. = FALSE)
  }
  absent <- vars[!tolower(vars) %in% tolower(names(df))]
  absent <- absent[!duplicated(tolower(absent))]
  if (!length(absent)) return(df)
  value <- if (identical(type, "character")) NA_character_ else NA_real_
  for (name in absent) df[[name]] <- rep(value, nrow(df))
  # A run reads the notes of each program from here; a script that loaded no
  # registry still gets the columns and the printed note.
  env <- tryCatch(sas2r_registry_env(), error = function(e) NULL)
  if (!is.null(env)) {
    notes <- get0(".sas2r_log_notes", envir = env, inherits = FALSE)
    if (!is.list(notes)) notes <- list()
    for (name in absent) {
      notes[[length(notes) + 1L]] <- list(note = "uninitialized_variable", variable = name,
        type = type, rows = nrow(df))
    }
    assign(".sas2r_log_notes", notes, envir = env)
  }
  for (name in absent) {
    message(structure(class = c("sas2r_uninitialized_variable", "message", "condition"),
      list(message = paste0("NOTE: Variable ", name, " is uninitialized.\n"), call = NULL, variable = name)))
  }
  df
}
