#' The fixed-name files every staged bundle carries, in bootstrap order
#'
#' Written by [write_registry()], [write_helpers()], and [write_formats()], and
#' sourced in this order by the bootstrap header [module_bootstrap()] emits.
#' Their names are fixed, so they are not derived from any source file and
#' cannot be renamed out of the way of one: a scanned source that happens to
#' carry one of these names is guarded against in
#' [guard_staged_paths_against_sources()] before anything is written.
#'
#' @noRd
SAS2R_BUNDLE_FILES <- c("_sas2r_registry.R", "sas2r-helpers.R",
                        "_sas2r_formats.R")

#' Emit runtime helpers script
#'
#' Copies the self-contained `sas2r-helpers.R` template to the specified output directory.
#'
#' @param out_dir Output directory path.
#' @return Invisible destination path.
#' @noRd
write_helpers <- function(out_dir) {
  src <- system.file("templates", "sas2r-helpers.R", package = "sas2r")
  dest <- file.path(out_dir, "sas2r-helpers.R")
  file.copy(src, dest, overwrite = TRUE)
  invisible(dest)
}

#' Spell one libref as an R list-element name
#'
#' A libref is `[A-Za-z_]\w*` lower-cased, so it is always syntactic and the
#' quoted branch is a guard rather than a case reached today.
#'
#' @param libref One libref.
#' @return The name as it appears in generated R.
#' @noRd
libref_list_key <- function(libref) {
  if (identical(make.names(libref), libref)) libref else deparse(libref)
}

#' Emit one `.sas2r_registry` entry
#'
#' Every value is [deparse()]d rather than pasted, so an arbitrary directory
#' name cannot become code. See [confine_libref_path()].
#'
#' @param libref,path,engine,write The entry's four fields.
#' @param comma Whether a trailing comma follows.
#' @return One line of R code.
#' @noRd
libref_registry_entry <- function(libref, read_path, write_path, engine, write, comma = TRUE) {
  sprintf("  %s = list(read_path = %s, write_path = %s, engine = %s, write = %s)%s",
          libref_list_key(libref), deparse(read_path), deparse(write_path),
          deparse(engine), deparse(write), if (comma) "," else "")
}

#' Make a `LIBNAME`'s own path expression safe to put in generated R
#'
#' The expression is SAS source text, so it is arbitrary. It reaches generated
#' R only on the routes where sas2r could *not* use it -- a marker comment and
#' a commented registry entry -- and it is never resolved or opened, so
#' [confine_libref_path()] is the wrong tool: it would refuse the very cases
#' this exists to record, and canonicalize a path that names no local
#' directory. What is needed instead is that it cannot leave the line it is on:
#' a newline in a comment would put whatever follows it into the module as
#' code. Control characters are therefore folded to spaces, and the value is
#' still [deparse()]d wherever it lands inside a string literal.
#'
#' @param expr One path expression, possibly `NA`.
#' @return The expression with control characters folded out, or `NA`.
#' @noRd
emittable_path_expression <- function(expr) {
  if (length(expr) != 1L || is.na(expr) || !nzchar(expr)) return(NA_character_)
  gsub("[[:cntrl:]]+", " ", expr)
}

#' Spell the path a `LIBNAME` named, for a marker that reports no binding
#'
#' @param expr One path expression, possibly `NA`.
#' @return `" path='...'"`, or `""` when the statement named none.
#' @noRd
source_path_marker <- function(expr) {
  value <- emittable_path_expression(expr)
  if (is.na(value)) "" else sprintf(" path='%s'", value)
}

#' Emit the commented registry entry for a libref nothing binds
#'
#' Carries the path the `LIBNAME` named when there is one, so a bundle
#' translated away from the study drive still records what the SAS asked for,
#' and the `<FILL>` guidance beside it either way -- the path is what was
#' wanted, and it is still a path sas2r could not use.
#'
#' @param libref One libref from [effective_librefs()]`$undeclared`.
#' @param bindings The projection's `bindings` tibble.
#' @return A character vector of commented lines.
#' @noRd
undeclared_registry_entry <- function(libref, bindings) {
  # Only the rows that made the libref undeclared. A libref bound at the top of
  # a program and cleared half-way down has a perfectly good path on its first
  # row, and offering that path as the one to fill in would undo the CLEAR the
  # program wrote on purpose. The rows below the clear name no path, and that
  # is the honest answer for the reads below it.
  rows <- bindings[!is.na(bindings$libref) & bindings$libref == libref &
                     !is.na(bindings$status) &
                     bindings$status %in% c("unbound", "conditionally_bound"), ,
                   drop = FALSE]
  reasons <- if (!nrow(rows)) character() else
    rows$fallback_reason[!is.na(rows$fallback_reason)]
  reason <- if (length(reasons)) reasons[1] else "no_source_binding"
  exprs <- if (!nrow(rows)) character() else
    vapply(rows$source_path_expression, emittable_path_expression,
           character(1), USE.NAMES = FALSE)
  exprs <- exprs[!is.na(exprs)]
  if (!length(exprs)) {
    return(c(
      sprintf("# %s: <FILL: ask your SAS admin> -- nothing in this project binds it (%s).",
              libref, reason),
      paste0("#", libref_registry_entry(libref, "<FILL: ask your SAS admin>",
                                        "<FILL: ask your SAS admin>",
                                        "sas7bdat", "rds"))))
  }
  c(sprintf("# %s: <FILL> -- the path below is the one the LIBNAME names; sas2r could not use it (%s).",
            libref, reason),
    "#   Check it, then uncomment the entry.",
    paste0("#", libref_registry_entry(libref, exprs[1], exprs[1], "sas7bdat", "rds")))
}

#' Emit libref registry script
#'
#' Emits `_sas2r_registry.R`, the *seed* of the generated bundle's runtime
#' registry: the libraries configuration declares, plus a session `work`
#' directory. It is deliberately no longer a static project-wide map of every
#' `LIBNAME` the scan saw. A libref is bound at a point in an execution, not
#' once per project, so the bindings a source `LIBNAME` establishes are emitted
#' where those statements stand -- see [emit_libref_statement()] -- and this
#' file carries only what is in force before the program runs a statement.
#'
#' What the seed holds therefore comes from [effective_librefs()] and never
#' from a second parse of `project$librefs`: the projection is the single
#' authority on which binding the project selected, and a registry built beside
#' it could disagree with it.
#'
#' Librefs a generated read or write needs and nothing establishes keep a
#' commented entry carrying `<FILL>` guidance, so code-only translation stays
#' complete and the gap is visible in the file the user edits. The set is
#' decided on *bindings* rather than on declarations, which widens it by
#' exactly the cases that used to emit a path that could not work: a `LIBNAME`
#' naming a directory that is not there, one whose engine names no local
#' directory, and one whose path is an unexpanded macro expression all used to
#' reach this file as ordinary entries.
#'
#' Widening it that way must not cost the user the path the SAS *named*. The
#' ordinary case for a Windows-authored `libname adam 'J:\study\adam';` is to
#' be translated on a machine where that drive is not mounted, and a bundle in
#' which nothing records what the SAS said leaves the user with a libref, a
#' reason, and no way back to the library. So the commented entry carries the
#' `LIBNAME`'s own path expression whenever there is one, with the `<FILL>`
#' guidance beside it rather than in place of it: the path is what the SAS
#' asked for, and it is still a path sas2r could not use. Only a libref no
#' `LIBNAME` anywhere names -- the environment-provided case -- has no path to
#' record, and only that one falls back to a bare placeholder.
#'
#' @param project A `sas2r_project` object.
#' @param out_dir Output directory path.
#' @param effective The projection from [effective_librefs()]. Defaulted so a
#'   caller holding only a project still works; [sas_transpile()] passes the
#'   one it already built.
#' @param library_map Optional library map from [build_attempt_library_map()] to
#'   override default staged library entries with attempt-specific paths.
#' @return Invisible destination path.
#' @noRd
write_registry <- function(project, out_dir, effective = effective_librefs(project),
                           library_map = NULL) {
  if (!is.null(library_map)) {
    work <- library_map[["work"]]
    configured <- library_map[setdiff(names(library_map), "work")]
    entries <- vapply(names(configured), function(libref) {
      entry <- configured[[libref]]
      r_path <- confine_libref_path(entry$read_path %||% entry$path, libref)
      w_path <- confine_libref_path(entry$write_path %||% entry$path, libref)
      libref_registry_entry(libref, r_path, w_path,
                            entry$engine %||% "sas7bdat", entry$write %||% "rds")
    }, character(1), USE.NAMES = FALSE)
    undec_entries <- character()
    work_entry <- if (is.null(work)) {
      libref_registry_entry("work", "work", "work", "rds", "rds", comma = FALSE)
    } else {
      r_path <- confine_libref_path(work$read_path %||% work$path, "work")
      w_path <- confine_libref_path(work$write_path %||% work$path, "work")
      libref_registry_entry("work", r_path, w_path,
                            work$engine %||% "rds", work$write %||% "rds", comma = FALSE)
    }
  } else {
    seed <- effective$seed
    work <- seed[["work"]]
    configured <- seed[setdiff(names(seed), "work")]
    entries <- vapply(names(configured), function(libref) {
      entry <- configured[[libref]]
      r_path <- confine_libref_path(entry$read_path %||% entry$path, libref)
      w_path <- confine_libref_path(entry$write_path %||% entry$path, libref)
      libref_registry_entry(libref, r_path, w_path,
                            entry$engine, entry$write)
    }, character(1), USE.NAMES = FALSE)
    undec_entries <- unlist(lapply(effective$undeclared, function(libref) {
      undeclared_registry_entry(libref, effective$bindings)
    }), use.names = FALSE)
    work_entry <- if (is.null(work)) {
      libref_registry_entry("work", "work", "work", "rds", "rds", comma = FALSE)
    } else {
      r_path <- confine_libref_path(work$read_path %||% work$path, "work")
      w_path <- confine_libref_path(work$write_path %||% work$path, "work")
      libref_registry_entry("work", r_path, w_path,
                            work$engine, work$write, comma = FALSE)
    }
  }
  lines <- c(
    "# Generated by sas2r -- libref registry. Edit paths as needed.",
    "#",
    "# This is the seed: the libraries your configuration declares, plus the",
    "# session `work` directory. A libref a LIBNAME statement binds is not listed",
    "# here -- the staged module carries an sas2r_libname_assign() or",
    "# sas2r_libname_clear() call where that statement stands, so a libref that",
    "# is rebound or cleared part-way through a program behaves the way it does",
    "# in SAS.",
    "#",
    "# A commented <FILL> entry below is a library something reads and nothing",
    "# binds. Uncommenting it is enough unless the program itself clears the",
    "# libref; to survive a LIBNAME ... CLEAR, name the library under",
    "# `libraries:` in your sas2r configuration and translate again.",
    ".sas2r_registry <- list(",
    entries, undec_entries, work_entry, ")",
    "dir.create(.sas2r_registry$work$write_path, showWarnings = FALSE, recursive = TRUE)"
  )
  writeLines(lines, file.path(out_dir, "_sas2r_registry.R"))
  invisible(file.path(out_dir, "_sas2r_registry.R"))
}

#' Emit the runtime registry change one source `LIBNAME` statement makes
#'
#' The statement is emitted from the binding the project *already selected* at
#' that statement's own point of use, and from nothing else. Exactly one path
#' reaches the generated line -- never both the source and the configured one,
#' and never a runtime choice between them -- because a bundle that decided at
#' run time which library to read would be answering a question the project has
#' already answered, differently and invisibly.
#'
#' The four outcomes:
#'
#' * **bound.** One `sas2r_libname_assign()` naming the selected directory.
#'   When the location came from configuration rather than from the statement
#'   -- the directory the `LIBNAME` names is missing, unreadable, an unexpanded
#'   macro, or behind an engine that is not a local directory -- a comment says
#'   so, because otherwise a reader comparing the module against the SAS would
#'   find a path the SAS never mentions.
#' * **bound, and the statement is a `CLEAR`.** Also an assign: clearing a
#'   libref that configuration also names puts the *configured* library back in
#'   force, which is what the project resolved for every point of use below the
#'   clear, so the runtime registry has to agree. Emitting a clear here would
#'   drop the seed entry and strand those reads.
#' * **unbound.** A `CLEAR` with nothing configured emits
#'   `sas2r_libname_clear()`. An assignment that established nothing emits no
#'   call at all -- there is no path to name -- and leaves a visible marker
#'   plus the commented `<FILL>` entry [write_registry()] writes for it.
#' * **conditionally_bound.** A marker, never an assign. The location is
#'   reported and not established: the only `LIBNAME` for this libref sits
#'   inside a macro definition and runs only if something calls that macro, and
#'   sas2r analyses no macro call graph. Establishing it at run time would make
#'   the bundle bind a library SAS would not have bound. A macro definition is
#'   one translation unit, so such a statement is inside a `macro_deferred`
#'   stub and does not reach here today; the branch is the guard that keeps it
#'   that way.
#'
#' An `ambiguous_libref` statement is refused rather than emitted -- see
#' [sas_transpile()].
#'
#' @param row One `"statement"` row of [effective_librefs()]`$bindings`.
#' @param seed The projection's configured libraries.
#' @return A character vector of R source lines.
#' @noRd
emit_libref_statement <- function(row, seed = list()) {
  libref <- row$libref
  if (identical(row$status, "ambiguous_libref")) {
    cli::cli_abort(
      c("Libref {.val {libref}} has more than one plausible binding in {.file {row$use_file}}.",
        "x" = "Ambiguity: {.val {row$fallback_reason}}.",
        "i" = "A staged module is one file per source file, so it has one place to put the binding."),
      class = "sas2r_ambiguous_libref"
    )
  }
  configured <- seed[[libref]]
  if (identical(row$status, "bound")) {
    path <- confine_libref_path(row$selected_path, libref)
    engine <- if (identical(row$selection_origin, "source")) {
      # `nzchar(NA)` is TRUE, so the NA test has to come first.
      if (!is.na(row$source_engine) && nzchar(row$source_engine))
        row$source_engine else "sas7bdat"
    } else {
      configured$engine %||% "sas7bdat"
    }
    # `write` is how sas2r saves a dataset, which SAS has no way to say, so it
    # comes from configuration whichever authority supplied the location.
    write <- configured$write %||% "rds"
    note <- if (identical(row$selection_origin, "source")) character() else
      sprintf("# sas2r:libref_fallback libref=%s reason=%s -- the configured library, not the path the LIBNAME names",
              libref, row$fallback_reason)
    return(c(note,
             sprintf("sas2r_libname_assign(%s, %s, engine = %s, write = %s)",
                     deparse(libref), deparse(path), deparse(engine),
                     deparse(write))))
  }
  if (identical(row$action, "clear")) {
    return(sprintf("sas2r_libname_clear(%s)", deparse(libref)))
  }
  reason <- if (is.na(row$fallback_reason)) "no_source_binding" else
    row$fallback_reason
  # The marker reports no binding, so it has to report the path the statement
  # named -- otherwise the one place in the bundle that stands where the
  # LIBNAME stood says less than the LIBNAME did.
  named <- source_path_marker(row$source_path_expression)
  if (identical(row$status, "conditionally_bound")) {
    return(sprintf(
      "# sas2r:libref_conditional libref=%s reason=%s%s -- inside a macro definition, so it reports a library without establishing one",
      libref, reason, named))
  }
  c(sprintf("# sas2r:libref_unbound libref=%s reason=%s%s", libref, reason,
            named),
    sprintf("# No runtime binding is established here. Fill in the commented %s entry",
            libref),
    "# in _sas2r_registry.R to make the reads below it work.")
}

#' Emit the marker one `LIBNAME` naming `work` leaves behind
#'
#' `work` is the session library the bundle's own seed creates; nothing else
#' creates it. An emitted `sas2r_libname_assign("work", ...)` would repoint it
#' at a directory no `dir.create()` ever made, and the first `lib_write()` to
#' `work` below it would fail -- in a bundle whose SAS looked entirely
#' ordinary. So the statement is reported and not applied, which is also what
#' [effective_librefs()] does with every *reference* to `work`.
#'
#' Reporting it matters: silently dropping the statement would leave the unit's
#' anchor with nothing under it, and the emitter's fallback would then describe
#' a perfectly readable `LIBNAME` as a form sas2r cannot read.
#'
#' @param row One row of [effective_librefs()]`$session_library`.
#' @return One line of R source.
#' @noRd
emit_session_library_statement <- function(row) {
  sprintf("# sas2r:libref_session_library libref=%s action=%s%s -- WORK is the session library this bundle creates, so a LIBNAME naming it does not rebind it",
          row$libref, row$action,
          source_path_marker(row$source_path_expression))
}

#' Emit the runtime call that executes one staged include module
#'
#' The emitted call names the module by its path *relative to the staged
#' bundle*, never by an absolute one, so a staged bundle stays relocatable. The
#' path is re-checked here rather than trusted from the manifest: it is derived
#' from source text and configuration, and everything derived from those is
#' confined before it can reach a read. `sas2r_source_include()` checks it a
#' second time at run time, when the bundle may have been edited by hand.
#'
#' @param staged_module Relative staged module path, from
#'   [canonical_include_staged_path()].
#' @return One line of R code.
#' @noRd
emit_include_call <- function(staged_module) {
  if (path_is_absolute_or_escaping(staged_module)) {
    cli::cli_abort("unsafe staged include path", class = "sas2r_include_path_error")
  }
  sprintf("sas2r_source_include(%s, envir = environment())",
          deparse(gsub("\\\\", "/", staged_module)))
}

#' Emit lib_write statements for multiple output datasets
#'
#' @param var_name Character name of R variable to write.
#' @param outputs Character vector of SAS output dataset names.
#' @return Character vector of `lib_write(...)` code lines.
#' @noRd
emit_lib_writes <- function(var_name, outputs) {
  vapply(outputs, function(out_ds) {
    op <- split_ds(out_ds)
    sprintf('lib_write(%s, "%s", "%s")', var_name, op[["lib"]], op[["member"]])
  }, character(1))
}

