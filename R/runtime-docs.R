#' The runtime every translated program carries
#'
#' @description
#' Every program sas2r translates calls a small runtime -- [lib_read()],
#' [lib_write()], [sas_sort()], [sas_merge()], [chr_cmp()], the format
#' machinery and the rest. Since sas2r 0.2.0 that runtime is exported package
#' code with a help page per helper, **and** every bundle still carries its
#' own copy as `sas2r-helpers.R`, rendered from the same source files
#' (`R/runtime-*.R`) so the two are identical function for function. A
#' bundle always loads its own copy: its behavior depends only on its own
#' contents, never on which sas2r is installed. That is what lets a
#' translated program run without sas2r installed.
#'
#' The vignette `vignette("runtime-helpers", package = "sas2r")` explains how
#' to run programs, how to use the runtime interactively, and the stability
#' policy behind it.
#'
#' @section How the runtime reaches a program:
#' A bootstrap header at the top of each generated program locates the bundle
#' root (the nearest directory at or above the program holding
#' `_sas2r_registry.R`) and sources three files into the program's own
#' environment: `_sas2r_registry.R` (where each libref reads from and writes
#' to), `sas2r-helpers.R` (this runtime), and `_sas2r_formats.R` (the compiled
#' format catalog). Run a program with `source("<bundle>/<program>.R")`, or
#' with `Rscript` from any directory.
#'
#' To use the runtime at the console, attach sas2r and load the registry --
#' `source("<bundle>/_sas2r_registry.R")` -- or `source()` the same three files
#' the programs load.
#'
#' @section The helpers:
#' \describe{
#'   \item{Data access}{[lib_read()], [lib_write()], [sas2r_libname_assign()],
#'     [sas2r_libname_clear()], [sas2r_fold_names()]; datasets read through
#'     `lib_read()` carry class `sas2r_dataset`, whose `$` and `[[` methods
#'     resolve column names case-insensitively.}
#'   \item{SAS value semantics}{[chr_cmp()], [sas_if_else()], [sas_sum()] and
#'     [sas_mean()], [sas_min()] and [sas_max()], [sas_round()],
#'     [sas_length()], [sas_substr()], [sas_compress()], [sas_display()]; and
#'     two operators the package defines but does not export: `a %notin% b`
#'     (`NOT IN` with SAS missing semantics -- a missing `a` is in no list;
#'     character comparison ignores trailing blanks) and `a %+% b` (`||`
#'     concatenation where a missing operand contributes nothing rather than
#'     `"NA"`; SAS pads fixed-width operands, this does not).}
#'   \item{Sorting and merging}{[sas_sort()], [sas_merge()].}
#'   \item{Formats}{[apply_format()], [sas_put()].}
#'   \item{Includes}{[sas2r_source_include()].}
#' }
#'
#' @section Internal helpers:
#' Generated code calls these; they are not exported, not meant for direct
#' use, and may change with the generator: `sas2r_registry_env()` (where the
#' `.sas2r_registry` list lives -- lexically where the runtime was loaded,
#' then the global environment, then any active frame), `sas2r_lib_entry()`
#' (the registry entry for a libref), `sas2r_lib_member_path()` (the file a
#' member resolves to, refusing any name that is not a plain member name),
#' `sas2r_libref_stop()` (classed libref errors), `split_ds()` (split a
#' two-level SAS name after macro substitution).
#'
#' @section Versioning and stability:
#' The runtime follows the sas2r version, and every vendored copy states the
#' version that generated it. Within a major version an existing helper never
#' changes what it does for the same inputs; a helper is retired only after a
#' replacement ships and the old name has warned for at least one minor
#' version. Tests hold the source files, the vendored template, the help
#' pages, and the lint allowlist in sync. The direction of travel -- a
#' separate `sas2r.runtime` package when usage justifies it -- is recorded in
#' `docs/decisions/0003-runtime-helpers-distribution.md`.
#'
#' @name sas2r_runtime
#' @aliases sas2r_helpers sas2r_registry_env sas2r_lib_entry
#'   sas2r_lib_member_path sas2r_libref_stop split_ds
#' @seealso [sas_translate()], [sas_write()], [sas_code()]
NULL
