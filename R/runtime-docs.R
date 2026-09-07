#' The runtime every translated program carries
#'
#' @description
#' Every program sas2r translates calls a small runtime of thirty functions --
#' `lib_read()`, `lib_write()`, `sas_sort()`, `sas_merge()`, `chr_cmp()`, the
#' format machinery and the rest. The runtime is **not** part of the sas2r
#' namespace: it ships as `sas2r-helpers.R` inside every generated bundle, and
#' the bundle's programs load it at startup. That is what lets a translated
#' program run without sas2r installed. This topic documents the runtime; the
#' vignette `vignette("runtime-helpers", package = "sas2r")` explains how to
#' use it interactively and the stability policy behind it.
#'
#' Each vendored `sas2r-helpers.R` states the sas2r version that generated it.
#' A bundle always loads its own copy: its behavior depends only on its own
#' contents, never on which sas2r is installed.
#'
#' @section How the runtime reaches a program:
#' A bootstrap header at the top of each generated program locates the bundle
#' root (the nearest directory at or above the program holding
#' `_sas2r_registry.R`) and sources three files into the program's own
#' environment: `_sas2r_registry.R` (where each libref reads from and writes
#' to), `sas2r-helpers.R` (this runtime), and `_sas2r_formats.R` (the compiled
#' format catalog). Run a program with `source("<bundle>/<program>.R")`, or
#' with `Rscript` from any directory. To use the runtime at the console,
#' `source()` the same three files from the bundle directory.
#'
#' @section Data access:
#' \describe{
#'   \item{`lib_read("lib", "member")`}{Read a dataset from a libref, as
#'     configured in the registry: the write path is tried first (so a
#'     dataset the bundle produced earlier wins), then the read path, for
#'     `.rds`, `.sas7bdat`, and `.xpt` in that order. The result carries class
#'     `sas2r_dataset`, whose `$` and `[[` methods resolve column names
#'     case-insensitively, as SAS variable names do. Exactly one call form is
#'     accepted; combined `"lib.member"` strings, single-argument calls, and
#'     `dataset=`/`table=` aliases fail with the canonical form in the message.}
#'   \item{`lib_write(df, "lib", "member")`}{Write a dataset to a libref's
#'     write path in its configured format (`rds` or `xpt`), creating the
#'     directory on demand. Data frame first; the same single call form.}
#'   \item{`sas2r_libname_assign(libref, read_path, write_path, engine, write)`}{
#'     A `LIBNAME` statement at its point of use: rebinds the libref from this
#'     point on, so a program that reassigns a libref part-way through behaves
#'     as it does in SAS.}
#'   \item{`sas2r_libname_clear(libref)`}{`LIBNAME libref CLEAR`.}
#'   \item{`sas2r_fold_names(df)`}{Lower-case every column name. Deterministic
#'     translations fold frames at entry so lower-case source references find
#'     upper-case data columns.}
#' }
#'
#' @section SAS semantics for values:
#' \describe{
#'   \item{`chr_cmp(a, b, op = NULL)`}{SAS comparison of possibly-missing
#'     values. Character operands ignore trailing blanks; a missing character
#'     value (`""` or `NA`) sorts below every non-missing one; numeric missing
#'     sorts below every number. With `op` `NULL` the result is `-1`/`0`/`1`
#'     (missing equals missing); with an operator such as `"=="` or `"<"` it
#'     is the logical SAS would produce -- never `NA`.}
#'   \item{`a %notin% b`}{`NOT IN` with SAS missing semantics: a missing `a` is
#'     not in any list. Character comparison ignores trailing blanks.}
#'   \item{`a %+% b`}{`||` concatenation where a missing operand contributes
#'     nothing rather than `"NA"`. SAS pads fixed-width operands; this does not.}
#'   \item{`sas_if_else(cond, yes, no)`}{`IF/THEN/ELSE` assignment: a missing
#'     condition is false, and the result keeps the class (`Date`, labelled)
#'     of the branch that carries it, which `ifelse()` would strip.}
#'   \item{`sas_sum(...)`, `sas_mean(...)`}{Row-wise across the arguments, as
#'     the SAS functions are: each observation gets the sum or mean of its
#'     non-missing arguments, and only an all-missing observation is missing.
#'     Column aggregation (`PROC MEANS`, `PROC SQL`) is emitted inline and
#'     never routed through these.}
#'   \item{`sas_min(...)`, `sas_max(...)`}{Row-wise, ignoring missing; all
#'     missing yields missing; a single argument passes through elementwise.}
#'   \item{`sas_round(x, unit = 1)`}{`ROUND`: half away from zero, to a
#'     rounding unit -- not R's banker's rounding.}
#'   \item{`sas_length(x)`}{`LENGTH`: trailing blanks do not count, and a
#'     missing or blank value has length 1.}
#'   \item{`sas_substr(x, pos, len = NULL)`}{`SUBSTR`; `len` `NULL` reads to
#'     the end.}
#'   \item{`sas_compress(x, chars = " ")`}{`COMPRESS`: remove every character
#'     listed in `chars`.}
#'   \item{`sas_display(x)`}{Values as SAS prints them: numeric missing is
#'     `"."`, character missing is blank, no scientific notation.}
#' }
#'
#' @section Sorting and merging:
#' \describe{
#'   \item{`sas_sort(df, by, descending = character())`}{`PROC SORT`: stable,
#'     missing values lowest, `by` matched case-insensitively, variables named
#'     in `descending` sorted in reverse.}
#'   \item{`sas_merge(a, b, by, keep = "both")`}{`MERGE a b; BY by;` with `IN=`
#'     semantics chosen by `keep`: `"both"`, `"left"`, `"right"`,
#'     `"left_only"`, `"right_only"`, `"full"`. Where both datasets carry a
#'     non-key column, the later dataset's value wins; columns follow
#'     statement order and rows follow SAS `BY` ordering. A many-to-many key
#'     is refused, because no join reproduces SAS's row walking there.}
#' }
#'
#' @section Formats:
#' \describe{
#'   \item{`apply_format(x, fmt)`}{Apply a compiled format from
#'     `_sas2r_formats.R`: exact `values` first, then `ranges` (`lo`..`hi` to a
#'     label), then `other` for anything unmatched. Missing input stays missing
#'     unless the format defines `other`.}
#'   \item{`sas_put(x, fmt)`}{`PUT(x, fmt.)`; the same as `apply_format()`.}
#' }
#'
#' @section Includes:
#' \describe{
#'   \item{`sas2r_source_include(relative_path, envir = parent.frame())`}{An
#'     `%INCLUDE` at its call site: runs the included module, translated once
#'     into its own staged file, in the calling program's environment. The
#'     path is relative to the bundle root, never to the working directory;
#'     absolute paths, `.`/`..` components, and anything resolving outside the
#'     bundle are refused.}
#' }
#'
#' @section Internal helpers:
#' Generated code calls these; they are not meant for direct use and may change
#' with the generator: `sas2r_lib_entry()` (the registry entry for a libref),
#' `sas2r_lib_member_path()` (the file a member resolves to, refusing any name
#' that is not a plain member name), `sas2r_libref_stop()` (classed libref
#' errors), `split_ds()` (split a two-level SAS name after macro substitution),
#' and `emit_proc_sort()`, a compatibility alias for `sas_sort()` kept for
#' bundles generated by earlier versions and scheduled for retirement.
#'
#' @section Versioning and stability:
#' The runtime follows the sas2r version, and every vendored copy states the
#' version that generated it. Within a major version an existing helper never
#' changes what it does for the same inputs; a helper is retired only after a
#' replacement ships and the old name has warned for at least one minor
#' version. The lint allowlist, this topic, and the template are held in sync
#' by tests. The direction of travel -- one exported source of truth, vendored
#' by default, and eventually a separate `sas2r.runtime` package -- is recorded
#' in `docs/decisions/0003-runtime-helpers-distribution.md`.
#'
#' @name sas2r_runtime
#' @aliases sas2r_helpers lib_read lib_write sas_sort sas_merge chr_cmp
#'   sas_if_else sas_sum sas_mean sas_min sas_max sas_round sas_length
#'   sas_substr sas_compress sas_display sas_put apply_format sas2r_fold_names
#'   sas2r_source_include sas2r_libname_assign sas2r_libname_clear
#'   sas2r_lib_entry sas2r_lib_member_path sas2r_libref_stop split_ds
#'   emit_proc_sort
#' @seealso [sas_translate()], [sas_write()], [sas_code()]
NULL
