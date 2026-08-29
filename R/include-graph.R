#' Schema version of the `%include` occurrence graph
#'
#' Bumping this version changes every occurrence identity, so it must only move
#' when the meaning of an occurrence itself changes.
#' @noRd
INCLUDE_GRAPH_VERSION <- 5L

#' Maximum `%include` nesting depth the scanner follows
#'
#' A package constant, not configuration: a project cannot raise it.
#' @noRd
INCLUDE_MAX_DEPTH <- 10L

#' Exact column order of `project$include_graph$occurrences`
#'
#' This is the single source of the column order: both the zero-row prototype in
#' [empty_include_occurrences()] and the per-file table assembled in
#' `sas_project()` are ordered by it rather than by a hand-written literal.
#'
#' Column semantics worth knowing before consuming the table:
#'
#' * `parent_unit_id` is the **project-level** translation unit id, so it joins
#'   directly to `project$units$unit_id`. It is deliberately *not* part of
#'   `occurrence_id`, which is keyed on the file-local unit index instead --
#'   see [include_occurrence_id()].
#' * A non-`NA` `target_file` means the target *resolved* to a path, not that
#'   the file was scanned. `cycle` and `depth_exceeded` occurrences carry a
#'   resolved `target_file` for a file the scanner deliberately never read, so
#'   a non-`NA` `target_file` does not imply a staged module exists for it;
#'   join against `include_graph$files$canonical_file` to find out.
#' * When a target is both a cycle and past [INCLUDE_MAX_DEPTH], the scanner
#'   checks the cycle first, so the occurrence gets `status = "cycle"`. That
#'   ordering is deterministic and is the more precise diagnosis: the depth was
#'   only reached by going around the cycle.
#' @noRd
INCLUDE_OCCURRENCE_COLUMNS <- c(
  "occurrence_id", "parent_file", "parent_unit_id", "line",
  "target_expression", "target_file", "resolution_origin",
  "status", "reason", "depth"
)

#' Zero-row prototype of every occurrence column, keyed by name
#'
#' Types live here, order lives in [INCLUDE_OCCURRENCE_COLUMNS]; keeping them
#' apart means a column can never be declared in one place and ordered in
#' another.
#' @noRd
INCLUDE_OCCURRENCE_PROTOTYPE <- list(
  occurrence_id = character(),
  parent_file = character(),
  parent_unit_id = integer(),
  line = integer(),
  target_expression = character(),
  target_file = character(),
  resolution_origin = character(),
  status = character(),
  reason = character(),
  depth = integer()
)

#' Every status an occurrence can carry
#' @noRd
INCLUDE_OCCURRENCE_STATUSES <- c(
  "resolved", "unresolved", "dynamic", "cycle", "depth_exceeded"
)

#' Every origin a resolution can be attributed to
#' @noRd
INCLUDE_RESOLUTION_ORIGINS <- c(
  "absolute", "fileref", "including_dir", "project_root",
  "configured_fallback", "none"
)

#' Canonicalize a filesystem path for include bookkeeping
#'
#' One canonical form is used for occurrence identities, scan keys, and the
#' graph's `canonical_file` column so they always compare equal.
#'
#' [normalizePath()] resolves symlinks, which is what makes two routes to one
#' physical file dedupe to a single scan. The consequence for identity is
#' recorded in [include_identity_anchors()]: a symlink *inside* the project root
#' whose target lies outside it canonicalizes to the outside path, so the file
#' is named against no anchor.
#'
#' @param path Character vector of paths.
#' @return Character vector of canonicalized paths using forward slashes.
#' @noRd
include_normalize_path <- function(path) {
  if (!length(path)) return(character())
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Case-insensitive scan key for a physical file
#'
#' @param path Character vector of paths.
#' @return Character vector of lower-cased canonical paths.
#' @noRd
include_scan_key <- function(path) {
  tolower(include_normalize_path(path))
}

#' Sentinel naming a file that lies under no anchor at all
#'
#' Deliberately carries no trailing separator: the canonical path it is pasted
#' in front of is absolute and so supplies its own leading one. Pasting is a
#' plain prefix, which keeps the path-to-key map injective -- adding or removing
#' a separator conditionally would map two different paths onto one key.
#' @noRd
INCLUDE_EXTERNAL_PARENT_PREFIX <- "<outside-root>"

#' Sentinel naming the `n`-th configured include root
#' @noRd
INCLUDE_ANCHOR_INCLUDE_ROOT <- "<include-root:%d>/"

#' Sentinel naming the directory of the `n`-th configured `autoexec` entry
#' @noRd
INCLUDE_ANCHOR_AUTOEXEC <- "<autoexec:%d>/"

#' Require a scalar identity input
#'
#' Occurrence identity is a scalar computation. Silently keeping `x[1]` of a
#' longer vector would mint an id for a site the caller never named, and a
#' zero-length input would recycle away to no id at all, so the length contract
#' is enforced rather than assumed.
#'
#' @param x Value to check.
#' @param arg Argument name, for the error message.
#' @return `x`, unchanged, once its length is known to be one.
#' @noRd
include_identity_scalar <- function(x, arg) {
  if (length(x) != 1L) {
    cli::cli_abort(
      "{.arg {arg}} must be a single value, not length {length(x)}.",
      class = "sas2r_include_identity_not_scalar"
    )
  }
  x
}

#' Ordinal of each include site among the sites it is indistinguishable from
#'
#' A file-local unit index, a line, and a target expression normally pin one
#' `%include` statement, but not always: SAS allows several statements on a
#' line, and two identical ones inside a single translation unit --
#' `%macro m; %include 'inc.sas'; %include 'inc.sas'; %mend;` -- agree on all
#' three. Nothing else about those two sites differs, so identity needs the one
#' remaining fact: which of them this is.
#'
#' The ordinal counts only within an otherwise-identical group, so it moves only
#' when a duplicate of that exact statement is added or removed on that exact
#' line. It is not a statement index: adding an unrelated statement earlier in
#' the file leaves every ordinal alone.
#'
#' Two consequences of *any* ordinal, stated rather than hidden:
#'
#' * An ordinal is a position within its group, so an id can transfer between
#'   sites when the group shrinks. Given
#'   `%macro m; %include 'a.sas'; %include 'a.sas'; %mend;` the two sites hold
#'   ordinals 1 and 2 and so ids X and Y. Editing the **first** target to
#'   `'b.sas'` empties the group's first slot, and the second site -- untouched,
#'   byte-identical -- becomes ordinal 1 and takes id X, the id that until then
#'   named the site above it. An approval ledger keyed on `occurrence_id`
#'   therefore carries an approval of the removed site onto the surviving one.
#'   Only a content hash of the whole unit could tell the two apart, and that
#'   would re-mint on every unrelated edit, so the residual is accepted rather
#'   than traded for churn. It is the same class of stated, bounded residual as
#'   the `../`-out-of-root one recorded in [include_identity_anchors()].
#' * The grouping is exact-match only, and is computed with [match()] over the
#'   composite key rather than by naming an environment slot after it. An
#'   environment name tops out at 10000 bytes, which a machine-generated
#'   `%include` argument can exceed; [match()] has no such ceiling.
#'
#' @param file_unit_id File-local translation unit index of each `%include`.
#' @param line Line number of each `%include`.
#' @param target_expression Literal target text of each `%include`.
#' @return Integer vector of 1-based ordinals, one per site.
#' @noRd
include_site_ordinal <- function(file_unit_id, line, target_expression) {
  n <- length(target_expression)
  if (!n) return(integer())
  key <- paste(as.integer(file_unit_id), as.integer(line), target_expression,
               sep = "\u001f")
  group <- match(key, unique(key))
  ordinal <- integer(n)
  for (idx in split(seq_len(n), group)) ordinal[idx] <- seq_along(idx)
  ordinal
}

#' Machine-independent coordinate frame for naming scanned files
#'
#' This is the single place the machine-independence argument lives;
#' [include_identity_parent()] and [include_occurrence_id()] refer back to it
#' rather than restating it.
#'
#' An absolute path is machine state, not source content: folding one into an
#' occurrence identity makes the same bytes hash differently in a developer
#' checkout and in CI. So every scanned file is named by its position relative
#' to an *anchor* -- a directory the project's own configuration names -- and
#' never by where it happens to sit on this machine. The anchor list is built
#' once per scan, in a fixed order that comes from configuration and from
#' nothing else:
#'
#' 1. the project root, contributing no prefix at all, so an in-root file keeps
#'    its plain root-relative path;
#' 2. each configured `include_roots` entry, prefixed `<include-root:N>/` where
#'    `N` is its **configured** position, not its scan position;
#' 3. the directory of each configured `autoexec` entry, prefixed
#'    `<autoexec:N>/`, again by configured position.
#'
#' A file is named against the first anchor that contains it; a file under no
#' anchor keeps its canonical absolute path behind
#' [INCLUDE_EXTERNAL_PARENT_PREFIX].
#'
#' What that frame buys, and why:
#'
#' * **Machine-independent.** An anchor's own absolute location never reaches
#'   the key -- only its configured index and the path below it do -- so
#'   relocating the checkout, or pointing `include_roots` at a different
#'   absolute directory holding the same sources, leaves every id unchanged.
#'   That holds only because every anchor arrives here already carrying its own
#'   base. A *relative* `include_roots` entry carries none, and normalizing one
#'   here would let [normalizePath()] supply [getwd()] as the base: the same
#'   bytes would then name a different directory -- and so mint different ids --
#'   from a developer's checkout root and from CI's working directory.
#'   [config_resolve_paths()] therefore resolves such an entry before it gets
#'   here, against the configuration file's directory or the project root, and
#'   `sas_project()` hands the same resolved vector to `include_candidates()` so
#'   that resolution and identity cannot disagree about what a root is.
#'   The one branch that does carry an absolute path is the no-anchor fallback,
#'   and a file normally reaches it because the *source* named an absolute
#'   location, or named a path relative to a file that was reached that way.
#'   Such a path is source content: byte-identical sources name the same
#'   absolute location on any machine, so the same bytes still mint the same id.
#'   The residual below is the one route into that branch for which this does
#'   not hold.
#' * **Order-free, and so scan-mode-invariant.** The key is a function of the
#'   file and the anchor list alone. Nothing about the include *chain* -- which
#'   site reached the file, or which reached it first -- is an input, so a
#'   shared macro library included by two programs gets one identity whichever
#'   program the scanner walks first, and whether a driver is scanned alone or
#'   as one file of its directory.
#' * **Composition-invariant.** Adding, removing, or renaming an unrelated
#'   `.sas` file changes neither the anchor list nor any other file's position
#'   beneath it.
#' * **Collision-free.** Within one anchor, distinct files have distinct
#'   relative paths; across anchors, the prefixes differ. Two copies of one
#'   macro library keep distinct identities even though each holds a `setup.sas`
#'   whose relative `%include 'common.sas'` sits on the same file-local unit and
#'   the same line.
#'
#' Consequences worth knowing:
#'
#' * A symlink under the project root whose target lies outside it canonicalizes
#'   to the outside path (see [include_normalize_path()]) and so is named
#'   against whichever anchor holds its *target*, not against the root. A
#'   project that vendors macros by symlinking them into the tree gets
#'   target-side identities for them -- deterministic, and still
#'   machine-independent whenever the target lies under a configured anchor.
#' * A project directory literally named `<include-root:1>` or `<autoexec:1>`
#'   mints a root-relative key indistinguishable from that anchor's own, and one
#'   named `<outside-root>` shadows the no-anchor sentinel. Nothing silently
#'   accepts a duplicate that results: `sas_project()` asserts occurrence-id
#'   uniqueness across every scanned file when it binds the occurrence table.
#' * The frame is root-relative, so the root is part of it: scanning
#'   `proj/sub/driver.sas` alone roots the frame at `proj/sub` and names the
#'   driver `driver.sas`, while scanning `proj` recursively roots it at `proj`
#'   and names the same file `sub/driver.sas`. Those are different keys for one
#'   file. This is not the scan-mode invariance above -- that is invariance
#'   across *how* one project is walked, and this is a change of *which* project
#'   is being named -- but it is inherent to any root-relative frame and is
#'   stated here so a ledger is not carried between scan scopes by accident.
#'   Pick one scope per project and keep it.
#' * **Residual (silent).** The `include_roots` prefix carries an entry's
#'   configured *position*, so prepending an unrelated root to `includes.roots`
#'   turns `<include-root:1>/k.sas` into `<include-root:2>/k.sas` and re-mints
#'   every occurrence recorded under that root. Nothing about any source file
#'   changed, so nothing warns; an approval ledger keyed on `occurrence_id`
#'   silently loses those approvals. Keying on the entry's text instead would
#'   trade this for re-minting whenever the directory is *spelled* differently,
#'   which is the more common edit and would also put an absolute path back in
#'   the key. Append to `includes.roots` rather than reordering it. This is the
#'   same class of bounded, ledger-invalidating residual as the ordinal transfer
#'   in [include_site_ordinal()] and the no-anchor one below.
#' * **Residual.** A file that lies under no anchor and was reached by a
#'   *relative* source spelling is named by its absolute path, which moves with
#'   the checkout although the source did not change. The commonest route in is
#'   a `../` walk out of the root, but the branch is not specific to `../` or to
#'   quoted targets: `filename mylib '../shared'; %include mylib(setup);`
#'   arrives at it too, with resolution origin `fileref`, as does any other
#'   relative spelling that lands outside every anchor.
#'
#'   The blast radius is larger than the one file. That file's key is the frame
#'   for everything it includes, so every occurrence inside it *and its whole
#'   included subtree* re-mints on relocation, while the occurrences in the
#'   parent that reached it -- which is under an anchor -- stay stable. A ledger
#'   therefore keeps its approvals for the parent and loses them for the entire
#'   subtree, exactly the split the ordinal residual in [include_site_ordinal()]
#'   describes for a single site.
#'
#'   Naming such a file relative to the root instead would fix this case and
#'   break the commoner one -- a source-named absolute library, which must *not*
#'   be re-keyed when the checkout moves and the library does not -- and the two
#'   cannot be told apart from the file alone, because both can be the very same
#'   path. Listing the directory in `includes.roots` makes it an anchor and
#'   restores relocatable ids.
#'
#' @param project_root Root directory of the project; a scalar naming a
#'   directory. Every entry of `include_roots` and `autoexec` must already carry
#'   its own base -- see [config_resolve_paths()].
#' @param include_roots Configured fallback roots, in configured order.
#' @param autoexec Configured `autoexec` paths, in configured order.
#' @return An ordered list of anchors, each a list of `dir` (a canonical
#'   directory ending in `/`) and `prefix` (that anchor's key sentinel).
#' @noRd
include_identity_anchors <- function(project_root, include_roots = character(),
                                     autoexec = character()) {
  as_dir <- function(p) {
    p <- include_normalize_path(p)
    if (endsWith(p, "/")) p else paste0(p, "/")
  }
  anchors <- list()
  add <- function(dir, prefix) {
    # Checked where anchors are made rather than re-tested on every lookup: an
    # empty directory would prefix-match every path and so capture the whole
    # project into whichever anchor held it. `dir` is the post-as_dir() value,
    # and as_dir() always appends a trailing "/" when its normalized input
    # lacks one, so its shortest possible output is "/", never "" -- nzchar()
    # alone cannot see that. "/" is exactly as dangerous as "": every scanned
    # file's canonical path is itself absolute, so a "/" anchor prefix-matches
    # all of them too. A configured include root of "/", a project root of
    # "/", or an autoexec file directly at the filesystem root all reach this
    # with dir == "/".
    stopifnot(
      "an anchor directory must be a non-empty path" =
        length(dir) == 1L && !is.na(dir) && nzchar(dir),
      "an anchor directory must not be the filesystem root" = dir != "/"
    )
    anchors[[length(anchors) + 1L]] <<- list(dir = dir, prefix = prefix)
  }

  # The root is an identity input like any other, so it gets the same scalar
  # discipline: taking `[1]` of a longer vector would frame the whole project on
  # a directory the caller never named, and a frame with no root at all would
  # name every in-root file against the no-anchor fallback -- absolutely, and so
  # machine-dependently. Neither is invented quietly.
  root <- as.character(include_identity_scalar(project_root, "project_root"))
  if (is.na(root) || !nzchar(root)) {
    cli::cli_abort(
      "{.arg project_root} must name a directory, not {.val {root}}.",
      class = "sas2r_include_identity_no_root"
    )
  }
  add(as_dir(root), "")

  roots <- as.character(include_roots %||% character())
  for (i in seq_along(roots)) {
    if (is.na(roots[i]) || !nzchar(roots[i])) next
    add(as_dir(roots[i]), sprintf(INCLUDE_ANCHOR_INCLUDE_ROOT, i))
  }

  auto <- as.character(autoexec %||% character())
  for (i in seq_along(auto)) {
    if (is.na(auto[i]) || !nzchar(auto[i])) next
    add(as_dir(dirname(auto[i])), sprintf(INCLUDE_ANCHOR_AUTOEXEC, i))
  }
  anchors
}

#' Machine-independent identity key for a parent file
#'
#' The file's canonical path re-expressed in the coordinate frame of
#' [include_identity_anchors()], which carries the whole argument for why that
#' frame is machine-independent, order-free, composition-invariant, and
#' collision-free. This function is only the lookup: the first anchor that
#' contains the file wins.
#'
#' The include chain is deliberately not an input, and that is the property the
#' two earlier schemes lacked. The scanner reads each physical file once, so any
#' key derived from the site that reached it is really a key on whichever chain
#' arrived first -- which changes with an unrelated rename, with the scan mode,
#' and with nothing in the file itself.
#'
#' A `parent_file` that is `NA` or empty names no file, so there is no key to
#' compute and none is invented: returning a bare sentinel would hand two
#' different unnamed parents the same identity.
#'
#' @param parent_file Path of the file containing the `%include`; a scalar.
#' @param anchors Anchor list from [include_identity_anchors()].
#' @return A character path key, machine-independent except in the no-anchor
#'   fallback documented on [include_identity_anchors()].
#' @noRd
include_identity_parent <- function(parent_file, anchors) {
  raw_parent <- as.character(include_identity_scalar(parent_file, "parent_file"))
  if (is.na(raw_parent) || !nzchar(raw_parent)) {
    cli::cli_abort(
      "{.arg parent_file} must name a file, not {.val {raw_parent}}.",
      class = "sas2r_include_identity_no_parent"
    )
  }
  parent <- include_normalize_path(raw_parent)
  for (anchor in anchors) {
    if (startsWith(parent, anchor$dir)) {
      return(paste0(anchor$prefix, substring(parent, nchar(anchor$dir) + 1L)))
    }
  }
  paste0(INCLUDE_EXTERNAL_PARENT_PREFIX, parent)
}

#' Stable identity for one `%include` occurrence
#'
#' Identity is a pure function of the include *site* -- the including file, the
#' translation unit and line it sits on, and the literal target expression --
#' so the same physical file included twice yields two distinct occurrences.
#' When even those agree, [include_site_ordinal()] supplies the tiebreak.
#'
#' Both site inputs are deliberately position-free and machine-free:
#'
#' * `file_unit_id` is the unit's index **within its own source file**, taken
#'   before `sas_project()` offsets it into the project-level numbering. The
#'   project-level id shifts whenever any earlier-scanned file gains, loses, or
#'   stops being scanned at all, so keying on it would silently re-mint every
#'   later occurrence id; the file-local index depends on nothing outside the
#'   file. The project-level id stays available as the `parent_unit_id` column
#'   of the occurrences table for joining.
#' * `parent_identity` is the including file already reduced to an anchor key by
#'   [include_identity_parent()]; [include_identity_anchors()] carries the
#'   argument for why that key is machine-independent and order-free. This
#'   function never touches the filesystem, so the reduction happens in exactly
#'   one place and no absolute path can leak in behind its back.
#'
#' Every argument is a scalar; passing vectors would recycle into a vector of
#' ids rather than an id, so the length contract is checked, not assumed.
#'
#' @param parent_identity Identity key of the file containing the `%include`,
#'   from [include_identity_parent()].
#' @param file_unit_id File-local translation unit index of the `%include`,
#'   before any project-level offset is applied.
#' @param line Line number of the `%include` statement.
#' @param target_expression Literal target text as written in the source.
#' @param site_ordinal Which of the otherwise-identical sites this is, from
#'   [include_site_ordinal()]; `1L` unless the same statement appears more than
#'   once on one line of one unit.
#' @return A character occurrence id.
#' @noRd
include_occurrence_id <- function(parent_identity, file_unit_id, line,
                                  target_expression, site_ordinal = 1L) {
  key <- paste(
    INCLUDE_GRAPH_VERSION,
    include_identity_scalar(parent_identity, "parent_identity"),
    as.integer(include_identity_scalar(file_unit_id, "file_unit_id")),
    as.integer(include_identity_scalar(line, "line")),
    include_identity_scalar(target_expression, "target_expression"),
    as.integer(include_identity_scalar(site_ordinal, "site_ordinal")),
    sep = "\u001f"
  )
  paste0("include_", substr(migration_hash(key), 1L, 32L))
}

#' Build one resolution candidate
#'
#' @param path Candidate path.
#' @param origin One of [INCLUDE_RESOLUTION_ORIGINS].
#' @return A list with `path` and `origin`.
#' @noRd
include_candidate <- function(path, origin) {
  list(path = as.character(path)[1], origin = origin)
}

#' Derive the path a fileref-style include target points at
#'
#' Handles both `ref` and `ref(member)` forms. Unknown filerefs derive nothing,
#' which keeps an unregistered fileref an unresolved occurrence rather than a
#' bare relative path search.
#'
#' @param target Literal include target text.
#' @param filerefs Named list of fileref -> path bindings harvested from source.
#' @return Derived path, or `NA_character_` when the target is not a known fileref.
#' @noRd
include_fileref_target <- function(target, filerefs) {
  if (!length(filerefs)) return(NA_character_)
  m <- regmatches(target, regexec(
    "^([A-Za-z_]\\w*)(?:\\s*\\(\\s*([A-Za-z_]\\w*)\\s*\\))?", target))[[1]]
  if (length(m) < 2L || !nzchar(m[2])) return(NA_character_)
  base_dir <- filerefs[[tolower(m[2])]]
  if (is.null(base_dir)) return(NA_character_)
  base_dir <- as.character(base_dir)[1]
  if (is.na(base_dir) || !nzchar(base_dir)) return(NA_character_)
  if (length(m) >= 3L && nzchar(m[3])) {
    file.path(base_dir, paste0(m[3], ".sas"))
  } else {
    base_dir
  }
}

#' Enumerate include resolution candidates, source first
#'
#' An absolute target names exactly one location: it is never re-anchored onto
#' another root and its basename is never grafted onto a search root, so an
#' inaccessible absolute path cannot be silently satisfied by a same-named file
#' somewhere else. Relative targets are searched fileref binding first, then the
#' including file's own directory, then the project root, and only then the
#' configured fallback roots, which always receive the relative target unchanged.
#'
#' @param target Literal include target text.
#' @param including_file Path of the file containing the `%include`.
#' @param project_root Root directory of the project.
#' @param include_roots Character vector of configured fallback roots.
#' @param filerefs Named list of fileref -> path bindings; empty for quoted targets.
#' @return A list of candidates, each a list with `path` and `origin`.
#' @noRd
include_candidates <- function(target, including_file, project_root,
                               include_roots = character(), filerefs = list()) {
  target <- as.character(target)[1]
  if (is.na(target) || !nzchar(target)) return(list())

  include_roots <- as.character(include_roots %||% character())
  include_roots <- include_roots[!is.na(include_roots) & nzchar(include_roots)]

  if (is_abs_path(target)) {
    return(list(include_candidate(target, "absolute")))
  }

  including_dir <- dirname(as.character(including_file)[1])
  cands <- list()

  fileref_path <- include_fileref_target(target, filerefs)
  if (!is.na(fileref_path)) {
    if (is_abs_path(fileref_path)) {
      cands <- c(cands, list(include_candidate(fileref_path, "fileref")))
    } else {
      anchors <- c(including_dir, project_root, include_roots)
      cands <- c(cands, lapply(anchors, function(anchor) {
        include_candidate(file.path(anchor, fileref_path), "fileref")
      }))
    }
  }

  c(
    cands,
    list(
      include_candidate(file.path(including_dir, target), "including_dir"),
      include_candidate(file.path(project_root, target), "project_root")
    ),
    lapply(include_roots, function(r) {
      include_candidate(file.path(r, target), "configured_fallback")
    })
  )
}

#' Resolve a candidate path to the readable regular file it names
#'
#' Directories exist but cannot be read as SAS source, so they never satisfy an
#' include; the occurrence stays unresolved instead of failing at read time.
#'
#' A quoted include is written the way SAS resolved it -- case-insensitively on
#' Windows -- so on a case-sensitive filesystem the literal spelling can miss a
#' file that is genuinely there (`%include 'SETUP.sas'` against `setup.sas`).
#' When the exact spelling misses, a unique case-insensitive sibling in the
#' same directory satisfies the include, and its on-disk name is what resolves;
#' an ambiguous match (two siblings differing only by case) stays unresolved
#' rather than guessing.
#'
#' @param path A candidate path from [include_candidates()].
#' @return The existing file's path (in its on-disk spelling for a
#'   case-insensitive hit), or `NA_character_` when nothing satisfies it.
#' @noRd
include_candidate_file_path <- function(path) {
  if (is.na(path) || !nzchar(path)) return(NA_character_)
  if (file.exists(path)) {
    if (dir.exists(path)) return(NA_character_)
    return(path)
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) return(NA_character_)
  siblings <- list.files(dir, all.files = TRUE, full.names = FALSE)
  hits <- siblings[tolower(siblings) == tolower(basename(path))]
  if (length(hits) != 1L) return(NA_character_)
  resolved <- file.path(dir, hits)
  if (dir.exists(resolved)) return(NA_character_)
  resolved
}

#' Resolve one `%include` target, source first
#'
#' @inheritParams include_candidates
#' @return A list with `path`, `origin`, `status`, and `reason`.
#' @noRd
resolve_include_target <- function(target, including_file, project_root,
                                   include_roots = character(),
                                   filerefs = list()) {
  # Plan-mandated coarse test, kept as specified. Known false positive: a quoted
  # target under a directory whose name contains `&` or `%` -- 'R&D/setup.sas' --
  # is classified dynamic, never resolved, and raises the dynamic_include flag.
  # A quoted target is a literal path, so for that case the check could safely
  # narrow to `&` or `%` followed by an identifier character (the only forms that
  # actually begin a macro variable or macro call reference).
  dynamic <- grepl("[&%]", target)
  if (isTRUE(dynamic)) {
    return(list(path = NA_character_, origin = "none",
                status = "dynamic", reason = "dynamic_target"))
  }
  candidates <- include_candidates(
    target = target,
    including_file = including_file,
    project_root = project_root,
    include_roots = include_roots,
    filerefs = filerefs
  )
  resolved_paths <- vapply(candidates, function(cand) {
    include_candidate_file_path(cand$path)
  }, character(1))
  hit <- which(!is.na(resolved_paths))[1]
  if (is.na(hit)) {
    return(list(path = NA_character_, origin = "none",
                status = "unresolved", reason = "target_not_found"))
  }
  list(path = include_normalize_path(resolved_paths[[hit]]),
       origin = candidates[[hit]]$origin,
       status = "resolved", reason = NA_character_)
}

#' Empty occurrence table with the exact documented schema
#'
#' Assembled from [INCLUDE_OCCURRENCE_PROTOTYPE] in [INCLUDE_OCCURRENCE_COLUMNS]
#' order, so the schema is never restated as a literal.
#'
#' @return A zero-row tibble carrying [INCLUDE_OCCURRENCE_COLUMNS].
#' @noRd
empty_include_occurrences <- function() {
  include_occurrence_tibble(INCLUDE_OCCURRENCE_PROTOTYPE)
}

#' Assemble an occurrence table from named columns in the declared order
#'
#' The caller supplies columns by name; ordering and completeness are settled
#' here against [INCLUDE_OCCURRENCE_COLUMNS], and the two closed vocabularies
#' are checked so a typo'd status or origin cannot reach the contract that
#' downstream tasks consume.
#'
#' @param cols Named list of column vectors, one per [INCLUDE_OCCURRENCE_COLUMNS].
#' @return A tibble with exactly [INCLUDE_OCCURRENCE_COLUMNS], in that order.
#' @noRd
include_occurrence_tibble <- function(cols) {
  stopifnot(
    "occurrence columns must match INCLUDE_OCCURRENCE_COLUMNS exactly" =
      setequal(names(cols), INCLUDE_OCCURRENCE_COLUMNS) &&
        length(cols) == length(INCLUDE_OCCURRENCE_COLUMNS),
    "occurrence status outside INCLUDE_OCCURRENCE_STATUSES" =
      all(cols$status %in% INCLUDE_OCCURRENCE_STATUSES),
    "resolution origin outside INCLUDE_RESOLUTION_ORIGINS" =
      all(cols$resolution_origin %in% INCLUDE_RESOLUTION_ORIGINS)
  )
  tibble::as_tibble(cols[INCLUDE_OCCURRENCE_COLUMNS])
}

#' Combine per-file occurrence blocks into the project-level table
#'
#' [include_occurrence_tibble()] is called once per scanned file, so it can only
#' see collisions inside a single file. `occurrence_id` is the join key that
#' `include_graph$files$parent_occurrence_id` points at, and a duplicate would
#' fan a files-to-occurrences join out silently rather than fail. The guard sits
#' here, at the one point where every file's block is visible at once, so a
#' future change to the identity scheme cannot mint duplicates unnoticed.
#'
#' @param blocks List of per-file occurrence tibbles.
#' @return A tibble of every occurrence, with unique `occurrence_id`s.
#' @noRd
include_bind_occurrences <- function(blocks) {
  occ <- fast_bind(blocks, empty_include_occurrences())
  dup <- unique(occ$occurrence_id[duplicated(occ$occurrence_id)])
  if (length(dup)) {
    sites <- occ[occ$occurrence_id %in% dup, , drop = FALSE]
    cli::cli_abort(
      c("{.field occurrence_id} is not unique across the include graph.",
        "x" = "{length(dup)} id{?s} cover{?s/} more than one include site.",
        "i" = "Colliding site{?s}: {.val {paste0(sites$parent_file, ':', sites$line)}}."),
      class = "sas2r_include_occurrence_id_collision"
    )
  }
  occ
}

#' Directory every out-of-root include module is staged beneath
#' @noRd
INCLUDE_EXTERNAL_STAGED_DIR <- "_includes"

#' Rewrite one relative path so the file it names ends in `.R`
#'
#' A staged module holds generated R and must be named as R, whatever the
#' target was called in SAS. `%INCLUDE` targets are routinely `.inc`, `.txt`,
#' or extensionless, and a rewrite that only matched `.sas` left those modules
#' carrying the source file's own name -- which, for an in-place transpilation,
#' meant the generated module was written straight over the user's SAS source.
#'
#' The final extension of the *basename* is replaced, never a dot inside a
#' directory name, and a name that is nothing but an extension (`.inc`) keeps
#' its name and gains the suffix rather than collapsing to `.R`.
#'
#' @param rel_path A relative path, forward-slash separated.
#' @return The same path with its file component ending in `.R`.
#' @noRd
staged_r_module_path <- function(rel_path) {
  rel_path <- gsub("\\\\", "/", rel_path)
  dir <- dirname(rel_path)
  base <- basename(rel_path)
  stem <- sub("\\.[^.]+$", "", base)
  if (!nzchar(stem)) stem <- base
  out <- paste0(stem, ".R")
  if (identical(dir, ".") || !nzchar(dir)) out else paste0(dir, "/", out)
}

#' Canonical relative staged .R path for a source SAS file
#' @param src_file Source SAS file path.
#' @param project_dir Project root directory.
#' @return Relative staged .R path.
#' @noRd
canonical_rel_staged_path <- function(src_file, project_dir = ".") {
  if (is.null(src_file) || length(src_file) == 0L || is.na(src_file[1]) || !nzchar(src_file[1])) return(NA_character_)
  src_file <- src_file[1]
  base_dir <- if (!is.null(project_dir) && length(project_dir) > 0L && !is.na(project_dir[1]) && nzchar(project_dir[1])) project_dir[1] else ""
  clean_path <- function(p) gsub("//+", "/", sub("^/private/", "/", normalizePath(p, winslash = "/", mustWork = FALSE)))
  rel_path <- if (nzchar(base_dir)) {
    norm_base <- clean_path(base_dir)
    norm_f <- clean_path(src_file)
    prefix <- paste0(norm_base, "/")
    if (startsWith(norm_f, prefix)) {
      substring(norm_f, nchar(prefix) + 1L)
    } else {
      basename(src_file)
    }
  } else {
    basename(src_file)
  }
  staged_r_module_path(rel_path)
}


#' Relative staged `.R` module path for one scanned SAS file
#'
#' An included file that lies under the project root keeps its root-relative
#' path, so `inc/prep.sas` stages as `inc/prep.R` exactly as a program file of
#' the same location would -- [canonical_rel_staged_path()] and this function
#' agree there, and the manifest, the identity layer, and the emitted call site
#' therefore cannot disagree about where a module lives.
#'
#' A file *outside* the root has no root-relative path to keep, and its
#' basename alone is not an identity: a project including `../shared/setup.sas`
#' and `../vendor/setup.sas` would stage both as `setup.R` and silently lose
#' one. The out-of-root branch therefore interposes a directory named by a
#' digest of the file's canonical absolute path, which is distinct whenever the
#' files are distinct and stable for as long as the file stays put.
#'
#' The digest is of the canonical path, not of the file's bytes: two copies of
#' one library keep separate modules, and editing a file does not move its
#' module out from under the manifest.
#'
#' @param source_file Path of a scanned SAS file; it must exist.
#' @param project_root Root directory of the project.
#' @return A relative path with forward slashes, always safe to join onto the
#'   staged output directory.
#' @noRd
canonical_include_staged_path <- function(source_file, project_root) {
  source <- normalizePath(source_file, winslash = "/", mustWork = TRUE)
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (startsWith(source, prefix)) {
    return(staged_r_module_path(substring(source, nchar(prefix) + 1L)))
  }
  digest <- substr(migration_hash(source), 1L, 16L)
  file.path(INCLUDE_EXTERNAL_STAGED_DIR, digest,
            staged_r_module_path(basename(source)))
}

#' Empty physical-file table of the include graph
#'
#' @return A zero-row tibble of scanned files.
#' @noRd
empty_include_files <- function() {
  tibble::tibble(
    file = character(), canonical_file = character(), origin = character(),
    depth = integer(), parent_occurrence_id = character()
  )
}
