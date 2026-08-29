#' Schema version of the libref binding registry
#'
#' The version is the first field of the hashed identity key, so bumping it
#' re-mints every `binding_id` in every project. It therefore moves whenever
#' the identity key changes at all -- when the *meaning* of an effective
#' binding changes, and equally when only the **set of inputs** the key is
#' built from changes, because either one makes an id minted by an older
#' sas2r name something different from the same id minted by this one. An
#' earlier wording said it moves "only when the meaning of an effective
#' binding itself changes", and round 2 then changed the identity inputs
#' without a bump; the rule above is what the practice has to be, and is what
#' the three bumps so far have each answered to:
#'
#' * `1L` to `2L` -- round 2 replaced the defining site's raw provenance with
#'   anchor-frame keys and sentinel-ised the execution context.
#' * `2L` to `3L` -- round 3 anchors the identity on the **point of use** and
#'   drops the defining site's own coordinates. See [libref_binding_id()].
#' * `3L` to `4L` -- round 4 drops the execution context from the key
#'   altogether, so a caller who names one gets the same id as a caller who
#'   does not, and re-expresses the answer's path in the anchor frame instead
#'   of keying on its literal spelling. See [libref_binding_id()].
#'
#' Nothing persists a libref `binding_id` -- the schema-v3 approval ledger
#' binds source-unit identities and content hashes, never a libref binding --
#' so no stored approval is pinned to an older id and a re-mint strands
#' nothing.
#' @noRd
LIBREF_REGISTRY_VERSION <- 4L

#' Field separator inside a hashed identity key
#'
#' A control character no path, libref, or engine token can contain, so no two
#' different field splittings can paste to the same key.
#' @noRd
LIBREF_KEY_SEP <- "\u001f"

#' Engines whose `LIBNAME` path names a local directory sas2r can look at
#'
#' Everything outside this list -- a remote or database engine, or a
#' member-oriented transport file -- names something that is not a readable
#' directory, so its source binding is never "accessible" and configuration is
#' allowed to supply the location instead.
#' @noRd
LIBREF_LOCAL_ENGINES <- c("", "base", "v9", "v8", "v7", "v6", "sas7bdat")

#' Exact field order of an effective binding record
#'
#' The record is a contract: later work projects these bindings into generated
#' R and joins output evidence back onto them, so the field list is declared
#' once here rather than spelled out at each construction site.
#'
#' `status` is one of `"bound"`, `"conditionally_bound"`, `"unbound"`, or
#' `"ambiguous_libref"`. `"conditionally_bound"` means the only source binding
#' sits inside a macro definition, so it runs only if something calls that
#' macro: the location is reported but not established, and a consumer that
#' needs an established one must treat it as it treats `"unbound"`.
#'
#' `selected_path` is canonical but **unconfined**: a source `LIBNAME` may name
#' any absolute path, and a relative one is resolved against the project root
#' without being held inside it, so `'../../elsewhere'` resolves to a real
#' directory outside the project. Nothing in this file opens what it names --
#' resolution only stats -- so every consumer MUST confine `selected_path`
#' itself before reading anything under it.
#'
#' `context_truncated` marks a record resolved in an execution context whose
#' frame walk hit [LIBREF_MAX_CONTEXT_FRAMES]. See [libref_binding_at()].
#' @noRd
LIBREF_BINDING_FIELDS <- c(
  "binding_id", "libref", "root_program", "include_occurrence_id", "file",
  "line", "action", "source_path_expression", "source_path", "configured_path",
  "selected_path", "selection_origin", "status", "fallback_reason",
  "context_truncated"
)

#' Largest number of execution frames one root program may expand into
#'
#' A package constant, not configuration. Include expansion is per execution
#' context, so a file reached by several include chains is expanded once per
#' chain -- deliberately, since that is what makes a binding context-aware --
#' and a wide, deep include graph can therefore fan out geometrically. The cap
#' turns that into a reported truncation rather than an unbounded walk.
#' @noRd
LIBREF_MAX_CONTEXT_FRAMES <- 2000L

#' Zero-row prototype of the executed libref event table
#' @noRd
empty_libref_events <- function() {
  tibble::tibble(
    context_id = integer(), root_program = character(), frame_index = integer(),
    include_occurrence_id = character(), libref = character(),
    action = character(), engine = character(), path_expression = character(),
    file = character(), line = integer(), file_unit_id = integer(),
    conditional = logical(), key = list()
  )
}

#' Zero-row prototype of the execution frame table
#' @noRd
empty_libref_frames <- function() {
  tibble::tibble(
    context_id = integer(), root_program = character(), root_key = character(),
    frame_index = integer(), file = character(),
    canonical_key = character(), include_occurrence_id = character(),
    parent_frame = integer(), depth = integer(), key_prefix = list()
  )
}

#' Zero-row prototype of the per-file source libref event table
#' @noRd
empty_libref_source_events <- function() {
  tibble::tibble(
    file = character(), canonical_key = character(),
    libref = character(), action = character(), engine = character(),
    path_expression = character(), line = integer(), file_unit_id = integer(),
    conditional = logical(), intra = integer()
  )
}

#' Zero-row prototype of the `%include` site table the registry expands
#' @noRd
empty_libref_include_sites <- function() {
  tibble::tibble(
    occurrence_id = character(), parent_file = character(),
    file_unit_id = integer(), line = integer(), target_file = character(),
    status = character()
  )
}

#' Compare two execution-order keys
#'
#' A key is the chain of `(line, file-local unit)` coordinates from the top of
#' an execution context down to a statement, so lexicographic order over it is
#' execution order. When one key is a prefix of the other the two statements
#' cannot be separated -- a `%include` and a `LIBNAME` sharing one line of one
#' translation unit, or a point of use whose key deliberately stops at its line
#' -- and the comparison says so rather than inventing an order. That is the
#' same inclusive reading that lets `libname adam 'x'; set adam.adsl;` written
#' on a single line see its own binding.
#'
#' Reading a prefix as a tie makes this relation **non-transitive**, not merely
#' partial: with `A = (1,2,1,1)`, `B = (1,2,1,1,1,1)` and `C = (1,2,1,2)`, `A`
#' ties `B` and `B` ties `C` while `A` precedes `C`. The maxima loop in
#' [libref_visible_event()] therefore has no total order to lean on, and its
#' *set* of maxima can in principle depend on the order it walks the events.
#' The regime is narrow -- a `LIBNAME` and a `%include` sharing one line of one
#' translation unit, which only happens inside an open step or macro
#' definition -- and the walk order is fixed by scan order, so the answer is
#' deterministic for a given project. The tie is still reported as ambiguity
#' rather than resolved, which is the safe direction to fail in.
#'
#' @param a,b Integer key vectors.
#' @return `-1L` when `a` executes first, `1L` when `b` does, `0L` when neither
#'   precedes the other.
#' @noRd
libref_key_cmp <- function(a, b) {
  n <- min(length(a), length(b))
  for (i in seq_len(n)) {
    if (a[i] != b[i]) return(if (a[i] < b[i]) -1L else 1L)
  }
  0L
}

#' Stable identity for one effective libref binding
#'
#' A binding is a fact about a **point of use**: this libref, used here, names
#' that library. The identity is keyed on exactly that -- the place where the
#' question was asked, plus the answer -- and deliberately not on where the
#' `LIBNAME` that supplied the answer happened to execute.
#'
#' That is the whole of the round-3 change, and it is what makes the id
#' composition- and scan-mode-invariant. The defining site's coordinates are
#' not invariant: which statement wins, and which execution frame it won in,
#' both move when an unrelated program `%include`s the file, or when two root
#' programs make the same binding and the scan gains a second one. The point
#' of use moves under none of that -- it is a file and a line the caller
#' already holds, and `use_identity` re-expresses that file in the
#' configuration-derived anchor frame of [include_identity_anchors()], which
#' holds it still under a relocated checkout for every file that lies under an
#' anchor. The exceptions are that frame's own, and they are not restated
#' here: [include_identity_anchors()] documents the out-of-anchor file that is
#' named absolutely, and `includes.roots` is the remedy for it.
#'
#' Dropping the defining site is safe against collision here in a way the
#' earlier sentinel schemes were not. Sentinel-ising the site would have let
#' two libraries in different directories share one id; anchoring on the point
#' of use cannot, because one libref used at one place in one execution
#' resolves to exactly one location -- when it would resolve to two, the
#' answer is `ambiguous_libref`, which is itself part of the key. What is kept
#' from the answer is `action` and `path_identity`, so an edited `LIBNAME`
#' re-keys its binding instead of inheriting the id of the library it no
#' longer names.
#'
#' That is a property of the *point of use*, not a proof that `path_identity`
#' itself never collides two different directories onto one key -- it can, in
#' a narrow case qualified further down.
#'
#' `path_identity` is the answer's *location* re-expressed in the same anchor
#' frame `use_identity` uses, and only for a binding the source supplied: it
#' is [libref_anchor_identity()] of the selected directory. Round 3 keyed on
#' the literal source path expression instead, which made two spellings of one
#' directory -- `'data/adam'` in one root program and `'./data/adam'` in
#' another -- two identities for one binding, so adding a root program re-keyed
#' it. The anchor frame is the third exit round 3 missed: it keeps the re-key
#' on a genuinely edited `LIBNAME` (a different directory is a different
#' anchor key) without reading the spelling, and it holds the key still under
#' a relocated checkout for every selected directory that lies under an
#' anchor. The exceptions are that frame's own, and they are not restated
#' here: [include_identity_anchors()] documents the out-of-anchor file named
#' absolutely, and the last **Residual (accepted)** note below this doc block
#' documents the matching exposure for a selected directory reached by a
#' relative `LIBNAME` that escapes every anchor; `includes.roots` is the
#' remedy for both.
#'
#' A binding configuration supplied keeps the *failed* source expression in
#' this slot instead. A configured path is normalized against the
#' configuration file's directory, so folding the selected path in would put
#' machine state back into the identity; the expression is source content and
#' travels with the checkout. The asymmetry is deliberate and it is visible in
#' the key, because `selection_origin` is a field of it.
#'
#' The execution context is **not** an input, whether or not the caller named
#' one. Round 3 folded `root_identity`/`frame_occurrence_id` in for a named
#' call and sentinel-ised them otherwise, which made the same effective binding
#' answer to two ids depending on which *question* was asked: the lineage
#' column, which never names a context, could not be joined against a record
#' resolved by a caller that did -- silently, as a zero-match join rather than
#' an error. `binding_id` is now a fact about the binding alone. Nothing is
#' lost against collision from dropping the context: two contexts that
#' disagree at one point of use are `ambiguous_libref` to an unnamed caller,
#' and to named callers they disagree on the selected directory, which is
#' ordinarily `path_identity`. `root_program` and `include_occurrence_id` stay
#' on the record as displayed provenance.
#'
#' **Residual (accepted).** "Ordinarily" is doing work above: `path_identity`
#' is [libref_anchor_identity()] of the selected directory, and that lookup is
#' collision-free only up to the same sentinel-shadowing
#' [include_identity_anchors()] already documents -- a project directory
#' spelled like an anchor sentinel, `<include-root:1>` say, mints the key a
#' file actually reached through that configured anchor would also mint. Two
#' named-context resolutions with genuinely different `selected_path`s can
#' therefore mint the *same* `binding_id`. The backstop that closes this for
#' occurrence ids -- `sas_project()` asserting id uniqueness across every
#' scanned file -- does not reach it, because a library directory named by a
#' `LIBNAME` is never itself a scanned file. Contrived enough not to be a
#' trust issue in practice, and accepted on the same terms as
#' [include_identity_anchors()]'s own sentinel-shadowing residual rather than
#' closed differently here.
#'
#' **Residual (accepted).** The anchor frame is root-relative and
#' `sas_project()` roots it at the scanned directory, so scanning `proj`
#' recursively and scanning `proj/sub/p.sas` name one file `sub/p.sas` and
#' `p.sas` and mint two ids for one binding -- and `path_identity` moves with
#' the root for the same reason. This is not scan *mode* (one root, walked two
#' ways), which is invariant and pinned; it is a change of scan *root*. It is
#' inherent to any root-relative frame and is exactly the residual
#' [include_identity_anchors()] already accepts for occurrence ids, so it is
#' accepted here on the same terms rather than closed differently in two
#' places: pick one scan scope per project and keep it.
#'
#' **Residual (accepted).** `path_identity` is [libref_anchor_identity()] of
#' the selected directory, which falls back to the directory's absolute path
#' when it lies under no anchor -- see [include_identity_anchors()]. A source
#' `LIBNAME` that names a location outside every anchor by a *relative*
#' spelling, such as `libname adam '../shared/adam';` naming a directory
#' outside the project root, reaches exactly that fallback: two checkouts of
#' the byte-identical statement at two different absolute bases mint two
#' different `binding_id`s, because the fallback is the checkout's own
#' absolute path rather than the source text. This is the same out-of-anchor
#' exposure [include_identity_anchors()] already accepts for `use_identity`
#' and for occurrence ids, so it is accepted here on the same terms: naming
#' the library's parent directory in `includes.roots` makes it an anchor and
#' restores invariance, because [libref_anchor_identity()] reads the same
#' anchor list `use_identity` does.
#'
#' @param libref Library reference, already lower-cased.
#' @param status Binding status.
#' @param selection_origin Which authority supplied the location.
#' @param use_identity Anchor-frame key of the file the point of use sits in.
#' @param use_line Line the point of use sits on.
#' @param action `"assign"`, `"clear"`, or `NA`.
#' @param path_identity The answer's location in the anchor frame when the
#'   source supplied it, and the failed source path expression otherwise.
#' @return A character binding id.
#' @noRd
libref_binding_id <- function(libref, status, selection_origin,
                              use_identity = NA_character_,
                              use_line = NA_integer_,
                              action = NA_character_,
                              path_identity = NA_character_) {
  blank <- function(x) {
    if (length(x) != 1L || is.na(x)) "" else as.character(x)
  }
  key <- paste(
    LIBREF_REGISTRY_VERSION, blank(libref), blank(status),
    blank(selection_origin),
    blank(use_identity), blank(use_line), blank(action),
    blank(path_identity),
    sep = LIBREF_KEY_SEP
  )
  paste0("libref_", substr(migration_hash(key), 1L, 32L))
}

#' Assemble one effective binding record
#'
#' @param use_identity Anchor-frame key of the point of use's file, and
#'   `use_line` its line. These are the identity's anchor and are always the
#'   *caller's* coordinates, never the defining statement's -- the record's own
#'   `file` and `line` display the defining statement, which is a different
#'   place and is not an identity input. See [libref_binding_id()].
#' @param path_identity The key's path field: it is the *selected* directory
#'   in the anchor frame when the source supplied the location, and the failed
#'   source path expression when configuration did. Like
#'   `use_identity`/`use_line` (see [libref_binding_id()]), it has no matching
#'   field in [LIBREF_BINDING_FIELDS], so a `binding_id` cannot be recomputed
#'   or audited from a record's fifteen fields alone -- a later task holding
#'   only a record is missing this input, exactly as it is missing those two.
#'   Every construction site must pass it deliberately -- the default is the
#'   right answer only for a record that names no location at all, such as
#'   `ambiguous_libref`. See [libref_binding_id()].
#' @return A named list carrying exactly [LIBREF_BINDING_FIELDS], in order.
#' @noRd
libref_binding_record <- function(libref, status, selection_origin,
                                  root_program = NA_character_,
                                  include_occurrence_id = NA_character_,
                                  file = NA_character_, line = NA_integer_,
                                  action = NA_character_,
                                  source_path_expression = NA_character_,
                                  source_path = NA_character_,
                                  configured_path = NA_character_,
                                  selected_path = NA_character_,
                                  fallback_reason = NA_character_,
                                  context_truncated = FALSE,
                                  use_identity = NA_character_,
                                  use_line = NA_integer_,
                                  path_identity = NA_character_) {
  record <- list(
    binding_id = libref_binding_id(
      libref = libref, status = status, selection_origin = selection_origin,
      use_identity = use_identity, use_line = use_line, action = action,
      path_identity = path_identity
    ),
    libref = as.character(libref),
    root_program = as.character(root_program),
    include_occurrence_id = as.character(include_occurrence_id),
    file = as.character(file),
    line = as.integer(line),
    action = as.character(action),
    source_path_expression = as.character(source_path_expression),
    source_path = as.character(source_path),
    configured_path = as.character(configured_path),
    selected_path = as.character(selected_path),
    selection_origin = as.character(selection_origin),
    status = as.character(status),
    fallback_reason = as.character(fallback_reason),
    # Deliberately not an input to `binding_id`: truncation is a fact about
    # this scan, not about the binding, and folding it in would make the same
    # statement mint two ids depending on how wide the include graph was.
    context_truncated = isTRUE(context_truncated)
  )
  record[LIBREF_BINDING_FIELDS]
}

#' Decide whether a source `LIBNAME` names a directory sas2r can see
#'
#' Accessibility is decided at the *directory* binding and nowhere else. A
#' readable, searchable, statically resolved directory wins even when the
#' dataset a later statement asks for is not in it: an absent member is a data
#' problem, not a reason to relocate the whole library onto whatever
#' configuration happens to say.
#'
#' Nothing here reads the directory. The path is canonicalized and then only
#' stat-ed, so resolution never opens library content.
#'
#' @param path_expression Literal path text from the `LIBNAME` statement.
#' @param engine Engine token from the statement, possibly `""`.
#' @param project_root Base for a relative source path.
#' @return A list with `ok`, `path` (canonical, or `NA`), and `reason`.
#' @noRd
libref_source_path_state <- function(path_expression, engine, project_root) {
  fail <- function(reason, path = NA_character_) {
    list(ok = FALSE, path = path, reason = reason)
  }
  if (length(path_expression) != 1L || is.na(path_expression) ||
      !nzchar(path_expression)) {
    return(fail("source_path_unavailable"))
  }
  # A macro variable or macro call is not a path until SAS expands it, and
  # sas2r never expands one, so the location is simply not known here.
  if (grepl("[&%]", path_expression)) {
    return(fail("source_path_unresolved_macro"))
  }
  # A source path is SAS text, not configuration, so it uses the same
  # absoluteness test `%include` targets use: `~` is a shell convention SAS
  # does not promise, and reading it as anchored would silently point a library
  # at the invoking user's home directory.
  raw <- if (is_abs_path(path_expression)) {
    path_expression
  } else {
    file.path(project_root, path_expression)
  }
  path <- normalizePath(raw, winslash = "/", mustWork = FALSE)
  # A non-local engine cannot supply a directory, but it did name something,
  # and dropping that text would make the downgrade look like it came from
  # nowhere. So the location is still recorded -- resolved as a path when a
  # file or directory is actually there, as for a member-oriented `xport`
  # transport file, and left `NA` when the engine names no filesystem object
  # at all, as for a database schema.
  if (!tolower(engine %||% "") %in% LIBREF_LOCAL_ENGINES) {
    return(fail("source_engine_unsupported",
                if (file.exists(path)) path else NA_character_))
  }
  if (!dir.exists(path)) return(fail("source_path_unavailable", path))
  readable <- file.access(path, 4L) == 0L
  searchable <- .Platform$OS.type != "unix" || file.access(path, 1L) == 0L
  if (!readable || !searchable) return(fail("source_path_unreadable", path))
  list(ok = TRUE, path = path, reason = NA_character_)
}

#' Bind a list of single-row lists into a tibble with a fixed prototype
#'
#' [fast_bind()] cannot be reused here: these tables carry a list column of
#' execution keys, which its `unlist()` would flatten into one long vector.
#'
#' @param rows List of named lists, each supplying every prototype column.
#' @param prototype Zero-row tibble declaring column order and types.
#' @return A tibble.
#' @noRd
libref_bind_rows <- function(rows, prototype) {
  if (!length(rows)) return(prototype)
  cols <- names(prototype)
  out <- lapply(cols, function(nm) {
    values <- lapply(rows, `[[`, nm)
    proto <- prototype[[nm]]
    if (is.list(proto)) return(values)
    flat <- unlist(values, use.names = FALSE)
    if (is.integer(proto)) as.integer(flat)
    else if (is.logical(proto)) as.logical(flat)
    else as.character(flat)
  })
  names(out) <- cols
  tibble::as_tibble(out)
}

#' Build the per-root-program libref execution registry
#'
#' SAS binds a libref at a point in an execution, not once per project, so the
#' registry is a set of execution contexts -- one per root program, each with
#' the configured `autoexec` files as its prologue -- and never one global
#' last-writer map. An included file is expanded once per include occurrence
#' that reaches it, which is what lets one physical file hold two different
#' `adam` bindings under two different root programs.
#'
#' @param source_events Per-file libref events, shaped by
#'   [empty_libref_source_events()].
#' @param include_sites `%include` sites, shaped by
#'   [empty_libref_include_sites()].
#' @param scanned_files Every physically scanned file.
#' @param root_programs Root program files, in scan order.
#' @param env_files Configured `autoexec` files, in configured order.
#' @param libraries Normalized `config$libraries`.
#' @param project_root Project root directory.
#' @param anchors Identity anchors from [include_identity_anchors()].
#' @return A `sas2r_libref_registry` list.
#' @noRd
build_libref_registry <- function(source_events, include_sites, scanned_files,
                                  root_programs, env_files, libraries,
                                  project_root, anchors) {
  scanned_keys <- include_scan_key(scanned_files)
  sites <- include_sites
  if (nrow(sites) > 0L) {
    sites <- sites[sites$status == "resolved" & !is.na(sites$target_file), ]
  }
  if (nrow(sites) > 0L) {
    sites$parent_key <- include_scan_key(sites$parent_file)
    sites$target_key <- include_scan_key(sites$target_file)
    sites <- sites[sites$target_key %in% scanned_keys, ]
  }
  if (nrow(sites) > 0L) {
    sites <- sites[order(sites$parent_key, sites$line, sites$file_unit_id), ]
  }
  sites_by_parent <- if (nrow(sites) > 0L) {
    split(seq_len(nrow(sites)), sites$parent_key)
  } else {
    list()
  }
  events_by_file <- if (nrow(source_events) > 0L) {
    split(seq_len(nrow(source_events)), source_events$canonical_key)
  } else {
    list()
  }

  frames <- list()
  events <- list()
  truncated <- character()

  expand <- function(file, occurrence_id, parent_frame, key_prefix, depth,
                     chain, context) {
    if (length(frames) >= context$frame_budget) return(FALSE)
    canonical <- include_scan_key(file)
    frame_index <- length(frames) + 1L
    frames[[frame_index]] <<- list(
      context_id = context$id, root_program = context$root_program,
      root_key = context$root_key,
      frame_index = frame_index, file = file, canonical_key = canonical,
      include_occurrence_id = occurrence_id, parent_frame = parent_frame,
      depth = depth, key_prefix = as.integer(key_prefix)
    )
    for (i in events_by_file[[canonical]] %||% integer()) {
      events[[length(events) + 1L]] <<- list(
        context_id = context$id, root_program = context$root_program,
        frame_index = frame_index, include_occurrence_id = occurrence_id,
        libref = source_events$libref[i], action = source_events$action[i],
        engine = source_events$engine[i],
        path_expression = source_events$path_expression[i],
        file = source_events$file[i], line = source_events$line[i],
        file_unit_id = source_events$file_unit_id[i],
        conditional = isTRUE(source_events$conditional[i]),
        key = as.integer(c(key_prefix, source_events$line[i],
                           source_events$file_unit_id[i],
                           source_events$intra[i]))
      )
    }
    complete <- TRUE
    for (s in sites_by_parent[[canonical]] %||% integer()) {
      target_key <- sites$target_key[s]
      # The scanner reads each physical file once, so its own cycle and depth
      # verdicts describe whichever chain reached the file first. This walk
      # follows a different chain per context, so it re-tests both here.
      if (target_key %in% chain) next
      if (depth + 1L > INCLUDE_MAX_DEPTH) next
      complete <- expand(
        file = sites$target_file[s], occurrence_id = sites$occurrence_id[s],
        parent_frame = frame_index,
        key_prefix = c(key_prefix, sites$line[s], sites$file_unit_id[s]),
        depth = depth + 1L, chain = c(chain, target_key), context = context
      ) && complete
    }
    complete
  }

  add_context <- function(id, root_program) {
    context <- list(
      id = id, root_program = root_program,
      root_key = if (is.na(root_program)) NA_character_ else
        include_scan_key(root_program),
      frame_budget = length(frames) + LIBREF_MAX_CONTEXT_FRAMES
    )
    complete <- TRUE
    rank <- 0L
    top_level <- c(env_files, if (!is.na(root_program)) root_program)
    for (f in top_level) {
      rank <- rank + 1L
      complete <- expand(
        file = f, occurrence_id = NA_character_, parent_frame = NA_integer_,
        key_prefix = rank, depth = 0L, chain = include_scan_key(f),
        context = context
      ) && complete
    }
    if (!complete) {
      truncated[[length(truncated) + 1L]] <<-
        if (is.na(root_program)) "<environment>" else root_program
    }
  }

  roots <- as.character(root_programs %||% character())
  if (!length(roots) && length(env_files)) {
    # A project made only of environment files still executes something, so the
    # prologue becomes a context of its own rather than a hole in the registry.
    add_context(1L, NA_character_)
  }
  for (i in seq_along(roots)) add_context(i, roots[i])

  structure(list(
    version = LIBREF_REGISTRY_VERSION,
    frames = libref_bind_rows(frames, empty_libref_frames()),
    events = libref_bind_rows(events, empty_libref_events()),
    libraries = libraries %||% list(),
    project_root = project_root,
    anchors = anchors,
    # Kept so a zero-frame resolution can tell "the cap may have dropped this
    # file" from "the scan never saw this file" -- see
    # [libref_context_truncated()].
    scanned_keys = scanned_keys,
    truncated_contexts = as.character(truncated)
  ), class = "sas2r_libref_registry")
}

#' Anchor-frame key for a path, tolerating a path that names nothing
#'
#' Both of a binding identity's path coordinates go through here: the file the
#' point of use sits in, and -- for a source-supplied binding -- the directory
#' the answer selected. Both are re-expressed in the machine-independent frame
#' of [include_identity_anchors()], and using one function for both is what
#' makes them comparable. See [libref_binding_id()].
#'
#' @param file Path of the file or directory.
#' @param anchors Anchor list from [include_identity_anchors()].
#' @return A character identity key, or `NA_character_`.
#' @noRd
libref_anchor_identity <- function(file, anchors) {
  if (length(file) != 1L || is.na(file) || !nzchar(file)) return(NA_character_)
  include_identity_parent(file, anchors)
}

#' Select the execution frames a point of use could sit in
#'
#' @inheritParams resolve_libref_at
#' @return The matching rows of `registry$frames`.
#' @noRd
libref_matching_frames <- function(registry, file, root_program, occurrence_id) {
  frames <- registry$frames
  if (nrow(frames) == 0L) return(frames)
  keep <- frames$canonical_key == include_scan_key(file)
  if (!is.null(root_program)) {
    wanted <- include_scan_key(as.character(root_program)[1])
    keep <- keep & !is.na(frames$root_key) & frames$root_key == wanted
  }
  if (!is.null(occurrence_id)) {
    wanted <- as.character(occurrence_id)[1]
    keep <- keep & !is.na(frames$include_occurrence_id) &
      frames$include_occurrence_id == wanted
  }
  frames[keep, ]
}

#' Candidate event rows for one libref, grouped by execution context
#'
#' Which events can bind a libref depends on the libref and on nothing else,
#' so the filter runs once per point of use rather than once per execution
#' frame -- a file reached by a wide include graph has many frames and they all
#' share this answer.
#'
#' `libname _all_ clear;` unbinds every libref at once, so it is an event for
#' this libref too even though it never names it.
#'
#' @param events `registry$events`.
#' @param libref Lower-cased libref.
#' @return A named list of integer row-index vectors, keyed by `context_id`.
#' @noRd
libref_candidate_rows <- function(events, libref) {
  if (nrow(events) == 0L) return(list())
  rows <- which(events$libref == libref |
                  (events$libref == "_all_" & events$action == "clear"))
  if (!length(rows)) return(list())
  split(rows, events$context_id[rows])
}

#' One executed libref event, as a plain list of scalars
#'
#' Deliberately not a one-row tibble: a point of use in a widely included file
#' resolves once per execution frame, and `[.tbl_df` on each of them is the
#' whole cost of the walk.
#'
#' @param events `registry$events`.
#' @param i Row index.
#' @return A named list carrying the fields a binding record is built from.
#' @noRd
libref_event_at <- function(events, i) {
  list(
    include_occurrence_id = events$include_occurrence_id[i],
    file = events$file[i], line = events$line[i],
    action = events$action[i], engine = events$engine[i],
    path_expression = events$path_expression[i],
    file_unit_id = events$file_unit_id[i],
    conditional = events$conditional[i]
  )
}

#' Latest unambiguous source event visible at a point of use
#'
#' A `LIBNAME` inside a macro definition runs only if something calls that
#' macro, so it does not *on its own* establish a binding -- and by the same
#' reading it must not *un*-establish one either. Selection therefore runs over
#' the unconditional events alone whenever any are visible, and drops to the
#' conditional ones only when nothing unconditional is. Without that rule a
#' macro-def `LIBNAME` that is merely later in the file would win selection and
#' then downgrade the answer, so an accessible unconditional binding written
#' above it would lose its own point of use -- exactly the contract
#' [resolve_libref_at()] exists to keep.
#'
#' @param events `registry$events`.
#' @param rows Candidate row indices for this libref in this context.
#' @param key_prefix Execution-order key prefix of the frame.
#' @param line Line of the point of use.
#' @return A list with `event` (from [libref_event_at()], or `NULL`),
#'   `ambiguous`, and `had_assign` -- whether an *establishing* assignment for
#'   this libref was visible, which is what separates "cleared here" from
#'   "never bound". Conditional assignments do not count: a `LIBNAME` in a
#'   macro definition establishes nothing, so a later clear had nothing of its
#'   to take away and must not report that it did.
#' @noRd
libref_visible_event <- function(events, rows, key_prefix, line) {
  none <- list(event = NULL, ambiguous = FALSE, had_assign = FALSE)
  if (!length(rows)) return(none)
  keys <- events$key
  point_key <- as.integer(c(key_prefix, line))
  rows <- rows[vapply(rows,
                      function(i) libref_key_cmp(keys[[i]], point_key) <= 0L,
                      logical(1))]
  if (!length(rows)) return(none)
  had_assign <- any(events$action[rows] == "assign" &
                      !events$conditional[rows])
  # Conditional events are non-establishing, so they are not even candidates
  # while an unconditional one is visible.
  pool <- rows[!events$conditional[rows]]
  if (!length(pool)) pool <- rows
  # Maxima under a non-transitive relation -- see libref_key_cmp(). Ties are
  # accumulated rather than broken, so an inseparable pair leaves two maxima
  # and is reported as ambiguity below instead of being silently ordered.
  maxima <- integer()
  for (i in pool) {
    if (!length(maxima)) { maxima <- i; next }
    cmps <- vapply(maxima,
                   function(j) libref_key_cmp(keys[[i]], keys[[j]]),
                   integer(1))
    if (all(cmps > 0L)) maxima <- i
    else if (any(cmps < 0L)) next
    else maxima <- c(maxima, i)
  }
  # `conditional` is part of the effect: two tied statements that differ only
  # in whether a macro definition encloses them are not the same binding, so
  # the tie must be reported rather than settled by whichever came first. The
  # pool above is homogeneous in `conditional`, so this is a guard on that
  # invariant rather than a case reachable today.
  effects <- unique(paste(events$action[maxima], events$engine[maxima],
                          events$path_expression[maxima],
                          events$conditional[maxima],
                          sep = LIBREF_KEY_SEP))
  list(event = libref_event_at(events, maxima[1]),
       ambiguous = length(effects) > 1L,
       had_assign = had_assign)
}

#' Resolve the effective binding of a libref without raising for ambiguity
#'
#' `resolve_libref_at()` is the caller-facing wrapper that turns an ambiguous
#' record into a condition; lineage annotation records the status instead.
#'
#' @inheritParams resolve_libref_at
#' @return An effective binding record.
#' @noRd
libref_binding_at <- function(registry, libref, file, line,
                              root_program = NULL, occurrence_id = NULL) {
  if (!inherits(registry, "sas2r_libref_registry")) {
    cli::cli_abort("{.arg registry} must be a sas2r libref registry.",
                   class = "sas2r_libref_registry_error")
  }
  libref <- tolower(as.character(libref)[1])
  line <- as.integer(line)[1]
  file <- as.character(file)[1]
  # A point of use is a place in an execution. Without all three coordinates
  # there is no place to resolve at, and quietly picking the newest binding
  # anywhere would be exactly the global last-writer answer this registry
  # exists to avoid.
  if (is.na(libref) || !nzchar(libref) || is.na(file) || !nzchar(file) ||
      is.na(line)) {
    cli::cli_abort(
      c("A point of use needs a libref, a file, and a line.",
        "x" = "Got libref {.val {libref}} in {.val {file}} at line {.val {line}}."),
      class = "sas2r_libref_registry_error"
    )
  }
  configured <- registry$libraries[[libref]]
  configured_path <- if (is.null(configured)) NA_character_ else configured$path
  named_root <- if (is.null(root_program)) NA_character_ else
    as.character(root_program)[1]

  frames <- libref_matching_frames(registry, file, root_program, occurrence_id)
  truncated <- libref_context_truncated(registry, frames, file, root_program)
  # The identity's anchor, computed once from the caller's file rather than
  # once per frame. Every frame for this point of use is a frame *of this
  # file*: `libref_matching_frames()` selects on canonical equality, and
  # `include_normalize_path()` collapses `./`, case, and symlink routes, so a
  # per-frame lookup would agree with this one on every frame and only cost a
  # repeat of it.
  use_identity <- libref_anchor_identity(file, registry$anchors)

  if (nrow(frames) == 0L) {
    return(libref_configured_record(
      libref = libref, reason = "no_source_binding",
      configured_path = configured_path, root_program = named_root,
      context_truncated = truncated,
      use_identity = use_identity, use_line = line
    ))
  }

  # Frame fields are pulled out as plain vectors and the event table is
  # filtered once, so resolving every frame costs one pass over the candidate
  # rows rather than a tibble row-subset per frame.
  by_context <- libref_candidate_rows(registry$events, libref)
  context_ids <- as.character(frames$context_id)
  frame_files <- frames$file
  frame_roots <- frames$root_program
  frame_prefixes <- frames$key_prefix
  # Ambiguity is a *disagreement*, not a frame count. One physical file
  # commonly executes in several contexts for reasons that have nothing to do
  # with the libref -- most ordinarily because a directory scan globs every
  # `.sas` file, so an included file is a root program as well and is expanded
  # twice. The harm the ambiguity guard exists to prevent is borrowing one
  # program's binding for another, and that cannot happen when every context
  # lands on the same effect. Only the effect is compared.
  #
  # The frames are resolved one at a time and the walk stops at the first
  # disagreement: the verdict cannot change once two contexts differ, and the
  # per-context cap means a wide project's total frame count scales as
  # `n_roots x LIBREF_MAX_CONTEXT_FRAMES`.
  first <- NULL
  first_effect <- NULL
  for (i in seq_len(nrow(frames))) {
    record <- libref_binding_in_frame(
      registry, libref, line,
      rows = by_context[[context_ids[i]]] %||% integer(),
      key_prefix = frame_prefixes[[i]], frame_file = frame_files[i],
      context_root = frame_roots[i],
      configured_path = configured_path, truncated = truncated,
      use_identity = use_identity
    )
    effect <- paste(record$selection_origin, record$status,
                    record$selected_path, sep = LIBREF_KEY_SEP)
    if (is.null(first)) {
      first <- record
      first_effect <- effect
      next
    }
    if (!identical(effect, first_effect)) {
      return(libref_binding_record(
        libref = libref, status = "ambiguous_libref",
        selection_origin = "none",
        root_program = named_root, file = file, line = line,
        configured_path = configured_path,
        fallback_reason = "multiple_execution_contexts",
        context_truncated = truncated,
        use_identity = use_identity, use_line = line
      ))
    }
  }
  # The contexts agree, so the effect is context-free. The *provenance* is not
  # -- two root programs can make the same binding with their own `LIBNAME`
  # statements -- and the record keeps the first context's, in registry order.
  # That is why the defining site's coordinates are not identity inputs: the
  # displayed provenance may move with composition, and the id must not. See
  # [libref_binding_id()].
  first
}

#' Resolve one point of use inside a single execution frame
#'
#' @param rows Candidate event rows for this libref in this frame's context.
#' @param key_prefix Execution-order key prefix of the frame.
#' @param frame_file File the frame expanded.
#' @param context_root Root program of the frame's execution context.
#' @param configured_path Configured location for this libref, or `NA`.
#' @param truncated Whether this execution context was truncated.
#' @param use_identity Anchor-frame key of the point of use's file, from the
#'   caller's own coordinates -- see [libref_binding_record()].
#' @return An effective binding record.
#' @noRd
libref_binding_in_frame <- function(registry, libref, line, rows, key_prefix,
                                    frame_file, context_root,
                                    configured_path, truncated,
                                    use_identity = NA_character_) {
  configured_record <- function(reason, event = NULL,
                                source_path = NA_character_) {
    libref_configured_record(
      libref = libref, reason = reason, configured_path = configured_path,
      root_program = context_root,
      event = event, source_path = source_path, context_truncated = truncated,
      use_identity = use_identity, use_line = line
    )
  }

  found <- libref_visible_event(registry$events, rows, key_prefix, line)
  if (found$ambiguous) {
    return(libref_binding_record(
      libref = libref, status = "ambiguous_libref", selection_origin = "none",
      root_program = context_root, file = frame_file, line = line,
      configured_path = configured_path,
      fallback_reason = "multiple_source_bindings",
      context_truncated = truncated,
      use_identity = use_identity, use_line = line
    ))
  }
  selected <- found$event
  if (is.null(selected)) {
    return(configured_record("no_source_binding"))
  }
  if (identical(selected$action, "clear")) {
    # A clear only *unbinds* something that was bound. With no assignment
    # anywhere in front of it there was never a source binding to lose, and
    # saying "cleared" would describe an event that did not happen.
    return(configured_record(
      reason = if (isTRUE(found$had_assign)) "source_binding_cleared"
               else "no_source_binding",
      event = selected
    ))
  }

  state <- libref_source_path_state(selected$path_expression, selected$engine,
                                    registry$project_root)
  # The key's path field for a source-supplied location: the directory the
  # statement actually reached, in the same anchor frame `use_identity` uses.
  # Computed here rather than at each record site so the two source-supplied
  # statuses cannot drift apart. See [libref_binding_id()].
  path_identity <- if (isTRUE(state$ok)) {
    libref_anchor_identity(state$path, registry$anchors)
  } else {
    NA_character_
  }
  # A `LIBNAME` inside a macro definition runs only if something calls that
  # macro, and sas2r does no macro reachability analysis, so the statement is
  # a *conditional* event: it reports a location without establishing one.
  # That is not enough to discard an explicit configured path, which is why
  # the plan's fallback list already names unresolved macros. Nothing is
  # thrown away -- what the macro body said stays in `source_path_expression`
  # and `source_path` either way.
  if (isTRUE(selected$conditional)) {
    # An unusable path is reported under its own reason first. Conditionality
    # and unusability are both true of such a statement, and the specific one
    # is the more useful: "the directory it names is not there" tells a reader
    # what to fix, while "it sits in a macro" would leave them looking for a
    # caller that could never have helped.
    if (!state$ok) {
      return(configured_record(state$reason, event = selected,
                               source_path = state$path))
    }
    if (!is.na(configured_path)) {
      return(configured_record("source_binding_conditional", event = selected,
                               source_path = state$path))
    }
    # With nothing configured the conditional binding is the only location on
    # offer, so it is reported -- under a status that says it is not
    # established.
    return(libref_binding_record(
      libref = libref, status = "conditionally_bound",
      selection_origin = "source", root_program = context_root,
      include_occurrence_id = selected$include_occurrence_id,
      file = selected$file, line = selected$line, action = selected$action,
      source_path_expression = selected$path_expression,
      source_path = state$path, configured_path = configured_path,
      selected_path = state$path,
      fallback_reason = "source_binding_conditional",
      context_truncated = truncated,
      use_identity = use_identity, use_line = line,
      path_identity = path_identity
    ))
  }

  if (!state$ok) {
    return(configured_record(state$reason, event = selected,
                             source_path = state$path))
  }
  libref_binding_record(
    libref = libref, status = "bound", selection_origin = "source",
    root_program = context_root,
    include_occurrence_id = selected$include_occurrence_id,
    file = selected$file, line = selected$line, action = selected$action,
    source_path_expression = selected$path_expression,
    source_path = state$path, configured_path = configured_path,
    selected_path = state$path, context_truncated = truncated,
    use_identity = use_identity, use_line = line,
    path_identity = path_identity
  )
}

#' Whether the frames a point of use resolves in came from a truncated walk
#'
#' Truncation drops execution frames, and a dropped frame silently changes the
#' answer: a file that really executes twice can look like it executes once,
#' which changes `status`, `selection_origin`, and therefore `binding_id`. The
#' registry cannot avoid the cap, so it marks the records the cap could have
#' reached instead of leaving them indistinguishable from complete ones.
#'
#' Zero matching frames is the awkward case: a missing frame is exactly what
#' truncation produces, but it is also what an unreachable file produces, and
#' marking every zero-frame resolution would tar files no truncated context
#' could ever have contained. Two things narrow it. A caller who named a root
#' program is asking about that context alone, so only that context's
#' truncation is relevant. And a file the scan never saw could not have become
#' a frame under any cap, so truncation cannot explain its absence. What is
#' left -- a scanned file that no complete context reached -- is genuinely
#' indistinguishable from a dropped frame, and stays marked.
#'
#' @param registry A libref registry.
#' @param frames The matching rows of `registry$frames`.
#' @param file File of the point of use.
#' @param root_program Root program the caller named, or `NULL`.
#' @return `TRUE` when a truncated context could have shaped this resolution.
#' @noRd
libref_context_truncated <- function(registry, frames, file = NA_character_,
                                     root_program = NULL) {
  truncated <- as.character(registry$truncated_contexts %||% character())
  if (!length(truncated)) return(FALSE)
  keys <- vapply(truncated, function(ctx) {
    if (identical(ctx, "<environment>")) ctx else include_scan_key(ctx)
  }, character(1), USE.NAMES = FALSE)
  if (nrow(frames) == 0L) {
    if (!is.null(root_program)) {
      return(include_scan_key(as.character(root_program)[1]) %in% keys)
    }
    scanned <- as.character(registry$scanned_keys %||% character())
    return(include_scan_key(file) %in% scanned)
  }
  frame_keys <- ifelse(is.na(frames$root_key), "<environment>", frames$root_key)
  any(frame_keys %in% keys)
}

#' Librefs whose *effective* binding names an engine sas2r cannot look at
#'
#' The flag says a libref quietly ended up unbound, so it has to be decided on
#' the binding that is actually in force at a point of use, not on any event in
#' the file. `libname adam xport 'x.xpt'; libname adam '/dir';` names an
#' unsupported engine and then replaces it: the effective binding is the local
#' directory, nothing was downgraded, and there is nothing to report.
#'
#' The bindings are not resolved here. [attach_lineage_bindings()] has already
#' resolved exactly one record per distinct point of use, and those records are
#' what this reads -- so there is one resolution pass over a project, not two,
#' and one definition of "distinct point of use" rather than two copies of it.
#' Events still supply the *candidates*, which keeps the common project -- no
#' unsupported engine anywhere -- at one vectorised pass and no record walk.
#'
#' **Known gap.** A libref whose only `LIBNAME` names an unsupported engine and
#' which *nothing ever references* is reported by nothing at all: it has no
#' point of use, so it has no effective binding to flag, and `libref_undeclared`
#' does not fire either because the program did declare it. That is a real loss
#' of information, not merely a narrowing of a noisy flag -- round 1 traded it
#' for the false positives that flagging events rather than bindings produced.
#' Closing it needs a report keyed on declarations rather than on points of use,
#' which is a different flag from this one.
#'
#' @param registry A libref registry.
#' @param records Effective binding records, one per distinct point of use,
#'   from [attach_lineage_bindings()].
#' @return Character vector of `"<libref>: <engine>"` details, possibly empty.
#' @noRd
libref_unsupported_engine_details <- function(registry, records = list()) {
  ev <- registry$events
  if (nrow(ev) == 0L || !length(records)) return(character())
  configured <- names(registry$libraries %||% list())
  keep <- ev$action == "assign" & !ev$conditional &
    !tolower(ev$engine) %in% LIBREF_LOCAL_ENGINES &
    !ev$libref %in% configured
  if (!any(keep)) return(character())
  candidates <- unique(ev$libref[keep])

  details <- character()
  for (record in records) {
    if (!record$libref %in% candidates) next
    if (!identical(record$fallback_reason, "source_engine_unsupported")) next
    # The record carries no engine token -- LIBREF_BINDING_FIELDS is a fixed
    # contract -- so the defining event is looked up by the provenance the
    # record does carry. Every field matched here is copied onto the record
    # from the very row being sought, so a live record cannot miss; the guard
    # is for a future provenance change, and it degrades to silence rather
    # than to a flag reading "adam: NA", which names no engine and tells a
    # reader nothing.
    hit <- which(ev$libref == record$libref & ev$file == record$file &
                   ev$line == record$line & ev$action == "assign" &
                   ev$path_expression == record$source_path_expression)
    if (!length(hit)) next
    details <- c(details, paste0(record$libref, ": ", ev$engine[hit[1]]))
  }
  unique(details)
}

#' Build the record for a binding configuration had to supply, or could not
#'
#' @param reason Why the source binding did not supply the location.
#' @param event The source event that failed, or `NULL` when there was none.
#'   It supplies the record's *displayed* provenance only, plus the one
#'   identity input a configured binding keeps -- the failed path expression,
#'   which the key takes because the *selected* path here is a configured one
#'   and so machine-dependent. See [libref_binding_id()]. Everything else in
#'   the identity is `use_identity`/`use_line` -- see
#'   [libref_binding_record()].
#' @noRd
libref_configured_record <- function(libref, reason, configured_path,
                                     root_program,
                                     event = NULL,
                                     source_path = NA_character_,
                                     context_truncated = FALSE,
                                     use_identity = NA_character_,
                                     use_line = NA_integer_) {
  has_config <- !is.na(configured_path)
  libref_binding_record(
    libref = libref,
    status = if (has_config) "bound" else "unbound",
    selection_origin = if (has_config) "configured_fallback" else "none",
    root_program = root_program,
    include_occurrence_id = if (is.null(event)) NA_character_ else
      event$include_occurrence_id,
    file = if (is.null(event)) NA_character_ else event$file,
    line = if (is.null(event)) NA_integer_ else event$line,
    action = if (is.null(event)) NA_character_ else event$action,
    source_path_expression = if (is.null(event)) NA_character_ else
      event$path_expression,
    source_path = source_path,
    configured_path = configured_path,
    selected_path = if (has_config) configured_path else NA_character_,
    fallback_reason = reason,
    context_truncated = context_truncated,
    use_identity = use_identity, use_line = use_line,
    path_identity = if (is.null(event)) NA_character_ else
      event$path_expression
  )
}

#' Attach the effective binding of every non-`work` dataset reference
#'
#' `work` is the session's own temporary library: it is never bound by a
#' `LIBNAME` statement in the projects sas2r translates and never relocated by
#' configuration, so its rows carry no binding and say so with `NA` rather than
#' with an invented record. Every other row gets a record, including the ones
#' nothing binds -- an unbound or ambiguous reference is a finding, and a
#' finding needs an id to be reported against.
#'
#' Resolution here names no execution context. A reference inside a file that
#' executes in several contexts is resolved in each of them and recorded as
#' `ambiguous_libref` only when they *disagree*; when they agree the shared
#' effect is recorded.
#'
#' `binding_id` carries no execution context at all -- see
#' [libref_binding_id()] -- so this column is joinable against a record a later
#' caller resolves through [resolve_libref_at()] whether or not that caller
#' names `root_program` and `occurrence_id`. The two agree exactly when they
#' describe the same binding, which is what a join on an id has to mean; an
#' earlier scheme minted a *different* id for a named context, and a caller who
#' held the root program and passed it got a silent zero-match join against
#' this column rather than an error.
#'
#' The resolved records come back alongside the annotated lineage. They are the
#' only place a project resolves its bindings, and every later report that
#' needs more of a binding than the two columns carry -- the engine a
#' downgraded binding named, say -- reads them instead of resolving again. The
#' `lineage` contract stays at `binding_id` and `binding_status`: the records
#' hold all fifteen [LIBREF_BINDING_FIELDS], which is more than a per-row
#' column could carry anyway.
#'
#' @param lineage Dataset reference tibble.
#' @param registry A `sas2r_libref_registry`.
#' @return A list of `lineage`, with `binding_id` and `binding_status` columns
#'   added, and `records`, one effective binding record per distinct point of
#'   use in the order the points of use first appear.
#' @noRd
attach_lineage_bindings <- function(lineage, registry) {
  lineage$binding_id <- rep(NA_character_, nrow(lineage))
  lineage$binding_status <- rep(NA_character_, nrow(lineage))
  none <- list(lineage = lineage, records = list())
  if (nrow(lineage) == 0L) return(none)
  librefs <- sub("\\..*$", "", lineage$dataset)
  rows <- which(librefs != "work")
  if (!length(rows)) return(none)
  resolved <- libref_point_of_use_records(
    registry, librefs[rows], lineage$file[rows], lineage$line[rows])
  lineage$binding_id[rows] <- vapply(resolved$records[resolved$slot], `[[`,
                                     character(1), "binding_id")
  lineage$binding_status[rows] <- vapply(resolved$records[resolved$slot], `[[`,
                                         character(1), "status")
  list(lineage = lineage, records = resolved$records)
}

#' Resolve one effective binding per *distinct* point of use
#'
#' The single definition of "distinct point of use" in the package: a libref,
#' a file, and a line. Two references to different datasets of one library on
#' one line share a binding, and so do a `LIBNAME` and a reference written on
#' the same line -- the second is what makes
#' `libname adam 'x'; set adam.adsl;` resolve once rather than twice.
#'
#' [attach_lineage_bindings()] and [effective_librefs()] both go through here
#' rather than each spelling the dedup out, so the two cannot drift apart on
#' what counts as one point of use. They are separate *passes* -- the first
#' runs during `sas_project()`, the second at transpile time -- and that is
#' deliberate: the projection is built from a finished project rather than
#' carried on it, so nothing has to keep a resolved record set in sync with a
#' project object callers can hold across a rescan.
#'
#' @param registry A `sas2r_libref_registry`.
#' @param libref,file,line Parallel vectors of points of use.
#' @return A list of `records`, one per distinct point of use in the order the
#'   points of use first appear, and `slot`, the index into `records` for each
#'   input position.
#' @noRd
libref_point_of_use_records <- function(registry, libref, file, line) {
  if (!length(libref)) return(list(records = list(), slot = integer()))
  keys <- paste(libref, file, line, sep = LIBREF_KEY_SEP)
  unique_keys <- unique(keys)
  records <- lapply(match(unique_keys, keys), function(i) {
    libref_binding_at(registry, libref[i], file[i], line[i])
  })
  list(records = records, slot = match(keys, unique_keys))
}

#' Resolve which library a libref names at one point of use
#'
#' An accessible source `LIBNAME` binding wins at its point of use, and
#' configuration is a fallback and relocation mechanism rather than the
#' authority: a project whose `libname adam '/study/adam';` is readable needs
#' no `libraries:` block at all. Configuration supplies the location only when
#' the source binding cannot -- the libref was never assigned or has been
#' cleared, its path is a macro expression, its engine is not a local directory
#' engine, or the directory is missing or unreadable.
#'
#' "At its point of use" is a claim about *source position*, and it holds only
#' for a statement that is unconditionally reached. A `LIBNAME` written inside
#' a macro definition is not: it runs only if something calls that macro, and
#' sas2r analyses no macro call graph. Such a statement is therefore reported
#' but not established -- configuration wins over it when configuration exists,
#' and otherwise the record comes back `status = "conditionally_bound"` so a
#' consumer can tell a reported location from an established one. Every other
#' analysis in the package reads source position as execution; this is the one
#' place that reading would override an explicit user choice, so this is the
#' one place it is qualified.
#'
#' Because such a statement does not establish a binding, it does not displace
#' one either: a conditional `LIBNAME` is consulted only when no unconditional
#' one is visible at the point of use. An accessible `libname adam '/study';`
#' therefore keeps winning at every point of use below it no matter how many
#' macro definitions later in the file also name `adam` -- being merely later
#' in the file is not enough to take a binding away.
#'
#' Selection happens in an execution context, never in a global map. One
#' physical file often executes in several contexts, and that alone is not
#' ambiguity: the contexts are each resolved and the answer is ambiguous only
#' when they *disagree* on `(selection_origin, status, selected_path)`. When
#' they do, the caller must name the context with `root_program` and/or
#' `occurrence_id`; otherwise the ambiguity is raised rather than resolved,
#' because borrowing one program's binding for another is exactly the error
#' this registry exists to prevent. Configuration can never break such a tie:
#' an accessible source binding is not overridable, so a configured path would
#' only be hiding the question.
#'
#' @param registry A `sas2r_libref_registry`, from `project$libref_registry`.
#' @param libref Library reference to resolve; case-insensitive.
#' @param file File the reference is used in.
#' @param line Line the reference sits on.
#' @param root_program Root program whose execution context to resolve in.
#' @param occurrence_id `%include` occurrence that brought `file` into that
#'   context, from `project$include_graph$occurrences`.
#' @return An effective binding record: a named list carrying exactly
#'   [LIBREF_BINDING_FIELDS].
#' @noRd
resolve_libref_at <- function(registry, libref, file, line,
                              root_program = NULL, occurrence_id = NULL) {
  record <- libref_binding_at(registry, libref, file, line,
                              root_program = root_program,
                              occurrence_id = occurrence_id)
  if (identical(record$status, "ambiguous_libref")) {
    cli::cli_abort(
      c("Libref {.val {record$libref}} has more than one plausible binding in {.file {file}}.",
        "x" = "Ambiguity: {.val {record$fallback_reason}}.",
        "i" = "Name the execution context with {.arg root_program} and {.arg occurrence_id}."),
      class = "sas2r_ambiguous_libref"
    )
  }
  record
}

#' Exact column order of the effective-libref projection
#'
#' The projection is a contract, not an internal helper: [write_registry()] and
#' the staged-module emitter read it today, and confined output-evidence
#' discovery reads the same rows later, so the column list is declared once
#' here.
#'
#' The first five columns describe the *point of use* the row was resolved at;
#' the rest are the fifteen [LIBREF_BINDING_FIELDS] of the record resolved
#' there, unchanged and in their own order.
#'
#' * `kind` -- `"statement"` for a source `LIBNAME`/`CLEAR` resolved at its own
#'   coordinates, `"reference"` for a dataset reference resolved at its own.
#' * `unit_id` -- the translation unit the point of use sits in.
#' * `use_file`, `use_line` -- the point of use itself. The record's own `file`
#'   and `line` are the *defining* statement's, which is a different place.
#' * `source_engine` -- the engine token of the `LIBNAME` a `"statement"` row
#'   *is*, verbatim from the scan and possibly `""`; `NA` on a `"reference"`
#'   row, which is not a statement and so names no engine.
#'
#' `root_program` and `include_occurrence_id` come through from the record and
#' are the row's execution-context provenance. They are displayed provenance,
#' not identity: `binding_id` is context-free by construction, so a row is
#' joinable against a record a later caller resolves with or without naming a
#' context. See [libref_binding_id()].
#' @noRd
EFFECTIVE_LIBREF_FIELDS <- c(
  "kind", "unit_id", "use_file", "use_line", "source_engine",
  LIBREF_BINDING_FIELDS
)

#' Zero-row prototype of the effective-libref projection
#' @noRd
empty_effective_librefs <- function() {
  proto <- tibble::tibble(
    kind = character(), unit_id = integer(), use_file = character(),
    use_line = integer(), source_engine = character(),
    binding_id = character(), libref = character(),
    root_program = character(), include_occurrence_id = character(),
    file = character(), line = integer(), action = character(),
    source_path_expression = character(), source_path = character(),
    configured_path = character(), selected_path = character(),
    selection_origin = character(), status = character(),
    fallback_reason = character(), context_truncated = logical()
  )
  proto[EFFECTIVE_LIBREF_FIELDS]
}

#' Project a finished project onto the bindings its generated bundle needs
#'
#' The one projection of `project$libref_registry` that generated R and, later,
#' confined output-evidence discovery are built from. Its whole purpose is that
#' there be exactly one: before it, [write_registry()] independently reparsed
#' `project$librefs` and emitted a single static project-wide map, so a
#' generated bundle could disagree with the very bindings the project had
#' already selected -- an accessible source `LIBNAME` won in `project$lineage`
#' and lost in the emitted registry, and a `LIBNAME ... CLEAR` reached neither.
#'
#' Nothing here re-decides anything. Every row's answer is
#' [libref_binding_at()]'s, resolved at that row's own point of use, and the
#' projection only says *which* points of use a generated bundle has to have an
#' answer for:
#'
#' * every non-`work` source `LIBNAME` or `LIBNAME ... CLEAR` statement, at its
#'   own coordinates -- which is what lets the emitter put the binding change
#'   where the statement stands rather than once for the whole project.
#'   Resolving a `LIBNAME` at its own line sees the statement itself, because an
#'   execution key that stops at a line ties with every key that extends it (see
#'   [libref_key_cmp()]);
#' * every non-`work` dataset reference, at its own coordinates -- the points
#'   of use a generated read or write actually needs an answer for.
#'
#' `work` is the session's own temporary library and is never resolved, exactly
#' as in [attach_lineage_bindings()] -- and that holds for statements as much
#' as for references. It did not always: a `libname work '/tmp/x';` used to
#' resolve like any other statement and emit an assign, which repointed `work`
#' at a directory the bundle's own seed never created and made the next
#' `lib_write()` to `work` fail. Such statements are kept out of `bindings` and
#' listed in `session_library` instead, so the emitter can still show them
#' without binding anything.
#'
#' No execution context is named. A physical file that executes in several
#' contexts is resolved in each, and a row comes back `ambiguous_libref` only
#' when they disagree -- the right answer for a projection whose consumer emits
#' *one* module per physical file and so has one place to put the answer.
#'
#' @param project A `sas2r_project` object.
#' @return A `sas2r_effective_librefs` list with
#'   * `version` -- [LIBREF_REGISTRY_VERSION], the schema the rows answer to;
#'   * `seed` -- the normalized configured libraries, the only bindings that
#'     exist before the program runs a statement;
#'   * `bindings` -- a tibble carrying exactly [EFFECTIVE_LIBREF_FIELDS];
#'   * `undeclared` -- librefs a generated read or write needs and no binding
#'     establishes, in first-use order;
#'   * `session_library` -- the `LIBNAME` statements naming `work`, which bind
#'     nothing and are carried only so the emitter can report them.
#' @noRd
effective_librefs <- function(project) {
  registry <- project$libref_registry
  if (!inherits(registry, "sas2r_libref_registry")) {
    cli::cli_abort(
      "{.arg project} must carry a sas2r libref registry.",
      class = "sas2r_libref_registry_error"
    )
  }
  seed <- registry$libraries %||% list()

  # `work` is filtered out of statements exactly as it is out of references:
  # it is the session library the generated seed creates, and resolving a
  # `LIBNAME` naming it would produce a binding the emitter then had to apply.
  all_st <- if (is.null(project$librefs)) NULL else project$librefs
  st_keep <- if (is.null(all_st)) integer() else
    which(tolower(all_st$libref) != "work")
  session_library <- if (is.null(all_st)) empty_session_library() else
    session_library_statements(all_st)
  lr <- if (is.null(all_st)) NULL else all_st[st_keep, , drop = FALSE]
  st_n <- if (is.null(lr)) 0L else nrow(lr)
  lin <- project$lineage
  ref_rows <- if (is.null(lin) || !nrow(lin)) integer() else
    which(sub("\\..*$", "", lin$dataset) != "work")

  kind <- c(rep("statement", st_n), rep("reference", length(ref_rows)))
  libref <- c(if (st_n) tolower(lr$libref) else character(),
              if (length(ref_rows)) sub("\\..*$", "", lin$dataset[ref_rows])
              else character())
  use_file <- c(if (st_n) as.character(lr$file) else character(),
                if (length(ref_rows)) as.character(lin$file[ref_rows])
                else character())
  use_line <- c(if (st_n) as.integer(lr$line) else integer(),
                if (length(ref_rows)) as.integer(lin$line[ref_rows])
                else integer())
  unit_id <- c(if (st_n) as.integer(lr$unit_id) else integer(),
               if (length(ref_rows)) as.integer(lin$unit_id[ref_rows])
               else integer())
  source_engine <- c(if (st_n) as.character(lr$engine) else character(),
                     rep(NA_character_, length(ref_rows)))

  if (!length(kind)) {
    return(structure(
      list(version = LIBREF_REGISTRY_VERSION, seed = seed,
           bindings = empty_effective_librefs(), undeclared = character(),
           session_library = session_library),
      class = "sas2r_effective_librefs"
    ))
  }

  resolved <- libref_point_of_use_records(registry, libref, use_file, use_line)
  records <- resolved$records[resolved$slot]
  # A record whose field is missing or not a scalar is a malformed record, and
  # the honest projection of one is `NA`. Falling back to the vapply template
  # would say `""` for `binding_id` -- the column later tasks join evidence on
  # -- and `FALSE` for `context_truncated`, which is not an absence but a
  # positive claim that the frame walk ran to completion.
  field <- function(nm, type) {
    missing_value <- type
    missing_value[[1L]] <- NA
    vapply(records, function(r) {
      value <- r[[nm]]
      if (length(value) != 1L) missing_value else value
    }, type)
  }
  bindings <- tibble::tibble(
    kind = kind, unit_id = unit_id, use_file = use_file, use_line = use_line,
    source_engine = source_engine,
    binding_id = field("binding_id", character(1)),
    libref = field("libref", character(1)),
    root_program = field("root_program", character(1)),
    include_occurrence_id = field("include_occurrence_id", character(1)),
    file = field("file", character(1)),
    line = field("line", integer(1)),
    action = field("action", character(1)),
    source_path_expression = field("source_path_expression", character(1)),
    source_path = field("source_path", character(1)),
    configured_path = field("configured_path", character(1)),
    selected_path = field("selected_path", character(1)),
    selection_origin = field("selection_origin", character(1)),
    status = field("status", character(1)),
    fallback_reason = field("fallback_reason", character(1)),
    context_truncated = field("context_truncated", logical(1))
  )

  # A libref a generated read or write needs and nothing establishes. Only
  # `reference` rows count: a `LIBNAME` naming a directory that is not there is
  # a finding only if something actually reads the library, and a libref that
  # is declared and never used needs no placeholder to fill in.
  #
  # `conditionally_bound` counts as unestablished, because it is: the only
  # `LIBNAME` for it sits inside a macro definition and runs only if something
  # calls that macro. `ambiguous_libref` deliberately does not -- the libref is
  # bound, differently per execution context, and a placeholder would invite
  # the user to paper over a question the emitter refuses to answer for them.
  undeclared <- unique(bindings$libref[
    bindings$kind == "reference" &
      bindings$status %in% c("unbound", "conditionally_bound")])

  structure(
    list(version = LIBREF_REGISTRY_VERSION, seed = seed, bindings = bindings,
         undeclared = as.character(undeclared),
         session_library = session_library),
    class = "sas2r_effective_librefs"
  )
}

#' Zero-row prototype of the `LIBNAME work` statement table
#' @noRd
empty_session_library <- function() {
  tibble::tibble(
    unit_id = integer(), use_file = character(), use_line = integer(),
    libref = character(), action = character(),
    source_path_expression = character()
  )
}

#' The `LIBNAME` statements naming `work`, in source order
#'
#' Kept out of [effective_librefs()]`$bindings` so nothing resolves or applies
#' them, and kept at all so the emitter can report them rather than leave a
#' unit anchor with nothing under it. See [emit_session_library_statement()].
#'
#' @param librefs `project$librefs`.
#' @return A tibble shaped like [empty_session_library()].
#' @noRd
session_library_statements <- function(librefs) {
  rows <- librefs[tolower(librefs$libref) == "work", , drop = FALSE]
  if (!nrow(rows)) return(empty_session_library())
  tibble::tibble(
    unit_id = as.integer(rows$unit_id),
    use_file = as.character(rows$file),
    use_line = as.integer(rows$line),
    libref = tolower(as.character(rows$libref)),
    action = as.character(rows$action),
    source_path_expression = as.character(rows$path_expression)
  )
}

#' Canonicalize and confine a selected library path before it is emitted
#'
#' [LIBREF_BINDING_FIELDS] says `selected_path` is canonical but **unconfined**,
#' and that every consumer must confine it before reading anything under it.
#' This is the emitter's half of that obligation, and it is deliberately not a
#' containment test: a library legitimately lives outside the project -- a
#' study's `/study/adam` is the ordinary case -- so requiring the path to sit
#' under the project root would refuse correct translations rather than protect
#' anything.
#'
#' What it does instead is make the path safe to *emit*, which is the only
#' thing the transpiler does with it. Nothing under a library is opened at
#' transpile time; the reads happen in the generated bundle, and the bundle's
#' own half of the confinement is `sas2r_lib_member_path()` in
#' `inst/templates/sas2r-helpers.R`, which holds every member read or write
#' inside the directory the binding named.
#'
#' So: one non-empty scalar, no control characters -- a newline or a NUL would
#' be embedded verbatim into a generated string literal -- and then the *same*
#' canonicalization [libref_source_path_state()] and [normalize_library_entries()]
#' already applied, which on a path either of them produced is a fixed point.
#'
#' Applying a *different* one would be the bug, not the fix.
#' [normalize_library_config()] already documents why: two canonicalizations of
#' one directory come back spelled two ways, and a consumer joining on
#' `selected_path` sees two libraries. [canonical_output_path()] is the obvious
#' candidate and is the wrong tool here -- it rewrites `\\` to `/` for the sake
#' of Windows separators, which silently renames a directory legitimately
#' called `we\\ird` on a POSIX filesystem.
#'
#' The value is embedded with [deparse()] at every call site, which is what
#' makes an arbitrary directory name -- backslashes, quotes, non-ASCII -- unable
#' to become code.
#'
#' @param path A selected library path.
#' @param libref The libref it belongs to, for the error message.
#' @return The canonical path.
#' @noRd
confine_libref_path <- function(path, libref) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl("[[:cntrl:]]", path)) {
    cli::cli_abort(
      c("Libref {.val {libref}} selected a location sas2r cannot emit.",
        "x" = "Wanted one non-empty path without control characters."),
      class = "sas2r_libref_path_error"
    )
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Build attempt-local copy-on-write library map
#'
#' Maps configured libraries to attempt-local candidate directories for writing,
#' while preserving read access to configured inputs. For WORK, both read and write
#' paths point to the attempt work directory.
#'
#' @param project A `sas2r_project` object, or an effective librefs projection.
#' @param attempt_dir The attempt directory root path.
#' @return A named list of library specifications with `read_path`, `write_path`,
#'   `engine`, and `write`.
#' @noRd
build_attempt_library_map <- function(project, attempt_dir) {
  effective <- if (inherits(project, "sas2r_effective_librefs")) project else effective_librefs(project)
  seed <- effective$seed
  out <- list()
  for (libref in names(seed)) {
    if (libref == "work") next
    entry <- seed[[libref]]
    cand_dir <- file.path(attempt_dir, libref)
    out[[libref]] <- list(
      read_path = confine_libref_path(entry$read_path %||% entry$path, libref),
      write_path = normalizePath(cand_dir, winslash = "/", mustWork = FALSE),
      engine = entry$engine %||% "sas7bdat",
      write = entry$write %||% "rds"
    )
  }
  work_dir <- file.path(attempt_dir, "work")
  out[["work"]] <- list(
    read_path = normalizePath(work_dir, winslash = "/", mustWork = FALSE),
    write_path = normalizePath(work_dir, winslash = "/", mustWork = FALSE),
    engine = "rds",
    write = "rds"
  )
  out
}

