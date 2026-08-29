#' Order translation units by data dependency (topological sort)
#'
#' @param lineage Dataset references tibble from extract_dataset_refs().
#' @param unit_ids Integer vector of unit IDs to order.
#' @return Integer vector of ordered unit IDs, or `NULL` if a dependency cycle is detected.
#' @noRd
unit_order <- function(lineage, unit_ids) {
  writer <- split(lineage$unit_id[lineage$role == "creates"],
                  lineage$dataset[lineage$role == "creates"])
  reads <- lineage[lineage$role == "reads", ]
  n_reads <- nrow(reads)
  if (n_reads == 0L) return(unit_ids)

  read_units <- reads$unit_id
  read_dsets <- reads$dataset
  edges_from_list <- vector("list", n_reads)
  edges_to_list <- vector("list", n_reads)

  for (k in seq_len(n_reads)) {
    r_unit <- read_units[k]
    w <- writer[[read_dsets[k]]]
    if (!length(w)) next
    # In sequential execution, reader depends on the closest preceding writer if available,
    # or forward writer if reader precedes all writers in file order
    idx <- findInterval(r_unit - 1L, w)
    chosen_w <- if (idx > 0L) {
      w_cand <- w[idx]
      if (w_cand != r_unit) w_cand else {
        w_sub <- w[w < r_unit]
        if (length(w_sub)) max(w_sub) else w[w != r_unit]
      }
    } else {
      w[w != r_unit]
    }
    if (length(chosen_w)) {
      edges_from_list[[k]] <- chosen_w
      edges_to_list[[k]] <- rep(r_unit, length(chosen_w))
    }
  }
  edges_from <- unlist(edges_from_list, use.names = FALSE)
  if (is.null(edges_from)) edges_from <- integer()
  edges_to <- unlist(edges_to_list, use.names = FALSE)
  if (is.null(edges_to)) edges_to <- integer()

  indeg <- stats::setNames(integer(length(unit_ids)), unit_ids)
  tab_to <- table(as.character(edges_to))
  indeg[names(tab_to)] <- as.integer(tab_to)

  adj <- split(edges_to, edges_from)
  queue <- unit_ids[indeg[as.character(unit_ids)] == 0L]
  out <- integer(length(unit_ids))
  out_len <- 0L
  q_head <- 1L
  while (q_head <= length(queue)) {
    v <- queue[q_head]
    q_head <- q_head + 1L
    out_len <- out_len + 1L
    out[out_len] <- v
    kids <- adj[[as.character(v)]]
    for (kid in kids) {
      ckid <- as.character(kid)
      indeg[ckid] <- indeg[ckid] - 1L
      if (indeg[ckid] == 0L) queue <- c(queue, kid)
    }
  }
  if (out_len < length(unit_ids)) return(NULL)  # cycle
  out[seq_len(out_len)]
}

# Bumped whenever a cached per-file scan product changes shape; stale entries
# under an older version are simply never looked up again.
SCAN_CACHE_SCHEMA_VERSION <- "2.0"

# Attach retained source comments to the translation units that own their
# private character spans. Comments between units belong to the next unit;
# comments after the last unit (or in a comment-only file) remain unattached.
#
# @param comments Tibble from sas_source_records()$comments.
# @param spanned_units Tibble from sas_units() retaining char_start and char_end.
# @return `comments` with file-local unit_id and placement columns.
# @noRd
attach_comments_to_units <- function(comments, spanned_units) {
  comments <- tibble::as_tibble(comments)
  comments$unit_id <- rep(NA_integer_, nrow(comments))
  comments$placement <- rep("unattached", nrow(comments))
  if (!nrow(comments) || !nrow(spanned_units)) return(comments)

  unit_ids <- unique(as.integer(spanned_units$unit_id))
  unit_spans <- tibble::tibble(
    unit_id = unit_ids,
    char_start = vapply(unit_ids, function(unit_id) {
      min(spanned_units$char_start[spanned_units$unit_id == unit_id])
    }, integer(1)),
    char_end = vapply(unit_ids, function(unit_id) {
      max(spanned_units$char_end[spanned_units$unit_id == unit_id])
    }, integer(1))
  )
  unit_spans <- unit_spans[order(unit_spans$char_start, unit_spans$char_end), ]

  for (i in seq_len(nrow(comments))) {
    inside <- which(
      comments$char_start[i] >= unit_spans$char_start &
        comments$char_end[i] <= unit_spans$char_end
    )
    if (length(inside)) {
      owner <- inside[1L]
      comments$unit_id[i] <- unit_spans$unit_id[owner]
      comments$placement[i] <- "internal"
      next
    }

    next_unit <- which(unit_spans$char_start > comments$char_end[i])
    if (length(next_unit)) {
      comments$unit_id[i] <- unit_spans$unit_id[next_unit[1L]]
      comments$placement[i] <- "leading"
    }
  }
  comments
}

#' Scan SAS project files into an in-memory project representation
#'
#' Scans a SAS project directory or single file into statements, translation
#' units, declared library references, macro calls, and dataset references.
#'
#' @param path Path to SAS file or project directory.
#' @param config Optional project configuration list or `sas2r_config` object.
#' @param recursive Logical; whether to recurse into subdirectories.
#' @param cache Logical; whether to cache and reuse per-file parsed products.
#' @return An object of S3 class `"sas2r_project"`. Its top-level `comments`
#'   tibble retains SAS comment evidence with `comment_id`, `text`, `kind`,
#'   `line_start`/`line_end`, `char_start`/`char_end`, `unit_id`, `placement`,
#'   `file`, and `origin` fields. Comments internal to a translation unit stay
#'   with that unit. Comments between units attach forward to the next unit;
#'   comments after the last executable unit and comments in comment-only files
#'   remain unattached and are not sent to a worker. Comments are fallback-only
#'   supporting evidence, not intent or authority: executable SAS takes
#'   precedence, and source approval identity is derived from code only.
#' @noRd
sas_project <- function(path, config = NULL, recursive = FALSE, cache = FALSE) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path) || (!dir.exists(path) && !file.exists(path))) {
    stop("Path does not exist or is invalid: '", path, "'")
  }
  is_dir <- dir.exists(path)
  root <- if (is_dir) path else dirname(path)

  base_cfg <- sas_config(start = root)
  config <- if (is.null(config)) {
    base_cfg
  } else if (inherits(config, "sas2r_config")) {
    config
  } else if (is.list(config)) {
    for (nm in names(config)) {
      base_cfg[[nm]] <- config[[nm]]
    }
    base_cfg
  } else {
    base_cfg
  }
  # sas_config() has already resolved everything read from `_sas2r.yml` against
  # the configuration file's directory. What can still be relative here came
  # from a caller-supplied config list, which has no file of its own, so the
  # project root is its base -- the same rule include_roots follows below.
  # Re-normalizing an already-normalized entry is a no-op. The base is taken
  # canonical, matching what normalize_library_config() does with a
  # configuration file's directory, so a library directory comes back spelled
  # one way whichever authority named it.
  config$libraries <- normalize_library_entries(config$libraries,
                                                include_normalize_path(root))
  config$macro_search_path <- config_resolve_paths(
    config$macro_search_path %||% character(), root)

  cache_file <- file.path(root, ".sas2r", "scan_cache.rds")
  scan_cache <- if (cache && file.exists(cache_file)) {
    res <- tryCatch(readRDS(cache_file), error = function(e) list())
    if (!is.list(res)) list() else res
  } else {
    list()
  }

  autoexec <- config$autoexec %||% character()
  flags_env <- list()
  if (!length(autoexec)) {
    cand <- file.path(root, "autoexec.sas")
    if (file.exists(cand)) {
      autoexec <- cand
      flags_env[[length(flags_env) + 1L]] <- tibble::tibble(
        kind = "autoexec_autodiscovered", detail = cand)
    }
  } else {
    autoexec <- config_resolve_paths(autoexec, root)
    missing_auto <- autoexec[!file.exists(autoexec)]
    for (m_path in missing_auto) {
      flags_env[[length(flags_env) + 1L]] <- tibble::tibble(
        kind = "autoexec_missing", detail = m_path)
    }
  }

  env_files <- if (length(autoexec) > 0L) autoexec[file.exists(autoexec)] else character()
  norm_env <- include_scan_key(env_files)

  # Two `autoexec` entries naming one physical file would queue that file twice
  # and mint one occurrence id twice. That is a configuration mistake, so it is
  # reported here in configuration language rather than surfacing downstream as
  # the include graph's internal uniqueness invariant. The check is on the
  # physical file, not the basename -- two autoexec files that merely share a
  # basename are distinct anchors and stay distinct. Behaviour change worth
  # naming: a duplicated autoexec holding no `%include` at all used to be
  # scanned twice in silence, and now aborts here with the rest.
  dup_env <- unique(norm_env[duplicated(norm_env)])
  if (length(dup_env)) {
    cli::cli_abort(
      c("Configured {.field environment.autoexec} names the same file more than once.",
        "x" = "Repeated: {.file {unique(env_files[norm_env %in% dup_env])}}.",
        "i" = "List each autoexec file once."),
      class = "sas2r_autoexec_duplicate"
    )
  }

  program_files <- if (is_dir) {
    all <- list.files(path, pattern = "\\.sas$", full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
    sort(all, method = "radix")
  } else path

  if (length(norm_env) > 0L) {
    program_files <- program_files[!include_scan_key(program_files) %in% norm_env]
  }

  # Each queued item carries the occurrence that pulled it in, so an included
  # file always knows its include site (NA for roots, which have none).
  queue <- c(
    lapply(env_files, function(f) {
      list(file = f, origin = "environment", depth = 0L,
           chain = include_scan_key(f), parent_occurrence_id = NA_character_)
    }),
    lapply(program_files, function(f) {
      list(file = f, origin = "program", depth = 0L,
           chain = include_scan_key(f), parent_occurrence_id = NA_character_)
    })
  )
  seen <- c(norm_env, include_scan_key(program_files))
  # Resolved once, here, and used for both resolution and identity below. A
  # relative root reaching normalizePath() unresolved would be resolved against
  # getwd(), which is neither project content nor configuration: the same bytes
  # would then mint different occurrence ids from a developer's checkout root
  # and from CI's working directory. sas_config() has already applied the
  # config-file base to anything read from `_sas2r.yml`, so what is still
  # relative here came from a caller-supplied config and is anchored on the
  # project root instead -- the same rule autoexec follows above.
  include_roots <- config_resolve_paths(config$include_roots %||% character(), root)
  # The coordinate frame every scanned file is named in, built once and from
  # configuration only -- never from the scan. See include_identity_anchors().
  identity_anchors <- include_identity_anchors(root, include_roots, autoexec)

  scanned_files <- list()
  scanned_origins <- list()
  scanned_depths <- list()
  scanned_parent_occ <- list()
  occurrences_list <- list()
  stmt_list <- list()
  comments_list <- list()
  unit_rows <- list()
  librefs_list <- list()
  libref_events_list <- list()
  include_sites_list <- list()
  includes_list <- list()
  defs_list <- list()
  calls_list <- list()
  fmt_defs_list <- list()
  fmt_uses_list <- list()
  fn_defs_list <- list()
  fn_uses_list <- list()
  lineage_list <- list()
  offset <- 0L
  flags_list <- list()
  filerefs <- list()

  cache_dirty <- FALSE
  queue_idx <- 1L
  while (queue_idx <= length(queue)) {
    item <- queue[[queue_idx]]
    queue_idx <- queue_idx + 1L

    f <- item$file
    origin <- item$origin
    depth <- item$depth
    chain <- item$chain
    # This file reduced to one machine-independent key, computed once from the
    # file and the anchor frame alone. Nothing about the chain that reached it
    # is an input, so a file reachable from several include sites gets one
    # identity whichever site arrives first -- see include_identity_parent().
    parent_identity <- include_identity_parent(f, identity_anchors)

    scanned_files[[length(scanned_files) + 1L]] <- f
    scanned_origins[[length(scanned_origins) + 1L]] <- origin
    scanned_depths[[length(scanned_depths) + 1L]] <- depth
    scanned_parent_occ[[length(scanned_parent_occ) + 1L]] <-
      item$parent_occurrence_id %||% NA_character_

    raw_text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    hit <- NULL
    if (cache) {
      h <- paste0("v", SCAN_CACHE_SCHEMA_VERSION, "_", cli::hash_md5(raw_text))
      hit <- scan_cache[[h]]
    }

    if (is.null(hit)) {
      source_records <- sas_source_records(raw_text)
      spanned_units <- sas_units(source_records$statements)
      comments_raw <- attach_comments_to_units(
        source_records$comments, spanned_units
      )
      u_raw <- tibble::as_tibble(spanned_units[, setdiff(
        names(spanned_units), c("char_start", "char_end")
      ), drop = FALSE])
      if (nrow(u_raw) > 0L) {
        agg <- u_raw[!duplicated(u_raw$unit_id), ]
        tab_uid <- table(u_raw$unit_id)
        min_start <- tapply(u_raw$line_start, u_raw$unit_id, min)
        max_end <- tapply(u_raw$line_end, u_raw$unit_id, max)
        uid_chr <- as.character(agg$unit_id)
        unit_rows_raw <- tibble::tibble(
          unit_id = agg$unit_id,
          unit_type = agg$unit_type,
          label = substr(agg$text, 1L, 60L),
          line_start = as.integer(min_start[uid_chr]),
          line_end = as.integer(max_end[uid_chr]),
          n_stmts = as.integer(tab_uid[uid_chr])
        )
      } else {
        unit_rows_raw <- tibble::tibble(
          unit_id = integer(), unit_type = character(), label = character(),
          line_start = integer(), line_end = integer(), n_stmts = integer()
        )
      }
      lr_raw <- extract_librefs(u_raw)
      inc_raw <- extract_includes(u_raw)
      defs_raw <- extract_macro_defs(u_raw)
      calls_raw <- extract_macro_calls(u_raw)
      lineage_raw <- extract_dataset_refs(u_raw)
      fmt_defs_raw <- extract_format_defs(u_raw)
      fmt_uses_raw <- extract_format_uses(u_raw)
      fn_defs_raw <- extract_function_defs(u_raw)
      fn_uses_raw <- extract_function_uses(u_raw)

      hit <- list(
        units = u_raw,
        comments = comments_raw,
        unit_rows = unit_rows_raw,
        librefs = lr_raw,
        includes = inc_raw,
        defs = defs_raw,
        calls = calls_raw,
        lineage = lineage_raw,
        fmt_defs = fmt_defs_raw,
        fmt_uses = fmt_uses_raw,
        fn_defs = fn_defs_raw,
        fn_uses = fn_uses_raw
      )
      if (cache) {
        scan_cache[[h]] <- hit
        cache_dirty <- TRUE
      }
    }

    n_u <- length(unique(hit$units$unit_id))
    u <- hit$units
    u_rows <- hit$unit_rows
    if (nrow(u) > 0L) {
      u$unit_id <- u$unit_id + offset
      u$file <- f
      u$origin <- origin
      u_rows$unit_id <- u_rows$unit_id + offset
      u_rows$file <- f
      u_rows$origin <- origin
    } else {
      u$file <- character()
      u$origin <- character()
      u_rows$file <- character()
      u_rows$origin <- character()
    }
    stmt_list[[f]] <- u
    unit_rows[[f]] <- u_rows

    file_comments <- hit$comments
    if (nrow(file_comments) > 0L) {
      attached <- !is.na(file_comments$unit_id)
      file_comments$unit_id[attached] <-
        file_comments$unit_id[attached] + offset
    }
    file_comments$file <- rep(f, nrow(file_comments))
    file_comments$origin <- rep(origin, nrow(file_comments))
    comments_list[[f]] <- file_comments

    lr <- hit$librefs
    lr_events <- empty_libref_source_events()
    if (nrow(lr) > 0L) {
      lr$file <- f
      # Captured before the project-level offset, for the reason
      # include_occurrence_id() records: the file-local index depends on
      # nothing outside the file, while the offset id shifts whenever an
      # earlier-scanned file gains, loses, or stops being scanned. The offset
      # id stays as the joinable unit_id column.
      lr_file_unit_id <- as.integer(lr$unit_id)
      lr_events <- tibble::tibble(
        file = rep(f, nrow(lr)),
        canonical_key = rep(include_scan_key(f), nrow(lr)),
        libref = lr$libref, action = lr$action, engine = lr$engine,
        path_expression = lr$path_expression,
        line = as.integer(lr$line),
        file_unit_id = lr_file_unit_id,
        # A `LIBNAME` inside a macro definition only runs if something calls
        # that macro, and sas2r does not analyse macro reachability, so the
        # event is carried as conditional rather than as an established
        # binding. See libref_binding_at().
        conditional = !is.na(lr$unit_type) & lr$unit_type == "macro_def",
        # Execution order within one unit and line, so two `LIBNAME`
        # statements sharing a line still have an order. Not an identity
        # input: a binding is identified by its point of use, never by the
        # position of the statement that supplied it -- see
        # libref_binding_id().
        intra = include_site_ordinal(lr_file_unit_id, lr$line,
                                     rep("", nrow(lr)))
      )
      lr$unit_id <- lr$unit_id + offset
    }
    librefs_list[[length(librefs_list) + 1L]] <- lr
    libref_events_list[[length(libref_events_list) + 1L]] <- lr_events

    # Includes are pushed onto includes_list only after resolution below, so the
    # compatibility table can carry each occurrence's identity and outcome.
    inc <- hit$includes
    inc_file_unit_id <- integer()
    if (nrow(inc) > 0L) {
      inc$file <- f
      # Captured before the project-level offset: occurrence identity is keyed
      # on the file-local unit index so that scanning an unrelated file first
      # cannot re-key this file's occurrences. The offset id below stays as the
      # joinable parent_unit_id column.
      inc_file_unit_id <- as.integer(inc$unit_id)
      inc$unit_id <- inc$unit_id + offset
    }

    md <- hit$defs
    if (nrow(md) > 0L) {
      md$file <- f
      md$unit_id <- md$unit_id + offset
    }
    defs_list[[length(defs_list) + 1L]] <- md

    mc <- hit$calls
    if (nrow(mc) > 0L) {
      mc$file <- f
      mc$source_file <- f
      mc$component_id <- if (identical(origin, "environment") || tolower(basename(f)) == "autoexec.sas") "setup" else tools::file_path_sans_ext(basename(f))
    }
    calls_list[[length(calls_list) + 1L]] <- mc

    lin <- hit$lineage
    if (nrow(lin) > 0L) {
      lin$file <- f
      lin$unit_id <- lin$unit_id + offset
    }
    lineage_list[[length(lineage_list) + 1L]] <- lin

    fd <- hit$fmt_defs
    if (!is.null(fd) && nrow(fd) > 0L) {
      fd$file <- f
      fd$unit_id <- fd$unit_id + offset
    }
    if (!is.null(fd) && nrow(fd) > 0L) fmt_defs_list[[length(fmt_defs_list) + 1L]] <- fd

    fu <- hit$fmt_uses
    if (!is.null(fu) && nrow(fu) > 0L) {
      fu$file <- f
      fu$unit_id <- fu$unit_id + offset
    }
    if (!is.null(fu) && nrow(fu) > 0L) fmt_uses_list[[length(fmt_uses_list) + 1L]] <- fu

    fnd <- hit$fn_defs
    if (!is.null(fnd) && nrow(fnd) > 0L) {
      fnd$file <- f
      fnd$unit_id <- fnd$unit_id + offset
    }
    if (!is.null(fnd) && nrow(fnd) > 0L) fn_defs_list[[length(fn_defs_list) + 1L]] <- fnd

    fnu <- hit$fn_uses
    if (!is.null(fnu) && nrow(fnu) > 0L) {
      fnu$file <- f
      fnu$unit_id <- fnu$unit_id + offset
    }
    if (!is.null(fnu) && nrow(fnu) > 0L) fn_uses_list[[length(fn_uses_list) + 1L]] <- fnu

    offset <- offset + n_u

    new_fr <- extract_filerefs(u)
    for (nm in names(new_fr)) filerefs[[nm]] <- new_fr[[nm]]

    if (nrow(inc) > 0L) {
      n_inc <- nrow(inc)
      occ_depth <- depth + 1L
      occ_ids <- character(n_inc)
      occ_targets <- rep(NA_character_, n_inc)
      occ_origins <- character(n_inc)
      occ_statuses <- character(n_inc)
      occ_reasons <- rep(NA_character_, n_inc)
      chain_tokens <- strsplit(chain, " -> ")[[1]]
      # Several statements may share a line, so two identical %include sites can
      # agree on unit, line, and target; the ordinal is what tells them apart.
      occ_ordinals <- include_site_ordinal(inc_file_unit_id, inc$line, inc$target)

      for (k in seq_len(n_inc)) {
        target_expr <- inc$target[k]
        occ_id <- include_occurrence_id(parent_identity, inc_file_unit_id[k],
                                        inc$line[k], target_expr,
                                        occ_ordinals[k])
        # A quoted target is a literal path and never consults filerefs.
        res <- resolve_include_target(
          target = target_expr,
          including_file = f,
          project_root = root,
          include_roots = include_roots,
          filerefs = if (isTRUE(inc$quoted[k])) list() else filerefs
        )

        # The occurrence is recorded before any cycle, depth, or already-seen
        # decision: every include site survives, only physical scans dedupe.
        occ_ids[k] <- occ_id
        occ_targets[k] <- res$path
        occ_origins[k] <- res$origin
        occ_statuses[k] <- res$status
        occ_reasons[k] <- res$reason

        if (res$status == "dynamic") {
          flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
            kind = "dynamic_include", detail = target_expr
          )
          next
        }
        if (res$status == "unresolved") {
          flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
            kind = "unresolved_include", detail = target_expr
          )
          next
        }

        target_path <- res$path
        norm_target <- tolower(target_path)
        # Both branches below keep the resolved, non-NA target_file on the
        # occurrence while deliberately never scanning that file, so consumers
        # must not read a non-NA target_file as "a module was staged for this".
        # Cycle is tested before depth, so a target that is both is reported as
        # "cycle": deterministic, and the more precise diagnosis, since the
        # depth was only reached by going around the cycle.
        if (norm_target %in% chain_tokens) {
          occ_statuses[k] <- "cycle"
          occ_reasons[k] <- "cycle_detected"
          flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
            kind = "include_cycle",
            detail = paste(c(chain, norm_target), collapse = " -> ")
          )
          next
        }
        if (occ_depth > INCLUDE_MAX_DEPTH) {
          occ_statuses[k] <- "depth_exceeded"
          occ_reasons[k] <- "max_depth_exceeded"
          flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
            kind = "include_depth_exceeded",
            detail = paste0(f, " -> ", target_path)
          )
          next
        }
        if (!norm_target %in% seen) {
          seen <- c(seen, norm_target)
          queue[[length(queue) + 1L]] <- list(
            file = target_path,
            origin = "included",
            depth = occ_depth,
            chain = paste0(chain, " -> ", norm_target),
            parent_occurrence_id = occ_id
          )
        }
      }

      # Columns by name only: include_occurrence_tibble() owns the column order
      # and rejects any status or origin outside its declared vocabulary.
      occurrences_list[[length(occurrences_list) + 1L]] <- include_occurrence_tibble(list(
        occurrence_id = occ_ids,
        parent_file = rep(f, n_inc),
        parent_unit_id = as.integer(inc$unit_id),
        line = as.integer(inc$line),
        target_expression = inc$target,
        target_file = occ_targets,
        resolution_origin = occ_origins,
        status = occ_statuses,
        reason = occ_reasons,
        depth = rep(occ_depth, n_inc)
      ))

      # The same sites again, keyed on the file-local unit index the occurrence
      # ids are keyed on. The occurrences table deliberately publishes only the
      # project-level parent_unit_id, which shifts with scan composition, so
      # the libref registry cannot order execution by it.
      include_sites_list[[length(include_sites_list) + 1L]] <- tibble::tibble(
        occurrence_id = occ_ids,
        parent_file = rep(f, n_inc),
        file_unit_id = inc_file_unit_id,
        line = as.integer(inc$line),
        target_file = occ_targets,
        status = occ_statuses
      )

      inc$occurrence_id <- occ_ids
      inc$target_file <- occ_targets
      inc$resolution_origin <- occ_origins
      inc$status <- occ_statuses
    }
    includes_list[[length(includes_list) + 1L]] <- inc
  }

  if (cache && cache_dirty) {
    tryCatch({
      atomic_write_file(function(tf) saveRDS(scan_cache, tf), cache_file, pattern = "scan_cache_")
    }, error = function(e) {
      cli::cli_warn("cannot write scan cache to {.file {cache_file}}: {conditionMessage(e)}",
                    class = "sas2r_cache_write_failed")
    })
  }

  # Harvest sasautos from environment and program files
  all_scanned_files <- unlist(scanned_files, use.names = FALSE) %||% character()
  for (f in all_scanned_files) {
    st <- stmt_list[[f]]
    if (!is.null(st) && nrow(st) > 0L) {
      is_env <- f %in% env_files
      kind_name <- if (is_env) "sasautos_from_environment" else "sasautos_from_program"
      paths <- extract_sasautos_options(st)
      if (length(paths) > 0L) {
        config$macro_search_path <- unique(c(config$macro_search_path, paths))
        flags_env[[length(flags_env) + 1L]] <- tibble::tibble(
          kind = kind_name, detail = paste(paths, collapse = ","))
      }
    }
  }

  if (length(flags_env) > 0L) {
    flags_list <- c(flags_list, flags_env)
  }

  files <- unlist(scanned_files, use.names = FALSE)
  if (is.null(files)) files <- character()
  scanned_origins_vec <- unlist(scanned_origins, use.names = FALSE)
  if (is.null(scanned_origins_vec)) scanned_origins_vec <- character()
  scanned_depths_vec <- unlist(scanned_depths, use.names = FALSE)
  if (is.null(scanned_depths_vec)) scanned_depths_vec <- integer()
  scanned_parent_occ_vec <- unlist(scanned_parent_occ, use.names = FALSE)
  if (is.null(scanned_parent_occ_vec)) scanned_parent_occ_vec <- character()

  include_graph <- list(
    files = if (length(files)) {
      tibble::tibble(
        file = files,
        canonical_file = include_normalize_path(files),
        origin = scanned_origins_vec,
        depth = as.integer(scanned_depths_vec),
        parent_occurrence_id = as.character(scanned_parent_occ_vec)
      )
    } else {
      empty_include_files()
    },
    occurrences = include_bind_occurrences(occurrences_list)
  )


  statements <- fast_bind(stmt_list, tibble::tibble(
    stmt_id = integer(), text = character(), first_token = character(),
    type = character(), line_start = integer(), line_end = integer(),
    unit_id = integer(), unit_type = character(), file = character(),
    origin = character()
  ))

  comments <- fast_bind(comments_list, tibble::tibble(
    comment_id = integer(), text = character(), kind = character(),
    line_start = integer(), line_end = integer(),
    char_start = integer(), char_end = integer(), unit_id = integer(),
    placement = character(), file = character(), origin = character()
  ))

  units <- fast_bind(unit_rows, tibble::tibble(
    file = character(), unit_id = integer(), unit_type = character(),
    label = character(), line_start = integer(), line_end = integer(),
    n_stmts = integer(), origin = character()
  ))

  librefs <- fast_bind(librefs_list, tibble::tibble(
    libref = character(), action = character(), engine = character(),
    path_expression = character(), path = character(), line = integer(),
    unit_id = integer(), unit_type = character(), file = character()
  ))

  includes <- fast_bind(includes_list, tibble::tibble(
    target = character(), quoted = logical(),
    line = integer(), file = character(), unit_id = integer(),
    occurrence_id = character(), target_file = character(),
    resolution_origin = character(), status = character()
  ))

  defs <- fast_bind(defs_list, tibble::tibble(
    name = character(), params = character(),
    line_start = integer(), line_end = integer(),
    file = character(), unit_id = integer()
  ))

  calls <- fast_bind(calls_list, empty_macro_calls())

  fmt_defs <- fast_bind(fmt_defs_list, tibble::tibble(
    name = character(), type = character(), line = integer(),
    unit_id = integer(), file = character()
  ))

  fmt_uses <- fast_bind(fmt_uses_list, tibble::tibble(
    name = character(), line = integer(), unit_id = integer(), file = character()
  ))

  fn_defs <- fast_bind(fn_defs_list, tibble::tibble(
    name = character(), line = integer(), unit_id = integer(), file = character()
  ))

  fn_uses <- fast_bind(fn_uses_list, tibble::tibble(
    name = character(), line = integer(), unit_id = integer(), file = character()
  ))

  resolution <- resolve_macro_calls(calls, defs, config, project_dir = root)

  # One execution context per root program, with the configured autoexec files
  # as its prologue -- never one global last-writer map across unrelated
  # programs. Built here, from the finished scan, because an included file is
  # expanded once per include occurrence that reaches it while the scan reads
  # each physical file exactly once.
  libref_registry <- build_libref_registry(
    source_events = fast_bind(libref_events_list,
                              empty_libref_source_events()),
    include_sites = fast_bind(include_sites_list,
                              empty_libref_include_sites()),
    scanned_files = files,
    root_programs = program_files,
    env_files = env_files,
    libraries = config$libraries,
    project_root = root,
    anchors = identity_anchors
  )
  for (ctx in libref_registry$truncated_contexts) {
    flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
      kind = "libref_context_truncated", detail = ctx)
  }

  lineage <- fast_bind(lineage_list, tibble::tibble(
    unit_id = integer(), dataset = character(),
    role = character(), line = integer(),
    file = character(), proc = character()
  ))
  bound <- attach_lineage_bindings(lineage, libref_registry)
  lineage <- bound$lineage

  draft_proj <- structure(list(
    units = units,
    statements = statements,
    lineage = lineage,
    macros = list(defs = defs, calls = calls, resolution = resolution),
    include_graph = include_graph,
    config = config,
    dependency_facts = list(
      includes = includes,
      include_graph = include_graph,
      macros = list(defs = defs, calls = calls, resolution = resolution),
      lineage = lineage,
      librefs = librefs,
      libref_registry = libref_registry,
      formats = list(defs = fmt_defs, uses = fmt_uses),
      functions = list(defs = fn_defs, uses = fn_uses),
      env_files = env_files,
      root_programs = program_files
    )
  ), class = "sas2r_project")

  output_contracts <- infer_output_contracts(draft_proj, config$outputs)

  if (nrow(units) > 0L) {
    proj_graph <- build_dependency_graph(draft_proj, output_contracts = output_contracts)
    proj_sched <- stable_dependency_schedule(proj_graph)
    if (any(proj_sched$group_kind == "cycle")) {
      flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
        kind = "dependency_cycle",
        detail = "dataset lineage contains a cycle; using file order"
      )
    }
    sched_unit_ids <- integer()
    for (cid in proj_sched$component_id) {
      c_nodes <- proj_graph$nodes[proj_graph$nodes$component_id == cid & proj_graph$nodes$type %in% c("setup", "source_unit"), ]
      if (nrow(c_nodes) > 0L) {
        c_uids <- c_nodes$original_index[!is.na(c_nodes$original_index)]
        sched_unit_ids <- c(sched_unit_ids, c_uids)
      }
    }
    remaining_uids <- setdiff(units$unit_id, sched_unit_ids)
    ord <- c(sched_unit_ids, remaining_uids)
  } else {
    ord <- integer()
  }
  for (nm in unique(resolution$name[resolution$status == "unresolved"])) {
    flags_list[[length(flags_list) + 1L]] <- tibble::tibble(kind = "unresolved_macro",
      detail = nm)
  }
  for (k in which(resolution$n_matches > 1L & !duplicated(resolution$name))) {
    flags_list[[length(flags_list) + 1L]] <- tibble::tibble(kind = "macro_shadowing",
      detail = paste0(resolution$name[k], ": ", resolution$source[k],
                      " shadows ", resolution$shadowed[k]))
  }
  refs <- unique(sub("\\..*$", "", lineage$dataset))
  # Only an assignment declares a libref. `libname adam clear;` unbinds one and
  # never says where it lived, so a clear-only project is exactly the case the
  # undeclared report exists for and must not be silenced by its own clear.
  assigned <- librefs$libref[librefs$action == "assign"]
  declared <- unique(c("work", assigned, tolower(names(config$libraries))))
  for (lr in setdiff(refs, declared)) {
    flags_list[[length(flags_list) + 1L]] <- tibble::tibble(kind = "libref_undeclared", detail = lr)
  }
  # A source binding whose engine names no local directory cannot supply a
  # location, so without a configured entry the libref quietly ends up unbound
  # even though the program declared it. The downgrade is legitimate; being
  # silent about it is not.
  for (detail in libref_unsupported_engine_details(libref_registry,
                                                   bound$records)) {
    flags_list[[length(flags_list) + 1L]] <- tibble::tibble(
      kind = "libref_engine_unsupported", detail = detail)
  }

  flags <- if (length(flags_list) > 0L) {
    do.call(rbind, flags_list)
  } else {
    tibble::tibble(kind = character(), detail = character())
  }

  source_hashes <- if (length(files) > 0L) {
    vapply(files, function(f) {
      if (file.exists(f)) {
        txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
        migration_hash(txt)
      } else {
        migration_hash("")
      }
    }, character(1))
  } else {
    stats::setNames(character(), character())
  }

  dependency_facts <- list(
    includes = includes,
    include_graph = include_graph,
    macros = list(defs = defs, calls = calls, resolution = resolution),
    lineage = lineage,
    librefs = librefs,
    libref_registry = libref_registry,
    formats = list(defs = fmt_defs, uses = fmt_uses),
    functions = list(defs = fn_defs, uses = fn_uses),
    env_files = env_files,
    root_programs = program_files,
    output_contracts = output_contracts
  )

  structure(list(
    files = tibble::tibble(
      file = files,
      origin = scanned_origins_vec,
      n_statements = vapply(files, function(f) nrow(stmt_list[[f]]), integer(1))
    ),
    units = units, statements = statements, comments = comments, librefs = librefs,
    libref_registry = libref_registry,
    includes = includes, include_graph = include_graph,
    macros = list(defs = defs, calls = calls, resolution = resolution),
    lineage = lineage, output_contracts = output_contracts, order = ord, flags = flags, config = config,
    dependency_facts = dependency_facts, source_hashes = source_hashes,
    project_dir = root
  ), class = "sas2r_project")
}
