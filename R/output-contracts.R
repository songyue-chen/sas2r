# Infer actual final datasets and TLF output contracts

OUTPUT_CONTRACT_COLUMNS <- c(
  "target_id", "target_key", "kind", "logical_name", "path_expression",
  "resolution", "producer_node_id", "required", "reference_path",
  "assertions", "source_file", "line", "reason"
)

OUTPUT_TLF_EXTENSIONS <- c("pdf", "rtf", "html", "htm", "png", "jpg", "jpeg")

#' Canonical empty output contracts tibble
#' @return A 0-row tibble with all 13 output contract columns.
#' @noRd
empty_output_contracts <- function() {
  tibble::tibble(
    target_id = character(),
    target_key = character(),
    kind = character(),
    logical_name = character(),
    path_expression = character(),
    resolution = character(),
    producer_node_id = character(),
    required = logical(),
    reference_path = character(),
    assertions = list(),
    source_file = character(),
    line = integer(),
    reason = character()
  )
}

#' Strip quotes from a character string
#' @noRd
strip_quotes <- function(x) {
  if (is.null(x) || !length(x)) return("")
  gsub("^['\"]|['\"]$", "", trimws(as.character(x)))
}

#' Classify output target key as TLF or dataset by file extension
#' @noRd
classify_target_kind <- function(key) {
  ext <- tolower(tools::file_ext(key))
  if (ext %in% OUTPUT_TLF_EXTENSIONS) "tlf" else "dataset"
}

#' Validate user-specified outputs configuration or overrides
#'
#' @param overrides Character vector, named list, or NULL.
#' @return Validated overrides structure.
#' @noRd
validate_output_overrides <- function(overrides) {
  if (is.null(overrides)) return(NULL)

  if (is.character(overrides)) {
    if (anyNA(overrides) || !all(nzchar(overrides))) {
      cli::cli_abort(
        "outputs overrides character vector must not contain NA or empty strings",
        class = "sas2r_output_contract_error"
      )
    }
    return(overrides)
  }

  if (is.list(overrides)) {
    allowed <- c("datasets", "tlfs", "references", "assertions")
    unknown <- setdiff(names(overrides), allowed)
    if (length(unknown) > 0L) {
      cli::cli_abort(
        c("Unknown or unsupported field in outputs configuration: {.val {unknown}}.",
          "i" = "Allowed: {.val {allowed}}."),
        class = "sas2r_output_contract_error"
      )
    }

    if (!is.null(overrides$datasets)) {
      if (!is.character(overrides$datasets) || anyNA(overrides$datasets)) {
        cli::cli_abort(
          "outputs$datasets must be a character vector",
          class = "sas2r_output_contract_error"
        )
      }
    }

    if (!is.null(overrides$tlfs)) {
      if (!is.character(overrides$tlfs) || anyNA(overrides$tlfs)) {
        cli::cli_abort(
          "outputs$tlfs must be a character vector",
          class = "sas2r_output_contract_error"
        )
      }
    }

    if (!is.null(overrides$references)) {
      if ((!is.character(overrides$references) && !is.list(overrides$references)) ||
          is.null(names(overrides$references))) {
        cli::cli_abort(
          "outputs$references must be a named vector or list",
          class = "sas2r_output_contract_error"
        )
      }
    }

    if (!is.null(overrides$assertions)) {
      if (!is.list(overrides$assertions) || is.null(names(overrides$assertions))) {
        cli::cli_abort(
          "outputs$assertions must be a named list",
          class = "sas2r_output_contract_error"
        )
      }
    }

    return(overrides)
  }

  cli::cli_abort(
    "outputs must be NULL, a character vector, or a structured named list",
    class = "sas2r_output_contract_error"
  )
}

#' Merge user overrides into inferred output contracts by target_key
#'
#' @param contracts Inferred output contracts tibble.
#' @param overrides Validated user overrides (NULL, character vector, or list).
#' @return Merged output contracts tibble.
#' @noRd
merge_output_overrides <- function(contracts, overrides = NULL) {
  if (is.null(overrides)) return(contracts)

  records <- if (nrow(contracts) > 0L) {
    lapply(seq_len(nrow(contracts)), function(i) {
      list(
        target_id = contracts$target_id[i],
        target_key = contracts$target_key[i],
        kind = contracts$kind[i],
        logical_name = contracts$logical_name[i],
        path_expression = contracts$path_expression[i],
        resolution = contracts$resolution[i],
        producer_node_id = contracts$producer_node_id[i],
        required = contracts$required[i],
        reference_path = contracts$reference_path[i],
        assertions = contracts$assertions[[i]],
        source_file = contracts$source_file[i],
        line = contracts$line[i],
        reason = contracts$reason[i]
      )
    })
  } else {
    list()
  }

  find_record_idx <- function(key) {
    if (!length(records)) return(integer(0))
    keys <- vapply(records, `[[`, character(1), "target_key")
    which(keys == key)
  }

  add_or_update_target <- function(raw_key, kind = NULL, ref_path = NA_character_,
                                   assertions = list(), reason = "user_override") {
    clean_p <- gsub("\\\\", "/", trimws(raw_key))
    norm_k <- tolower(clean_p)
    target_kind <- kind %||% classify_target_kind(norm_k)
    final_key <- if (target_kind == "dataset") norm_ds(norm_k) else norm_k

    idx <- find_record_idx(final_key)
    if (length(idx) > 0L) {
      i <- idx[1L]
      records[[i]]$required <<- TRUE
      if (!is.na(ref_path) && nzchar(ref_path)) {
        records[[i]]$reference_path <<- ref_path
      }
      if (length(assertions) > 0L) {
        records[[i]]$assertions <<- assertions
      }
    } else {
      is_dyn <- grepl("&", clean_p)
      records[[length(records) + 1L]] <<- list(
        target_id = "",
        target_key = final_key,
        kind = target_kind,
        logical_name = if (target_kind == "dataset") final_key else clean_p,
        path_expression = if (target_kind == "tlf") clean_p else NA_character_,
        resolution = if (is_dyn) "dynamic" else "declared",
        producer_node_id = NA_character_,
        required = TRUE,
        reference_path = ref_path,
        assertions = assertions,
        source_file = NA_character_,
        line = NA_integer_,
        reason = reason
      )
    }
  }

  if (is.character(overrides)) {
    for (k in overrides) {
      add_or_update_target(k)
    }
  } else if (is.list(overrides)) {
    # 1. Datasets
    if (!is.null(overrides$datasets)) {
      for (ds in overrides$datasets) {
        add_or_update_target(ds, kind = "dataset")
      }
    }
    # 2. TLFs
    if (!is.null(overrides$tlfs)) {
      for (tlf in overrides$tlfs) {
        add_or_update_target(tlf, kind = "tlf")
      }
    }
    # 3. References
    if (!is.null(overrides$references)) {
      for (ref_name in names(overrides$references)) {
        ref_val <- as.character(overrides$references[[ref_name]])
        add_or_update_target(ref_name, ref_path = ref_val)
      }
    }
    # 4. Assertions
    if (!is.null(overrides$assertions)) {
      for (ast_name in names(overrides$assertions)) {
        ast_val <- overrides$assertions[[ast_name]]
        if (!is.list(ast_val)) ast_val <- as.list(ast_val)
        add_or_update_target(ast_name, assertions = ast_val)
      }
    }
  }

  if (length(records) == 0L) return(empty_output_contracts())

  target_keys <- vapply(records, `[[`, character(1), "target_key")
  kinds <- vapply(records, `[[`, character(1), "kind")
  log_names <- vapply(records, `[[`, character(1), "logical_name")
  path_exprs <- vapply(records, function(r) as.character(r$path_expression %||% NA_character_), character(1))
  resolutions <- vapply(records, `[[`, character(1), "resolution")
  producer_nodes <- vapply(records, function(r) as.character(r$producer_node_id %||% NA_character_), character(1))
  required_vec <- vapply(records, `[[`, logical(1), "required")
  ref_paths <- vapply(records, function(r) as.character(r$reference_path %||% NA_character_), character(1))
  assertions_list <- lapply(records, function(r) r$assertions %||% list())
  source_files <- vapply(records, function(r) as.character(r$source_file %||% NA_character_), character(1))
  lines_vec <- vapply(records, function(r) as.integer(r$line %||% NA_integer_), integer(1))
  reasons <- vapply(records, `[[`, character(1), "reason")

  target_ids <- sprintf("target_%04d", seq_along(records))

  out <- tibble::tibble(
    target_id = target_ids,
    target_key = target_keys,
    kind = kinds,
    logical_name = log_names,
    path_expression = path_exprs,
    resolution = resolutions,
    producer_node_id = producer_nodes,
    required = required_vec,
    reference_path = ref_paths,
    assertions = assertions_list,
    source_file = source_files,
    line = lines_vec,
    reason = reasons
  )

  class(out) <- c("sas2r_output_contracts", "tbl_df", "tbl", "data.frame")
  out
}

#' Infer output contracts for terminal datasets and actual TLF artifact sites
#'
#' Scans project lineage and statements to infer terminal dataset writes and
#' literal PDF, RTF, HTML, and image output sites. Merges user overrides
#' by normalized `target_key`.
#'
#' @param project A `sas2r_project` object or NULL.
#' @param overrides Optional character vector or structured named list of user overrides.
#' @return A `sas2r_output_contracts` tibble with canonical 13 columns.
#' @noRd
infer_output_contracts <- function(project, overrides = NULL) {
  if (is.null(project) || !inherits(project, "sas2r_project")) {
    validated_overrides <- validate_output_overrides(overrides)
    if (is.null(validated_overrides)) {
      return(empty_output_contracts())
    }
    return(merge_output_overrides(empty_output_contracts(), validated_overrides))
  }

  if (is.null(overrides) && !is.null(project$config$outputs)) {
    overrides <- project$config$outputs
  }
  validated_overrides <- validate_output_overrides(overrides)

  stmts <- project$statements %||% tibble::tibble()
  units <- project$units %||% tibble::tibble()
  lineage <- project$lineage %||% tibble::tibble()

  records <- list()
  seen_keys <- character()

  # Helper: register inferred record
  add_inferred_record <- function(target_key, kind, logical_name, path_expression,
                                  resolution, producer_node_id, source_file, line, reason) {
    clean_k <- tolower(gsub("\\\\", "/", target_key))
    if (clean_k %in% seen_keys) {
      idx <- which(seen_keys == clean_k)[1L]
      if (records[[idx]]$resolution == "dynamic" && resolution != "dynamic") {
        records[[idx]]$resolution <<- resolution
        records[[idx]]$path_expression <<- path_expression
      }
      if (is.na(records[[idx]]$source_file) && !is.na(source_file)) {
        records[[idx]]$source_file <<- source_file
        records[[idx]]$line <<- line
        records[[idx]]$producer_node_id <<- producer_node_id
      }
      return(invisible(NULL))
    }
    seen_keys <<- c(seen_keys, clean_k)
    records[[length(records) + 1L]] <<- list(
      target_id = "",
      target_key = clean_k,
      kind = kind,
      logical_name = logical_name,
      path_expression = path_expression,
      resolution = resolution,
      producer_node_id = producer_node_id,
      required = TRUE,
      reference_path = NA_character_,
      assertions = list(),
      source_file = as.character(source_file %||% NA_character_),
      line = as.integer(line %||% NA_integer_),
      reason = reason
    )
  }

  # Helper: node ID for unit
  unit_node_id <- function(u_id, f, l) {
    if (is.na(u_id)) return(NA_character_)
    paste0("node_unit_", substr(migration_hash(list(
      file = f, line = l, unit_id = u_id, type = "source_unit"
    )), 1L, 16L))
  }

  # 1. Infer terminal persistent dataset writes from lineage
  if (nrow(lineage) > 0L) {
    creates <- lineage[lineage$role == "creates", ]
    reads <- lineage[lineage$role == "reads", ]

    if (nrow(creates) > 0L) {
      is_persistent <- sub("\\..*$", "", creates$dataset) != "work"
      persistent_creates <- creates[is_persistent, ]

      unique_persistent_ds <- unique(persistent_creates$dataset)
      for (ds in unique_persistent_ds) {
        ds_readers <- unique(reads$unit_id[reads$dataset == ds])
        is_intermediate_for_persistent <- FALSE
        if (length(ds_readers) > 0L) {
          reader_creates <- creates[creates$unit_id %in% ds_readers, ]
          if (nrow(reader_creates) > 0L) {
            reader_created_persistent <- reader_creates$dataset[sub("\\..*$", "", reader_creates$dataset) != "work"]
            if (length(reader_created_persistent) > 0L && !all(reader_created_persistent == ds)) {
              is_intermediate_for_persistent <- TRUE
            }
          }
        }

        if (!is_intermediate_for_persistent) {
          c_rows <- persistent_creates[persistent_creates$dataset == ds, ]
          last_c <- c_rows[nrow(c_rows), ]
          p_node_id <- unit_node_id(last_c$unit_id, last_c$file, last_c$line)
          norm_name <- norm_ds(tolower(ds))

          add_inferred_record(
            target_key = norm_name,
            kind = "dataset",
            logical_name = norm_name,
            path_expression = NA_character_,
            resolution = "resolved",
            producer_node_id = p_node_id,
            source_file = last_c$file,
            line = last_c$line,
            reason = "terminal_lineage"
          )
        }
      }
    }
  }

  # Harvest filerefs
  filerefs <- extract_filerefs(stmts)

  # 2. Discover TLF artifact destinations from statements
  if (nrow(stmts) > 0L) {
    for (k in seq_len(nrow(stmts))) {
      txt <- stmts$text[k]
      tok <- tolower(stmts$first_token[k])
      src_f <- as.character(stmts$file[k] %||% "")
      l_start <- as.integer(stmts$line_start[k] %||% 0L)
      u_id <- if ("unit_id" %in% names(stmts)) as.integer(stmts$unit_id[k]) else NA_integer_
      p_node_id <- unit_node_id(u_id, src_f, l_start)

      # a) FILENAME statements pointing to TLFs
      if (tok == "filename") {
        m_fn <- regmatches(txt, regexec("^filename\\s+([A-Za-z_]\\w*)\\s+['\"]([^'\"]+)['\"]", txt, ignore.case = TRUE, perl = TRUE))[[1]]
        if (length(m_fn) >= 3L && nzchar(m_fn[3])) {
          fn_path <- m_fn[3]
          ext <- tolower(tools::file_ext(fn_path))
          if (ext %in% OUTPUT_TLF_EXTENSIONS) {
            clean_p <- gsub("\\\\", "/", fn_path)
            is_dyn <- grepl("&", clean_p)
            add_inferred_record(
              target_key = clean_p,
              kind = "tlf",
              logical_name = clean_p,
              path_expression = fn_path,
              resolution = if (is_dyn) "dynamic" else "resolved",
              producer_node_id = p_node_id,
              source_file = src_f,
              line = l_start,
              reason = "filename_tlf"
            )
          }
        }
        next
      }

      # b) ODS statements
      if (tok == "ods") {
        # Check if closing
        if (grepl("^ods\\s+([A-Za-z_]\\w*|_all_)\\s+close\\b", txt, ignore.case = TRUE)) {
          next
        }

        # ODS GRAPHICS
        if (grepl("^ods\\s+graphics\\b", txt, ignore.case = TRUE)) {
          m_img <- regmatches(txt, regexec("imagename\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_.-]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
          img_name <- if (length(m_img) >= 2L && nzchar(m_img[2])) m_img[2] else if (length(m_img) >= 3L && nzchar(m_img[3])) m_img[3] else ""
          m_fmt <- regmatches(txt, regexec("(?:imagefmt|outputfmt)\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
          img_fmt <- if (length(m_fmt) >= 2L && nzchar(m_fmt[2])) m_fmt[2] else if (length(m_fmt) >= 3L && nzchar(m_fmt[3])) m_fmt[3] else ""
          m_dir <- regmatches(txt, regexec("(?:outdir|gpath|path)\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_./\\\\]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
          img_dir <- if (length(m_dir) >= 2L && nzchar(m_dir[2])) m_dir[2] else if (length(m_dir) >= 3L && nzchar(m_dir[3])) m_dir[3] else ""
          m_outf <- regmatches(txt, regexec("outfile\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_./\\\\]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
          img_outf <- if (length(m_outf) >= 2L && nzchar(m_outf[2])) m_outf[2] else if (length(m_outf) >= 3L && nzchar(m_outf[3])) m_outf[3] else ""

          if (nzchar(img_outf)) {
            raw_p <- img_outf
            clean_p <- gsub("\\\\", "/", raw_p)
            is_dyn <- grepl("&", clean_p)
            add_inferred_record(
              target_key = clean_p,
              kind = "tlf",
              logical_name = clean_p,
              path_expression = raw_p,
              resolution = if (is_dyn) "dynamic" else "resolved",
              producer_node_id = p_node_id,
              source_file = src_f,
              line = l_start,
              reason = "ods_graphics"
            )
          } else if (nzchar(img_name)) {
            fmt_val <- if (nzchar(img_fmt)) img_fmt else "png"
            if (!grepl("\\.[A-Za-z0-9]+$", img_name)) img_name <- paste0(img_name, ".", fmt_val)
            raw_p <- if (nzchar(img_dir)) file.path(img_dir, img_name) else img_name
            clean_p <- gsub("\\\\", "/", raw_p)
            is_dyn <- grepl("&", clean_p)
            add_inferred_record(
              target_key = clean_p,
              kind = "tlf",
              logical_name = clean_p,
              path_expression = raw_p,
              resolution = if (is_dyn) "dynamic" else "resolved",
              producer_node_id = p_node_id,
              source_file = src_f,
              line = l_start,
              reason = "ods_graphics"
            )
          }
          next
        }

        # ODS PDF/RTF/HTML/LISTING/DOCUMENT/etc.
        m_dest <- regmatches(txt, regexec("^ods\\s+([A-Za-z_]\\w*)", txt, ignore.case = TRUE, perl = TRUE))[[1]]
        dest <- if (length(m_dest) >= 2L) tolower(m_dest[2]) else "tlf"

        m_f <- regmatches(txt, regexec("(?:file|body|out|filename)\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_&./\\\\]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
        if (length(m_f) >= 2L) {
          raw_val <- if (nzchar(m_f[2])) m_f[2] else if (length(m_f) >= 3L && nzchar(m_f[3])) m_f[3] else ""
          if (nzchar(raw_val)) {
            if (!grepl("['\"/\\\\]|\\.", raw_val) && tolower(raw_val) %in% names(filerefs)) {
              clean_p <- filerefs[[tolower(raw_val)]]
              raw_p <- clean_p
            } else {
              clean_p <- raw_val
              raw_p <- raw_val
            }

            m_pdir <- regmatches(txt, regexec("(?:path|gpath)\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_./\\\\]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
            pdir <- if (length(m_pdir) >= 2L && nzchar(m_pdir[2])) m_pdir[2] else if (length(m_pdir) >= 3L && nzchar(m_pdir[3])) m_pdir[3] else ""
            if (nzchar(pdir) && !is_anchored_path(clean_p) && !startsWith(clean_p, paste0(pdir, "/"))) {
              clean_p <- file.path(pdir, clean_p)
            }

            clean_p <- gsub("\\\\", "/", clean_p)
            is_dyn <- grepl("&", clean_p)
            add_inferred_record(
              target_key = clean_p,
              kind = "tlf",
              logical_name = clean_p,
              path_expression = raw_p,
              resolution = if (is_dyn) "dynamic" else "resolved",
              producer_node_id = p_node_id,
              source_file = src_f,
              line = l_start,
              reason = paste0("ods_", dest)
            )
          }
        }
        next
      }

      # c) GOPTIONS statements
      if (tok == "goptions") {
        m_gsf <- regmatches(txt, regexec("gsfname\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_&]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
        gsf <- if (length(m_gsf) >= 2L && nzchar(m_gsf[2])) m_gsf[2] else if (length(m_gsf) >= 3L && nzchar(m_gsf[3])) m_gsf[3] else ""
        if (nzchar(gsf)) {
          if (tolower(gsf) %in% names(filerefs)) {
            clean_p <- filerefs[[tolower(gsf)]]
            raw_p <- clean_p
          } else {
            clean_p <- gsf
            raw_p <- gsf
          }
          clean_p <- gsub("\\\\", "/", clean_p)
          is_dyn <- grepl("&", clean_p)
          add_inferred_record(
            target_key = clean_p,
            kind = "tlf",
            logical_name = clean_p,
            path_expression = raw_p,
            resolution = if (is_dyn) "dynamic" else "resolved",
            producer_node_id = p_node_id,
            source_file = src_f,
            line = l_start,
            reason = "goptions"
          )
        }
        next
      }

      # d) PROC EXPORT statements
      if (tok == "proc" && grepl("^proc\\s+export\\b", txt, ignore.case = TRUE)) {
        m_exp <- regmatches(txt, regexec("outfile\\s*=\\s*(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_&./\\\\]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
        exp_f <- if (length(m_exp) >= 2L && nzchar(m_exp[2])) m_exp[2] else if (length(m_exp) >= 3L && nzchar(m_exp[3])) m_exp[3] else ""
        if (nzchar(exp_f)) {
          if (tolower(exp_f) %in% names(filerefs)) {
            clean_p <- filerefs[[tolower(exp_f)]]
            raw_p <- clean_p
          } else {
            clean_p <- exp_f
            raw_p <- exp_f
          }
          clean_p <- gsub("\\\\", "/", clean_p)
          is_dyn <- grepl("&", clean_p)
          ext <- tolower(tools::file_ext(clean_p))
          kind <- if (ext %in% OUTPUT_TLF_EXTENSIONS) "tlf" else "dataset"
          add_inferred_record(
            target_key = clean_p,
            kind = kind,
            logical_name = clean_p,
            path_expression = raw_p,
            resolution = if (is_dyn) "dynamic" else "resolved",
            producer_node_id = p_node_id,
            source_file = src_f,
            line = l_start,
            reason = "proc_export"
          )
        }
        next
      }

      # e) DATA step FILE statements
      if (tok == "file") {
        m_file <- regmatches(txt, regexec("^file\\s+(?:['\"]([^'\"]+)['\"]|([A-Za-z0-9_&./\\\\]+))", txt, ignore.case = TRUE, perl = TRUE))[[1]]
        file_dest <- if (length(m_file) >= 2L && nzchar(m_file[2])) m_file[2] else if (length(m_file) >= 3L && nzchar(m_file[3])) m_file[3] else ""
        if (nzchar(file_dest) && tolower(file_dest) != "print") {
          if (tolower(file_dest) %in% names(filerefs)) {
            clean_p <- filerefs[[tolower(file_dest)]]
            raw_p <- clean_p
          } else {
            clean_p <- file_dest
            raw_p <- file_dest
          }
          clean_p <- gsub("\\\\", "/", clean_p)
          is_dyn <- grepl("&", clean_p)
          ext <- tolower(tools::file_ext(clean_p))
          kind <- if (ext %in% OUTPUT_TLF_EXTENSIONS) "tlf" else "dataset"
          add_inferred_record(
            target_key = clean_p,
            kind = kind,
            logical_name = clean_p,
            path_expression = raw_p,
            resolution = if (is_dyn) "dynamic" else "resolved",
            producer_node_id = p_node_id,
            source_file = src_f,
            line = l_start,
            reason = "data_file"
          )
        }
        next
      }
    }
  }

  inferred_tbl <- if (length(records) > 0L) {
    tibble::tibble(
      target_id = sprintf("target_%04d", seq_along(records)),
      target_key = vapply(records, `[[`, character(1), "target_key"),
      kind = vapply(records, `[[`, character(1), "kind"),
      logical_name = vapply(records, `[[`, character(1), "logical_name"),
      path_expression = vapply(records, function(r) as.character(r$path_expression %||% NA_character_), character(1)),
      resolution = vapply(records, `[[`, character(1), "resolution"),
      producer_node_id = vapply(records, function(r) as.character(r$producer_node_id %||% NA_character_), character(1)),
      required = vapply(records, `[[`, logical(1), "required"),
      reference_path = vapply(records, function(r) as.character(r$reference_path %||% NA_character_), character(1)),
      assertions = lapply(records, function(r) r$assertions %||% list()),
      source_file = vapply(records, function(r) as.character(r$source_file %||% NA_character_), character(1)),
      line = vapply(records, function(r) as.integer(r$line %||% NA_integer_), integer(1)),
      reason = vapply(records, `[[`, character(1), "reason")
    )
  } else {
    empty_output_contracts()
  }

  merge_output_overrides(inferred_tbl, validated_overrides)
}

#' Write output contracts to JSON file
#'
#' @param contracts A `sas2r_output_contracts` tibble.
#' @param path Output JSON file path.
#' @return The file path invisibly.
#' @noRd
write_output_contracts <- function(contracts, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(
    unclass(contracts),
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows",
    null = "null"
  )
  writeLines(as.character(json), path)
  invisible(path)
}

#' @export
print.sas2r_output_contracts <- function(x, ...) {
  cli::cat_line(cli::rule(left = "sas2r output contracts"))
  n <- nrow(x)
  cli::cat_line(paste0("Total targets: ", n))
  if (n > 0L) {
    ds_n <- sum(x$kind == "dataset")
    tlf_n <- sum(x$kind == "tlf")
    cli::cat_line(paste0("  Datasets: ", ds_n, ", TLFs: ", tlf_n))
  }
  invisible(x)
}
