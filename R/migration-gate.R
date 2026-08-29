#' Migration Gate: Assess Final Outputs and Deterministic Four-State Bundle Status
#'
#' Evaluates terminal datasets, structural TLF contracts (PDF, RTF, HTML, PNG, JPEG),
#' source-derived lineage evidence, and derives canonical four-state bundle status:
#' blocked, needs_review, migration_ready, validated.

#' Canonical bundle status enum
#' @noRd
BUNDLE_STATUSES <- c(
  "blocked",
  "needs_review",
  "migration_ready",
  "validated"
)

#' Locate an output target candidate file within an attempt directory structure
#'
#' @param contract An output contract list or row.
#' @param attempt A migration attempt record, attempt directory, or list.
#' @return Canonical file path if found, or NA_character_.
#' @noRd
find_attempt_candidate_file <- function(contract, attempt) {
  if (is.null(contract) || is.null(attempt)) return(NA_character_)

  # If candidate_data is directly provided as data frame
  if (is.data.frame(attempt)) return(NA_character_)

  # Extract attempt directory paths
  attempt_dir <- if (is.list(attempt)) {
    attempt$attempt_dir %||% (if (!is.null(attempt$paths)) attempt$paths$attempts else NULL)
  } else if (is.character(attempt) && length(attempt) == 1L) {
    attempt
  } else {
    NULL
  }

  outputs_dir <- if (is.list(attempt)) attempt$outputs_dir else NULL
  work_dir <- if (is.list(attempt)) attempt$work_dir else NULL

  search_dirs <- unique(c(outputs_dir, attempt_dir, work_dir))
  search_dirs <- search_dirs[!is.na(search_dirs) & nzchar(search_dirs) & dir.exists(search_dirs)]

  t_key <- if (is.data.frame(contract)) contract$target_key[1L] else contract$target_key %||% ""
  l_name <- if (is.data.frame(contract)) contract$logical_name[1L] else contract$logical_name %||% ""
  kind <- if (is.data.frame(contract)) contract$kind[1L] else contract$kind %||% "dataset"

  t_key <- gsub("\\\\", "/", trimws(as.character(t_key)))
  l_name <- gsub("\\\\", "/", trimws(as.character(l_name)))

  if (kind == "dataset") {
    libref <- sub("\\..*$", "", l_name)
    stem <- sub("^.*\\.", "", l_name)
    if (!nzchar(stem)) stem <- tools::file_path_sans_ext(basename(t_key))

    exts <- c("rds", "xpt", "sas7bdat")

    # Direct candidate checks in search roots
    for (sdir in search_dirs) {
      for (ext in exts) {
        cands <- c(
          file.path(sdir, libref, paste0(stem, ".", ext)),
          file.path(sdir, paste0(stem, ".", ext)),
          file.path(sdir, paste0(l_name, ".", ext)),
          file.path(sdir, paste0(libref, ".", stem, ".", ext))
        )
        for (cand in cands) {
          if (file.exists(cand) && !file.info(cand)$isdir) {
            return(normalizePath(cand, winslash = "/", mustWork = FALSE))
          }
        }
      }
    }

    # Recursive search within attempt directories (excluding bundle and logs)
    for (sdir in search_dirs) {
      all_f <- list.files(sdir, recursive = TRUE, full.names = TRUE)
      for (ext in exts) {
        pattern <- paste0("(?i)(^|/)", stem, "\\.", ext, "$")
        hits <- all_f[grepl(pattern, all_f)]
        hits <- hits[!grepl("/bundle/|/logs/", hits)]
        if (length(hits) > 0L) {
          return(normalizePath(hits[1L], winslash = "/", mustWork = FALSE))
        }
      }
    }
  } else {
    # TLF target
    for (sdir in search_dirs) {
      cands <- c(
        file.path(sdir, t_key),
        file.path(sdir, basename(t_key)),
        file.path(sdir, l_name),
        file.path(sdir, basename(l_name))
      )
      for (cand in cands) {
        if (file.exists(cand) && !file.info(cand)$isdir) {
          return(normalizePath(cand, winslash = "/", mustWork = FALSE))
        }
      }
    }

    # Recursive search within attempt directories
    for (sdir in search_dirs) {
      all_f <- list.files(sdir, recursive = TRUE, full.names = TRUE)
      pattern <- paste0("(?i)(^|/)", basename(t_key), "$")
      hits <- all_f[grepl(pattern, all_f)]
      hits <- hits[!grepl("/bundle/|/logs/", hits)]
      if (length(hits) > 0L) {
        return(normalizePath(hits[1L], winslash = "/", mustWork = FALSE))
      }
    }
  }

  NA_character_
}

#' Assess a dataset target output
#'
#' Evaluates file existence, non-empty content, parsability as a dataset,
#' schema comparison, keys, required columns, numeric tolerances, and optional
#' reference dataset comparison. Returns explicit checks and differences.
#'
#' @param contract Output contract specification list or 1-row data frame.
#' @param attempt Migration attempt record, attempt directory, or list.
#' @param comparison_rules Optional list of comparison rules and overrides.
#' @return Named list containing explicit checks, differences, passed flag, and target status.
#' @noRd
assess_dataset_target <- function(contract, attempt, comparison_rules = list()) {
  t_id <- if (is.data.frame(contract)) contract$target_id[1L] else contract$target_id %||% ""
  t_key <- if (is.data.frame(contract)) contract$target_key[1L] else contract$target_key %||% ""
  l_name <- if (is.data.frame(contract)) contract$logical_name[1L] else contract$logical_name %||% ""
  required <- if (is.data.frame(contract)) isTRUE(contract$required[1L]) else isTRUE(contract$required)

  assertions <- if (is.data.frame(contract) && is.list(contract$assertions)) {
    if (length(contract$assertions) > 0L) contract$assertions[[1L]] else list()
  } else {
    contract$assertions %||% list()
  }

  checks <- list()
  diffs <- list(mismatches = list(), cosmetic = list(), structure = list())

  cand_data <- NULL
  cand_path <- NA_character_

  # Check if candidate data frame was directly passed
  if (is.data.frame(attempt)) {
    cand_data <- attempt
    checks$candidate_exists <- list(name = "candidate_exists", passed = TRUE, details = "In-memory data frame provided")
    checks$candidate_readable <- list(name = "candidate_readable", passed = TRUE, details = "In-memory data frame readable")
  } else if (is.list(attempt) && is.data.frame(attempt$candidate_data)) {
    cand_data <- attempt$candidate_data
    checks$candidate_exists <- list(name = "candidate_exists", passed = TRUE, details = "In-memory candidate_data provided")
    checks$candidate_readable <- list(name = "candidate_readable", passed = TRUE, details = "In-memory candidate_data readable")
  } else {
    cand_path <- find_attempt_candidate_file(contract, attempt)
    if (is.na(cand_path) || !file.exists(cand_path)) {
      checks$candidate_exists <- list(
        name = "candidate_exists",
        passed = FALSE,
        details = "Candidate dataset file not found"
      )
      return(list(
        target_id = t_id,
        target_key = t_key,
        kind = "dataset",
        required = required,
        passed = FALSE,
        status = "missing_candidate",
        has_reference = FALSE,
        reference_passed = FALSE,
        has_assertions = length(assertions) > 0L,
        checks = checks,
        differences = diffs,
        candidate_path = NA_character_,
        reference_path = NA_character_
      ))
    }

    checks$candidate_exists <- list(
      name = "candidate_exists",
      passed = TRUE,
      details = "Candidate file exists",
      path = cand_path
    )

    # Read candidate dataset
    ext <- tolower(tools::file_ext(cand_path))
    cand_data <- tryCatch(
      switch(
        ext,
        rds = readRDS(cand_path),
        xpt = haven::read_xpt(cand_path),
        sas7bdat = haven::read_sas(cand_path),
        cli::cli_abort("Unsupported candidate format")
      ),
      error = function(e) NULL
    )

    if (is.null(cand_data) || !inherits(cand_data, "data.frame")) {
      checks$candidate_readable <- list(
        name = "candidate_readable",
        passed = FALSE,
        details = "Failed to read candidate dataset file"
      )
      return(list(
        target_id = t_id,
        target_key = t_key,
        kind = "dataset",
        required = required,
        passed = FALSE,
        status = "unreadable",
        has_reference = FALSE,
        reference_passed = FALSE,
        has_assertions = length(assertions) > 0L,
        checks = checks,
        differences = diffs,
        candidate_path = cand_path,
        reference_path = NA_character_
      ))
    }

    checks$candidate_readable <- list(
      name = "candidate_readable",
      passed = TRUE,
      details = "Candidate dataset read successfully"
    )
  }

  # 1. Assertions: Required columns check
  req_cols <- assertions$required_columns %||% comparison_rules$required_columns %||% character()
  if (length(req_cols) > 0L) {
    req_cols_folded <- tolower(as.character(req_cols))
    cand_cols_folded <- tolower(names(cand_data))
    missing_cols <- setdiff(req_cols_folded, cand_cols_folded)

    if (length(missing_cols) > 0L) {
      checks$required_columns <- list(
        name = "required_columns",
        passed = FALSE,
        missing_columns = missing_cols,
        details = paste0("Missing required column(s): ", paste(missing_cols, collapse = ", "))
      )
    } else {
      checks$required_columns <- list(
        name = "required_columns",
        passed = TRUE,
        details = "All required columns present"
      )
    }
  }

  # 2. Assertions: Row count / min_rows check
  if (!is.null(assertions$min_rows)) {
    min_r <- as.integer(assertions$min_rows)
    if (nrow(cand_data) < min_r) {
      checks$min_rows <- list(
        name = "min_rows",
        passed = FALSE,
        details = sprintf("Expected at least %d rows, got %d", min_r, nrow(cand_data))
      )
    } else {
      checks$min_rows <- list(
        name = "min_rows",
        passed = TRUE,
        details = "Row count requirement met"
      )
    }
  }

  # 3. Reference dataset comparison
  ref_path <- if (is.data.frame(contract)) contract$reference_path[1L] else contract$reference_path
  if (is.null(ref_path) || is.na(ref_path) || !nzchar(ref_path)) {
    ref_path <- comparison_rules$reference_path %||% comparison_rules$references[[t_key]] %||% NA_character_
  }

  has_ref <- !is.na(ref_path) && nzchar(ref_path)
  ref_passed <- FALSE

  if (has_ref) {
    if (!file.exists(ref_path)) {
      checks$reference_exists <- list(
        name = "reference_exists",
        passed = FALSE,
        details = paste0("Reference file not found at ", ref_path)
      )
    } else {
      checks$reference_exists <- list(
        name = "reference_exists",
        passed = TRUE,
        details = "Reference file exists",
        path = ref_path
      )

      ref_ext <- tolower(tools::file_ext(ref_path))
      ref_data <- tryCatch(
        switch(
          ref_ext,
          rds = readRDS(ref_path),
          xpt = haven::read_xpt(ref_path),
          sas7bdat = haven::read_sas(ref_path),
          cli::cli_abort("Unsupported reference format")
        ),
        error = function(e) NULL
      )

      if (is.null(ref_data) || !inherits(ref_data, "data.frame")) {
        checks$reference_readable <- list(
          name = "reference_readable",
          passed = FALSE,
          details = "Failed to read reference dataset"
        )
      } else {
        checks$reference_readable <- list(
          name = "reference_readable",
          passed = TRUE,
          details = "Reference dataset read successfully"
        )

        # Tolerance policy: an explicitly configured tolerance is the whole
        # policy (its unspecified half stays 0); with nothing configured the
        # defaults come from compare_profile(), defined once, so the gate and
        # direct compare_datasets() calls judge by the same rules.
        num_tol <- assertions$numeric_tolerance %||% comparison_rules$numeric_tolerance
        abs_tol <- comparison_rules$tol_abs %||% num_tol
        rel_tol <- comparison_rules$tol_rel
        if (is.null(abs_tol) && is.null(rel_tol)) {
          prof_defaults <- compare_profile()
          abs_tol <- prof_defaults$numeric$abs
          rel_tol <- prof_defaults$numeric$rel
        } else {
          abs_tol <- abs_tol %||% 0.0
          rel_tol <- rel_tol %||% 0.0
        }
        keys <- assertions$keys %||% comparison_rules$keys %||% NULL

        prof <- compare_profile(
          abs = abs_tol,
          rel = rel_tol,
          padding = "cosmetic",
          sas_null_equals_na = TRUE
        )

        # Engine alignment: configured keys steer row identity, and with none
        # configured the keys are inferred and validated -- a content-equal
        # reorder or duplicate-key permutation is no longer a wall of
        # positional cell mismatches.
        comp_res <- tryCatch(
          compare_datasets_aligned(ref_data, cand_data, profile = prof, keys = keys),
          error = function(e) NULL
        )

        if (!is.null(comp_res)) {
          ref_passed <- isTRUE(comp_res$passed)
          checks$reference_comparison <- list(
            name = "reference_comparison",
            passed = ref_passed,
            details = if (ref_passed) "Comparison passed within tolerance" else "Differences observed",
            summary = comp_res$summary
          )
          diffs$mismatches <- comp_res$details
          diffs$cosmetic <- comp_res$cosmetic
          diffs$structure <- comp_res$structure
          # The redacted digest is the only comparison artifact that may cross
          # to an LLM (names, counts, magnitudes, pattern hints -- never cell
          # values or row numbers). It is derived here, where the full
          # comparison object exists; the repair loop forwards it in place of
          # the raw mismatch details.
          diffs$digest <- tryCatch(
            unclass(diff_digest(comp_res, label = t_key)),
            error = function(e) NULL
          )
        } else {
          checks$reference_comparison <- list(
            name = "reference_comparison",
            passed = FALSE,
            details = "compare_datasets failed"
          )
        }
      }
    }
  }

  all_checks_passed <- all(vapply(checks, function(chk) {
    if (is.null(chk$passed) || is.na(chk$passed)) TRUE else isTRUE(chk$passed)
  }, logical(1)))

  status <- if (all_checks_passed) {
    "passed"
  } else if (isFALSE(checks$candidate_exists$passed)) {
    "missing_candidate"
  } else if (isFALSE(checks$candidate_readable$passed)) {
    "unreadable"
  } else {
    "failed"
  }

  list(
    target_id = t_id,
    target_key = t_key,
    kind = "dataset",
    required = required,
    passed = all_checks_passed,
    status = status,
    has_reference = has_ref && isTRUE(checks$reference_exists$passed),
    reference_passed = if (has_ref) ref_passed else FALSE,
    has_assertions = length(assertions) > 0L,
    checks = checks,
    differences = diffs,
    candidate_path = cand_path,
    reference_path = if (has_ref) ref_path else NA_character_
  )
}

#' Assess a TLF output target with format-specific structural checks
#'
#' Checks PDF (header/trailer/text), RTF (envelope/text), HTML (structure/tables/text),
#' and PNG/JPEG (magic bytes/dimensions). Treats presentation/cosmetic differences as
#' non-material, and honestly records unavailable extractors.
#'
#' @param contract Output contract specification list or 1-row data frame.
#' @param attempt Migration attempt record, attempt directory, or list.
#' @param comparison_rules Optional list of comparison rules and overrides.
#' @return Named list containing explicit checks, differences, passed flag, and target status.
#' @noRd
assess_tlf_target <- function(contract, attempt, comparison_rules = list()) {
  t_id <- if (is.data.frame(contract)) contract$target_id[1L] else contract$target_id %||% ""
  t_key <- if (is.data.frame(contract)) contract$target_key[1L] else contract$target_key %||% ""
  l_name <- if (is.data.frame(contract)) contract$logical_name[1L] else contract$logical_name %||% ""
  required <- if (is.data.frame(contract)) isTRUE(contract$required[1L]) else isTRUE(contract$required)

  assertions <- if (is.data.frame(contract) && is.list(contract$assertions)) {
    if (length(contract$assertions) > 0L) contract$assertions[[1L]] else list()
  } else {
    contract$assertions %||% list()
  }

  checks <- list()
  diffs <- list(mismatches = list(), cosmetic = list())
  dimensions <- list(width = NA_integer_, height = NA_integer_)

  cand_path <- find_attempt_candidate_file(contract, attempt)
  if (is.na(cand_path) || !file.exists(cand_path)) {
    checks$candidate_exists <- list(
      name = "candidate_exists",
      passed = FALSE,
      details = "Candidate TLF file not found"
    )
    return(list(
      target_id = t_id,
      target_key = t_key,
      kind = "tlf",
      required = required,
      passed = FALSE,
      status = "missing_candidate",
      has_reference = FALSE,
      reference_passed = FALSE,
      has_assertions = length(assertions) > 0L,
      dimensions = dimensions,
      checks = checks,
      differences = diffs,
      candidate_path = NA_character_,
      reference_path = NA_character_
    ))
  }

  checks$candidate_exists <- list(
    name = "candidate_exists",
    passed = TRUE,
    details = "Candidate TLF file exists",
    path = cand_path
  )

  file_size <- file.info(cand_path)$size
  if (is.na(file_size) || file_size == 0L) {
    checks$file_nonempty <- list(
      name = "file_nonempty",
      passed = FALSE,
      details = "Candidate TLF file is empty (0 bytes)"
    )
    return(list(
      target_id = t_id,
      target_key = t_key,
      kind = "tlf",
      required = required,
      passed = FALSE,
      status = "unreadable",
      has_reference = FALSE,
      reference_passed = FALSE,
      has_assertions = length(assertions) > 0L,
      dimensions = dimensions,
      checks = checks,
      differences = diffs,
      candidate_path = cand_path,
      reference_path = NA_character_
    ))
  }

  checks$file_nonempty <- list(
    name = "file_nonempty",
    passed = TRUE,
    details = sprintf("File size: %d bytes", file_size)
  )

  ext <- tolower(tools::file_ext(cand_path))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(t_key))

  req_text <- assertions$required_text %||% comparison_rules$required_text %||% character()
  has_unavail <- FALSE

  # 1. PDF format checks
  if (ext == "pdf") {
    raw_head <- readBin(cand_path, what = "raw", n = min(file_size, 1024L))
    is_pdf_head <- length(raw_head) >= 5L && identical(raw_head[seq_len(5L)], charToRaw("%PDF-"))

    checks$header_valid <- list(
      name = "header_valid",
      passed = is_pdf_head,
      details = if (is_pdf_head) "Valid %PDF- header" else "Missing %PDF- header"
    )

    tail_n <- min(file_size, 2048L)
    con <- file(cand_path, "rb")
    seek(con, where = max(0L, file_size - tail_n), origin = "start")
    tail_bytes <- readBin(con, what = "raw", n = tail_n)
    close(con)
    tail_str <- tryCatch(rawToChar(tail_bytes), error = function(e) "")
    is_pdf_tail <- grepl("%%EOF", tail_str, useBytes = TRUE) || grepl("startxref", tail_str, useBytes = TRUE)

    checks$trailer_valid <- list(
      name = "trailer_valid",
      passed = is_pdf_tail,
      details = if (is_pdf_tail) "Valid PDF trailer found" else "Missing PDF trailer/EOF"
    )

    if (length(req_text) > 0L) {
      force_unavail <- isTRUE(comparison_rules$force_text_extractor_unavailable)
      extracted_text <- NULL
      if (!force_unavail) {
        if (requireNamespace("pdftools", quietly = TRUE)) {
          extracted_text <- tryCatch(pdftools::pdf_text(cand_path), error = function(e) NULL)
        }
        if (is.null(extracted_text)) {
          raw_full <- readBin(cand_path, what = "raw", n = min(file_size, 512L * 1024L))
          full_str <- tryCatch(rawToChar(raw_full), error = function(e) "")
          if (nzchar(full_str) && all(vapply(req_text, function(rt) grepl(tolower(rt), tolower(full_str), fixed = TRUE), logical(1)))) {
            extracted_text <- full_str
          }
        }
      }

      if (is.null(extracted_text)) {
        has_unavail <- TRUE
        checks$text_extraction <- list(
          name = "text_extraction",
          status = "unavailable",
          passed = NA,
          details = "PDF text extractor unavailable"
        )
      } else {
        combined_text <- paste(extracted_text, collapse = "\n")
        missing_text <- req_text[!vapply(req_text, function(rt) grepl(tolower(rt), tolower(combined_text), fixed = TRUE), logical(1))]
        if (length(missing_text) > 0L) {
          checks$required_text <- list(
            name = "required_text",
            passed = FALSE,
            missing_text = missing_text,
            details = paste0("Missing required text: ", paste(missing_text, collapse = ", "))
          )
        } else {
          checks$required_text <- list(
            name = "required_text",
            passed = TRUE,
            details = "All required text found in PDF"
          )
        }
      }
    }

  # 2. RTF format checks
  } else if (ext == "rtf") {
    rtf_lines <- readLines(cand_path, warn = FALSE)
    rtf_content <- paste(rtf_lines, collapse = "\n")
    starts_rtf <- grepl("^\\s*\\{\\\\rtf", rtf_content)
    chars <- strsplit(rtf_content, "")[[1L]]
    open_b <- sum(chars == "{")
    close_b <- sum(chars == "}")
    envelope_ok <- starts_rtf && (open_b > 0L) && (open_b == close_b)

    checks$envelope_valid <- list(
      name = "envelope_valid",
      passed = envelope_ok,
      details = if (envelope_ok) "Valid RTF envelope" else "Invalid RTF envelope or unbalanced braces"
    )

    if (length(req_text) > 0L) {
      plain_txt <- gsub("\\\\[a-zA-Z0-9]+ ?", " ", rtf_content)
      plain_txt <- gsub("[{}]", "", plain_txt)
      missing_text <- req_text[!vapply(req_text, function(rt) grepl(tolower(rt), tolower(plain_txt), fixed = TRUE), logical(1))]
      if (length(missing_text) > 0L) {
        checks$required_text <- list(
          name = "required_text",
          passed = FALSE,
          missing_text = missing_text,
          details = paste0("Missing required text: ", paste(missing_text, collapse = ", "))
        )
      } else {
        checks$required_text <- list(
          name = "required_text",
          passed = TRUE,
          details = "All required text found in RTF"
        )
      }
    }

  # 3. HTML format checks
  } else if (ext %in% c("html", "htm")) {
    html_lines <- readLines(cand_path, warn = FALSE)
    html_content <- paste(html_lines, collapse = "\n")
    has_html_tags <- grepl("<html|<!doctype html|<table|<body|<head", html_content, ignore.case = TRUE)

    checks$structure_valid <- list(
      name = "structure_valid",
      passed = has_html_tags,
      details = if (has_html_tags) "Valid HTML document structure" else "Invalid HTML document structure"
    )

    if (length(req_text) > 0L) {
      plain_txt <- gsub("<[^>]+>", " ", html_content)
      plain_txt <- gsub("&nbsp;", " ", plain_txt)
      missing_text <- req_text[!vapply(req_text, function(rt) grepl(tolower(rt), tolower(plain_txt), fixed = TRUE), logical(1))]
      if (length(missing_text) > 0L) {
        checks$required_text <- list(
          name = "required_text",
          passed = FALSE,
          missing_text = missing_text,
          details = paste0("Missing required text: ", paste(missing_text, collapse = ", "))
        )
      } else {
        checks$required_text <- list(
          name = "required_text",
          passed = TRUE,
          details = "All required text found in HTML"
        )
      }
    }

  # 4. PNG and JPEG image checks
  } else if (ext %in% c("png", "jpg", "jpeg")) {
    raw_img <- readBin(cand_path, what = "raw", n = min(file_size, 4096L))
    if (ext == "png") {
      png_sig <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
      is_sig_ok <- length(raw_img) >= 8L && identical(raw_img[1:8], png_sig)
      checks$signature_valid <- list(
        name = "signature_valid",
        passed = is_sig_ok,
        details = if (is_sig_ok) "Valid PNG signature" else "Invalid PNG signature"
      )

      if (is_sig_ok && length(raw_img) >= 24L) {
        w <- readBin(raw_img[17:20], what = "integer", size = 4, endian = "big")
        h <- readBin(raw_img[21:24], what = "integer", size = 4, endian = "big")
        dimensions$width <- w
        dimensions$height <- h
        dim_ok <- !is.na(w) && !is.na(h) && w > 0 && h > 0
        checks$dimensions_valid <- list(
          name = "dimensions_valid",
          passed = dim_ok,
          details = sprintf("Dimensions: %dx%d", w, h)
        )
      }
    } else {
      is_jpg_sig <- length(raw_img) >= 3L && identical(raw_img[1:3], as.raw(c(0xFF, 0xD8, 0xFF)))
      checks$signature_valid <- list(
        name = "signature_valid",
        passed = is_jpg_sig,
        details = if (is_jpg_sig) "Valid JPEG signature" else "Invalid JPEG signature"
      )
    }
  }

  # Reference comparison
  ref_path <- if (is.data.frame(contract)) contract$reference_path[1L] else contract$reference_path
  if (is.null(ref_path) || is.na(ref_path) || !nzchar(ref_path)) {
    ref_path <- comparison_rules$reference_path %||% comparison_rules$references[[t_key]] %||% NA_character_
  }

  has_ref <- !is.na(ref_path) && nzchar(ref_path) && file.exists(ref_path)
  ref_passed <- FALSE
  if (has_ref) {
    # No TLF content comparator exists yet, so a reference that merely exists
    # is recorded honestly as not compared; it must never count as validation
    # evidence. passed = NA keeps the target flowing without claiming a pass.
    checks$reference_comparison <- list(
      name = "reference_comparison",
      passed = NA,
      details = "Reference file present; TLF content comparison not implemented, not counted as validation evidence"
    )
    diffs$cosmetic <- list("cosmetic_styling_allowance")
  }

  all_checks_passed <- all(vapply(checks, function(chk) {
    if (is.null(chk$passed) || is.na(chk$passed)) TRUE else isTRUE(chk$passed)
  }, logical(1)))

  status <- if (all_checks_passed) {
    if (has_unavail) "needs_review" else "passed"
  } else if (isFALSE(checks$candidate_exists$passed)) {
    "missing_candidate"
  } else if (isFALSE(checks$file_nonempty$passed)) {
    "unreadable"
  } else {
    "failed"
  }

  list(
    target_id = t_id,
    target_key = t_key,
    kind = "tlf",
    required = required,
    passed = all_checks_passed && !has_unavail,
    status = status,
    has_reference = has_ref,
    reference_passed = ref_passed,
    has_assertions = length(assertions) > 0L,
    dimensions = dimensions,
    checks = checks,
    differences = diffs,
    candidate_path = cand_path,
    reference_path = if (has_ref) ref_path else NA_character_
  )
}

#' Assess all required migration targets and attach contributing lineage evidence
#'
#' Assesses datasets and TLF targets against inferred/declared contracts, evaluates
#' upstream lineage evidence, and promotes only actually covered components within
#' their unchanged bindings.
#'
#' @param contracts Inferred or configured output contracts.
#' @param attempt Migration attempt record.
#' @param graph Dependency graph.
#' @param evidence_histories Named list of component evidence histories.
#' @param comparison_rules Optional comparison rules and tolerance configuration.
#' @return Comprehensive assessment record.
#' @noRd
assess_final_outputs <- function(
  contracts,
  attempt,
  graph = NULL,
  evidence_histories = list(),
  comparison_rules = list()
) {
  # Normalize contracts to a data frame or empty contracts
  contract_df <- if (inherits(contracts, "sas2r_output_contracts") || is.data.frame(contracts)) {
    contracts
  } else if (is.list(contracts) && length(contracts) > 0L && is.list(contracts[[1L]])) {
    # list of contract rows
    as.data.frame(do.call(rbind, lapply(contracts, as.data.frame)))
  } else {
    empty_output_contracts()
  }

  # Check execution status from immutable attempt record
  exec_completed <- isTRUE(attempt$completed)
  exec_passed <- isTRUE(attempt$passed) || identical(attempt$exit_status, 0L)
  exec_deferred <- isTRUE(attempt$deferred) || isTRUE(attempt$runtime_deferred) ||
    identical(attempt$status, "deferred") || (!exec_completed && !exec_passed && isTRUE(attempt$reason == "execute_disabled"))

  exec_summary <- list(
    completed = exec_completed,
    passed = exec_passed,
    exit_status = attempt$exit_status %||% (if (exec_passed) 0L else 1L),
    deferred = exec_deferred,
    reason = attempt$reason %||% NA_character_
  )

  assessed_targets <- list()
  all_req_passed <- TRUE
  any_target_missing <- FALSE
  any_target_failed <- FALSE
  has_reference_evaluated <- FALSE
  has_assertions_evaluated <- FALSE

  if (nrow(contract_df) > 0L) {
    for (i in seq_len(nrow(contract_df))) {
      c_row <- contract_df[i, ]
      kind <- c_row$kind %||% "dataset"

      res <- if (identical(kind, "tlf")) {
        assess_tlf_target(c_row, attempt, comparison_rules = comparison_rules)
      } else {
        assess_dataset_target(c_row, attempt, comparison_rules = comparison_rules)
      }

      assessed_targets[[c_row$target_key]] <- res

      if (isTRUE(c_row$required)) {
        if (!isTRUE(res$passed)) {
          all_req_passed <- FALSE
          if (identical(res$status, "missing_candidate")) any_target_missing <- TRUE
          else any_target_failed <- TRUE
        }
        if (isTRUE(res$has_reference) && isTRUE(res$reference_passed)) {
          has_reference_evaluated <- TRUE
        }
        # Assertions count as evaluated only when they were judged against an
        # actually-run reference comparison; mere presence of keys/tolerance in
        # a config is comparison configuration, not equivalence evidence.
        if (isTRUE(res$has_assertions) && isTRUE(res$reference_passed)) {
          has_assertions_evaluated <- TRUE
        }
      }
    }
  }

  # Lineage evidence evaluation and component promotion
  updated_histories <- evidence_histories
  lineage_summaries <- list()
  has_lineage_review_unavail <- FALSE
  has_lineage_review_only <- FALSE
  all_lineage_cids <- character()

  for (t_key in names(assessed_targets)) {
    tgt_res <- assessed_targets[[t_key]]
    c_lineage <- if (!is.null(graph)) {
      lin <- evidence_for_output_lineage(graph, updated_histories, tgt_res$target_id)
      if (length(lin$upstream_components) == 0L && nzchar(tgt_res$target_key)) {
        lin_key <- evidence_for_output_lineage(graph, updated_histories, tgt_res$target_key)
        if (length(lin_key$upstream_components) > 0L) lin <- lin_key
      }
      lin
    } else {
      list(
        target_id = tgt_res$target_id,
        upstream_components = character(),
        min_level = "runtime_verified",
        review_unavailable = FALSE,
        is_blocked = FALSE,
        blockers = character()
      )
    }
    lineage_summaries[[t_key]] <- c_lineage

    if (isTRUE(tgt_res$required)) {
      all_lineage_cids <- unique(c(all_lineage_cids, c_lineage$upstream_components))
      if (isTRUE(c_lineage$review_unavailable)) {
        has_lineage_review_unavail <- TRUE
      }
      if (identical(c_lineage$min_level, "reviewed_only") || isTRUE(c_lineage$has_review_only)) {
        has_lineage_review_only <- TRUE
      }

      # Promote covered upstream components if target passed
      if (isTRUE(tgt_res$passed) && length(c_lineage$upstream_components) > 0L) {
        for (cid in c_lineage$upstream_components) {
          h <- updated_histories[[cid]]
          if (!is.null(h)) {
            ev <- current_component_evidence(h)
            curr_lvl <- ev$level

            if (isTRUE(tgt_res$has_reference) && isTRUE(tgt_res$reference_passed)) {
              # Promote to reference_validated
              if (identical(curr_lvl, "reviewed_only") && isTRUE(exec_passed)) {
                h <- promote_component_evidence(h, "runtime_verified", coverage = paste0("call:", cid))
                h <- promote_component_evidence(h, "output_verified", coverage = paste0("output:", t_key))
                h <- promote_component_evidence(h, "reference_validated", basis_id = paste0("reference:", t_key))
              } else if (identical(curr_lvl, "runtime_verified")) {
                h <- promote_component_evidence(h, "output_verified", coverage = paste0("output:", t_key))
                h <- promote_component_evidence(h, "reference_validated", basis_id = paste0("reference:", t_key))
              } else if (identical(curr_lvl, "output_verified")) {
                h <- promote_component_evidence(h, "reference_validated", basis_id = paste0("reference:", t_key))
              }
            } else {
              # Promote to output_verified
              if (identical(curr_lvl, "reviewed_only") && isTRUE(exec_passed)) {
                h <- promote_component_evidence(h, "runtime_verified", coverage = paste0("call:", cid))
                h <- promote_component_evidence(h, "output_verified", coverage = paste0("output:", t_key))
              } else if (identical(curr_lvl, "runtime_verified")) {
                h <- promote_component_evidence(h, "output_verified", coverage = paste0("output:", t_key))
              }
            }
            updated_histories[[cid]] <- h
          }
        }
      }
    }
  }

  # Re-evaluate overall lineage status based on updated histories
  has_lineage_review_unavail <- FALSE
  has_lineage_review_only <- FALSE
  for (t_key in names(assessed_targets)) {
    tgt_res <- assessed_targets[[t_key]]
    if (isTRUE(tgt_res$required)) {
      lin_cids <- lineage_summaries[[t_key]]$upstream_components %||% character()
      for (cid in lin_cids) {
        h <- updated_histories[[cid]]
        if (!is.null(h)) {
          ev <- current_component_evidence(h)
          if (isTRUE(ev$review_unavailable)) {
            has_lineage_review_unavail <- TRUE
          }
          if (identical(ev$level, "reviewed_only")) {
            has_lineage_review_only <- TRUE
          }
        }
      }
    }
  }

  overall_lineage <- list(
    upstream_components = all_lineage_cids,
    review_unavailable = has_lineage_review_unavail,
    has_review_only = has_lineage_review_only,
    is_blocked = has_lineage_review_unavail,
    has_reference_evaluated = has_reference_evaluated,
    has_assertions_evaluated = has_assertions_evaluated
  )

  assessment <- list(
    schema_version = MIGRATION_SCHEMA_VERSION,
    execution = exec_summary,
    targets = assessed_targets,
    contracts = contract_df,
    lineage_evidence = overall_lineage,
    lineage_by_target = lineage_summaries,
    evidence_histories = updated_histories,
    all_required_passed = all_req_passed,
    passing_targets = names(assessed_targets)[vapply(assessed_targets, function(t) isTRUE(t$passed), logical(1))],
    status = "blocked"
  )

  # Derive bundle status
  assessment$status <- derive_bundle_status(assessment)
  structure(assessment, class = "sas2r_bundle_assessment")
}

#' Derive deterministic four-state bundle status from gate assessment
#'
#' Evaluates execution records, target checks, and lineage evidence to derive
#' canonical status: `blocked`, `needs_review`, `migration_ready`, or `validated`.
#' Strictly ignores model-derived fields.
#'
#' @param assessment Gate assessment record or structured status list.
#' @return Canonical status string from `BUNDLE_STATUSES`.
#' @noRd
derive_bundle_status <- function(assessment) {
  if (is.null(assessment) || !is.list(assessment)) {
    return("blocked")
  }

  exec <- assessment$execution %||% list()
  targets <- assessment$targets %||% list()
  lineage <- assessment$lineage_evidence %||% list()

  exec_completed <- isTRUE(exec$completed)
  exec_passed <- isTRUE(exec$passed) || identical(exec$exit_status, 0L)
  exec_deferred <- isTRUE(exec$deferred) || isTRUE(exec$runtime_deferred) ||
    identical(exec$reason, "execute_disabled") || identical(exec$status, "deferred")

  # 1. Check for blocked conditions
  if (!exec_completed && !exec_deferred) {
    return("blocked")
  }
  if (exec_completed && !exec_passed) {
    return("blocked")
  }

  # Inspect target checks
  any_target_missing <- FALSE
  any_target_failed <- FALSE
  any_target_needs_review <- FALSE
  all_required_passed <- TRUE
  has_ref_pass <- FALSE
  has_ast_pass <- FALSE

  if (length(targets) > 0L) {
    for (t in targets) {
      req <- if (is.null(t$required)) TRUE else isTRUE(t$required)
      t_passed <- isTRUE(t$passed)
      t_status <- t$status %||% (if (t_passed) "passed" else "failed")

      if (req) {
        if (t_status %in% c("missing_candidate", "unreadable")) {
          any_target_missing = TRUE
          all_required_passed = FALSE
        } else if (t_status == "failed" || !t_passed) {
          any_target_failed = TRUE
          all_required_passed = FALSE
        } else if (t_status == "needs_review") {
          any_target_needs_review = TRUE
          all_required_passed = FALSE
        }

        # Only an actually-run, passing reference comparison is validation
        # evidence; a passing existence check next to an uncompared reference
        # is not.
        if (isTRUE(t$has_reference) && isTRUE(t$reference_passed)) {
          has_ref_pass = TRUE
        }
        if (isTRUE(t$has_assertions) && t_passed && isTRUE(t$reference_passed)) {
          has_ast_pass = TRUE
        }
      }
    }
  }

  if (any_target_missing || any_target_failed) {
    return("blocked")
  }

  # 2. Check for needs_review conditions
  if (exec_deferred) {
    return("needs_review")
  }
  if (isTRUE(lineage$review_unavailable)) {
    return("needs_review")
  }
  if (isTRUE(lineage$has_review_only) || identical(lineage$min_level, "reviewed_only")) {
    return("needs_review")
  }
  if (any_target_needs_review) {
    return("needs_review")
  }

  # 3. Check for validated vs migration_ready
  if (!all_required_passed) {
    return("needs_review")
  }

  # validated requires demonstrated equivalence: a reference comparison that
  # ran and passed (directly, via lineage, or via reference-backed assertions).
  has_validation_evidence <- isTRUE(lineage$has_reference_evaluated) ||
    has_ref_pass ||
    has_ast_pass ||
    identical(lineage$min_level, "reference_validated")

  if (has_validation_evidence) {
    return("validated")
  }

  "migration_ready"
}
