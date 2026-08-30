#' Context constructor for row alignment
#'
#' @param by Character vector of BY variables.
#' @param sort List of sort variables/directions or character vector.
#' @param merge List of merge keys or character vector.
#' @param lineage Upstream lineage metadata or candidate keys.
#' @param known_identifiers Character vector of known identifier column names.
#' @param order_contract Explicit ordering contract if established by source.
#' @param collation Character collation sequence (e.g. "standard", "unknown").
#' @return A list representing alignment context.
#' @noRd
alignment_context <- function(by = character(),
                              sort = list(vars = by, descending = rep(FALSE, length(by))),
                              merge = list(),
                              lineage = list(),
                              known_identifiers = by,
                              order_contract = NULL,
                              collation = NULL) {
  list(
    by = as.character(by),
    sort = sort,
    merge = merge,
    lineage = lineage,
    known_identifiers = as.character(known_identifiers),
    order_contract = order_contract,
    collation = collation
  )
}

#' Empty alignment context
#'
#' @return An empty alignment context list.
#' @noRd
empty_alignment_context <- function() {
  list(
    by = character(),
    sort = list(vars = character(), descending = logical()),
    merge = list(),
    lineage = list(),
    known_identifiers = character(),
    order_contract = NULL,
    collation = NULL
  )
}

#' Maximum residual group size limit for LSAP solver
#' @noRd
RESIDUAL_GROUP_LIMIT <- 100L

#' Generate row signatures for deterministic multiset grouping
#'
#' @param df A data.frame or tibble.
#' @return A character vector of row signatures.
#' @noRd
row_signatures <- function(df) {
  if (nrow(df) == 0L) return(character(0))
  if (ncol(df) == 0L) return(rep("", nrow(df)))

  col_strs <- lapply(df, function(col) {
    if (is.double(col)) {
      tagged <- haven::is_tagged_na(col)
      # Element-wise: format() chooses one representation for the whole
      # vector, so the same value would hash differently in two frames whose
      # other values differ in magnitude.
      out <- sprintf("%.17g", col)
      if (any(tagged)) {
        out[tagged] <- paste0(".tag_", haven::na_tag(col[tagged]))
      }
      out[is.na(col) & !tagged] <- ".NA"
      out
    } else if (is.character(col)) {
      out <- col
      out[is.na(col)] <- "\x01_NA_\x01"
      out
    } else {
      out <- as.character(col)
      out[is.na(col)] <- "\x01_NA_\x01"
      out
    }
  })

  do.call(paste, c(col_strs, sep = "\x1f"))
}

#' Check if two data frames have identical rows
#'
#' @param df1 First data frame.
#' @param df2 Second data frame.
#' @return Logical scalar.
#' @noRd
is_same_rows <- function(df1, df2) {
  if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2)) return(FALSE)
  if (nrow(df1) == 0L) return(TRUE)
  for (i in seq_along(df1)) {
    c1 <- df1[[i]]
    c2 <- df2[[i]]
    if (is.double(c1) && is.double(c2)) {
      t1 <- haven::na_tag(c1)
      t2 <- haven::na_tag(c2)
      both_na <- is.na(c1) & is.na(c2)
      same_tag <- (is.na(t1) & is.na(t2)) | (!is.na(t1) & !is.na(t2) & t1 == t2)
      val_eq <- !is.na(c1) & !is.na(c2) & (c1 == c2)
      if (!all(val_eq | (both_na & same_tag))) return(FALSE)
    } else {
      eq <- (is.na(c1) & is.na(c2)) | (!is.na(c1) & !is.na(c2) & c1 == c2)
      if (!all(eq)) return(FALSE)
    }
  }
  TRUE
}

#' Normalize comparison frame column names and values
#'
#' Converts column names to lowercase, checks for duplicates, normalizes
#' character strings and factors, and preserves haven tagged NAs.
#'
#' @param df A data.frame or tibble.
#' @param profile A comparison profile (default `compare_profile()`).
#' @return A normalized tibble.
#' @noRd
normalize_output_frame <- function(df, profile = compare_profile()) {
  if (!is.data.frame(df)) {
    cli::cli_abort("Expected a data.frame or tibble, got {.cls {class(df)}}.",
                   class = "sas2r_invalid_frame_error")
  }

  col_names <- tolower(names(df))
  if (anyDuplicated(col_names)) {
    dup_cols <- unique(col_names[duplicated(col_names)])
    cli::cli_abort("Dataset contains duplicated column names after case-folding: {.val {dup_cols}}.",
                   class = "sas2r_duplicate_column_error")
  }

  pad_is_cosmetic <- identical(profile$padding, "cosmetic")
  sas_null_na <- isTRUE(profile$sas_null_equals_na)

  supported_kinds <- c("numeric", "date", "datetime", "time", "character", "logical")
  kinds <- character(ncol(df))
  names(kinds) <- col_names
  notes <- list()
  norm_cols <- list()

  for (i in seq_along(df)) {
    nm <- col_names[i]
    col <- df[[i]]
    col_notes <- character()

    if (inherits(col, "haven_labelled")) {
      col <- haven::zap_labels(col)
      col_notes <- c(col_notes, "zap_labels")
    }

    if (is.factor(col)) {
      col <- as.character(col)
      col_notes <- c(col_notes, "factor_to_character")
    }

    k <- col_kind(col)
    kinds[nm] <- k

    if (is.character(col)) {
      enc <- Encoding(col)
      conv <- enc != "bytes"
      if (any(conv)) {
        col[conv] <- enc2utf8(col[conv])
      }
      if (any(enc != "UTF-8" & enc != "unknown" & enc != "bytes")) {
        col_notes <- c(col_notes, "enc2utf8")
      }
      if (pad_is_cosmetic) {
        col <- sub(" +$", "", col)
      }
      if (sas_null_na) {
        blank <- is.na(col) | !nzchar(col)
        sp <- which(startsWith(col, " "))
        if (length(sp) > 0L) blank[sp] <- blank[sp] | grepl("^ *$", col[sp])
        col[blank] <- NA_character_
      }
    }

    norm_cols[[nm]] <- col
    notes[[nm]] <- col_notes
  }

  unsupported <- kinds[!kinds %in% supported_kinds]
  out <- if (length(norm_cols) > 0L) {
    tibble::as_tibble(norm_cols, .name_repair = "minimal")
  } else {
    tibble::tibble()
  }

  attr(out, "kinds") <- kinds
  attr(out, "unsupported_kinds") <- unsupported
  attr(out, "notes") <- notes
  out
}

#' Infer candidate alignment keys from source contracts and heuristics
#'
#' Forms ordered, deduplicated candidate sets from BY clauses, latest PROC SORT,
#' merge keys, creator lineage, and known identifier columns visible in the schema.
#'
#' @param reference Reference data frame or tibble.
#' @param candidate Candidate data frame or tibble.
#' @param context Optional alignment context list.
#' @return A list of candidate key descriptors with `keys` and `provenance`.
#' @noRd
infer_alignment_keys <- function(reference, candidate, context = NULL) {
  ref_cols <- tolower(names(reference))
  cand_cols <- tolower(names(candidate))
  common_cols <- intersect(ref_cols, cand_cols)

  candidates <- list()
  seen_signatures <- character()

  add_candidate <- function(keys, provenance) {
    keys <- tolower(as.character(keys))
    keys <- keys[nzchar(keys)]
    if (length(keys) == 0L) return()
    if (!all(keys %in% common_cols)) return()

    sig <- paste(keys, collapse = "\t")
    if (sig %in% seen_signatures) return()

    seen_signatures <<- c(seen_signatures, sig)
    candidates[[length(candidates) + 1L]] <<- list(
      keys = keys,
      provenance = provenance
    )
  }

  # 1. Output BY clause (highest precedence source contract)
  if (!is.null(context$by)) {
    by_vars <- if (is.list(context$by)) context$by$vars else context$by
    by_vars <- as.character(by_vars)
    if (length(by_vars) > 0L) {
      add_candidate(by_vars, "by")
    }
  }

  # 2. Latest PROC SORT BY contract
  if (!is.null(context$sort)) {
    sort_vars <- if (is.list(context$sort)) context$sort$vars else context$sort
    sort_vars <- as.character(sort_vars)
    if (length(sort_vars) > 0L) {
      add_candidate(sort_vars, "sort")
    }
  }

  # 3. Merge keys
  if (!is.null(context$merge)) {
    merge_vars <- if (is.list(context$merge)) {
      if (!is.null(context$merge$by)) context$merge$by
      else if (!is.null(context$merge$keys)) context$merge$keys
      else character()
    } else {
      context$merge
    }
    merge_vars <- as.character(merge_vars)
    if (length(merge_vars) > 0L) {
      add_candidate(merge_vars, "merge")
    }
  }

  # 4. Creator lineage keys
  if (!is.null(context$lineage)) {
    lineage_vars <- if (is.list(context$lineage)) {
      if (!is.null(context$lineage$keys)) context$lineage$keys
      else character()
    } else {
      context$lineage
    }
    lineage_vars <- as.character(lineage_vars)
    if (length(lineage_vars) > 0L) {
      add_candidate(lineage_vars, "lineage")
    }
  }

  # Add individual subsets from multi-key source contracts
  if (!is.null(context$by)) {
    by_vars <- if (is.list(context$by)) context$by$vars else context$by
    by_vars <- as.character(by_vars)
    if (length(by_vars) > 1L) {
      for (v in by_vars) add_candidate(v, "by_subset")
    }
  }
  if (!is.null(context$sort)) {
    sort_vars <- if (is.list(context$sort)) context$sort$vars else context$sort
    sort_vars <- as.character(sort_vars)
    if (length(sort_vars) > 1L) {
      for (v in sort_vars) add_candidate(v, "sort_subset")
    }
  }

  # 5. Known identifier columns passed in context
  if (!is.null(context$known_identifiers)) {
    known_vars <- tolower(as.character(context$known_identifiers))
    known_vars <- known_vars[nzchar(known_vars) & (known_vars %in% common_cols)]
    if (length(known_vars) > 0L) {
      for (kv in known_vars) {
        add_candidate(kv, "known_identifiers")
      }
      if (length(known_vars) > 1L) {
        add_candidate(known_vars, "known_identifiers")
      }
    }
  }

  # 6. Schema heuristics: standard clinical / domain variables present in common columns
  subject_vars <- c("usubjid", "subjid", "id", "subject", "row_id", "record_id")
  subject_vars_present <- intersect(subject_vars, common_cols)

  study_vars <- c("studyid")
  study_vars_present <- intersect(study_vars, common_cols)

  seq_vars_present <- common_cols[grepl("seq$", common_cols)]

  visit_vars <- c("avisitn", "visitnum", "avisit", "visit")
  visit_vars_present <- intersect(visit_vars, common_cols)

  param_vars <- c("paramcd", "paramn", "param", "parm")
  param_vars_present <- intersect(param_vars, common_cols)

  date_vars <- c("adt", "adtm", "astdt", "aendt", "date", "dt")
  date_vars_present <- intersect(date_vars, common_cols)

  # Single subject identifiers
  for (sv in subject_vars_present) {
    add_candidate(sv, "heuristic")
  }

  # Subject + Sequence combinations
  for (sv in subject_vars_present) {
    for (seq_v in seq_vars_present) {
      if (seq_v != sv) {
        add_candidate(c(sv, seq_v), "heuristic")
      }
    }
  }

  # Subject + Param + Visit combinations
  for (sv in subject_vars_present) {
    for (pv in param_vars_present) {
      for (vv in visit_vars_present) {
        add_candidate(c(sv, pv, vv), "heuristic")
      }
    }
  }

  # Subject + Param + Date combinations
  for (sv in subject_vars_present) {
    for (pv in param_vars_present) {
      for (dv in date_vars_present) {
        add_candidate(c(sv, pv, dv), "heuristic")
      }
    }
  }

  # Subject + Visit combinations
  for (sv in subject_vars_present) {
    for (vv in visit_vars_present) {
      add_candidate(c(sv, vv), "heuristic")
    }
  }

  # Subject + Param combinations
  for (sv in subject_vars_present) {
    for (pv in param_vars_present) {
      add_candidate(c(sv, pv), "heuristic")
    }
  }

  # Study + Subject combinations
  for (stv in study_vars_present) {
    for (sv in subject_vars_present) {
      add_candidate(c(stv, sv), "heuristic")
    }
  }

  candidates
}

#' Validate an alignment key candidate across reference and candidate frames
#'
#' Requires all key columns to exist on both sides with matching column kinds,
#' and checks uniqueness of key tuples independently on reference and candidate.
#'
#' @param reference Reference data frame or tibble.
#' @param candidate Candidate data frame or tibble.
#' @param keys Character vector of key column names.
#' @param profile Comparison profile; key validity must be judged under the
#'   same normalization rules as the comparison the keys gate.
#' @param normalized `TRUE` when `reference` and `candidate` are already the
#'   output of [normalize_output_frame()] under `profile`. Normalization is
#'   idempotent, so re-running it would only cost a full pass over both frames
#'   -- once per candidate key, on top of the caller's own pass.
#' @return A list with validation results and matched row pairs if valid.
#' @noRd
validate_alignment_key <- function(reference, candidate, keys,
                                   profile = compare_profile(),
                                   normalized = FALSE) {
  keys <- tolower(as.character(keys))
  keys <- keys[nzchar(keys)]

  empty_pairs <- tibble::tibble(reference_row = integer(), candidate_row = integer())

  if (length(keys) == 0L) {
    return(list(
      keys = keys,
      valid = FALSE,
      unique_reference = FALSE,
      unique_candidate = FALSE,
      reason = "empty_keys",
      pairs = empty_pairs,
      only_reference = if (nrow(reference) > 0L) seq_len(nrow(reference)) else integer(),
      only_candidate = if (nrow(candidate) > 0L) seq_len(nrow(candidate)) else integer()
    ))
  }

  ref_cols <- tolower(names(reference))
  cand_cols <- tolower(names(candidate))

  ref_has_all <- all(keys %in% ref_cols)
  cand_has_all <- all(keys %in% cand_cols)

  if (!ref_has_all || !cand_has_all) {
    return(list(
      keys = keys,
      valid = FALSE,
      unique_reference = FALSE,
      unique_candidate = FALSE,
      reason = "missing_columns",
      pairs = empty_pairs,
      only_reference = if (nrow(reference) > 0L) seq_len(nrow(reference)) else integer(),
      only_candidate = if (nrow(candidate) > 0L) seq_len(nrow(candidate)) else integer()
    ))
  }

  ref_norm <- if (normalized) reference else normalize_output_frame(reference, profile = profile)
  cand_norm <- if (normalized) candidate else normalize_output_frame(candidate, profile = profile)

  # Check kind compatibility for all key columns
  for (k in keys) {
    kb <- col_kind(ref_norm[[k]])
    kc <- col_kind(cand_norm[[k]])
    if (kb != kc) {
      return(list(
        keys = keys,
        valid = FALSE,
        unique_reference = FALSE,
        unique_candidate = FALSE,
        reason = "type_mismatch",
        pairs = empty_pairs,
        only_reference = if (nrow(reference) > 0L) seq_len(nrow(reference)) else integer(),
        only_candidate = if (nrow(candidate) > 0L) seq_len(nrow(candidate)) else integer()
      ))
    }
  }

  ref_k <- ref_norm[keys]
  cand_k <- cand_norm[keys]

  unique_ref <- (nrow(ref_norm) == 0L) || (!vctrs::vec_duplicate_any(ref_k))
  unique_cand <- (nrow(cand_norm) == 0L) || (!vctrs::vec_duplicate_any(cand_k))
  valid <- unique_ref && unique_cand

  if (!valid) {
    reason <- if (!unique_ref && !unique_cand) "duplicate_both"
    else if (!unique_ref) "duplicate_reference"
    else "duplicate_candidate"

    return(list(
      keys = keys,
      valid = FALSE,
      unique_reference = unique_ref,
      unique_candidate = unique_cand,
      reason = reason,
      pairs = empty_pairs,
      only_reference = if (nrow(reference) > 0L) seq_len(nrow(reference)) else integer(),
      only_candidate = if (nrow(candidate) > 0L) seq_len(nrow(candidate)) else integer()
    ))
  }

  if (nrow(ref_norm) == 0L && nrow(cand_norm) == 0L) {
    return(list(
      keys = keys,
      valid = TRUE,
      unique_reference = TRUE,
      unique_candidate = TRUE,
      reason = "unique_key",
      pairs = empty_pairs,
      only_reference = integer(),
      only_candidate = integer()
    ))
  }

  m <- vctrs::vec_match(ref_k, cand_k)
  matched_ref <- which(!is.na(m))
  matched_cand <- m[matched_ref]
  only_ref <- which(is.na(m))

  cand_matched_mask <- logical(nrow(cand_norm))
  cand_matched_mask[matched_cand] <- TRUE
  only_cand <- which(!cand_matched_mask)

  pairs <- tibble::tibble(
    reference_row = as.integer(matched_ref),
    candidate_row = as.integer(matched_cand)
  )

  list(
    keys = keys,
    valid = TRUE,
    unique_reference = TRUE,
    unique_candidate = TRUE,
    reason = "unique_key",
    pairs = pairs,
    only_reference = as.integer(only_ref),
    only_candidate = as.integer(only_cand)
  )
}

#' Match exact normalized row multisets within groups or whole datasets
#'
#' Within each duplicate key group—or the entire frame when keyless—hashes normalized full
#' rows, pairs identical hash/value groups deterministically by original row number, and extracts
#' remaining residuals.
#'
#' @param ref_norm Normalized reference data frame or tibble.
#' @param cand_norm Normalized candidate data frame or tibble.
#' @param ref_rows Integer vector of reference row indices to consider.
#' @param cand_rows Integer vector of candidate row indices to consider.
#' @param cols Character vector of columns to compare (default NULL uses common columns).
#' @return A list containing `pairs` (tibble of reference_row, candidate_row),
#'   `ref_residuals` (integer vector), and `cand_residuals` (integer vector).
#' @noRd
match_row_multisets <- function(ref_norm, cand_norm,
                                ref_rows = seq_len(nrow(ref_norm)),
                                cand_rows = seq_len(nrow(cand_norm)),
                                cols = NULL) {
  ref_rows <- as.integer(ref_rows)
  cand_rows <- as.integer(cand_rows)

  if (is.null(cols)) {
    cols <- intersect(names(ref_norm), names(cand_norm))
  } else {
    cols <- intersect(cols, intersect(names(ref_norm), names(cand_norm)))
  }

  if (length(ref_rows) == 0L || length(cand_rows) == 0L || length(cols) == 0L) {
    return(list(
      pairs = tibble::tibble(reference_row = integer(), candidate_row = integer()),
      ref_residuals = ref_rows,
      cand_residuals = cand_rows
    ))
  }

  sub_ref <- ref_norm[ref_rows, cols, drop = FALSE]
  sub_cand <- cand_norm[cand_rows, cols, drop = FALSE]

  sig_ref <- row_signatures(sub_ref)
  sig_cand <- row_signatures(sub_cand)

  ref_by_sig <- split(ref_rows, sig_ref)
  cand_by_sig <- split(cand_rows, sig_cand)

  all_sigs <- union(names(ref_by_sig), names(cand_by_sig))

  matched_ref <- integer()
  matched_cand <- integer()
  resid_ref <- integer()
  resid_cand <- integer()

  for (s in all_sigs) {
    r_idx <- ref_by_sig[[s]]
    c_idx <- cand_by_sig[[s]]

    n_r <- length(r_idx)
    n_c <- length(c_idx)

    if (n_r > 0L && n_c > 0L) {
      k <- min(n_r, n_c)
      matched_ref <- c(matched_ref, r_idx[seq_len(k)])
      matched_cand <- c(matched_cand, c_idx[seq_len(k)])
      if (n_r > k) {
        resid_ref <- c(resid_ref, r_idx[(k + 1L):n_r])
      }
      if (n_c > k) {
        resid_cand <- c(resid_cand, c_idx[(k + 1L):n_c])
      }
    } else if (n_r > 0L) {
      resid_ref <- c(resid_ref, r_idx)
    } else if (n_c > 0L) {
      resid_cand <- c(resid_cand, c_idx)
    }
  }

  if (length(matched_ref) > 0L) {
    o <- order(matched_ref, matched_cand)
    pairs <- tibble::tibble(
      reference_row = as.integer(matched_ref[o]),
      candidate_row = as.integer(matched_cand[o])
    )
  } else {
    pairs <- tibble::tibble(reference_row = integer(), candidate_row = integer())
  }

  list(
    pairs = pairs,
    ref_residuals = as.integer(sort(resid_ref)),
    cand_residuals = as.integer(sort(resid_cand))
  )
}

#' Bounded minimum-cost residual assignment via LSAP
#'
#' Computes optimal assignment between residual reference and candidate rows using
#' `clue::solve_LSAP`. Checks for ambiguity and enforces group size caps.
#'
#' @param ref_norm Normalized reference data frame or tibble.
#' @param cand_norm Normalized candidate data frame or tibble.
#' @param ref_residuals Integer vector of reference row indices.
#' @param cand_residuals Integer vector of candidate row indices.
#' @param cols Character vector of column names to compare.
#' @param profile Comparison profile.
#' @param cap Maximum residual group size allowed for LSAP assignment.
#' @return A list containing `pairs`, `only_reference`, `only_candidate`,
#'   `ambiguous`, `ambiguous_groups`, and `resource_state`.
#' @importFrom clue solve_LSAP
#' @noRd
minimum_cost_residual_pairs <- function(ref_norm, cand_norm,
                                        ref_residuals, cand_residuals,
                                        cols = NULL,
                                        profile = compare_profile(),
                                        cap = RESIDUAL_GROUP_LIMIT) {
  ref_residuals <- as.integer(ref_residuals)
  cand_residuals <- as.integer(cand_residuals)
  n_r <- length(ref_residuals)
  n_c <- length(cand_residuals)

  empty_pairs <- tibble::tibble(reference_row = integer(), candidate_row = integer())
  empty_ambig <- tibble::tibble()

  if (n_r == 0L || n_c == 0L) {
    return(list(
      pairs = empty_pairs,
      only_reference = ref_residuals,
      only_candidate = cand_residuals,
      ambiguous = FALSE,
      ambiguous_groups = empty_ambig,
      resource_state = "complete"
    ))
  }

  if (max(n_r, n_c) > cap) {
    return(list(
      pairs = empty_pairs,
      only_reference = ref_residuals,
      only_candidate = cand_residuals,
      ambiguous = FALSE,
      ambiguous_groups = empty_ambig,
      resource_state = "residual_group_limit"
    ))
  }

  if (is.null(cols)) {
    cols <- intersect(names(ref_norm), names(cand_norm))
  } else {
    cols <- intersect(cols, intersect(names(ref_norm), names(cand_norm)))
  }

  C <- matrix(0.0, nrow = n_r, ncol = n_c)

  for (v in cols) {
    col_ref <- ref_norm[[v]][ref_residuals]
    col_cand <- cand_norm[[v]][cand_residuals]
    k <- col_kind(col_ref)
    tol <- effective_tol(profile, v)

    if (k %in% c("numeric", "date", "datetime", "time")) {
      for (i in seq_len(n_r)) {
        vr <- col_ref[i]
        for (j in seq_len(n_c)) {
          vc <- col_cand[j]
          tag_r <- if (is.double(vr)) haven::na_tag(vr) else NA_character_
          tag_c <- if (is.double(vc)) haven::na_tag(vc) else NA_character_
          both_na <- is.na(vr) & is.na(vc)
          if (both_na) {
            same_tag <- (is.na(tag_r) & is.na(tag_c)) | (!is.na(tag_r) & !is.na(tag_c) & tag_r == tag_c)
            if (!same_tag) C[i, j] <- C[i, j] + 0.5
          } else if (xor(is.na(vr), is.na(vc))) {
            C[i, j] <- C[i, j] + 1.0
          } else {
            v_rn <- if (k == "time") as.numeric(vr, units = "secs") else as.numeric(unclass(vr))
            v_cn <- if (k == "time") as.numeric(vc, units = "secs") else as.numeric(unclass(vc))
            has_explicit_rel <- !is.null(profile$overrides[[tolower(v)]]$rel)
            rel_tol <- if (has_explicit_rel) tol$rel else if (k == "numeric") tol$rel else 0.0
            eq <- num_equal(v_rn, v_cn, tol$abs, rel_tol)
            if (!eq) {
              C[i, j] <- C[i, j] + 1.0
            }
          }
        }
      }
    } else if (k == "character") {
      for (i in seq_len(n_r)) {
        vr <- col_ref[i]
        for (j in seq_len(n_c)) {
          vc <- col_cand[j]
          diff_type <- chr_classify(vr, vc, sas_null_equals_na = isTRUE(profile$sas_null_equals_na))
          if (diff_type == "padding") {
            C[i, j] <- C[i, j] + 0.1
          } else if (diff_type == "case") {
            C[i, j] <- C[i, j] + 0.2
          } else if (diff_type != "equal") {
            C[i, j] <- C[i, j] + 1.0
          }
        }
      }
    } else {
      for (i in seq_len(n_r)) {
        vr <- col_ref[i]
        for (j in seq_len(n_c)) {
          vc <- col_cand[j]
          if (!na_equal(vr, vc)) {
            C[i, j] <- C[i, j] + 1.0
          }
        }
      }
    }
  }

  ambiguous <- FALSE
  if (n_r <= n_c) {
    sol <- as.vector(clue::solve_LSAP(C))
    cost <- sum(C[cbind(seq_len(n_r), sol)])

    penalty <- sum(abs(C)) + 1e5
    for (i in seq_len(n_r)) {
      C_mod <- C
      C_mod[i, sol[i]] <- penalty
      sol_alt <- as.vector(clue::solve_LSAP(C_mod))
      cost_alt <- sum(C[cbind(seq_len(n_r), sol_alt)])
      if (isTRUE(all.equal(cost_alt, cost)) && !identical(sol_alt, sol)) {
        ambiguous <- TRUE
        break
      }
    }

    matched_ref <- ref_residuals
    matched_cand <- cand_residuals[sol]
    only_ref <- integer()
    only_cand <- if (n_c > n_r) cand_residuals[-sol] else integer()
  } else {
    Ct <- t(C)
    sol <- as.vector(clue::solve_LSAP(Ct))
    cost <- sum(Ct[cbind(seq_len(n_c), sol)])

    penalty <- sum(abs(Ct)) + 1e5
    for (j in seq_len(n_c)) {
      Ct_mod <- Ct
      Ct_mod[j, sol[j]] <- penalty
      sol_alt <- as.vector(clue::solve_LSAP(Ct_mod))
      cost_alt <- sum(Ct[cbind(seq_len(n_c), sol_alt)])
      if (isTRUE(all.equal(cost_alt, cost)) && !identical(sol_alt, sol)) {
        ambiguous <- TRUE
        break
      }
    }

    matched_ref <- ref_residuals[sol]
    matched_cand <- cand_residuals
    only_ref <- if (n_r > n_c) ref_residuals[-sol] else integer()
    only_cand <- integer()
  }

  o <- order(matched_ref, matched_cand)
  pairs <- tibble::tibble(
    reference_row = as.integer(matched_ref[o]),
    candidate_row = as.integer(matched_cand[o])
  )

  ambig_groups <- if (ambiguous) {
    tibble::tibble(
      group_id = 1L,
      n_reference = n_r,
      n_candidate = n_c,
      cost = cost,
      reason = "ambiguous_alignment"
    )
  } else {
    tibble::tibble()
  }

  list(
    pairs = pairs,
    only_reference = as.integer(sort(only_ref)),
    only_candidate = as.integer(sort(only_cand)),
    ambiguous = ambiguous,
    ambiguous_groups = ambig_groups,
    resource_state = "complete"
  )
}

#' Produce integer row order permutation following SAS sort and missing semantics
#'
#' @param x A vector, list, data frame, or tibble to sort.
#' @param descending Logical vector indicating sort direction per column.
#' @param collation Character collation sequence (default NULL or "standard"; if "unknown", returns "unknown_collation").
#' @return An integer vector representing row ordering permutation, or "unknown_collation".
#' @noRd
sas_order_key <- function(x, descending = FALSE, collation = NULL) {
  if (identical(collation, "unknown")) {
    return("unknown_collation")
  }

  if (is.data.frame(x) || (is.list(x) && !inherits(x, "POSIXlt") && !inherits(x, "Date"))) {
    df <- as.data.frame(x)
    n_cols <- ncol(df)
    if (n_cols == 0L) return(integer(0))
    if (length(descending) < n_cols) {
      descending <- rep_len(descending, n_cols)
    }

    order_args <- list()
    for (i in seq_len(n_cols)) {
      col <- df[[i]]
      desc <- isTRUE(descending[i])
      col_keys <- make_sas_order_keys(col, desc)
      order_args <- c(order_args, col_keys)
    }
    return(do.call(order, order_args))
  } else {
    desc <- isTRUE(descending[1])
    col_keys <- make_sas_order_keys(x, desc)
    return(do.call(order, col_keys))
  }
}

#' Helper to generate multi-component sort keys for SAS ordering
#' @noRd
make_sas_order_keys <- function(col, descending = FALSE) {
  if (is.factor(col)) col <- as.character(col)

  if (is.numeric(col)) {
    if (is.double(col)) {
      is_tag <- haven::is_tagged_na(col)
      is_ord_na <- is.na(col) & !is_tag
      tag_val <- haven::na_tag(col)
    } else {
      is_tag <- rep(FALSE, length(col))
      is_ord_na <- is.na(col)
      tag_val <- rep(NA_character_, length(col))
    }

    is_under <- is_tag & (tolower(tag_val) == "_")
    is_letter <- is_tag & (tolower(tag_val) %in% letters)

    let_rank <- integer(length(col))
    if (any(is_letter)) {
      let_rank[is_letter] <- vapply(tolower(tag_val[is_letter]), function(t) utf8ToInt(t) - utf8ToInt("a") + 1L, integer(1))
    }

    is_nonmissing <- !is.na(col)

    if (!descending) {
      cat_group <- integer(length(col))
      cat_group[is_under] <- 1L
      cat_group[is_ord_na] <- 2L
      cat_group[is_letter] <- 3L
      cat_group[is_nonmissing] <- 4L

      sub_rank <- integer(length(col))
      sub_rank[is_letter] <- let_rank[is_letter]

      val <- col
      val[!is_nonmissing] <- 0

      list(cat_group, sub_rank, val)
    } else {
      cat_group <- integer(length(col))
      cat_group[is_nonmissing] <- 1L
      cat_group[is_letter] <- 2L
      cat_group[is_ord_na] <- 3L
      cat_group[is_under] <- 4L

      sub_rank <- integer(length(col))
      sub_rank[is_letter] <- 27L - let_rank[is_letter]

      val <- col
      val[!is_nonmissing] <- 0

      list(cat_group, sub_rank, -val)
    }
  } else if (is.character(col)) {
    blank <- is.na(col) | !nzchar(sub(" +$", "", col))
    cleaned <- sub(" +$", "", col)
    cleaned[blank] <- ""

    if (!descending) {
      cat_group <- ifelse(blank, 1L, 2L)
      list(cat_group, cleaned)
    } else {
      cat_group <- ifelse(blank, 2L, 1L)
      list(cat_group, -xtfrm(cleaned))
    }
  } else {
    na_mask <- is.na(col)
    val <- col
    if (!descending) {
      cat_group <- ifelse(na_mask, 1L, 2L)
      list(cat_group, xtfrm(val))
    } else {
      cat_group <- ifelse(na_mask, 2L, 1L)
      list(cat_group, -xtfrm(val))
    }
  }
}

#' Analyze dataset row order independently of content equivalence
#'
#' @param reference Reference data frame or tibble.
#' @param candidate Candidate data frame or tibble.
#' @param pairs Matched pairs tibble (reference_row, candidate_row).
#' @param context Optional alignment context list.
#' @return A list containing order analysis metadata.
#' @examples
#' ref <- data.frame(id = 1:3, v = c("a", "b", "c"))
#' cand <- data.frame(id = c(3, 1, 2), v = c("c", "a", "b"))
#' pairs <- tibble::tibble(reference_row = 1:3, candidate_row = c(2L, 3L, 1L))
#'
#' # Content is equivalent; only the row order differs.
#' res <- analyze_output_order(ref, cand, pairs,
#'   context = list(order_contract = list(vars = "id", descending = FALSE)))
#' res$order_equivalent
#' res$reason
#' @export
analyze_output_order <- function(reference, candidate, pairs, context = NULL) {
  meaningful <- FALSE
  vars <- character()
  descending <- logical()

  if (!is.null(context$order_contract)) {
    meaningful <- TRUE
    vars <- if (is.list(context$order_contract)) context$order_contract$vars else context$order_contract
    descending <- if (is.list(context$order_contract) && !is.null(context$order_contract$descending)) {
      context$order_contract$descending
    } else {
      rep(FALSE, length(vars))
    }
  } else if (!is.null(context$sort)) {
    sort_vars <- if (is.list(context$sort)) context$sort$vars else context$sort
    if (length(sort_vars) > 0L) {
      meaningful <- TRUE
      vars <- sort_vars
      descending <- if (is.list(context$sort) && !is.null(context$sort$descending)) {
        context$sort$descending
      } else {
        rep(FALSE, length(vars))
      }
    }
  } else if (!is.null(context$by)) {
    by_vars <- if (is.list(context$by)) context$by$vars else context$by
    if (length(by_vars) > 0L) {
      meaningful <- TRUE
      vars <- by_vars
      descending <- rep(FALSE, length(vars))
    }
  }

  vars <- tolower(as.character(vars))
  if (length(descending) < length(vars)) {
    descending <- rep_len(descending, length(vars))
  }

  n_ref <- nrow(reference)
  n_cand <- nrow(candidate)
  n_pairs <- if (is.data.frame(pairs)) nrow(pairs) else 0L

  content_equivalent <- (n_ref == n_cand) && (n_pairs == n_ref)
  order_equivalent <- content_equivalent && (n_ref > 0L) &&
    all(pairs$reference_row == pairs$candidate_row) &&
    all(pairs$reference_row == seq_len(n_ref))

  if (n_ref == 0L || n_cand == 0L) {
    return(list(
      meaningful = meaningful,
      content_equivalent = (n_ref == 0L && n_cand == 0L),
      order_equivalent = (n_ref == 0L && n_cand == 0L),
      variables = vars,
      descending = descending,
      missing_policy = "sas_missing_first",
      reason = "insufficient_evidence"
    ))
  }

  if (order_equivalent) {
    return(list(
      meaningful = meaningful,
      content_equivalent = TRUE,
      order_equivalent = TRUE,
      variables = vars,
      descending = descending,
      missing_policy = "sas_missing_first",
      reason = "same_order"
    ))
  }

  if (identical(context$collation, "unknown")) {
    return(list(
      meaningful = meaningful,
      content_equivalent = content_equivalent,
      order_equivalent = FALSE,
      variables = vars,
      descending = descending,
      missing_policy = "sas_missing_first",
      reason = "unknown_collation"
    ))
  }

  if (length(vars) == 0L || !all(vars %in% names(reference)) || !all(vars %in% names(candidate))) {
    reason <- if (meaningful) "insufficient_evidence" else "incidental_reorder"
    return(list(
      meaningful = meaningful,
      content_equivalent = content_equivalent,
      order_equivalent = FALSE,
      variables = vars,
      descending = descending,
      missing_policy = "sas_missing_first",
      reason = reason
    ))
  }

  ref_k <- reference[vars]
  cand_k <- candidate[vars]

  ref_sas_order <- sas_order_key(ref_k, descending = descending)
  ref_is_sorted <- is_same_rows(ref_k, ref_k[ref_sas_order, , drop = FALSE])

  cand_r_order_args <- list()
  for (i in seq_along(vars)) {
    v <- vars[i]
    d <- descending[i]
    col <- cand_k[[v]]
    if (is.numeric(col)) {
      cand_r_order_args[[length(cand_r_order_args) + 1L]] <- if (d) -col else col
    } else {
      cand_r_order_args[[length(cand_r_order_args) + 1L]] <- if (d) -xtfrm(col) else xtfrm(col)
    }
  }
  cand_r_order_args$na.last <- TRUE
  cand_r_order <- do.call(order, cand_r_order_args)
  cand_is_r_sorted <- is_same_rows(cand_k, cand_k[cand_r_order, , drop = FALSE])

  has_na_in_keys <- any(vapply(ref_k, anyNA, logical(1))) || any(vapply(cand_k, anyNA, logical(1)))

  if (has_na_in_keys && ref_is_sorted && cand_is_r_sorted) {
    cand_sas_order <- sas_order_key(cand_k, descending = descending)
    if (!is_same_rows(cand_k, cand_k[cand_sas_order, , drop = FALSE])) {
      return(list(
        meaningful = meaningful,
        content_equivalent = content_equivalent,
        order_equivalent = FALSE,
        variables = vars,
        descending = descending,
        missing_policy = "sas_missing_first",
        reason = "sas_missing_placement"
      ))
    }
  }

  if (is_same_rows(ref_k, cand_k)) {
    return(list(
      meaningful = meaningful,
      content_equivalent = content_equivalent,
      order_equivalent = FALSE,
      variables = vars,
      descending = descending,
      missing_policy = "sas_missing_first",
      reason = "unstable_ties"
    ))
  }

  reason <- if (meaningful) "genuine_order_difference" else "incidental_reorder"

  list(
    meaningful = meaningful,
    content_equivalent = content_equivalent,
    order_equivalent = FALSE,
    variables = vars,
    descending = descending,
    missing_policy = "sas_missing_first",
    reason = reason
  )
}

#' Construct a row alignment object
#'
#' @param method Character string describing alignment method.
#' @param candidate_keys List of candidate key descriptors.
#' @param selected_keys Character vector of chosen key variables.
#' @param pairs Data frame of matched pairs (reference_row, candidate_row).
#' @param only_reference Integer vector of unmatched reference rows.
#' @param only_candidate Integer vector of unmatched candidate rows.
#' @param duplicate_groups Summary of duplicate groups if any.
#' @param ambiguous_groups Summary of ambiguous groups if any.
#' @param order List of order metadata.
#' @param resource_state Resource status string.
#' @return An object of class `sas2r_row_alignment`.
#' @noRd
new_row_alignment <- function(method, candidate_keys, selected_keys, pairs,
                              only_reference, only_candidate,
                              duplicate_groups, ambiguous_groups, order,
                              resource_state = "complete") {
  structure(list(
    method = method,
    candidate_keys = candidate_keys,
    selected_keys = selected_keys,
    key_unique_reference = identical(method, "unique_key"),
    key_unique_candidate = identical(method, "unique_key"),
    pairs = pairs,
    only_reference = as.integer(only_reference),
    only_candidate = as.integer(only_candidate),
    duplicate_groups = duplicate_groups,
    ambiguous_groups = ambiguous_groups,
    order = order,
    resource_state = resource_state
  ), class = "sas2r_row_alignment")
}

#' @export
print.sas2r_row_alignment <- function(x, ...) {
  cli::cli_h1("sas2r row alignment")
  cli::cli_text("{.field method}: {x$method}")
  if (length(x$selected_keys) > 0L) {
    cli::cli_text("{.field selected_keys}: {paste(x$selected_keys, collapse = ', ')}")
  } else {
    cli::cli_text("{.field selected_keys}: <none>")
  }
  cli::cli_text("{.field matched_pairs}: {nrow(x$pairs)}")
  cli::cli_text("{.field only_reference}: {length(x$only_reference)}")
  cli::cli_text("{.field only_candidate}: {length(x$only_candidate)}")
  if (is.data.frame(x$duplicate_groups) && nrow(x$duplicate_groups) > 0L) {
    cli::cli_text("{.field duplicate_groups}: {nrow(x$duplicate_groups)}")
  }
  if (is.data.frame(x$ambiguous_groups) && nrow(x$ambiguous_groups) > 0L) {
    cli::cli_text("{.field ambiguous_groups}: {nrow(x$ambiguous_groups)}")
  }
  cli::cli_text("{.field resource_state}: {x$resource_state}")
  invisible(x)
}

#' Align reference and candidate output rows by source-derived keys
#'
#' Automatically infers credible candidate keys from source contracts (BY, SORT,
#' merge, lineage) and schema identifiers without requiring manual key configuration.
#' Handles unique keys, duplicate keys, and keyless datasets with multiset and residual
#' assignment.
#'
#' @param reference Reference data frame or tibble.
#' @param candidate Candidate data frame or tibble.
#' @param context Optional alignment context list.
#' @param profile Comparison profile (default `compare_profile()`).
#' @param normalized `TRUE` when `reference` and `candidate` are already the
#'   output of [normalize_output_frame()] under `profile`. The normalized pair
#'   is then threaded down into [validate_alignment_key()] instead of being
#'   re-derived once per candidate key.
#' @return An object of class `sas2r_row_alignment`.
#' @noRd
align_output_rows <- function(reference, candidate, context = NULL,
                              profile = compare_profile(),
                              normalized = FALSE) {
  ref_norm <- if (normalized) reference else normalize_output_frame(reference, profile = profile)
  cand_norm <- if (normalized) candidate else normalize_output_frame(candidate, profile = profile)

  candidate_descriptors <- infer_alignment_keys(ref_norm, cand_norm, context = context)

  evaluated_candidates <- list()
  selected_val <- NULL
  selected_key_names <- character()
  first_valid_key <- NULL
  first_valid_key_names <- character()

  for (cand in candidate_descriptors) {
    v <- validate_alignment_key(ref_norm, cand_norm, cand$keys,
                                profile = profile, normalized = TRUE)
    eval_entry <- list(
      keys = cand$keys,
      provenance = cand$provenance,
      unique_reference = v$unique_reference,
      unique_candidate = v$unique_candidate,
      valid = v$valid,
      reason = v$reason
    )
    evaluated_candidates[[length(evaluated_candidates) + 1L]] <- eval_entry

    if (is.null(selected_val) && isTRUE(v$valid)) {
      selected_val <- v
      selected_key_names <- cand$keys
    }

    if (is.null(first_valid_key) && !v$reason %in% c("empty_keys", "missing_columns", "type_mismatch")) {
      first_valid_key <- cand
      first_valid_key_names <- cand$keys
    }
  }

  # 1. Unique Key Case
  if (!is.null(selected_val)) {
    order_info <- analyze_output_order(ref_norm, cand_norm, selected_val$pairs, context = context)
    return(new_row_alignment(
      method = "unique_key",
      candidate_keys = evaluated_candidates,
      selected_keys = selected_key_names,
      pairs = selected_val$pairs,
      only_reference = selected_val$only_reference,
      only_candidate = selected_val$only_candidate,
      duplicate_groups = tibble::tibble(),
      ambiguous_groups = tibble::tibble(),
      order = order_info,
      resource_state = "complete"
    ))
  }

  # 2. Duplicate Key Case (candidate key exists but has duplicate keys on ref or cand)
  if (!is.null(first_valid_key)) {
    sel_keys <- first_valid_key_names
    non_key_cols <- setdiff(intersect(names(ref_norm), names(cand_norm)), sel_keys)

    ref_k <- ref_norm[sel_keys]
    cand_k <- cand_norm[sel_keys]

    ref_key_sig <- row_signatures(ref_k)
    cand_key_sig <- row_signatures(cand_k)

    ref_by_k <- split(seq_len(nrow(ref_norm)), ref_key_sig)
    cand_by_k <- split(seq_len(nrow(cand_norm)), cand_key_sig)

    all_k_sigs <- union(names(ref_by_k), names(cand_by_k))

    all_pairs_list <- list()
    all_only_ref <- integer()
    all_only_cand <- integer()
    all_ambig_list <- list()
    overall_resource_state <- "complete"
    dup_group_rows <- list()

    for (ks in all_k_sigs) {
      r_rows <- ref_by_k[[ks]]
      c_rows <- cand_by_k[[ks]]

      if (length(r_rows) > 0L && length(c_rows) > 0L) {
        # Stage 1: Exact multiset matching
        m_res <- match_row_multisets(ref_norm, cand_norm, r_rows, c_rows, cols = non_key_cols)
        if (nrow(m_res$pairs) > 0L) {
          all_pairs_list[[length(all_pairs_list) + 1L]] <- m_res$pairs
        }

        # Stage 2: Residual matching
        if (length(m_res$ref_residuals) > 0L || length(m_res$cand_residuals) > 0L) {
          lsap_res <- minimum_cost_residual_pairs(
            ref_norm, cand_norm,
            m_res$ref_residuals, m_res$cand_residuals,
            cols = non_key_cols,
            profile = profile
          )
          if (nrow(lsap_res$pairs) > 0L) {
            all_pairs_list[[length(all_pairs_list) + 1L]] <- lsap_res$pairs
          }
          if (length(lsap_res$only_reference) > 0L) {
            all_only_ref <- c(all_only_ref, lsap_res$only_reference)
          }
          if (length(lsap_res$only_candidate) > 0L) {
            all_only_cand <- c(all_only_cand, lsap_res$only_candidate)
          }
          if (nrow(lsap_res$ambiguous_groups) > 0L) {
            all_ambig_list[[length(all_ambig_list) + 1L]] <- lsap_res$ambiguous_groups
          }
          if (identical(lsap_res$resource_state, "residual_group_limit")) {
            overall_resource_state <- "residual_group_limit"
            dup_group_rows[[length(dup_group_rows) + 1L]] <- tibble::tibble(
              key = ks,
              n_reference = length(r_rows),
              n_candidate = length(c_rows),
              resource_state = "residual_group_limit"
            )
          }
        }
      } else if (length(r_rows) > 0L) {
        all_only_ref <- c(all_only_ref, r_rows)
      } else if (length(c_rows) > 0L) {
        all_only_cand <- c(all_only_cand, c_rows)
      }
    }

    pairs <- if (length(all_pairs_list) > 0L) {
      p_df <- vctrs::vec_rbind(!!!all_pairs_list)
      o <- order(p_df$reference_row, p_df$candidate_row)
      p_df[o, , drop = FALSE]
    } else {
      tibble::tibble(reference_row = integer(), candidate_row = integer())
    }

    ambig_df <- if (length(all_ambig_list) > 0L) vctrs::vec_rbind(!!!all_ambig_list) else tibble::tibble()
    dup_df <- if (length(dup_group_rows) > 0L) vctrs::vec_rbind(!!!dup_group_rows) else tibble::tibble()

    order_info <- analyze_output_order(ref_norm, cand_norm, pairs, context = context)

    return(new_row_alignment(
      method = "duplicate_key_multiset",
      candidate_keys = evaluated_candidates,
      selected_keys = sel_keys,
      pairs = pairs,
      only_reference = as.integer(sort(all_only_ref)),
      only_candidate = as.integer(sort(all_only_cand)),
      duplicate_groups = dup_df,
      ambiguous_groups = ambig_df,
      order = order_info,
      resource_state = overall_resource_state
    ))
  }

  # 3. Keyless Multiset Case (no candidate keys)
  all_cols <- intersect(names(ref_norm), names(cand_norm))
  m_res <- match_row_multisets(ref_norm, cand_norm, cols = all_cols)

  all_pairs_list <- list()
  if (nrow(m_res$pairs) > 0L) {
    all_pairs_list[[length(all_pairs_list) + 1L]] <- m_res$pairs
  }

  all_only_ref <- integer()
  all_only_cand <- integer()
  ambig_df <- tibble::tibble()
  overall_resource_state <- "complete"
  dup_df <- tibble::tibble()

  if (length(m_res$ref_residuals) > 0L || length(m_res$cand_residuals) > 0L) {
    lsap_res <- minimum_cost_residual_pairs(
      ref_norm, cand_norm,
      m_res$ref_residuals, m_res$cand_residuals,
      cols = all_cols,
      profile = profile
    )
    if (nrow(lsap_res$pairs) > 0L) {
      all_pairs_list[[length(all_pairs_list) + 1L]] <- lsap_res$pairs
    }
    all_only_ref <- lsap_res$only_reference
    all_only_cand <- lsap_res$only_candidate
    ambig_df <- lsap_res$ambiguous_groups
    if (identical(lsap_res$resource_state, "residual_group_limit")) {
      overall_resource_state <- "residual_group_limit"
      dup_df <- tibble::tibble(
        key = "<all>",
        n_reference = length(m_res$ref_residuals),
        n_candidate = length(m_res$cand_residuals),
        resource_state = "residual_group_limit"
      )
    }
  }

  pairs <- if (length(all_pairs_list) > 0L) {
    p_df <- vctrs::vec_rbind(!!!all_pairs_list)
    o <- order(p_df$reference_row, p_df$candidate_row)
    p_df[o, , drop = FALSE]
  } else {
    tibble::tibble(reference_row = integer(), candidate_row = integer())
  }

  order_info <- analyze_output_order(ref_norm, cand_norm, pairs, context = context)

  new_row_alignment(
    method = "keyless_multiset",
    candidate_keys = evaluated_candidates,
    selected_keys = character(),
    pairs = pairs,
    only_reference = as.integer(sort(all_only_ref)),
    only_candidate = as.integer(sort(all_only_cand)),
    duplicate_groups = dup_df,
    ambiguous_groups = ambig_df,
    order = order_info,
    resource_state = overall_resource_state
  )
}
