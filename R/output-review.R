# Output review bounded comparison reports, cell difference analysis, and tool interfaces

OUTPUT_REPORT_SCHEMA_VERSION <- 2L

OUTPUT_REPORT_FIELDS <- c(
  "schema_version", "report_id", "target_id", "logical_dataset", "role",
  "contributing_unit_ids", "staged_code_hashes", "structure", "alignment",
  "order", "mismatches", "examples", "resource_state", "truncated",
  "truncated_fields", "stale", "stale_reason", "reference_hash",
  "candidate_hash", "profile_hash", "schema_hash"
)

# Every field below crosses the model boundary through R/tools.R, so each one
# needs a cap of its own and the whole object needs a cap on its serialized
# size. Capping only the example rows and two value strings left the complete
# column schema of both customer datasets, every evaluated candidate key, and
# one row per variable unbounded.
#
# A per-field cap exists to keep the serialized report inside the byte budget,
# so it has to be sized against that budget rather than against a round number.
OUTPUT_REPORT_MAX_SERIALIZED_BYTES <- 65536L
OUTPUT_REPORT_MAX_VALUE_CHARS <- 200L
OUTPUT_REPORT_MAX_KEY_CHARS <- 200L
# A SAS name is at most 32 characters, so a thousand column names serialize to
# roughly half the budget -- and a thousand columns is wider than any ADaM or
# SDTM dataset. At 100 the most ordinary real input there is, an ADSL of about
# 150 columns whose whole report used a fifth of the budget, came back
# structurally abridged; so did a genuine "50 columns missing from the
# candidate" finding, since the same cap bounds those two vectors.
OUTPUT_REPORT_MAX_COLUMN_NAMES <- 1024L
# One row per differing variable, so a dataset's width bounds it too; a row is
# a name plus a few counts, so the same reasoning allows fewer of them.
OUTPUT_REPORT_MAX_VARIABLE_ROWS <- 512L
# Evaluated keys are inference bookkeeping, not findings: twenty is already
# more provenance than a reader needs.
OUTPUT_REPORT_MAX_CANDIDATE_KEYS <- 20L

#' Return default or custom bounded-report content limits
#'
#' @param max_value_chars Maximum characters per example cell value.
#' @param max_key_chars Maximum characters of joined key values per example.
#' @param max_column_names Maximum column names listed per structural vector.
#' @param max_candidate_keys Maximum evaluated candidate keys reported.
#' @param max_variable_rows Maximum per-variable mismatch rows reported.
#' @param max_serialized_bytes Maximum serialized size of the whole report.
#' @return A named list of limit constants.
#' @noRd
output_report_limits <- function(max_value_chars = OUTPUT_REPORT_MAX_VALUE_CHARS,
                                 max_key_chars = OUTPUT_REPORT_MAX_KEY_CHARS,
                                 max_column_names = OUTPUT_REPORT_MAX_COLUMN_NAMES,
                                 max_candidate_keys = OUTPUT_REPORT_MAX_CANDIDATE_KEYS,
                                 max_variable_rows = OUTPUT_REPORT_MAX_VARIABLE_ROWS,
                                 max_serialized_bytes = OUTPUT_REPORT_MAX_SERIALIZED_BYTES) {
  list(
    max_value_chars = as.integer(max_value_chars),
    max_key_chars = as.integer(max_key_chars),
    max_column_names = as.integer(max_column_names),
    max_candidate_keys = as.integer(max_candidate_keys),
    max_variable_rows = as.integer(max_variable_rows),
    max_serialized_bytes = as.numeric(max_serialized_bytes)
  )
}

#' Generate schema hash for comparison report specification
#' @noRd
comparison_report_schema_hash <- function() {
  schema_def <- paste(
    c(paste0("v", OUTPUT_REPORT_SCHEMA_VERSION), OUTPUT_REPORT_FIELDS),
    collapse = "::"
  )
  as.character(cli::hash_sha256(schema_def))
}

#' Generate deterministic report ID from target ID and bound hashes
#' @noRd
comparison_report_id <- function(target_id, hashes = list()) {
  parts <- c(
    as.character(target_id),
    as.character(hashes$reference %||% ""),
    as.character(hashes$candidate %||% ""),
    as.character(hashes$profile %||% ""),
    as.character(hashes$staged_code %||% "")
  )
  sig <- paste(parts, collapse = "::")
  h <- as.character(cli::hash_sha256(sig))
  paste0("report_", substr(h, 1, 16))
}

#' Column kind lookup keyed by case-folded column name
#'
#' Prefers the `kinds` attribute [normalize_output_frame()] records (already
#' keyed by folded name) and otherwise derives kinds from the folded frame, so
#' both paths answer to the same names.
#'
#' @param original The frame as handed in, carrying the `kinds` attribute.
#' @param folded The same frame with case-folded column names.
#' @return A named character vector of column kinds.
#' @noRd
column_kind_lookup <- function(original, folded) {
  recorded <- attr(original, "kinds")
  if (!is.null(recorded) && !is.null(names(recorded))) return(recorded)
  if (!ncol(folded)) return(stats::setNames(character(), character()))
  stats::setNames(vapply(folded, col_kind, character(1)), names(folded))
}

#' Summarize column structure and type alignment between frames
#'
#' Column identity is decided in exactly one place for the whole package:
#' [align_columns()], which case-folds names and refuses a frame whose names
#' collide after folding. Matching on raw names here would make a
#' `USUBJID`/`usubjid` pair common on the [compare_datasets()] path and
#' `only_reference`/`only_candidate` on this one.
#'
#' @param ref_norm Normalized reference frame.
#' @param cand_norm Normalized candidate frame.
#' @return A list containing structural column summaries, keyed by folded name.
#' @noRd
summarize_column_differences <- function(ref_norm, cand_norm) {
  aligned <- align_columns(ref_norm, cand_norm)

  common_cols <- aligned$common
  only_ref <- aligned$only_base
  only_cand <- aligned$only_comp

  ref_kinds <- column_kind_lookup(ref_norm, aligned$base)
  cand_kinds <- column_kind_lookup(cand_norm, aligned$comp)

  supported_kinds <- c("numeric", "date", "datetime", "time", "character", "logical")

  kind_diff_vars <- common_cols[ref_kinds[common_cols] != cand_kinds[common_cols]]
  kind_mismatches <- if (length(kind_diff_vars) > 0L) {
    tibble::tibble(
      var = kind_diff_vars,
      reference_kind = unname(ref_kinds[kind_diff_vars]),
      candidate_kind = unname(cand_kinds[kind_diff_vars])
    )
  } else {
    tibble::tibble(var = character(), reference_kind = character(), candidate_kind = character())
  }

  unsupp_ref <- common_cols[!ref_kinds[common_cols] %in% supported_kinds]
  unsupp_cand <- common_cols[!cand_kinds[common_cols] %in% supported_kinds]
  unsupp_all <- union(unsupp_ref, unsupp_cand)

  unsupported_kinds <- if (length(unsupp_all) > 0L) {
    tibble::tibble(
      var = unsupp_all,
      reference_kind = ifelse(unsupp_all %in% names(ref_kinds), unname(ref_kinds[unsupp_all]), NA_character_),
      candidate_kind = ifelse(unsupp_all %in% names(cand_kinds), unname(cand_kinds[unsupp_all]), NA_character_)
    )
  } else {
    tibble::tibble(var = character(), reference_kind = character(), candidate_kind = character())
  }

  list(
    rows_reference = nrow(ref_norm),
    rows_candidate = nrow(cand_norm),
    cols_reference = ncol(ref_norm),
    cols_candidate = ncol(cand_norm),
    columns_common = common_cols,
    columns_only_reference = only_ref,
    columns_only_candidate = only_cand,
    column_kind_mismatches = kind_mismatches,
    unsupported_column_kinds = unsupported_kinds
  )
}

#' Diff aligned cells across matched row pairs
#'
#' @param ref_norm Normalized reference frame.
#' @param cand_norm Normalized candidate frame.
#' @param pairs Matched pairs tibble (reference_row, candidate_row).
#' @param profile Comparison profile.
#' @param selected_keys Character vector of selected key variables.
#' @param max_examples Maximum number of examples to include.
#' @return A list containing `vars`, `cosmetic`, `examples`, `total_mismatches`, and `truncated`.
#' @noRd
#' Mismatches that could still survive the global example ordering
#'
#' Examples are finally chosen with `order(severity, variable, reference_row,
#' candidate_row)` truncated to `max_examples`, so within one variable only the
#' lowest-numbered rows of each severity can ever be selected. Building a record
#' for every mismatching cell and discarding all but twenty made the comparator
#' cost scale with how wrong a translation is rather than with what it reports:
#' a 32k-row frame differing in twelve columns took over five minutes, against
#' eight seconds for the same frame differing in three.
#' @noRd
example_candidate_positions <- function(severities, reference_rows,
                                        candidate_rows, max_examples) {
  keep <- logical(length(severities))
  for (severity in unique(severities)) {
    at <- which(severities == severity)
    if (length(at) > max_examples) {
      ordered <- order(reference_rows[at], candidate_rows[at], method = "radix")
      at <- at[utils::head(ordered, max_examples)]
    }
    keep[at] <- TRUE
  }
  which(keep)
}

diff_aligned_cells <- function(ref_norm, cand_norm, pairs,
                               profile = compare_profile(),
                               selected_keys = character(),
                               max_examples = OUTPUT_EVIDENCE_MAX_EXAMPLES,
                               details_cap = 0L) {
  # summarize_column_differences() reports folded names; the cell loop indexes
  # the frames by those names, so the frames must be folded too. Folding is
  # idempotent for a frame normalize_output_frame() already produced.
  folded <- align_columns(ref_norm, cand_norm)
  ref_norm <- folded$base
  cand_norm <- folded$comp

  structure_sum <- summarize_column_differences(ref_norm, cand_norm)
  cell_vars <- setdiff(
    structure_sum$columns_common,
    union(structure_sum$column_kind_mismatches$var, structure_sum$unsupported_column_kinds$var)
  )

  bi <- if (is.data.frame(pairs) && nrow(pairs) > 0L) as.integer(pairs$reference_row) else integer()
  ci <- if (is.data.frame(pairs) && nrow(pairs) > 0L) as.integer(pairs$candidate_row) else integer()
  n_pairs <- length(bi)

  pad_is_cosmetic <- identical(profile$padding, "cosmetic")
  cosmetic_list <- list()
  var_rows <- list()
  raw_examples <- list()
  total_mismatches <- 0L

  # Full-fidelity per-cell details for the local compare_datasets() contract.
  # details_cap = 0 -- the model-boundary report path -- builds none, so the
  # bounded examples stay the only raw values that can leave this function.
  details_list <- list()
  details_budget <- as.integer(details_cap)
  add_details <- function(v, bad, base_vals, comp_vals, classes) {
    if (!length(bad) || details_budget <= 0L) return(invisible(NULL))
    take <- seq_len(min(length(bad), details_budget))
    details_budget <<- details_budget - length(take)
    details_list[[length(details_list) + 1L]] <<- tibble::tibble(
      var = v,
      base_row = bi[bad[take]],
      comp_row = ci[bad[take]],
      base_value = format_cell_value(base_vals[bad[take]]),
      comp_value = format_cell_value(comp_vals[bad[take]]),
      class = classes[take]
    )
    invisible(NULL)
  }

  for (v in cell_vars) {
    b_col <- ref_norm[[v]]
    c_col <- cand_norm[[v]]
    b <- if (n_pairs > 0L) b_col[bi] else b_col[0]
    c_ <- if (n_pairs > 0L) c_col[ci] else c_col[0]

    k <- col_kind(b_col)
    tol <- effective_tol(profile, v)

    n_pad <- 0L; n_case <- 0L; n_nadiff <- 0L; n_tag <- 0L
    max_ad <- NA_real_; mean_ad <- NA_real_; diff_sd <- NA_real_

    if (k %in% c("numeric", "date", "datetime", "time")) {
      if (k == "time") {
        bn <- as.numeric(b, units = "secs")
        cn <- as.numeric(c_, units = "secs")
      } else {
        bn <- as.numeric(unclass(b))
        cn <- as.numeric(unclass(c_))
      }

      has_explicit_rel <- !is.null(profile$overrides[[tolower(v)]]$rel)
      rel_tol <- if (has_explicit_rel) tol$rel else if (k == "numeric") tol$rel else 0.0
      eq <- num_equal(bn, cn, tol$abs, rel_tol)

      if (profile$na_tags != "ignore" && (anyNA(b) || anyNA(c_))) {
        tags_ok <- na_tags_match(b, c_)
        n_tag <- sum(!tags_ok)
        if (profile$na_tags == "strict" && n_tag > 0L) {
          eq <- eq & tags_ok
        } else if (n_tag > 0L) {
          cosmetic_list[[length(cosmetic_list) + 1L]] <-
            tibble::tibble(var = v, kind = "na_tag", n = n_tag)
        }
      }

      bad <- which(!eq)
      cls_bad <- ifelse(xor(is.na(bn[bad]), is.na(cn[bad])), "na_diff", "value")

      ad <- abs(bn[bad] - cn[bad])
      ad <- ad[is.finite(ad)]
      if (length(ad)) { max_ad <- max(ad); mean_ad <- mean(ad) }

      if (length(bad) >= 3L) {
        diffs <- bn[bad] - cn[bad]
        diffs <- diffs[is.finite(diffs)]
        if (length(diffs) >= 3L) diff_sd <- stats::sd(diffs)
      }

      n_nadiff <- sum(cls_bad == "na_diff")
      n_mismatch <- length(bad)
      total_mismatches <- total_mismatches + n_mismatch
      add_details(v, bad, b, c_, cls_bad)

      if (length(bad) > 0L) {
        severities <- ifelse(cls_bad == "na_diff", 1L, 2L)
        for (idx in example_candidate_positions(
               severities, bi[bad], ci[bad], max_examples)) {
          b_i <- bad[idx]
          ref_r <- bi[b_i]
          cand_r <- ci[b_i]
          raw_examples[[length(raw_examples) + 1L]] <- list(
            severity = severities[idx],
            variable = v,
            reference_row = ref_r,
            candidate_row = cand_r,
            reference_value = format_cell_value(b[b_i]),
            candidate_value = format_cell_value(c_[b_i]),
            class = cls_bad[idx]
          )
        }
      }
    } else if (k == "character") {
      cls <- chr_classify(b, c_, sas_null_equals_na = isTRUE(profile$sas_null_equals_na))
      n_pad <- sum(cls == "padding")
      if (pad_is_cosmetic && n_pad > 0L) {
        cosmetic_list[[length(cosmetic_list) + 1L]] <-
          tibble::tibble(var = v, kind = "padding", n = n_pad)
      }
      mismatch_classes <- c("diff", "case", "na_diff", if (!pad_is_cosmetic) "padding")
      bad <- which(cls %in% mismatch_classes)
      n_case <- sum(cls == "case")
      n_nadiff <- sum(cls == "na_diff")
      n_mismatch <- length(bad)
      total_mismatches <- total_mismatches + n_mismatch
      add_details(v, bad, b, c_,
                  ifelse(cls[bad] == "case", "case",
                         ifelse(cls[bad] == "na_diff", "na_diff", "value")))

      if (length(bad) > 0L) {
        cls_bad_chr <- cls[bad]
        severities <- ifelse(cls_bad_chr == "na_diff", 1L,
                      ifelse(cls_bad_chr == "case", 3L,
                      ifelse(cls_bad_chr == "padding", 4L, 2L)))
        for (idx in example_candidate_positions(
               severities, bi[bad], ci[bad], max_examples)) {
          b_i <- bad[idx]
          ref_r <- bi[b_i]
          cand_r <- ci[b_i]
          cl_val <- cls[b_i]
          raw_examples[[length(raw_examples) + 1L]] <- list(
            severity = severities[idx],
            variable = v,
            reference_row = ref_r,
            candidate_row = cand_r,
            reference_value = format_cell_value(b[b_i]),
            candidate_value = format_cell_value(c_[b_i]),
            class = cl_val
          )
        }
      }
    } else if (k == "logical") {
      eq <- na_equal(b, c_)
      bad <- which(!eq)
      n_nadiff <- sum(xor(is.na(b[bad]), is.na(c_[bad])))
      n_mismatch <- length(bad)
      total_mismatches <- total_mismatches + n_mismatch
      add_details(v, bad, b, c_,
                  ifelse(xor(is.na(b[bad]), is.na(c_[bad])), "na_diff", "value"))

      if (length(bad) > 0L) {
        for (idx in seq_along(bad)) {
          b_i <- bad[idx]
          ref_r <- bi[b_i]
          cand_r <- ci[b_i]
          is_na_diff <- xor(is.na(b[b_i]), is.na(c_[b_i]))
          raw_examples[[length(raw_examples) + 1L]] <- list(
            severity = ifelse(is_na_diff, 1L, 2L),
            variable = v,
            reference_row = ref_r,
            candidate_row = cand_r,
            reference_value = format_cell_value(b[b_i]),
            candidate_value = format_cell_value(c_[b_i]),
            class = ifelse(is_na_diff, "na_diff", "value")
          )
        }
      }
    }

    n_cosmetic_val <- if (k == "character" && pad_is_cosmetic) n_pad else 0L
    var_rows[[v]] <- tibble::tibble(
      var = v, kind = k, n_compared = n_pairs,
      n_mismatch = n_mismatch, n_cosmetic = n_cosmetic_val,
      n_na_diff = n_nadiff, n_case = n_case,
      max_abs_diff = max_ad, mean_abs_diff = mean_ad,
      diff_sd = diff_sd,
      tag_mismatches = n_tag
    )
  }

  vars_df <- if (length(var_rows) > 0L) {
    vctrs::vec_rbind(!!!var_rows)
  } else {
    tibble::tibble(
      var = character(), kind = character(), n_compared = integer(),
      n_mismatch = integer(), n_cosmetic = integer(),
      n_na_diff = integer(), n_case = integer(),
      max_abs_diff = double(), mean_abs_diff = double(),
      diff_sd = double(),
      tag_mismatches = integer()
    )
  }

  cosmetic_df <- if (length(cosmetic_list) > 0L) {
    vctrs::vec_rbind(!!!cosmetic_list)
  } else {
    tibble::tibble(var = character(), kind = character(), n = integer())
  }

  # Build bounded deterministic example excerpts
  truncated <- total_mismatches > max_examples

  if (length(raw_examples) > 0L) {
    sev_vec <- vapply(raw_examples, `[[`, integer(1), "severity")
    var_vec <- vapply(raw_examples, `[[`, character(1), "variable")
    ref_r_vec <- vapply(raw_examples, `[[`, integer(1), "reference_row")
    cand_r_vec <- vapply(raw_examples, `[[`, integer(1), "candidate_row")

    ord <- order(sev_vec, var_vec, ref_r_vec, cand_r_vec, method = "radix")
    chosen_indices <- utils::head(ord, max_examples)
    chosen_examples <- raw_examples[chosen_indices]

    ex_ref_row <- vapply(chosen_examples, `[[`, integer(1), "reference_row")
    ex_cand_row <- vapply(chosen_examples, `[[`, integer(1), "candidate_row")
    ex_var <- vapply(chosen_examples, `[[`, character(1), "variable")
    ex_ref_val <- vapply(chosen_examples, `[[`, character(1), "reference_value")
    ex_cand_val <- vapply(chosen_examples, `[[`, character(1), "candidate_value")

    # Format key values
    ex_key_vals <- character(length(chosen_examples))
    if (length(selected_keys) > 0L && all(selected_keys %in% names(ref_norm))) {
      for (i in seq_along(chosen_examples)) {
        r_num <- ex_ref_row[i]
        vals <- vapply(selected_keys, function(sk) {
          format_cell_value(ref_norm[[sk]][r_num])
        }, character(1))
        ex_key_vals[i] <- paste(vals, collapse = ", ")
      }
    } else {
      ex_key_vals <- as.character(ex_ref_row)
    }

    # Value and key length caps belong to bound_comparison_report(), which is
    # the one place that can also record that a cap omitted detail. Clipping
    # here as well would leave nothing over the cap for it to notice, and the
    # report would say `truncated = FALSE` about a value it had shortened.
    examples_df <- tibble::tibble(
      reference_row = as.integer(ex_ref_row),
      candidate_row = as.integer(ex_cand_row),
      key_values = as.character(ex_key_vals),
      variable = as.character(ex_var),
      reference_value = as.character(ex_ref_val),
      candidate_value = as.character(ex_cand_val)
    )
  } else {
    examples_df <- tibble::tibble(
      reference_row = integer(),
      candidate_row = integer(),
      key_values = character(),
      variable = character(),
      reference_value = character(),
      candidate_value = character()
    )
  }

  details_df <- NULL
  if (details_cap > 0L) {
    details_df <- if (length(details_list) > 0L) {
      vctrs::vec_rbind(!!!details_list)
    } else {
      tibble::tibble(var = character(), base_row = integer(),
                     comp_row = integer(), base_value = character(),
                     comp_value = character(), class = character())
    }
    attr(details_df, "details_truncated") <- total_mismatches > details_cap
    attr(details_df, "details_cap") <- as.integer(details_cap)
  }

  list(
    vars = vars_df,
    cosmetic = cosmetic_df,
    examples = examples_df,
    details = details_df,
    total_mismatches = total_mismatches,
    truncated = truncated
  )
}

#' Serialized size of a comparison report in bytes
#'
#' Measured with the same `jsonlite` call [write_comparison_report()] uses, so
#' the number bounds the bytes that actually cross the model boundary rather
#' than an unrelated in-memory footprint.
#'
#' @param report A comparison report list.
#' @return Size in bytes, or `Inf` when the report cannot be serialized.
#' @noRd
comparison_report_serialized_bytes <- function(report) {
  tryCatch(
    {
      json <- jsonlite::toJSON(unclass(report), auto_unbox = TRUE,
                               dataframe = "rows", null = "null")
      sum(nchar(as.character(json), type = "bytes"))
    },
    error = function(e) Inf
  )
}

# The shrinkable slots, in the fixed order `which.max()` breaks ties on. Each
# names a countable collection inside the report; halving the largest one until
# the report fits is deterministic and terminates, because every step strictly
# reduces a non-negative integer.
COMPARISON_REPORT_SHRINK_SLOTS <- c(
  "examples", "by_variable", "cosmetic", "candidate_keys",
  "columns_common", "columns_only_reference", "columns_only_candidate"
)

# The closed vocabulary of `truncated_fields`. Every shrinkable slot can be
# named, plus the two string clips that shorten a value in place rather than
# dropping a row, plus the evidence limit that fired before assembly.
COMPARISON_REPORT_TRUNCATION_FIELDS <- c(
  COMPARISON_REPORT_SHRINK_SLOTS, "example_values", "example_keys",
  "resource_state"
)

#' Current element count of one shrinkable report slot
#' @noRd
comparison_report_slot_size <- function(report, slot) {
  value <- switch(
    slot,
    examples = report$examples,
    by_variable = report$mismatches$by_variable,
    cosmetic = report$mismatches$cosmetic,
    candidate_keys = report$alignment$candidate_keys,
    columns_common = report$structure$columns_common,
    columns_only_reference = report$structure$columns_only_reference,
    columns_only_candidate = report$structure$columns_only_candidate,
    NULL
  )
  if (is.null(value)) return(0L)
  if (is.data.frame(value)) return(as.integer(nrow(value)))
  as.integer(length(value))
}

#' Keep only the first `n` elements of one shrinkable report slot
#' @noRd
comparison_report_slot_head <- function(report, slot, n) {
  n <- max(as.integer(n), 0L)
  trim <- function(value) {
    if (is.null(value)) return(value)
    if (is.data.frame(value)) return(value[seq_len(min(n, nrow(value))), , drop = FALSE])
    utils::head(value, n)
  }
  switch(
    slot,
    examples = report$examples <- trim(report$examples),
    by_variable = report$mismatches$by_variable <- trim(report$mismatches$by_variable),
    cosmetic = report$mismatches$cosmetic <- trim(report$mismatches$cosmetic),
    candidate_keys = report$alignment$candidate_keys <- trim(report$alignment$candidate_keys),
    columns_common = report$structure$columns_common <- trim(report$structure$columns_common),
    columns_only_reference =
      report$structure$columns_only_reference <- trim(report$structure$columns_only_reference),
    columns_only_candidate =
      report$structure$columns_only_candidate <- trim(report$structure$columns_only_candidate)
  )
  report
}

#' Cap every model-visible field of a comparison report
#'
#' Applies the per-field caps first, then shrinks the largest remaining
#' collection until the serialized report fits the byte budget.
#'
#' @param report A comparison report list.
#' @param limits Report content limits from [output_report_limits()].
#' @return A list with the capped `report` and `omitted`, the names of the
#'   fields a cap took detail out of. A reader has to be able to tell an
#'   abridged column list from dropped mismatch examples: one boolean standing
#'   for five different omissions tells them only that something is missing.
#' @noRd
bound_comparison_report <- function(report, limits = output_report_limits()) {
  omitted <- character()
  note <- function(field) omitted <<- unique(c(omitted, field))

  clip <- function(x, n, field) {
    if (!length(x)) return(x)
    x <- as.character(x)
    too_long <- !is.na(x) & nchar(x, type = "chars") > n
    if (any(too_long)) {
      x[too_long] <- substr(x[too_long], 1L, n)
      note(field)
    }
    x
  }

  if (is.data.frame(report$examples) && nrow(report$examples) > 0L) {
    report$examples$reference_value <-
      clip(report$examples$reference_value, limits$max_value_chars, "example_values")
    report$examples$candidate_value <-
      clip(report$examples$candidate_value, limits$max_value_chars, "example_values")
    report$examples$key_values <-
      clip(report$examples$key_values, limits$max_key_chars, "example_keys")
  }

  cap_slot <- function(rep, slot, cap) {
    size <- comparison_report_slot_size(rep, slot)
    if (size > cap) {
      note(slot)
      rep <- comparison_report_slot_head(rep, slot, cap)
    }
    rep
  }

  for (slot in c("columns_common", "columns_only_reference", "columns_only_candidate")) {
    report <- cap_slot(report, slot, limits$max_column_names)
  }
  report <- cap_slot(report, "candidate_keys", limits$max_candidate_keys)
  report <- cap_slot(report, "by_variable", limits$max_variable_rows)
  report <- cap_slot(report, "cosmetic", limits$max_variable_rows)

  # Per-field caps cannot bound the total on their own: a hundred column names
  # and a hundred variable rows are each small and together are not.
  guard <- 0L
  while (comparison_report_serialized_bytes(report) > limits$max_serialized_bytes) {
    guard <- guard + 1L
    if (guard > 256L) break
    sizes <- vapply(COMPARISON_REPORT_SHRINK_SLOTS,
                    function(s) comparison_report_slot_size(report, s), integer(1))
    if (all(sizes <= 0L)) break
    slot <- COMPARISON_REPORT_SHRINK_SLOTS[[which.max(sizes)]]
    report <- comparison_report_slot_head(report, slot, sizes[[which.max(sizes)]] %/% 2L)
    note(slot)
  }

  list(report = report, omitted = omitted)
}

#' Validate a comparison report object against closed schema and limits
#'
#' @param report A comparison report list.
#' @param limits Evidence limits list.
#' @param content_limits Bounded-report content limits from
#'   [output_report_limits()].
#' @return The report invisibly if valid.
#' @noRd
validate_comparison_report <- function(report, limits = output_evidence_limits(),
                                       content_limits = output_report_limits()) {
  if (!is.list(report)) {
    cli::cli_abort("Comparison report must be a list.",
                   class = "sas2r_invalid_report_error")
  }

  missing_fields <- setdiff(OUTPUT_REPORT_FIELDS, names(report))
  if (length(missing_fields) > 0L) {
    cli::cli_abort("Comparison report missing required fields: {.val {missing_fields}}.",
                   class = "sas2r_invalid_report_error")
  }

  if (!identical(as.integer(report$schema_version), OUTPUT_REPORT_SCHEMA_VERSION)) {
    cli::cli_abort("Unsupported schema version {.val {report$schema_version}}.",
                   class = "sas2r_invalid_report_error")
  }

  if (!is_scalar_character(report$report_id) || !nzchar(report$report_id) ||
      grepl("[/\\\\]|\\.\\.", report$report_id)) {
    cli::cli_abort("Invalid report ID {.val {report$report_id}}.",
                   class = "sas2r_invalid_report_error")
  }

  if (!is_scalar_character(report$target_id) || !nzchar(report$target_id)) {
    cli::cli_abort("Invalid target ID in comparison report.",
                   class = "sas2r_invalid_report_error")
  }

  if (!is.data.frame(report$examples)) {
    cli::cli_abort("Report examples must be a data frame.",
                   class = "sas2r_invalid_report_error")
  }

  # The pair is one statement in two parts. A report claiming truncation while
  # naming no field, or naming fields while claiming none, tells the reader
  # nothing they can rely on.
  if (!is.character(report$truncated_fields) || anyNA(report$truncated_fields)) {
    cli::cli_abort("Report {.field truncated_fields} must be a character vector.",
                   class = "sas2r_invalid_report_error")
  }
  if (!isTRUE(report$truncated) && !isFALSE(report$truncated)) {
    cli::cli_abort("Report {.field truncated} must be TRUE or FALSE.",
                   class = "sas2r_invalid_report_error")
  }
  if (!identical(isTRUE(report$truncated), length(report$truncated_fields) > 0L)) {
    cli::cli_abort(
      c("Report {.field truncated} disagrees with {.field truncated_fields}.",
        "x" = "truncated = {report$truncated} with {length(report$truncated_fields)} field{?s} named."),
      class = "sas2r_invalid_report_error"
    )
  }
  unknown_trunc <- setdiff(report$truncated_fields, COMPARISON_REPORT_TRUNCATION_FIELDS)
  if (length(unknown_trunc) > 0L) {
    cli::cli_abort("Unknown truncated field{?s}: {.val {unknown_trunc}}.",
                   class = "sas2r_invalid_report_error")
  }

  required_example_cols <- c("reference_row", "candidate_row", "key_values",
                             "variable", "reference_value", "candidate_value")
  missing_ex_cols <- setdiff(required_example_cols, names(report$examples))
  if (length(missing_ex_cols) > 0L) {
    cli::cli_abort("Report examples missing columns: {.val {missing_ex_cols}}.",
                   class = "sas2r_invalid_report_error")
  }

  if (nrow(report$examples) > limits$max_examples) {
    cli::cli_abort("Report examples exceed cap ({nrow(report$examples)} > {limits$max_examples}).",
                   class = "sas2r_invalid_report_error")
  }

  # The example-row cap alone never bounded the object the model is handed.
  # Each remaining model-visible collection answers to its own cap here, so a
  # report assembled outside new_comparison_report() cannot reach a tool
  # unbounded either.
  slot_caps <- c(
    columns_common = content_limits$max_column_names,
    columns_only_reference = content_limits$max_column_names,
    columns_only_candidate = content_limits$max_column_names,
    candidate_keys = content_limits$max_candidate_keys,
    by_variable = content_limits$max_variable_rows,
    cosmetic = content_limits$max_variable_rows
  )
  for (slot in names(slot_caps)) {
    size <- comparison_report_slot_size(report, slot)
    if (size > slot_caps[[slot]]) {
      cli::cli_abort(
        "Report field {.field {slot}} exceeds cap ({size} > {slot_caps[[slot]]}).",
        class = "sas2r_invalid_report_error"
      )
    }
  }

  # Privacy and containment check: no full dataset attached
  for (nm in names(report)) {
    if (nm %in% c("reference", "candidate") && inherits(report[[nm]], "data.frame")) {
      cli::cli_abort("Comparison report must never attach full dataset frames.",
                     class = "sas2r_invalid_report_error")
    }
  }

  invisible(report)
}

#' Construct a new comparison report object
#'
#' @param target Discovered target metadata row or list.
#' @param alignment Row alignment result object.
#' @param comparison Comparison differences list.
#' @param examples Bounded examples data frame.
#' @param hashes Hash list.
#' @param resource_state Resource status string.
#' @param content_limits Bounded-report content limits from
#'   [output_report_limits()].
#' @return An object of class `sas2r_comparison_report`.
#' @noRd
new_comparison_report <- function(target, alignment, comparison,
                                  examples, hashes, resource_state,
                                  content_limits = output_report_limits()) {
  t_id <- if (is.data.frame(target)) target$target_id[1] else target$target_id
  l_ds <- if (is.data.frame(target)) target$logical_dataset[1] else target$logical_dataset
  r_role <- if (is.data.frame(target)) target$role[1] else target$role
  c_units <- if (is.data.frame(target)) {
    if (is.list(target$contributing_unit_ids)) as.integer(target$contributing_unit_ids[[1]])
    else as.integer(target$contributing_unit_ids)
  } else {
    if (is.list(target$contributing_unit_ids)) as.integer(target$contributing_unit_ids[[1]])
    else as.integer(target$contributing_unit_ids)
  }

  rep_id <- hashes$report_id %||% comparison_report_id(t_id, hashes)

  align_sum <- if (inherits(alignment, "sas2r_row_alignment")) {
    list(
      method = alignment$method,
      selected_keys = alignment$selected_keys,
      candidate_keys = alignment$candidate_keys,
      pairs_count = nrow(alignment$pairs),
      only_reference_count = length(alignment$only_reference),
      only_candidate_count = length(alignment$only_candidate),
      duplicate_groups_count = if (is.data.frame(alignment$duplicate_groups)) nrow(alignment$duplicate_groups) else 0L,
      ambiguous_groups_count = if (is.data.frame(alignment$ambiguous_groups)) nrow(alignment$ambiguous_groups) else 0L,
      resource_state = alignment$resource_state
    )
  } else {
    as.list(alignment)
  }

  mismatches_sum <- list(
    total_mismatch_cells = comparison$total_mismatches %||% 0L,
    by_variable = comparison$vars %||% tibble::tibble(),
    cosmetic = comparison$cosmetic %||% tibble::tibble()
  )

  # Named, not counted: an abridged column list and a dropped mismatch example
  # are different findings, and the reader has to be able to act on which one
  # happened. `resource_state` names itself here because evidence was already
  # limited before the report was assembled.
  truncated_fields <- character()
  if (!identical(resource_state, "complete")) {
    truncated_fields <- c(truncated_fields, "resource_state")
  }
  if (isTRUE(comparison$truncated) || isTRUE(attr(examples, "truncated"))) {
    truncated_fields <- c(truncated_fields, "examples")
  }

  report <- list(
    schema_version = OUTPUT_REPORT_SCHEMA_VERSION,
    report_id = rep_id,
    target_id = t_id,
    logical_dataset = l_ds,
    role = r_role,
    contributing_unit_ids = c_units,
    staged_code_hashes = hashes$staged_code %||% character(),
    structure = comparison$structure %||% list(),
    alignment = align_sum,
    order = alignment$order %||% list(meaningful = FALSE),
    mismatches = mismatches_sum,
    examples = examples,
    resource_state = resource_state,
    truncated = length(truncated_fields) > 0L,
    truncated_fields = sort(unique(truncated_fields)),
    stale = FALSE,
    stale_reason = NA_character_,
    reference_hash = hashes$reference %||% "",
    candidate_hash = hashes$candidate %||% "",
    profile_hash = hashes$profile %||% "",
    schema_hash = comparison_report_schema_hash()
  )

  bounded <- bound_comparison_report(report, limits = content_limits)
  report <- bounded$report
  report$truncated_fields <- sort(unique(c(truncated_fields, bounded$omitted)))
  report$truncated <- length(report$truncated_fields) > 0L

  validate_comparison_report(report, content_limits = content_limits)
  structure(report, class = c("sas2r_comparison_report", "sas2r_output_comparison_report"))
}

#' Compare aligned outputs under SAS-aware tolerance rules
#'
#' @param reference Reference data frame or tibble.
#' @param candidate Candidate data frame or tibble.
#' @param target Discovered target metadata row or list.
#' @param context Optional alignment context list.
#' @param profile Comparison profile.
#' @param hashes Hash list.
#' @param resource_state Resource status string.
#' @param max_examples Maximum number of examples to include.
#' @return An object of class `sas2r_comparison_report`.
#' @export
compare_aligned_outputs <- function(reference, candidate, target,
                                    context = NULL,
                                    profile = compare_profile(),
                                    hashes = list(),
                                    resource_state = "complete",
                                    max_examples = OUTPUT_EVIDENCE_MAX_EXAMPLES) {
  ref_norm <- normalize_output_frame(reference, profile = profile)
  cand_norm <- normalize_output_frame(candidate, profile = profile)

  # The normalized pair is threaded down instead of re-derived. Handing the raw
  # frames to align_output_rows() cost a second full pass, plus two more per
  # candidate key inside validate_alignment_key() -- a dozen passes over a
  # 50,000 x 500 frame for one comparison.
  alignment <- align_output_rows(ref_norm, cand_norm, context = context,
                                 profile = profile, normalized = TRUE)

  structure_sum <- summarize_column_differences(ref_norm, cand_norm)
  diff_res <- diff_aligned_cells(
    ref_norm, cand_norm, alignment$pairs,
    profile = profile,
    selected_keys = alignment$selected_keys,
    max_examples = max_examples
  )
  diff_res$structure <- structure_sum

  ref_h <- hashes$reference %||% {
    as.character(cli::hash_sha256(paste(row_signatures(ref_norm), collapse = "\n")))
  }
  cand_h <- hashes$candidate %||% {
    as.character(cli::hash_sha256(paste(row_signatures(cand_norm), collapse = "\n")))
  }
  prof_h <- hashes$profile %||% {
    as.character(cli::hash_sha256(paste(
      profile$version, profile$tol_abs, profile$tol_rel,
      profile$padding, profile$na_tags, profile$sas_null_equals_na,
      sep = "::"
    )))
  }

  hashes$reference <- ref_h
  hashes$candidate <- cand_h
  hashes$profile <- prof_h

  overall_resource_state <- if (alignment$resource_state != "complete") {
    alignment$resource_state
  } else {
    resource_state
  }

  new_comparison_report(
    target = target,
    alignment = alignment,
    comparison = diff_res,
    examples = diff_res$examples,
    hashes = hashes,
    resource_state = overall_resource_state
  )
}

#' @noRd
build_output_comparison_report <- compare_aligned_outputs

#' Read a bounded comparison report from the registry
#'
#' @param report_id Report identifier string.
#' @param registry Registry environment or list.
#' @return An object of class `sas2r_comparison_report`.
#' @export
read_comparison_report <- function(report_id, registry) {
  if (!is_scalar_character(report_id) || !nzchar(report_id)) {
    cli::cli_abort("Invalid report ID {.val {report_id}}.",
                   class = "sas2r_report_not_registered")
  }
  # Path-shaped input is refused on its own terms, with its own condition
  # subclass, so the traversal guard stays distinguishable from an ordinary
  # unknown identifier. The subclass keeps the established parent class.
  if (grepl("[/\\\\]|\\.\\.", report_id)) {
    cli::cli_abort(
      "Report {.val {report_id}} looks like a path; only registered report IDs are accepted.",
      class = c("sas2r_report_path_rejected", "sas2r_report_not_registered")
    )
  }

  report <- if (is.environment(registry)) {
    if (!exists(report_id, envir = registry, inherits = FALSE)) {
      cli::cli_abort("Report {.val {report_id}} not found in output review registry.",
                     class = "sas2r_report_not_registered")
    }
    get(report_id, envir = registry)
  } else if (is.list(registry)) {
    if (!report_id %in% names(registry)) {
      cli::cli_abort("Report {.val {report_id}} not found in output review registry.",
                     class = "sas2r_report_not_registered")
    }
    registry[[report_id]]
  } else {
    cli::cli_abort("Invalid registry object.",
                   class = "sas2r_report_not_registered")
  }

  validate_comparison_report(report)
  report
}

#' @export
print.sas2r_comparison_report <- function(x, ...) {
  cli::cat_line(cli::rule(left = "sas2r output comparison report"))
  cli::cat_line(paste0("report_id: ", x$report_id))
  cli::cat_line(sprintf("target_id: %s (%s, %s)", x$target_id, x$logical_dataset, x$role))
  cli::cat_line(sprintf("dimensions: reference %dx%d, candidate %dx%d",
                        x$structure$rows_reference, x$structure$cols_reference,
                        x$structure$rows_candidate, x$structure$cols_candidate))
  cli::cat_line(sprintf("alignment: %s (matched %d pairs, only_ref %d, only_cand %d)",
                        x$alignment$method, x$alignment$pairs_count,
                        x$alignment$only_reference_count, x$alignment$only_candidate_count))
  cli::cat_line(sprintf("mismatches: %d cell difference(s)", x$mismatches$total_mismatch_cells))
  cli::cat_line(sprintf("order: meaningful=%s, equivalent=%s (%s)",
                        x$order$meaningful, x$order$order_equivalent, x$order$reason))
  cli::cat_line(sprintf("examples: %d example(s)", nrow(x$examples)))
  cli::cat_line(paste0("resource_state: ", x$resource_state))
  trunc_fields <- x$truncated_fields %||% character()
  cli::cat_line(sprintf(
    "truncated: %s",
    if (length(trunc_fields)) paste(trunc_fields, collapse = ", ")
    else if (isTRUE(x$truncated)) "yes" else "no"
  ))
  invisible(x)
}

#' @export
as.data.frame.sas2r_comparison_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$examples, row.names = row.names, optional = optional, ...)
}

#' Split a SAS `BY` statement into variables and sort directions
#'
#' `DESCENDING` is a prefix modifier on the variable that follows it, which is
#' how [emit_proc_sort()] already reads the same clause.
#'
#' @param text One `BY` statement's text.
#' @return A list with `vars` (lowercase) and `descending` (logical, aligned).
#' @noRd
parse_sas_by_clause <- function(text) {
  empty <- list(vars = character(), descending = logical())
  if (!is_scalar_character(text) || is.na(text) || !nzchar(trimws(text))) return(empty)
  body <- trimws(sub("^by(\\s+|$)", "", trimws(text), ignore.case = TRUE))
  if (!nzchar(body)) return(empty)
  toks <- tolower(strsplit(body, "\\s+")[[1]])
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(empty)

  vars <- character()
  descending <- logical()
  pending_desc <- FALSE
  for (tok in toks) {
    if (tok %in% c("descending", "notsorted")) {
      pending_desc <- pending_desc || identical(tok, "descending")
      next
    }
    vars <- c(vars, tok)
    descending <- c(descending, pending_desc)
    pending_desc <- FALSE
  }
  list(vars = vars, descending = descending)
}

#' Contributing units that write one output target
#'
#' A dataset's row order is set by the step that writes it. A `PROC SORT` that
#' orders some *other* dataset is upstream lineage and nothing more: adopting
#' its `BY` -- and its `DESCENDING` flags -- as the target's order contract
#' invents an order the target never had, which then decides whether a reorder
#' is a genuine defect and whether a missing-value gap is SAS collation.
#'
#' @param lineage The project's dataset lineage tibble.
#' @param target_ds The target's logical dataset name.
#' @return `NULL` when lineage cannot answer the question, otherwise a list of
#'   `creates` (units that write the target, including `PROC SORT ... OUT=`)
#'   and `reads_only` (units that name the target as input and write nothing,
#'   which is how an in-place `PROC SORT` rewrites it in place).
#' @noRd
output_target_writer_units <- function(lineage, target_ds) {
  if (!is_scalar_character(target_ds) || is.na(target_ds) || !nzchar(target_ds)) {
    return(NULL)
  }
  if (is.null(lineage) || !is.data.frame(lineage) || !nrow(lineage) ||
      !all(c("unit_id", "dataset", "role") %in% names(lineage))) {
    return(NULL)
  }
  ds <- tolower(as.character(lineage$dataset))
  writes <- !is.na(lineage$role) & lineage$role == "creates"
  hit <- !is.na(ds) & ds == tolower(target_ds)
  list(
    creates = sort(unique(as.integer(lineage$unit_id[writes & hit]))),
    reads_only = sort(setdiff(
      unique(as.integer(lineage$unit_id[!writes & hit])),
      unique(as.integer(lineage$unit_id[writes]))
    ))
  )
}

#' Build the source-derived alignment context for one output target
#'
#' [infer_alignment_keys()] ranks BY, latest PROC SORT, merge keys, and creator
#' lineage above its schema heuristic, and [analyze_output_order()] can only
#' call a reorder meaningful when a source order contract reaches it. Both take
#' that half of their input from here; passing `NULL` silences it entirely.
#'
#' Key candidates are permissive -- every one is validated for uniqueness
#' before it is used, so an upstream sort's or merge's variables are a fair
#' guess. The *order* contract is not: it is asserted about the target itself,
#' so it is taken only from a unit that writes the target.
#'
#' @param target_row One row of the output target plan.
#' @param project The `sas2r_project` the translation was produced from.
#' @return An alignment context list; the empty context when the source says
#'   nothing about row identity or order.
#' @noRd
build_output_alignment_context <- function(target_row, project) {
  empty <- empty_alignment_context()
  if (is.null(project) || is.null(project$statements) ||
      !is.data.frame(project$statements) || !nrow(project$statements)) {
    return(empty)
  }

  units <- if (is.data.frame(target_row)) target_row$contributing_unit_ids else
    target_row[["contributing_unit_ids"]]
  units <- suppressWarnings(as.integer(unlist(units)))
  units <- sort(unique(units[!is.na(units)]))
  if (!length(units)) return(empty)

  stmts <- project$statements
  if (!all(c("unit_id", "text") %in% names(stmts))) return(empty)
  has_tokens <- "first_token" %in% names(stmts)

  target_ds <- if (is.data.frame(target_row)) target_row$logical_dataset else
    target_row[["logical_dataset"]]
  target_ds <- if (length(target_ds)) as.character(target_ds)[1] else NA_character_
  if (!is.na(target_ds)) target_ds <- tolower(trimws(target_ds))
  writers <- output_target_writer_units(project$lineage, target_ds)

  merge_vars <- character()
  lineage_vars <- character()
  contracts <- list()

  unit_types <- if (!is.null(project$units) && is.data.frame(project$units) &&
                    all(c("unit_id", "unit_type") %in% names(project$units))) {
    stats::setNames(as.character(project$units$unit_type), as.character(project$units$unit_id))
  } else {
    character()
  }
  # `[[` on a name this vector does not carry -- and on the zero-length
  # fallback above, which carries no names at all -- raises "subscript out of
  # bounds" rather than answering NULL, so a `%||%` default after it is
  # unreachable. Single-bracket indexing answers NA, which is defaultable.
  unit_type_of <- function(uid) {
    hit <- unname(unit_types[as.character(uid)])
    if (length(hit) != 1L || is.na(hit)) "" else hit
  }

  # Ascending unit order, so that among the units writing the target the last
  # contract found is the operative one.
  for (uid in units) {
    rows <- stmts[!is.na(stmts$unit_id) & stmts$unit_id == uid, , drop = FALSE]
    if (!nrow(rows)) next
    if ("type" %in% names(rows)) rows <- rows[rows$type == "code", , drop = FALSE]
    if (!nrow(rows)) next

    tokens <- if (has_tokens) tolower(as.character(rows$first_token)) else
      tolower(sub("\\s.*$", "", trimws(as.character(rows$text))))
    by_text <- as.character(rows$text)[tokens == "by"]
    if (!length(by_text)) next
    parsed <- parse_sas_by_clause(by_text[1])
    if (!length(parsed$vars)) next

    lineage_vars <- union(lineage_vars, parsed$vars)

    u_type <- unit_type_of(uid)
    proc_text <- as.character(rows$text)[tokens == "proc"]
    is_sort <- startsWith(u_type, "proc") &&
      length(proc_text) > 0L &&
      grepl("^\\s*proc\\s+sort\\b", tolower(proc_text[1]))

    kind <- if (is_sort) "sort" else if (any(tokens == "merge")) "merge" else "by"
    if (identical(kind, "merge")) merge_vars <- parsed$vars

    # `OUT=<target>` writes the target; an in-place `PROC SORT` names it on
    # `DATA=` and writes nothing. Anything else -- a sort of an upstream
    # dataset, a `PROC PRINT ... BY` -- leaves the target's order untouched.
    writes_target <- if (is.null(writers)) {
      NA
    } else {
      uid %in% writers$creates || (is_sort && uid %in% writers$reads_only)
    }

    contracts[[length(contracts) + 1L]] <- list(
      uid = uid, kind = kind, vars = parsed$vars,
      descending = parsed$descending, writes_target = writes_target
    )
  }

  if (!length(contracts)) return(empty)

  # With no lineage to say which unit writes the target, the last contributing
  # unit is the only stand-in available. With lineage saying none of them does,
  # the target has no source order contract at all -- which is the honest
  # answer, and the one that leaves order analysis inert.
  writing <- Filter(function(ct) isTRUE(ct$writes_target), contracts)
  operative <- if (length(writing)) {
    writing[[length(writing)]]
  } else if (is.null(writers)) {
    contracts[[length(contracts)]]
  } else {
    NULL
  }

  by_vars <- character()
  sort_vars <- character()
  sort_desc <- logical()
  if (!is.null(operative)) {
    # A `PROC SORT` states the output order outright. A DATA step `BY`, with
    # `MERGE` or with `SET`, states it just as firmly -- SAS requires the
    # inputs in that order and writes the output in it -- and its `DESCENDING`
    # flags are part of the statement.
    sort_vars <- operative$vars
    sort_desc <- operative$descending
    # `by` is the highest-precedence key contract in infer_alignment_keys(), so
    # it carries only a plain BY of the writing unit; a merge's keys are
    # reported as merge keys and a sort's as a sort.
    if (identical(operative$kind, "by")) by_vars <- operative$vars
  }

  known <- unique(c(by_vars, sort_vars, merge_vars, lineage_vars))
  if (!length(known)) return(empty)

  list(
    by = by_vars,
    sort = list(vars = sort_vars, descending = sort_desc),
    merge = list(by = merge_vars),
    lineage = list(keys = setdiff(lineage_vars, c(by_vars, sort_vars, merge_vars))),
    known_identifiers = known,
    order_contract = NULL,
    collation = NULL
  )
}
