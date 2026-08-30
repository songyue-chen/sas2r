DETAILS_CAP <- 1000L

format_cell_value <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  if (inherits(x, "POSIXct")) {
    return(format(x, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"))
  }
  if (is.double(x)) {
    tagged <- haven::is_tagged_na(x)
    if (any(tagged)) {
      out <- format(x, digits = 17, trim = TRUE)
      out[tagged] <- paste0(".", haven::na_tag(x[tagged]))
      return(out)
    }
    return(format(x, digits = 17, trim = TRUE))
  }
  as.character(x)
}


#' Compare two datasets under SAS-aware tolerance rules
#'
#' A thin wrapper over the shared cell comparator `diff_aligned_cells()`: rows
#' pair by the given keys (or in row order when `keys` is `NULL`), and every
#' cell verdict comes from the same loop the output-review engine uses.
#'
#' @param base The reference dataset (e.g. read from a SAS golden output).
#' @param comp The candidate dataset (e.g. produced by translated R).
#' @param profile A [compare_profile()].
#' @param keys Key columns for row matching; NULL compares in row order.
#' @return An object of class `sas2r_comparison`.
#' @examples
#' sas <- data.frame(usubjid = c("01-001", "01-002", "01-003"),
#'                   aval = c(1.0, 2.0, 3.0))
#' r <- data.frame(usubjid = c("01-001", "01-002", "01-003"),
#'                 aval = c(1.0, 2.0, 3.0 + 1e-10))
#'
#' # A floating-point difference inside tolerance is not a mismatch.
#' cmp <- compare_datasets(sas, r, keys = "usubjid")
#' passed(cmp)
#'
#' # A genuine difference is reported.
#' r$aval[3] <- 3.5
#' passed(compare_datasets(sas, r, keys = "usubjid"))
#' @export
compare_datasets <- function(base, comp, profile = compare_profile(),
                             keys = profile$keys) {
  al <- align_columns(base, comp)
  if (!is.null(keys)) keys <- tolower(keys)
  mk <- match_rows(al$base, al$comp, keys)
  compare_datasets_impl(
    al = al, profile = profile, keys = keys,
    pairing = list(
      base_idx = mk$base_idx,
      comp_idx = mk$comp_idx,
      rows_only_base = length(mk$only_base),
      rows_only_comp = length(mk$only_comp),
      dup_fanout = mk$dup_fanout,
      structure_extras = list()
    )
  )
}

#' Compare two datasets with engine row alignment
#'
#' The migration gate's comparison path: rows pair through
#' [align_output_rows()] -- key inference from schema identifiers, uniqueness
#' validation, duplicate-key multiset matching, and keyless multiset fallback
#' -- instead of positional matching, so a content-equal reorder is not
#' reported as a wall of cell mismatches. Configured `keys` steer key selection
#' as row identity; they are deliberately not treated as an order contract.
#'
#' @param base The reference dataset.
#' @param comp The candidate dataset.
#' @param profile A [compare_profile()].
#' @param keys Optional key columns; inferred from the data when `NULL`.
#' @return An object of class `sas2r_comparison` whose `structure` also
#'   carries `alignment_method`, `selected_keys`, `order`, and
#'   `alignment_resource_state`.
#' @noRd
compare_datasets_aligned <- function(base, comp, profile = compare_profile(),
                                     keys = NULL) {
  al <- align_columns(base, comp)
  if (!is.null(keys)) {
    keys <- tolower(keys)
    missing_keys <- setdiff(keys, al$common)
    if (length(missing_keys)) {
      cli::cli_abort(
        "Key column{?s} missing from the compared datasets: {.val {missing_keys}}.",
        class = "sas2r_invalid_argument"
      )
    }
  }

  ref_eng <- normalize_output_frame(al$base, profile = profile)
  cand_eng <- normalize_output_frame(al$comp, profile = profile)
  ctx <- if (length(keys)) {
    list(
      by = character(),
      sort = list(vars = character(), descending = logical()),
      merge = list(),
      lineage = list(keys = keys),
      known_identifiers = keys,
      order_contract = NULL,
      collation = NULL
    )
  } else {
    NULL
  }
  alignment <- align_output_rows(ref_eng, cand_eng, context = ctx,
                                 profile = profile, normalized = TRUE)

  compare_datasets_impl(
    al = al, profile = profile, keys = keys,
    pairing = list(
      base_idx = as.integer(alignment$pairs$reference_row),
      comp_idx = as.integer(alignment$pairs$candidate_row),
      rows_only_base = length(alignment$only_reference),
      rows_only_comp = length(alignment$only_candidate),
      dup_fanout = identical(alignment$method, "duplicate_key_multiset"),
      structure_extras = list(
        alignment_method = alignment$method,
        selected_keys = alignment$selected_keys,
        order = alignment$order,
        alignment_resource_state = alignment$resource_state
      )
    )
  )
}

#' Assemble a `sas2r_comparison` from column alignment and a row pairing
#'
#' The one assembly both public comparison paths share; the per-cell verdicts
#' come from `diff_aligned_cells()`, the engine's loop.
#' @noRd
compare_datasets_impl <- function(al, profile, keys, pairing) {
  base <- al$base
  comp <- al$comp

  # Check for unsupported column kinds (R6)
  supported_kinds <- c("numeric", "date", "datetime", "time", "character", "logical")
  base_kinds <- vapply(al$common, function(v) col_kind(base[[v]]), character(1))
  unsupported_vars <- names(base_kinds)[!base_kinds %in% supported_kinds]
  unsupported_kinds <- if (length(unsupported_vars)) {
    tibble::tibble(var = unsupported_vars, kind = base_kinds[unsupported_vars])
  } else {
    tibble::tibble(var = character(), kind = character())
  }

  cell_vars <- setdiff(al$common, c(al$kind_mismatch$var, unsupported_vars))

  cosmetic <- list()

  # attribute comparison on original columns
  for (v in al$common) {
    for (a in profile$attrs_cosmetic) {
      ab <- attr(base[[v]], a, exact = TRUE)
      ac <- attr(comp[[v]], a, exact = TRUE)
      if (!identical(ab, ac)) {
        cosmetic[[length(cosmetic) + 1L]] <-
          tibble::tibble(var = v, kind = paste0("attr:", a), n = 1L)
      }
    }
  }

  # Normalize the compared columns once, keeping the notes the old inline loop
  # recorded. Padding is deliberately left in place here -- unlike the engine's
  # normalize_output_frame() -- so chr_classify() can still report it.
  nb_frame <- base
  nc_frame <- comp
  for (v in cell_vars) {
    nb <- normalize_col(base[[v]])
    nc <- normalize_col(comp[[v]])
    for (note in unique(c(nb$notes, nc$notes))) {
      cosmetic[[length(cosmetic) + 1L]] <-
        tibble::tibble(var = v, kind = paste0("normalize:", note), n = 1L)
    }
    nb_frame[[v]] <- nb$x
    nc_frame[[v]] <- nc$x
  }

  pairs <- tibble::tibble(
    reference_row = as.integer(pairing$base_idx),
    candidate_row = as.integer(pairing$comp_idx)
  )
  cells <- diff_aligned_cells(nb_frame, nc_frame, pairs, profile = profile,
                              max_examples = 0L, details_cap = DETAILS_CAP)

  details <- cells$details
  vars <- cells$vars

  cosmetic <- if (length(cosmetic)) {
    vctrs::vec_rbind(!!!cosmetic, cells$cosmetic)
  } else {
    cells$cosmetic
  }

  # Single-source attribute difference count (N8)
  attr_diffs <- sum(startsWith(cosmetic$kind, "attr:"))

  n_value_mismatch <- sum(vars$n_mismatch)
  structural_bad <- length(al$only_base) + length(al$only_comp) +
    nrow(al$kind_mismatch) + nrow(unsupported_kinds) +
    pairing$rows_only_base + pairing$rows_only_comp
  ok <- n_value_mismatch == 0L && structural_bad == 0L

  summary <- tibble::tibble(
    metric = c("rows_base", "rows_comp", "rows_matched", "rows_only_base",
               "rows_only_comp", "vars_common", "vars_only_base",
               "vars_only_comp", "vars_kind_mismatch", "vars_unsupported",
               "value_mismatch_cells", "cosmetic_cells", "attr_diffs"),
    value = c(nrow(base), nrow(comp), length(pairing$base_idx),
              pairing$rows_only_base, pairing$rows_only_comp,
              length(al$common), length(al$only_base),
              length(al$only_comp), nrow(al$kind_mismatch), nrow(unsupported_kinds),
              n_value_mismatch, sum(cosmetic$n), attr_diffs))

  structure(list(passed = ok, summary = summary, vars = vars,
                 details = details, cosmetic = cosmetic,
                 structure = c(
                   list(
                     common = al$common,
                     only_base = al$only_base,
                     only_comp = al$only_comp,
                     kind_mismatch = al$kind_mismatch,
                     unsupported_kinds = unsupported_kinds,
                     dup_fanout = pairing$dup_fanout,
                     rows_only_base = pairing$rows_only_base,
                     rows_only_comp = pairing$rows_only_comp),
                   pairing$structure_extras),
                 profile = profile, keys = keys),
            class = "sas2r_comparison")
}


#' Did a comparison pass within tolerance?
#'
#' @param x An object of class `sas2r_comparison`.
#' @return Logical indicating whether the comparison passed.
#' @examples
#' sas <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.0))
#' r <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.0))
#' passed(compare_datasets(sas, r, keys = "usubjid"))
#' @export
passed <- function(x) {
  stopifnot(inherits(x, "sas2r_comparison"))
  x$passed
}

#' @export
print.sas2r_comparison <- function(x, ...) {
  cli::cli_h1("sas2r dataset comparison")
  if (x$passed) cli::cli_alert_success("PASSED within tolerance (profile v{x$profile$version})")
  else cli::cli_alert_danger("FAILED (profile v{x$profile$version})")
  for (k in seq_len(nrow(x$summary))) {
    cli::cli_text("{.field {x$summary$metric[k]}}: {x$summary$value[k]}")
  }

  # Name structural offenders if any (N6)
  st <- x$structure
  has_struct <- length(st$only_base) || length(st$only_comp) ||
    nrow(st$kind_mismatch) || (!is.null(st$unsupported_kinds) && nrow(st$unsupported_kinds))
  if (has_struct) {
    cli::cli_h2("structural differences")
    if (length(st$only_base)) {
      cli::cli_alert_warning("columns only in base: {toString(st$only_base)}")
    }
    if (length(st$only_comp)) {
      cli::cli_alert_warning("columns only in compare: {toString(st$only_comp)}")
    }
    if (nrow(st$kind_mismatch)) {
      for (i in seq_len(nrow(st$kind_mismatch))) {
        cli::cli_alert_warning("type mismatch: {st$kind_mismatch$var[i]} (base: {st$kind_mismatch$base_kind[i]}, comp: {st$kind_mismatch$comp_kind[i]})")
      }
    }
    if (!is.null(st$unsupported_kinds) && nrow(st$unsupported_kinds)) {
      for (i in seq_len(nrow(st$unsupported_kinds))) {
        cli::cli_alert_warning("unsupported kind: {st$unsupported_kinds$var[i]} ({st$unsupported_kinds$kind[i]})")
      }
    }
  }

  bad <- x$vars[x$vars$n_mismatch > 0L, ]
  if (nrow(bad)) {
    cli::cli_h2("variables with mismatches")
    for (k in seq_len(nrow(bad))) {
      diff_info <- if (!is.na(bad$max_abs_diff[k])) {
        sprintf(" (max abs diff %s)", bad$max_abs_diff[k])
      } else ""
      cli::cli_alert_warning(
        "{bad$var[k]}: {bad$n_mismatch[k]} mismatch{?es}{diff_info}")
    }
  }
  invisible(x)
}


