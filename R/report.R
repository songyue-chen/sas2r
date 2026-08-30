md_table <- function(df) {
  if (!nrow(df)) return("(none)")
  clean_cell <- function(col) {
    s <- as.character(col)
    s[is.na(s)] <- "NA"
    s <- gsub("\\|", "\\\\|", s)
    s <- gsub("[\r\n]+", " ", s)
    s
  }
  clean_df <- as.data.frame(lapply(df, clean_cell), stringsAsFactors = FALSE)
  hdr <- paste0("| ", paste(names(clean_df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(clean_df)), collapse = "|"), "|")
  rows <- apply(clean_df, 1L, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(hdr, sep, rows), collapse = "\n")
}

#' Write a PROC-COMPARE-style markdown report or versioned JSON comparison report
#'
#' @param x An object of class `sas2r_comparison` or `sas2r_comparison_report`.
#' @param file Path to the output markdown or JSON file.
#' @param base_label Label for the base dataset.
#' @param comp_label Label for the compare dataset.
#' @param run_id Run ID for output review directory structure.
#' @param dir Base directory for `.sas2r` output review files.
#' @param ... Additional options.
#' @return Path to the written file, invisibly.
#' @examples
#' sas <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.0))
#' r <- data.frame(usubjid = c("01-001", "01-002"), aval = c(1.0, 2.5))
#' cmp <- compare_datasets(sas, r, keys = "usubjid")
#'
#' # PROC COMPARE-style markdown.
#' md <- file.path(tempdir(), "sas2r-comparison.md")
#' write_comparison_report(cmp, file = md)
#' cat(head(readLines(md), 6), sep = "\n")
#' @export
write_comparison_report <- function(x, file = NULL, base_label = "SAS (base)",
                                    comp_label = "R (compare)",
                                    run_id = "default", dir = NULL, ...) {
  if (inherits(x, "sas2r_comparison_report")) {
    validate_comparison_report(x)
    if (is.null(file)) {
      root_dir <- dir %||% ".sas2r"
      out_dir <- file.path(root_dir, "output-review", run_id)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      if (.Platform$OS.type == "unix") {
        tryCatch(Sys.chmod(out_dir, mode = "0700"), error = function(e) NULL)
      }
      file <- file.path(out_dir, paste0(x$report_id, ".json"))
    } else {
      out_dir <- dirname(file)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        if (.Platform$OS.type == "unix") {
          tryCatch(Sys.chmod(out_dir, mode = "0700"), error = function(e) NULL)
        }
      }
    }

    json_str <- jsonlite::toJSON(unclass(x), auto_unbox = TRUE, pretty = TRUE, dataframe = "rows")
    tmp_file <- tempfile(pattern = "comp_report_", tmpdir = dirname(file))
    writeLines(as.character(json_str), tmp_file)
    if (.Platform$OS.type == "unix") {
      tryCatch(Sys.chmod(tmp_file, mode = "0600"), error = function(e) NULL)
    }
    file.rename(tmp_file, file)
    return(invisible(file))
  }

  if (!inherits(x, "sas2r_comparison")) {
    cli::cli_abort("Expected an object of class {.cls sas2r_comparison} or {.cls sas2r_comparison_report}.",
                   class = "sas2r_invalid_report_object")
  }
  if (is.null(file)) {
    cli::cli_abort("File path must be specified for sas2r_comparison markdown report.",
                   class = "sas2r_invalid_report_object")
  }
  verdict <- if (x$passed) "**PASSED within tolerance**" else "**FAILED**"
  cap <- attr(x$details, "details_cap")
  if (is.null(cap)) cap <- DETAILS_CAP
  details_heading <- sprintf("## Differing cells (first %d)", cap)

  structure_lines <- c("## Structure", "")
  st <- x$structure
  has_structure_diffs <- FALSE
  if (length(st$only_base)) {
    has_structure_diffs <- TRUE
    structure_lines <- c(structure_lines, "### Columns only in base", "",
                         paste0("- ", st$only_base), "")
  }
  if (length(st$only_comp)) {
    has_structure_diffs <- TRUE
    structure_lines <- c(structure_lines, "### Columns only in compare", "",
                         paste0("- ", st$only_comp), "")
  }
  if (nrow(st$kind_mismatch)) {
    has_structure_diffs <- TRUE
    structure_lines <- c(structure_lines, "### Column type mismatches", "",
                         md_table(st$kind_mismatch), "")
  }
  if (!is.null(st$unsupported_kinds) && nrow(st$unsupported_kinds)) {
    has_structure_diffs <- TRUE
    structure_lines <- c(structure_lines, "### Unsupported column kinds", "",
                         md_table(st$unsupported_kinds), "")
  }
  if (!has_structure_diffs) {
    structure_lines <- c(structure_lines, "(none)", "")
  }

  lines <- c(
    "# Dataset comparison", "",
    sprintf("- base: %s", base_label),
    sprintf("- compare: %s", comp_label),
    sprintf("- profile version: %s", x$profile$version),
    sprintf("- verdict: %s", verdict), "",
    "## Summary", "", md_table(x$summary), "",
    structure_lines,
    "## Variables", "", md_table(x$vars), "",
    "## Cosmetic differences (tolerated and reported)", "",
    md_table(x$cosmetic), "",
    details_heading, "", md_table(x$details), ""
  )
  if (isTRUE(attr(x$details, "details_truncated")))
    lines <- c(lines, sprintf("_details truncated at %d rows_", cap), "")
  writeLines(lines, file)
  invisible(file)
}


