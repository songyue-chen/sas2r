# Tutorial expectations derived from the corrected SAS and synthetic input.
# These are NOT independent SAS-generated reference results.
verify_demo_output <- function(result, project_dir) {
  if (!identical(result$status, "migration_ready")) {
    stop("Run is not migration_ready. Inspect START_HERE.html before accepting output.")
  }
  if (is.null(result$outputs_dir)) stop("No executed output directory is available.")
  dataset <- file.path(result$outputs_dir, "datasets", "adam", "final_ds.rds")
  pdf <- file.path(result$outputs_dir, "tlf", "table1.pdf")
  stopifnot(file.exists(dataset), file.exists(pdf))
  actual <- readRDS(dataset)
  stopifnot(identical(tolower(names(actual)), c("aval_flag", "usubjid", "avisitn", "aval", "trtp", "high_flag")))
  expected <- read.csv(file.path(project_dir, "example", "expected-final.csv"),
                       colClasses = c("character", "integer", "numeric", "character", "character", "integer"),
                       na.strings = "", check.names = FALSE)
  # SAS names are case-insensitive; retain the actual output's original names.
  position <- match(tolower(names(expected)), tolower(names(actual)))
  stopifnot(nrow(actual) == nrow(expected), !anyNA(position),
            ncol(actual) == ncol(expected))
  for (i in seq_along(position)) {
    x <- actual[[position[i]]]
    y <- expected[[i]]
    # SAS character values compare equal after trailing blank padding is removed.
    # Preserve leading/internal spaces and the actual generated output.
    if (is.character(x) && is.character(y)) {
      x <- sub(" +$", "", x)
      y <- sub(" +$", "", y)
    }
    if (!isTRUE(all.equal(as.vector(x), as.vector(y), check.attributes = FALSE))) {
      stop("Output mismatch in ", names(expected)[i], ". Inspect the generated code and report.")
    }
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Dataset checks passed. Install pdftools, then rerun verification for PDF content.")
  }
  text <- paste(pdftools::pdf_text(pdf), collapse = "\n")
  required <- c(names(expected), "MISSING", "RECORDED", "12.5", "14.2", "9.8")
  missing <- required[!vapply(required, function(x) grepl(tolower(x), tolower(text), fixed = TRUE), logical(1))]
  if (length(missing)) stop("PDF content missing: ", paste(missing, collapse = ", "))
  verify_demo_pdf_layout(pdf, actual)
  message("PASS: 5 rows, values, missingness, flags, row and column order, PDF text and demo table layout. These are tutorial checks, not SAS-reference equivalence.")
  invisible(actual)
}

# A deliberately narrow check for this five-row, one-page plain listing.
# It is not a general PDF validator or a SAS styling-equivalence comparison.
verify_demo_pdf_layout <- function(pdf, actual) {
  pages <- pdftools::pdf_data(pdf)
  if (length(pages) != 1L) stop("Demo PDF must contain one complete table page; inspect pagination.")
  p <- pages[[1L]]
  headings <- c("Obs", names(actual))
  positions <- match(tolower(headings), tolower(p$text))
  if (anyNA(positions)) stop("PDF table headings are incomplete.")
  h <- p[positions, ]
  if (diff(range(h$y)) > 3 || any(diff(h$x) <= 0)) {
    stop("PDF table headings are not in source column order on one line.")
  }
  size <- pdftools::pdf_pagesize(pdf)
  if (any(p$x < 0 | p$x + p$width > size$width[[1]] |
          p$y < 0 | p$y + p$height > size$height[[1]])) {
    stop("PDF table text extends outside the page.")
  }
  body <- p[p$y > max(h$y) + 3, ]
  ys <- sort(unique(body$y))
  # PDF extractors may round glyph baselines by a point; group those together.
  row_y <- ys[c(TRUE, diff(ys) > 3)]
  gaps <- diff(c(min(h$y), row_y))
  if (length(row_y) != nrow(actual) || any(gaps < 8 | gaps > 30)) {
    stop("PDF table has missing/overlapping rows or excessive spacing. Inspect the rendered page.")
  }
  expected <- cbind(Obs = as.character(seq_len(nrow(actual))),
    as.data.frame(lapply(actual, function(x) {
      if (is.numeric(x)) ifelse(is.na(x), ".", format(x, trim = TRUE, scientific = FALSE))
      else sub(" +$", "", as.character(x))
    }), stringsAsFactors = FALSE))
  for (i in seq_len(nrow(actual))) {
    row <- body[abs(body$y - row_y[[i]]) <= 3, ]
    for (j in seq_along(headings)) {
      right <- if (j < nrow(h)) h$x[[j + 1L]] - 3 else size$width[[1]]
      cell <- row[row$x >= h$x[[j]] - 3 & row$x + row$width <= right, ]
      cell <- cell[order(cell$x), ]
      text <- paste(cell$text, collapse = " ")
      value <- expected[[j]][[i]]
      # Numeric renderers may choose 15 or 15.0; compare numeric display values.
      numeric_cell <- j == 1L || is.numeric(actual[[j - 1L]])
      ok <- if (numeric_cell && value != ".") {
        isTRUE(suppressWarnings(as.numeric(text) == as.numeric(value)))
      } else identical(text, value)
      if (!ok) stop("PDF header/value alignment or content mismatch: row ", i,
                    ", column ", headings[[j]], ". Inspect the generated report code.")
    }
  }
  invisible(TRUE)
}
