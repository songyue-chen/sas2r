#' Draw a plain PDF listing with aligned columns
#'
#' Writes a plain table using shared column positions, fixed line spacing and
#' repeated headers. This is a drawing helper, not a complete PROC PRINT or ODS
#' implementation: the caller must select/order rows and columns, apply required
#' formats, and implement source titles, BY groups, totals and other report logic.
#'
#' @param data A data frame in the source-required row and column order.
#' @param file Explicit destination path ending in `.pdf` (case-insensitive).
#'   Non-PDF paths are rejected before opening a device or writing a file.
#'   Its parent directory must exist;
#'   generated programs can obtain it from [sas_output_path()].
#' @param labels Character column headings, one per data column.
#' @param obs Include a leading `Obs` column with sequential observation numbers.
#'   Use `FALSE` for source NOOBS or when supplying a source ID column instead.
#' @param pointsize Positive font size in points.
#' @param rows_per_page Positive integer maximum number of body text lines per
#'   page. Multiline cells use multiple lines; headers repeat on every page.
#' @return The destination path, invisibly.
#' @details
#' Values use [sas_display()] unless already formatted by the caller. Columns
#' are left-aligned in a monospace font. The PDF page widens to fit the longest
#' heading or value in each column; text is not truncated or shrunk to fit.
#' Newlines in cells are retained. This helper does not reproduce SAS page size,
#' style or arbitrary ODS layouts. Choose another renderer when those effects
#' are required. PDF validity alone does not establish output equivalence.
#' This helper writes PDF only. For SAS ODS RTF output, use an RTF-capable
#' renderer and retain the `.rtf` destination; changing the filename does not
#' convert the document format.
#' @family runtime helpers
#' @examples
#' local({
#'   path <- tempfile(fileext = ".pdf")
#'   on.exit(unlink(path))
#'   sas_print_table(data.frame(subject = c("01", "02"), value = c(12.5, NA)), path)
#' })
#' @export
sas_print_table <- function(data, file, labels = names(data), obs = TRUE,
                            pointsize = 10, rows_per_page = 40L) {
  if (!is.data.frame(data) || !ncol(data)) {
    stop("sas_print_table: data must be a data frame with at least one column.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("sas_print_table: file must be an explicit PDF destination.", call. = FALSE)
  }
  if (!endsWith(tolower(file), ".pdf")) {
    stop(paste0("sas_print_table: file must end in .pdf (case-insensitive); ",
                "this helper writes PDF only. Use an RTF-capable renderer for RTF output."),
         call. = FALSE)
  }
  if (!is.character(labels) || length(labels) != ncol(data) || anyNA(labels)) {
    stop("sas_print_table: labels must contain one heading per column.", call. = FALSE)
  }
  if (!is.logical(obs) || length(obs) != 1L || is.na(obs)) {
    stop("sas_print_table: obs must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(pointsize) || length(pointsize) != 1L ||
      !is.finite(pointsize) || pointsize <= 0) {
    stop("sas_print_table: pointsize must be positive and finite.", call. = FALSE)
  }
  if (!is.numeric(rows_per_page) || length(rows_per_page) != 1L ||
      !is.finite(rows_per_page) || rows_per_page < 1 || rows_per_page != floor(rows_per_page)) {
    stop("sas_print_table: rows_per_page must be a positive integer.", call. = FALSE)
  }
  columns <- lapply(data, sas_display)
  if (isTRUE(obs)) {
    columns <- c(list(as.character(seq_len(nrow(data)))), columns)
    labels <- c("Obs", labels)
  }
  # Split every cell before measuring so a multiline cell cannot overlap the
  # following record. Empty cells still occupy one line.
  split_cell <- function(x) {
    lines <- strsplit(paste0(x, "\n"), "\n", fixed = TRUE)[[1L]]
    if (length(lines)) lines else ""
  }
  cells <- lapply(seq_along(columns), function(j) {
    lapply(c(labels[[j]], columns[[j]]), split_cell)
  })
  heights <- vapply(seq_len(nrow(data) + 1L), function(i) {
    max(vapply(cells, function(col) length(col[[i]]), integer(1)))
  }, integer(1))
  lines <- do.call(cbind, lapply(cells, function(col) {
    unlist(lapply(seq_along(col), function(i) {
      c(col[[i]], rep("", heights[[i]] - length(col[[i]])))
    }), use.names = FALSE)
  }))
  header <- lines[seq_len(heights[[1L]]), , drop = FALSE]
  body <- lines[-seq_len(heights[[1L]]), , drop = FALSE]

  # Measure on a null PDF device with the same font as the destination. Never
  # implicitly open Rplots.pdf or alter the caller's graphics parameters.
  grDevices::pdf(NULL, pointsize = pointsize, family = "mono")
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  widths <- apply(lines, 2L, function(x) max(graphics::strwidth(x, units = "inches")))
  gap <- graphics::strwidth("  ", units = "inches")
  grDevices::dev.off(device)
  margin <- 0.5
  line_height <- pointsize * 1.4 / 72
  width <- max(7, 2 * margin + sum(widths) + gap * (ncol(lines) - 1L))
  height <- max(3, 2 * margin + (nrow(header) + rows_per_page) * line_height)
  x <- margin + c(0, utils::head(cumsum(widths + gap), -1L))
  grDevices::pdf(file, width = width, height = height, pointsize = pointsize, family = "mono")
  device <- grDevices::dev.cur()
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  pages <- max(1L, ceiling(nrow(body) / rows_per_page))
  for (page in seq_len(pages)) {
    first <- (page - 1L) * rows_per_page + 1L
    last <- min(page * rows_per_page, nrow(body))
    content <- if (first <= last) rbind(header, body[first:last, , drop = FALSE]) else header
    graphics::plot.new()
    graphics::plot.window(c(0, width), c(0, height))
    y <- height - margin - (seq_len(nrow(content)) - 0.5) * line_height
    for (j in seq_len(ncol(content))) {
      graphics::text(x[[j]], y, labels = content[, j], adj = c(0, 0.5))
    }
  }
  invisible(file)
}
