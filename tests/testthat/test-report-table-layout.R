table_pdf_tokens <- function(path) {
  skip_if_not_installed("pdftools")
  pdftools::pdf_data(path)
}

test_that("plain listings align unequal headers and values with compact rows", {
  path <- withr::local_tempfile(fileext = ".pdf")
  # Synthetic PROC PRINT input: short identifiers under long column names,
  # long values under short names, and missing values must stay associated.
  data <- data.frame(subject_identifier = c("A1", "B234"),
                     x = c("long_value", "q"), aval = c(12.5, NA))
  original <- data
  result <- sas_print_table(data, path)
  expect_identical(data, original)
  expect_identical(result, path)
  pages <- table_pdf_tokens(path)
  expect_length(pages, 1L)
  p <- pages[[1L]]
  xpos <- function(text) p$x[match(text, p$text)]
  expect_equal(xpos("subject_identifier"), xpos("A1"), tolerance = 1)
  expect_equal(xpos("subject_identifier"), xpos("B234"), tolerance = 1)
  expect_equal(xpos("x"), xpos("long_value"), tolerance = 1)
  expect_equal(xpos("aval"), xpos("12.5"), tolerance = 1)
  expect_true(all(c("Obs", "1", "2", ".") %in% p$text))
  y <- sort(unique(p$y))
  expect_length(y, 3L)
  expect_true(all(diff(y) >= 10 & diff(y) <= 18))
  # No column's text reaches the following column.
  for (row in split(p, p$y)) {
    row <- row[order(row$x), ]
    expect_true(all(head(row$x + row$width, -1) < tail(row$x, -1)))
  }
})

test_that("pagination repeats headers and retains every observation", {
  path <- withr::local_tempfile(fileext = ".pdf")
  data <- data.frame(id = sprintf("SUB%02d", 1:7), value = 101:107)
  sas_print_table(data, path, rows_per_page = 3)
  pages <- table_pdf_tokens(path)
  expect_length(pages, 3L)
  for (page in pages) expect_true(all(c("Obs", "id", "value") %in% page$text))
  ids <- unlist(lapply(pages, function(p) p$text[grepl("^SUB", p$text)]))
  expect_identical(ids, data$id)
  expect_true("7" %in% pages[[3]]$text)
})

test_that("wide and multiline cells remain visible without overlapping rows", {
  path <- withr::local_tempfile(fileext = ".pdf")
  value <- paste(rep("W", 120), collapse = "")
  sas_print_table(data.frame(id = c("first\ncontinued", "last"), text = c(value, "end")),
                  path, obs = FALSE, labels = c("Subject", "Description"))
  pages <- table_pdf_tokens(path)
  p <- pages[[1]]
  expect_true(all(c(value, "first", "continued", "last", "end") %in% p$text))
  expect_false("Obs" %in% p$text)
  expect_true(p$y[match("continued", p$text)] < p$y[match("last", p$text)])
  size <- pdftools::pdf_pagesize(path)
  expect_true(all(p$x >= 0 & p$x + p$width < size$width[[1]]))
  expect_true(all(p$y >= 0 & p$y + p$height < size$height[[1]]))
})

test_that("empty listings retain headers and rendering restores the caller's device", {
  path <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(NULL)
  device <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(device), add = TRUE)
  before <- graphics::par(c("mar", "family", "cex"))
  sas_print_table(data.frame(id = character(), value = double()), path)
  expect_identical(grDevices::dev.cur(), device)
  expect_identical(graphics::par(c("mar", "family", "cex")), before)
  p <- table_pdf_tokens(path)[[1L]]
  expect_setequal(p$text, c("Obs", "id", "value"))
  expect_error(sas_print_table(data.frame(x = 1), path, labels = character()), "labels")
  expect_error(sas_print_table(data.frame(x = 1), path, rows_per_page = 0), "rows_per_page")
  expect_error(sas_print_table(data.frame(x = 1), path, pointsize = NA_real_), "pointsize")
  expect_identical(grDevices::dev.cur(), device)
})

test_that("exported runtime renders listings without loading sas2r", {
  path <- withr::local_tempfile(fileext = ".pdf")
  runtime <- new.env(parent = baseenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), runtime)
  runtime$sas_print_table(data.frame(long_header = "x", n = "long_value"), path)
  p <- table_pdf_tokens(path)[[1L]]
  expect_equal(p$x[match("long_header", p$text)], p$x[match("x", p$text)], tolerance = 1)
  expect_true("long_value" %in% p$text)
})

test_that("PDF listings reject other destinations before creating or overwriting files", {
  root <- withr::local_tempdir()
  runtime <- new.env(parent = baseenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), runtime)
  for (render in list(sas_print_table, runtime$sas_print_table)) {
    before <- grDevices::dev.list()
    for (name in c("table.rtf", "table.RTF", "table.html", "table.docx", "table.xlsx", "table", "table.pdf.rtf")) {
      path <- file.path(root, name)
      expect_error(render(data.frame(x = 1), path), "PDF only")
      expect_false(file.exists(path), info = name)
      # A mistaken render must not truncate an existing report, either.
      original <- charToRaw("{\\rtf1 Existing report}")
      writeBin(original, path)
      expect_error(render(data.frame(x = 1), path), "PDF only")
      expect_identical(readBin(path, "raw", n = 100L), original)
      unlink(path)
    }
    expect_identical(grDevices::dev.list(), before)
    for (name in c("table.pdf", "table.PDF", "table.PdF")) {
      path <- file.path(root, name)
      expect_identical(render(data.frame(x = 1), path), path)
      expect_identical(rawToChar(readBin(path, "raw", n = 5L)), "%PDF-")
    }
  }
})

test_that("PDF listings preserve supported text and reject unrenderable headings or cells", {
  root <- withr::local_tempdir()
  path <- file.path(root, "listing.pdf")
  before <- grDevices::dev.list()
  for (where in c("heading", "cell")) {
    for (text in c("\u0394", "\u60a3\u8005", "\u2265", "soft\u00adhyphen")) {
      data <- data.frame(x = if (where == "cell") text else "value")
      labels <- if (where == "heading") text else "heading"
      expect_error(sas_print_table(data, path, labels), "encoding")
      expect_false(file.exists(path))
      writeBin(charToRaw("previous report"), path)
      expect_error(sas_print_table(data, path, labels), "encoding")
      expect_identical(readBin(path, "raw", 32L), charToRaw("previous report"))
      unlink(path)
    }
  }
  expect_identical(grDevices::dev.list(), before)
  expect_no_warning(sas_print_table(data.frame(x = "caf\u00e9"), path, labels = "co\u00fbt"))
  expect_true(all(c("caf\u00e9", "co\u00fbt") %in% table_pdf_tokens(path)[[1L]]$text))
})

test_that("headings and cells share normalized whitespace and layout", {
  root <- withr::local_tempdir()
  actual <- file.path(root, "actual.pdf")
  expected <- file.path(root, "expected.pdf")
  runtime <- new.env(parent = baseenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), runtime)
  for (render in list(sas_print_table, runtime$sas_print_table)) {
    listing <- function(separator, path) {
      render(data.frame(x = paste0("first", separator, "last"), y = "end"), path,
        labels = c(paste0("left", separator, "right"), "next"))
    }
    for (separator in c("\t", "\r\n", "\r")) {
      listing(if (separator == "\t") " " else "\n", expected)
      listing(separator, actual)
      tokens <- table_pdf_tokens(actual)
      expect_equal(tokens, table_pdf_tokens(expected))
      expect_true(all(c("left", "right", "first", "last", "end") %in% tokens[[1L]]$text))
    }
    writeLines("original", actual)
    expect_error(listing("\001", actual), "control character")
    expect_identical(readLines(actual), "original")
  }
})
