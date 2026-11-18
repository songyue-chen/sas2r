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
