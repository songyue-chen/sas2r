# The scanner follows %str()/%nrstr() arguments and two instream-data ending
# rules. These tests hold it to the guard rail those rules came with: a source
# containing none of the constructs is classified exactly as before, and the
# fallback is the previous classification, character for character.
# helper-legacy-scan.R is the frozen reference; helper-scan-corpus.R builds
# synthetic sources.

legacy_pass_mask <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  sas_scan_pass(chars, grepl("[[:space:]]", chars), legacy = TRUE)$mask
}

test_that("packaged and test fixture sources are classified exactly as before", {
  roots <- c(test_path("fixtures"), system.file("examples", package = "sas2r"))
  files <- unique(unlist(lapply(roots[nzchar(roots) & dir.exists(roots)], list.files,
    pattern = "\\.sas$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)))
  expect_gt(length(files), 10L)
  for (file in files) {
    text <- paste(read_sas_source(file), collapse = "\n")
    expect_identical(sas_scan(text), legacy_sas_scan(text), info = basename(file))
  }
})

test_that("sources without %str() or instream data are classified exactly as before", {
  withr::local_seed(20261124)
  corpus <- scan_corpus_plain(600L)
  expect_false(any(grepl("%(nr)?str|datalines|cards", corpus, ignore.case = TRUE)))
  same <- vapply(corpus, function(text) identical(sas_scan(text), legacy_sas_scan(text)), logical(1))
  expect_identical(sum(!same), 0L, info = if (any(!same)) deparse(corpus[which(!same)[1L]]))
})

test_that("conventionally ended instream data is classified exactly as before", {
  withr::local_seed(20261125)
  corpus <- scan_corpus_instream(400L)
  # The corpus does exercise both block forms, with semicolons inside "4" data.
  expect_true(any(grepl("datalines;|cards;", corpus, ignore.case = TRUE)))
  expect_true(any(grepl("(datalines4|cards4);\n[^\n]*;", corpus, ignore.case = TRUE)))
  same <- vapply(corpus, function(text) identical(sas_scan(text), legacy_sas_scan(text)), logical(1))
  expect_identical(sum(!same), 0L, info = if (any(!same)) deparse(corpus[which(!same)[1L]]))
})

test_that("the fallback pass is the previous classification on any source", {
  withr::local_seed(20261126)
  corpus <- c(scan_corpus_triggers(600L), scan_corpus_plain(100L), scan_corpus_instream(100L))
  same <- vapply(corpus, function(text) identical(legacy_pass_mask(text), legacy_sas_scan(text)$mask), logical(1))
  expect_identical(sum(!same), 0L, info = if (any(!same)) deparse(corpus[which(!same)[1L]]))
})

test_that("the newer rules keep every character and only retag semicolons", {
  withr::local_seed(20261127)
  corpus <- scan_corpus_triggers(600L)
  changed <- 0L
  for (text in corpus) {
    scan <- sas_scan(text)
    expect_identical(paste(scan$chars, collapse = ""), text)
    expect_true(all(scan$mask %in% c("c", "s", "q", "k", "d", "m")))
    expect_true(all(scan$chars[scan$mask == "m"] == ";"))
    if (!identical(scan$mask, legacy_sas_scan(text)$mask)) changed <- changed + 1L
    # Every statement's text is drawn from the source, in order.
    statements <- tryCatch(sas_source_records(text)$statements, sas2r_sas_parse_error = function(e) NULL)
    if (!is.null(statements) && nrow(statements)) {
      expect_false(is.unsorted(statements$char_start))
      expect_true(all(statements$char_end <= nchar(text)))
    }
  }
  # The corpus is not vacuous: the newer rules do apply to a good share of it.
  expect_gt(changed, 100L)
})
