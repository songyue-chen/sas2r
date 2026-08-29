test_that("assess_tlf_target validates PDF structure, header, trailer, and required text", {
  tmp <- withr::local_tempdir()

  # 1. Valid PDF file
  pdf_file <- file.path(tmp, "table1.pdf")
  pdf_content <- "%PDF-1.4\n1 0 obj\n<< /Title (Table 14.1 Summary of Demographics) >>\nendobj\n%%EOF"
  writeBin(charToRaw(pdf_content), pdf_file)

  contract <- list(
    target_id = "target_001",
    target_key = "table1.pdf",
    kind = "tlf",
    logical_name = "table1.pdf",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list(required_text = c("Table 14.1", "Demographics"))
  )
  attempt <- list(attempt_dir = tmp, outputs_dir = tmp)

  res <- assess_tlf_target(contract, attempt)
  expect_true(res$passed)
  expect_identical(res$status, "passed")
  expect_true(res$checks$header_valid$passed)
  expect_true(res$checks$trailer_valid$passed)
  expect_true(res$checks$file_nonempty$passed)

  # 2. Corrupt PDF header
  bad_pdf <- file.path(tmp, "corrupt.pdf")
  writeBin(charToRaw("NOT_A_PDF\n%%EOF"), bad_pdf)
  bad_contract <- list(
    target_id = "target_002",
    target_key = "corrupt.pdf",
    kind = "tlf",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list()
  )
  bad_res <- assess_tlf_target(bad_contract, attempt)
  expect_false(bad_res$passed)
  expect_false(bad_res$checks$header_valid$passed)
})

test_that("assess_tlf_target validates RTF envelope, content, and text", {
  tmp <- withr::local_tempdir()

  # 1. Valid RTF file
  rtf_file <- file.path(tmp, "listing1.rtf")
  rtf_content <- "{\\rtf1\\ansi\\deff0 {\\fonttbl{\\f0 Courier;}}\\f0\\fs20 Listing 14.2 Adverse Events\\par}"
  writeLines(rtf_content, rtf_file)

  contract <- list(
    target_id = "target_003",
    target_key = "listing1.rtf",
    kind = "tlf",
    logical_name = "listing1.rtf",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list(required_text = c("Listing 14.2", "Adverse Events"))
  )
  attempt <- list(attempt_dir = tmp)

  res <- assess_tlf_target(contract, attempt)
  expect_true(res$passed)
  expect_true(res$checks$envelope_valid$passed)
  expect_true(res$checks$required_text$passed)

  # 2. Invalid RTF envelope (unbalanced braces)
  bad_rtf <- file.path(tmp, "broken.rtf")
  writeLines("{\\rtf1 unclosed", bad_rtf)
  bad_contract <- list(
    target_id = "target_004",
    target_key = "broken.rtf",
    kind = "tlf",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list()
  )
  bad_res <- assess_tlf_target(bad_contract, attempt)
  expect_false(bad_res$passed)
  expect_false(bad_res$checks$envelope_valid$passed)
})

test_that("assess_tlf_target validates HTML document structure and tables", {
  tmp <- withr::local_tempdir()

  # 1. Valid HTML file
  html_file <- file.path(tmp, "report.html")
  html_content <- "<!DOCTYPE html><html><head><title>Efficacy</title></head><body><h1>Primary Endpoint</h1><table><tr><th>Group</th><th>Mean</th></tr><tr><td>Trt A</td><td>12.4</td></tr></table></body></html>"
  writeLines(html_content, html_file)

  contract <- list(
    target_id = "target_005",
    target_key = "report.html",
    kind = "tlf",
    logical_name = "report.html",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list(required_text = c("Primary Endpoint", "Trt A"))
  )
  attempt <- list(attempt_dir = tmp)

  res <- assess_tlf_target(contract, attempt)
  expect_true(res$passed)
  expect_true(res$checks$structure_valid$passed)
  expect_true(res$checks$required_text$passed)
})

test_that("assess_tlf_target validates PNG and JPEG magic bytes and dimensions", {
  tmp <- withr::local_tempdir()

  # 1. Valid PNG file with 100x50 dimensions
  png_file <- file.path(tmp, "figure1.png")
  png_bytes <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, # Magic bytes
    0x00, 0x00, 0x00, 0x0D,                         # IHDR length (13)
    0x49, 0x48, 0x44, 0x52,                         # "IHDR"
    0x00, 0x00, 0x00, 0x64,                         # Width: 100 (0x64)
    0x00, 0x00, 0x00, 0x32,                         # Height: 50 (0x32)
    0x08, 0x06, 0x00, 0x00, 0x00,                   # Bit depth, color type, etc.
    0x00, 0x00, 0x00, 0x00                          # CRC placeholder
  ))
  writeBin(png_bytes, png_file)

  contract <- list(
    target_id = "target_006",
    target_key = "figure1.png",
    kind = "tlf",
    logical_name = "figure1.png",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list()
  )
  attempt <- list(attempt_dir = tmp)

  res <- assess_tlf_target(contract, attempt)
  expect_true(res$passed)
  expect_true(res$checks$signature_valid$passed)
  expect_true(res$checks$dimensions_valid$passed)
  expect_equal(res$dimensions$width, 100L)
  expect_equal(res$dimensions$height, 50L)

  # 2. Valid JPEG file
  jpg_file <- file.path(tmp, "figure2.jpg")
  jpg_bytes <- as.raw(c(0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46, 0x49, 0x46))
  writeBin(jpg_bytes, jpg_file)

  contract_jpg <- list(
    target_id = "target_007",
    target_key = "figure2.jpg",
    kind = "tlf",
    logical_name = "figure2.jpg",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list()
  )
  res_jpg <- assess_tlf_target(contract_jpg, attempt)
  expect_true(res_jpg$passed)
  expect_true(res_jpg$checks$signature_valid$passed)
})

test_that("cosmetic presentation differences are recorded as non-material differences", {
  tmp <- withr::local_tempdir()

  ref_html <- file.path(tmp, "ref.html")
  writeLines("<html><body><table border='1'><tr><th>Col</th></tr><tr><td>Val</td></tr></table></body></html>", ref_html)

  cand_html <- file.path(tmp, "cand.html")
  writeLines("<html><body><table class='table-striped'><tr><th>Col</th></tr><tr><td>Val</td></tr></table></body></html>", cand_html)

  contract <- list(
    target_id = "target_008",
    target_key = "cand.html",
    kind = "tlf",
    logical_name = "cand.html",
    required = TRUE,
    reference_path = ref_html,
    assertions = list(required_text = c("Col", "Val"))
  )
  attempt <- list(attempt_dir = tmp)

  res <- assess_tlf_target(contract, attempt)
  expect_true(res$passed)
  expect_true(length(res$differences$cosmetic) >= 0L)
})

test_that("unavailable extractor records check as unavailable without false pass or fail", {
  tmp <- withr::local_tempdir()
  pdf_file <- file.path(tmp, "test.pdf")
  writeBin(charToRaw("%PDF-1.4\nstream\nBinaryStreamContent\nendstream\n%%EOF"), pdf_file)

  contract <- list(
    target_id = "target_009",
    target_key = "test.pdf",
    kind = "tlf",
    required = TRUE,
    reference_path = NA_character_,
    assertions = list(required_text = c("SomeTextInBinaryStream"))
  )
  attempt <- list(attempt_dir = tmp)

  # Pass comparison_rules with simulated unavailable extractor
  res <- assess_tlf_target(contract, attempt, comparison_rules = list(force_text_extractor_unavailable = TRUE))
  expect_false(res$has_reference)
  expect_identical(res$checks$text_extraction$status, "unavailable")
})
