qc_fixture_data <- function() {
  x <- data.frame(USUBJID = c("01", "02"), AGE = c(50, 60),
                  ADT = as.Date(c("2020-01-01", "2020-01-02")))
  attr(x$AGE, "label") <- "Age in years"
  attr(x$ADT, "format.sas") <- "DATE9."
  x
}

qc_fixture_profile <- function() qc_profile(
  required_columns = c("USUBJID", "AGE", "ADT"),
  labels = c(AGE = "Age in years"), formats = c(ADT = "DATE9."),
  types = c(USUBJID = "character", AGE = "numeric", ADT = "Date"),
  column_order = c("USUBJID", "AGE", "ADT"), keys = "USUBJID",
  unique_keys = TRUE, row_count = 2, min_rows = 1, max_rows = 3,
  numeric_tolerance = 0, tolerances = list(AGE = list(abs = 0.01, rel = 0))
)

test_that("named QC profiles pass and detect independently seeded defects", {
  original <- qc_fixture_data()
  assertions <- qc_fixture_profile()
  check <- function(x) check_dataset_qc(x, assertions)
  expect_true(all(vapply(check(original), `[[`, logical(1), "passed")))
  changed <- original; attr(changed$AGE, "label") <- NULL
  expect_false(check(changed)$labels$passed)
  changed <- original; attr(changed$ADT, "format.sas") <- "DATETIME20."
  expect_false(check(changed)$formats$passed)
  changed <- original; changed$AGE <- as.character(changed$AGE)
  expect_false(check(changed)$types$passed)
  expect_false(check(original[c(2, 1, 3)])$column_order$passed)
  expect_false(check(original[-2])$required_columns$passed)
  changed <- original; changed$USUBJID[2] <- "01"
  expect_false(check(changed)$unique_keys$passed)
  changed <- original; changed$USUBJID[2] <- NA_character_
  expect_false(check(changed)$keys_nonmissing$passed)
  changed$USUBJID[2] <- "  "
  expect_false(check(changed)$keys_nonmissing$passed)
  expect_false(check(original[-1])$keys_present$passed)
  expect_false(check(original[1, ])$row_count$passed)
  expect_false(check(original[FALSE, ])$min_rows$passed)
  expect_false(check(original[c(1, 2, 1, 2), ])$max_rows$passed)
  folded <- original; names(folded) <- tolower(names(folded))
  expect_true(all(vapply(check(folded), `[[`, logical(1), "passed")))
})

test_that("profiles resolve in R and YAML with whole-field target overrides", {
  overrides <- list(profiles = list(subject = qc_fixture_profile()),
                    assertions = list("adam.adsl" = list(profile = "subject", row_count = 3)))
  out <- infer_output_contracts(NULL, overrides)
  expect_identical(out$assertions[[1]]$profile, "subject")
  expect_equal(out$assertions[[1]]$row_count, 3)
  expect_identical(out$assertions[[1]]$labels, list(AGE = "Age in years"))
  file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(outputs = overrides), file)
  cfg <- sas_config(file)
  from_yaml <- infer_output_contracts(NULL, cfg$outputs)$assertions[[1]]
  expect_identical(unname(unlist(from_yaml$labels)), "Age in years")
  expect_true(all(vapply(check_dataset_qc(qc_fixture_data(),
    within(from_yaml, row_count <- 2)), `[[`, logical(1), "passed")))
  overrides$assertions[[1]]$profile <- "typo"
  expect_error(infer_output_contracts(NULL, overrides), "Unknown QC profile")
  expect_error(qc_profile(unique_keys = TRUE), "requires keys")
  expect_error(qc_profile(min_rows = 3, max_rows = 1), "contradict")
  expect_error(qc_profile(types = c(AGE = "number")), "unsupported")
  expect_error(qc_profile(tolerances = list(AGE = list(abs = -1))), "non-negative")
  expect_error(infer_output_contracts(NULL, list(profiles = list(x = list(lables = "Age")))), "lables")
})

test_that("quality and per-variable tolerance requirements reach the output gate", {
  ref <- qc_fixture_data()
  reference <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ref, reference)
  contract <- infer_output_contracts(NULL, list(
    profiles = list(subject = qc_fixture_profile()),
    references = list("adam.adsl" = reference),
    assertions = list("adam.adsl" = list(profile = "subject"))
  ))
  candidate <- ref; candidate$AGE[1] <- 50.005
  expect_true(assess_dataset_target(contract, candidate)$passed)
  candidate$AGE[1] <- 50.02
  expect_false(assess_dataset_target(contract, candidate)$checks$reference_comparison$passed)
  candidate <- ref; attr(candidate$AGE, "label") <- NULL
  result <- assess_dataset_target(contract, candidate)
  expect_false(result$passed)
  expect_false(result$checks$labels$passed)
  contract$reference_path <- NA_character_
  result <- assess_dataset_target(contract, ref)
  expect_true(result$passed)
  expect_false(result$has_reference)
  expect_false(result$reference_passed)
})

test_that("each declared clinical property can fail a target with a diagnostic", {
  defects <- list(
    labels = function(x) { attr(x$AGE, "label") <- "wrong"; x },
    formats = function(x) { attr(x$ADT, "format.sas") <- NULL; x },
    types = function(x) { x$ADT <- as.numeric(x$ADT); x },
    column_order = function(x) x[c(2, 1, 3)],
    required_columns = function(x) x[-2],
    unique_keys = function(x) { x$USUBJID[2] <- "01"; x },
    keys_nonmissing = function(x) { x$USUBJID[1] <- NA_character_; x },
    keys_present = function(x) x[-1],
    row_count = function(x) x[1, ],
    min_rows = function(x) x[FALSE, ],
    max_rows = function(x) x[c(1, 2, 1, 2), ]
  )
  contract <- infer_output_contracts(NULL, list(
    profiles = list(subject = qc_fixture_profile()),
    assertions = list("adam.adsl" = list(profile = "subject"))))
  for (property in names(defects)) {
    result <- assess_dataset_target(contract, defects[[property]](qc_fixture_data()))
    expect_false(result$passed, info = property)
    expect_false(result$checks[[property]]$passed, info = property)
    expect_match(result$reason, property, fixed = TRUE)
  }
})

test_that("clinical key uniqueness uses the comparator's SAS padding rules", {
  data <- qc_fixture_data()
  data$USUBJID <- c("01", "01 ")
  result <- assess_dataset_target(list(target_key = "adam.adsl", required = TRUE,
    assertions = qc_fixture_profile()), data)
  expect_false(result$passed)
  expect_false(result$checks$unique_keys$passed)
  expect_equal(result$checks$unique_keys$duplicate_rows, 1)
})

test_that("alignment keys alone preserve reference comparisons with missing or repeated keys", {
  ref <- data.frame(AVISITN = c(NA_real_, NA_real_, 1), AVAL = c(10, 20, 30))
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ref, path)
  contract <- list(target_key = "adam.advs", required = TRUE,
    reference_path = path, assertions = list(keys = "AVISITN"))
  result <- assess_dataset_target(contract, ref)
  expect_true(result$passed)
  expect_true(result$reference_passed)
  expect_null(result$checks$keys_nonmissing)
  expect_null(result$checks$unique_keys)
})
