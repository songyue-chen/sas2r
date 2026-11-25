# A production run that finds a reviewer comparison summary outside its closed
# format withholds it and carries on. Under test that must be a failure, so a
# defect in the summary is seen here and never becomes a quietly missing
# summary. Tests of the production path switch this off locally.
withr::local_options(
  list(sas2r.strict_discrepancy_summary = TRUE),
  .local_envir = testthat::teardown_env()
)
