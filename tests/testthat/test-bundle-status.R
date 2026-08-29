test_that("bundle status follows execution outputs and lineage evidence", {
  # Helper constructors for the five canonical cases
  case_missing_output <- function() {
    list(
      execution = list(completed = TRUE, passed = TRUE, exit_status = 0L),
      targets = list(
        list(target_id = "t1", target_key = "adam.adsl", required = TRUE,
             status = "missing_candidate", passed = FALSE, has_reference = FALSE)
      ),
      lineage_evidence = list(
        is_blocked = FALSE, review_unavailable = FALSE, min_level = "runtime_verified"
      )
    )
  }

  case_high_risk_review_only <- function() {
    list(
      execution = list(completed = TRUE, passed = TRUE, exit_status = 0L),
      targets = list(
        list(target_id = "t1", target_key = "adam.adsl", required = TRUE,
             status = "passed", passed = TRUE, has_reference = FALSE)
      ),
      lineage_evidence = list(
        is_blocked = FALSE, review_unavailable = FALSE, min_level = "reviewed_only",
        has_review_only = TRUE
      )
    )
  }

  case_contracts_pass <- function() {
    list(
      execution = list(completed = TRUE, passed = TRUE, exit_status = 0L),
      targets = list(
        list(target_id = "t1", target_key = "adam.adsl", required = TRUE,
             status = "passed", passed = TRUE, has_reference = FALSE)
      ),
      lineage_evidence = list(
        is_blocked = FALSE, review_unavailable = FALSE, min_level = "runtime_verified"
      )
    )
  }

  case_references_pass <- function() {
    list(
      execution = list(completed = TRUE, passed = TRUE, exit_status = 0L),
      targets = list(
        list(target_id = "t1", target_key = "adam.adsl", required = TRUE,
             status = "passed", passed = TRUE, has_reference = TRUE, reference_passed = TRUE)
      ),
      lineage_evidence = list(
        is_blocked = FALSE, review_unavailable = FALSE, min_level = "reference_validated"
      )
    )
  }

  case_low_evidence_off_lineage <- function() {
    list(
      execution = list(completed = TRUE, passed = TRUE, exit_status = 0L),
      targets = list(
        list(target_id = "t1", target_key = "adam.adsl", required = TRUE,
             status = "passed", passed = TRUE, has_reference = FALSE)
      ),
      lineage_evidence = list(
        is_blocked = FALSE, review_unavailable = FALSE, min_level = "runtime_verified"
      ),
      off_lineage_components = list(
        list(component_id = "unrelated_tool", level = "reviewed_only", review_unavailable = TRUE)
      )
    )
  }

  expect_identical(derive_bundle_status(case_missing_output()), "blocked")
  expect_identical(derive_bundle_status(case_high_risk_review_only()),
                   "needs_review")
  expect_identical(derive_bundle_status(case_contracts_pass()),
                   "migration_ready")
  expect_identical(derive_bundle_status(case_references_pass()), "validated")
  expect_identical(derive_bundle_status(case_low_evidence_off_lineage()),
                   "migration_ready")
})

test_that("derive_bundle_status handles execution failure and skipped execution", {
  # Execution failed
  case_exec_failed <- list(
    execution = list(completed = TRUE, passed = FALSE, exit_status = 1L),
    targets = list(
      list(target_id = "t1", required = TRUE, passed = TRUE)
    ),
    lineage_evidence = list(is_blocked = FALSE, min_level = "runtime_verified")
  )
  expect_identical(derive_bundle_status(case_exec_failed), "blocked")

  # Execution skipped / deferred
  case_exec_deferred <- list(
    execution = list(completed = FALSE, passed = FALSE, deferred = TRUE, reason = "execute_disabled"),
    targets = list(
      list(target_id = "t1", required = TRUE, passed = TRUE)
    ),
    lineage_evidence = list(is_blocked = FALSE, min_level = "reviewed_only")
  )
  expect_identical(derive_bundle_status(case_exec_deferred), "needs_review")
})

test_that("derive_bundle_status rejects model-derived status bypass", {
  # An adversary attempts to pass status = 'validated' directly while targets are failing
  case_spoofed <- list(
    status = "validated",
    model_status = "validated",
    execution = list(completed = TRUE, passed = TRUE, exit_status = 0L),
    targets = list(
      list(target_id = "t1", required = TRUE, passed = FALSE, status = "failed")
    ),
    lineage_evidence = list(is_blocked = FALSE, min_level = "runtime_verified")
  )
  expect_identical(derive_bundle_status(case_spoofed), "blocked")
})

test_that("assertions alone or a merely-present reference cannot grant validated", {
  base_exec <- list(completed = TRUE, passed = TRUE, exit_status = 0L)

  # Keys/tolerance assertions configured but no reference comparison ever ran:
  # that is comparison configuration, not equivalence evidence.
  case_assertions_only <- list(
    execution = base_exec,
    targets = list(
      list(target_id = "t1", target_key = "adam.adsl", required = TRUE,
           status = "passed", passed = TRUE, has_reference = FALSE,
           reference_passed = FALSE, has_assertions = TRUE)
    ),
    lineage_evidence = list(
      is_blocked = FALSE, review_unavailable = FALSE,
      min_level = "runtime_verified", has_assertions_evaluated = TRUE
    )
  )
  expect_identical(derive_bundle_status(case_assertions_only), "migration_ready")

  # A reference file that exists but was never actually compared.
  case_ref_not_compared <- list(
    execution = base_exec,
    targets = list(
      list(target_id = "t1", target_key = "t_demo.rtf", required = TRUE,
           status = "passed", passed = TRUE, has_reference = TRUE,
           reference_passed = FALSE, has_assertions = FALSE)
    ),
    lineage_evidence = list(
      is_blocked = FALSE, review_unavailable = FALSE, min_level = "runtime_verified"
    )
  )
  expect_identical(derive_bundle_status(case_ref_not_compared), "migration_ready")
})
