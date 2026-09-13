test_that("helper documentation stays synchronized with the runtime help", {
  man_dir <- testthat::test_path("..", "..", "man")
  skip_if_not(dir.exists(man_dir))
  expect_equal(helper_documentation(), runtime_helper_documentation(man_dir))
})

test_that("helper call checks reject invented arguments using actual signatures", {
  for (code in c('sas_merge(a, b, by = "id", all.x = TRUE)',
                 'sas_merge(a, b, by = "id", in_a = "left")',
                 'sas_sort(df, "id", nodupkey = TRUE)',
                 'sas_if_else(test = x, yes = y, no = z)')) {
    expect_true(any(lint_r_code(code)$kind == "helper_misuse"), info = code)
  }
  for (code in c('sas_merge(a, b, "id", "left")',
                 'sas_merge(by = "id", b = right, a = left, keep = "full")',
                 'sas_sum(x, y, z)', 'sas_min(a = x, b = y)',
                 'wrapper <- function(...) sas_sum(...)',
                 'x |> sas_sort("id")')) {
    expect_false(any(lint_r_code(code)$kind == "helper_misuse"), info = code)
  }
})

test_that("helper call violations fail the program gate even without declared helper_use", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines('out <- sas_merge(a, b, by = "id", type = "left")', path)
  checks <- check_program_revision(path, new_behavioral_contract("arbitrary_program"))
  expect_false(checks$pass)
  expect_true(any(grepl("unused argument", checks$errors)))
})

test_that("a missing helper reference gives an actionable error instead of empty interfaces", {
  for (path in c("", tempfile("missing-reference-"))) {
    lookup <- helper_documentation
    environment(lookup) <- list2env(list(system.file = function(...) path),
                                    parent = environment(helper_documentation))
    expect_error(lookup(), "Reinstall sas2r", class = "sas2r_helper_reference_missing")
  }
})
