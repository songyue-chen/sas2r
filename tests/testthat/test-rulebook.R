test_that("rulebook loads and contains the seed maps", {
  rb <- load_rulebook()
  expect_identical(rb$functions[["upcase"]], "toupper")
  expect_identical(rb$functions[["sum"]], "sas_sum")
  expect_identical(rb$operators[["ge"]], ">=")
  expect_identical(rb$procs$sort$status, "supported")
  expect_identical(rb$procs$transpose$status, "deferred")
  expect_identical(rb$semantics$schema_version, 1L)
  expect_identical(rb$semantics$rules$functions.round$strategy, "sas_round")
  expect_true(is.character(rb$functions))
  expect_true(is.character(rb$operators))
})

test_that("load_rulebook memoizes results across calls", {
  rb1 <- load_rulebook()
  rb2 <- load_rulebook()
  expect_identical(rb1, rb2)
})

test_that("executable rulebook YAML never evaluates expressions", {
  dir <- withr::local_tempdir()
  marker <- file.path(dir, "rulebook-expression-ran")
  map_file <- file.path(dir, "functions.yml")
  expression <- sprintf("file.create(%s)", deparse(marker))
  writeLines(sprintf("probe: !expr %s", deparse(expression)), map_file)
  withr::local_options(list(yaml.eval.expr = TRUE))

  parsed <- read_rulebook_yaml(map_file)
  expect_identical(parsed$probe, expression)
  expect_false(file.exists(marker))
})
