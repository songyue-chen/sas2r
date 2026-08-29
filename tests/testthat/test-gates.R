test_that("parse gate passes clean code and fails banned code", {
  g <- gate_parse('x <- lib_read("w", "a") |> dplyr::filter((!is.na(k) & k == 1))')
  expect_true(g$pass)
  g2 <- gate_parse("system('ls')")
  expect_false(g2$pass)
  expect_identical(g2$lint$kind[1], "banned_function")
})

test_that("parse gate fails on syntax errors", {
  g <- gate_parse("x <- + % invalid syntax")
  expect_false(g$pass)
  expect_true(any(g$lint$level == "error"))
  expect_true(any(g$lint$kind == "parse_failure"))
})

test_that("parse gate passes code with warnings only", {
  # unwrapped comparison generates warning, not error
  g <- gate_parse("x <- a > 10")
  expect_true(g$pass)
  expect_true(any(g$lint$level == "warn"))
})

test_that("check_program_revision validates clean R program file", {
  dir <- withr::local_tempdir()
  r_file <- file.path(dir, "prog.R")
  writeLines("out <- data.frame(x = 1:10)\nlib_write(out, 'work', 'out')", r_file)

  res <- check_program_revision(r_file)
  expect_true(res$pass)
  expect_length(res$errors, 0L)
})

test_that("check_program_revision fails on missing file or parse error", {
  res_missing <- check_program_revision("/nonexistent/file.R")
  expect_false(res_missing$pass)
  expect_match(res_missing$errors[1], "file not found")

  dir <- withr::local_tempdir()
  r_file <- file.path(dir, "bad.R")
  writeLines("out <- + % bad syntax", r_file)
  res_bad <- check_program_revision(r_file)
  expect_false(res_bad$pass)
  expect_match(res_bad$errors[1], "parse_error")
})

test_that("check_program_revision validates parameter contracts and helper names", {
  dir <- withr::local_tempdir()
  r_file <- file.path(dir, "fn.R")
  writeLines("my_calc <- function(a, b) {\n  sas_sum(a, b)\n}", r_file)

  cntr <- list(
    component_id = "my_calc",
    parameters = list(list(name = "a"), list(name = "b")),
    helper_use = list("sas_sum")
  )
  res <- check_program_revision(r_file, contract = cntr)
  expect_true(res$pass)

  cntr_bad_helper <- list(
    component_id = "my_calc",
    parameters = list(list(name = "a"), list(name = "b")),
    helper_use = list("nonexistent_helper_fn")
  )
  res_bad_h <- check_program_revision(r_file, contract = cntr_bad_helper)
  expect_false(res_bad_h$pass)
  expect_match(res_bad_h$errors[1], "unknown_helper")
})

