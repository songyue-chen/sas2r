test_that("clean emitted code lints quietly", {
  code <- 'x <- lib_read("adam", "adsl") |>\n  dplyr::filter((is.na(age) | age < 65))'
  l <- lint_r_code(code)
  expect_identical(nrow(l[l$level == "error", ]), 0L)
  expect_false("unwrapped_comparison" %in% l$kind)
})

test_that("banned functions and parse failures are errors", {
  expect_identical(lint_r_code("system('rm -rf /')")$kind[1], "banned_function")
  expect_identical(lint_r_code("x <- ((")$kind[1], "parse_failure")
})

test_that("unwrapped comparisons are flagged", {
  l <- lint_r_code("y <- dplyr::filter(df, age < 65)")
  expect_true("unwrapped_comparison" %in% l$kind)

  # F19: is.na(other) does not silence unwrapped comparison on age
  l2 <- lint_r_code("dplyr::filter(df, is.na(other) & age < 65)")
  expect_true("unwrapped_comparison" %in% l2$kind)

  # is.na(age) | age < 65 is properly wrapped and not flagged
  l3 <- lint_r_code("dplyr::filter(df, is.na(age) | age < 65)")
  expect_false("unwrapped_comparison" %in% l3$kind)
})

test_that("unknown functions and foreign namespaces are surfaced", {
  l <- lint_r_code("z <- data.table::setDT(df); imaginary_fn(1)")
  expect_true("disallowed_namespace" %in% l$kind)
  expect_true("unknown_function" %in% l$kind)
})

test_that("all banned functions are flagged as errors", {
  for (fn in c("system", "system2", "shell", "download.file", "url",
               "unlink", "file.remove", "Sys.setenv", "source",
               "eval", "parse", "library", "require")) {
    code <- sprintf("%s('arg')", fn)
    res <- lint_r_code(code)
    expect_true("banned_function" %in% res$kind, info = fn)
    expect_identical(res$level[res$kind == "banned_function"], "error")
  }
})

test_that("empty code returns empty tibble with correct schema", {
  res <- lint_r_code("")
  expect_s3_class(res, "tbl_df")
  expect_identical(names(res), c("level", "kind", "detail"))
  expect_identical(nrow(res), 0L)
})

test_that("sas2r helper functions are recognized and not marked unknown", {
  code <- 'res <- sas_sum(1, 2) %+% sas_compress(" a b ") |> sas_round(0.1)'
  l <- lint_r_code(code)
  expect_false("unknown_function" %in% l$kind)
})

test_that("custom allowlist and helpers are respected", {
  code <- "custom_pkg::my_fn(1); my_helper(2)"
  l_default <- lint_r_code(code)
  expect_true("disallowed_namespace" %in% l_default$kind)
  expect_true("unknown_function" %in% l_default$kind)

  l_custom <- lint_r_code(code, allowlist = c("custom_pkg"), helpers = c("my_helper"))
  expect_identical(nrow(l_custom), 0L)
})

test_that("SAS2R_HELPER_NAMES is in sync with sas2r-helpers.R template", {
  # Set-equal in both directions on purpose, and the constant is the whole
  # template surface rather than only what emitted units call -- see
  # ?SAS2R_HELPER_NAMES for why the internal helpers belong in it. A name here
  # the template does not define allows a call nothing can satisfy; a name the
  # template defines and this omits is a call lint_r_code() would reject.
  e <- new.env(parent = baseenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  expect_setequal(SAS2R_HELPER_NAMES, ls(e, all.names = TRUE))
})

test_that("parenthesized negation !(is.na(v)) is recognized without false warning", {
  code <- "dplyr::filter(df, !(is.na(age)) & age > 65)"
  l <- lint_r_code(code)
  expect_false("unwrapped_comparison" %in% l$kind)
})

test_that("empty index arguments do not crash the linter", {
  # `arg <- e[[i]]` binds the empty symbol to a variable, and is.null(arg) then
  # forces that binding, raising "argument \"arg\" is missing, with no default".
  # The guard for the empty symbol sat second in the && and never ran. Any
  # generated R using x[, 1] or df[1, ] -- the most ordinary indexing there is,
  # and unavoidable when translating SAS -- aborted the lint gate, which is
  # what failed every macro unit in the validation project.
  for (code in c("y <- x[, 1]", "y <- df[1, ]", "y <- m[, , 2]",
                 "y <- df[df$a > 1, ]", "f <- function(a, b) a")) {
    expect_no_error(lint_r_code(code), message = code)
  }
})

test_that("a lint finding is still reported through an empty index argument", {
  # The walk must continue past the empty slot rather than stopping there.
  lint <- lint_r_code("y <- df[, system('rm -rf /')]")

  expect_true(nrow(lint) > 0L)
})

test_that("parse failures are still reported as errors", {
  lint <- lint_r_code("y <- (")

  expect_true(any(lint$level == "error"))
  expect_true(any(lint$kind == "parse_failure"))
})
