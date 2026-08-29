tr <- function(x) tidy_expr(translate_expr(x))

test_that("operators and word forms translate", {
  expect_identical(tr("sex = 'M' and age ge 65"), "sex == 'M' & age >= 65")
  expect_identical(tr("trt ^= 'PLACEBO'"), "trt != 'PLACEBO'")
  expect_identical(tr("not ( x lt 5 )"), "!(x < 5)")
  expect_identical(tr("a || b"), "a %+% b")
})

test_that("functions map through the rulebook; in-lists become c()", {
  expect_identical(tr("upcase(sex) = 'M'"), "toupper(sex) == 'M'")
  expect_identical(tr("visit in (1, 2)"), "visit %in% c(1, 2)")
  expect_identical(tr("sum(a, b)"), "sas_sum(a, b)")
})

test_that("missing-value truth table wraps complex and repeated shapes", {
  expect_identical(wrap_missing("age < 65", "age"), "(is.na(age) | age < 65)")
  expect_identical(wrap_missing("age > 65", "age"), "(!is.na(age) & age > 65)")
  expect_identical(wrap_missing("age == 65", "age"), "(!is.na(age) & age == 65)")
  expect_identical(wrap_missing("age != 65", "age"), "(is.na(age) | age != 65)")
  expect_identical(
    wrap_missing("age < 65 & wt > 50", c("age", "wt")),
    "(is.na(age) | age < 65) & (!is.na(wt) & wt > 50)")

  # F5: Arithmetic RHS not mangled
  expect_identical(
    wrap_missing("age < b + 1", c("age", "b")),
    "(is.na(age) | age < b + 1)")

  # F5: Repeated comparison both wrapped
  expect_identical(
    wrap_missing("age < 65 | (age < 65 & sex == 'M')", c("age", "sex")),
    '(is.na(age) | age < 65) | ((is.na(age) | age < 65) & chr_cmp(sub("\\\\s+$", "", sex), "M", "=="))')

  # F5: Literal on left
  expect_identical(
    wrap_missing("65 > age", "age"),
    "(is.na(age) | 65 > age)")

  # F5: Var-var comparison
  expect_identical(
    wrap_missing("v1 == v2", c("v1", "v2")),
    'chr_cmp(v1, v2, "==")')

  # F5: Var-call comparison
  expect_identical(
    wrap_missing("code == substr(raw, 1, 3)", "code"),
    'chr_cmp(code, substr(raw, 1, 3), "==")')
})

test_that("unparseable output and unrecognized tokens error", {
  expect_error(tidy_expr("x ==== 1"), class = "sas2r_expr_parse_error")
  # F9: Unrecognized token e.g. prefix comparison =:
  expect_error(translate_expr("name =: 'AB'"), class = "sas2r_expr_parse_error")
})

test_that("expr_vars finds comparison operands", {
  expect_setequal(expr_vars("age ge 65 and sex = 'M'"), c("age", "sex"))
})

test_that("SAS space-delimited IN lists become comma-separated R vectors", {
  # SAS accepts `x in (1 2 3)` with no commas. Found in 4 of 8 real PhUSE WPCT
  # programs, e.g. `where (paramcd in ('DIABP') and atptn in (815 817))`.
  # Passing the list through verbatim produced `c(815 817)`, which does not
  # parse, and the abort took down the whole run.
  expect_identical(tidy_expr(translate_expr("x in (1 2 3)")), "x %in% c(1, 2, 3)")
  expect_identical(tidy_expr(translate_expr("x in (815 817)")), "x %in% c(815, 817)")
  expect_identical(tidy_expr(translate_expr("x not in (1 2)")), "x %notin% c(1, 2)")

  # Already-comma-separated lists are unchanged, and mixed forms normalize.
  expect_identical(tidy_expr(translate_expr("x in (1, 2)")), "x %in% c(1, 2)")
  expect_identical(tidy_expr(translate_expr("x in (1, 2 3)")), "x %in% c(1, 2, 3)")

  # A quoted element containing a space is one element, not two.
  expect_identical(tidy_expr(translate_expr("x in ('A B' 'C')")), "x %in% c('A B', 'C')")
  expect_identical(tidy_expr(translate_expr("x in ('DIABP')")), "x %in% c('DIABP')")

  # The real WPCT clause parses end to end.
  real <- tidy_expr(translate_expr("(paramcd in ('DIABP') and atptn in (815 817))"))
  expect_false(inherits(tryCatch(parse(text = real), error = function(e) e), "error"))
})

test_that("SAS ** exponentiation translates to ^", {
  expect_identical(tr("a ** 2"), "a ^ 2")
  expect_identical(tr("a**2 + b"), "a ^ 2 + b")
})

test_that("the SAS missing literal . becomes numeric NA with SAS comparison semantics", {
  expect_identical(tr("x = ."), "x == NA_real_")
  expect_identical(tr("x in (1 2 .)"), "x %in% c(1, 2, NA_real_)")

  # Comparisons against the missing literal must route through chr_cmp, where
  # missing sorts lowest, never through the is.na()-wrap (x == NA is always NA).
  expect_identical(wrap_missing("x == NA_real_", "x"), 'chr_cmp(x, NA_real_, "==")')
  expect_identical(wrap_missing("x < NA_real_", "x"), 'chr_cmp(x, NA_real_, "<")')

  h <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), h)
  h$x <- c(1, NA)
  eval_wrapped <- function(sas) {
    eval(parse(text = wrap_missing(tr(sas), "x")), envir = h)
  }
  expect_identical(eval_wrapped("x = ."), c(FALSE, TRUE))    # only missing equals .
  expect_identical(eval_wrapped("x ^= ."), c(TRUE, FALSE))
  expect_identical(eval_wrapped("x < ."), c(FALSE, FALSE))   # missing sorts lowest
  expect_identical(eval_wrapped("x > ."), c(TRUE, FALSE))
})

test_that("unmapped SAS functions refuse instead of binding to base R", {
  # SCAN() and DATE() exist in base R with unrelated behavior; silent
  # passthrough would emit code that runs and returns wrong values.
  expect_error(translate_expr("scan(str, 2)"),
               class = "sas2r_expr_unmapped_function")
  expect_error(translate_expr("date() - 1"),
               class = "sas2r_expr_unmapped_function")
  # The refusal is still an expression-level parse refusal for callers.
  expect_error(translate_expr("scan(str, 2)"),
               class = "sas2r_expr_parse_error")
  # Mapped functions and operator word forms keep working.
  expect_identical(tr("upcase(sex)"), "toupper(sex)")
  expect_identical(tr("not (x in (1 2))"), "!(x %in% c(1, 2))")
})

test_that("variable tokens fold to lowercase like SAS name resolution", {
  expect_identical(tr("AVAL > 10 and Sex = 'M'"), "aval > 10 & sex == 'M'")
  # Mapped functions keep their R target's spelling; only bare names fold.
  expect_identical(tr("UPCASE(Sex)"), "toupper(sex)")
})
