test_that("macro artifacts write known keyword defaults as exact R literals", {
  contract <- parse_macro_contract("keyword_macro", "label=ready, count=2.5")
  result <- list(
    code = 'keyword_macro <- function(label = "ready", count = 2.5) NULL',
    macro_contract = contract
  )
  out <- withr::local_tempdir()

  emit_macro_artifacts(result, "keyword_macro", out)
  spec_txt <- paste(readLines(file.path(
    out, "tests_macros", "test-keyword_macro.R"
  )), collapse = "\n")

  expect_match(spec_txt,
               'expect_identical(names(formals(fn)), c("label", "count"))',
               fixed = TRUE)
  expect_match(spec_txt,
               'expect_identical(formals(fn)[["label"]], "ready")',
               fixed = TRUE)
  expect_match(spec_txt,
               "expect_identical(eval(formals(fn)[[\"count\"]]), 2.5)",
               fixed = TRUE)
})

test_that("macro artifact tests parse and run for signed and non-syntactic contracts", {
  contract <- parse_macro_contract("if", "_hidden=-2.5")
  result <- list(
    code = "`if` <- function(`_hidden` = -2.5) NULL",
    macro_contract = contract
  )
  out <- withr::local_tempdir()

  emit_macro_artifacts(result, "if", out)
  spec_path <- file.path(out, "tests_macros", "test-if.R")
  spec_txt <- paste(readLines(spec_path, warn = FALSE), collapse = "\n")
  parsed <- try(parse(file = spec_path), silent = TRUE)

  expect_false(inherits(parsed, "try-error"))
  expect_match(spec_txt, 'fn <- get("if", inherits = FALSE)', fixed = TRUE)
  expect_match(spec_txt,
               'expect_identical(names(formals(fn)), c("_hidden"))',
               fixed = TRUE)
  expect_match(spec_txt,
               'expect_identical(eval(formals(fn)[["_hidden"]]), -2.5)',
               fixed = TRUE)
  if (!inherits(parsed, "try-error")) {
    spec_results <- withr::with_dir(
      dirname(spec_path),
      testthat::test_file(basename(spec_path), reporter = "silent")
    )
    spec_df <- as.data.frame(spec_results)
    expect_true(all(spec_df$failed == 0L & !spec_df$error))
    expect_true(all(spec_df$passed > 0L))
  }
})

test_that("numeric macro artifact tests run for plain and unary formal literals", {
  cases <- list(
    list(sas_default = "2.5", r_literal = "2.5", expected = "2.5"),
    list(sas_default = "+2.5", r_literal = '"+2.5"', expected = '"+2.5"'),
    list(sas_default = "-0", r_literal = '"-0"', expected = '"-0"'),
    list(sas_default = "-2.5", r_literal = "-2.5", expected = "-2.5")
  )

  for (i in seq_along(cases)) {
    case <- cases[[i]]
    macro_name <- paste0("numeric_case_", i)
    contract <- parse_macro_contract(
      macro_name, paste0("value=", case$sas_default)
    )
    result <- list(
      code = sprintf("%s <- function(value = %s) NULL", macro_name, case$r_literal),
      macro_contract = contract
    )
    out <- withr::local_tempdir()

    emit_macro_artifacts(result, macro_name, out)
    spec_path <- file.path(out, "tests_macros", paste0("test-", macro_name, ".R"))
    spec_txt <- paste(readLines(spec_path, warn = FALSE), collapse = "\n")
    parsed <- try(parse(file = spec_path), silent = TRUE)

    expect_false(inherits(parsed, "try-error"), info = case$sas_default)
    expect_match(
      spec_txt,
      paste0(
        if (startsWith(case$r_literal, '"')) 'expect_identical(formals(fn)[["value"]], ' else 'expect_identical(eval(formals(fn)[["value"]]), ', case$expected, ")"
      ),
      fixed = TRUE,
      info = case$sas_default
    )
    if (!inherits(parsed, "try-error")) {
      spec_results <- withr::with_dir(
        dirname(spec_path),
        testthat::test_file(basename(spec_path), reporter = "silent")
      )
      spec_df <- as.data.frame(spec_results)
      expect_true(all(spec_df$failed == 0L & !spec_df$error), info = case$sas_default)
      expect_true(all(spec_df$passed > 0L), info = case$sas_default)
    }
  }
})


comment_evidence_project <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(c(
    "/* selected leading evidence */",
    "data w1; set w.root;",
    "/* selected internal evidence */",
    "retain c 0; c = c + x; run;",
    "/* neighbor-only evidence */",
    "data w2; x = 1; run;"
  ), file.path(dir, "comments.sas"))
  project <- sas_project(dir)
  out <- withr::local_tempdir(.local_envir = parent.frame())
  transpilation <- sas_transpile(project, out)
  unit_id <- transpilation$manifest$unit_id[transpilation$manifest$tier == "stub"][1]
  list(project = project, transpilation = transpilation, unit_id = unit_id)
}

test_that("unit comment evidence is stable and attached to its unit", {
  # Removing unit filtering, ordering, or provenance fields would let a worker
  # act on another unit's note or make a cited comment impossible to locate.
  fixture <- comment_evidence_project()
  selected <- fixture$project$comments[
    !is.na(fixture$project$comments$unit_id) &
      fixture$project$comments$unit_id == fixture$unit_id,
    , drop = FALSE
  ]
  expected <- paste(sprintf(
    "[%s; lines %d-%d; %s; %s]\n%s",
    selected$file, selected$line_start, selected$line_end,
    selected$kind, selected$placement, selected$text
  ), collapse = "\n\n")

  evidence <- sas2r:::render_unit_comment_evidence(
    fixture$project, fixture$unit_id
  )

  expect_identical(evidence, expected)
  expect_match(evidence, "selected leading evidence", fixed = TRUE)
  expect_match(evidence, "selected internal evidence", fixed = TRUE)
  expect_false(grepl("neighbor-only evidence", evidence, fixed = TRUE))
})
