test_that("macro contracts preserve canonical parameters and known defaults", {
  contract <- parse_macro_contract(
    "derive_flag",
    "input, dataset=adsl, limit=-2.5, label='Analysis ''Population''', note=\"ready\""
  )

  expect_identical(contract$name, "derive_flag")
  expect_identical(names(contract$parameters), c(
    "position", "name", "kind", "sas_default", "default_status",
    "default_key", "r_default"
  ))
  expect_identical(contract$parameters$position, 1:5)
  expect_identical(contract$parameters$name,
                   c("input", "dataset", "limit", "label", "note"))
  expect_identical(contract$parameters$kind,
                   c("positional", rep("keyword", 4L)))
  expect_identical(contract$parameters$sas_default,
                   c("", "adsl", "-2.5", "'Analysis ''Population'''", "\"ready\""))
  expect_identical(contract$parameters$default_status, rep("known", 5L))
  expect_identical(contract$parameters$default_key, c(
    "character:", "character:adsl", "numeric:-2.5",
    "character:Analysis 'Population'", "character:ready"
  ))
  expect_identical(contract$parameters$r_default[[1]], "")
  expect_identical(contract$parameters$r_default[[2]], "adsl")
  expect_identical(contract$parameters$r_default[[3]], -2.5)
  expect_identical(contract$parameters$r_default[[4]], "Analysis 'Population'")
  expect_identical(contract$parameters$r_default[[5]], "ready")
})

test_that("macro contract splitting respects scan masks and nested arguments", {
  contract <- parse_macro_contract(
    "example",
    "first=%str(one,two), second=%sysfunc(cats(a,b)), quoted='x,y', percent=\"100%\", decimal=1.1, dynamic=&runtime, expression=a+b, quoted_macro=\"%upcase(label)\", blank="
  )

  expect_identical(contract$parameters$name,
                   c("first", "second", "quoted", "percent", "decimal", "dynamic", "expression", "quoted_macro", "blank"))
  expect_identical(contract$parameters$sas_default, c(
    "%str(one,two)", "%sysfunc(cats(a,b))", "'x,y'", "\"100%\"", "1.1", "&runtime", "a+b", "\"%upcase(label)\"", ""
  ))
  expect_identical(contract$parameters$default_status,
                   c("unresolved", "unresolved", "known", "known", "known", "unresolved", "unresolved", "unresolved", "known"))
  expect_true(all(startsWith(
    contract$parameters$default_key[contract$parameters$default_status == "unresolved"],
    "unresolved:"
  )))
  expect_identical(contract$parameters$r_default[[3]], "x,y")
  expect_identical(contract$parameters$r_default[[4]], "100%")
  expect_identical(contract$parameters$default_key[[5]], "numeric:1.1")
  expect_identical(contract$parameters$r_default[[9]], "")
  expect_null(contract$parameters$r_default[[1]])
  expect_null(contract$parameters$r_default[[2]])
  expect_null(contract$parameters$r_default[[6]])
  expect_null(contract$parameters$r_default[[7]])
  expect_null(contract$parameters$r_default[[8]])
})

test_that("macro contract parsing rejects malformed signatures", {
  expect_error(parse_macro_contract("bad", "a=1, a=2"),
               class = "sas2r_macro_contract_error")
  expect_error(parse_macro_contract("bad", "a=1, b"),
               class = "sas2r_macro_contract_error")
  expect_error(parse_macro_contract("bad", "a,,b"),
               class = "sas2r_macro_contract_error")
})

test_that("macro contract lookup uses exactly the requested unit definition", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "%macro alpha(value=wrong);",
    "%mend alpha;",
    "%macro beta(value=right, count=2);",
    "%mend beta;"
  ), file.path(dir, "macros.sas"))
  project <- sas_project(dir)
  beta_id <- project$macros$defs$unit_id[project$macros$defs$name == "beta"]

  contract <- macro_contract_for_unit(project, beta_id)

  expect_identical(contract$name, "beta")
  expect_identical(contract$parameters$name, c("value", "count"))
  expect_identical(contract$parameters$default_key,
                   c("character:right", "numeric:2"))
  expect_error(macro_contract_for_unit(project, 999L),
               class = "sas2r_macro_contract_error")
})

test_that("macro contract validation checks R formals without executing code", {
  contract <- parse_macro_contract(
    "beta", "input, dataset=adsl, count=2, label='ready', dynamic=&runtime"
  )

  exact <- validate_macro_contract(
    "other <- function(x) x\nbeta <- function(input = '', dataset = 'adsl', count = 2, label = 'ready', dynamic = NULL) { stop('not run') }",
    contract
  )
  expect_true(exact$pass)
  expect_identical(exact$errors, character())
  expect_identical(exact$unresolved, "dynamic")

  wrong_order <- validate_macro_contract(
    "beta <- function(dataset = 'adsl', input = '', count = 2, label = 'ready', dynamic = NULL) NULL",
    contract
  )
  expect_false(wrong_order$pass)
  expect_match(wrong_order$errors, "order")

  wrong_default <- validate_macro_contract(
    "beta <- function(input = '', dataset = 'other', count = 2, label = 'ready', dynamic = NULL) NULL",
    contract
  )
  expect_false(wrong_default$pass)
  expect_match(wrong_default$errors, "dataset.*default")

  sentinel <- withr::local_tempfile()
  never_run <- sprintf(
    "beta <- function(input = '', dataset = 'adsl', count = 2, label = 'ready', dynamic = NULL) { writeLines('executed', %s) }",
    shQuote(sentinel)
  )
  result <- validate_macro_contract(never_run, contract)
  expect_true(result$pass)
  expect_false(file.exists(sentinel))
})

test_that("macro contract validation reports assignment and parse failures", {
  contract <- parse_macro_contract("beta", "value=1")

  missing <- validate_macro_contract("other <- function(value = 1) NULL", contract)
  expect_false(missing$pass)
  expect_match(missing$errors, "exactly one top-level")

  duplicate <- validate_macro_contract(
    "beta <- function(value = 1) NULL\nbeta = function(value = 1) NULL", contract
  )
  expect_false(duplicate$pass)
  expect_match(duplicate$errors, "exactly one top-level")

  malformed <- validate_macro_contract("beta <- function(value = ) NULL", contract)
  expect_false(malformed$pass)
  expect_match(malformed$errors, "parse")
})
