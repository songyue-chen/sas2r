test_that("every executable rule has a semantic disposition", {
  rb <- load_rulebook()
  cov <- semantic_coverage(rb, load_semantic_registry())
  expect_true(cov$ok)
  expect_length(cov$missing, 0L)
  expect_length(cov$unexpected, 0L)
  expect_length(cov$strategy_mismatches, 0L)
  expect_length(cov$invalid_dispositions, 0L)
  expect_setequal(
    cov$required,
    c(
      paste0("functions.", names(rb$functions)),
      paste0("operators.", names(rb$operators)),
      paste0("procs.", c("sort", "format", "sql", "means", "freq"))
    )
  )
})

test_that("coverage reports an executable rule removed from the evidence plane", {
  rb <- load_rulebook()
  reg <- load_semantic_registry()
  reg$rules$functions.abs <- NULL
  expect_identical(semantic_coverage(rb, reg)$missing, "functions.abs")
})

test_that("coverage reports dispatch strategy mismatches", {
  rb <- load_rulebook()
  reg <- load_semantic_registry()
  reg$rules$functions.abs$strategy <- "not_abs"
  reg$rules$procs.sql$strategy <- "not_emit_proc_sql"

  cov <- semantic_coverage(rb, reg)
  expect_setequal(
    cov$strategy_mismatches,
    c("functions.abs", "procs.sql")
  )
  expect_false(cov$ok)
})

test_that("coverage reports stale semantic rules", {
  rb <- load_rulebook()
  reg <- load_semantic_registry()
  reg$rules$functions.stale <- reg$rules$functions.abs

  cov <- semantic_coverage(rb, reg)
  expect_identical(cov$unexpected, "functions.stale")
  expect_false(cov$ok)
})

test_that("coverage rejects deferred or unsupported executable rules", {
  rb <- load_rulebook()
  reg <- load_semantic_registry()
  reg$rules$functions.abs$classification <- "unsupported"
  reg$rules$functions.abs$implementation_status <- "deferred"

  cov <- semantic_coverage(rb, reg)
  expect_identical(cov$invalid_dispositions, "functions.abs")
  expect_false(cov$ok)
})

test_that("round records the SAS/R default difference and adapter", {
  s <- semantic_rule("functions.round")
  expect_identical(s$classification, "adapter_required")
  expect_identical(s$strategy, "sas_round")
  expect_identical(s$implementation_status, "implemented_unverified")
  expect_match(s$sas_default, "away from zero", ignore.case = TRUE)
  expect_match(s$r_default, "ties to even", ignore.case = TRUE)
  expect_true(any(grepl(
    "CAMIS/Comp/r-sas_rounding",
    vapply(s$sources, `[[`, "", "url")
  )))
})

test_that("unsupported statistical domains cannot masquerade as supported", {
  reg <- load_semantic_registry()
  expect_identical(reg$known_domains$cox_ties$implementation_status, "deferred")
  expect_identical(reg$known_domains$quantiles$implementation_status, "deferred")
  expect_true(all(vapply(
    reg$known_domains,
    function(x) identical(x$implementation_status, "deferred"),
    logical(1)
  )))
})

test_that("semantic registry validation rejects malformed source evidence", {
  reg <- load_semantic_registry()
  reg$rules$functions.abs$sources[[1]]$accessed <- "2026-02-30"
  expect_error(
    validate_semantic_registry(reg),
    "valid YYYY-MM-DD",
    class = "sas2r_semantic_registry_error"
  )

  reg <- load_semantic_registry()
  reg$rules$functions.abs$sources[[1]]$note <- "unreviewed"
  expect_error(
    validate_semantic_registry(reg),
    "unknown field",
    class = "sas2r_semantic_registry_error"
  )
})

test_that("semantic registry validation rejects schema drift", {
  reg <- load_semantic_registry()
  reg$schema_version <- 2L
  expect_error(
    validate_semantic_registry(reg),
    "schema_version",
    class = "sas2r_semantic_registry_error"
  )

  reg <- load_semantic_registry()
  reg$rules$functions.abs$implementation_status <- "verified_by_assumption"
  expect_error(
    validate_semantic_registry(reg),
    "implementation_status",
    class = "sas2r_semantic_registry_error"
  )
})

test_that("schema version 1 rejects verified status without fixture provenance", {
  reg <- load_semantic_registry()
  reg$rules$functions.abs$implementation_status <- "verified"
  expect_error(
    validate_semantic_registry(reg),
    "fixture provenance",
    class = "sas2r_semantic_registry_error"
  )
})

test_that("semantic registry rejects invalid and non-executable rule IDs", {
  reg <- load_semantic_registry()
  reg$rules[["functions.bad id"]] <- reg$rules$functions.abs
  expect_error(
    validate_semantic_registry(reg),
    "invalid rule id",
    class = "sas2r_semantic_registry_error"
  )

  reg <- load_semantic_registry()
  reg$rules$functions.not_in_dispatch <- reg$rules$functions.abs
  expect_error(
    validate_semantic_registry(reg),
    "executable surface",
    class = "sas2r_semantic_registry_error"
  )
})

test_that("semantic registry never evaluates YAML expressions", {
  dir <- withr::local_tempdir()
  marker <- file.path(dir, "yaml-expression-ran")
  registry_file <- file.path(dir, "semantics.yml")
  expression <- sprintf("file.create(%s)", deparse(marker))
  writeLines(c(
    sprintf("schema_version: !expr %s", deparse(expression)),
    "rules: {}",
    "known_domains: {}"
  ), registry_file)
  withr::local_options(list(yaml.eval.expr = TRUE))

  expect_error(
    load_semantic_registry(registry_file),
    class = "sas2r_semantic_registry_error"
  )
  expect_false(file.exists(marker))
})

test_that("semantic registry validation rejects duplicate mapping fields", {
  reg <- load_semantic_registry()
  reg$rules$functions.abs <- c(
    reg$rules$functions.abs,
    list(scope = "duplicate scope must not override reviewed scope")
  )
  expect_error(
    validate_semantic_registry(reg),
    "duplicate field",
    class = "sas2r_semantic_registry_error"
  )
})

test_that("semantic rulebook flags exercised rules without verification evidence", {
  src <- withr::local_tempfile(fileext = ".sas")
  writeLines(c(
    "data out; set input; y = round(x, 1); if y = 2 then z = 1; run;",
    "proc means data=out; var y; run;"
  ), src)

  p <- sas_project(src)
  procs <- c("means")
  ids <- sas2r:::semantic_exercised_rules(p, procs)
  expect_true(all(c("functions.round", "operators.=", "procs.means") %in% ids))
  usage <- sas2r:::semantic_usage_table(ids)
  expect_true(nrow(usage) >= 3L)
})

test_that("semantic expression rules ignore assignment and PROC option equals signs", {
  src <- withr::local_tempfile(fileext = ".sas")
  writeLines(c(
    "data out; set input; x=1; run;",
    "proc sort data=out out=sorted; by x; run;"
  ), src)

  p <- sas_project(src)
  ids <- sas2r:::semantic_exercised_rules(p, "sort")
  expect_false("operators.=" %in% ids)
})

test_that("semantic expression rules do not treat an identifier named in as an operator", {
  src <- withr::local_tempfile(fileext = ".sas")
  writeLines("data out; set input; result = in; run;", src)

  p <- sas_project(src)
  ids <- sas2r:::semantic_exercised_rules(p, character())
  expect_false("operators.in" %in% ids)
})

test_that("semantic expression rules recognize operators in parsed DATA-step expressions", {
  src <- withr::local_tempfile(fileext = ".sas")
  writeLines(
    "data out; set input; if x in (1, 2) then y=1; if y = 1 then z=2; run;",
    src
  )

  p <- sas_project(src)
  ids <- sas2r:::semantic_exercised_rules(p, character())
  expect_true(all(c("operators.in", "operators.=") %in% ids))
})

test_that("semantic expression rules recognize both operators in NOT IN expressions", {
  src <- withr::local_tempfile(fileext = ".sas")
  writeLines(
    "data out; set input; if x not in (1, 2) then y=1; run;",
    src
  )

  p <- sas_project(src)
  ids <- sas2r:::semantic_exercised_rules(p, character())
  expect_true(all(c("operators.not", "operators.in") %in% ids))
})
