test_that("macro definitions capture name and params", {
  u <- sas_units(sas_statements("%macro derive_flag(ds=, var=);\ndata x; run;\n%mend;"))
  defs <- extract_macro_defs(u)
  expect_identical(defs$name, "derive_flag")
  expect_identical(defs$params, "ds=, var=")
  expect_identical(defs$line_start, 1L)
  expect_identical(defs$line_end, 3L)
})

test_that("macro calls exclude macro-language builtins", {
  u <- sas_units(sas_statements("%let x=1;\n%derive_flag(ds=adsl)\n%if &x %then %put hi;"))
  calls <- extract_macro_calls(u)
  expect_identical(unique(calls$name), "derive_flag")
})

test_that("parameterless macro def", {
  u <- sas_units(sas_statements("%macro setup;\noptions nodate;\n%mend setup;"))
  defs <- extract_macro_defs(u)
  expect_identical(defs$name, "setup")
  expect_identical(defs$params, "")
})

test_that("empty units return empty tibbles with correct schemas", {
  u_empty <- sas_units(sas_statements(""))[0, ]
  defs <- extract_macro_defs(u_empty)
  expect_identical(nrow(defs), 0L)
  expect_identical(names(defs), c("name", "params", "line_start", "line_end", "file", "unit_id"))

  calls <- extract_macro_calls(u_empty)
  expect_identical(nrow(calls), 0L)
  expect_identical(names(calls), c("call_id", "component_id", "source_file", "line", "column", "name", "call_text"))
})

test_that("multiple macro defs and calls across units", {
  code <- paste(
    "%macro mac1(a);",
    "  %put a=&a;",
    "%mend;",
    "",
    "%macro mac2(b=1, c=2);",
    "  data _null_; run;",
    "%mend;",
    "",
    "%mac1(10);",
    "%mac2(b=2);",
    "%mac1(20);",
    sep = "\n"
  )
  u <- sas_units(sas_statements(code))
  defs <- extract_macro_defs(u)
  expect_identical(nrow(defs), 2L)
  expect_identical(defs$name, c("mac1", "mac2"))
  expect_identical(defs$params, c("a", "b=1, c=2"))

  calls <- extract_macro_calls(u)
  expect_identical(calls$name, c("mac1", "mac2", "mac1"))
})

test_that("project definitions shadow search-path files", {
  dir <- withr::local_tempdir()
  defs <- tibble::tibble(name = "m1", params = "", line_start = 1L, line_end = 3L)
  calls <- tibble::tibble(name = "m1", line = 10L)
  mdir <- file.path(dir, "macros"); dir.create(mdir)
  writeLines("%macro m1; %mend;", file.path(mdir, "m1.sas"))
  cfg <- structure(list(libraries = list(), macro_search_path = mdir,
                        source = NA_character_, raw = list()),
                   class = "sas2r_config")
  res <- resolve_macro_calls(calls, defs, cfg, project_dir = dir)
  expect_identical(res$status, "resolved_project")
})

test_that("first search-path match wins; later matches recorded as shadowed", {
  dir <- withr::local_tempdir()
  local_dir <- file.path(dir, "local"); org_dir <- file.path(dir, "org")
  dir.create(local_dir); dir.create(org_dir)
  writeLines("%macro m2; %mend;", file.path(local_dir, "m2.sas"))
  writeLines("%macro m2; %mend;", file.path(org_dir, "m2.sas"))
  cfg <- structure(list(libraries = list(),
                        macro_search_path = c(local_dir, org_dir),
                        source = NA_character_, raw = list()),
                   class = "sas2r_config")
  res <- resolve_macro_calls(tibble::tibble(name = "m2", line = 1L),
                             tibble::tibble(name = character(), params = character(),
                                            line_start = integer(), line_end = integer()),
                             cfg, project_dir = dir)
  expect_identical(res$status, "resolved_path")
  expect_identical(res$source, file.path(local_dir, "m2.sas"))
  expect_identical(res$n_matches, 2L)
  expect_match(res$shadowed, "org")
})

test_that("unresolved calls are reported, not dropped", {
  cfg <- structure(list(libraries = list(), macro_search_path = character(),
                        source = NA_character_, raw = list()),
                   class = "sas2r_config")
  res <- resolve_macro_calls(tibble::tibble(name = "ghost", line = 5L),
                             tibble::tibble(name = character(), params = character(),
                                            line_start = integer(), line_end = integer()),
                             cfg)
  expect_identical(res$status, "unresolved")
})

test_that("macro calls inside single quotes are ignored, double quotes are kept", {
  u1 <- sas_units(sas_statements("title 'Results for %sub group';"))
  calls1 <- extract_macro_calls(u1)
  expect_identical(nrow(calls1), 0L)

  u2 <- sas_units(sas_statements("title \"Results for %my_macro group\";"))
  calls2 <- extract_macro_calls(u2)
  expect_identical(calls2$name, "my_macro")

  # Apostrophe inside double-quoted string does not confuse single-quote stripper
  u3 <- sas_units(sas_statements("put \"don't\" '%m';"))
  calls3 <- extract_macro_calls(u3)
  expect_identical(nrow(calls3), 0L)

  u4 <- sas_units(sas_statements("title \"Bob's %rpt\" 'x y';"))
  calls4 <- extract_macro_calls(u4)
  expect_identical(calls4$name, "rpt")
})


test_that("q-family and autocall macro functions are excluded from user calls", {
  u <- sas_units(sas_statements("%let x = %qtrim(&val); %let y = %qleft(&x); %let z = %sysprod;"))
  calls <- extract_macro_calls(u)
  expect_identical(nrow(calls), 0L)
})


