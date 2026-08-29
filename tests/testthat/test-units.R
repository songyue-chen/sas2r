units_of <- function(src) {
  u <- sas_units(sas_statements(src))
  unique(u[, c("unit_id", "unit_type")])
}

test_that("data step, proc step, and globals are separated", {
  agg <- units_of("libname adam '/d';\ndata w1; set adam.adsl; run;\nproc sort data=w1; by id; run;")
  expect_identical(agg$unit_type, c("global", "data_step", "proc_step"))
})

test_that("implicit boundary: new data step closes the previous one", {
  agg <- units_of("data a; x=1;\ndata b; y=2; run;")
  expect_identical(agg$unit_type, c("data_step", "data_step"))
})

test_that("macro definitions absorb their body, nesting included", {
  agg <- units_of("%macro outer; %macro inner; %mend inner; %mend outer; data z; run;")
  expect_identical(agg$unit_type, c("macro_def", "data_step"))
})

test_that("statements inside a macro body belong to the macro unit", {
  u <- sas_units(sas_statements("%macro m1(x);\n data inner; set &x; run;\n%mend;"))
  expect_true(all(u$unit_type == "macro_def"))
  expect_identical(length(unique(u$unit_id)), 1L)
})

test_that("stray run outside any step is a global unit", {
  agg <- units_of("run;")
  expect_identical(agg$unit_type, "global")
})

test_that("empty statements tibble returns empty tibble with unit columns", {
  u <- sas_units(sas_statements(""))
  expect_identical(nrow(u), 0L)
  expect_true("unit_id" %in% names(u))
  expect_true("unit_type" %in% names(u))
})

test_that("token lowercasing handles uppercase/mixed-case statement first tokens", {
  agg <- units_of("DATA w1; SET adam.adsl; RUN;\n%MACRO m; %MEND m;\nPROC SORT data=w1; RUN;")
  expect_identical(agg$unit_type, c("data_step", "macro_def", "proc_step"))
})

test_that("macro definition inside open data step restores enclosing step", {
  u <- sas_units(sas_statements("data a; x=1; %macro m; %put hi; %mend; y=2; run;"))
  expect_identical(u$unit_type, c("data_step", "data_step", "macro_def", "macro_def", "macro_def", "data_step", "data_step"))
  # data step statements share the same unit_id
  expect_identical(u$unit_id[1], u$unit_id[6])
})


test_that("run; quit; idiom does not create a spurious extra global unit", {
  u <- sas_units(sas_statements("proc datasets lib=work; delete x; run; quit;"))
  expect_identical(unique(u$unit_type), "proc_step")
  expect_identical(length(unique(u$unit_id)), 1L)
})

test_that("datalines payload containing proc or macro keywords does not open false units", {
  u1 <- sas_units(sas_statements("data a;\ninput w $ x;\ndatalines;\nproc 1\n;\nrun;\nproc print data=a; run;"))
  expect_identical(u1$unit_type[u1$type == "datalines_data"], "data_step")
  expect_identical(unique(u1$unit_type), c("data_step", "proc_step"))

  u2 <- sas_units(sas_statements("data a;\ndatalines;\n%macro x\n;\nrun;\nproc print data=a; run;"))
  expect_identical(u2$unit_type[u2$type == "datalines_data"], "data_step")
  expect_identical(unique(u2$unit_type), c("data_step", "proc_step"))
})



