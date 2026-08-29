test_that("not in translates to %notin% c(...)", {
  expect_identical(tidy_expr(translate_expr("visit not in (1, 2)")),
                   "visit %notin% c(1, 2)")
})

test_that("%notin% matches SAS missing semantics", {
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  expect_true(e$`%notin%`(NA, c(1, 2)))
  expect_false(e$`%notin%`(2, c(1, 2)))
})

test_that("a not-in data step is now T1, not a stub", {
  u <- sas_units(sas_statements(
    "data x; set w.t; if visit not in (1, 2) then delete; run;"))
  ir <- parse_data_step(u[u$unit_type == "data_step", ])
  expect_identical(nrow(ir$blockers), 0L)
  expect_match(emit_data_step(ir)$code, "%notin% c\\(1, 2\\)")
})
