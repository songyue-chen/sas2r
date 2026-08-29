merge_ir <- function(iftxt = "if ina and not inb;") parse_data_step({
  u <- sas_units(sas_statements(sprintf(
    "data m; merge a (in=ina) b (in=inb); by k; %s run;", iftxt)))
  u[u$unit_type == "data_step", ]
})

test_that("in-flag patterns map to keep modes", {
  expect_match(emit_merge_step(merge_ir("if ina;"))$code, 'keep = "left"')
  expect_match(emit_merge_step(merge_ir("if inb;"))$code, 'keep = "right"')
  expect_match(emit_merge_step(merge_ir("if ina and inb;"))$code, 'keep = "both"')
  expect_match(emit_merge_step(merge_ir("if inb and ina;"))$code, 'keep = "both"')
  expect_match(emit_merge_step(merge_ir("if ina and not inb;"))$code,
               'keep = "left_only"')
  expect_match(emit_merge_step(merge_ir("if inb and not ina;"))$code,
               'keep = "right_only"')
  expect_match(emit_merge_step(merge_ir(""))$code, 'keep = "full"')
})

test_that("cardinality flag always present; code executes", {
  skip_if_not_installed("dplyr")
  em <- emit_merge_step(merge_ir("if ina;"))
  expect_true("merge_cardinality_unproven" %in% em$flags)
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(work = list(path = withr::local_tempdir(),
                                        engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) {
    if (member == "a") tibble::tibble(k = c(1, 2), v = c("a1", "a2"))
    else tibble::tibble(k = 2, w = "b2")
  }
  eval(parse(text = em$code), envir = e)
  m <- get("m", envir = e)
  expect_identical(nrow(m), 2L)
  expect_identical(m$w[m$k == 2], "b2")
})

test_that("three-way merge is not T1", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements(
      "data m; merge a (in=x) b (in=y) c (in=z); by k; run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_merge_step(ir)
  expect_true(is.na(em$code))
  expect_true("merge_unsupported_shape" %in% em$flags)
})

test_that("merge without by statement is not T1", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements(
      "data m; merge a (in=ina) b (in=inb); run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_merge_step(ir)
  expect_true(is.na(em$code))
  expect_true("merge_unsupported_shape" %in% em$flags)
})

test_that("merge with unsupported if filter is not T1", {
  em <- emit_merge_step(merge_ir("if ina or inb;"))
  expect_true(is.na(em$code))
  expect_true("merge_unsupported_shape" %in% em$flags)
})

test_that("merge with non-work libraries and multi-key by emits properly", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements(
      "data adam.out; merge sdtm.dm (in=indm) sdtm.vs (in=invs); by studyid usubjid; if indm and not invs; run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_merge_step(ir)
  expect_match(em$code, 'out <- sas_merge\\(sas2r_fold_names\\(lib_read\\("sdtm", "dm"\\)\\), sas2r_fold_names\\(lib_read\\("sdtm", "vs"\\)\\), by = c\\("studyid", "usubjid"\\), keep = "left_only"\\)')
  expect_match(em$code, 'lib_write\\(out, "adam", "out"\\)')
  expect_true("merge_cardinality_unproven" %in% em$flags)
  expect_type(em$stmt_map, "integer")
})

test_that("merge without in-flags and no filter emits keep full", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements(
      "data m; merge a b; by k; run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_merge_step(ir)
  expect_match(em$code, 'keep = "full"')
  expect_true("merge_cardinality_unproven" %in% em$flags)
})
