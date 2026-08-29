fixture_ir <- function() parse_data_step({
  u <- sas_units(sas_statements(
    "data out; set adam.adsl; where saffl = 'Y';\nbmi = wt / 2;\nif age ge 65 then agegr = 'E';\nif bad lt 0 then delete;\nkeep usubjid bmi agegr; run;"))
  u[u$unit_type == "data_step", ]
})

test_that("emitted code is complete, ordered, and parseable", {
  em <- emit_data_step(fixture_ir(), src_file = "01.sas")
  expect_no_error(parse(text = em$code))
  expect_match(em$code, 'lib_read("adam", "adsl")', fixed = TRUE)
  expect_match(em$code, 'dplyr::filter(chr_cmp(', fixed = TRUE)
  expect_match(em$code, '"Y", "==")', fixed = TRUE)
  expect_match(em$code, "mutate(bmi = wt / 2)", fixed = TRUE)
  expect_match(em$code, "filter\\(!\\(\\(is.na\\(bad\\) \\| bad < 0\\)\\)\\)")
  expect_match(em$code, "select\\(usubjid, bmi, agegr\\)")
  expect_match(em$code, "# sas L1")
  expect_type(em$stmt_map, "integer")
  expect_type(em$flags, "character")
})

test_that("emitted code executes with SAS semantics", {
  skip_if_not_installed("dplyr")
  em <- emit_data_step(fixture_ir())
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(work = list(read_path = withr::local_tempdir(),
                                        write_path = withr::local_tempdir(),
                                        engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) tibble::tibble(
    usubjid = c("A", "B", "C", "D"),
    saffl = c("Y", "Y", "N", "Y"),
    wt = c(80, 60, 70, 90),
    age = c(70, 40, 50, NA),
    bad = c(1, NA, 1, 2))
  eval(parse(text = em$code), envir = e)
  out <- get("out", envir = e)
  # C dropped by where; B dropped by if_delete (bad is NA -> SAS lt includes missing)
  expect_identical(out$usubjid, c("A", "D"))
  expect_identical(out$agegr, c("E", NA))   # age NA fails ge 65 in SAS too
  expect_identical(names(out), c("usubjid", "bmi", "agegr"))
})

test_that("if_assign on an existing variable preserves prior values", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements(
      "data x; set w.t; flag = 'N'; if age ge 65 then flag = 'Y'; run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_data_step(ir)
  expect_match(em$code, "mutate\\(flag = sas_if_else\\(\\(!is.na\\(age\\) & age >= 65\\), 'Y', flag\\)\\)")
})

test_that("non-work output emits lib_write", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements("data adam.new; set w.t; run;"))
    u[u$unit_type == "data_step", ]
  })
  expect_match(emit_data_step(ir)$code, 'lib_write\\(new, "adam", "new"\\)')
})

test_that("drop and rename statements emit properly", {
  ir <- parse_data_step({
    u <- sas_units(sas_statements(
      "data out; set in.ds; drop bad_var other_var; rename old_id = new_id; run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_data_step(ir)
  expect_match(em$code, "select\\(-c\\(bad_var, other_var\\)\\)")
  expect_match(em$code, "rename\\(new_id = old_id\\)")
})

test_that("emit_data_step stops on invalid ir route or blockers", {
  # invalid route
  ir_invalid <- structure(list(route = "merge", blockers = tibble::tibble(stmt_id = integer(), reason = character())),
                          class = "sas2r_ir")
  expect_error(emit_data_step(ir_invalid))

  # with blockers
  ir_blocked <- structure(list(route = "datastep", blockers = tibble::tibble(stmt_id = 1L, reason = "retain")),
                          class = "sas2r_ir")
  expect_error(emit_data_step(ir_blocked))
})

test_that("sas_cond_to_r and split_ds helper functions work", {
  expect_identical(sas_cond_to_r("x = 1"), "(!is.na(x) & x == 1)")
  # R2-1: In-list condition does not drop missing-value wrapping on sibling comparisons
  expect_identical(sas_cond_to_r("visit in (1, 2) and age ge 65"),
                   "visit %in% c(1, 2) & (!is.na(age) & age >= 65)")
  expect_identical(split_ds("work.test"), c(lib = "work", member = "test"))
  expect_identical(split_ds("test"), c(lib = "work", member = "test"))
})

test_that("if_assign preserves existing input columns across non-matching rows", {
  # R2-4: input column retention
  ir <- parse_data_step({
    u <- sas_units(sas_statements("data out; set in.ds; if age >= 65 then agegr = 'E'; run;"))
    u[u$unit_type == "data_step", ]
  })
  em <- emit_data_step(ir)
  expect_match(em$code, "if \\(\"agegr\" %in% names\\(dplyr::pick\\(dplyr::everything\\(\\)\\)\\)\\)")

  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(work = list(read_path = withr::local_tempdir(),
                                        write_path = withr::local_tempdir(),
                                        engine = "rds", write = "rds"))

  # Case 1: agegr already exists on input
  e$lib_read <- function(libref, member) tibble::tibble(age = c(70, 40), agegr = c("OLD", "YOUNG"))
  eval(parse(text = em$code), envir = e)
  out1 <- get("out", envir = e)
  expect_identical(out1$agegr, c("E", "YOUNG"))


  # Case 2: agegr does not exist on input
  e$lib_read <- function(libref, member) tibble::tibble(age = c(70, 40))
  eval(parse(text = em$code), envir = e)
  out2 <- get("out", envir = e)
  expect_identical(out2$agegr, c("E", NA))
})

test_that("data _null_ steps are blocked as null_step_deferred", {
  # R2-3: data _null_
  ir <- parse_data_step({
    u <- sas_units(sas_statements("data _null_; set work.x; y = 1; run;"))
    u[u$unit_type == "data_step", ]
  })
  expect_identical(ir$blockers$reason, "null_step_deferred")
})

test_that("T1 emission is case-insensitive about variable names, like SAS", {
  skip_if_not_installed("dplyr")
  # SAS resolves names case-insensitively: lowercase source against an
  # uppercase dataset is the everyday clinical mix and must execute.
  u <- sas_units(sas_statements("data adam.out1; set adam.adsl; aval_double = aval * 2; run;"))
  ir <- parse_data_step(u[u$unit_type == "data_step", ])
  em <- emit_data_step(ir)
  expect_match(em$code, "sas2r_fold_names")

  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    adam = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  e$lib_read <- function(...) tibble::tibble(USUBJID = c("01", "02"), AVAL = c(10, 20))
  eval(parse(text = em$code), envir = e)
  out <- get("out1", envir = e)
  expect_identical(out$aval_double, c(20, 40))
})
