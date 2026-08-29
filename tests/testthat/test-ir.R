unit1 <- function(src) {
  u <- sas_units(sas_statements(src))
  u[u$unit_id == u$unit_id[u$first_token == "data"][1], ]
}

test_that("clean row-wise step parses fully", {
  ir <- parse_data_step(unit1(
    "data out; set adam.adsl; where saffl = 'Y'; bmi = wt / 2;
     if age ge 65 then agegr = 'E'; if bad lt 0 then delete;
     keep usubjid bmi agegr; run;"))
  expect_identical(ir$route, "datastep")
  expect_identical(ir$inputs, "adam.adsl")
  expect_identical(ir$outputs, "work.out")
  expect_identical(vapply(ir$steps, `[[`, "", "kind"),
    c("where", "assign", "if_assign", "if_delete", "keep"))
  expect_identical(nrow(ir$blockers), 0L)
})

test_that("merge route captures in= flags and by", {
  ir <- parse_data_step(unit1(
    "data m; merge a (in=ina) b (in=inb); by k; if ina and not inb; run;"))
  expect_identical(ir$route, "merge")
  expect_identical(ir$by, "k")
  expect_identical(ir$in_flags, c(a = "ina", b = "inb"))
  expect_identical(vapply(ir$steps, `[[`, "", "kind"), "merge_filter")
})

test_that("blockers: retain, macro residue, first-dot", {
  expect_identical(parse_data_step(unit1(
    "data x; set y; retain c 0; run;"))$blockers$reason, "retain")
  expect_identical(parse_data_step(unit1(
    "data x; set y; v = &mv; run;"))$blockers$reason, "macro_residue")
  expect_identical(parse_data_step(unit1(
    "data x; set y; by k; if first.k then n = 1; run;"))$blockers$reason[1],
    "by_group_logic")
})

test_that("unknown statements are blockers, never dropped", {
  ir <- parse_data_step(unit1("data x; set y; array a{3} v1-v3; run;"))
  expect_match(ir$blockers$reason, "unsupported_statement:array")
})

test_that("drop and rename steps parse correctly", {
  ir <- parse_data_step(unit1(
    "data x; set y; drop a b; rename c = d e = f; run;"))
  expect_identical(vapply(ir$steps, `[[`, "", "kind"), c("drop", "rename"))
  expect_identical(ir$steps[[1]]$vars, c("a", "b"))
  expect_identical(ir$steps[[2]]$pairs,
                   matrix(c("c", "e", "d", "f"), nrow = 2,
                          dimnames = list(NULL, c("old", "new"))))
})

test_that("blockers for multi_set, output, update, and if_subsetting", {
  expect_identical(parse_data_step(unit1(
    "data x; set a b; run;"))$blockers$reason, "multi_set")
  expect_identical(parse_data_step(unit1(
    "data x; set y; output; run;"))$blockers$reason, "output_statement")
  expect_identical(parse_data_step(unit1(
    "data x; update a b; by k; run;"))$blockers$reason, "update_statement")
  expect_identical(parse_data_step(unit1(
    "data x; set y; if age > 65; run;"))$blockers$reason, "unsupported_statement:if_subsetting")
  expect_identical(parse_data_step(unit1(
    "data x; set y; if age > 65 then put 'old'; run;"))$blockers$reason,
    "unsupported_statement:if_then_action")
})

test_that("macro residue in single quotes does not block, but double quotes block", {
  # Single quotes: literal in SAS, not resolved -> no blocker
  ir_single <- parse_data_step(unit1(
    "data x; set y; msg = '100% pure & clean'; run;"))
  expect_identical(nrow(ir_single$blockers), 0L)

  # Double quotes: literal % and & without variable names do NOT block (R2-6)
  ir_double_literal <- parse_data_step(unit1(
    "data x; set y; msg = \"100% pure & clean\"; run;"))
  expect_identical(nrow(ir_double_literal$blockers), 0L)

  # Double quotes: macro variable &trt resolved in SAS -> blocker (F10)
  ir_double <- parse_data_step(unit1(
    "data x; set y; t = \"&trt\"; run;"))
  expect_identical(ir_double$blockers$reason, "macro_residue")
})

test_that("SET-less data step triggers no_input_dataset blocker", {
  ir_noset <- parse_data_step(unit1("data a; x = 1; run;"))
  expect_identical(ir_noset$blockers$reason, "no_input_dataset")
})

test_that("data _null_ produces empty outputs and null_step_deferred blocker", {
  ir <- parse_data_step(unit1("data _null_; set y; run;"))
  expect_identical(ir$outputs, character(0))
  expect_identical(ir$inputs, "work.y")
  expect_identical(ir$blockers$reason, "null_step_deferred")
})

test_that("dataset options on data and set statements block instead of silently vanishing", {
  # set adsl(where=(age>18)) previously emitted with the WHERE simply gone.
  ir <- parse_data_step(unit1(
    "data out; set adam.adsl(where=(age > 18)); run;"))
  expect_true("dataset_options" %in% ir$blockers$reason)

  ir2 <- parse_data_step(unit1(
    "data out(keep=usubjid age); set adam.adsl; run;"))
  expect_true("dataset_options" %in% ir2$blockers$reason)

  ir3 <- parse_data_step(unit1(
    "data out; set adam.adsl(keep=usubjid age rename=(age=age2)); run;"))
  expect_true("dataset_options" %in% ir3$blockers$reason)
})

test_that("merge dataset options beyond bare in= block instead of corrupting inputs", {
  ir <- parse_data_step(unit1(
    "data m; merge a (in=ina) b (in=inb keep=k x); by k; if ina; run;"))
  expect_true("merge_dataset_options" %in% ir$blockers$reason)
  # The garbled option tokens must not have been read as input datasets.
  expect_false(any(c("work.keep", "work.x", "work.in", "work.inb") %in% ir$inputs))

  # A bare in= flag keeps working.
  ir_ok <- parse_data_step(unit1(
    "data m; merge a (in=ina) b (in=inb); by k; if ina and not inb; run;"))
  expect_false(any(grepl("dataset_options", ir_ok$blockers$reason)))
})
