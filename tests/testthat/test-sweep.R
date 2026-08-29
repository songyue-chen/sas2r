test_that("var-vs-call comparisons dispatch through chr_cmp", {
  w <- wrap_missing("code == substr(raw, 1, 3)", c("code", "raw"))
  expect_match(w, "chr_cmp\\(code, substr\\(raw, 1, 3\\), \"==\"\\)")
})

test_that("chr_cmp trims padded character results like SAS", {
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  expect_true(e$chr_cmp("AB", "AB  ", "=="))     # padding-insensitive
  expect_false(e$chr_cmp(NA_character_, "A", "=="))
  expect_true(e$chr_cmp(NA_character_, "A", "<"))  # missing sorts low
  expect_true(e$chr_cmp(2, 1, ">"))                # numeric passthrough

  # blank-as-missing parity (Finding 9)
  expect_false(e$chr_cmp(NA_character_, "", "!=")) # missing != missing is FALSE
  expect_false(e$chr_cmp("", "", "!="))
  expect_true(e$chr_cmp("A", "", "!="))
  expect_true(e$chr_cmp(NA_character_, "", "=="))
  expect_true(e$chr_cmp("", "", "=="))
  expect_false(e$chr_cmp(NA_character_, "", "<"))
  expect_true(e$chr_cmp(NA_character_, "", "<="))

  # numeric missing truth table
  expect_true(e$chr_cmp(NA_real_, 5, "<"))
  expect_true(e$chr_cmp(NA_real_, 5, "<="))
  expect_false(e$chr_cmp(5, NA_real_, "<"))
  expect_false(e$chr_cmp(5, NA_real_, "<="))
  expect_true(e$chr_cmp(5, NA_real_, ">"))
  expect_true(e$chr_cmp(NA_real_, NA_real_, "=="))
  expect_false(e$chr_cmp(NA_real_, NA_real_, "!="))
})

test_that("a CLASS variable named missing is not an option", {
  u <- sas_units(sas_statements(
    "proc means data=w.a nway noprint; class missing; var x; output out=w.o n=n; run;"))
  em <- emit_proc_means(u[u$unit_type == "proc_step", ])
  expect_false(is.na(em$code))
  expect_match(em$code, "group_by\\(missing\\)")
})

test_that("CLASS options other than missing are rejected into stubs", {
  u <- sas_units(sas_statements(
    "proc means data=w.a nway noprint; class sex / order=freq; var x; output out=w.o n=n; run;"))
  em <- emit_proc_means(u[u$unit_type == "proc_step", ])
  expect_true(is.na(em$code))
  expect_identical(em$flags, "means_not_t1")

  # / missing option is accepted
  u2 <- sas_units(sas_statements(
    "proc means data=w.a nway noprint; class sex / missing; var x; output out=w.o n=n; run;"))
  em2 <- emit_proc_means(u2[u2$unit_type == "proc_step", ])
  expect_false(is.na(em2$code))
})

test_that("chr_cmp strips trailing whitespace (including newlines and tabs)", {
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  val_with_newline <- "A\n"
  expect_true(e$chr_cmp(val_with_newline, "A", "=="))

  val_with_tab <- "A\t  "
  expect_true(e$chr_cmp(val_with_tab, "A", "=="))
})
