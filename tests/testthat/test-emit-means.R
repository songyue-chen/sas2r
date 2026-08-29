proc_unit_means <- function(src) {
  u <- sas_units(sas_statements(src))
  u[u$unit_type == "proc_step", ]
}

means_unit <- function() {
  proc_unit_means(
    "proc means data=w.adsl nway noprint; class trt; var aval; output out=w.stats n=n mean=m std=s; run;")
}

test_that("means emits grouped summarise with SAS NA semantics", {
  em <- emit_proc_means(means_unit())
  expect_no_error(parse(text = em$code))
  expect_match(em$code, "dplyr::group_by\\(trt\\)")
  expect_match(em$code, "n = sum\\(!is.na\\(aval\\)\\)")
  expect_match(em$code, "m = if \\(all\\(is.na\\(aval\\)\\)\\) NA_real_ else mean\\(aval, na.rm = TRUE\\)")
  expect_true("means_no_type" %in% em$flags)
})

test_that("known answers: hand-computed with missings", {
  skip_if_not_installed("dplyr")
  em <- emit_proc_means(means_unit())
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    w = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  input <- tibble::tibble(trt = c("A", "A", "A", "B", "B"),
                          aval = c(1, 3, NA, 5, NA))
  e$lib_read <- function(...) input
  eval(parse(text = em$code), envir = e)
  s <- get("stats", envir = e)
  a <- s[s$trt == "A", ]
  expect_identical(a$n, 2L)          # SAS n counts non-missing
  expect_identical(a$m, 2)           # mean(1, 3)
  expect_equal(a$s, sd(c(1, 3)))     # std over non-missing
  b <- s[s$trt == "B", ]
  expect_true(is.na(b$s))            # single non-missing -> std missing in SAS
  expect_identical(b$n, 1L)
})

test_that("oracle agrees with emission on the known-answer input", {
  skip_if_not_installed("dplyr")
  input <- tibble::tibble(trt = c("A", "A", "B"), aval = c(1, NA, 4))
  o <- means_oracle(input, class = "trt", var = "aval",
                    stats = c(n = "n", m = "mean"))
  expect_identical(o$n, c(1L, 1L))
  expect_identical(o$m, c(1, 4))
})

test_that("unsupported shapes reject cleanly", {
  u <- sas_units(sas_statements("proc means data=w.a; var x; run;"))
  em <- emit_proc_means(u[u$unit_type == "proc_step", ])
  expect_true(is.na(em$code))
  expect_identical(em$flags, "means_not_t1")

  # multiple class vars
  u_cls <- proc_unit_means("proc means data=w.a nway noprint; class c1 c2; var x; output out=w.b mean=m; run;")
  em_cls <- emit_proc_means(u_cls)
  expect_true(is.na(em_cls$code))
  expect_identical(em_cls$flags, "means_not_t1")

  # multiple var vars
  u_var <- proc_unit_means("proc means data=w.a noprint; var x1 x2; output out=w.b mean=m; run;")
  em_var <- emit_proc_means(u_var)
  expect_true(is.na(em_var$code))
  expect_identical(em_var$flags, "means_not_t1")

  # unsupported statement e.g. weight
  u_wt <- proc_unit_means("proc means data=w.a noprint; var x; weight w; output out=w.b mean=m; run;")
  em_wt <- emit_proc_means(u_wt)
  expect_true(is.na(em_wt$code))
  expect_identical(em_wt$flags, "means_not_t1")

  # class without nway defers
  u_non_nway <- proc_unit_means("proc means data=w.a noprint; class trt; var x; output out=w.b mean=m; run;")
  em_non_nway <- emit_proc_means(u_non_nway)
  expect_true(is.na(em_non_nway$code))
  expect_identical(em_non_nway$flags, "means_class_requires_nway")
})

test_that("proc means without class statement emits ungrouped summarise", {
  u <- proc_unit_means(
    "proc means data=w.inp noprint; var val; output out=w.out n=cnt mean=avg sum=tot min=lo max=hi median=med std=stdev; run;")
  em <- emit_proc_means(u)
  expect_false(grepl("group_by", em$code))
  expect_match(em$code, 'out <- lib_read\\("w", "inp"\\)')
  expect_match(em$code, 'cnt = sum\\(!is.na\\(val\\)\\)')
  expect_match(em$code, 'avg = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else mean\\(val, na.rm = TRUE\\)')
  expect_match(em$code, 'tot = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else sum\\(val, na.rm = TRUE\\)')
  expect_match(em$code, 'lo = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else min\\(val, na.rm = TRUE\\)')
  expect_match(em$code, 'med = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else stats::median\\(val, na.rm = TRUE\\)')
  expect_match(em$code, 'lib_write\\(out, "w", "out"\\)')

  # Evaluate execution on test data including all NA
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    w = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  input <- tibble::tibble(val = c(10, 20, 30, NA))
  e$lib_read <- function(...) input
  eval(parse(text = em$code), envir = e)
  res <- get("out", envir = e)
  expect_identical(res$cnt, 3L)
  expect_identical(res$avg, 20)
  expect_identical(res$tot, 60)
  expect_identical(res$lo, 10)
  expect_identical(res$hi, 30)
  expect_identical(res$med, 20)
  expect_equal(res$stdev, stats::sd(c(10, 20, 30)))
  expect_identical(res$`_freq_`, 4L)
})

test_that("transpile integrates proc means via rulebook", {
  dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  sas_code <- "libname adam 'path';\nproc means data=adam.adsl nway noprint; class trt01p; var age; output out=adam.summary mean=mean_age n=n_age; run;"
  writeLines(sas_code, file.path(dir, "prog.sas"))
  proj <- sas_project(dir)
  tr <- sas_transpile(proj, out_dir)
  expect_identical(sum(tr$manifest$tier == "t1"), 2L) # libname + proc means
  out_r <- file.path(out_dir, "prog.R")
  expect_true(file.exists(out_r))
  code <- paste(readLines(out_r), collapse = "\n")
  expect_match(code, "dplyr::summarise")
  expect_match(code, "mean_age = if \\(all\\(is.na\\(age\\)\\)\\) NA_real_ else mean\\(age, na.rm = TRUE\\)")
})

test_that("proc means excludes NA/blank class observations by default", {
  skip_if_not_installed("dplyr")
  u <- proc_unit_means("proc means data=w.inp nway noprint; class trt; var aval; output out=w.out mean=m; run;")
  em <- emit_proc_means(u)
  expect_match(em$code, "dplyr::filter\\(!is.na\\(trt\\)")

  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(w = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  input <- tibble::tibble(trt = c("A", "A", NA, ""), aval = c(10, 20, 100, 200))
  e$lib_read <- function(...) input
  eval(parse(text = em$code), envir = e)
  res <- get("out", envir = e)
  expect_identical(nrow(res), 1L)
  expect_identical(res$trt, "A")
  expect_identical(res$m, 15)

  # Oracle matches emission
  o <- means_oracle(input, class = "trt", var = "aval", stats = c(m = "mean"))
  expect_identical(nrow(o), 1L)
  expect_identical(o$trt, "A")
  expect_identical(o$m, 15)
})

test_that("data=w.missing does not trigger missing option and empty oracle fallback is typed", {
  u <- proc_unit_means("proc means data=w.missing nway noprint; class trt; var aval; output out=w.out mean=m; run;")
  em <- emit_proc_means(u)
  expect_false(em$parse$missing)

  # Empty oracle fallback preserves character type
  all_na_input <- tibble::tibble(trt = c(NA_character_, ""), aval = c(10, 20))
  o_empty <- means_oracle(all_na_input, class = "trt", var = "aval", stats = c(m = "mean", n = "n"))
  expect_identical(nrow(o_empty), 0L)
  expect_type(o_empty$trt, "character")
  expect_type(o_empty$m, "double")
  expect_type(o_empty$n, "integer")
  expect_type(o_empty$`_freq_`, "integer")
})
