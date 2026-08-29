freq_unit <- function(opts = "") {
  u <- sas_units(sas_statements(sprintf(
    "proc freq data=w.adsl noprint; tables trt / out=w.f%s; run;", opts)))
  u[u$unit_type == "proc_step", ]
}

test_that("known answers: missing excluded by default, included with /missing", {
  skip_if_not_installed("dplyr")
  input <- tibble::tibble(trt = c("A", "A", "B", NA))
  run <- function(em) {
    e <- new.env(parent = globalenv())
    sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
    e$.sas2r_registry <- list(w = list(path = withr::local_tempdir(),
                                       engine = "rds", write = "rds"))
    e$lib_read <- function(...) input
    eval(parse(text = em$code), envir = e)
    get("f", envir = e)
  }
  f1 <- run(emit_proc_freq(freq_unit()))
  expect_identical(nrow(f1), 2L)                 # NA level absent
  expect_identical(f1$count[f1$trt == "A"], 2L)
  expect_equal(f1$percent[f1$trt == "A"], 100 * 2 / 3)   # base excludes NA
  f2 <- run(emit_proc_freq(freq_unit(" missing")))
  expect_identical(nrow(f2), 3L)                 # NA level present
  expect_equal(f2$percent[is.na(f2$trt)], 25)
})

test_that("freq oracle agrees with hand counts", {
  o <- freq_oracle(tibble::tibble(x = c("A", "B", "B", NA)), "x", missing = FALSE)
  expect_identical(o$count, c(1L, 2L))
  expect_equal(o$percent, c(100 / 3, 200 / 3))
})

test_that("two-way tables defer", {
  u <- sas_units(sas_statements(
    "proc freq data=w.a noprint; tables x*y / out=w.f; run;"))
  em <- emit_proc_freq(u[u$unit_type == "proc_step", ])
  expect_true(is.na(em$code))
  expect_identical(em$flags, "freq_multiway_deferred")
})

test_that("unsupported shapes reject cleanly as freq_not_t1", {
  # missing noprint
  u1 <- sas_units(sas_statements("proc freq data=w.a; tables x / out=w.f; run;"))
  em1 <- emit_proc_freq(u1[u1$unit_type == "proc_step", ])
  expect_true(is.na(em1$code))
  expect_identical(em1$flags, "freq_not_t1")

  # missing out=
  u2 <- sas_units(sas_statements("proc freq data=w.a noprint; tables x; run;"))
  em2 <- emit_proc_freq(u2[u2$unit_type == "proc_step", ])
  expect_true(is.na(em2$code))
  expect_identical(em2$flags, "freq_not_t1")

  # missing data=
  u3 <- sas_units(sas_statements("proc freq noprint; tables x / out=w.f; run;"))
  em3 <- emit_proc_freq(u3[u3$unit_type == "proc_step", ])
  expect_true(is.na(em3$code))
  expect_identical(em3$flags, "freq_not_t1")

  # multiple tables statements
  u4 <- sas_units(sas_statements("proc freq data=w.a noprint; tables x / out=w.f; tables y / out=w.g; run;"))
  em4 <- emit_proc_freq(u4[u4$unit_type == "proc_step", ])
  expect_true(is.na(em4$code))
  expect_identical(em4$flags, "freq_not_t1")

  # unsupported statement in proc freq (e.g. weight)
  u5 <- sas_units(sas_statements("proc freq data=w.a noprint; tables x / out=w.f; weight w; run;"))
  em5 <- emit_proc_freq(u5[u5$unit_type == "proc_step", ])
  expect_true(is.na(em5$code))
  expect_identical(em5$flags, "freq_not_t1")
})

test_that("freq oracle with missing = TRUE includes NA in count and percent", {
  o <- freq_oracle(tibble::tibble(x = c("A", "B", "B", NA)), "x", missing = TRUE)
  expect_identical(o$count, c(1L, 2L, 1L))
  expect_equal(o$percent, c(25, 50, 25))
})

test_that("transpile integrates proc freq via rulebook", {
  dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  sas_code <- "libname adam 'path';\nproc freq data=adam.adsl noprint; tables trt01p / out=adam.freq_trt missing; run;"
  writeLines(sas_code, file.path(dir, "prog.sas"))
  proj <- sas_project(dir)
  tr <- sas_transpile(proj, out_dir)
  expect_identical(sum(tr$manifest$tier == "t1"), 2L) # libname + proc freq
  out_r <- file.path(out_dir, "prog.R")
  expect_true(file.exists(out_r))
  code <- paste(readLines(out_r), collapse = "\n")
  expect_match(code, "dplyr::count\\(trt01p, name = \"count\"\\)")
  expect_match(code, "percent = 100 \\* count / sum\\(count\\)")
})

test_that("out=work.missing does not trigger missing option, and blanks are filtered", {
  u <- sas_units(sas_statements(
    "proc freq data=w.inp noprint; tables trt / out=work.missing; run;"))
  em <- emit_proc_freq(u[u$unit_type == "proc_step", ])
  expect_false(em$parse$missing)
  expect_match(em$code, "dplyr::filter\\(!is.na\\(trt\\) & \\(!is.character\\(trt\\) \\| trimws\\(trt\\) != \"\"\\)\\)")

  # Numeric variable typing in oracle
  o_num <- freq_oracle(tibble::tibble(num = c(1, 2, 2, NA)), "num", missing = FALSE)
  expect_type(o_num$num, "double")
  expect_identical(o_num$num, c(1, 2))
})
