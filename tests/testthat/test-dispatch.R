proc_unit <- function(src) {
  u <- sas_units(sas_statements(src))
  u[u$unit_type == "proc_step", ]
}

test_that("dispatch resolves emitters from the rulebook", {
  rb <- load_rulebook()
  d <- dispatch_proc(proc_unit("proc sort data=w.a out=b; by k; run;"), rb)
  expect_null(d$reason)
  expect_match(d$em$code, "sas_sort")
})

test_that("deferred and unknown procs produce reasons, not code", {
  rb <- load_rulebook()
  d1 <- dispatch_proc(proc_unit("proc transpose data=w.a out=b; run;"), rb)
  expect_identical(d1$reason, "proc_deferred:transpose")
  d2 <- dispatch_proc(proc_unit("proc mystery data=w.a; run;"), rb)
  expect_identical(d2$reason, "proc_unknown:mystery")
})

test_that("sql wrapper requires exactly one create statement", {
  rb <- load_rulebook()
  d <- dispatch_proc(proc_unit("proc sql; drop table w.x; quit;"), rb)
  expect_identical(d$reason, "sql_not_t1")
})

test_that("a rulebook edit changes routing without code changes", {
  rb <- load_rulebook()
  rb$procs$sort$emitter <- NULL
  rb$procs$sort$status <- "deferred"
  d <- dispatch_proc(proc_unit("proc sort data=w.a out=b; by k; run;"), rb)
  expect_identical(d$reason, "proc_deferred:sort")
})

test_that("format and print proc dispatch correctly", {
  rb <- load_rulebook()
  d_fmt <- dispatch_proc(proc_unit("proc format; value f 1='a'; run;"), rb)
  expect_null(d_fmt$reason)
  expect_match(d_fmt$em$code, "formats compiled into _sas2r_formats.R")

  d_prt <- dispatch_proc(proc_unit("proc print data=w.a; run;"), rb)
  expect_identical(d_prt$reason, "display_only")
  expect_null(d_prt$em)
})
