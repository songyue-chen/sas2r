test_that("select-from-where-group-order translates and executes", {
  skip_if_not_installed("dplyr")
  em <- emit_sql_create(
    "create table s as select trt, count(*) as n, avg(aval) as m from work.adsl where saffl = 'Y' group by trt order by trt", 1L)
  expect_no_error(parse(text = em$code))
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    work = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) tibble::tibble(
    trt = c("A", "A", "B"), saffl = c("Y", "Y", "Y"), aval = c(1, 3, 5))
  eval(parse(text = em$code), envir = e)
  s <- get("s", envir = e)
  expect_identical(s$n, c(2L, 1L))
  expect_identical(s$m, c(2, 5))
})

test_that("plain column select without grouping", {
  em <- emit_sql_create("create table t as select a, b from work.x", 1L)
  expect_match(em$code, "dplyr::select\\(a, b\\)")
})

test_that("select star without column list emits no dplyr::select", {
  em <- emit_sql_create("create table t as select * from work.x", 1L)
  expect_false(grepl("dplyr::select", em$code))
  expect_match(em$code, 't <- lib_read\\("work", "x"\\)')
})

test_that("aggregates map correctly (count, sum, avg, min, max)", {
  em <- emit_sql_create(
    "create table res as select sum(val) as sm, avg(val) as av, min(val) as mn, max(val) as mx, count(*) as n from work.dt", 1L)
  expect_match(em$code, "sm = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else sum\\(val, na.rm = TRUE\\)")
  expect_match(em$code, "av = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else mean\\(val, na.rm = TRUE\\)")
  expect_match(em$code, "mn = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else min\\(val, na.rm = TRUE\\)")
  expect_match(em$code, "mx = if \\(all\\(is.na\\(val\\)\\)\\) NA_real_ else max\\(val, na.rm = TRUE\\)")
  expect_match(em$code, "n = dplyr::n\\(\\)")

  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    work = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) tibble::tibble(val = c(10, 20, NA, 30))
  eval(parse(text = em$code), envir = e)
  res <- get("res", envir = e)
  expect_identical(res$sm, 60)
  expect_identical(res$av, 20)
  expect_identical(res$mn, 10)
  expect_identical(res$mx, 30)
  expect_identical(res$n, 4L)
})

test_that("joins and subqueries are rejected as not-T1", {
  em <- emit_sql_create(
    "create table t as select * from a left join b on a.k = b.k", 1L)
  expect_true(is.na(em$code))
  expect_identical(em$flags, "sql_not_t1")

  em2 <- emit_sql_create(
    "create table t as select * from (select * from x)", 1L)
  expect_identical(em2$flags, "sql_not_t1")

  em3 <- emit_sql_create(
    "create table t as select calculated x from y", 1L)
  expect_identical(em3$flags, "sql_not_t1")

  em4 <- emit_sql_create(
    "create table t as select a into :var from y", 1L)
  expect_identical(em4$flags, "sql_not_t1")

  em5 <- emit_sql_create(
    "create table t as select a from y(where=(a=1))", 1L)
  expect_identical(em5$flags, "sql_not_t1")

  # F7 / R2-7: distinct, having, desc, non-agg as alias, mixed agg+detail, unaliased expressions
  expect_identical(emit_sql_create("create table t as select distinct a from y", 1L)$flags, "sql_not_t1")
  expect_identical(emit_sql_create("create table t as select a, count(*) as n from y group by a having count(*) > 1", 1L)$flags, "sql_not_t1")
  expect_identical(emit_sql_create("create table t as select a from y order by a desc", 1L)$flags, "sql_not_t1")
  expect_identical(emit_sql_create("create table t as select a as new_a from y", 1L)$flags, "sql_not_t1")
  expect_identical(emit_sql_create("create table t as select trt, sex, count(*) as n from y group by trt", 1L)$flags, "sql_not_t1")
  expect_identical(emit_sql_create("create table t as select sum(a+b) from y", 1L)$flags, "sql_not_t1")
})

test_that("multiline SQL and case insensitivity", {
  sql <- "
    CREATE TABLE work.out AS
    SELECT trt, COUNT(*) AS n
    FROM adam.adsl
    WHERE age >= 18
    GROUP BY trt
    ORDER BY trt;
  "
  em <- emit_sql_create(sql, 42L)
  expect_false(is.na(em$code))
  expect_identical(em$stmt_map, 42L)
  expect_match(em$code, 'out <- lib_read\\("adam", "adsl"\\)')
  expect_match(em$code, "dplyr::filter")
  expect_match(em$code, "dplyr::group_by\\(trt\\)")
  expect_match(em$code, 'dplyr::summarise\\(n = dplyr::n\\(\\), .groups = "drop"\\)')
  expect_match(em$code, "dplyr::arrange\\(trt\\)")
})

test_that("aggregates over an all-missing column yield NA without warnings", {
  em <- emit_sql_create(
    "create table res as select sum(val) as sm, avg(val) as av, min(val) as mn, max(val) as mx from work.dt", 1L)
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    work = list(path = withr::local_tempdir(), engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) tibble::tibble(val = c(NA_real_, NA_real_))
  expect_no_warning(eval(parse(text = em$code), envir = e))
  res <- get("res", envir = e)
  expect_identical(res$sm, NA_real_)
  expect_identical(res$av, NA_real_)
  expect_identical(res$mn, NA_real_)
  expect_identical(res$mx, NA_real_)
})

test_that("created SQL tables persist to the target library, not just a local variable", {
  em <- emit_sql_create("create table adam.res as select a, b from work.x", 1L)
  expect_match(em$code, 'lib_write\\(res, "adam", "res"\\)')

  # Behavioral: the dataset must actually reach the library directory.
  lib_dir <- withr::local_tempdir()
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  e$.sas2r_registry <- list(
    adam = list(read_path = lib_dir, write_path = lib_dir, engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) tibble::tibble(a = 1:2, b = 3:4)
  eval(parse(text = em$code), envir = e)
  expect_true(file.exists(file.path(lib_dir, "res.rds")))

  # An unqualified target defaults to the work library.
  em2 <- emit_sql_create("create table t as select a from work.x", 1L)
  expect_match(em2$code, 'lib_write\\(t, "work", "t"\\)')
})
