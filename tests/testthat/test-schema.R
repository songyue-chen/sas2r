test_that("kinds come from code evidence; roots are inferred and tainted", {
  dir <- withr::local_tempdir()
  writeLines(paste(
    "data w1; set w.root; where sex = 'M'; bmi = wt / 2; run;",
    "data w2; set w1; if bmi ge 30 then obese = 'Y'; run;", sep = "\n"),
    file.path(dir, "a.sas"))
  sc <- infer_schemas(sas_project(dir))
  root <- sc[["w.root"]]
  expect_identical(root$vars$kind[root$vars$var == "sex"], "character")
  expect_identical(root$vars$kind[root$vars$var == "wt"], "numeric")
  expect_identical(root$taint, "inferred")   # root schema is a guess
  w2 <- sc[["work.w2"]]
  expect_identical(w2$vars$kind[w2$vars$var == "obese"], "character")
  expect_identical(w2$vars$source[w2$vars$var == "bmi"], "propagated")
  expect_identical(w2$taint, "inferred")     # taint flows downstream
})

test_that("input root taint propagates downstream even with literal assignments", {
  dir <- withr::local_tempdir()
  writeLines("data w1; set w.root; x = 1; keep x; run;", file.path(dir, "a.sas"))
  sc <- infer_schemas(sas_project(dir))
  expect_identical(sc[["work.w1"]]$vars$kind, "numeric")
  expect_identical(sc[["work.w1"]]$taint, "inferred")
})

test_that("drop and rename filter and transform propagated schema", {
  dir <- withr::local_tempdir()
  writeLines(paste(
    "data w1; set w.root; x = 1; y = 'abc'; z = 3; run;",
    "data w2; set w1; drop y; rename x = x_new; run;", sep = "\n"),
    file.path(dir, "a.sas"))
  sc <- infer_schemas(sas_project(dir))
  w2_vars <- sc[["work.w2"]]$vars
  expect_false("y" %in% w2_vars$var)
  expect_true("x_new" %in% w2_vars$var)
  expect_false("x" %in% w2_vars$var)
  expect_true("z" %in% w2_vars$var)
})

test_that("infer_schemas handles double quotes and complex expressions", {
  dir <- withr::local_tempdir()
  writeLines(paste(
    "data w1; set w.root; where region = \"NORTH\"; total = price * qty + fee; label = \"TOTAL_REV\"; run;",
    sep = "\n"),
    file.path(dir, "a.sas"))
  sc <- infer_schemas(sas_project(dir))
  root <- sc[["w.root"]]
  expect_identical(root$vars$kind[root$vars$var == "region"], "character")
  expect_identical(root$vars$kind[root$vars$var == "price"], "numeric")
  expect_identical(root$vars$kind[root$vars$var == "qty"], "numeric")
  expect_identical(root$vars$kind[root$vars$var == "fee"], "numeric")

  w1 <- sc[["work.w1"]]
  expect_identical(w1$vars$kind[w1$vars$var == "label"], "character")
  expect_identical(w1$vars$kind[w1$vars$var == "total"], "numeric")
})

test_that("infer_schemas handles empty and proc steps gracefully", {
  dir <- withr::local_tempdir()
  writeLines(paste(
    "proc sort data=w.root out=sorted; by id; run;",
    "data w1; set sorted; x = 10; run;", sep = "\n"),
    file.path(dir, "a.sas"))
  sc <- infer_schemas(sas_project(dir))
  expect_true("work.w1" %in% names(sc))
  expect_identical(sc[["work.w1"]]$vars$kind[sc[["work.w1"]]$vars$var == "x"], "numeric")
})

test_that("infer_schemas infers character for in-lists, char inequalities, and string functions", {
  dir <- withr::local_tempdir()
  writeLines(paste(
    "data w1; set w.root;",
    "where region in ('NORTH', 'SOUTH') and code >= 'B';",
    "full_tag = prefix || '_' || suffix;",
    "sub_code = substr(raw, 1, 3);",
    "run;", sep = "\n"),
    file.path(dir, "a.sas"))
  sc <- infer_schemas(sas_project(dir))
  root <- sc[["w.root"]]
  expect_identical(root$vars$kind[root$vars$var == "region"], "character")
  expect_identical(root$vars$kind[root$vars$var == "code"], "character")
  expect_identical(root$vars$kind[root$vars$var == "prefix"], "character")
  expect_identical(root$vars$kind[root$vars$var == "suffix"], "character")
  expect_identical(root$vars$kind[root$vars$var == "raw"], "character")

  w1 <- sc[["work.w1"]]
  expect_identical(w1$vars$kind[w1$vars$var == "full_tag"], "character")
  expect_identical(w1$vars$kind[w1$vars$var == "sub_code"], "character")
})
