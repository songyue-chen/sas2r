test_that("mask distinguishes single- and double-quoted strings", {
  sc <- sas_scan("x = 'a' ; y = \"b\" ;")
  expect_identical(sc$mask[which(sc$chars == "a")], "s")
  expect_identical(sc$mask[which(sc$chars == "b")], "q")
})

test_that("mask_strings blanks single-quoted content, offsets stable", {
  out <- mask_strings("x = '%notacall';")
  expect_identical(nchar(out), nchar("x = '%notacall';"))
  expect_false(grepl("%notacall", out, fixed = TRUE))
})

test_that("keep_double preserves double-quoted content", {
  src <- 'x = "%realcall" ; y = \'%ghost\' ;'
  out <- mask_strings(src, keep_double = TRUE)
  expect_true(grepl("%realcall", out, fixed = TRUE))
  expect_false(grepl("%ghost", out, fixed = TRUE))
})

test_that("macro calls inside single quotes are not extracted; double quotes are", {
  u <- sas_units(sas_statements("data a; x = '%no'; y = \"%yes\"; run;"))
  calls <- extract_macro_calls(u)
  expect_identical(calls$name, "yes")
})

test_that("paren matching in macro args ignores parens inside any string", {
  s <- sas_statements("%m('a)b') data x; run;")
  expect_identical(s$first_token[1], "%m")
  expect_identical(s$first_token[2], "data")
})
