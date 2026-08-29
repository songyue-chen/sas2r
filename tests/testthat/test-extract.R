test_that("libname with and without engine", {
  s <- sas_statements("libname adam xport '/data/adam.xpt';\nlibname sdtm \"/data/sdtm\";")
  lr <- extract_librefs(s)
  expect_identical(lr$libref, c("adam", "sdtm"))
  expect_identical(lr$engine, c("xport", ""))
  expect_identical(lr$action, c("assign", "assign"))
  expect_identical(lr$path_expression, c("/data/adam.xpt", "/data/sdtm"))
  # `path` is kept as a compatibility alias of `path_expression`
  expect_identical(lr$path, lr$path_expression)
})

test_that("libname clear is an event, not a dropped statement", {
  s <- sas_statements("libname adam '/data/adam';\nlibname adam clear;")
  lr <- extract_librefs(s)
  expect_identical(lr$action, c("assign", "clear"))
  expect_identical(lr$libref, c("adam", "adam"))
  expect_identical(lr$line, c(1L, 2L))
  # a clear binds no path, and says so rather than inventing one
  expect_identical(lr$path_expression, c("/data/adam", NA_character_))
  expect_identical(lr$path, lr$path_expression)
  expect_identical(names(lr), c("libref", "action", "engine", "path_expression",
                                "path", "line", "unit_id", "unit_type"))
  # statements alone carry no translation unit, exactly as for %include
  expect_identical(lr$unit_id, c(NA_integer_, NA_integer_))
  expect_identical(extract_librefs(sas_units(s))$unit_id, c(1L, 2L))

  empty_lr <- extract_librefs(sas_statements(""))
  expect_identical(nrow(empty_lr), 0L)
  expect_identical(names(empty_lr), names(lr))
})

test_that("quoted and fileref includes are both recorded", {
  s <- sas_statements("%include '/org/macros/setup.sas';\n%include mymacs(alloc);")
  inc <- extract_includes(s)
  expect_identical(nrow(inc), 2L)
  expect_identical(inc$quoted, c(TRUE, FALSE))
  expect_identical(inc$target[1], "/org/macros/setup.sas")
  expect_identical(inc$target[2], "mymacs(alloc)")
  # statements alone carry no translation unit, so the include site is unknown
  expect_identical(inc$unit_id, c(NA_integer_, NA_integer_))

  # unit-aware statements anchor each include to the unit it sits in. The
  # fixture deliberately puts a non-include statement first and a multi-
  # statement unit in between, so the include rows sit at statement positions
  # 2 and 5 while their unit ids are 2 and 4: dropping the positional subset in
  # extract_includes() would return the whole unit column, not these two.
  u <- sas_units(sas_statements(
    "options nodate;\n%include 'a.sas';\ndata x; run;\n%include 'b.sas';"))
  unit_inc <- extract_includes(u)
  expect_identical(unit_inc$target, c("a.sas", "b.sas"))
  expect_identical(which(u$first_token == "%include"), c(2L, 5L))
  expect_identical(unit_inc$unit_id, c(2L, 4L))
  expect_false(identical(unit_inc$unit_id, as.integer(u$unit_id)))

  empty_inc <- extract_includes(sas_statements(""))
  expect_identical(nrow(empty_inc), 0L)
  expect_identical(names(empty_inc), c("target", "quoted", "line", "unit_id"))
})

test_that("data step lineage: creates and reads, _null_ excluded, options stripped", {
  u <- sas_units(sas_statements(
    "data out1 adam.two (keep=id) _null_; merge a (in=ina) adam.b (in=inb); run;"))
  refs <- extract_dataset_refs(u)
  expect_setequal(refs$dataset[refs$role == "creates"], c("work.out1", "adam.two"))
  expect_setequal(refs$dataset[refs$role == "reads"], c("work.a", "adam.b"))
})

test_that("proc lineage: data= reads, out= creates", {
  u <- sas_units(sas_statements("proc sort data=adam.adsl out=w_srt; by id; run;"))
  refs <- extract_dataset_refs(u)
  expect_identical(refs$dataset[refs$role == "reads"], "adam.adsl")
  expect_identical(refs$dataset[refs$role == "creates"], "work.w_srt")
})

test_that("proc sql lineage: create table, from, join", {
  u <- sas_units(sas_statements(
    "proc sql; create table w2 as select * from adam.adsl a left join work.vs b on a.id=b.id; quit;"))
  refs <- extract_dataset_refs(u)
  expect_identical(refs$dataset[refs$role == "creates"], "work.w2")
  expect_setequal(refs$dataset[refs$role == "reads"], c("adam.adsl", "work.vs"))
})

test_that("nested parentheses in data step options do not leak tokens", {
  u <- sas_units(sas_statements(
    "data adam.adsl_sub; set adam.adsl (where=(saffl='Y' and age > 18) keep=usubjid age); run;"))
  refs <- extract_dataset_refs(u)
  expect_identical(refs$dataset[refs$role == "creates"], "adam.adsl_sub")
  expect_identical(refs$dataset[refs$role == "reads"], "adam.adsl")
})

test_that("proc output and tables out= statements capture creates in lineage", {
  u1 <- sas_units(sas_statements(
    "proc summary data=adam.adsl nway; class trtp; var age; output out=work.summary mean=m_age; run;"))
  refs1 <- extract_dataset_refs(u1)
  expect_setequal(refs1$dataset[refs1$role == "reads"], "adam.adsl")
  expect_setequal(refs1$dataset[refs1$role == "creates"], "work.summary")

  u2 <- sas_units(sas_statements(
    "proc freq data=adam.adsl; tables trtp * sex / out=work.freq_table; run;"))
  refs2 <- extract_dataset_refs(u2)
  expect_setequal(refs2$dataset[refs2$role == "reads"], "adam.adsl")
  expect_setequal(refs2$dataset[refs2$role == "creates"], "work.freq_table")
})

test_that("norm_ds normalizes single-level and two-level datasets correctly", {
  expect_identical(norm_ds(character(0)), character(0))
  expect_identical(norm_ds("adsl"), "work.adsl")
  expect_identical(norm_ds("ADSL"), "work.adsl")
  expect_identical(norm_ds("adam.ADSL"), "adam.adsl")
  expect_identical(norm_ds(c("a", "ADAM.B")), c("work.a", "adam.b"))
})

test_that("bare data; and set; statements emit no phantom lineage", {
  refs1 <- extract_dataset_refs(sas_units(sas_statements("data; set old; run;")))
  expect_false("work.data" %in% refs1$dataset)
  expect_identical(refs1$dataset[refs1$role == "reads"], "work.old")

  refs2 <- extract_dataset_refs(sas_units(sas_statements("data new; set; run;")))
  expect_false("work.set" %in% refs2$dataset)
  expect_identical(refs2$dataset[refs2$role == "creates"], "work.new")
})

test_that("datalines payload containing libname statements does not extract fake librefs", {
  s <- sas_statements("data a;\ndatalines;\nlibname fake '/tmp/x'\n;\nrun;")
  lr <- extract_librefs(s)
  expect_identical(nrow(lr), 0L)
})



test_that("a libname carries the translation unit type it sits in", {
  s <- sas_units(sas_statements(paste(
    "%macro never_called;", "libname ghost '/ghost';", "%mend;",
    "libname real '/real';", sep = "\n")))
  lr <- extract_librefs(s)
  expect_identical(lr$libref, c("ghost", "real"))
  # a LIBNAME inside a macro definition is conditional: it is only reached if
  # something calls the macro, so the unit type has to travel with the event
  expect_identical(lr$unit_type, c("macro_def", "global"))
  # statements alone carry no translation unit, exactly as for unit_id
  expect_identical(extract_librefs(sas_statements("libname a '/x';"))$unit_type,
                   NA_character_)
})
