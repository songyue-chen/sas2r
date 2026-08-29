proc_unit <- function(src) {
  u <- sas_units(sas_statements(src))
  u[u$unit_type == "proc_step", ]
}

test_that("proc sort with out=, descending, and nodupkey", {
  em <- emit_proc_sort(proc_unit(
    "proc sort data=adam.adsl out=srt nodupkey; by usubjid descending age; run;"))
  expect_match(em$code, 'srt <- sas_sort\\(sas2r_fold_names\\(lib_read\\("adam", "adsl"\\)\\), by = c\\("usubjid", "age"\\), descending = c\\("age"\\)\\)')
  expect_match(em$code, "dplyr::distinct")
  expect_no_error(parse(text = em$code))
})

test_that("proc sort in-place without out=", {
  em <- emit_proc_sort(proc_unit(
    "proc sort data=adam.adsl; by usubjid; run;"))
  expect_match(em$code, 'adsl <- sas_sort\\(sas2r_fold_names\\(lib_read\\("adam", "adsl"\\)\\), by = c\\("usubjid"\\)\\)')
  expect_match(em$code, 'lib_write\\(adsl, "adam", "adsl"\\)')
  expect_no_error(parse(text = em$code))
})

test_that("proc sort distinguishes nodup from nodupkey", {
  # nodupkey: dedups by by-variables (pick(all_of(by)))
  em_nodupkey <- emit_proc_sort(proc_unit(
    "proc sort data=adam.adsl nodupkey; by usubjid; run;"))
  expect_match(em_nodupkey$code, "dplyr::distinct\\(dplyr::pick\\(dplyr::all_of")

  # nodup: dedups across all columns (distinct(.keep_all = TRUE)) (F17)
  em_nodup <- emit_proc_sort(proc_unit(
    "proc sort data=adam.adsl nodup; by usubjid; run;"))
  expect_match(em_nodup$code, "dplyr::distinct\\(\\.keep_all = TRUE\\)")
})

test_that("proc sort missing by or data statement returns flag", {
  em_by <- emit_proc_sort(proc_unit(
    "proc sort data=adam.adsl; run;"))
  expect_true(is.na(em_by$code))
  expect_identical(em_by$flags, "sort_missing_by")

  # F18: missing data= statement
  em_data <- emit_proc_sort(proc_unit(
    "proc sort; by usubjid; run;"))
  expect_true(is.na(em_data$code))
  expect_identical(em_data$flags, "sort_missing_data")
})

test_that("value formats compile: discrete, range, other", {
  p_src <- "proc format; value sexf 1='M' 2='F' other='U';
            value agef 0-17='CHILD' 18-64='ADULT' 65-120='ELDER'; run;"
  dir <- withr::local_tempdir()
  writeLines(p_src, file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  expect_identical(cat_$catalog$sexf$values[["1"]], "M")
  expect_identical(cat_$catalog$sexf$other, "U")
  expect_identical(cat_$catalog$agef$ranges[[2]]$label, "ADULT")
  expect_identical(nrow(cat_$flags), 0L)
})

test_that("picture formats are flagged, not dropped", {
  dir <- withr::local_tempdir()
  writeLines("proc format; picture pctf low-high='009.9%'; run;",
             file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  expect_match(cat_$flags$reason, "format_unsupported:pctf")
})

test_that("invalue formats are flagged", {
  dir <- withr::local_tempdir()
  writeLines("proc format; invalue inf '1'=1 '2'=2; run;",
             file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  expect_match(cat_$flags$reason, "format_unsupported:inf")
})

test_that("multilabel format option is flagged", {
  dir <- withr::local_tempdir()
  writeLines("proc format; value mlfmt (multilabel) 1='One' 1-2='OneTwo'; run;",
             file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  expect_match(cat_$flags$reason, "format_unsupported:mlfmt")
})

test_that("character format with quotes and commas compiles", {
  p_src <- "proc format; value $yn 'Y', 'y'='YES' 'N', 'n'='NO' other='UNKNOWN'; run;"
  dir <- withr::local_tempdir()
  writeLines(p_src, file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  expect_identical(cat_$catalog[["$yn"]]$values[["Y"]], "YES")
  expect_identical(cat_$catalog[["$yn"]]$values[["y"]], "YES")
  expect_identical(cat_$catalog[["$yn"]]$values[["N"]], "NO")
  expect_identical(cat_$catalog[["$yn"]]$values[["n"]], "NO")
  expect_identical(cat_$catalog[["$yn"]]$other, "UNKNOWN")
})

test_that("unquoted format labels compile correctly", {
  # F6: unquoted labels like 1=YES 0=NO
  p_src <- "proc format; value yn 1=YES 0=NO other=UNKNOWN; run;"
  dir <- withr::local_tempdir()
  writeLines(p_src, file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  expect_identical(cat_$catalog$yn$values[["1"]], "YES")
  expect_identical(cat_$catalog$yn$values[["0"]], "NO")
  expect_identical(cat_$catalog$yn$other, "UNKNOWN")
})

test_that("written formats file is sourceable and works with apply_format", {
  dir <- withr::local_tempdir()
  writeLines("proc format; value yn 1='YES' 0='NO'; run;", file.path(dir, "f.sas"))
  cat_ <- compile_format_catalog(sas_project(dir))
  out <- withr::local_tempdir()
  write_formats(cat_$catalog, out)
  e <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  sys.source(file.path(out, "_sas2r_formats.R"), e)
  expect_identical(e$apply_format(c(1, 0), e$.sas2r_formats$yn), c("YES", "NO"))
})

test_that("proc sort with dataset options refuses instead of dropping them", {
  em <- emit_proc_sort(proc_unit(
    "proc sort data=adam.adsl(where=(saffl = 'Y')) out=srt; by usubjid; run;"))
  expect_true(is.na(em$code))
  expect_identical(em$flags, "sort_dataset_options")
})
