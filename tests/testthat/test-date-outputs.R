# SAS dates in delivered datasets. Every dataset here is made up in the test.

sas_day <- function(x) as.numeric(as.Date(x) - as.Date("1960-01-01"))

# A project of one or more SAS programs writing to `adam`, with optional
# reference datasets saved as RDS.
date_fixture <- function(programs, outputs, references = list(), envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  dir.create(file.path(root, "inputs"))
  dir.create(file.path(root, "ref"))
  for (name in names(programs)) writeLines(programs[[name]], file.path(root, paste0(name, ".sas")))
  paths <- lapply(names(references), function(target) {
    path <- file.path(root, "ref", paste0(sub("^.*[.]", "", target), ".rds"))
    saveRDS(references[[target]], path)
    path
  })
  config <- list(
    libraries = list(raw = list(path = file.path(root, "inputs"), engine = "rds"),
                     adam = list(path = file.path(root, "adam"), engine = "rds", write = "rds")),
    outputs = c(list(datasets = outputs),
                if (length(references)) list(references = stats::setNames(paths, names(references)))),
    migration = list(execution_order = paste0(names(programs), ".sas")))
  list(root = root, config = config, project = sas_project(root, config = config))
}

# One program: a derived date, a duration, and the steps given between the
# derivation and the sorted output.
adv_program <- function(declare = "  format adt yymmdd10.;", middle = character(),
                        last = c("proc sort data=work.a out=adam.adv;", "  by usubjid;", "run;")) {
  c("data work.a;", "  set raw.visits;", "  adt = input(adtc, yymmdd10.);",
    "  dur = adt - rfdt + 1;", declare, "run;", middle, last)
}

# The runtime a bundle carries, with one writable library.
date_runtime <- function(write = "rds", dir = NULL, dates = list(), envir = parent.frame()) {
  h <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), h)
  dir <- dir %||% withr::local_tempdir(.local_envir = envir)
  h$.sas2r_registry <- list(adam = list(read_path = dir, write_path = dir, engine = write, write = write))
  if (length(dates)) h$.sas2r_output_dates <- dates
  list(h = h, dir = dir, file = file.path(dir, paste0("adv.", write)))
}
dated <- c(adt = "yymmdd10.")

test_that("the date formats are exactly the ones haven reads back as a date", {
  formats <- c("DATE", "DATE9", "DATE11", "YYMMDD10", "YYMMDDD10", "YYMMDDN8", "YYMMDDS10",
    "MMDDYY10", "MMDDYYS10", "DDMMYY10", "DDMMYYP10", "E8601DA", "E8601DA10", "B8601DA8",
    "IS8601DA10", "WEEKDATE29", "WEEKDATX", "DATETIME20", "E8601DT", "TIME8", "MONYY7",
    "YYQ6", "YYMM", "WORDDATE18", "JULIAN7", "NLDATE", "YEAR4", "BEST12")
  columns <- paste0("v", seq_along(formats))
  df <- as.data.frame(stats::setNames(
    lapply(formats, function(f) structure(c(19000, 0), format.sas = f)), columns))
  path <- withr::local_tempfile(fileext = ".xpt")
  haven::write_xpt(df, path)
  back <- haven::read_xpt(path)
  read_as_date <- vapply(back, function(x) inherits(x, "Date"), logical(1), USE.NAMES = FALSE)
  typed <- !is.na(vapply(paste0(formats, "."), sas_date_format_name, character(1), USE.NAMES = FALSE))
  expect_identical(typed, read_as_date)
  expect_identical(sas_date_format_name("yymmdd10."), "YYMMDD10")
  expect_identical(sas_date_format_name("date9.2"), NA_character_)
  expect_identical(sas_date_format_name("$char10."), NA_character_)
  # DATEAMPM is a datetime format in SAS; haven reads it as a date. It stays a
  # number here, so such a column is reported as a type difference.
  expect_identical(sas_date_format_name("dateampm."), NA_character_)

  # lib_write() carries its own copy of the list, because the runtime is a
  # standalone file. It stores as a date exactly the formats above.
  rt <- date_runtime()
  plain <- as.data.frame(stats::setNames(lapply(formats, function(f) c(19000, 0)), columns))
  rt$h$lib_write(plain, "adam", "adv", formats = stats::setNames(paste0(formats, "."), columns))
  stored <- vapply(readRDS(rt$file), function(x) inherits(x, "Date"), logical(1), USE.NAMES = FALSE)
  expect_identical(stored, read_as_date)
  # A format is read the same whether or not it has its period, in any case.
  for (written in c("yymmdd10.", "YYMMDD10.", "yymmdd10", " Yymmdd10. ")) {
    expect_identical(declared_date_format_name(written), "YYMMDD10", info = written)
  }
  expect_identical(declared_date_format_name("8.2"), NA_character_)
  expect_identical(declared_date_format_name("datetime20."), NA_character_)
  expect_identical(declared_date_format_name(NA_character_), NA_character_)
})

test_that("a FORMAT or ATTRIB statement is read exactly or not at all", {
  read <- function(text) parse_format_declaration(text)
  expect_identical(read("format astdt aendt yymmdd10.")$aendt, list(kind = "date", format = "YYMMDD10"))
  mixed <- read("format adt date9. aval 8.2 name $char20. old")
  expect_identical(vapply(mixed, `[[`, "", "kind"),
                   c(adt = "date", aval = "other", name = "other", old = "none"))
  attrib <- read("attrib adt label='Date of = visit; format=best.' format=date9. aval length=8 label=\"A value\"")
  expect_identical(attrib, list(adt = list(kind = "date", format = "DATE9")))
  expect_identical(read("attrib adt label='Analysis date' length=8"), list())
  for (text in c("format &vars date9.", "format %dates() date9.", "format _numeric_ best12.",
                 "format dt1-dt5 date9.", "format a--b date9.", "format dt: date9.",
                 "format default=best12. adt date9.", "format date9.",
                 "attrib adt format=&fmt", "attrib adt length=$ 8", "attrib adt unknown=1",
                 "length adt 8")) {
    expect_null(read(text), info = text)
  }
})

test_that("lib_write stores a declared date as a Date and lib_read returns the SAS day number", {
  rt <- date_runtime()
  iso <- c("1959-12-31", "1960-01-01", "2024-02-29", NA, "2026-11-24")
  df <- data.frame(usubjid = paste0("S", 1:5), adt = sas_day(iso), rfdt = sas_day("1959-12-25"))
  df$adt[4] <- haven::tagged_na("a")
  attr(df$adt, "label") <- "Analysis date"
  df$dur <- df$adt - df$rfdt + 1
  # A format that is not a date format changes nothing.
  returned <- rt$h$lib_write(df, "adam", "adv", formats = c(adt = "yymmdd10.", dur = "8.2", usubjid = "$char10."))
  expect_identical(returned, df)

  stored <- readRDS(rt$file)
  expect_s3_class(stored$adt, "Date")
  expect_identical(format(stored$adt), iso)
  expect_identical(attr(stored$adt, "format.sas"), "YYMMDD10")
  expect_identical(attr(stored$adt, "label"), "Analysis date")
  expect_identical(haven::na_tag(unclass(stored$adt))[4], "a")
  # Not declared: the reference date and the duration stay numbers.
  expect_identical(stored$rfdt, df$rfdt)
  expect_identical(stored$dur, df$dur)
  expect_identical(stored$usubjid, df$usubjid)

  back <- rt$h$lib_read("adam", "adv")
  expect_identical(as.data.frame(back)$adt, df$adt)
  expect_identical(haven::na_tag(back$adt)[4], "a")
  # Date arithmetic in a later program is the SAS arithmetic, with no format
  # carried onto the result.
  aval <- back$adt - back$rfdt + 1
  expect_identical(as.vector(aval), c(7, 8, 23443, NA, 24442))
  expect_null(attr(aval, "format.sas"))

  # The argument names each column once with a string.
  for (bad in list("yymmdd10.", c(adt = "date9.", ADT = "date9."), c(adt = NA_character_), list(adt = "date9."), 9)) {
    expect_error(rt$h$lib_write(df, "adam", "adv", formats = bad), class = "sas2r_lib_write_arguments")
  }
  expect_error(rt$h$lib_write(df, "adam", "adv", dataset = "x"), class = "sas2r_lib_write_arguments")
})

test_that("only a plain number that converts exactly is stored as a date", {
  rt <- date_runtime()
  write <- function(adt) {
    rt$h$lib_write(data.frame(id = seq_along(adt), adt = adt), "adam", "adv", formats = dated)
    readRDS(rt$file)$adt
  }
  # 0.1 does not survive the shift between the two origins exactly, so the
  # column is left alone; 19000.5 does, and reads back as the same number.
  expect_identical(write(c(0.1, 19001)), c(0.1, 19001))
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(0.1, 19001))
  expect_s3_class(write(c(19000.5, 19001)), "Date")
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(19000.5, 19001))
  expect_identical(write(c("2024-02-29", "2024-03-01")), c("2024-02-29", "2024-03-01"))
  expect_identical(write(c(NA, NA)), c(NA, NA))
  # A declared column that is a Date already is stored as it is and read back
  # as a day number, like any other declared date.
  already <- as.Date(c("2024-02-29", NA))
  expect_identical(write(already), already)
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(sas_day("2024-02-29"), NA))
  whole <- write(c(23435L, NA))
  expect_identical(format(whole), c("2024-02-29", NA))
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(23435, NA))

  rt$h$lib_write(data.frame(id = integer(), adt = numeric()), "adam", "adv", formats = dated)
  expect_s3_class(readRDS(rt$file)$adt, "Date")
  expect_identical(rt$h$lib_read("adam", "adv")$adt, numeric())

  # An upper-case member and column match a lower-case declaration, and the
  # reverse. This uses its own library: `adv` and `ADV` are one file on a
  # case-insensitive file system and two files on a case-sensitive one.
  upper <- date_runtime()
  upper$h$lib_write(data.frame(ID = 1, ADT = 23435), "adam", "ADV", formats = dated)
  stored <- readRDS(file.path(upper$dir, "ADV.rds"))
  expect_identical(format(stored$ADT), "2024-02-29")
  expect_identical(upper$h$lib_read("adam", "ADV")$ADT, 23435)
  lower <- date_runtime()
  lower$h$lib_write(data.frame(id = 1, adt = 23435), "adam", "adv", formats = c(ADT = "DATE9."))
  expect_identical(format(readRDS(lower$file)$adt), "2024-02-29")
})

test_that("nothing changes for a dataset written without formats", {
  rt <- date_runtime()
  rt$h$lib_write(data.frame(id = 1, adt = 23435), "adam", "adv")
  expect_identical(readRDS(rt$file)$adt, 23435)
  expect_false(exists(".sas2r_output_dates", envir = rt$h, inherits = FALSE))
  # A date that arrives as a Date and is passed on is written and read as one.
  kept <- data.frame(id = 1, adt = as.Date("2024-02-29"))
  rt$h$lib_write(kept, "adam", "adv")
  expect_identical(rt$h$lib_read("adam", "adv")$adt, kept$adt)
  # A member written again without the format is read as it was written.
  rt$h$lib_write(data.frame(id = 1, adt = 23435), "adam", "adv", formats = dated)
  expect_identical(rt$h$lib_read("adam", "adv")$adt, 23435)
  rt$h$lib_write(kept, "adam", "adv")
  expect_identical(rt$h$lib_read("adam", "adv")$adt, kept$adt)
  expect_null(rt$h$.sas2r_output_dates$adam.adv)

  dir <- withr::local_tempdir()
  write_formats(list(), dir)
  expect_identical(readLines(file.path(dir, "_sas2r_formats.R")),
                   c("# Generated by sas2r -- format catalog.", ".sas2r_formats <- list()"))
  write_formats(list(), dir, list(adam.adv = c(adt = "YYMMDD10")))
  env <- new.env()
  sys.source(file.path(dir, "_sas2r_formats.R"), env)
  expect_identical(env$.sas2r_output_dates, list(adam.adv = c(adt = "YYMMDD10")))
})

test_that("a program run in another session reads a declared date back through the bundle's list", {
  code <- 'lib_write(adv, "adam", "adv", formats = c(adt = "yymmdd10."))'
  writer <- date_runtime()
  writer$h$lib_write(data.frame(id = 1, adt = 23435), "adam", "adv", formats = dated)
  # The list written into `_sas2r_formats.R` comes from the program's own code.
  reader <- date_runtime(dir = writer$dir, dates = output_date_registry(list(code)))
  expect_identical(reader$h$lib_read("adam", "adv")$adt, 23435)
  # Without it, a separate session sees what is stored.
  bare <- date_runtime(dir = writer$dir)
  expect_s3_class(bare$h$lib_read("adam", "adv")$adt, "Date")
})

test_that("an XPT output carries the date once, with the declared SAS format", {
  rt <- date_runtime(write = "xpt")
  df <- data.frame(USUBJID = c("S1", "S2", "S3"), ADT = sas_day(c("1959-12-31", "2024-02-29", NA)))
  rt$h$lib_write(df, "adam", "adv", formats = dated)
  stored <- haven::read_xpt(rt$file)
  expect_identical(format(stored$ADT), c("1959-12-31", "2024-02-29", NA))
  expect_identical(attr(stored$ADT, "format.sas"), "YYMMDD10")
  expect_identical(rt$h$lib_read("adam", "adv")$ADT, df$ADT)
})

test_that("a stored date is compared by calendar value against the reference", {
  rt <- date_runtime()
  iso <- c("1959-12-31", "1960-01-01", "2024-02-29", NA)
  df <- data.frame(usubjid = paste0("S", 1:4), adt = sas_day(iso), dur = c(7, 8, 29, NA))
  rt$h$lib_write(df, "adam", "adv", formats = dated)
  stored <- readRDS(rt$file)
  reference <- data.frame(usubjid = df$usubjid, adt = structure(as.Date(iso), format.sas = "DATE9"),
                          dur = df$dur)
  # Same dates under another display format pass; the format is cosmetic.
  expect_true(passed(compare_datasets(reference, stored, keys = "usubjid")))
  expect_true(passed(compare_datasets(reference[1:3, ], stored[1:3, ], keys = c("usubjid", "adt"))))
  shifted <- reference
  shifted$adt[3] <- shifted$adt[3] + 1
  expect_false(passed(compare_datasets(shifted, stored, keys = "usubjid")))
  missing <- reference
  missing$adt[2] <- NA
  expect_false(passed(compare_datasets(missing, stored, keys = "usubjid")))
  # A date the program does not declare stays a number and is still reported.
  undeclared <- date_runtime()
  undeclared$h$lib_write(df, "adam", "adv")
  result <- compare_datasets(reference, readRDS(undeclared$file), keys = "usubjid")
  expect_false(passed(result))
  expect_identical(result$structure$kind_mismatch$var, "adt")
})

test_that("the formats a program declares are read from its code and never from running it", {
  code <- c('adae <- lib_read("adam", "adsl")',
            'lib_write(adae, "adam", "ADAE", formats = c(ASTDT = "yymmdd10.", aendt = "DATE9", aval = "8.2"))',
            'lib_write(x, "work", "t", formats = fm)',
            'f <- function(lib, member) lib_write(y, lib, member, formats = c(d = "date9."))',
            'sas2r::lib_write(z, "work", "z", formats = c(d = "e8601da."))',
            'lib_write(w, "work", "w")')
  declared <- declared_write_formats(code)
  expect_identical(vapply(declared, `[[`, "", "dataset"), c("adam.adae", "work.t", NA, "work.z"))
  expect_identical(declared[[1]]$formats, c(astdt = "yymmdd10.", aendt = "DATE9", aval = "8.2"))
  # Not written out: nothing can be read from it.
  expect_null(declared[[2]]$formats)
  # A dataset named at run time is not in the list a later session reads.
  expect_identical(output_date_registry(list(paste(code, collapse = "\n"))),
    list(adam.adae = c(astdt = "YYMMDD10", aendt = "DATE9"), work.z = c(d = "E8601DA")))
  # A later write of the same dataset replaces what an earlier one declared.
  again <- c('lib_write(a, "work", "z", formats = c(d = "date9."))', 'lib_write(a, "work", "z", formats = c(v = "8."))')
  expect_identical(output_date_registry(list(again[1], again[2])), list())
  expect_identical(output_date_registry(list(again[2], again[1])), list(work.z = c(d = "DATE9")))
  # Code that does not parse, or nothing at all, declares nothing and stops nothing.
  expect_identical(declared_write_formats("lib_write(a, "), list())
  expect_identical(output_date_registry(list("lib_write(a, ", NULL, character())), list())
  expect_identical(output_date_registry(), list())
})

test_that("generated code must write its formats out", {
  errors <- function(code) { found <- lint_r_code(code); found$detail[found$level == "error"] }
  expect_identical(errors('lib_write(df, "adam", "adv")'), character())
  expect_identical(errors('lib_write(df, "adam", "adv", formats = c(adt = "yymmdd10."))'), character())
  # A dataset named at run time, as a translated macro does, may still declare.
  expect_identical(errors('lib_write(df, lib, member, formats = c(adt = "yymmdd10."))'), character())
  for (code in c('lib_write(df, "adam", "adv", formats = fm)',
                 'lib_write(df, "adam", "adv", formats = c("yymmdd10."))',
                 'lib_write(df, "adam", "adv", formats = c(adt = paste0("date", 9, ".")))',
                 'lib_write(df, "adam", "adv", formats = readRDS("reference.rds"))',
                 'lib_write(df, "adam", "adv", formats = c(adt = "date9.", "date9."))')) {
    expect_match(errors(code), "write formats out with a name and a string for each column", info = code)
  }
  expect_match(errors('lib_write(df, "adam", "adv", c(adt = "yymmdd10."))'), "takes formats by name")
  expect_match(errors('lib_write(df, "adam", formats = c(adt = "yymmdd10."))'), "needs both a libref and a member")
  expect_match(errors('lib_write(df, "adam", "adv", dataset = "x")'), "aliases are not accepted")
})

test_that("a declared date format must be one the SAS source gives", {
  write <- function(...) paste0('lib_write(adv, "adam", "adv", formats = c(', paste(..., sep = ", "), '))')
  problems <- function(program, code, others = list()) {
    fx <- date_fixture(c(list(p01 = program), others), "adam.adv")
    check_declared_date_formats(code, fx$project)
  }
  step <- function(...) c("data adam.adv;", "  set raw.visits;", ..., "run;")
  # Backed by name: a FORMAT or ATTRIB statement that gives that variable a date format.
  expect_identical(problems(step("  format adt yymmdd10.;"), write('adt = "yymmdd10."')), character())
  expect_identical(problems(step("  attrib adt label='Date' format=date9.;"), write('ADT = "DATE9."')), character())
  # The format may be written with another width: it is the same variable.
  expect_identical(problems(step("  format adt yymmdd10.;"), write('adt = "yymmdd8."')), character())
  # Backed by format: a statement whose variables cannot be read by name.
  expect_identical(problems(step("  format &datevars yymmdd10.;"), write('adt = "yymmdd10."')), character())
  expect_identical(problems(step("  format dt1-dt5 date9.;"), write('dt3 = "date9."')), character())
  expect_identical(problems(c("proc sql;", "  create table adam.adv as select usubjid, mdy(1, 1, 2020) as adt format=date9. from raw.visits;", "quit;"),
    write('adt = "date9."')), character())
  # A declaration inside a macro definition, and one in another program.
  expect_identical(problems(c("%macro mk;", step("  format adt e8601da.;"), "%mend;", "%mk;"), write('adt = "e8601da."')), character())
  expect_identical(problems(step("  x = 1;"), write('adt = "yymmdd10."'),
    others = list(p00 = c("data work.a;", "  adt = 1;", "  format adt yymmdd10.;", "run;"))), character())

  # Not backed: no statement gives that variable a date format.
  for (program in list(step("  x = 1;"), step("  format rfdt yymmdd10.;"), step("  format adt 8.2;"),
                       step("  note = 'format adt yymmdd10.';"), step("  format &datevars date9.;"))) {
    found <- problems(program, write('adt = "yymmdd10."'))
    expect_length(found, 1L)
    expect_match(found, "unbacked_date_format: lib_write() gives the column adt the date format yymmdd10.", fixed = TRUE)
  }
  # Only date formats are checked, and only declarations that are written out.
  expect_identical(problems(step("  x = 1;"), write('aval = "8.2"', 'name = "$char20."')), character())
  expect_identical(problems(step("  x = 1;"), 'lib_write(adv, "adam", "adv", formats = fm)'), character())
  expect_identical(problems(step("  x = 1;"), 'lib_write(adv, "adam", "adv")'), character())
  expect_identical(check_declared_date_formats(write('adt = "date9."'), NULL), character())

  # The revision check applies it, so an invented format is repair feedback.
  fx <- date_fixture(list(p01 = step("  x = 1;")), "adam.adv")
  r_path <- withr::local_tempfile(fileext = ".R")
  writeLines(c('adv <- lib_read("raw", "visits")', write('adt = "yymmdd10."')), r_path)
  checked <- check_program_revision(r_path, contract = list(component_id = "p01"), project = fx$project)
  expect_false(checked$pass)
  expect_true(any(grepl("unbacked_date_format", checked$errors, fixed = TRUE)))
})

test_that("the report says what was declared and what was stored", {
  dir <- withr::local_tempdir()
  save <- function(data, name) { path <- file.path(dir, name); saveRDS(data, path); path }
  day <- sas_day("2024-02-29")
  candidate <- save(data.frame(id = 1, adt = as.Date("2024-02-29"), odd = 0.1, rfdt = day,
                               indt = as.Date("2024-01-01")), "candidate.rds")
  reference <- save(data.frame(id = 1, adt = as.Date("2024-02-29"), odd = 0.1, rfdt = as.Date("2024-02-29"),
                               indt = as.Date("2024-01-01")), "reference.rds")
  record <- date_columns_record(c(adt = "YYMMDD10", odd = "DATE9", gone = "DATE9"), candidate, reference)
  expect_identical(record$typed, list(list(var = "adt", format = "YYMMDD10", evidence = "declared")))
  expect_identical(vapply(record$deferred, `[[`, "", "var"), c("odd", "gone", "rfdt"))
  expect_match(record$deferred[[1]]$reason, "not whole SAS day numbers in a plain numeric column, so it was stored as numeric")
  expect_match(record$deferred[[2]]$reason, "the dataset it wrote has no such column")
  # A date in the reference decides nothing: it is reported, and only where
  # the program wrote a number. `indt` was passed on as a date.
  expect_identical(record$deferred[[3]][c("evidence", "reason")], list(evidence = "reference",
    reason = "the reference stores a date, but the generated program declares no date format for this column"))

  # With no reference, the declared columns are reported all the same.
  alone <- date_columns_record(c(adt = "YYMMDD10"), candidate)
  expect_identical(alone, list(typed = list(list(var = "adt", format = "YYMMDD10", evidence = "declared")),
                               deferred = list()))
  expect_null(date_columns_record(NULL, candidate))
  expect_null(date_columns_record(NULL, candidate, save(data.frame(id = 1), "plain.rds")))
  expect_null(date_columns_record(c(adt = "YYMMDD10"), file.path(dir, "absent.rds"), reference))

  lines <- date_columns_report_lines(list(adam.adv = list(date_columns = record)))
  expect_identical(lines[1], "## Date Columns")
  expect_true("- **adam.adv** `adt`: stored as a date (SAS format YYMMDD10, declared by the generated program)." %in% lines)
  expect_true(any(grepl("**adam.adv** `rfdt`: left as a SAS day number. The reference stores a date", lines, fixed = TRUE)))
  expect_identical(date_columns_report_lines(list(x = list(date_columns = NULL))), character())
})

test_that("a migration run delivers declared dates, with or without a reference, and keeps SAS arithmetic downstream", {
  programs <- list(p01 = adv_program(),
                   p02 = c("data adam.adtte;", "  set adam.adv;", "  aval = adt - rfdt + 1;", "run;"))
  iso <- c("1960-01-01", "2024-02-29", "1959-12-31", NA)
  rfdt <- sas_day(c("1959-12-25", "2024-02-01", "1959-12-25", "2024-02-01"))
  expected <- data.frame(usubjid = paste0("S", 1:4), adtc = iso, rfdt = rfdt,
    adt = structure(as.Date(iso), format.sas = "DATE9"), dur = c(8, 29, 7, NA))
  r <- list(
    p01 = c("a <- as.data.frame(lib_read('raw', 'visits'))",
            "a$adt <- as.numeric(as.Date(a$adtc) - as.Date('1960-01-01'))",
            "a$dur <- a$adt - a$rfdt + 1",
            "lib_write(a, 'work', 'a', formats = c(adt = 'yymmdd10.'))",
            "lib_write(sas_sort(lib_read('work', 'a'), by = 'usubjid'), 'adam', 'adv', formats = c(adt = 'yymmdd10.'))"),
    # A later program must receive the SAS day number, not a Date. It passes
    # the date on, so it declares the format again.
    p02 = c("t <- lib_read('adam', 'adv')",
            "stopifnot(is.numeric(t$adt), !inherits(t$adt, 'Date'), is.null(attr(t$adt, 'format.sas')))",
            "t$aval <- t$adt - t$rfdt + 1",
            "lib_write(t, 'adam', 'adtte', formats = c(adt = 'yymmdd10.'))"))
  run <- function(references) {
    fx <- date_fixture(programs, c("adam.adv", "adam.adtte"), references, envir = parent.frame())
    saveRDS(expected[c(3, 1, 2, 4), c("usubjid", "adtc", "rfdt")], file.path(fx$root, "inputs", "visits.rds"))
    state <- new_migration_state(fx$project, file.path(fx$root, "migration"), config = fx$config)
    state$output_contracts <- fx$project$output_contracts
    for (id in names(programs)) {
      sas <- paste(programs[[id]], collapse = "\n")
      code <- paste(r[[id]], collapse = "\n")
      r_path <- file.path(fx$root, paste0(id, ".R"))
      writeLines(code, r_path)
      # Each declaration is one the SAS source gives.
      expect_identical(check_declared_date_formats(code, fx$project), character())
      binding <- new_component_binding(migration_hash(sas), migration_hash(code),
        migration_hash("helpers"), migration_hash("review"), migration_hash("closure"))
      state$selected_revisions[[id]] <- list(component_id = id, revision_id = "r1", r_code = code,
        staged_file = paste0(id, ".R"), r_path = r_path, binding = binding,
        contract = list(component_id = id, staged_file = paste0(id, ".R"), sas_text = sas, binding = binding))
      state$histories[[id]] <- record_completed_review(new_component_evidence_history(id, binding))
    }
    attempt <- run_bundle_attempt(state, sequence = 1L)
    assessment <- assess_final_outputs(state$output_contracts, attempt, state$graph, state$histories,
      comparison_rules = fx$config$outputs, project = state$project)
    list(fx = fx, state = state, attempt = attempt, assessment = assessment)
  }

  with <- run(list(adam.adv = expected, adam.adtte = cbind(expected, aval = expected$dur)))
  expect_true(with$attempt$passed)
  expect_true(with$assessment$targets$adam.adv$passed)
  expect_true(with$assessment$targets$adam.adtte$passed)
  expect_identical(with$assessment$targets$adam.adv$date_columns,
    list(typed = list(list(var = "adt", format = "YYMMDD10", evidence = "declared")), deferred = list()))
  # The bundle lists the declared dates for a program run on its own.
  listed <- new.env()
  sys.source(file.path(with$attempt$attempt_dir, "bundle", "_sas2r_formats.R"), listed)
  expect_identical(listed$.sas2r_output_dates,
    list(work.a = c(adt = "YYMMDD10"), adam.adv = c(adt = "YYMMDD10"), adam.adtte = c(adt = "YYMMDD10")))

  adv <- readRDS(with$assessment$targets$adam.adv$candidate_path)
  expect_s3_class(adv$adt, "Date")
  expect_identical(format(adv$adt), iso)
  expect_identical(adv$dur, c(8, 29, 7, NA))
  adtte <- readRDS(with$assessment$targets$adam.adtte$candidate_path)
  expect_s3_class(adtte$adt, "Date")
  expect_identical(adtte$aval, c(8, 29, 7, NA))

  # The delivered file is the file the programs wrote, byte for byte.
  with$state$selected_attempt <- with$attempt
  with$state$execute <- TRUE
  delivered <- materialize_run_outputs(with$state)
  saved <- file.path(with$state$paths$outputs, "datasets", "adam", "adv.rds")
  expect_identical(unname(cli::hash_file_sha256(saved)), unname(delivered$adam.adv$sha256))
  expect_s3_class(readRDS(saved)$adt, "Date")

  lines <- date_columns_report_lines(with$assessment$targets)
  expect_true(any(grepl("**adam.adv** `adt`: stored as a date (SAS format YYMMDD10, declared by the generated program)",
                        lines, fixed = TRUE)))

  # The same programs with no reference write the same dates: a reference
  # checks the result and decides nothing about it.
  without <- run(list())
  expect_true(without$attempt$passed)
  bare <- readRDS(without$assessment$targets$adam.adv$candidate_path)
  expect_identical(bare$adt, adv$adt)
  expect_identical(readRDS(without$assessment$targets$adam.adtte$candidate_path)$adt, adtte$adt)
  expect_identical(without$assessment$targets$adam.adv$date_columns$typed,
                   with$assessment$targets$adam.adv$date_columns$typed)
})
