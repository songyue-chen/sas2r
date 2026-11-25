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

decided <- function(fx, target = "adam.adv") source_date_outputs(fx$project)[[target]]

test_that("the date formats are exactly the ones haven reads back as a date", {
  formats <- c("DATE", "DATE9", "DATE11", "YYMMDD10", "YYMMDDD10", "YYMMDDN8", "YYMMDDS10",
    "MMDDYY10", "MMDDYYS10", "DDMMYY10", "DDMMYYP10", "E8601DA", "E8601DA10", "B8601DA8",
    "IS8601DA10", "WEEKDATE29", "WEEKDATX", "DATETIME20", "E8601DT", "TIME8", "MONYY7",
    "YYQ6", "YYMM", "WORDDATE18", "JULIAN7", "NLDATE", "YEAR4", "BEST12")
  df <- as.data.frame(stats::setNames(
    lapply(formats, function(f) structure(c(19000, 0), format.sas = f)), paste0("v", seq_along(formats))))
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
  expect_identical(unname(parse_rename_statement("rename trt01a = trta trt01an=trtan")[, "new"]),
                   c("trta", "trtan"))
  expect_null(parse_rename_statement("rename &old = new"))
  expect_null(parse_rename_statement("rename a1-a3 = b1-b3"))
})

test_that("a declared date is typed from the source alone when every step is read exactly", {
  fx <- date_fixture(list(p01 = adv_program()), "adam.adv")
  got <- decided(fx)
  expect_identical(got$typed$var, "adt")
  expect_identical(got$typed$format, "YYMMDD10")
  expect_identical(got$typed$evidence, "source")
  expect_identical(got$typed$line, 5L)
  expect_identical(nrow(got$deferred), 0L)
  # The duration computed from the date, and the undeclared reference date,
  # are not in the list.
  expect_identical(output_date_registry(fx$project), list(adam.adv = c(adt = "YYMMDD10")))
})

test_that("the declaration that reaches the output decides", {
  typed <- function(...) decided(date_fixture(list(p01 = adv_program(...)), "adam.adv"))$typed$format
  reason <- function(...) decided(date_fixture(list(p01 = adv_program(...)), "adam.adv"))$deferred$reason
  copy <- function(extra = character(), set = "  set work.a;") c("data work.a;", set, extra, "run;")

  expect_identical(typed(middle = copy()), "YYMMDD10")
  expect_identical(typed(declare = "  format adt best12.;", middle = copy("  format adt date9.;")), "DATE9")
  expect_identical(typed(middle = copy("  rename dur = duration;")), "YYMMDD10")
  expect_identical(typed(middle = c("%let note = &sysdate;", "options nocenter;")), "YYMMDD10")
  expect_identical(typed(middle = c("data work.other;", "  set raw.visits;", "run;")), "YYMMDD10")
  expect_identical(typed(middle = c("data work.b;", "  set raw.visits;", "run;",
                                    copy(set = "  set work.a work.b;"))), "YYMMDD10")

  expect_match(reason(middle = copy("  format adt;")), "without a date format")
  expect_match(reason(middle = copy("  format adt best12.;")), "without a date format")
  expect_match(reason(middle = copy("  format _numeric_ best12.;")), "not read exactly")
  expect_match(reason(middle = copy("  rename adt = visitdt;")), "RENAME")
  expect_match(reason(middle = copy(set = "  set work.a(rename=(adt=visitdt));")), "RENAME=")
  expect_match(reason(middle = c("data work.b;", "  set raw.visits;", "run;",
                                 copy(set = "  set work.b work.a;"))), "not created earlier")
  expect_match(reason(middle = "%tidy(work.a);"), "macro-generated code at line 7")
  expect_match(reason(middle = c("proc datasets lib=work;", "  modify a;", "  format adt;", "quit;")),
               "change datasets in place")
  expect_match(reason(middle = copy("  retain &keepvars;")), "macro-generated code")
  expect_match(reason(last = c("proc sort data=work.a out=adam.adv(rename=(adt=x));", "  by usubjid;", "run;")),
               "PROC SORT")
  expect_match(reason(last = c("proc transpose data=work.a out=adam.adv;", "run;")),
               "not one whose output formats are followed")
  expect_true(all(grepl("no reference column to confirm", reason(middle = "%tidy(work.a);"))))
})

test_that("macro-generated code on the way needs the reference to store a date", {
  program <- c(
    "%macro make_empty(dataset=);", "  data EMPTY_&dataset;",
    "    attrib usubjid label='Subject' length=$8;", "    if 0;", "  run;", "%mend make_empty;",
    "%make_empty(dataset=ADV);",
    "data adv;", "  set raw.visits;", "  adt = input(adtc, yymmdd10.);", "  dur = adt - rfdt + 1;",
    "  format adt yymmdd10.;", "run;",
    "data adv;", "  retain &advKEEPSTRING;", "  set EMPTY_adv adv;", "run;",
    "proc sort data=adv(keep=&advKEEPSTRING) out=adam.adv;", "  by usubjid;", "run;")
  reference <- function(adt) data.frame(usubjid = "S1", adt = adt, dur = 8, visitdt = as.Date("2024-01-01"))

  none <- decided(date_fixture(list(p01 = program), "adam.adv"))
  expect_identical(nrow(none$typed), 0L)
  expect_match(none$deferred$reason, "macro-generated code")
  expect_match(none$deferred$reason, "no reference column to confirm")

  dated <- decided(date_fixture(list(p01 = program), "adam.adv",
                                list(adam.adv = reference(as.Date("2024-02-29")))))
  expect_identical(dated$typed[, c("var", "format", "evidence")],
                   tibble::tibble(var = "adt", format = "YYMMDD10", evidence = "reference"))
  # A date in the reference with no declaration in the source is reported and
  # left alone: the reference alone decides nothing.
  expect_identical(dated$deferred$var, "visitdt")
  expect_match(dated$deferred$reason, "no date format declaration")

  number <- date_fixture(list(p01 = program), "adam.adv", list(adam.adv = reference(23435)))
  expect_identical(nrow(decided(number)$typed), 0L)
  expect_match(decided(number)$deferred$reason[1], "reference stores this column as numeric")
  expect_identical(output_date_registry(number$project), list())

  # A source that also removes the format is not settled by the reference.
  removed <- sub("  retain &advKEEPSTRING;", "  retain &advKEEPSTRING; format adt;", program, fixed = TRUE)
  both <- decided(date_fixture(list(p01 = removed), "adam.adv",
                               list(adam.adv = reference(as.Date("2024-02-29")))))
  expect_identical(nrow(both$typed), 0L)
  expect_match(both$deferred$reason[both$deferred$var == "adt"], "different format")
})

test_that("only date formats are declarations, and a traced date stays a number when the reference has one", {
  other <- function(declare) decided(date_fixture(list(p01 = adv_program(declare = declare)), "adam.adv"))
  for (declare in c("  format adt datetime20.;", "  format adt monyy7.;", "  format adt visitfmt.;",
                    "  format dur 8.;", "  informat adt yymmdd10.;")) {
    got <- other(declare)
    expect_identical(c(nrow(got$typed), nrow(got$deferred)), c(0L, 0L), info = declare)
  }
  conflict <- decided(date_fixture(list(p01 = adv_program()), "adam.adv",
    list(adam.adv = data.frame(usubjid = "S1", adt = 23435))))
  expect_identical(nrow(conflict$typed), 0L)
  expect_match(conflict$deferred$reason, "reference stores this column as numeric")
})

test_that("a format declared for an upstream output is inherited only where the reference confirms it", {
  programs <- list(p01 = adv_program(),
                   p02 = c("data adam.adtte;", "  set adam.adv;", "  aval = adt - rfdt + 1;", "run;"))
  alone <- date_fixture(programs, c("adam.adv", "adam.adtte"))
  expect_identical(output_date_registry(alone$project), list(adam.adv = c(adt = "YYMMDD10")))
  expect_identical(nrow(decided(alone, "adam.adtte")$deferred), 0L)

  confirmed <- date_fixture(programs, c("adam.adv", "adam.adtte"),
    list(adam.adtte = data.frame(usubjid = "S1", adt = as.Date("2024-02-29"), aval = 29)))
  expect_identical(output_date_registry(confirmed$project)$adam.adtte, c(adt = "YYMMDD10"))
  expect_identical(decided(confirmed, "adam.adtte")$typed$evidence, "reference")

  absent <- date_fixture(programs, c("adam.adv", "adam.adtte"),
    list(adam.adtte = data.frame(usubjid = "S1", aval = 29)))
  expect_null(output_date_registry(absent$project)$adam.adtte)
})

test_that("a failure while reading declarations leaves every column a number", {
  fx <- date_fixture(list(p01 = adv_program()), "adam.adv")
  broken <- fx$project
  broken$lineage <- "not a table"
  expect_identical(output_date_registry(broken), list())
  expect_identical(source_date_outputs(list()), list())
})

# The runtime a bundle carries, with one writable library.
date_runtime <- function(write = "rds", dates = list(adam.adv = c(adt = "YYMMDD10")),
                         envir = parent.frame()) {
  h <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), h)
  dir <- withr::local_tempdir(.local_envir = envir)
  h$.sas2r_registry <- list(adam = list(read_path = dir, write_path = dir, engine = write, write = write))
  if (length(dates)) h$.sas2r_output_dates <- dates
  list(h = h, file = file.path(dir, paste0("adv.", write)))
}

test_that("lib_write stores a declared date as a Date and lib_read returns the SAS day number", {
  rt <- date_runtime()
  iso <- c("1959-12-31", "1960-01-01", "2024-02-29", NA, "2026-11-24")
  df <- data.frame(usubjid = paste0("S", 1:5), adt = sas_day(iso), rfdt = sas_day("1959-12-25"))
  df$adt[4] <- haven::tagged_na("a")
  attr(df$adt, "label") <- "Analysis date"
  df$dur <- df$adt - df$rfdt + 1
  returned <- rt$h$lib_write(df, "adam", "adv")
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

  back <- rt$h$lib_read("adam", "adv")
  expect_identical(as.data.frame(back)$adt, df$adt)
  expect_identical(haven::na_tag(back$adt)[4], "a")
  # Date arithmetic in a later program is the SAS arithmetic, with no format
  # carried onto the result.
  aval <- back$adt - back$rfdt + 1
  expect_identical(as.vector(aval), c(7, 8, 23443, NA, 24442))
  expect_null(attr(aval, "format.sas"))
})

test_that("only a plain number that converts exactly is stored as a date", {
  rt <- date_runtime()
  write <- function(adt) {
    rt$h$lib_write(data.frame(id = seq_along(adt), adt = adt), "adam", "adv")
    readRDS(rt$file)$adt
  }
  # 0.1 does not survive the shift between the two origins exactly, so the
  # column is left alone; 19000.5 does, and reads back as the same number.
  expect_identical(write(c(0.1, 19001)), c(0.1, 19001))
  expect_s3_class(write(c(19000.5, 19001)), "Date")
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(19000.5, 19001))
  expect_identical(write(c("2024-02-29", "2024-03-01")), c("2024-02-29", "2024-03-01"))
  expect_identical(write(c(NA, NA)), c(NA, NA))
  already <- as.Date(c("2024-02-29", NA))
  expect_identical(write(already), already)
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(sas_day("2024-02-29"), NA))
  whole <- write(c(23435L, NA))
  expect_identical(format(whole), c("2024-02-29", NA))
  expect_identical(rt$h$lib_read("adam", "adv")$adt, c(23435, NA))

  rt$h$lib_write(data.frame(id = integer(), adt = numeric()), "adam", "adv")
  expect_s3_class(readRDS(rt$file)$adt, "Date")
  expect_identical(rt$h$lib_read("adam", "adv")$adt, numeric())

  # An upper-case member and column match the lower-case declaration. This
  # uses its own library: `adv` and `ADV` are one file on a case-insensitive
  # file system and two files on a case-sensitive one.
  upper <- date_runtime()
  upper$h$lib_write(data.frame(ID = 1, ADT = 23435), "adam", "ADV")
  stored <- readRDS(file.path(dirname(upper$file), "ADV.rds"))
  expect_identical(format(stored$ADT), "2024-02-29")
  expect_identical(upper$h$lib_read("adam", "ADV")$ADT, 23435)
})

test_that("nothing changes for a dataset or a bundle without declared dates", {
  rt <- date_runtime(dates = list(adam.other = c(adt = "DATE9")))
  rt$h$lib_write(data.frame(id = 1, adt = 23435), "adam", "adv")
  expect_identical(readRDS(rt$file)$adt, 23435)
  bare <- date_runtime(dates = list())
  bare$h$lib_write(data.frame(id = 1, adt = 23435), "adam", "adv")
  expect_identical(readRDS(bare$file)$adt, 23435)
  kept <- data.frame(id = 1, adt = as.Date("2024-02-29"))
  bare$h$lib_write(kept, "adam", "adv")
  expect_identical(bare$h$lib_read("adam", "adv")$adt, kept$adt)

  dir <- withr::local_tempdir()
  write_formats(list(), dir)
  expect_identical(readLines(file.path(dir, "_sas2r_formats.R")),
                   c("# Generated by sas2r -- format catalog.", ".sas2r_formats <- list()"))
  write_formats(list(), dir, list(adam.adv = c(adt = "YYMMDD10")))
  env <- new.env()
  sys.source(file.path(dir, "_sas2r_formats.R"), env)
  expect_identical(env$.sas2r_output_dates, list(adam.adv = c(adt = "YYMMDD10")))
})

test_that("an XPT output carries the date once, with the declared SAS format", {
  rt <- date_runtime(write = "xpt")
  df <- data.frame(USUBJID = c("S1", "S2", "S3"), ADT = sas_day(c("1959-12-31", "2024-02-29", NA)))
  rt$h$lib_write(df, "adam", "adv")
  stored <- haven::read_xpt(rt$file)
  expect_identical(format(stored$ADT), c("1959-12-31", "2024-02-29", NA))
  expect_identical(attr(stored$ADT, "format.sas"), "YYMMDD10")
  expect_identical(rt$h$lib_read("adam", "adv")$ADT, df$ADT)
})

test_that("a stored date is compared by calendar value against the reference", {
  rt <- date_runtime()
  iso <- c("1959-12-31", "1960-01-01", "2024-02-29", NA)
  df <- data.frame(usubjid = paste0("S", 1:4), adt = sas_day(iso), dur = c(7, 8, 29, NA))
  rt$h$lib_write(df, "adam", "adv")
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
  # A date the source does not declare stays a number and is still reported.
  undeclared <- date_runtime(dates = list())
  undeclared$h$lib_write(df, "adam", "adv")
  result <- compare_datasets(reference, readRDS(undeclared$file), keys = "usubjid")
  expect_false(passed(result))
  expect_identical(result$structure$kind_mismatch$var, "adt")
})

test_that("a migration run delivers typed dates and keeps SAS arithmetic downstream", {
  programs <- list(p01 = adv_program(),
                   p02 = c("data adam.adtte;", "  set adam.adv;", "  aval = adt - rfdt + 1;", "run;"))
  iso <- c("1960-01-01", "2024-02-29", "1959-12-31", NA)
  rfdt <- sas_day(c("1959-12-25", "2024-02-01", "1959-12-25", "2024-02-01"))
  expected <- data.frame(usubjid = paste0("S", 1:4), adtc = iso, rfdt = rfdt,
    adt = structure(as.Date(iso), format.sas = "DATE9"), dur = c(8, 29, 7, NA))
  fx <- date_fixture(programs, c("adam.adv", "adam.adtte"),
    list(adam.adv = expected, adam.adtte = cbind(expected, aval = expected$dur)))
  saveRDS(expected[c(3, 1, 2, 4), c("usubjid", "adtc", "rfdt")], file.path(fx$root, "inputs", "visits.rds"))
  r <- list(
    p01 = c("a <- as.data.frame(lib_read('raw', 'visits'))",
            "a$adt <- as.numeric(as.Date(a$adtc) - as.Date('1960-01-01'))",
            "a$dur <- a$adt - a$rfdt + 1",
            "lib_write(a, 'work', 'a')",
            "lib_write(sas_sort(lib_read('work', 'a'), by = 'usubjid'), 'adam', 'adv')"),
    # A later program must receive the SAS day number, not a Date.
    p02 = c("t <- lib_read('adam', 'adv')",
            "stopifnot(is.numeric(t$adt), !inherits(t$adt, 'Date'), is.null(attr(t$adt, 'format.sas')))",
            "t$aval <- t$adt - t$rfdt + 1",
            "lib_write(t, 'adam', 'adtte')"))
  state <- new_migration_state(fx$project, file.path(fx$root, "migration"), config = fx$config)
  state$output_contracts <- fx$project$output_contracts
  for (id in names(programs)) {
    sas <- paste(programs[[id]], collapse = "\n")
    code <- paste(r[[id]], collapse = "\n")
    r_path <- file.path(fx$root, paste0(id, ".R"))
    writeLines(code, r_path)
    binding <- new_component_binding(migration_hash(sas), migration_hash(code),
      migration_hash("helpers"), migration_hash("review"), migration_hash("closure"))
    state$selected_revisions[[id]] <- list(component_id = id, revision_id = "r1", r_code = code,
      staged_file = paste0(id, ".R"), r_path = r_path, binding = binding,
      contract = list(component_id = id, staged_file = paste0(id, ".R"), sas_text = sas, binding = binding))
    state$histories[[id]] <- record_completed_review(new_component_evidence_history(id, binding))
  }
  attempt <- run_bundle_attempt(state, sequence = 1L)
  expect_true(attempt$passed)
  assessment <- assess_final_outputs(state$output_contracts, attempt, state$graph, state$histories,
    comparison_rules = fx$config$outputs, project = state$project)
  expect_true(assessment$targets$adam.adv$passed)
  expect_true(assessment$targets$adam.adtte$passed)
  expect_identical(assessment$targets$adam.adv$date_columns$typed[[1]][c("var", "format", "evidence", "file", "line")],
    list(var = "adt", format = "YYMMDD10", evidence = "reference", file = "p01.sas", line = 5L))

  adv <- readRDS(assessment$targets$adam.adv$candidate_path)
  expect_s3_class(adv$adt, "Date")
  expect_identical(format(adv$adt), iso)
  expect_identical(adv$dur, c(8, 29, 7, NA))
  adtte <- readRDS(assessment$targets$adam.adtte$candidate_path)
  expect_s3_class(adtte$adt, "Date")
  expect_identical(adtte$aval, c(8, 29, 7, NA))

  # The delivered file is the file the programs wrote, byte for byte.
  state$selected_attempt <- attempt
  state$execute <- TRUE
  delivered <- materialize_run_outputs(state)
  saved <- file.path(state$paths$outputs, "datasets", "adam", "adv.rds")
  expect_identical(unname(cli::hash_file_sha256(saved)), unname(delivered$adam.adv$sha256))
  expect_s3_class(readRDS(saved)$adt, "Date")

  lines <- date_columns_report_lines(assessment$targets)
  expect_identical(lines[1], "## Date Columns")
  expect_true(any(grepl("**adam.adv** `adt`: stored as a date (SAS format YYMMDD10, declared at p01.sas:5",
                        lines, fixed = TRUE)))
  expect_identical(date_columns_report_lines(list(x = list(date_columns = NULL))), character())
})
