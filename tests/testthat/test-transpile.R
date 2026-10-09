test_that("the t1 fixture file executes end to end with correct semantics", {
  skip_if_not_installed("dplyr")
  p <- sas_project(test_path("fixtures", "scanner-project"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(p, out)
  expect_s3_class(tr, "sas2r_transpilation")
  expect_true(file.exists(file.path(out, "sas2r-helpers.R")))
  expect_true(file.exists(file.path(out, "autoexec.R")))
  staged <- readLines(file.path(out, "03_t1.R"))
  expect_match(staged[1:3], "NOT VERIFIED", all = FALSE)
  expect_true(any(grepl("sas_sort", staged)))
  expect_true(any(grepl("eldfl", staged)))
  m <- tr$manifest
  expect_true(all(m$tier %in% c("t1", "stub")))
  expect_true(any(m$tier == "stub" & m$reason == "macro_residue"))
  expect_true(any(m$tier == "t1"))
  expect_true(all(m$lint_errors == 0L))
  stub <- paste(readLines(file.path(out, "02_summary.R")), collapse = "\n")
  expect_match(stub, "sas2r:untranslated")
  expect_match(stub, "ghost_macro")
  e <- new.env(parent = globalenv())
  sys.source(file.path(out, "sas2r-helpers.R"), e)
  e$.sas2r_registry <- list(work = list(path = out, engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) tibble::tibble(
    usubjid = c("01", "02", "03"), sex = c("F", "M", "F"),
    saffl = c("Y", "Y", "N"), age = c(70, NA, 80))
  code <- readLines(file.path(out, "03_t1.R"))
  code <- code[!grepl("^source\\(", code)]
  eval(parse(text = paste(code, collapse = "\n")), envir = e)
  eld <- get("elderly", envir = e)
  expect_identical(eld$usubjid, c("01", "02"))
  expect_identical(eld$eldfl, c("Y", NA))
  expect_identical(names(eld), c("usubjid", "sex", "eldfl"))
})

test_that("untranslatable expressions stub cleanly without aborting transpile", {
  # F1: untranslatable expression does not abort whole run
  dir <- withr::local_tempdir()
  writeLines("data good; set work.x; a = 1; run;\ndata bad; set work.x; z = put(y, best.); run;\ndata good2; set work.x; b = 2; run;",
             file.path(dir, "mixed.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(dir), out)
  m <- tr$manifest
  expect_true(any(m$tier == "stub" & m$reason == "expr_parse_failed"))
  expect_true(sum(m$tier == "t1") >= 2L)
  expect_true(file.exists(file.path(out, "mixed.R")))
})

test_that("an unmapped SAS function stubs with a named refusal, never base R passthrough", {
  dir <- withr::local_tempdir()
  writeLines("data bad; set work.x; z = scan(y, 2); run;",
             file.path(dir, "prog.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(dir), out)
  m <- tr$manifest
  expect_true(any(m$tier == "stub" & m$reason == "unmapped_function:scan"))
  # The stub quotes the SAS source in comments; no executable line may carry
  # the base R scan() the old passthrough produced.
  code_lines <- readLines(file.path(out, "prog.R"))
  executable <- code_lines[!grepl("^\\s*#", code_lines)]
  expect_no_match(paste(executable, collapse = "\n"), "scan\\(")
})

test_that("resolved include units are translated and sourced at the include site", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  writeLines("data work.from_include; set work.input; run;",
             file.path(root, "inc", "prep.sas"))
  writeLines(c("data work.before; set work.input; run;",
               "%include 'inc/prep.sas';",
               "data work.after; set work.from_include; run;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()

  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)
  inc_rows <- tr$manifest[basename(tr$manifest$file) == "prep.sas", ]
  expect_true(nrow(inc_rows) > 0L)
  expect_false(any(inc_rows$reason == "included_file", na.rm = TRUE))
  expect_true(all(!is.na(inc_rows$staged_file)))
  expect_true(all(file.exists(file.path(out, unique(inc_rows$staged_file)))))

  driver <- readLines(file.path(out, "driver.R"), warn = FALSE)
  call_line <- grep("sas2r_source_include", driver)
  before_line <- grep("before <-", driver)
  after_line <- grep("after <-", driver)
  expect_true(before_line < call_line && call_line < after_line)
})

test_that("a non-.sas include target stages as .R and leaves its source intact", {
  # Finding 2: the staged module used to keep the target's own extension, so
  # transpiling in place overwrote the user's SAS source with generated R.
  root <- withr::local_tempdir()
  inc <- file.path(root, "prep.inc")
  writeLines("data work.hidden; set work.input; run;", inc)
  writeLines(c("%include 'prep.inc';", "data work.after; set work.hidden; run;"),
             file.path(root, "driver.sas"))
  before <- readBin(inc, "raw", file.size(inc))

  tr <- sas_transpile(sas_project(root), root)   # transpile in place

  expect_identical(readBin(inc, "raw", file.size(inc)), before)
  expect_true("prep.R" %in% tr$manifest$staged_file)
  expect_false(any(tr$manifest$staged_file == "prep.inc"))
  expect_true(any(grepl('sas2r_source_include\\("prep\\.R"',
                        readLines(file.path(root, "driver.R"), warn = FALSE))))
})

test_that("a staged module that would overwrite a scanned source is refused", {
  # Second half of Finding 2: the guard, not the naming rule, is what makes a
  # future change to staged naming safe.
  root <- withr::local_tempdir()
  writeLines("data work.hidden; set work.input; run;", file.path(root, "prep.R"))
  writeLines("%include 'prep.R';", file.path(root, "driver.sas"))
  expect_error(sas_transpile(sas_project(root), root),
               class = "sas2r_staged_path_overwrites_source")
  expect_identical(readLines(file.path(root, "prep.R"), warn = FALSE),
                   "data work.hidden; set work.input; run;")
})
