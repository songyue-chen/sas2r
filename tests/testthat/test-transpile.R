test_that("transpile writes staged files with banner, helpers, and stubs", {
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
})

test_that("manifest tiers: t1 for supported, stub with reason for the rest", {
  p <- sas_project(test_path("fixtures", "scanner-project"))
  out <- withr::local_tempdir()
  m <- sas_transpile(p, out)$manifest
  expect_true(all(m$tier %in% c("t1", "stub")))
  expect_true(any(m$tier == "stub" & m$reason == "macro_residue"))  # 01_adsl macro call
  expect_true(any(m$tier == "t1"))
  expect_true(all(m$lint_errors == 0L))
})

test_that("stub blocks preserve the original SAS visibly", {
  p <- sas_project(test_path("fixtures", "scanner-project"))
  out <- withr::local_tempdir()
  sas_transpile(p, out)
  staged <- paste(readLines(file.path(out, "02_summary.R")), collapse = "\n")
  expect_match(staged, "sas2r:untranslated")
  expect_match(staged, "ghost_macro")
})

test_that("the t1 fixture file executes end to end with correct semantics", {
  skip_if_not_installed("dplyr")
  p <- sas_project(test_path("fixtures", "scanner-project"))
  out <- withr::local_tempdir()
  sas_transpile(p, out)
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

test_that("libname paths with backslashes escape safely in registry", {
  # F3: backslash paths in libnames. `C:\data\adam` is not a directory on the
  # translating machine, so the libref comes back unbound and the path reaches
  # the registry through the commented-entry route. The assertion names the
  # path so the escaping it guards is actually exercised: without it the test
  # passes on a file that mentions no path at all.
  dir <- withr::local_tempdir()
  writeLines("libname adam 'C:\\data\\adam'; data a; set adam.adsl; run;", file.path(dir, "lib.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(dir), out)
  reg_lines <- readLines(file.path(out, "autoexec.R"))
  reg <- paste(reg_lines, collapse = "\n")
  expect_no_error(parse(text = reg))
  expect_true(grepl("C:\\\\data\\\\adam", reg, fixed = TRUE))
  expect_false(grepl("C:\\data\\adam", reg, fixed = TRUE))
  # F21: libname in manifest
  m <- tr$manifest
  expect_true(any(m$flags == "registry" & m$tier == "t1"))
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

test_that("include paths are confined when emitted and again at run time", {
  expect_error(emit_include_call("/abs/mod.R"),
               class = "sas2r_include_path_error")
  expect_error(emit_include_call("../outside/mod.R"),
               class = "sas2r_include_path_error")
  expect_identical(emit_include_call(file.path("inc", "prep.R")),
                   'sas2r_source_include("inc/prep.R", envir = environment())')

  out <- withr::local_tempdir()
  write_helpers(out)
  e <- new.env(parent = globalenv())
  sys.source(file.path(out, "sas2r-helpers.R"), e)
  for (bad in c("/etc/passwd", "../escape.R", "./mod.R", "")) {
    expect_error(e$sas2r_source_include(bad, envir = e),
                 class = "sas2r_include_path_error", info = bad)
  }
  # A bundle with no registry above it stops instead of walking forever.
  withr::local_dir(withr::local_tempdir())
  expect_error(e$sas2r_source_include("mod.R", envir = e),
               class = "sas2r_include_root_error")
})

# ---- Fix round 1: findings 1-3 and the minor items ------------------------

# A `%INCLUDE` standing inside a macro, DATA-step, or PROC-step body cannot
# become a call where it stands, so the target module is unreachable. It must
# not reach the manifest as translated code a human could approve.

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

test_that("sourcing a driver into a sandbox leaves globalenv untouched", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  writeLines("data work.from_include; set work.input; run;",
             file.path(root, "inc", "prep.sas"))
  writeLines(c("%include 'inc/prep.sas';",
               "data work.after; set work.from_include; run;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  sas_transpile(sas_project(root, recursive = TRUE), out)

  watched <- c(".sas2r_registry", "lib_read", "lib_write", "from_include",
               "sas2r_source_include")
  withr::defer(suppressWarnings(rm(list = intersect(watched, ls(globalenv(), all.names = TRUE)),
                                   envir = globalenv())))
  expect_false(any(vapply(watched, exists, logical(1),
                          envir = globalenv(), inherits = FALSE)))

  withr::local_dir(out)
  e <- new.env(parent = globalenv())
  sys.source("sas2r-helpers.R", e)
  e$.sas2r_registry <- list(work = list(path = ".", engine = "rds", write = "rds"))
  e$lib_read <- function(libref, member) {
    f <- paste0(member, ".rds")
    if (file.exists(f)) readRDS(f) else data.frame(x = 1:3)
  }
  runnable <- readLines("driver.R", warn = FALSE)
  writeLines(runnable[!grepl("^source[(]", runnable)], "driver_run.R")
  sys.source("driver_run.R", envir = e)

  expect_true(exists("from_include", envir = e, inherits = FALSE))
  # The included module did not re-enter the bootstrap, so nothing escaped.
  expect_false(any(vapply(watched, exists, logical(1),
                          envir = globalenv(), inherits = FALSE)))
})

# ---- Fix round 2: findings 1-3 and the minor items ------------------------

test_that("a live chain of includes stays translated all the way down", {
  # The other side of the fixed point: reachability must still propagate.
  root <- withr::local_tempdir()
  writeLines("data work.deep; set work.input; run;", file.path(root, "c.sas"))
  writeLines("%include 'c.sas';", file.path(root, "b.sas"))
  writeLines("%include 'b.sas';", file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  expect_true(all(tr$manifest$tier == "t1"))
  expect_false(any(grepl("orphan_module", tr$manifest$flags)))
  expect_true(any(grepl('^sas2r_source_include\\("c\\.R"',
                        readLines(file.path(out, "b.R"), warn = FALSE))))
})

# ---- Fix round 3: Item 1 and the minor items -------------------------------

test_that("generated registry uses the same selected binding as project lineage", {
  root <- withr::local_tempdir()
  source_lib <- file.path(root, "source"); dir.create(source_lib)
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(sprintf("libname adam %s; data x; set adam.adsl; run;",
                     deparse(source_lib)), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  out <- withr::local_tempdir(); sas_transpile(p, out)
  text <- paste(readLines(file.path(out, "p.R")), collapse = "\n")
  expect_match(text, "sas2r_libname_assign")
  # The emitted path is the *canonical* selected one, which is not the literal
  # string withr::local_tempdir() hands back on every platform: macOS resolves
  # /var to /private/var, and R's own tempdir() spelling carries a doubled
  # separator that normalizePath() collapses. Both spellings of the fallback
  # are refused below, so the discrimination this test exists for -- the
  # accessible source library wins, and the configured one never reaches
  # generated R -- is unchanged.
  canonical <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  expect_match(text, canonical(source_lib), fixed = TRUE)
  expect_false(grepl(fallback, text, fixed = TRUE))
  expect_false(grepl(canonical(fallback), text, fixed = TRUE))
  # The project's own lineage and the generated module agree, which is the
  # whole point of the single projection: both name the source library.
  expect_identical(
    resolve_libref_at(p$libref_registry, "adam",
                      file.path(root, "p.sas"), 1L)$selected_path,
    canonical(source_lib))
})

test_that("a program runs with Rscript from its folder, and not from elsewhere before the autoexec", {
  # ADR 0004: programs run where their autoexec is, as SAS programs do. A batch
  # job runs from the run folder; from anywhere else the first line fails
  # because autoexec.R is not there.
  bundle <- withr::local_tempdir()
  write_helpers(bundle)
  write_formats(list(), bundle)
  write_autoexec(NULL, bundle)
  writeLines(c(module_bootstrap(), 'cat("BOOT_OK")'), file.path(bundle, "prog.R"))

  res <- callr::rscript(file.path(bundle, "prog.R"), wd = bundle,
                        show = FALSE, fail_on_status = FALSE)
  expect_identical(res$status, 0L)
  expect_match(res$stdout, "BOOT_OK", fixed = TRUE)

  elsewhere <- withr::local_tempdir()
  res <- callr::rscript(file.path(bundle, "prog.R"), wd = elsewhere,
                        show = FALSE, fail_on_status = FALSE)
  expect_false(identical(res$status, 0L))
  expect_match(res$stderr, "autoexec\\.R")
})
