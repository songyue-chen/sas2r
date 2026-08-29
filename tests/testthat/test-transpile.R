test_that("transpile writes staged files with banner, helpers, and stubs", {
  p <- sas_project(system.file("examples", "demo_project", package = "sas2r"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(p, out)
  expect_s3_class(tr, "sas2r_transpilation")
  expect_true(file.exists(file.path(out, "sas2r-helpers.R")))
  expect_true(file.exists(file.path(out, "_sas2r_registry.R")))
  staged <- readLines(file.path(out, "03_t1.R"))
  expect_match(staged[1:3], "NOT VERIFIED", all = FALSE)
  expect_true(any(grepl("sas_sort", staged)))
  expect_true(any(grepl("eldfl", staged)))
})

test_that("manifest tiers: t1 for supported, stub with reason for the rest", {
  p <- sas_project(system.file("examples", "demo_project", package = "sas2r"))
  out <- withr::local_tempdir()
  m <- sas_transpile(p, out)$manifest
  expect_true(all(m$tier %in% c("t1", "stub")))
  expect_true(any(m$tier == "stub" & m$reason == "macro_residue"))  # 01_adsl macro call
  expect_true(any(m$tier == "t1"))
  expect_true(all(m$lint_errors == 0L))
})

test_that("stub blocks preserve the original SAS visibly", {
  p <- sas_project(system.file("examples", "demo_project", package = "sas2r"))
  out <- withr::local_tempdir()
  sas_transpile(p, out)
  staged <- paste(readLines(file.path(out, "02_summary.R")), collapse = "\n")
  expect_match(staged, "sas2r:untranslated")
  expect_match(staged, "ghost_macro")
})

test_that("the t1 fixture file executes end to end with correct semantics", {
  skip_if_not_installed("dplyr")
  p <- sas_project(system.file("examples", "demo_project", package = "sas2r"))
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

test_that("print method runs without error", {
  p <- sas_project(system.file("examples", "demo_project", package = "sas2r"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(p, out)
  expect_no_error(capture.output(print(tr), type = "message"))
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

test_that("failed proc sort stubs cleanly and does not emit NA as t1", {
  # F8: failed proc sort stubs cleanly
  dir <- withr::local_tempdir()
  writeLines("proc sort data=work.x; run;", file.path(dir, "nosort.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(dir), out)
  m <- tr$manifest
  expect_true(any(m$tier == "stub" & m$reason == "sort_missing_by"))
  staged <- readLines(file.path(out, "nosort.R"))
  expect_false(any(trimws(staged) == "NA"))
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
  reg_lines <- readLines(file.path(out, "_sas2r_registry.R"))
  reg <- paste(reg_lines, collapse = "\n")
  expect_no_error(parse(text = reg))
  expect_true(grepl("C:\\\\data\\\\adam", reg, fixed = TRUE))
  expect_false(grepl("C:\\data\\adam", reg, fixed = TRUE))
  # F21: libname in manifest
  m <- tr$manifest
  expect_true(any(m$flags == "registry" & m$tier == "t1"))
})

test_that("an ambiguity guard on a lineage row with no unit id does not error out", {
  # `lineage$unit_id` is NA on a row whose unit could not be attributed, and
  # `any(NA)` is NA -- so `if (!any(hit))` raised "missing value where
  # TRUE/FALSE needed" instead of either passing the unit or refusing it.
  dir <- withr::local_tempdir()
  writeLines("data a; x = 1; run;", file.path(dir, "p.sas"))
  p <- sas_project(dir)
  p$lineage$binding_status <- rep("ambiguous_libref", nrow(p$lineage))
  p$lineage$unit_id <- rep(NA_integer_, nrow(p$lineage))
  expect_silent(sas2r:::guard_unit_bindings(p, 1L, file.path(dir, "p.sas")))
  # An ambiguous row that *is* attributed still refuses.
  p$lineage$unit_id[1] <- 1L
  expect_error(sas2r:::guard_unit_bindings(p, 1L, file.path(dir, "p.sas")),
               class = "sas2r_ambiguous_libref")
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

test_that("one included module executes at every repeated occurrence", {
  root <- withr::local_tempdir()
  writeLines("data work.x; set work.input; run;", file.path(root, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "%include 'inc.sas';"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)
  driver <- readLines(file.path(out, "driver.R"), warn = FALSE)
  expect_equal(sum(grepl("sas2r_source_include", driver)), 2L)
  expect_equal(length(unique(tr$manifest$staged_file[
    basename(tr$manifest$file) == "inc.sas"])), 1L)
})

test_that("duplicate basenames in subdirectories mirror relative directory structure", {
  # R2-2: relative directory mirroring
  dir <- withr::local_tempdir()
  sub_dir <- file.path(dir, "sub")
  dir.create(sub_dir)
  writeLines("data top; set work.x; run;", file.path(dir, "an.sas"))
  writeLines("data sub; set work.y; run;", file.path(sub_dir, "an.sas"))

  p <- sas_project(dir, recursive = TRUE)
  out <- withr::local_tempdir()
  sas_transpile(p, out)

  expect_true(file.exists(file.path(out, "an.R")))
  expect_true(file.exists(file.path(out, "sub", "an.R")))
  expect_true(any(grepl("top <-", readLines(file.path(out, "an.R")))))
  expect_true(any(grepl("sub <-", readLines(file.path(out, "sub", "an.R")))))
})

test_that("unsupported merge shape records merge_unsupported_shape in manifest", {
  # R2-8: merge rejection reason
  dir <- withr::local_tempdir()
  writeLines("data m; merge a b c; by k; run;", file.path(dir, "merge3.sas"))
  p <- sas_project(dir)
  out <- withr::local_tempdir()
  tr <- sas_transpile(p, out)
  m <- tr$manifest
  expect_true(any(m$reason == "merge_unsupported_shape" & m$tier == "stub"))
})

test_that("a staged include module runs at its site inside the caller's frame", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  writeLines("data work.from_include; set work.input; run;",
             file.path(root, "inc", "prep.sas"))
  writeLines(c("%include 'inc/prep.sas';",
               "data work.after; set work.from_include; run;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  withr::local_dir(out)
  e <- new.env(parent = globalenv())
  sys.source("sas2r-helpers.R", e)
  e$.sas2r_registry <- list(work = list(path = ".", engine = "rds", write = "rds"))
  # Only `work.input` has a stand-in: any other member read without the
  # include having run first is a distinguishable row count, so the assertion
  # below fails when the module did not execute.
  e$lib_read <- function(libref, member) {
    f <- paste0(member, ".rds")
    if (file.exists(f)) readRDS(f)
    else if (identical(member, "input")) data.frame(x = 1:3)
    else data.frame(x = 1:9)
  }
  runnable <- readLines("driver.R", warn = FALSE)
  writeLines(runnable[!grepl("^source[(]", runnable)], "driver_run.R")
  sys.source("driver_run.R", envir = e)

  # The module executed into the driver's own frame, so the unit after the
  # include site sees what the included unit created.
  expect_true(exists("from_include", envir = e, inherits = FALSE))
  expect_identical(nrow(get("after", envir = e)), 3L)
  expect_true(file.exists("from_include.rds"))
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

test_that("only resolved occurrences become calls; the rest keep their reason", {
  root <- withr::local_tempdir()
  # a and b include each other: b's site at a resolves to a real path that the
  # scanner deliberately never re-read, so it must not become a call.
  writeLines(c("%include 'b.sas';", "data work.a; set work.input; run;"),
             file.path(root, "a.sas"))
  writeLines(c("%include 'a.sas';", "%include 'missing.sas';",
               "%include \"&root/x.sas\";"),
             file.path(root, "b.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "a.sas")), out)

  b_rows <- tr$manifest[basename(tr$manifest$file) == "b.sas", ]
  expect_setequal(b_rows$reason[b_rows$tier == "stub"],
                  c("cycle_detected", "target_not_found", "dynamic_target"))
  b_staged <- readLines(file.path(out, "b.R"), warn = FALSE)
  expect_false(any(grepl("^sas2r_source_include\\(", b_staged)))
  expect_true(any(grepl("reason=cycle_detected", b_staged)))

  # The one genuinely resolved site still emits its call.
  a_staged <- readLines(file.path(out, "a.R"), warn = FALSE)
  expect_equal(sum(grepl("^sas2r_source_include\\(", a_staged)), 1L)
})

test_that("a file included twice yields one module and one set of manifest rows", {
  root <- withr::local_tempdir()
  writeLines(c("data work.one; set work.input; run;",
               "data work.two; set work.one; run;"),
             file.path(root, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "data work.mid; set work.two; run;",
               "%include 'inc.sas';"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  inc_rows <- tr$manifest[basename(tr$manifest$file) == "inc.sas", ]
  expect_identical(nrow(inc_rows), 2L)
  expect_identical(unique(inc_rows$staged_file), "inc.R")
  expect_true(all(inc_rows$tier == "t1"))
  expect_identical(anyDuplicated(inc_rows$unit_id), 0L)
  expect_identical(length(list.files(out, pattern = "^inc\\.R$",
                                     recursive = TRUE)), 1L)
})

# ---- Fix round 1: findings 1-3 and the minor items ------------------------

# A `%INCLUDE` standing inside a macro, DATA-step, or PROC-step body cannot
# become a call where it stands, so the target module is unreachable. It must
# not reach the manifest as translated code a human could approve.
test_that("an include inside a non-global unit stubs the module and flags the parent", {
  bodies <- list(
    proc_step = "proc sort data=work.input out=work.s; by x; %include 'prep.sas'; run;",
    data_step = "data work.d; set work.input; %include 'prep.sas'; run;",
    macro_def = c("%macro go;", "%include 'prep.sas';", "%mend go;", "%go;")
  )
  for (ut in names(bodies)) {
    root <- withr::local_tempdir()
    writeLines("data work.prepped; set work.input; run;",
               file.path(root, "prep.sas"))
    writeLines(bodies[[ut]], file.path(root, "driver.sas"))
    out <- withr::local_tempdir()
    tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)

    mod <- tr$manifest[basename(tr$manifest$file) == "prep.sas", ]
    expect_true(nrow(mod) > 0L, info = ut)
    expect_true(all(mod$tier == "stub"), info = ut)
    expect_true(all(mod$reason == "include_site_not_emitted"), info = ut)
    expect_true(all(grepl("\\borphan_module\\b", mod$flags)), info = ut)

    parent <- tr$manifest[basename(tr$manifest$file) == "driver.sas", ]
    expect_true(any(grepl("\\binclude_site_not_emitted\\b", parent$flags)),
                info = ut)

    # Nothing translated is emitted for an unreachable module.
    staged <- readLines(file.path(out, unique(mod$staged_file)), warn = FALSE)
    expect_false(any(grepl("^lib_write\\(", staged)), info = ut)
    expect_true(any(grepl("reason=include_site_not_emitted", staged)), info = ut)
  }
})

test_that("an unreachable include module is never queued for agent translation", {
  root <- withr::local_tempdir()
  writeLines("data work.prepped; set work.input; run;", file.path(root, "prep.sas"))
  writeLines("proc sort data=work.input out=work.s; by x; %include 'prep.sas'; run;",
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)
  orphan_units <- tr$manifest$unit_id[tr$manifest$reason %in%
                                        "include_site_not_emitted"]
  expect_true(length(orphan_units) > 0L)
  expect_false(any(agent_stub_queue(tr$manifest) %in% orphan_units))
})

test_that("every site of a unit is decided on its own, and resolved calls survive", {
  # M5: a unit holding [resolved, cycle] must keep the resolved call rather
  # than discard everything accumulated before the first unemittable site.
  root <- withr::local_tempdir()
  writeLines("data work.p; set work.input; run;", file.path(root, "prep.sas"))
  writeLines("%include 'prep.sas';", file.path(root, "driver.sas"))
  p <- sas_project(file.path(root, "driver.sas"))
  modules <- transpile_staged_modules(p)

  occ <- p$include_graph$occurrences
  mixed <- occ[c(1L, 1L), , drop = FALSE]
  mixed$occurrence_id <- c("include_a", "include_b")
  mixed$line <- c(1L, 2L)
  mixed$status[2] <- "cycle"
  mixed$reason[2] <- "cycle_detected"
  p2 <- p
  p2$include_graph$occurrences <- mixed

  plan <- include_emission_plan(p2, modules)
  entry <- plan[[as.character(mixed$parent_unit_id[1])]]
  expect_equal(sum(grepl("^sas2r_source_include\\(", entry$calls)), 1L)
  expect_identical(entry$reason, "cycle_detected")
  expect_identical(entry$modules, "prep.R")
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

test_that("every staged module ends in .R whatever the target was named", {
  root <- withr::local_tempdir()
  named <- c("one.inc", "two.sas", "THREE.SAS", "four", "five.tar.sas")
  for (nm in named) writeLines("data work.a; set work.b; run;", file.path(root, nm))
  staged <- vapply(named, function(nm)
    canonical_include_staged_path(file.path(root, nm), root), character(1))
  expect_identical(unname(staged),
                   c("one.R", "two.R", "THREE.R", "four.R", "five.tar.R"))
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

test_that("no staged module sources the bundle from the working directory", {
  # Fix round 1 stripped the bootstrap from any resolved include target, which
  # in this shape -- a directory project, where every .sas file is a program --
  # cost an entry point its standalone bootstrap (round 2, finding 3). The
  # header is now idempotent and anchored on the module's own directory
  # instead, so an entry point keeps it and re-entry is what is prevented.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  writeLines("data work.from_include; set work.input; run;",
             file.path(root, "inc", "prep.sas"))
  writeLines(c("%include 'inc/prep.sas';",
               "data work.after; set work.from_include; run;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  p <- sas_project(root, recursive = TRUE)
  expect_true(all(p$files$origin == "program"))   # the shape under test
  sas_transpile(p, out)

  for (rel in c("driver.R", file.path("inc", "prep.R"))) {
    txt <- readLines(file.path(out, rel), warn = FALSE)
    expect_false(any(grepl("^source\\(", txt)), info = rel)
    expect_true(any(grepl('^if \\(!exists\\("\\.sas2r_registry"', txt)),
                info = rel)
  }

  # A file only ever reached through an include is not an entry point, and
  # carries no bootstrap of its own at all.
  out2 <- withr::local_tempdir()
  sas_transpile(sas_project(file.path(root, "driver.sas")), out2)
  inc <- readLines(file.path(out2, "inc", "prep.R"), warn = FALSE)
  expect_false(any(grepl("\\.sas2r_registry", inc)))
  expect_true(any(grepl("^# Included module", inc)))
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

test_that("the upward bundle-root walk stops at its level bound", {
  # M7: a registry further above than the bound must not be found, which is
  # what distinguishes the bound from filesystem-root termination.
  out <- withr::local_tempdir()
  write_helpers(out)
  e <- new.env(parent = globalenv())
  sys.source(file.path(out, "sas2r-helpers.R"), e)

  top <- withr::local_tempdir()
  writeLines("# registry above the bound", file.path(top, "_sas2r_registry.R"))
  deep <- top
  for (i in seq_len(70L)) {
    deep <- file.path(deep, "d")
    dir.create(deep)
  }
  withr::local_dir(deep)
  expect_error(e$sas2r_source_include("mod.R", envir = e),
               class = "sas2r_include_root_error")
})

test_that("a symlink inside the bundle cannot reach outside it", {
  # M6: the path components are ..-free, but a symlinked directory is not.
  skip_on_os("windows")
  out <- withr::local_tempdir()
  write_helpers(out)
  writeLines("_sas2r_registry_marker <- TRUE", file.path(out, "_sas2r_registry.R"))
  outside <- withr::local_tempdir()
  writeLines("escaped <- TRUE", file.path(outside, "mod.R"))
  linked <- file.symlink(outside, file.path(out, "link"))
  skip_if_not(isTRUE(linked), "symlinks unavailable")

  e <- new.env(parent = globalenv())
  sys.source(file.path(out, "sas2r-helpers.R"), e)
  withr::local_dir(out)
  expect_error(e$sas2r_source_include("link/mod.R", envir = e),
               class = "sas2r_include_path_error")
  expect_false(exists("escaped", envir = e, inherits = FALSE))
})

test_that("a module one live site reaches stays translated, and the dead site still shows", {
  # The boundary between the two markers: reachability belongs to the module,
  # a dropped site belongs to the unit that holds it.
  root <- withr::local_tempdir()
  writeLines("data work.p; set work.input; run;", file.path(root, "prep.sas"))
  writeLines(c("%include 'prep.sas';",
               "proc sort data=work.input out=work.s; by x; %include 'prep.sas'; run;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  mod <- tr$manifest[basename(tr$manifest$file) == "prep.sas", ]
  expect_true(all(mod$tier == "t1"))
  expect_false(any(grepl("orphan_module", mod$flags)))

  proc_row <- tr$manifest[tr$manifest$unit_type == "proc_step", ]
  expect_true(all(grepl("\\binclude_site_not_emitted\\b", proc_row$flags)))
  expect_equal(sum(grepl("^sas2r_source_include\\(",
                         readLines(file.path(out, "driver.R"), warn = FALSE))), 1L)
})

test_that("an out-of-root include target is staged as .R under its digest", {
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  writeLines("data work.a; set work.b; run;", file.path(outside, "shared.inc"))
  staged <- canonical_include_staged_path(file.path(outside, "shared.inc"), root)
  expect_match(staged, "^_includes/[0-9a-f]{16}/shared\\.R$")
})

# ---- Fix round 2: findings 1-3 and the minor items ------------------------

test_that("unreachability is transitive through an orphaned module's own sites", {
  # Finding 1: a global %include inside an orphaned module is not a live site,
  # because the unit holding it is stubbed with the rest of that module. A
  # single pass over every emitting unit counted it as live and staged the
  # deeper module as clean t1 translation nothing executes.
  root <- withr::local_tempdir()
  writeLines("data work.prepped; set work.input; run;", file.path(root, "b.sas"))
  writeLines("%include 'b.sas';", file.path(root, "a.sas"))
  writeLines("proc sort data=work.input out=work.s; by x; %include 'a.sas'; run;",
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  for (nm in c("a.sas", "b.sas")) {
    rows <- tr$manifest[basename(tr$manifest$file) == nm, ]
    expect_true(nrow(rows) > 0L, info = nm)
    expect_true(all(rows$tier == "stub"), info = nm)
    expect_true(all(rows$reason == "include_site_not_emitted"), info = nm)
    expect_true(all(grepl("\\borphan_module\\b", rows$flags)), info = nm)
  }
  b <- readLines(file.path(out, "b.R"), warn = FALSE)
  expect_true(any(grepl("UNREACHABLE MODULE", b)))
  expect_false(any(grepl("^lib_write\\(", b)))
  # a.R is stubbed whole, so the call that would have reached b.R is not there.
  expect_false(any(grepl("^sas2r_source_include\\(",
                         readLines(file.path(out, "a.R"), warn = FALSE))))
  expect_false(any(agent_stub_queue(tr$manifest) %in% tr$manifest$unit_id[
    basename(tr$manifest$file) %in% c("a.sas", "b.sas")]))
})

test_that("a three-level include chain marks the deepest module too", {
  root <- withr::local_tempdir()
  writeLines("data work.deep; set work.input; run;", file.path(root, "c.sas"))
  writeLines("%include 'c.sas';", file.path(root, "b.sas"))
  writeLines("%include 'b.sas';", file.path(root, "a.sas"))
  writeLines("proc sort data=work.input out=work.s; by x; %include 'a.sas'; run;",
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  tr <- sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  for (nm in c("a.sas", "b.sas", "c.sas")) {
    rows <- tr$manifest[basename(tr$manifest$file) == nm, ]
    expect_true(nrow(rows) > 0L, info = nm)
    expect_true(all(rows$tier == "stub"), info = nm)
    expect_true(all(grepl("\\borphan_module\\b", rows$flags)), info = nm)
  }
  expect_true(any(grepl("UNREACHABLE MODULE",
                        readLines(file.path(out, "c.R"), warn = FALSE))))
  # The driver itself is untouched: it is an entry point and translates as ever.
  drv <- tr$manifest[basename(tr$manifest$file) == "driver.sas", ]
  expect_true(all(drv$tier == "t1"))
})

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

test_that("a program the project lists as an entry point keeps its translation", {
  # Finding 2: being a resolved include target used to stub a file whole, even
  # when the project independently lists it as a program that runs directly.
  root <- withr::local_tempdir()
  writeLines(c("data work.prepped; set work.input; run;",
               paste("proc means data=work.prepped nway noprint;",
                     "class g; var y; output out=work.mm; run;")),
             file.path(root, "prep.sas"))
  writeLines("proc sort data=work.input out=work.s; by x; %include 'prep.sas'; run;",
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  p <- sas_project(root)
  expect_true(all(p$files$origin == "program"))   # the shape under test
  tr <- sas_transpile(p, out)

  prep <- tr$manifest[basename(tr$manifest$file) == "prep.sas", ]
  expect_false(any(grepl("orphan_module", prep$flags)))

  # The deterministic translation survives, unchanged, with no LLM in sight.
  ds <- prep[prep$unit_type == "data_step", ]
  expect_identical(ds$tier, "t1")
  expect_match(ds$code, 'prepped <- lib_read\\("work", "input"\\)')

  # And the unit that is not T1-eligible keeps its own reason and stays in the
  # agent queue instead of being filtered out as unreachable.
  mn <- prep[prep$unit_type == "proc_step", ]
  expect_identical(mn$tier, "stub")
  expect_identical(mn$reason, "means_not_t1")
  expect_true(mn$unit_id %in% agent_stub_queue(tr$manifest))

  staged <- readLines(file.path(out, "prep.R"), warn = FALSE)
  expect_false(any(grepl("UNREACHABLE MODULE", staged)))
  expect_true(any(grepl('^prepped <- lib_read\\("work", "input"\\)', staged)))

  # The dropped site is still reported, on the unit that actually holds it.
  drv <- tr$manifest[basename(tr$manifest$file) == "driver.sas", ]
  expect_true(all(grepl("\\binclude_site_not_emitted\\b", drv$flags)))
})

test_that("a dual-role module bootstraps standalone and skips it as an include", {
  # Finding 3: the header has to serve both roles at once. It is idempotent, so
  # it never re-enters the bootstrap, and anchored on the module's own
  # directory, so it does not depend on getwd().
  root <- withr::local_tempdir()
  writeLines("data work.prepped; set work.input; run;", file.path(root, "prep.sas"))
  writeLines(c("%include 'prep.sas';", "data work.after; set work.prepped; run;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  p <- sas_project(root)
  expect_true(all(p$files$origin == "program"))
  sas_transpile(p, out)

  watched <- c(".sas2r_registry", "lib_read", "lib_write", "sas2r_source_include",
               "prepped", "after", ".sas2r_boot_root", ".sas2r_boot_file")
  withr::defer(suppressWarnings(rm(
    list = intersect(watched, ls(globalenv(), all.names = TRUE)),
    envir = globalenv())))
  expect_false(any(vapply(watched, exists, logical(1),
                          envir = globalenv(), inherits = FALSE)))

  # (a) Standalone, and from a working directory that is not the bundle root:
  # the module finds the bundle from its own location and runs.
  elsewhere <- withr::local_tempdir()
  for (d in c(out, elsewhere)) {
    dir.create(file.path(d, "work"), showWarnings = FALSE)
    saveRDS(data.frame(x = 1:3), file.path(d, "work", "input.rds"))
  }
  withr::with_dir(elsewhere, {
    e1 <- new.env(parent = globalenv())
    sys.source(file.path(out, "prep.R"), envir = e1)
    for (nm in c(".sas2r_registry", "lib_read", "sas2r_source_include", "prepped"))
      expect_true(exists(nm, envir = e1, inherits = FALSE), info = nm)
    # The bootstrap tidies its own scratch bindings away.
    expect_false(exists(".sas2r_boot_root", envir = e1, inherits = FALSE))
    expect_false(exists(".sas2r_boot_file", envir = e1, inherits = FALSE))
  })

  withr::local_dir(out)
  # (b) Reached as an include, the header loads nothing: a caller's own
  # lib_read() survives the module rather than being sourced over.
  e2 <- new.env(parent = globalenv())
  sys.source("sas2r-helpers.R", e2)
  e2$.sas2r_registry <- list(work = list(path = "work", engine = "rds",
                                         write = "rds"))
  marker <- function(libref, member) data.frame(x = 1:7)
  e2$lib_read <- marker
  sys.source("driver.R", envir = e2)
  expect_identical(e2$lib_read, marker)
  expect_identical(nrow(e2$after), 7L)

  # (c) With no registry supplied the bootstrap does run -- into the sandbox,
  # never into globalenv().
  e3 <- new.env(parent = globalenv())
  sys.source("driver.R", envir = e3)
  expect_true(exists("after", envir = e3, inherits = FALSE))
  expect_true(exists(".sas2r_registry", envir = e3, inherits = FALSE))
  expect_false(any(vapply(watched, exists, logical(1),
                          envir = globalenv(), inherits = FALSE)))
})

test_that("a non-emitting parent shows its dropped include site in the code", {
  # M1: the manifest flag alone leaves driver.R reading as a clean sas_sort()
  # with no sign that a statement went missing.
  root <- withr::local_tempdir()
  writeLines("data work.p; set work.input; run;", file.path(root, "prep.sas"))
  writeLines("proc sort data=work.input out=work.s; by x; %include 'prep.sas'; run;",
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  driver <- readLines(file.path(out, "driver.R"), warn = FALSE)
  expect_true(any(grepl(
    "^# sas2r:untranslated include line=1 reason=include_site_not_emitted$",
    driver)))
  expect_false(any(grepl("^sas2r_source_include\\(", driver)))
  expect_true(any(grepl("^s <- sas_sort\\(", driver)))

  # The marker sits inside the unit's own block, so a later splice replaces it
  # with the unit rather than stranding it beside one.
  blk <- extract_unit_block(file.path(out, "driver.R"), 1L)
  expect_true(any(grepl("^# sas2r:untranslated include line=1", blk)))
})

test_that("a stubbed non-emitting parent carries the same dropped-site marker", {
  root <- withr::local_tempdir()
  writeLines("data work.p; set work.input; run;", file.path(root, "prep.sas"))
  writeLines(c("%macro go;", "%include 'prep.sas';", "%mend go;", "%go;"),
             file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  sas_transpile(sas_project(file.path(root, "driver.sas")), out)

  driver <- readLines(file.path(out, "driver.R"), warn = FALSE)
  expect_true(any(grepl(
    "^# sas2r:untranslated include line=2 reason=include_site_not_emitted$",
    driver)))
  blk <- extract_unit_block(file.path(out, "driver.R"), 1L)
  expect_true(any(grepl("^# sas2r:untranslated include line=2", blk)))
})

test_that("an aborting in-place transpile writes no bundle file into the source", {
  # M2: the three fixed-name files were written before any guard ran, so an
  # abort still left them behind in the user's own directory.
  root <- withr::local_tempdir()
  writeLines("data work.hidden; set work.input; run;", file.path(root, "prep.R"))
  writeLines("%include 'prep.R';", file.path(root, "driver.sas"))
  expect_error(sas_transpile(sas_project(root), root),
               class = "sas2r_staged_path_overwrites_source")
  for (nm in c("sas2r-helpers.R", "_sas2r_registry.R", "_sas2r_formats.R")) {
    expect_false(file.exists(file.path(root, nm)), info = nm)
  }
  expect_identical(setdiff(list.files(root), c("prep.R", "driver.sas")),
                   character())
})

test_that("a scanned source named like a bundle file is refused, not overwritten", {
  # M2: a fixed name cannot be renamed out of the way, so it has to be guarded.
  root <- withr::local_tempdir()
  writeLines("libname raw 'data';", file.path(root, "_sas2r_registry.R"))
  writeLines(c("environment:", "  autoexec:", "    - _sas2r_registry.R"),
             file.path(root, "_sas2r.yml"))
  writeLines("data work.a; set work.input; run;", file.path(root, "prog.sas"))
  before <- readLines(file.path(root, "_sas2r_registry.R"), warn = FALSE)

  p <- sas_project(root)
  expect_true("environment" %in% p$files$origin)   # the shape under test
  expect_error(sas_transpile(p, root),
               class = "sas2r_staged_path_overwrites_source")
  expect_identical(readLines(file.path(root, "_sas2r_registry.R"), warn = FALSE),
                   before)
})

test_that("a symlinked staged subdirectory cannot sidestep the source guard", {
  # M3: the staged side was joined without resolution, so a symlinked directory
  # under out_dir compared unequal to the source it actually points at.
  skip_on_os("windows")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "sub"))
  writeLines("data work.hidden; set work.input; run;",
             file.path(root, "sub", "prep.R"))
  writeLines("%include 'sub/prep.R';", file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  linked <- file.symlink(file.path(root, "sub"), file.path(out, "sub"))
  skip_if_not(isTRUE(linked), "symlinks unavailable")

  expect_error(sas_transpile(sas_project(root), out),
               class = "sas2r_staged_path_overwrites_source")
  expect_identical(readLines(file.path(root, "sub", "prep.R"), warn = FALSE),
                   "data work.hidden; set work.input; run;")
})

test_that("program and include staging agree on how a file is renamed to .R", {
  # M5: the program side rewrote only a final .sas, the include side any final
  # extension, so an explicitly listed prog.inc staged over its own source.
  root <- withr::local_tempdir()
  for (nm in c("one.inc", "two.sas", "THREE.SAS", "four", "five.tar.sas")) {
    writeLines("data work.a; set work.b; run;", file.path(root, nm))
    expect_identical(canonical_rel_staged_path(file.path(root, nm), root),
                     canonical_include_staged_path(file.path(root, nm), root),
                     info = nm)
  }
  expect_identical(canonical_rel_staged_path(file.path(root, "one.inc"), root),
                   "one.R")
})

test_that("an explicitly listed non-.sas program stages as .R beside its source", {
  root <- withr::local_tempdir()
  writeLines("data work.a; set work.input; run;", file.path(root, "prog.inc"))
  before <- readLines(file.path(root, "prog.inc"), warn = FALSE)
  tr <- sas_transpile(sas_project(file.path(root, "prog.inc")), root)
  expect_identical(unique(tr$manifest$staged_file), "prog.R")
  expect_identical(readLines(file.path(root, "prog.inc"), warn = FALSE), before)
  expect_true(file.exists(file.path(root, "prog.R")))
})

test_that("an autoexec include site keeps counting as live, and the module says so", {
  # The boundary of the transitive fix: an autoexec contributes environment and
  # is never staged as a module, so its %INCLUDE sites have no module to be
  # orphaned along with. That shape keeps its deterministic translation
  # complete -- not demoted -- but nothing in the emitted bundle ever calls the
  # module this site reaches, and the manifest row and the module header now
  # say so instead of presenting it as ordinary, fully-wired t1 code.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  writeLines("data work.shared; set work.input; run;",
             file.path(root, "inc", "shared.sas"))
  writeLines("%include 'inc/shared.sas';", file.path(root, "autoexec.sas"))
  writeLines("data work.a; set work.shared; run;", file.path(root, "prog.sas"))
  out <- withr::local_tempdir()

  p <- sas_project(root)
  expect_identical(p$files$origin[basename(p$files$file) == "shared.sas"],
                   "included")
  tr <- sas_transpile(p, out)
  shared <- tr$manifest[basename(tr$manifest$file) == "shared.sas", ]

  # The deterministic translation survives complete, with no LLM in sight.
  expect_identical(shared$tier, "t1")
  expect_match(shared$code, 'shared <- lib_read\\("work", "input"\\)')

  # An advisory, not a demotion: the row says plainly that nothing sources it.
  expect_true(all(grepl("\\binclude_parent_not_staged\\b", shared$flags)))

  # And it is true: no file anywhere in the bundle holds a call reaching it.
  written <- list.files(out, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  calls <- unlist(lapply(written, function(f) {
    grep("^sas2r_source_include\\(", readLines(f, warn = FALSE), value = TRUE)
  }))
  expect_length(calls, 0L)

  # The header no longer claims an execution that never happens for this file.
  staged <- readLines(file.path(out, unique(shared$staged_file)), warn = FALSE)
  expect_true(any(grepl("^# Included module", staged)))
  expect_false(any(grepl("It is executed at every", staged)))
})

# ---- Fix round 3: Item 1 and the minor items -------------------------------

test_that("sourcing the same bootstrap-carrying module twice into one env does not re-bootstrap", {
  # M7: Finding 3's idempotence guard is exercised elsewhere only for a
  # driver/include pairing -- two different files sharing one bootstrapped
  # environment. The same guard has to hold for one file sourced twice into
  # the same environment, which no test exercised directly.
  root <- withr::local_tempdir()
  writeLines("data work.prepped; set work.input; run;", file.path(root, "prep.sas"))
  out <- withr::local_tempdir()
  sas_transpile(sas_project(file.path(root, "prep.sas")), out)

  dir.create(file.path(out, "work"), showWarnings = FALSE)
  saveRDS(data.frame(x = 1:3), file.path(out, "work", "input.rds"))

  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("prep.R", envir = e))
  expect_true(exists(".sas2r_registry", envir = e, inherits = FALSE))
  expect_identical(nrow(get("prepped", envir = e)), 3L)

  # A caller's own lib_read() must survive a second source() of the very same
  # file, exactly as it survives an %INCLUDE reaching it from another file.
  marker <- function(libref, member) data.frame(x = 1:7)
  e$lib_read <- marker
  expect_no_error(withr::with_dir(out, sys.source("prep.R", envir = e)))
  expect_identical(e$lib_read, marker)
  # The unit's own code is not gated by the guard, and reran with the
  # caller's lib_read() rather than a freshly rebootstrapped one.
  expect_identical(nrow(e$prepped), 7L)
})

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
