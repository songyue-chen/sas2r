# Emission of the effective libref registry into generated R.
#
# The property under test throughout is one thing: the bundle sas2r writes uses
# exactly the bindings the project already selected. Not the parsed LIBNAME
# text, not a project-wide last-writer map, and not a runtime choice between
# two candidate paths -- the selected binding, at the point of use it was
# selected for.

# A project whose source LIBNAME points at a real directory, optionally with a
# configured library of the same name to fall back to.
libref_emit_fixture <- function(root, sas, libraries = NULL, dirs = character()) {
  for (d in dirs) dir.create(file.path(root, d), recursive = TRUE,
                             showWarnings = FALSE)
  writeLines(sas, file.path(root, "p.sas"))
  config <- if (is.null(libraries)) list() else list(libraries = libraries)
  sas_project(file.path(root, "p.sas"), config = config)
}

transpiled_text <- function(project, out) {
  suppressMessages(sas_transpile(project, out))
  list(module = paste(readLines(file.path(out, "p.R")), collapse = "\n"),
       registry = paste(readLines(file.path(out, "autoexec.R")),
                        collapse = "\n"))
}

test_that("startup does not apply a program-level source binding early", {
  root <- withr::local_tempdir()
  cfg <- file.path(root, "cfg"); dir.create(cfg)
  p <- libref_emit_fixture(
    root, c("libname adam \"src\";", "data x; set adam.adsl; run;"),
    libraries = list(sdtm = cfg), dirs = "src")
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)

  e <- new.env(); sys.source(file.path(out, "autoexec.R"), e, chdir = TRUE)
  # The seed is exactly the configured library plus `work`. `adam` is bound by
  # a statement, so it belongs in the module, not in the seed.
  expect_setequal(names(e$.sas2r_registry), c("sdtm", "work"))
  expect_identical(e$.sas2r_registry$sdtm$read_path,
                   normalizePath(cfg, winslash = "/", mustWork = FALSE))
  expect_false(grepl("src", txt$registry, fixed = TRUE))
  expect_match(txt$module, "sas2r_libname_assign\\(\"adam\"")
})

test_that("the runtime registry follows the program's own rebinding", {
  # End to end, with real files on both sides: the first read must come from
  # the library the LIBNAME names and the second from the configured library
  # the CLEAR falls back to. A static project-wide registry cannot produce two
  # different answers for one libref in one program, which is the whole point.
  root <- withr::local_tempdir()
  src <- file.path(root, "src"); dir.create(src)
  cfg <- file.path(root, "cfg"); dir.create(cfg)
  saveRDS(data.frame(x = 1:2), file.path(src, "one.rds"))
  saveRDS(data.frame(x = 1:5), file.path(cfg, "two.rds"))
  p <- libref_emit_fixture(root, c(
    "libname adam \"src\";",
    "data a; set adam.one; run;",
    "libname adam clear;",
    "data b; set adam.two; run;"), libraries = list(adam = cfg))
  out <- withr::local_tempdir()
  suppressMessages(sas_transpile(p, out))

  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_identical(nrow(e$a), 2L)
  expect_identical(nrow(e$b), 5L)
  # The live registry agrees with what the project resolved below the clear.
  expect_identical(
    e$.sas2r_registry$adam$read_path,
    sas2r:::libref_binding_at(p$libref_registry, "adam",
                              file.path(root, "p.sas"), 4L)$selected_path)
  # A sandboxed parent keeps the registry and the new helpers to itself.
  expect_false(exists(".sas2r_registry", envir = globalenv(), inherits = FALSE))
  expect_false(exists("sas2r_libname_assign", envir = globalenv(),
                      inherits = FALSE))
})

test_that("LIBNAME CLEAR with nothing configured clears the runtime binding", {
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c(
    "libname adam \"src\";",
    "libname adam clear;",
    "data b; set adam.two; run;"), dirs = "src")
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  expect_match(txt$module, "sas2r_libname_clear\\(\"adam\"\\)")
  # Nothing establishes `adam` below the clear, so the read has a placeholder
  # to fill rather than a path that cannot work.
  expect_match(txt$registry, "#  adam = list\\(read_path = \"<FILL")

  e <- new.env(); sys.source(file.path(out, "sas2r-helpers.R"), e)
  e$.sas2r_registry <- list(adam = list(read_path = "x", write_path = "x", engine = "rds",
                                        write = "rds"))
  environment(e$sas2r_libname_clear) <- e
  e$sas2r_libname_clear("ADAM")
  expect_null(e$.sas2r_registry$adam)
})

test_that("a configured library survives a CLEAR, exactly as the project resolved it", {
  root <- withr::local_tempdir()
  cfg <- file.path(root, "cfg"); dir.create(cfg)
  p <- libref_emit_fixture(root, c(
    "libname adam \"src\";",
    "libname adam clear;",
    "data b; set adam.two; run;"),
    libraries = list(adam = cfg), dirs = "src")
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  canonical_cfg <- normalizePath(cfg, winslash = "/", mustWork = FALSE)
  # A clear that falls back to configuration re-assigns; emitting a clear here
  # would drop the seed entry and strand every read below it.
  expect_match(txt$module, "source_binding_cleared")
  expect_true(grepl(sprintf("sas2r_libname_assign(\"adam\", \"%s\"",
                            canonical_cfg),
                    txt$module, fixed = TRUE))
  expect_false(grepl("sas2r_libname_clear", txt$module, fixed = TRUE))
  # Nothing to fill in: the configured library is in force below the clear.
  # (The header comment explains the <FILL> convention, so the entry pattern
  # is what has to be absent, not the word.)
  expect_false(grepl("#  adam = list(read_path = \"<FILL", txt$registry,
                     fixed = TRUE))
})

test_that("an unbound libref keeps translation complete behind a visible stub", {
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c("libname adam \"missing\";",
                                   "data x; set adam.adsl; run;"))
  out <- withr::local_tempdir()
  tr <- suppressMessages(sas_transpile(p, out))
  txt <- list(module = paste(readLines(file.path(out, "p.R")), collapse = "\n"),
              registry = paste(readLines(file.path(out, "autoexec.R")),
                               collapse = "\n"))
  # Visible in the module, visible in the registry, and no invented path.
  expect_match(txt$module,
               "# sas2r:libref_unbound libref=adam reason=source_path_unavailable")
  expect_false(grepl("sas2r_libname_assign", txt$module, fixed = TRUE))
  # The path the SAS actually names survives into both files. A bundle
  # translated where the study drive is not mounted must still record what the
  # LIBNAME said, or nothing in it says which library was meant.
  expect_match(txt$module, "path='missing'", fixed = TRUE)
  expect_match(txt$registry, "#  adam = list\\(read_path = \"missing\"")
  # ... and the <FILL> guidance stays alongside it, because the path could not
  # be used and uncommenting it unchanged may well not work either.
  expect_match(txt$registry, "# adam: <FILL>")
  # Neither a silent omission nor an abort: the data step is still translated.
  expect_true(any(tr$manifest$tier == "t1" & tr$manifest$flags == "registry"))
  expect_match(txt$module, "lib_read\\(\"adam\", \"adsl\"\\)")
})

test_that("an ambiguous binding is refused at transpile time, not in generated R", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "liba")); dir.create(file.path(root, "libb"))
  writeLines("data x; set adam.adsl; run;", file.path(root, "common.sas"))
  writeLines(c("libname adam \"liba\";", "%include \"common.sas\";"),
             file.path(root, "a.sas"))
  writeLines(c("libname adam \"libb\";", "%include \"common.sas\";"),
             file.path(root, "b.sas"))
  p <- sas_project(root)
  expect_true("ambiguous_libref" %in% p$lineage$binding_status)
  expect_error(suppressMessages(sas_transpile(p, withr::local_tempdir())),
               class = "sas2r_ambiguous_libref")
})

test_that("a project with no configuration at all still translates and runs", {

  # Code-only completeness: no configuration, no LLM, no output review.
  root <- withr::local_tempdir()
  src <- file.path(root, "src"); dir.create(src)
  saveRDS(data.frame(x = 1:3), file.path(src, "adsl.rds"))
  writeLines(c("libname adam \"src\";", "data x; set adam.adsl; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  out <- withr::local_tempdir()
  tr <- suppressMessages(sas_transpile(p, out))
  expect_false(any(tr$manifest$tier == "stub"))
  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_identical(nrow(e$x), 3L)
})
