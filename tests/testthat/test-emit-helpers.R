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
       registry = paste(readLines(file.path(out, "_sas2r_registry.R")),
                        collapse = "\n"))
}

test_that("the registry seeds configured libraries and work, never a source binding", {
  root <- withr::local_tempdir()
  cfg <- file.path(root, "cfg"); dir.create(cfg)
  p <- libref_emit_fixture(
    root, c("libname adam \"src\";", "data x; set adam.adsl; run;"),
    libraries = list(sdtm = cfg), dirs = "src")
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)

  e <- new.env(); sys.source(file.path(out, "_sas2r_registry.R"), e)
  # The seed is exactly the configured library plus `work`. `adam` is bound by
  # a statement, so it belongs in the module, not in the seed.
  expect_setequal(names(e$.sas2r_registry), c("sdtm", "work"))
  expect_identical(e$.sas2r_registry$sdtm$read_path,
                   normalizePath(cfg, winslash = "/", mustWork = FALSE))
  expect_false(grepl("src", txt$registry, fixed = TRUE))
  expect_match(txt$module, "sas2r_libname_assign\\(\"adam\"")
})

test_that("write_registry reads the projection rather than reparsing librefs", {
  # The discriminating test for "no independent reparse": the projection is
  # doctored so its seed disagrees with everything in the project, and the file
  # has to follow the projection. A write_registry() that still built entries
  # from project$librefs or project$flags would ignore both edits.
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(
    root, c("libname adam \"src\";", "data x; set adam.adsl; run;"),
    dirs = "src")
  effective <- sas2r:::effective_librefs(p)
  effective$seed <- list(ghost = list(read_path = "/somewhere/ghost",
                                      write_path = "/somewhere/ghost",
                                      engine = "sas7bdat", write = "xpt"))
  effective$undeclared <- "phantom"

  out <- withr::local_tempdir()
  sas2r:::write_registry(p, out, effective)
  e <- new.env(); sys.source(file.path(out, "_sas2r_registry.R"), e)
  expect_setequal(names(e$.sas2r_registry), c("ghost", "work"))
  expect_identical(e$.sas2r_registry$ghost$write, "xpt")
  expect_match(paste(readLines(file.path(out, "_sas2r_registry.R")),
                     collapse = "\n"),
               "#  phantom = list\\(read_path = \"<FILL")
})

test_that("a seed path is confined on the way into the registry, as a statement path is", {
  # Both emission paths reach generated R from source or configuration, so both
  # get the same control-character check. Without it the check guarded only the
  # statement half of the same obligation.
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(
    root, c("libname adam \"src\";", "data x; set adam.adsl; run;"),
    dirs = "src")
  effective <- sas2r:::effective_librefs(p)
  out <- withr::local_tempdir()
  for (bad in list(list(ghost = list(read_path = "a\nb", write_path = "a\nb", engine = "sas7bdat",
                                     write = "rds")),
                   list(work = list(read_path = "a\nb", write_path = "a\nb", engine = "rds",
                                    write = "rds")))) {
    doctored <- effective
    doctored$seed <- bad
    expect_error(sas2r:::write_registry(p, out, doctored),
                 class = "sas2r_libref_path_error")
  }
  # A configured path that is merely absolute and outside the project is not
  # refused -- a study library legitimately lives elsewhere.
  doctored <- effective
  doctored$seed <- list(ghost = list(read_path = tempdir(), write_path = tempdir(), engine = "sas7bdat",
                                     write = "rds"))
  expect_no_error(sas2r:::write_registry(p, out, doctored))
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

test_that("a configured fallback names the reason it was needed and only one path", {
  root <- withr::local_tempdir()
  cfg <- file.path(root, "cfg"); dir.create(cfg)
  # The source LIBNAME names a directory that is not there, so configuration
  # supplies the location.
  p <- libref_emit_fixture(root, c("libname adam \"missing\";",
                                   "data x; set adam.adsl; run;"),
                           libraries = list(adam = cfg))
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  expect_match(txt$module,
               "# sas2r:libref_fallback libref=adam reason=source_path_unavailable")
  expect_match(txt$module,
               normalizePath(cfg, winslash = "/", mustWork = FALSE),
               fixed = TRUE)
  # Exactly one path per binding: the failed source location never reaches
  # generated R, and there is no runtime choice between the two.
  expect_false(grepl("\"missing\"", txt$module, fixed = TRUE))
  expect_identical(
    lengths(regmatches(txt$module, gregexpr("sas2r_libname_assign", txt$module))),
    1L)
})

test_that("an unbound libref keeps translation complete behind a visible stub", {
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c("libname adam \"missing\";",
                                   "data x; set adam.adsl; run;"))
  out <- withr::local_tempdir()
  tr <- suppressMessages(sas_transpile(p, out))
  txt <- list(module = paste(readLines(file.path(out, "p.R")), collapse = "\n"),
              registry = paste(readLines(file.path(out, "_sas2r_registry.R")),
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

test_that("an unbound libref's path is escaped, not pasted, into module and registry", {
  # The escaping guard for the path that reaches generated R by the *unbound*
  # route. A Windows-authored `libname adam 'C:\\data\\adam';` translated on a
  # machine where that drive is not mounted is the ordinary case, and both the
  # marker comment and the commented registry entry carry the path verbatim.
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c("libname adam 'C:\\data\\adam';",
                                   "data x; set adam.adsl; run;"))
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  expect_true(grepl("C:\\data\\adam", txt$module, fixed = TRUE))
  # In the registry the path is a deparsed string literal, so the backslashes
  # are escaped and the file still parses.
  expect_true(grepl("C:\\\\data\\\\adam", txt$registry, fixed = TRUE))
  expect_no_error(parse(text = txt$registry))
  expect_no_error(parse(text = txt$module))
  # Uncommenting the entry yields exactly the path the LIBNAME named.
  entry <- grep("^#  adam = list", strsplit(txt$registry, "\n")[[1]],
                value = TRUE)
  expect_length(entry, 1L)
  e <- new.env()
  eval(parse(text = paste0("x <- list(", sub("^#", "", sub(",$", "", entry)),
                           ")")), e)
  expect_identical(e$x$adam$read_path, "C:\\data\\adam")
})

test_that("a LIBNAME inside a macro definition establishes nothing in the bundle", {
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c("%macro setup;",
                                   "libname adam \"mlib\";",
                                   "%mend;",
                                   "data b; set adam.two; run;"),
                           dirs = "mlib")
  # The registry reports the location without establishing it.
  expect_identical(
    sas2r:::libref_binding_at(p$libref_registry, "adam",
                              file.path(root, "p.sas"), 4L)$status,
    "conditionally_bound")
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  # A macro definition is one translation unit and stubs whole, so no assign is
  # emitted anywhere, and the libref is treated as unestablished.
  expect_false(grepl("sas2r_libname_assign", txt$module, fixed = TRUE))
  # The location the macro body reports is still recorded -- reporting it is
  # the whole difference between `conditionally_bound` and `unbound`.
  expect_match(txt$registry, "#  adam = list\\(read_path = \"mlib\"")
  expect_match(txt$registry, "# adam: <FILL>")
  expect_identical(sas2r:::effective_librefs(p)$undeclared, "adam")
})

test_that("a libref nothing names at all keeps the bare <FILL> placeholder", {
  # The complement of the two tests above: with no LIBNAME anywhere there is no
  # path to record, so the placeholder is all the file can honestly offer.
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, "data x; set phantom.a; run;")
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  expect_match(txt$registry, "#  phantom = list\\(read_path = \"<FILL: ask your SAS admin>\"")
  expect_match(txt$registry, "no_source_binding")
})


test_that("a LIBNAME form sas2r does not read is shown, not silently dropped", {
  root <- withr::local_tempdir()
  # A concatenated library is not a form the scanner reads, so it contributes
  # no binding. The unit still carries an anchor, and an anchor with nothing
  # under it would read as "this translated to nothing".
  p <- libref_emit_fixture(root, c("libname adam (one two);",
                                   "data x; set adam.adsl; run;"))
  expect_identical(nrow(p$librefs), 0L)
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  expect_match(txt$module,
               "# sas2r:libref_unrecognized unit=1 -- not a LIBNAME form sas2r reads")
  expect_match(txt$module, "# libname adam \\(one two\\)")
  expect_false(grepl("sas2r_libname_assign", txt$module, fixed = TRUE))
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

test_that("a stubbed unit is not refused for an ambiguity it never emits", {
  # Same two-context disagreement, but the unit that would read the library
  # cannot be translated anyway, so no read is generated and nothing needs the
  # binding. Refusing here would take correct translation away from the rest of
  # the project for a statement that emits nothing.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "liba")); dir.create(file.path(root, "libb"))
  writeLines("data x; set adam.adsl; z = put(y, best.); run;",
             file.path(root, "common.sas"))
  writeLines(c("libname adam \"liba\";", "%include \"common.sas\";"),
             file.path(root, "a.sas"))
  writeLines(c("libname adam \"libb\";", "%include \"common.sas\";"),
             file.path(root, "b.sas"))
  p <- sas_project(root)
  expect_true("ambiguous_libref" %in% p$lineage$binding_status)
  out <- withr::local_tempdir()
  tr <- suppressMessages(sas_transpile(p, out))
  expect_true(any(tr$manifest$tier == "stub" &
                    tr$manifest$reason == "expr_parse_failed"))
})

test_that("a directory name carrying a backslash is emitted safely", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  odd <- "we\\ird"
  dir.create(file.path(root, odd))
  saveRDS(data.frame(x = 1:4), file.path(root, odd, "adsl.rds"))
  writeLines(paste0("libname adam '", odd, "'; data x; set adam.adsl; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  out <- withr::local_tempdir()
  suppressMessages(sas_transpile(p, out))
  module <- readLines(file.path(out, "p.R"))
  expect_true(any(grepl("sas2r_libname_assign", module, fixed = TRUE)))
  expect_no_error(parse(text = paste(module, collapse = "\n")))
  # The emitted literal is the directory itself, escaped, not a stray escape.
  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_true(endsWith(e$.sas2r_registry$adam$read_path, odd))
})

test_that("effective_librefs projects statement and reference points of use", {
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c("libname adam \"src\";",
                                   "data x; set adam.adsl; run;",
                                   "libname adam clear;"), dirs = "src")
  eff <- sas2r:::effective_librefs(p)
  expect_identical(names(eff$bindings), sas2r:::EFFECTIVE_LIBREF_FIELDS)
  expect_setequal(unique(eff$bindings$kind), c("statement", "reference"))
  st <- eff$bindings[eff$bindings$kind == "statement", ]
  expect_identical(st$action, c("assign", "clear"))
  expect_identical(st$use_line, c(1L, 3L))
  # Execution-context provenance travels with every row.
  expect_true(all(st$root_program == file.path(root, "p.sas")))
  # `work` is the session library and is never resolved.
  expect_false("work" %in% eff$bindings$libref)
  # The reference row and the project's own lineage are the same answer.
  ref <- eff$bindings[eff$bindings$kind == "reference", ]
  expect_identical(ref$binding_id,
                   p$lineage$binding_id[p$lineage$dataset == "adam.adsl"])
})

test_that("effective_librefs keeps root-program and include-occurrence context", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "lib"))
  # The LIBNAME lives in the included file, so the frame that executed it is
  # the one a named %include occurrence created -- which is the context the
  # projection has to carry through.
  writeLines(c("libname adam \"lib\";", "data x; set adam.adsl; run;"),
             file.path(root, "inc.sas"))
  writeLines("%include \"inc.sas\";", file.path(root, "driver.sas"))
  p <- sas_project(file.path(root, "driver.sas"))
  eff <- sas2r:::effective_librefs(p)
  ref <- eff$bindings[eff$bindings$kind == "reference", ]
  expect_identical(nrow(ref), 1L)
  expect_identical(ref$use_file, p$files$file[basename(p$files$file) == "inc.sas"])
  expect_identical(ref$root_program,
                   p$files$file[basename(p$files$file) == "driver.sas"])
  expect_true(ref$include_occurrence_id %in%
                p$include_graph$occurrences$occurrence_id)
  expect_identical(ref$status, "bound")
  # The relative LIBNAME path is resolved against the project root, so the
  # included module and the driver agree on one directory.
  expect_identical(ref$selected_path,
                   normalizePath(file.path(root, "lib"), winslash = "/",
                                 mustWork = FALSE))
})

test_that("a LIBNAME in an included module binds in the including program", {
  # SAS semantics: a LIBNAME an %INCLUDE brings in affects the program that
  # included it. The staged module executes in the including program's own
  # environment, so the assign it emits has to reach that program's registry --
  # and still not reach globalenv() from a sandboxed parent.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  lib <- file.path(root, "lib"); dir.create(lib)
  saveRDS(data.frame(x = 1:9), file.path(lib, "adsl.rds"))
  writeLines(c("libname adam \"lib\";", "data x; set adam.adsl; run;"),
             file.path(root, "inc", "prep.sas"))
  writeLines(c("%include \"inc/prep.sas\";", "data y; set work.x; run;"),
             file.path(root, "driver.sas"))
  p <- sas_project(file.path(root, "driver.sas"))
  out <- withr::local_tempdir()
  suppressMessages(sas_transpile(p, out))

  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("driver.R", envir = e))
  expect_identical(nrow(e$x), 9L)
  expect_identical(nrow(e$y), 9L)
  expect_identical(e$.sas2r_registry$adam$read_path,
                   normalizePath(lib, winslash = "/", mustWork = FALSE))
  expect_false(exists(".sas2r_registry", envir = globalenv(), inherits = FALSE))
})

test_that("a LIBNAME naming work is shown but never rebinds the session library", {
  # `work` is the session library the seed creates; nothing else creates it. An
  # emitted assign would point `work` at a directory no dir.create() ever made,
  # and the first lib_write() to work would fail on a bundle whose SAS looked
  # perfectly ordinary. References already exclude `work`; statements must too.
  root <- withr::local_tempdir()
  p <- libref_emit_fixture(root, c("libname work \"wk\";",
                                   "data x; y = 1; run;"), dirs = "wk")
  eff <- sas2r:::effective_librefs(p)
  expect_false("work" %in% eff$bindings$libref)
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  expect_false(grepl("sas2r_libname_assign(\"work\"", txt$module, fixed = TRUE))
  # Shown, not silently dropped, and not mislabelled as an unreadable form.
  expect_match(txt$module,
               "# sas2r:libref_session_library libref=work action=assign path='wk'")
  expect_false(grepl("libref_unrecognized", txt$module, fixed = TRUE))
  # The bundle still runs, and `work` is still the directory the seed made.
  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_identical(e$.sas2r_registry$work$read_path, "work")
  expect_true(dir.exists(file.path(out, "work")))
  expect_no_error(withr::with_dir(out, {
    environment(e$lib_write) <- e
    e$lib_write(data.frame(x = 1), "work", "probe")
  }))
})

test_that("a configured work relocates the session library in the seed", {
  root <- withr::local_tempdir()
  wk <- file.path(root, "sessions", "wk")
  p <- libref_emit_fixture(root, "data x; y = 1; run;",
                           libraries = list(work = wk))
  out <- withr::local_tempdir()
  txt <- transpiled_text(p, out)
  canonical <- normalizePath(wk, winslash = "/", mustWork = FALSE)
  # One `work` entry, the configured one, and the bootstrap creates it.
  expect_identical(
    lengths(regmatches(txt$registry, gregexpr("^  work = list", txt$registry))),
    0L)
  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_identical(e$.sas2r_registry$work$read_path, canonical)
  expect_true(dir.exists(canonical))
  expect_length(e$.sas2r_registry[names(e$.sas2r_registry) == "work"], 1L)
})

test_that("sourcing a module twice re-establishes its own bindings over a mutated registry", {
  # A point-of-use assign is a statement, not a one-time initialisation. A
  # caller that edited `.sas2r_registry` between runs -- or a driver that
  # sources one module under two different libraries -- must get the module's
  # own binding back on the second pass, not the caller's leftover.
  root <- withr::local_tempdir()
  lib <- file.path(root, "lib"); dir.create(lib)
  other <- file.path(root, "other"); dir.create(other)
  saveRDS(data.frame(x = 1:5), file.path(lib, "adsl.rds"))
  saveRDS(data.frame(x = 1:2), file.path(other, "adsl.rds"))
  writeLines(c("libname adam \"lib\";", "data x; set adam.adsl; run;"),
              file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  out <- withr::local_tempdir()
  suppressMessages(sas_transpile(p, out))

  e <- new.env(parent = globalenv())
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_identical(nrow(e$x), 5L)
  # The caller repoints the libref at a different library, then runs again.
  e$.sas2r_registry$adam <- list(read_path = other, write_path = other, engine = "sas7bdat",
                                 write = "rds")
  withr::with_dir(out, sys.source("p.R", envir = e))
  expect_identical(nrow(e$x), 5L)
  expect_identical(e$.sas2r_registry$adam$read_path,
                   normalizePath(lib, winslash = "/", mustWork = FALSE))
})

test_that("confine_libref_path canonicalizes and refuses an unemittable path", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "x"))
  canonical <- normalizePath(file.path(root, "x"), winslash = "/",
                             mustWork = FALSE)
  expect_identical(sas2r:::confine_libref_path(file.path(root, "x"), "adam"),
                   canonical)
  # It is the *same* canonicalization the registry already applied, so it is a
  # fixed point -- a second, different one would spell one directory two ways.
  expect_identical(sas2r:::confine_libref_path(canonical, "adam"), canonical)
  # A library outside the project is legitimate and must not be refused.
  expect_no_error(sas2r:::confine_libref_path(tempdir(), "adam"))
  for (bad in list(NA_character_, "", "a\nb", character())) {
    expect_error(sas2r:::confine_libref_path(bad, "adam"),
                 class = "sas2r_libref_path_error")
  }
})

test_that("confining a library path never rewrites a backslash in its name", {
  # canonical_output_path() rewrites `\` to `/` for Windows separators, which
  # would silently rename a directory legitimately called `we\ird` here.
  skip_on_os("windows")
  root <- withr::local_tempdir()
  odd <- file.path(root, "we\\ird"); dir.create(odd)
  expect_identical(sas2r:::confine_libref_path(odd, "adam"),
                   normalizePath(odd, winslash = "/", mustWork = FALSE))
  expect_true(dir.exists(sas2r:::confine_libref_path(odd, "adam")))
})

test_that("a member name that is a path is refused before it is read", {
  out <- withr::local_tempdir()
  write_helpers(out)
  e <- new.env(parent = globalenv())
  sys.source(file.path(out, "sas2r-helpers.R"), e)
  e$.sas2r_registry <- list(adam = list(read_path = out, write_path = out, engine = "rds",
                                        write = "rds"))
  for (fn in c("sas2r_lib_entry", "sas2r_lib_member_path", "lib_read",
               "lib_write")) {
    environment(e[[fn]]) <- e
  }
  expect_error(e$lib_read("adam", "../escape"),
               class = "sas2r_libref_member_error")
  expect_error(e$lib_write(data.frame(x = 1), "adam", "sub/escape"),
               class = "sas2r_libref_member_error")
  expect_error(e$lib_read("nowhere", "adsl"), class = "sas2r_unknown_libref")
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
