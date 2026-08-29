# Two root programs bind `adam` to different readable directories and then
# include one physical file. The include occurrence ids come from the include
# graph itself, so the assertions below name an execution context rather than a
# scan position: nothing here depends on which root program was scanned first.
shared_include_libref_fixture <- function(envir = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = envir)
  program_dir <- file.path(base, "programs"); dir.create(program_dir)
  shared_dir <- file.path(program_dir, "shared"); dir.create(shared_dir)
  library_a <- file.path(base, "adam-a"); dir.create(library_a)
  library_b <- file.path(base, "adam-b"); dir.create(library_b)
  configured <- file.path(base, "adam-configured"); dir.create(configured)

  include_file <- file.path(shared_dir, "uses_adam.sas")
  writeLines("data work.x; set adam.adsl; run;", include_file)
  program_a <- file.path(program_dir, "a_root.sas")
  program_b <- file.path(program_dir, "b_root.sas")
  writeLines(c(sprintf("libname adam '%s';", library_a),
               "%include 'shared/uses_adam.sas';"), program_a)
  writeLines(c(sprintf("libname adam '%s';", library_b),
               "%include 'shared/uses_adam.sas';"), program_b)

  # A configured fallback is present throughout: it must never be used to pick
  # between two accessible source bindings.
  config <- list(libraries = list(adam = list(path = configured,
                                              engine = "sas7bdat",
                                              write = "rds")))
  occ <- sas_project(program_dir, config = config)$include_graph$occurrences
  occurrence_of <- function(program) {
    id <- occ$occurrence_id[include_scan_key(occ$parent_file) ==
                              include_scan_key(program)]
    stopifnot(length(id) == 1L)
    id
  }
  list(program_dir = program_dir, config = config, include_file = include_file,
       program_a = program_a, program_b = program_b,
       library_a = library_a, library_b = library_b, configured = configured,
       occurrence_a = occurrence_of(program_a),
       occurrence_b = occurrence_of(program_b))
}

test_that("scalar and expanded library config normalize identically", {
  root <- withr::local_tempdir()
  writeLines(c("libraries:", "  adam: data/adam"),
             file.path(root, "short.yml"))
  writeLines(c("libraries:", "  adam:", "    path: data/adam",
               "    engine: sas7bdat", "    write: rds"),
             file.path(root, "long.yml"))
  short <- sas_config(file.path(root, "short.yml"))$libraries$adam
  long <- sas_config(file.path(root, "long.yml"))$libraries$adam
  expect_identical(short, long)
  # The base is the configuration file's directory in canonical form, exactly
  # as `includes.roots` and `environment.autoexec` resolve it, so one directory
  # has one spelling whichever configured group named it.
  expect_identical(short$path,
                   normalizePath(file.path(include_normalize_path(root),
                                           "data/adam"),
                                 winslash = "/", mustWork = FALSE))
})

test_that("accessible source LIBNAME wins over configured fallback", {
  root <- withr::local_tempdir()
  source_lib <- file.path(root, "source-adam")
  fallback <- file.path(root, "fallback-adam")
  dir.create(source_lib); dir.create(fallback)
  writeLines(sprintf("libname adam %s; data work.x; set adam.adsl; run;",
                     deparse(source_lib)), file.path(root, "p.sas"))
  cfg <- list(libraries = list(adam = list(path = fallback,
                                           engine = "sas7bdat", write = "rds")))
  p <- sas_project(file.path(root, "p.sas"), config = cfg)
  ref <- p$lineage[p$lineage$dataset == "adam.adsl", ]
  binding <- resolve_libref_at(p$libref_registry, "adam", ref$file, ref$line)
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$selected_path, normalizePath(source_lib))
})

test_that("unavailable source uses config but accessible missing member does not", {
  root <- withr::local_tempdir()
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines("libname adam '/unmounted/study/adam'; data x; set adam.adsl; run;",
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 1L)
  expect_identical(binding$selection_origin, "configured_fallback")
  expect_identical(binding$fallback_reason, "source_path_unavailable")
})

test_that("one included file keeps distinct root-program libref contexts", {
  f <- shared_include_libref_fixture()
  p <- sas_project(f$program_dir, config = f$config)
  a <- resolve_libref_at(p$libref_registry, "adam", f$include_file, 1L,
                         root_program = f$program_a,
                         occurrence_id = f$occurrence_a)
  b <- resolve_libref_at(p$libref_registry, "adam", f$include_file, 1L,
                         root_program = f$program_b,
                         occurrence_id = f$occurrence_b)
  expect_identical(a$selected_path, normalizePath(f$library_a))
  expect_identical(b$selected_path, normalizePath(f$library_b))
  expect_error(resolve_libref_at(p$libref_registry, "adam",
                                 f$include_file, 1L),
               class = "sas2r_ambiguous_libref")
})

test_that("an accessible directory wins even when the member is absent", {
  root <- withr::local_tempdir()
  source_lib <- file.path(root, "source-adam"); dir.create(source_lib)
  fallback <- file.path(root, "fallback-adam"); dir.create(fallback)
  # The directory is readable and searchable; `adsl.sas7bdat` is simply not in
  # it. Accessibility is decided at the directory binding, so the absent member
  # must not send the binding to configuration.
  writeLines(sprintf("libname adam '%s'; data work.x; set adam.adsl; run;",
                     source_lib), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 1L)
  expect_false(file.exists(file.path(source_lib, "adsl.sas7bdat")))
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$status, "bound")
  expect_identical(binding$selected_path, normalizePath(source_lib))
  expect_identical(binding$fallback_reason, NA_character_)
})

test_that("a macro-valued or engine-unsupported source path falls back with a reason", {
  root <- withr::local_tempdir()
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(c("libname adam \"&root./adam\";",
               "data work.x; set adam.adsl; run;",
               "libname sdtm oracle '/study/sdtm';",
               "data work.y; set sdtm.dm; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback,
                                                  sdtm = fallback)))
  macro_bound <- resolve_libref_at(p$libref_registry, "adam",
                                   file.path(root, "p.sas"), 2L)
  remote_bound <- resolve_libref_at(p$libref_registry, "sdtm",
                                    file.path(root, "p.sas"), 4L)
  expect_identical(macro_bound$fallback_reason, "source_path_unresolved_macro")
  expect_identical(macro_bound$selected_path, normalizePath(fallback))
  expect_identical(remote_bound$fallback_reason, "source_engine_unsupported")
  expect_identical(remote_bound$selection_origin, "configured_fallback")
})

test_that("a cleared libref falls back to configuration from the clear onward", {
  root <- withr::local_tempdir()
  source_lib <- file.path(root, "source-adam"); dir.create(source_lib)
  fallback <- file.path(root, "fallback-adam"); dir.create(fallback)
  writeLines(c(sprintf("libname adam '%s';", source_lib),
               "data work.early; set adam.adsl; run;",
               "libname adam clear;",
               "data work.late; set adam.advs; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  early <- resolve_libref_at(p$libref_registry, "adam",
                             file.path(root, "p.sas"), 2L)
  late <- resolve_libref_at(p$libref_registry, "adam",
                            file.path(root, "p.sas"), 4L)
  expect_identical(early$action, "assign")
  expect_identical(early$selection_origin, "source")
  expect_identical(early$selected_path, normalizePath(source_lib))
  expect_identical(late$action, "clear")
  expect_identical(late$selection_origin, "configured_fallback")
  expect_identical(late$fallback_reason, "source_binding_cleared")
})

test_that("one root program's binding never leaks into another root program", {
  root <- withr::local_tempdir()
  lib_a <- file.path(root, "a-lib"); dir.create(lib_a)
  writeLines(c(sprintf("libname adam '%s';", lib_a),
               "data work.x; set adam.adsl; run;"), file.path(root, "a.sas"))
  writeLines("data work.y; set adam.advs; run;", file.path(root, "b.sas"))
  p <- sas_project(root)
  # a.sas is scanned first, so a global last-writer map would hand b.sas the
  # binding a.sas made. Execution context must not.
  a <- resolve_libref_at(p$libref_registry, "adam", file.path(root, "a.sas"), 2L)
  b <- resolve_libref_at(p$libref_registry, "adam", file.path(root, "b.sas"), 1L)
  expect_identical(a$selection_origin, "source")
  expect_identical(a$selected_path, normalizePath(lib_a))
  expect_identical(b$selection_origin, "none")
  expect_identical(b$status, "unbound")
  expect_identical(b$selected_path, NA_character_)
})

test_that("autoexec bindings are a prologue to every root program", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "env-adam"); dir.create(lib)
  writeLines(sprintf("libname adam '%s';", lib), file.path(root, "autoexec.sas"))
  writeLines("data work.x; set adam.adsl; run;", file.path(root, "a.sas"))
  writeLines("data work.y; set adam.advs; run;", file.path(root, "b.sas"))
  p <- sas_project(root)
  for (prog in c("a.sas", "b.sas")) {
    binding <- resolve_libref_at(p$libref_registry, "adam",
                                 file.path(root, prog), 1L)
    expect_identical(binding$selection_origin, "source")
    expect_identical(binding$selected_path, normalizePath(lib))
  }
  # The prologue executes inside each root program's own context, so the two
  # effective bindings are distinct records even though they select one path.
  ids <- vapply(c("a.sas", "b.sas"), function(prog) {
    resolve_libref_at(p$libref_registry, "adam",
                      file.path(root, prog), 1L)$binding_id
  }, character(1))
  expect_identical(length(unique(ids)), 2L)
})

test_that("an effective binding record carries all fifteen provenance fields", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_identical(
    names(binding),
    c("binding_id", "libref", "root_program", "include_occurrence_id", "file",
      "line", "action", "source_path_expression", "source_path",
      "configured_path", "selected_path", "selection_origin", "status",
      "fallback_reason", "context_truncated")
  )
  expect_match(binding$binding_id, "^libref_[0-9a-f]{32}$")
  expect_identical(binding$libref, "adam")
  expect_identical(include_scan_key(binding$root_program),
                   include_scan_key(file.path(root, "p.sas")))
  expect_identical(binding$include_occurrence_id, NA_character_)
  expect_identical(binding$line, 1L)
  expect_identical(binding$source_path_expression, lib)
  expect_identical(binding$configured_path, NA_character_)
  expect_identical(binding$status, "bound")
})

test_that("every non-work lineage row carries a binding id and status", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;",
               "data work.y; set sdtm.dm; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  expect_true(all(c("binding_id", "binding_status") %in% names(p$lineage)))

  bound <- p$lineage[p$lineage$dataset == "adam.adsl", ]
  expect_identical(nrow(bound), 1L)
  expect_false(is.na(bound$binding_id))
  expect_identical(bound$binding_status, "bound")

  # An undeclared libref still gets a record, so no non-work row is silent.
  undeclared <- p$lineage[p$lineage$dataset == "sdtm.dm", ]
  expect_false(is.na(undeclared$binding_id))
  expect_identical(undeclared$binding_status, "unbound")
  # ... and the existing undeclared-libref report is unchanged.
  expect_true("sdtm" %in% p$flags$detail[p$flags$kind == "libref_undeclared"])

  work_rows <- p$lineage[startsWith(p$lineage$dataset, "work."), ]
  expect_true(nrow(work_rows) > 0L)
  expect_true(all(is.na(work_rows$binding_id)))
  expect_true(all(is.na(work_rows$binding_status)))
})

test_that("a lineage row in a multiply-executed file is recorded as ambiguous", {
  f <- shared_include_libref_fixture()
  p <- sas_project(f$program_dir, config = f$config)
  rows <- p$lineage[p$lineage$dataset == "adam.adsl", ]
  expect_identical(nrow(rows), 1L)
  expect_identical(rows$binding_status, "ambiguous_libref")
  expect_false(is.na(rows$binding_id))
})

# B1 and B2a. The composition this fixture varies is an *unrelated* program,
# and the scan mode it varies is one no `%include` edge crosses; the shapes
# that add an include edge, or that scan a file which is also an include
# target, are pinned separately under "Fix round 3" below.
test_that("binding ids ignore location, an unrelated program, and plain scan mode", {
  make <- function(base, with_extra) {
    dir.create(file.path(base, "data", "adam"), recursive = TRUE)
    writeLines(c("libname adam 'data/adam';",
                 "data work.x; set adam.adsl; run;"),
               file.path(base, "p.sas"))
    if (with_extra) {
      writeLines("data work.z; run;", file.path(base, "aaa_extra.sas"))
    }
    base
  }
  id_of <- function(project, base) {
    resolve_libref_at(project$libref_registry, "adam",
                      file.path(base, "p.sas"), 2L)$binding_id
  }
  one <- make(withr::local_tempdir(), FALSE)
  two <- make(withr::local_tempdir(), TRUE)

  baseline <- id_of(sas_project(one), one)
  # a different absolute location, plus an unrelated program scanned before it
  expect_identical(id_of(sas_project(two), two), baseline)
  # scan mode: recursive directory scan, and a single-file scan
  expect_identical(id_of(sas_project(two, recursive = TRUE), two), baseline)
  expect_identical(id_of(sas_project(file.path(one, "p.sas")), one), baseline)
})

test_that("the registry stays empty and silent with no libname and no config", {
  root <- withr::local_tempdir()
  writeLines("data work.x; run;", file.path(root, "p.sas"))
  expect_silent(p <- sas_project(root))
  expect_identical(nrow(p$libref_registry$events), 0L)
  expect_identical(p$libref_registry$libraries, list())
})

test_that("libname _all_ clear unbinds a libref it never names", {
  root <- withr::local_tempdir()
  source_lib <- file.path(root, "source-adam"); dir.create(source_lib)
  fallback <- file.path(root, "fallback-adam"); dir.create(fallback)
  writeLines(c(sprintf("libname adam '%s';", source_lib),
               "data work.early; set adam.adsl; run;",
               "libname _all_ clear;",
               "data work.late; set adam.advs; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  early <- resolve_libref_at(p$libref_registry, "adam",
                             file.path(root, "p.sas"), 2L)
  late <- resolve_libref_at(p$libref_registry, "adam",
                            file.path(root, "p.sas"), 4L)
  expect_identical(early$selected_path, normalizePath(source_lib))
  expect_identical(late$action, "clear")
  expect_identical(late$fallback_reason, "source_binding_cleared")
  expect_identical(late$selected_path, normalizePath(fallback))
})

test_that("a geometric include fan-out is truncated and reported, not walked", {
  root <- withr::local_tempdir()
  depth <- 10L
  for (i in seq_len(depth)) {
    writeLines(rep(sprintf("%%include 'L%d.sas';", i), 2L),
               file.path(root, sprintf("L%d.sas", i - 1L)))
  }
  writeLines("data work.leaf; run;", file.path(root, sprintf("L%d.sas", depth)))

  # 2^0 + ... + 2^10 = 2047 execution frames, above LIBREF_MAX_CONTEXT_FRAMES
  p <- sas_project(file.path(root, "L0.sas"))

  expect_identical(nrow(p$libref_registry$frames), LIBREF_MAX_CONTEXT_FRAMES)
  expect_true("libref_context_truncated" %in% p$flags$kind)
  expect_identical(p$flags$detail[p$flags$kind == "libref_context_truncated"],
                   file.path(root, "L0.sas"))
  # the scan itself is unaffected: each physical file is still read once
  expect_identical(nrow(p$files), depth + 1L)
})

test_that("a point of use missing a coordinate is refused, not guessed at", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  expect_error(resolve_libref_at(p$libref_registry, "adam",
                                 file.path(root, "p.sas"), NA_integer_),
               class = "sas2r_libref_registry_error")
  expect_error(resolve_libref_at(p$libref_registry, "adam", NA_character_, 2L),
               class = "sas2r_libref_registry_error")
  expect_error(resolve_libref_at(p$libref_registry, NA_character_,
                                 file.path(root, "p.sas"), 2L),
               class = "sas2r_libref_registry_error")
  expect_error(resolve_libref_at(list(), "adam", file.path(root, "p.sas"), 2L),
               class = "sas2r_libref_registry_error")
})

test_that("a binding made inside an included file takes effect at its site", {
  root <- withr::local_tempdir()
  outer <- file.path(root, "outer-adam"); dir.create(outer)
  inner <- file.path(root, "inner-adam"); dir.create(inner)
  writeLines(sprintf("libname adam '%s';", inner), file.path(root, "inc.sas"))
  writeLines(c(sprintf("libname adam '%s';", outer),
               "data work.before; set adam.adsl; run;",
               "%include 'inc.sas';",
               "data work.after; set adam.advs; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))

  before <- resolve_libref_at(p$libref_registry, "adam",
                              file.path(root, "p.sas"), 2L)
  after <- resolve_libref_at(p$libref_registry, "adam",
                             file.path(root, "p.sas"), 4L)
  expect_identical(before$selected_path, normalizePath(outer))
  expect_identical(after$selected_path, normalizePath(inner))

  # inside the included file its own binding is already in force, and the
  # record names the include occurrence it executed under
  within <- resolve_libref_at(p$libref_registry, "adam",
                              file.path(root, "inc.sas"), 1L)
  expect_identical(within$selected_path, normalizePath(inner))
  expect_identical(within$include_occurrence_id,
                   p$include_graph$occurrences$occurrence_id[1])
  # the outer binding, made in the root program, carries no occurrence
  expect_identical(before$include_occurrence_id, NA_character_)
})

# --- Fix round 1: ambiguity is a disagreement, not a frame count -------------

# One program includes one file and both sit in the scanned directory, so the
# glob makes the included file a root program as well and it gets two frames.
# One `LIBNAME` resolved twice to one directory is not two plausible bindings,
# and this is the most ordinary project shape there is: reporting it as
# ambiguous would break code-only translation with no configuration at all.
test_that("a file that is both root and include target is not ambiguous", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.y; set adam.advs; run;"), file.path(root, "inc.sas"))
  writeLines("%include 'inc.sas';", file.path(root, "p.sas"))
  p <- sas_project(root)

  frames <- p$libref_registry$frames
  expect_identical(
    sum(frames$canonical_key == include_scan_key(file.path(root, "inc.sas"))),
    2L
  )
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "inc.sas"), 2L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$selected_path, normalizePath(lib, winslash = "/"))
  expect_identical(p$lineage$binding_status[p$lineage$dataset == "adam.advs"],
                   "bound")
})

test_that("two root programs binding one directory agree instead of colliding", {
  base <- withr::local_tempdir()
  programs <- file.path(base, "programs"); dir.create(programs)
  shared <- file.path(programs, "shared"); dir.create(shared)
  lib <- file.path(base, "adam"); dir.create(lib)
  writeLines("data work.x; set adam.adsl; run;",
             file.path(shared, "uses_adam.sas"))
  for (nm in c("a_root.sas", "b_root.sas")) {
    writeLines(c(sprintf("libname adam '%s';", lib),
                 "%include 'shared/uses_adam.sas';"), file.path(programs, nm))
  }
  p <- sas_project(programs)
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(shared, "uses_adam.sas"), 1L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$selected_path, normalizePath(lib, winslash = "/"))
})

test_that("a shared file with no source binding reaches its one configured entry", {
  base <- withr::local_tempdir()
  programs <- file.path(base, "programs"); dir.create(programs)
  shared <- file.path(programs, "shared"); dir.create(shared)
  configured <- file.path(base, "configured"); dir.create(configured)
  writeLines("data work.x; set adam.adsl; run;",
             file.path(shared, "uses_adam.sas"))
  for (nm in c("a_root.sas", "b_root.sas")) {
    writeLines("%include 'shared/uses_adam.sas';", file.path(programs, nm))
  }
  p <- sas_project(programs,
                   config = list(libraries = list(adam = configured)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(shared, "uses_adam.sas"), 1L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "configured_fallback")
  expect_identical(binding$fallback_reason, "no_source_binding")
  expect_identical(binding$selected_path,
                   normalizePath(configured, winslash = "/"))
})

test_that("frames that disagree are still ambiguous and configuration cannot break the tie", {
  f <- shared_include_libref_fixture()
  p <- sas_project(f$program_dir, config = f$config)
  record <- libref_binding_at(p$libref_registry, "adam", f$include_file, 1L)
  expect_identical(record$status, "ambiguous_libref")
  expect_identical(record$fallback_reason, "multiple_execution_contexts")
  expect_identical(record$selected_path, NA_character_)
  expect_error(resolve_libref_at(p$libref_registry, "adam", f$include_file, 1L),
               class = "sas2r_ambiguous_libref")
})

# --- Fix round 1: a LIBNAME in a macro definition is conditional -------------

test_that("a LIBNAME inside a macro definition never overrides configuration", {
  root <- withr::local_tempdir()
  ghost <- file.path(root, "ghost"); dir.create(ghost)
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(c("%macro never_called;",
               sprintf("libname adam '%s';", ghost),
               "%mend;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 4L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "configured_fallback")
  expect_identical(binding$fallback_reason, "source_binding_conditional")
  expect_identical(binding$selected_path,
                   normalizePath(fallback, winslash = "/"))
  # nothing is discarded: the macro body's claim is still on the record
  expect_identical(binding$source_path_expression, ghost)
  expect_identical(binding$source_path, normalizePath(ghost, winslash = "/"))
})

test_that("a conditional LIBNAME with no configuration is marked not established", {
  root <- withr::local_tempdir()
  ghost <- file.path(root, "ghost"); dir.create(ghost)
  writeLines(c("%macro never_called;",
               sprintf("libname adam '%s';", ghost),
               "%mend;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 4L)
  expect_identical(binding$status, "conditionally_bound")
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$fallback_reason, "source_binding_conditional")
  expect_identical(binding$selected_path, normalizePath(ghost, winslash = "/"))
  expect_identical(p$lineage$binding_status[p$lineage$dataset == "adam.adsl"],
                   "conditionally_bound")
})

test_that("a LIBNAME outside every macro definition stays established", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(c("%macro helper; %put hello; %mend;",
               sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 3L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$selected_path, normalizePath(lib, winslash = "/"))
})

# --- Fix round 1: minor corrections ------------------------------------------

test_that("a clear with no preceding assign reports no source binding", {
  root <- withr::local_tempdir()
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(c("libname adam clear;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_identical(binding$action, "clear")
  expect_identical(binding$fallback_reason, "no_source_binding")
  expect_identical(binding$selection_origin, "configured_fallback")
})

test_that("an unsupported engine over a real file is flagged, not silently unbound", {
  root <- withr::local_tempdir()
  xpt <- file.path(root, "adam.xpt"); writeLines("not really xport", xpt)
  writeLines(c(sprintf("libname adam xport '%s';", xpt),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_identical(binding$status, "unbound")
  expect_identical(binding$fallback_reason, "source_engine_unsupported")
  # the location the source named is still on the record, not dropped
  expect_identical(binding$source_path, normalizePath(xpt, winslash = "/"))
  flagged <- p$flags$detail[p$flags$kind == "libref_engine_unsupported"]
  expect_identical(flagged, "adam: xport")
})

test_that("an unsupported engine with a configured fallback is not flagged", {
  root <- withr::local_tempdir()
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(c("libname sdtm oracle '/study/sdtm';",
               "data work.y; set sdtm.dm; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(sdtm = fallback)))
  expect_false("libref_engine_unsupported" %in% p$flags$kind)
  binding <- resolve_libref_at(p$libref_registry, "sdtm",
                               file.path(root, "p.sas"), 2L)
  expect_identical(binding$selection_origin, "configured_fallback")
  # a remote engine names no filesystem object, so no path is invented for it
  expect_identical(binding$source_path, NA_character_)
})

test_that("a truncated execution context marks every binding it produced", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  depth <- 10L
  writeLines(c(sprintf("libname adam '%s';", lib),
               rep("%include 'L1.sas';", 2L)), file.path(root, "L0.sas"))
  for (i in seq(2L, depth)) {
    writeLines(rep(sprintf("%%include 'L%d.sas';", i), 2L),
               file.path(root, sprintf("L%d.sas", i - 1L)))
  }
  writeLines("data work.leaf; set adam.adsl; run;",
             file.path(root, sprintf("L%d.sas", depth)))
  p <- sas_project(file.path(root, "L0.sas"))
  expect_true("libref_context_truncated" %in% p$flags$kind)
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "L1.sas"), 1L)
  expect_true(binding$context_truncated)
})

test_that("an untruncated context says so on the record", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_false(binding$context_truncated)
})

test_that("two assigns on one line are ordered by their intra ordinal", {
  root <- withr::local_tempdir()
  first <- file.path(root, "first"); dir.create(first)
  second <- file.path(root, "second"); dir.create(second)
  writeLines(c(sprintf("libname adam '%s'; libname adam '%s';", first, second),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_identical(binding$selected_path, normalizePath(second, winslash = "/"))
  expect_identical(binding$source_path_expression, second)
})

test_that("a symlinked library resolves to its canonical directory", {
  root <- withr::local_tempdir()
  real <- file.path(root, "real-adam"); dir.create(real)
  link <- file.path(root, "link-adam")
  skip_if_not(suppressWarnings(file.symlink(real, link)), "no symlink support")
  writeLines(c(sprintf("libname adam '%s';", link),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_identical(binding$selected_path, normalizePath(real, winslash = "/"))
  # the spelling the source used is preserved separately from the resolution
  expect_identical(binding$source_path_expression, link)
})

test_that("a root program rebinds a libref its autoexec prologue set", {
  root <- withr::local_tempdir()
  env_lib <- file.path(root, "env-adam"); dir.create(env_lib)
  own_lib <- file.path(root, "own-adam"); dir.create(own_lib)
  writeLines(sprintf("libname adam '%s';", env_lib),
             file.path(root, "autoexec.sas"))
  writeLines(c("data work.early; set adam.adsl; run;",
               sprintf("libname adam '%s';", own_lib),
               "data work.late; set adam.advs; run;"), file.path(root, "a.sas"))
  p <- sas_project(root)
  early <- resolve_libref_at(p$libref_registry, "adam",
                             file.path(root, "a.sas"), 1L)
  late <- resolve_libref_at(p$libref_registry, "adam",
                            file.path(root, "a.sas"), 3L)
  expect_identical(early$selected_path, normalizePath(env_lib, winslash = "/"))
  expect_identical(late$selected_path, normalizePath(own_lib, winslash = "/"))
})

test_that("an unreadable source directory falls back with its own reason", {
  root <- withr::local_tempdir()
  locked <- file.path(root, "locked"); dir.create(locked)
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  Sys.chmod(locked, "000")
  withr::defer(Sys.chmod(locked, "700"))
  skip_if(file.access(locked, 4L) == 0L, "directory permissions not enforced")
  writeLines(c(sprintf("libname adam '%s';", locked),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 2L)
  expect_identical(binding$fallback_reason, "source_path_unreadable")
  expect_identical(binding$selection_origin, "configured_fallback")
  expect_identical(binding$source_path, normalizePath(locked, winslash = "/"))
})

test_that("two bindings tied by an inseparable include site are ambiguous", {
  root <- withr::local_tempdir()
  parent_lib <- file.path(root, "parent-adam"); dir.create(parent_lib)
  child_lib <- file.path(root, "child-adam"); dir.create(child_lib)
  writeLines(sprintf("libname adam '%s';", child_lib), file.path(root, "inc.sas"))
  # The `%include` and the `LIBNAME` share one line of one translation unit --
  # a single open DATA step swallows both -- so the execution-order key of the
  # statement written beside the include is a prefix of the key of every
  # statement the include brings in, and nothing separates them.
  writeLines(c("data work.seed;",
               sprintf("%%include 'inc.sas'; libname adam '%s';", parent_lib),
               "run;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  record <- libref_binding_at(p$libref_registry, "adam",
                              file.path(root, "p.sas"), 4L)
  expect_identical(record$status, "ambiguous_libref")
  expect_identical(record$fallback_reason, "multiple_source_bindings")
})

# --- Fix round 2: a conditional LIBNAME never un-establishes an unconditional one

test_that("a later conditional LIBNAME does not displace an established one", {
  make <- function(with_config) {
    # No .local_envir override: each root here is fully consumed into a
    # plain-scalar record before make() returns (nothing later re-reads the
    # directory from disk), so tying cleanup to make()'s own frame -- what
    # withr::local_tempdir() does by default -- is enough, and it does not
    # depend on how many frames of call-stack indirection sit above make().
    root <- withr::local_tempdir()
    real <- file.path(root, "real"); dir.create(real)
    ghost <- file.path(root, "ghost"); dir.create(ghost)
    fallback <- file.path(root, "fallback"); dir.create(fallback)
    writeLines(c(sprintf("libname adam '%s';", real),
                 "%macro never_called;",
                 sprintf("libname adam '%s';", ghost),
                 "%mend;",
                 "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
    cfg <- if (with_config) list(libraries = list(adam = fallback)) else NULL
    p <- sas_project(file.path(root, "p.sas"), config = cfg)
    list(real = normalizePath(real, winslash = "/"),
         binding = resolve_libref_at(p$libref_registry, "adam",
                                     file.path(root, "p.sas"), 5L))
  }
  # A macro-def LIBNAME does not on its own establish a binding, so it must
  # not un-establish the unconditional one written before it either -- with or
  # without configuration, the answer is the real directory.
  for (with_config in c(TRUE, FALSE)) {
    got <- make(with_config)
    expect_identical(got$binding$status, "bound")
    expect_identical(got$binding$selection_origin, "source")
    expect_identical(got$binding$selected_path, got$real)
  }
})

test_that("a later conditional LIBNAME contributes no provenance either", {
  root <- withr::local_tempdir()
  real <- file.path(root, "real"); dir.create(real)
  ghost <- file.path(root, "ghost"); dir.create(ghost)
  # The unconditional statement is written *first*, so nothing but the
  # establishment rule keeps it: by source order alone the macro-def statement
  # on line 3 is the later one and would win selection.
  writeLines(c(sprintf("libname adam '%s';", real),
               "%macro never_called;",
               sprintf("libname adam '%s';", ghost),
               "%mend;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 5L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$selected_path, normalizePath(real, winslash = "/"))
  # A statement that establishes nothing also reports nothing: the record's
  # provenance names line 1, not the macro body on line 3.
  expect_identical(binding$line, 1L)
  expect_identical(binding$source_path_expression, real)
})

test_that("a conditional-only binding still reaches its round-one answer", {
  make <- function(with_config) {
    # No .local_envir override: each root here is fully consumed into a
    # plain-scalar record before make() returns (nothing later re-reads the
    # directory from disk), so tying cleanup to make()'s own frame -- what
    # withr::local_tempdir() does by default -- is enough, and it does not
    # depend on how many frames of call-stack indirection sit above make().
    root <- withr::local_tempdir()
    ghost <- file.path(root, "ghost"); dir.create(ghost)
    fallback <- file.path(root, "fallback"); dir.create(fallback)
    writeLines(c("%macro never_called;",
                 sprintf("libname adam '%s';", ghost),
                 "%mend;",
                 "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
    cfg <- if (with_config) list(libraries = list(adam = fallback)) else NULL
    p <- sas_project(file.path(root, "p.sas"), config = cfg)
    list(ghost = normalizePath(ghost, winslash = "/"),
         fallback = normalizePath(fallback, winslash = "/"),
         binding = resolve_libref_at(p$libref_registry, "adam",
                                     file.path(root, "p.sas"), 4L))
  }
  configured <- make(TRUE)
  expect_identical(configured$binding$status, "bound")
  expect_identical(configured$binding$selection_origin, "configured_fallback")
  expect_identical(configured$binding$fallback_reason,
                   "source_binding_conditional")
  expect_identical(configured$binding$selected_path, configured$fallback)

  bare <- make(FALSE)
  expect_identical(bare$binding$status, "conditionally_bound")
  expect_identical(bare$binding$selection_origin, "source")
  expect_identical(bare$binding$selected_path, bare$ghost)
})

# --- Fix round 2: binding identity carries no scan order ---------------------

test_that("a binding id is stable when another root program joins the project", {
  base <- withr::local_tempdir()
  programs <- file.path(base, "programs"); dir.create(programs)
  shared <- file.path(programs, "shared"); dir.create(shared)
  lib <- file.path(base, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"),
             file.path(shared, "uses_adam.sas"))
  add_root <- function(nm) {
    writeLines("%include 'shared/uses_adam.sas';", file.path(programs, nm))
  }
  cfg <- list(includes = list(roots = programs))
  id_of <- function() {
    p <- sas_project(programs, config = cfg)
    libref_binding_at(p$libref_registry, "adam",
                      file.path(shared, "uses_adam.sas"), 2L)$binding_id
  }
  add_root("m_root.sas")
  only_m <- id_of()
  # Adding an unrelated program that also includes the file changes which
  # context is first in registry order. The effective binding is the same
  # statement selecting the same directory, so the id must not move.
  add_root("a_root.sas")
  expect_identical(id_of(), only_m)
})

# Round 4 (Item 3) replaced round 2's per-context id. Naming an execution
# context asks *where* to look, not *which binding* to identify: when two
# contexts reach the same binding they must produce one id, or a caller that
# holds the root program and passes it gets a silent zero-match join against
# `lineage$binding_id`, which never names a context.
test_that("naming an execution context does not move the binding id", {
  base <- withr::local_tempdir()
  programs <- file.path(base, "programs"); dir.create(programs)
  shared <- file.path(programs, "shared"); dir.create(shared)
  lib <- file.path(base, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"),
             file.path(shared, "uses_adam.sas"))
  for (nm in c("a_root.sas", "m_root.sas")) {
    writeLines("%include 'shared/uses_adam.sas';", file.path(programs, nm))
  }
  p <- sas_project(programs, config = list(includes = list(roots = programs)))
  use <- file.path(shared, "uses_adam.sas")
  ids <- vapply(c("a_root.sas", "m_root.sas"), function(nm) {
    resolve_libref_at(p$libref_registry, "adam", use, 2L,
                      root_program = file.path(programs, nm))$binding_id
  }, character(1))
  # The file really does execute in two contexts, and both reach the same
  # `LIBNAME` in the file itself, so this is one binding asked about twice.
  expect_identical(
    nrow(libref_matching_frames(p$libref_registry, use, NULL, NULL)), 2L)
  expect_identical(length(unique(ids)), 1L)
  # ... and the unnamed call, which is what lineage annotation makes, agrees.
  free <- libref_binding_at(p$libref_registry, "adam", use, 2L)$binding_id
  expect_identical(unname(ids[[1]]), free)
})

# --- Fix round 2: minor corrections ------------------------------------------

test_that("an unusable conditional path reports its own reason, not conditionality", {
  root <- withr::local_tempdir()
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  missing <- file.path(root, "nope", "missing")
  writeLines(c("%macro never_called;",
               sprintf("libname adam '%s';", missing),
               "%mend;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 4L)
  expect_identical(binding$selection_origin, "configured_fallback")
  expect_identical(binding$fallback_reason, "source_path_unavailable")
})

test_that("an unsupported engine a later LIBNAME replaces is not flagged", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  xpt <- file.path(root, "adam.xpt"); writeLines("not really xport", xpt)
  writeLines(c(sprintf("libname adam xport '%s';", xpt),
               sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 3L)
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "source")
  expect_identical(binding$selected_path, normalizePath(lib, winslash = "/"))
  # the effective binding is the local directory, so nothing was downgraded
  expect_false("libref_engine_unsupported" %in% p$flags$kind)
})

test_that("truncation elsewhere does not mark a reference it could not reach", {
  root <- withr::local_tempdir()
  lib <- file.path(root, "adam"); dir.create(lib)
  depth <- 10L
  writeLines(c(sprintf("libname adam '%s';", lib),
               rep("%include 'L1.sas';", 2L)), file.path(root, "L0.sas"))
  for (i in seq(2L, depth)) {
    writeLines(rep(sprintf("%%include 'L%d.sas';", i), 2L),
               file.path(root, sprintf("L%d.sas", i - 1L)))
  }
  writeLines("data work.leaf; set adam.adsl; run;",
             file.path(root, sprintf("L%d.sas", depth)))
  p <- sas_project(file.path(root, "L0.sas"))
  expect_true("libref_context_truncated" %in% p$flags$kind)
  # a file the scan never saw at all cannot have been dropped by the cap
  outsider <- resolve_libref_at(p$libref_registry, "adam",
                                file.path(root, "not-scanned.sas"), 1L)
  expect_identical(nrow(libref_matching_frames(p$libref_registry,
                                               file.path(root, "not-scanned.sas"),
                                               NULL, NULL)), 0L)
  expect_false(outsider$context_truncated)
})

# --- Fix round 3: binding identity is anchored on the point of use -----------

# One `p.sas` that binds `adam` and uses it, optionally joined by a program
# that `%include`s it. Adding that program does not touch `p.sas`, so every
# binding `p.sas` already had must keep the id it already had -- and the
# `data/adam` path expression is *relative*, so the source text, and with it
# the identity, is byte-identical wherever the fixture is created.
libref_include_join_fixture <- function(base, with_includer) {
  dir.create(file.path(base, "data", "adam"), recursive = TRUE)
  writeLines(c("libname adam 'data/adam';",
               "data work.x; set adam.adsl; run;"), file.path(base, "p.sas"))
  if (with_includer) {
    writeLines("%include 'p.sas';", file.path(base, "aaa_includes_p.sas"))
  }
  base
}

# B2b.
test_that("a binding id survives a new program that includes the file", {
  id_of <- function(project, base) {
    libref_binding_at(project$libref_registry, "adam",
                      file.path(base, "p.sas"), 2L)$binding_id
  }
  alone <- libref_include_join_fixture(withr::local_tempdir(), FALSE)
  joined <- libref_include_join_fixture(withr::local_tempdir(), TRUE)
  joined_project <- sas_project(joined)
  # p.sas really is executed twice now -- once as a root program and once as
  # an include target -- so this is the composition change, not a no-op.
  expect_identical(
    nrow(libref_matching_frames(joined_project$libref_registry,
                                file.path(joined, "p.sas"), NULL, NULL)),
    2L
  )
  # Both executions run the same statement and select the same directory, so
  # the effective binding at p.sas:2 is the one it always was.
  expect_identical(id_of(joined_project, joined),
                   id_of(sas_project(alone), alone))
})

# B3, scan *mode* only. Both scans below root the project at `base` --
# `sas_project()` roots a single-file scan at the file's own directory -- so
# what varies here is how one root is walked, not which root it is. Scanning a
# *different* root is a different question and is pinned, in the direction it
# fails, by "a binding id moves when the scan root moves" below.
test_that("a binding id is the same for a file and directory scan at one root", {
  base <- libref_include_join_fixture(withr::local_tempdir(), TRUE)
  id_of <- function(project) {
    libref_binding_at(project$libref_registry, "adam",
                      file.path(base, "p.sas"), 2L)$binding_id
  }
  # The directory scan sees p.sas twice, the single-file scan once. Scan mode
  # is not a property of the binding.
  expect_identical(id_of(sas_project(file.path(base, "p.sas"))),
                   id_of(sas_project(base)))
})

# Two root programs that each bind `adam` themselves and then include one
# shared file. `exprs` names the path expression each root writes. Both
# `data/adam` and `data/other` exist, so a fixture that names the other one is
# still `bound` from `source` and differs from its sibling in the selected
# directory alone.
libref_two_root_fixture <- function(base, exprs) {
  programs <- file.path(base, "programs"); dir.create(programs)
  shared <- file.path(programs, "shared"); dir.create(shared)
  dir.create(file.path(programs, "data", "adam"), recursive = TRUE)
  dir.create(file.path(programs, "data", "other"), recursive = TRUE)
  writeLines("data work.x; set adam.adsl; run;",
             file.path(shared, "uses_adam.sas"))
  for (nm in names(exprs)) {
    writeLines(c(sprintf("libname adam '%s';", exprs[[nm]]),
                 "%include 'shared/uses_adam.sas';"), file.path(programs, nm))
  }
  libref_binding_at(sas_project(programs)$libref_registry, "adam",
                    file.path(shared, "uses_adam.sas"), 1L)
}

# B2c: the defining LIBNAME sits in the differing root programs rather than in
# the shared file, so the two execution frames select different *statements*
# that agree on the directory.
test_that("a binding id survives a second root program that binds it identically", {
  only_m <- libref_two_root_fixture(withr::local_tempdir(),
                                    list(m_root.sas = "data/adam"))
  both <- libref_two_root_fixture(withr::local_tempdir(),
                                  list(a_root.sas = "data/adam",
                                       m_root.sas = "data/adam"))
  expect_identical(both$status, "bound")
  expect_identical(both$selection_origin, "source")
  expect_identical(both$binding_id, only_m$binding_id)
})

# The round-3 residual of B2c, closed in round 4 (Item 2). Two agreeing frames
# reaching one directory through *different* literal spellings used to mint two
# ids, so adding a root program re-keyed the binding. Round 3 argued the only
# exits were dropping the path from the identity -- which would let an edited
# `LIBNAME` keep the id of the library it no longer names -- or reporting
# agreeing frames as ambiguous, which round 1 deliberately stopped doing. There
# is a third: key on the selected directory re-expressed through the anchor
# frame `use_identity` already uses. The spelling stops mattering; the
# directory does not.
test_that("agreeing frames spelling one directory two ways keep one id", {
  only_m <- libref_two_root_fixture(withr::local_tempdir(),
                                    list(m_root.sas = "data/adam"))
  both <- libref_two_root_fixture(withr::local_tempdir(),
                                  list(a_root.sas = "./data/adam",
                                       m_root.sas = "data/adam"))
  # The two fixtures sit at different temporary roots, so the directory they
  # agree on is compared by its in-project spelling.
  in_project <- function(record) sub("^.*/programs/", "", record$selected_path)
  expect_identical(in_project(both), "data/adam")
  expect_identical(in_project(both), in_project(only_m))
  expect_identical(both$binding_id, only_m$binding_id)
})

# **Sole guard on the "edited `LIBNAME`" property.** Two tests fail when
# `path_identity` is dropped from `libref_binding_id()` -- this one and "two
# bindings at one point of use selecting two directories differ" below -- but
# they guard different things: that one is about two bindings alive at once,
# and this one is the only cover on a binding *edited over time*. A `LIBNAME`
# changed to name a different library must not inherit the id of the one it no
# longer names, because output evidence joins on `binding_id` and a stale join
# is worse than a re-key. Do not delete it while closing something else --
# replace it with another test of the same property first.
test_that("a root program naming a different directory re-keys the binding", {
  only_m <- libref_two_root_fixture(withr::local_tempdir(),
                                    list(m_root.sas = "data/adam"))
  other <- libref_two_root_fixture(withr::local_tempdir(),
                                   list(m_root.sas = "data/other"))
  # Both are accessible source bindings at one point of use, so every other
  # identity input -- libref, status, origin, action, use file, use line -- is
  # identical and only the directory can separate the two ids.
  for (field in c("libref", "status", "selection_origin", "action")) {
    expect_identical(other[[field]], only_m[[field]])
  }
  expect_identical(other$status, "bound")
  expect_false(identical(other$binding_id, only_m$binding_id))
})

# B4, in the sharpest shape there is: one physical point of use, one libref,
# two directories. Nothing about the *place* of use separates these two
# records, so only the identity's own inputs can.
test_that("two bindings at one point of use selecting two directories differ", {
  f <- shared_include_libref_fixture()
  p <- sas_project(f$program_dir, config = f$config)
  a <- resolve_libref_at(p$libref_registry, "adam", f$include_file, 1L,
                         root_program = f$program_a,
                         occurrence_id = f$occurrence_a)
  b <- resolve_libref_at(p$libref_registry, "adam", f$include_file, 1L,
                         root_program = f$program_b,
                         occurrence_id = f$occurrence_b)
  expect_false(identical(a$selected_path, b$selected_path))
  expect_false(identical(a$binding_id, b$binding_id))
})

# B4 again, within one execution: a rebinding between two points of use is a
# different binding and gets a different id.
test_that("a rebound libref mints a new id at the point of use below it", {
  root <- withr::local_tempdir()
  first_dir <- file.path(root, "adam-one"); dir.create(first_dir)
  second_dir <- file.path(root, "adam-two"); dir.create(second_dir)
  writeLines(c("libname adam 'adam-one';",
               "data work.x; set adam.adsl; run;",
               "libname adam 'adam-two';",
               "data work.y; set adam.advs; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  before <- resolve_libref_at(p$libref_registry, "adam",
                              file.path(root, "p.sas"), 2L)
  after <- resolve_libref_at(p$libref_registry, "adam",
                             file.path(root, "p.sas"), 4L)
  expect_identical(before$selected_path,
                   normalizePath(first_dir, winslash = "/"))
  expect_identical(after$selected_path,
                   normalizePath(second_dir, winslash = "/"))
  expect_false(identical(before$binding_id, after$binding_id))
})

# --- Fix round 3: minor corrections ------------------------------------------

test_that("a clear after only a conditional assign has nothing to clear", {
  root <- withr::local_tempdir()
  ghost <- file.path(root, "ghost"); dir.create(ghost)
  fallback <- file.path(root, "fallback"); dir.create(fallback)
  writeLines(c("%macro never_called;",
               sprintf("libname adam '%s';", ghost),
               "%mend;",
               "libname adam clear;",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"),
                   config = list(libraries = list(adam = fallback)))
  binding <- resolve_libref_at(p$libref_registry, "adam",
                               file.path(root, "p.sas"), 5L)
  # The macro-def assignment established nothing, so the clear on line 4 had
  # nothing to take away and must not claim it did.
  expect_identical(binding$fallback_reason, "no_source_binding")
  # The location answer is the same either way: this is a reason token, not a
  # change of behaviour.
  expect_identical(binding$status, "bound")
  expect_identical(binding$selection_origin, "configured_fallback")
  expect_identical(binding$selected_path,
                   normalizePath(fallback, winslash = "/"))
})

test_that("an unsupported-engine detail with no defining event is dropped", {
  root <- withr::local_tempdir()
  xpt <- file.path(root, "adam.xpt"); writeLines("not really xport", xpt)
  writeLines(c(sprintf("libname adam xport '%s';", xpt),
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  records <- attach_lineage_bindings(p$lineage, p$libref_registry)$records
  expect_identical(libref_unsupported_engine_details(p$libref_registry, records),
                   "adam: xport")
  # The engine token is read back off the defining event, because the record
  # carries no engine of its own. A record whose provenance matches no event
  # has no engine to report, and reporting "adam: NA" would be a flag with
  # nothing in it.
  orphan <- lapply(records, function(r) {
    r$source_path_expression <- paste0(r$source_path_expression, "-moved")
    r
  })
  expect_identical(libref_unsupported_engine_details(p$libref_registry, orphan),
                   character())
})

# --- Fix round 4: every identity input has a test that fails without it ------

# **Sole guard on `use_line`.** Two points of use of *one* binding, at two
# lines of one file: same expression, same status, same origin, same selected
# directory, and the same defining statement, so `use_line` is the only
# identity input that separates them and deleting it collapses the two ids into
# one. The round-3 test named for this property (`a rebound libref mints a new
# id at the point of use below it`) also varies the path expression, so it
# separates on the path and passes without `use_line` in the key.
test_that("two points of use of one binding differ only by line, and still differ", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data", "adam"), recursive = TRUE)
  writeLines(c("libname adam 'data/adam';",
               "data work.x; set adam.adsl; run;",
               "data work.y; set adam.advs; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  at <- function(line) {
    resolve_libref_at(p$libref_registry, "adam", file.path(root, "p.sas"), line)
  }
  first <- at(2L)
  second <- at(3L)
  # `file` and `line` here are the *defining* statement's, so this asserts the
  # two records name one `LIBNAME` as well as one directory.
  for (field in c("libref", "status", "selection_origin", "action", "file",
                  "line", "source_path_expression", "selected_path")) {
    expect_identical(first[[field]], second[[field]])
  }
  expect_identical(first$line, 1L)
  expect_false(identical(first$binding_id, second$binding_id))
})

# Item 3, in the shape that made the ruling: the simplest project there is.
# `lineage$binding_id` is minted by an unnamed call, and a later task that
# happens to hold the root program and passes it must land on the same row
# rather than on nothing at all.
test_that("a lineage binding id equals the one a named root program resolves", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data", "adam"), recursive = TRUE)
  writeLines(c("libname adam 'data/adam';",
               "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  row <- p$lineage[p$lineage$dataset == "adam.adsl", ]
  expect_identical(nrow(row), 1L)
  named <- resolve_libref_at(p$libref_registry, "adam", row$file, row$line,
                             root_program = file.path(root, "p.sas"))
  expect_identical(row$binding_id, named$binding_id)
  # ... and naming the occurrence as well, which for a root frame is NA, is
  # still the same binding.
  free <- resolve_libref_at(p$libref_registry, "adam", row$file, row$line)
  expect_identical(free$binding_id, named$binding_id)
})

# Item 4: the accepted scan-*root* residual of B3, pinned in the direction it
# fails so a later round cannot flip the trade-off in silence. The identity
# frame is root-relative and `sas_project()` roots it at the scanned directory,
# so one file is `sub/p.sas` under one scan and `p.sas` under the other. This
# is inherent to any root-relative frame and is the same residual
# `include_identity_anchors()` already accepts for occurrence ids; the remedy
# is to pick one scan scope per project, not to close it here.
test_that("a binding id moves when the scan root moves", {
  outer <- withr::local_tempdir()
  proj <- file.path(outer, "proj"); dir.create(proj)
  sub <- file.path(proj, "sub"); dir.create(sub)
  # The library sits outside *both* candidate roots and is named absolutely,
  # so `selected_path` is byte-identical under the two scans and its anchor key
  # is too. The point of use's own frame is the only thing that moves.
  lib <- file.path(outer, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam '%s';", lib),
               "data work.x; set adam.adsl; run;"), file.path(sub, "p.sas"))
  record_of <- function(project) {
    libref_binding_at(project$libref_registry, "adam",
                      file.path(sub, "p.sas"), 2L)
  }
  wide <- record_of(sas_project(proj, recursive = TRUE))
  narrow <- record_of(sas_project(file.path(sub, "p.sas")))
  expect_identical(wide$status, "bound")
  expect_identical(narrow$selected_path, wide$selected_path)
  expect_false(identical(narrow$binding_id, wide$binding_id))
})

# The mutation sweep in the round-4 report found `libref`, `status`, and
# `selection_origin` were in the key with nothing pinning them. Each of the
# three below is the sole cover on its input: it holds every other identity
# input fixed -- including the point of use, which is what makes them hard to
# vary one at a time -- so deleting that input from `libref_binding_id()` fails
# exactly this test.

# `libref`.
test_that("two librefs naming one directory at one point of use differ", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data", "shared"), recursive = TRUE)
  writeLines(c("libname adam 'data/shared';",
               "libname sdtm 'data/shared';",
               "data work.x; set adam.adsl; set sdtm.dm; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  at <- function(libref) {
    resolve_libref_at(p$libref_registry, libref, file.path(root, "p.sas"), 3L)
  }
  adam <- at("adam")
  sdtm <- at("sdtm")
  # One directory, one point of use, one status and origin: only the libref
  # itself separates the two bindings.
  expect_identical(adam$selected_path, sdtm$selected_path)
  expect_identical(adam$status, sdtm$status)
  expect_identical(adam$selection_origin, sdtm$selection_origin)
  expect_false(identical(adam$binding_id, sdtm$binding_id))
})

# `status`. A reported location and an established one are different facts
# about the same directory, so a consumer that treats `conditionally_bound` as
# unusable must not see the id of the `bound` record.
test_that("an established and a conditional binding on one directory differ", {
  make <- function(conditional) {
    # No .local_envir override: each root here is fully consumed into a
    # plain-scalar record before make() returns (nothing later re-reads the
    # directory from disk), so tying cleanup to make()'s own frame -- what
    # withr::local_tempdir() does by default -- is enough, and it does not
    # depend on how many frames of call-stack indirection sit above make().
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data", "adam"), recursive = TRUE)
    # The whole macro definition sits on line 1 so the point of use is line 2
    # in both projects. The defining site is not an identity input, but
    # `use_line` is.
    first <- if (conditional) {
      "%macro m; libname adam 'data/adam'; %mend;"
    } else {
      "libname adam 'data/adam';"
    }
    writeLines(c(first, "data work.x; set adam.adsl; run;"),
               file.path(root, "p.sas"))
    p <- sas_project(file.path(root, "p.sas"))
    resolve_libref_at(p$libref_registry, "adam", file.path(root, "p.sas"), 2L)
  }
  established <- make(FALSE)
  conditional <- make(TRUE)
  expect_identical(established$status, "bound")
  expect_identical(conditional$status, "conditionally_bound")
  expect_identical(established$selection_origin, conditional$selection_origin)
  expect_identical(established$action, conditional$action)
  expect_identical(basename(established$selected_path), "adam")
  expect_identical(basename(conditional$selected_path), "adam")
  expect_false(identical(established$binding_id, conditional$binding_id))
})

# `selection_origin`. Both records are `bound`, both name `data/adam` in the
# key -- one because the source reached it, the other because the source asked
# for it and missed -- so origin is the only thing left to tell "the library
# the program named" from "the configured library standing in for it".
test_that("a source binding and a fallback for the same expression differ", {
  make <- function(present) {
    # No .local_envir override: each root here is fully consumed into a
    # plain-scalar record before make() returns (nothing later re-reads the
    # directory from disk), so tying cleanup to make()'s own frame -- what
    # withr::local_tempdir() does by default -- is enough, and it does not
    # depend on how many frames of call-stack indirection sit above make().
    root <- withr::local_tempdir()
    if (present) dir.create(file.path(root, "data", "adam"), recursive = TRUE)
    fallback <- file.path(root, "fallback"); dir.create(fallback)
    writeLines(c("libname adam 'data/adam';",
                 "data work.x; set adam.adsl; run;"), file.path(root, "p.sas"))
    p <- sas_project(file.path(root, "p.sas"),
                     config = list(libraries = list(adam = fallback)))
    resolve_libref_at(p$libref_registry, "adam", file.path(root, "p.sas"), 2L)
  }
  from_source <- make(TRUE)
  from_config <- make(FALSE)
  expect_identical(from_source$status, from_config$status)
  expect_identical(from_source$status, "bound")
  expect_identical(from_source$selection_origin, "source")
  expect_identical(from_config$selection_origin, "configured_fallback")
  # The key's path field is `data/adam` for both: the anchor key of the
  # directory the source reached, and the expression the source missed with.
  expect_identical(from_source$source_path_expression, "data/adam")
  expect_identical(from_config$source_path_expression, "data/adam")
  expect_identical(from_source$action, from_config$action)
  expect_false(identical(from_source$binding_id, from_config$binding_id))
})

# `action`. Both records fall back to the same configured library at the same
# point of use with no source location at all, so `action` is the only input
# left -- and it carries a real distinction: "nothing ever bound `adam` here"
# and "`adam` was bound and then cleared here" are different findings, and a
# report joined on `binding_id` must not conflate an undeclared libref with a
# deliberately cleared one.
test_that("a cleared binding and a never-bound one differ at one point of use", {
  make <- function(lines) {
    # No .local_envir override: each root here is fully consumed into a
    # plain-scalar record before make() returns (nothing later re-reads the
    # directory from disk), so tying cleanup to make()'s own frame -- what
    # withr::local_tempdir() does by default -- is enough, and it does not
    # depend on how many frames of call-stack indirection sit above make().
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data", "adam"), recursive = TRUE)
    fallback <- file.path(root, "fallback"); dir.create(fallback)
    writeLines(lines, file.path(root, "p.sas"))
    p <- sas_project(file.path(root, "p.sas"),
                     config = list(libraries = list(adam = fallback)))
    resolve_libref_at(p$libref_registry, "adam", file.path(root, "p.sas"), 3L)
  }
  # Two filler lines, so the point of use sits on line 3 in both.
  never <- make(c("data work.z; run;", "data work.w; run;",
                  "data work.x; set adam.adsl; run;"))
  cleared <- make(c("libname adam 'data/adam';", "libname adam clear;",
                    "data work.x; set adam.adsl; run;"))
  expect_identical(never$status, cleared$status)
  expect_identical(never$selection_origin, cleared$selection_origin)
  # A clear names no path, so the key's path field is empty for both.
  expect_identical(never$source_path_expression, NA_character_)
  expect_identical(cleared$source_path_expression, NA_character_)
  expect_identical(never$action, NA_character_)
  expect_identical(cleared$action, "clear")
  expect_identical(never$fallback_reason, "no_source_binding")
  expect_identical(cleared$fallback_reason, "source_binding_cleared")
  expect_false(identical(never$binding_id, cleared$binding_id))
})

# --- Fix round 5: the out-of-anchor `path_identity` residual (Item 1) --------

# `path_identity` is [libref_anchor_identity()] of the selected directory,
# which falls back to the directory's absolute path when it lies under no
# anchor. A *relative* `LIBNAME` that escapes the project root reaches exactly
# that fallback, so two checkouts of byte-identical source at two different
# absolute bases mint two different `binding_id`s -- the same class of
# out-of-anchor exposure already accepted for `use_identity` and for
# occurrence ids, pinned here in the direction it fails so a later round
# cannot silently flip the trade-off. Naming the library's parent directory in
# `includes.roots` turns it into an anchor and restores invariance, which the
# companion test below pins as well.
libref_out_of_root_fixture <- function(base, include_roots = character()) {
  proj <- file.path(base, "proj"); dir.create(proj)
  dir.create(file.path(base, "shared", "adam"), recursive = TRUE)
  writeLines(c("libname adam '../shared/adam';",
               "data work.x; set adam.adsl; run;"), file.path(proj, "p.sas"))
  cfg <- if (length(include_roots)) list(include_roots = include_roots) else NULL
  p <- sas_project(proj, config = cfg)
  libref_binding_at(p$libref_registry, "adam", file.path(proj, "p.sas"), 2L)
}

test_that("a relative LIBNAME escaping the project root differs across checkouts", {
  checkout_a <- libref_out_of_root_fixture(withr::local_tempdir())
  checkout_b <- libref_out_of_root_fixture(withr::local_tempdir())
  expect_identical(checkout_a$status, "bound")
  expect_identical(checkout_a$selection_origin, "source")
  expect_identical(checkout_a$status, checkout_b$status)
  expect_identical(checkout_a$selection_origin, checkout_b$selection_origin)
  # The two checkouts really do select physically different directories --
  # different absolute temp bases -- so a differing id below is not a fixture
  # bug: the *source* is byte-identical and only the checkout's own location
  # moved.
  expect_false(identical(checkout_a$selected_path, checkout_b$selected_path))
  expect_false(identical(checkout_a$binding_id, checkout_b$binding_id))
})

test_that("naming the escaping library's parent in include_roots restores invariance", {
  make <- function(base) {
    # The library's parent directory becomes anchor <include-root:1>, so
    # libref_anchor_identity() names the library against it instead of
    # falling back to its absolute path -- the same anchor list use_identity
    # already reads.
    libref_out_of_root_fixture(base, include_roots = file.path(base, "shared"))
  }
  checkout_a <- make(withr::local_tempdir())
  checkout_b <- make(withr::local_tempdir())
  expect_identical(checkout_a$status, "bound")
  expect_identical(checkout_a$selection_origin, "source")
  # Still physically different directories; only the id's encoding of the
  # directory changed.
  expect_false(identical(checkout_a$selected_path, checkout_b$selected_path))
  expect_identical(checkout_a$binding_id, checkout_b$binding_id)
})

test_that("LIBNAME CLEAR removes the active runtime binding", {
  root <- withr::local_tempdir(); lib <- file.path(root, "adam"); dir.create(lib)
  writeLines(c(sprintf("libname adam %s;", deparse(lib)),
               "libname adam clear;", "data x; set adam.adsl; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  expect_identical(resolve_libref_at(p$libref_registry, "adam",
                                     file.path(root, "p.sas"), 3L)$status,
                   "unbound")
})

test_that("a malformed point-of-use record yields NA, never a false zero value", {
  # `field()` reads one value per record. A record whose field is missing or
  # not scalar used to fall back to the vapply template -- "" for a character
  # and FALSE for a logical -- so `context_truncated` became a positive claim
  # that the frame walk completed, and `binding_id`, the column two later
  # tasks join on, became the empty string.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "src"))
  writeLines(c("libname adam \"src\";", "data x; set adam.adsl; run;"),
             file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"))
  real <- sas2r:::libref_point_of_use_records
  testthat::local_mocked_bindings(
    libref_point_of_use_records = function(registry, libref, file, line) {
      out <- real(registry, libref, file, line)
      out$records <- lapply(out$records, function(r) {
        r$binding_id <- character()
        r$context_truncated <- logical()
        r$line <- integer()
        r
      })
      out
    },
    .package = "sas2r"
  )
  b <- sas2r:::effective_librefs(p)$bindings
  expect_true(all(is.na(b$binding_id)))
  expect_true(all(is.na(b$context_truncated)))
  expect_true(all(is.na(b$line)))
  expect_type(b$binding_id, "character")
  expect_type(b$context_truncated, "logical")
  expect_type(b$line, "integer")
})

test_that("build_attempt_library_map creates attempt-local candidate write paths", {
  root <- withr::local_tempdir()
  adam_dir <- file.path(root, "data", "adam")
  dir.create(adam_dir, recursive = TRUE)
  writeLines("data work.x; set adam.adsl; run;", file.path(root, "p.sas"))
  p <- sas_project(file.path(root, "p.sas"), config = list(libraries = list(adam = adam_dir)))

  attempt_dir <- file.path(root, "attempt_1")
  lib_map <- sas2r:::build_attempt_library_map(p, attempt_dir)

  expect_setequal(names(lib_map), c("adam", "work"))
  expect_identical(lib_map$adam$read_path, normalizePath(adam_dir, winslash = "/", mustWork = FALSE))
  expect_identical(lib_map$adam$write_path, normalizePath(file.path(attempt_dir, "adam"), winslash = "/", mustWork = FALSE))
  expect_identical(lib_map$adam$engine, "sas7bdat")
  expect_identical(lib_map$adam$write, "rds")

  expect_identical(lib_map$work$read_path, normalizePath(file.path(attempt_dir, "work"), winslash = "/", mustWork = FALSE))
  expect_identical(lib_map$work$write_path, normalizePath(file.path(attempt_dir, "work"), winslash = "/", mustWork = FALSE))
})

