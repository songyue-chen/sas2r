test_that("quoted includes are resolved, scanned, and their macros participate", {
  dir <- withr::local_tempdir()
  writeLines("%include 'macros/prep.sas';\n%prep()\ndata a; set b; run;",
             file.path(dir, "driver.sas"))
  dir.create(file.path(dir, "macros"))
  writeLines("%macro prep; options nodate; %mend;",
             file.path(dir, "macros", "prep.sas"))
  p <- sas_project(dir)
  expect_identical(sort(unique(p$files$origin)), c("included", "program"))
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "prep"], "resolved_project")
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("include chains resolve transitively", {
  dir <- withr::local_tempdir()
  writeLines("%include 'b.sas';\ndata x; run;", file.path(dir, "a.sas"))
  writeLines("%include 'c.sas';", file.path(dir, "b.sas"))
  writeLines("%macro deep; %mend;", file.path(dir, "c.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_identical(sum(p$files$origin == "included"), 2L)
})

test_that("include cycles are flagged, not infinite", {
  dir <- withr::local_tempdir()
  writeLines("%include 'b.sas';", file.path(dir, "a.sas"))
  writeLines("%include 'a.sas';", file.path(dir, "b.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_true("include_cycle" %in% p$flags$kind)
})

test_that("missing target keeps unresolved_include; config include_roots searched", {
  dir <- withr::local_tempdir()
  writeLines("%include 'nowhere.sas';", file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_true("unresolved_include" %in% p$flags$kind)
  shared <- withr::local_tempdir()
  writeLines("%macro fromroot; %mend;", file.path(shared, "found.sas"))
  writeLines(sprintf("includes:\n  roots:\n    - %s", shared),
             file.path(dir, "_sas2r.yml"))
  writeLines("%include 'found.sas';", file.path(dir, "b.sas"))
  p2 <- sas_project(file.path(dir, "b.sas"))
  expect_false("unresolved_include" %in% p2$flags$kind)
})

test_that("recursive = TRUE discovers .sas in subdirectories", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "sub"))
  writeLines("data a; run;", file.path(dir, "sub", "deep.sas"))
  expect_identical(nrow(sas_project(dir)$files), 0L)
  expect_identical(nrow(sas_project(dir, recursive = TRUE)$files), 1L)
})

test_that("include depth exceeding 10 is flagged and halts recursion", {
  dir <- withr::local_tempdir()
  for (i in 1:12) {
    writeLines(sprintf("%%include 'f%d.sas';", i + 1), file.path(dir, sprintf("f%d.sas", i)))
  }
  writeLines("data final; run;", file.path(dir, "f13.sas"))
  p <- sas_project(file.path(dir, "f1.sas"))
  expect_true("include_depth_exceeded" %in% p$flags$kind)
})

test_that("include graph keeps every occurrence but scans one physical file", {
  root <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "%include 'inc.sas';"),
             file.path(root, "driver.sas"))

  p <- sas_project(file.path(root, "driver.sas"))
  occ <- p$include_graph$occurrences
  expect_equal(nrow(occ), 2L)
  expect_true(all(occ$status == "resolved"))
  expect_identical(length(unique(occ$occurrence_id)), 2L)
  expect_identical(length(unique(occ$target_file)), 1L)
  expect_identical(sum(p$files$origin == "included"), 1L)
})

test_that("source-relative include wins over a configured fallback", {
  root <- withr::local_tempdir()
  fallback <- withr::local_tempdir()
  writeLines("%macro local; %mend;", file.path(root, "shared.sas"))
  writeLines("%macro fallback; %mend;", file.path(fallback, "shared.sas"))
  writeLines("%include 'shared.sas';", file.path(root, "driver.sas"))
  cfg <- structure(list(include_roots = fallback), class = "sas2r_config")

  p <- sas_project(file.path(root, "driver.sas"), config = cfg)
  occ <- p$include_graph$occurrences[1, ]
  expect_identical(occ$resolution_origin, "including_dir")
  expect_identical(normalizePath(occ$target_file),
                   normalizePath(file.path(root, "shared.sas")))
})

test_that("dynamic include targets and cycles remain explicit", {
  root <- withr::local_tempdir()
  writeLines("%include 'b.sas';", file.path(root, "a.sas"))
  writeLines(c("%include 'a.sas';", "%include \"&runtime_file\";"),
             file.path(root, "b.sas"))
  p <- sas_project(file.path(root, "a.sas"))
  expect_true(all(c("cycle", "dynamic") %in% p$include_graph$occurrences$status))
})

test_that("occurrence rows carry the documented schema and reproducible identities", {
  root <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "%include 'inc.sas';"),
             file.path(root, "driver.sas"))
  driver <- file.path(root, "driver.sas")

  p <- sas_project(driver)
  occ <- p$include_graph$occurrences
  expect_identical(
    names(occ),
    c("occurrence_id", "parent_file", "parent_unit_id", "line",
      "target_expression", "target_file", "resolution_origin",
      "status", "reason", "depth")
  )
  expect_type(occ$occurrence_id, "character")
  expect_type(occ$parent_file, "character")
  expect_type(occ$parent_unit_id, "integer")
  expect_type(occ$line, "integer")
  expect_type(occ$target_expression, "character")
  expect_type(occ$target_file, "character")
  expect_type(occ$resolution_origin, "character")
  expect_type(occ$status, "character")
  expect_type(occ$reason, "character")
  expect_type(occ$depth, "integer")

  expect_identical(occ$line, c(1L, 2L))
  expect_identical(occ$depth, c(1L, 1L))
  expect_identical(occ$target_expression, c("inc.sas", "inc.sas"))
  expect_true(all(is.na(occ$reason)))
  expect_true(all(occ$parent_unit_id %in% p$units$unit_id))

  # occurrence identity is a pure function of the include site: the file-local
  # unit index (2L here -- driver.sas is scanned first, so no offset applies),
  # the line, the literal target, and the root-relative parent path
  anchors <- include_identity_anchors(root)
  expect_identical(
    occ$occurrence_id[2],
    include_occurrence_id(include_identity_parent(driver, anchors), 2L, 2L,
                          "inc.sas")
  )
  # and it is reproducible across scans of the same sources
  expect_identical(sas_project(driver)$include_graph$occurrences$occurrence_id,
                   occ$occurrence_id)
})

test_that("the file graph records one canonical entry per scanned physical file", {
  root <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "%include 'inc.sas';"),
             file.path(root, "driver.sas"))

  p <- sas_project(file.path(root, "driver.sas"))
  gfiles <- p$include_graph$files
  expect_identical(names(gfiles),
                   c("file", "canonical_file", "origin", "depth",
                     "parent_occurrence_id"))
  expect_identical(nrow(gfiles), 2L)
  expect_identical(sum(gfiles$origin == "included"), 1L)
  expect_identical(gfiles$depth, c(0L, 1L))
  expect_identical(length(unique(gfiles$canonical_file)), 2L)

  occ <- p$include_graph$occurrences
  # every resolved occurrence points at a scanned physical file
  expect_true(all(occ$target_file %in% gfiles$canonical_file))
  # the scan was queued by the first occurrence that reached the file
  expect_true(is.na(gfiles$parent_occurrence_id[1]))
  expect_identical(gfiles$parent_occurrence_id[2], occ$occurrence_id[1])
})

test_that("projects without includes expose an empty occurrence table", {
  root <- withr::local_tempdir()
  writeLines("data a; run;", file.path(root, "a.sas"))
  p <- sas_project(root)
  occ <- p$include_graph$occurrences
  expect_identical(nrow(occ), 0L)
  expect_identical(
    names(occ),
    c("occurrence_id", "parent_file", "parent_unit_id", "line",
      "target_expression", "target_file", "resolution_origin",
      "status", "reason", "depth")
  )
  expect_identical(nrow(p$include_graph$files), 1L)
  expect_identical(nrow(p$includes), 0L)
})

test_that("project$includes keeps compatibility columns and gains occurrence context", {
  root <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  writeLines(c("%include 'inc.sas';", "%include 'gone.sas';"),
             file.path(root, "driver.sas"))
  p <- sas_project(file.path(root, "driver.sas"))

  expect_true(all(c("target", "quoted", "line", "file") %in% names(p$includes)))
  expect_true(all(c("occurrence_id", "target_file", "resolution_origin",
                    "status") %in% names(p$includes)))
  expect_identical(p$includes$target, c("inc.sas", "gone.sas"))
  expect_identical(p$includes$quoted, c(TRUE, TRUE))
  expect_identical(p$includes$occurrence_id,
                   p$include_graph$occurrences$occurrence_id)
  expect_identical(p$includes$status, c("resolved", "unresolved"))
  expect_identical(p$includes$resolution_origin, c("including_dir", "none"))
  expect_true(is.na(p$includes$target_file[2]))
})

test_that("resolution origins cover absolute, project root, and configured fallback", {
  root <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  fallback <- withr::local_tempdir()
  sub <- file.path(root, "sub")
  dir.create(sub)

  writeLines("%macro away; %mend;", file.path(elsewhere, "away.sas"))
  writeLines("%macro atroot; %mend;", file.path(root, "atroot.sas"))
  writeLines("%macro back; %mend;", file.path(fallback, "back.sas"))
  writeLines(c(
    sprintf("%%include '%s';", file.path(elsewhere, "away.sas")),
    "%include 'atroot.sas';",
    "%include 'back.sas';"
  ), file.path(sub, "driver.sas"))
  cfg <- structure(list(include_roots = fallback), class = "sas2r_config")

  # scanned as a project so that the project root is above the including file
  p <- sas_project(root, config = cfg, recursive = TRUE)
  occ <- p$include_graph$occurrences
  expect_identical(occ$parent_file, rep(file.path(sub, "driver.sas"), 3L))
  expect_identical(occ$resolution_origin,
                   c("absolute", "project_root", "configured_fallback"))
  expect_true(all(occ$status == "resolved"))
  # atroot.sas was already scanned as a program file; the occurrence survives
  # even though the physical file is not scanned a second time
  expect_identical(sum(basename(p$files$file) == "atroot.sas"), 1L)
})

test_that("cycle, depth, and dynamic occurrences keep their own status and reason", {
  root <- withr::local_tempdir()
  writeLines("%include 'b.sas';", file.path(root, "a.sas"))
  writeLines(c("%include 'a.sas';", "%include \"&runtime_file\";"),
             file.path(root, "b.sas"))
  p <- sas_project(file.path(root, "a.sas"))
  occ <- p$include_graph$occurrences

  cyc <- occ[occ$status == "cycle", ]
  expect_identical(nrow(cyc), 1L)
  expect_identical(cyc$resolution_origin, "including_dir")
  expect_false(is.na(cyc$target_file))
  expect_identical(cyc$reason, "cycle_detected")

  dyn <- occ[occ$status == "dynamic", ]
  expect_identical(nrow(dyn), 1L)
  expect_identical(dyn$resolution_origin, "none")
  expect_true(is.na(dyn$target_file))
  expect_identical(dyn$reason, "dynamic_target")
  expect_identical(dyn$target_expression, "&runtime_file")

  # the legacy flag surface is preserved
  expect_true("include_cycle" %in% p$flags$kind)
})

test_that("depth-exceeded occurrences are recorded rather than dropped", {
  dir <- withr::local_tempdir()
  for (i in 1:12) {
    writeLines(sprintf("%%include 'f%d.sas';", i + 1),
               file.path(dir, sprintf("f%d.sas", i)))
  }
  writeLines("data final; run;", file.path(dir, "f13.sas"))
  p <- sas_project(file.path(dir, "f1.sas"))
  occ <- p$include_graph$occurrences

  deep <- occ[occ$status == "depth_exceeded", ]
  expect_identical(nrow(deep), 1L)
  expect_identical(deep$depth, INCLUDE_MAX_DEPTH + 1L)
  expect_identical(deep$reason, "max_depth_exceeded")
  expect_identical(basename(deep$parent_file), "f11.sas")
  expect_identical(deep$target_expression, "f12.sas")
  expect_identical(sum(occ$status == "resolved"), INCLUDE_MAX_DEPTH)
  expect_false(any(basename(p$files$file) == "f12.sas"))
  # across many scanned files the compatibility table stays row-aligned
  expect_identical(p$includes$occurrence_id, occ$occurrence_id)
  expect_identical(p$includes$status, occ$status)
})

test_that("an inaccessible absolute include target never falls back to a basename", {
  root <- withr::local_tempdir()
  fallback <- withr::local_tempdir()
  writeLines("%macro sneaky; %mend;", file.path(fallback, "setup.sas"))
  absent <- "/no/such/directory/sas2r/setup.sas"

  cands <- include_candidates(absent, file.path(root, "a.sas"), root,
                              include_roots = fallback)
  expect_identical(length(cands), 1L)
  expect_identical(cands[[1]]$origin, "absolute")
  expect_identical(cands[[1]]$path, absent)

  res <- resolve_include_target(absent, file.path(root, "a.sas"), root,
                                include_roots = fallback)
  expect_identical(res$status, "unresolved")
  expect_identical(res$origin, "none")
  expect_true(is.na(res$path))
  expect_identical(res$reason, "target_not_found")
})

test_that("configured roots are searched only after every source-relative anchor", {
  root <- withr::local_tempdir()
  fallback <- withr::local_tempdir()
  cands <- include_candidates("shared.sas", file.path(root, "sub", "a.sas"),
                              root, include_roots = fallback)
  expect_identical(vapply(cands, function(x) x$origin, character(1)),
                   c("including_dir", "project_root", "configured_fallback"))
  expect_identical(cands[[3]]$path, file.path(fallback, "shared.sas"))
  # the relative target is appended unchanged under configured roots
  cands2 <- include_candidates("macros/shared.sas", file.path(root, "a.sas"),
                               root, include_roots = fallback)
  expect_identical(cands2[[3]]$path, file.path(fallback, "macros/shared.sas"))
})

test_that("a directory target is never treated as an includable file", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "macros"))
  res <- resolve_include_target("macros", file.path(root, "a.sas"), root)
  expect_identical(res$status, "unresolved")
  expect_identical(res$origin, "none")

  writeLines("%include 'macros';", file.path(root, "a.sas"))
  p <- sas_project(file.path(root, "a.sas"))
  expect_identical(p$include_graph$occurrences$status, "unresolved")
  expect_true("unresolved_include" %in% p$flags$kind)
})

test_that("an unrelated earlier-sorting file leaves occurrence ids unchanged", {
  root <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  writeLines("%include 'inc.sas';", file.path(root, "driver.sas"))
  before <- sas_project(root)$include_graph$occurrences
  expect_identical(nrow(before), 1L)

  # sorts before driver.sas, so every later file's unit numbering shifts
  writeLines("data unrelated; run;", file.path(root, "aaa.sas"))
  after <- sas_project(root)$include_graph$occurrences

  expect_identical(after$occurrence_id, before$occurrence_id)
})

test_that("editing an earlier-sorting file's unit count leaves occurrence ids unchanged", {
  root <- withr::local_tempdir()
  writeLines("data a; run;", file.path(root, "aaa.sas"))
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  writeLines("%include 'inc.sas';", file.path(root, "driver.sas"))
  before <- sas_project(root)$include_graph$occurrences

  writeLines(c("data a; run;", "data a2; run;"), file.path(root, "aaa.sas"))
  after <- sas_project(root)$include_graph$occurrences

  expect_identical(after$occurrence_id, before$occurrence_id)
})

test_that("a driver scanned alone and in its directory yields the same occurrence ids", {
  root <- withr::local_tempdir()
  writeLines("data a; run;", file.path(root, "aaa.sas"))
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  driver <- file.path(root, "driver.sas")
  writeLines("%include 'inc.sas';", driver)

  alone <- sas_project(driver)$include_graph$occurrences
  in_dir <- sas_project(root)$include_graph$occurrences

  expect_identical(alone$occurrence_id, in_dir$occurrence_id)
  # the project-level parent_unit_id column is still project-level, and it does
  # move between the two scans -- it just no longer keys the identity
  expect_false(identical(alone$parent_unit_id, in_dir$parent_unit_id))
})

test_that("byte-identical sources at different absolute paths share occurrence ids", {
  scan_copy <- function(dir, lib) {
    writeLines("data inc; run;", file.path(dir, "inc.sas"))
    writeLines(c("%include 'inc.sas';", "%include 'inc.sas';",
                 "%include 'setup.sas';"),
               file.path(dir, "driver.sas"))
    # the chain leaves the project root here: setup.sas exists only under the
    # configured include root, and pulls in a sibling of its own from there
    writeLines("%include 'common.sas';", file.path(lib, "setup.sas"))
    writeLines("data common; run;", file.path(lib, "common.sas"))
    cfg <- structure(list(include_roots = lib), class = "sas2r_config")
    sas_project(file.path(dir, "driver.sas"),
                config = cfg)$include_graph$occurrences
  }
  dir_a <- withr::local_tempdir()
  dir_b <- withr::local_tempdir()
  lib_a <- withr::local_tempdir()
  lib_b <- withr::local_tempdir()

  occ_a <- scan_copy(dir_a, lib_a)
  occ_b <- scan_copy(dir_b, lib_b)

  expect_false(identical(normalizePath(dir_a), normalizePath(dir_b)))
  expect_false(identical(normalizePath(lib_a), normalizePath(lib_b)))
  expect_identical(occ_a$resolution_origin,
                   c("including_dir", "including_dir", "configured_fallback",
                     "including_dir"))
  expect_identical(occ_a$occurrence_id, occ_b$occurrence_id)
  # including the occurrence recorded inside the out-of-root library, whose
  # parent is named against the configured anchor rather than by where that
  # anchor happens to sit
  expect_identical(occ_a$occurrence_id[occ_a$target_expression == "common.sas"],
                   occ_b$occurrence_id[occ_b$target_expression == "common.sas"])
  # distinct include sites in the same file stay distinct
  expect_identical(length(unique(occ_a$occurrence_id)), 4L)
})

test_that("two out-of-root parents sharing a basename keep distinct occurrence ids", {
  root <- withr::local_tempdir()
  lib_a <- withr::local_tempdir()
  lib_b <- withr::local_tempdir()

  # two copies of the same macro library, each with its own relative include
  for (lib in c(lib_a, lib_b)) {
    writeLines("%include 'common.sas';", file.path(lib, "setup.sas"))
    writeLines("data common; run;", file.path(lib, "common.sas"))
  }
  writeLines(
    sprintf("%%include '%s';", c(file.path(lib_a, "setup.sas"),
                                 file.path(lib_b, "setup.sas"))),
    file.path(root, "driver.sas")
  )

  occ <- sas_project(file.path(root, "driver.sas"))$include_graph$occurrences

  # both setup.sas files are scanned, and each contributes its own occurrence
  expect_identical(nrow(occ), 4L)
  expect_identical(sum(occ$target_expression == "common.sas"), 2L)
  # the two setup.sas parents are structurally distinct include sites, so their
  # occurrences must not share an identity even though basename, file-local unit
  # id, line, and target expression all coincide
  expect_identical(length(unique(occ$occurrence_id)), 4L)
})

test_that("two identical includes sharing a line and a unit stay distinct", {
  root <- withr::local_tempdir()
  writeLines("data inc; run;", file.path(root, "inc.sas"))
  # inside one macro definition, so both statements land in the same translation
  # unit -- file-local unit id, line, and target expression all coincide
  writeLines(c("%macro m;",
               "%include 'inc.sas'; %include 'inc.sas';",
               "%mend;"),
             file.path(root, "driver.sas"))

  occ <- sas_project(file.path(root, "driver.sas"))$include_graph$occurrences

  expect_identical(nrow(occ), 2L)
  expect_identical(occ$line, c(2L, 2L))
  expect_identical(occ$parent_unit_id, c(1L, 1L))
  expect_identical(length(unique(occ$occurrence_id)), 2L)
})

test_that("editing above an include site leaves the included subtree's ids unchanged", {
  root <- withr::local_tempdir()
  lib <- withr::local_tempdir()
  writeLines("%include 'common.sas';", file.path(lib, "setup.sas"))
  writeLines("data common; run;", file.path(lib, "common.sas"))
  driver <- file.path(root, "driver.sas")
  site <- sprintf("%%include '%s';", file.path(lib, "setup.sas"))

  writeLines(site, driver)
  before <- sas_project(driver)$include_graph$occurrences
  writeLines(c("data prelude; run;", site), driver)
  after <- sas_project(driver)$include_graph$occurrences

  child <- function(occ) occ$occurrence_id[occ$target_expression == "common.sas"]
  parent <- function(occ) occ$occurrence_id[occ$target_expression != "common.sas"]

  # the driver's own site moved down a line and a unit, so its id moves with it
  expect_false(identical(parent(after), parent(before)))
  # but nothing about setup.sas:1 -> common.sas changed, so its id must not:
  # an out-of-root file is keyed on the include site that reached it, never on
  # that site's occurrence id, which would drag the line number along
  expect_identical(length(child(before)), 1L)
  expect_identical(child(after), child(before))
})

test_that("an out-of-root parent's identity does not depend on where its root lives", {
  build <- function(root, lib) {
    writeLines("%include 'common.sas';", file.path(lib, "setup.sas"))
    writeLines("data common; run;", file.path(lib, "common.sas"))
    writeLines("%include 'setup.sas';", file.path(root, "driver.sas"))
    cfg <- structure(list(include_roots = lib), class = "sas2r_config")
    sas_project(file.path(root, "driver.sas"),
                config = cfg)$include_graph$occurrences
  }
  root_a <- withr::local_tempdir()
  lib_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()
  lib_b <- withr::local_tempdir()

  occ_a <- build(root_a, lib_a)
  occ_b <- build(root_b, lib_b)

  expect_identical(occ_a$resolution_origin,
                   c("configured_fallback", "including_dir"))
  expect_false(identical(normalizePath(lib_a), normalizePath(lib_b)))
  # the out-of-root parent lib/setup.sas is identified by the include site that
  # pulled it in, which is itself source-derived, so nothing machine-specific
  # reaches the hash
  expect_identical(occ_a$occurrence_id, occ_b$occurrence_id)
})

test_that("the occurrence schema has one source of column order and vocabulary", {
  expect_identical(names(empty_include_occurrences()), INCLUDE_OCCURRENCE_COLUMNS)

  scrambled <- rev(INCLUDE_OCCURRENCE_PROTOTYPE)
  expect_false(identical(names(scrambled), INCLUDE_OCCURRENCE_COLUMNS))
  # columns are supplied by name; the declared order is imposed here
  expect_identical(names(include_occurrence_tibble(scrambled)),
                   INCLUDE_OCCURRENCE_COLUMNS)

  one_row <- INCLUDE_OCCURRENCE_PROTOTYPE
  one_row[] <- lapply(one_row, function(x) c(x, if (is.integer(x)) 1L else NA_character_))
  one_row$status <- "resolved"
  one_row$resolution_origin <- "including_dir"
  expect_identical(nrow(include_occurrence_tibble(one_row)), 1L)

  bad_status <- one_row
  bad_status$status <- "resolvd"
  expect_error(include_occurrence_tibble(bad_status),
               "INCLUDE_OCCURRENCE_STATUSES")

  bad_origin <- one_row
  bad_origin$resolution_origin <- "including-dir"
  expect_error(include_occurrence_tibble(bad_origin),
               "INCLUDE_RESOLUTION_ORIGINS")

  missing_col <- one_row[setdiff(names(one_row), "reason")]
  expect_error(include_occurrence_tibble(missing_col),
               "INCLUDE_OCCURRENCE_COLUMNS")
})

test_that("occurrence identity names a parent against a configured anchor", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "sub")
  dir.create(nested)
  writeLines("data a; run;", file.path(nested, "a.sas"))

  # in-root: a plain root-relative path, no sentinel
  expect_identical(
    include_identity_parent(file.path(nested, "a.sas"),
                            include_identity_anchors(root)),
    "sub/a.sas"
  )

  # a configured include root is an anchor, named by its configured position,
  # so its own absolute location never reaches the key
  lib_a <- withr::local_tempdir()
  lib_b <- withr::local_tempdir()
  for (lib in c(lib_a, lib_b)) writeLines("data b; run;", file.path(lib, "b.sas"))
  anchors <- include_identity_anchors(root, include_roots = c(lib_a, lib_b))
  expect_identical(include_identity_parent(file.path(lib_a, "b.sas"), anchors),
                   paste0(sprintf(INCLUDE_ANCHOR_INCLUDE_ROOT, 1L), "b.sas"))
  # two anchors holding a same-named file stay distinct: the key carries which
  # anchor, not just the basename
  expect_false(identical(
    include_identity_parent(file.path(lib_a, "b.sas"), anchors),
    include_identity_parent(file.path(lib_b, "b.sas"), anchors)
  ))

  # a configured autoexec directory is an anchor too, by configured position
  auto <- withr::local_tempdir()
  writeLines("data c; run;", file.path(auto, "autoexec.sas"))
  auto_anchors <- include_identity_anchors(
    root, autoexec = file.path(auto, "autoexec.sas"))
  expect_identical(
    include_identity_parent(file.path(auto, "autoexec.sas"), auto_anchors),
    paste0(sprintf(INCLUDE_ANCHOR_AUTOEXEC, 1L), "autoexec.sas")
  )

  # under no anchor at all, the canonical absolute path is kept whole: a file
  # gets here only because the source named an absolute location, which is
  # source content, and keeping it whole is what makes the key collision-free
  outside <- withr::local_tempdir()
  writeLines("data d; run;", file.path(outside, "d.sas"))
  expect_identical(
    include_identity_parent(file.path(outside, "d.sas"),
                            include_identity_anchors(root)),
    paste0(INCLUDE_EXTERNAL_PARENT_PREFIX,
           normalizePath(file.path(outside, "d.sas"), winslash = "/"))
  )

  # no anchored key carries the anchor's own absolute location
  for (key in list(include_identity_parent(file.path(lib_a, "b.sas"), anchors),
                   include_identity_parent(file.path(nested, "a.sas"),
                                           include_identity_anchors(root)))) {
    expect_false(grepl(normalizePath(lib_a, winslash = "/"), key, fixed = TRUE))
    expect_false(grepl(normalizePath(root, winslash = "/"), key, fixed = TRUE))
  }
})

test_that("a parent identity never depends on the include chain that reached it", {
  root <- withr::local_tempdir()
  lib <- withr::local_tempdir()
  writeLines("data b; run;", file.path(lib, "b.sas"))
  anchors <- include_identity_anchors(root, include_roots = lib)

  # the only inputs are the file and the anchor frame, so there is no argument
  # through which a reaching site could re-key the file
  expect_identical(names(formals(include_identity_parent)),
                   c("parent_file", "anchors"))
  # and the key it returns is exactly the file's position in the frame: an
  # anchor index and the path below it, with no room left for a chain, a site,
  # or an absolute location to appear in it. Spelled as a literal on purpose --
  # rebuilding it from INCLUDE_ANCHOR_INCLUDE_ROOT would agree with any prefix
  # the constant happened to hold and so discriminate nothing.
  expect_identical(include_identity_parent(file.path(lib, "b.sas"), anchors),
                   "<include-root:1>/b.sas")

  # a parent that names no file has no key, and none is invented: a shared
  # sentinel would hand two different unnamed parents one identity
  expect_error(include_identity_parent(NA_character_, anchors),
               class = "sas2r_include_identity_no_parent")
  expect_error(include_identity_parent("", anchors),
               class = "sas2r_include_identity_no_parent")
})

test_that("a site ordinal counts only within an otherwise-identical group", {
  # unit, line, target
  expect_identical(
    include_site_ordinal(c(1L, 1L, 1L), c(2L, 2L, 3L),
                         c("inc.sas", "inc.sas", "inc.sas")),
    c(1L, 2L, 1L)
  )
  # a different target, unit, or line starts its own count
  expect_identical(
    include_site_ordinal(c(1L, 1L, 2L), c(2L, 2L, 2L),
                         c("a.sas", "b.sas", "a.sas")),
    c(1L, 1L, 1L)
  )
  # it is not a statement index: interleaving an unrelated site leaves the
  # ordinals of the identical pair alone
  expect_identical(
    include_site_ordinal(c(1L, 1L, 1L), c(2L, 2L, 2L),
                         c("inc.sas", "other.sas", "inc.sas")),
    c(1L, 1L, 2L)
  )
  expect_identical(include_site_ordinal(integer(), integer(), character()),
                   integer())
})

test_that("a symlink escaping the project root is named against its target", {
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  writeLines("data b; run;", file.path(outside, "b.sas"))
  linked <- file.symlink(file.path(outside, "b.sas"), file.path(root, "b.sas"))
  skip_if_not(linked, "symbolic links are unavailable on this platform")

  # documented, deliberate: include_normalize_path() resolves the symlink before
  # any anchor comparison, so a vendored-by-symlink file is named against
  # whichever anchor holds its target, not against its in-tree path
  expect_identical(
    include_identity_parent(file.path(root, "b.sas"),
                            include_identity_anchors(root)),
    paste0(INCLUDE_EXTERNAL_PARENT_PREFIX,
           normalizePath(file.path(outside, "b.sas"), winslash = "/"))
  )
  # and if the target does lie under a configured anchor, the key is that
  # anchor's -- still machine-independent, still nothing to do with the symlink
  expect_identical(
    include_identity_parent(file.path(root, "b.sas"),
                            include_identity_anchors(root, include_roots = outside)),
    paste0(sprintf(INCLUDE_ANCHOR_INCLUDE_ROOT, 1L), "b.sas")
  )
  # a real file at the same in-tree path is root-relative instead
  writeLines("data c; run;", file.path(root, "c.sas"))
  expect_identical(include_identity_parent(file.path(root, "c.sas"),
                                           include_identity_anchors(root)),
                   "c.sas")
})

test_that("occurrence identity refuses a non-scalar path argument", {
  root <- withr::local_tempdir()
  anchors <- include_identity_anchors(root)
  expect_error(
    include_identity_parent(c(file.path(root, "a.sas"), file.path(root, "b.sas")),
                            anchors),
    class = "sas2r_include_identity_not_scalar"
  )
  expect_error(include_identity_parent(character(), anchors),
               class = "sas2r_include_identity_not_scalar")
  # zero length is the other way an id could quietly go missing
  expect_error(include_occurrence_id(character(), 1L, 1L, "inc.sas"),
               class = "sas2r_include_identity_not_scalar")
  expect_error(include_occurrence_id("driver.sas", 1L, 1L, character()),
               class = "sas2r_include_identity_not_scalar")
  expect_error(include_occurrence_id("driver.sas", c(1L, 2L), 1L, "inc.sas"),
               class = "sas2r_include_identity_not_scalar")
  # the frame's own root is held to the same contract: a longer vector would
  # frame the project on a directory nobody named, and no root at all would name
  # every in-root file absolutely
  expect_error(include_identity_anchors(c(root, root)),
               class = "sas2r_include_identity_not_scalar")
  expect_error(include_identity_anchors(character()),
               class = "sas2r_include_identity_not_scalar")
  expect_error(include_identity_anchors(NA_character_),
               class = "sas2r_include_identity_no_root")
  expect_error(include_identity_anchors(""),
               class = "sas2r_include_identity_no_root")
})

test_that("an anchor directory that would prefix-match every path is refused", {
  root <- withr::local_tempdir()
  # add() receives the post-as_dir() value, and as_dir() always appends a
  # trailing "/" when its normalized input lacks one, so the degenerate case
  # that actually reaches add() is never the bare "" the old comment named --
  # it is "/", which passes nzchar() just as well and, because every scanned
  # file's canonical path is itself absolute, prefix-matches every one of them.
  # A configured include root of "/" is the direct way to construct it.
  expect_error(include_identity_anchors(root, include_roots = "/"))
  # a project root of "/" reaches add() the same way, with no include_roots
  # involved at all
  expect_error(include_identity_anchors("/"))
  # an autoexec file directly at the filesystem root is the third route in:
  # dirname() of it is "/"
  expect_error(include_identity_anchors(root, autoexec = "/autoexec.sas"))

  # confirms what "prefix-matches every path" means concretely: left
  # unrefused, this anchor would silently fold every absolute parent file into
  # the configured-include-root anchor, discarding the machine-independence
  # the whole frame exists to provide
  bad_anchors <- list(list(dir = "/", prefix = "<include-root:1>/"))
  expect_identical(include_identity_parent("/x/y.sas", bad_anchors),
                   "<include-root:1>/x/y.sas")
})

test_that("a no-anchor key joins the sentinel to the path with one separator", {
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  writeLines("data d; run;", file.path(outside, "d.sas"))
  key <- include_identity_parent(file.path(outside, "d.sas"),
                                 include_identity_anchors(root))

  # the sentinel is a plain prefix on an already-absolute path, so the key a
  # human reads in the collision message has no doubled separator, and the
  # path-to-key map stays injective
  expect_false(grepl("//", key, fixed = TRUE))
  expect_identical(
    key,
    paste0("<outside-root>",
           normalizePath(file.path(outside, "d.sas"), winslash = "/"))
  )
})

test_that("binding occurrence blocks refuses a duplicated occurrence id", {
  block <- function(id, file, line) {
    cols <- INCLUDE_OCCURRENCE_PROTOTYPE
    cols[] <- lapply(cols, function(x) c(x, if (is.integer(x)) 1L else NA_character_))
    cols$occurrence_id <- id
    cols$parent_file <- file
    cols$line <- line
    cols$status <- "resolved"
    cols$resolution_origin <- "including_dir"
    include_occurrence_tibble(cols)
  }
  a <- block("include_aaaa", "libA/setup.sas", 1L)
  b <- block("include_aaaa", "libB/setup.sas", 1L)

  # per-file assembly cannot see across files, so the guard lives at the bind
  expect_identical(nrow(include_bind_occurrences(list(a))), 1L)
  expect_error(include_bind_occurrences(list(a, b)),
               class = "sas2r_include_occurrence_id_collision")
  expect_error(include_bind_occurrences(list(a, b)), "libB/setup.sas:1")
  expect_identical(nrow(include_bind_occurrences(list())), 0L)
})

test_that("an out-of-root library keeps one identity however the scan reaches it", {
  root <- withr::local_tempdir()
  lib <- withr::local_tempdir()
  writeLines("%include 'util.sas';", file.path(lib, "shared.sas"))
  writeLines("data util; run;", file.path(lib, "util.sas"))
  site <- sprintf("%%include '%s';", file.path(lib, "shared.sas"))
  writeLines(site, file.path(root, "aaa.sas"))
  writeLines(site, file.path(root, "driver.sas"))

  inner <- function(p) {
    occ <- p$include_graph$occurrences
    ids <- occ$occurrence_id[occ$target_expression == "util.sas"]
    expect_identical(length(ids), 1L)
    ids
  }
  from_aaa <- inner(sas_project(file.path(root, "aaa.sas")))
  from_driver <- inner(sas_project(file.path(root, "driver.sas")))
  in_dir <- inner(sas_project(root))

  # P4: a different include site reaches the library in each scan, and the
  # occurrence inside the library belongs to none of them
  expect_identical(from_driver, from_aaa)
  # P3: scanning the directory reaches it through whichever file sorts first
  expect_identical(in_dir, from_aaa)
})

test_that("renaming an unrelated sibling leaves an out-of-root file's ids unchanged", {
  root <- withr::local_tempdir()
  lib <- withr::local_tempdir()
  writeLines("%include 'util.sas';", file.path(lib, "shared.sas"))
  writeLines("data util; run;", file.path(lib, "util.sas"))
  site <- sprintf("%%include '%s';", file.path(lib, "shared.sas"))
  writeLines(site, file.path(root, "aaa.sas"))
  writeLines(site, file.path(root, "driver.sas"))

  inner <- function(occ) occ$occurrence_id[occ$target_expression == "util.sas"]
  before <- inner(sas_project(root)$include_graph$occurrences)
  expect_identical(length(before), 1L)

  # P2: aaa.sas sorts first and so reaches the library first; renaming it to
  # sort last changes nothing about shared.sas:1 -> util.sas
  file.rename(file.path(root, "aaa.sas"), file.path(root, "zzz.sas"))
  expect_identical(inner(sas_project(root)$include_graph$occurrences), before)

  # and an unrelated earlier-sorting file added to the project is equally inert
  writeLines("data unrelated; run;", file.path(root, "aab.sas"))
  expect_identical(inner(sas_project(root)$include_graph$occurrences), before)
})

test_that("a target expression longer than an R name limit still gets an ordinal", {
  long <- strrep("a", 12000L)
  expect_identical(
    include_site_ordinal(c(1L, 1L, 1L), c(2L, 2L, 2L), c(long, "inc.sas", long)),
    c(1L, 1L, 2L)
  )
})

test_that("two absolute autoexec entries sharing a basename keep distinct ids", {
  root <- withr::local_tempdir()
  env_a <- withr::local_tempdir()
  env_b <- withr::local_tempdir()
  for (env in c(env_a, env_b)) {
    writeLines("%include 'shared.sas';", file.path(env, "autoexec.sas"))
    writeLines("data shared; run;", file.path(env, "shared.sas"))
  }
  writeLines("data a; run;", file.path(root, "driver.sas"))

  occ <- sas_project(root, config = list(
    autoexec = c(file.path(env_a, "autoexec.sas"),
                 file.path(env_b, "autoexec.sas"))
  ))$include_graph$occurrences

  # each configured autoexec contributes an anchor of its own, named by its
  # configured position, so two queue roots sharing a basename are structurally
  # distinct parents and never collapse onto one occurrence id
  expect_identical(nrow(occ), 2L)
  expect_identical(occ$target_expression, c("shared.sas", "shared.sas"))
  expect_identical(length(unique(occ$occurrence_id)), 2L)
})

test_that("an autoexec listed twice is refused in configuration language", {
  root <- withr::local_tempdir()
  env <- withr::local_tempdir()
  writeLines("data env; run;", file.path(env, "autoexec.sas"))
  writeLines("data a; run;", file.path(root, "driver.sas"))

  # one physical file queued twice would mint one occurrence id twice; that is a
  # configuration mistake and is reported as one, not as an internal invariant
  expect_error(
    sas_project(root, config = list(
      autoexec = rep(file.path(env, "autoexec.sas"), 2L))),
    class = "sas2r_autoexec_duplicate"
  )
})

test_that("a ../ walk out of every anchor is named absolutely, as documented", {
  build <- function(base, roots = character()) {
    dir.create(file.path(base, "proj"))
    dir.create(file.path(base, "shared"))
    writeLines("%include 'util.sas';", file.path(base, "shared", "setup.sas"))
    writeLines("data util; run;", file.path(base, "shared", "util.sas"))
    writeLines("%include '../shared/setup.sas';",
               file.path(base, "proj", "driver.sas"))
    cfg <- structure(list(include_roots = roots), class = "sas2r_config")
    occ <- sas_project(file.path(base, "proj", "driver.sas"),
                       config = cfg)$include_graph$occurrences
    occ$occurrence_id[occ$target_expression == "util.sas"]
  }
  base_a <- withr::local_tempdir()
  base_b <- withr::local_tempdir()

  # the stated residual: shared/ lies under no anchor, so setup.sas is named by
  # its absolute path and relocating the whole tree re-mints what is inside it
  expect_false(identical(build(base_a), build(base_b)))

  # and the documented fix -- naming the directory in includes.roots makes it an
  # anchor, which restores relocatable ids
  base_c <- withr::local_tempdir()
  base_d <- withr::local_tempdir()
  expect_identical(build(base_c, file.path(base_c, "shared")),
                   build(base_d, file.path(base_d, "shared")))
})

test_that("a relative includes.roots entry keeps ids free of the working directory", {
  build <- function(base) {
    dir.create(file.path(base, "proj"))
    dir.create(file.path(base, "shared"))
    writeLines("%include 'util.sas';", file.path(base, "shared", "setup.sas"))
    writeLines("data util; run;", file.path(base, "shared", "util.sas"))
    writeLines("%include '../shared/setup.sas';",
               file.path(base, "proj", "driver.sas"))
    # the portable spelling of the workaround include_identity_anchors()
    # prescribes for the ../ residual: relative to the configuration file that
    # sits beside the driver it configures
    writeLines(c("includes:", "  roots:", "    - ../shared"),
               file.path(base, "proj", "_sas2r.yml"))
    file.path(base, "proj", "driver.sas")
  }
  scan_from <- function(driver, cwd) {
    withr::with_dir(cwd, {
      occ <- sas_project(driver)$include_graph$occurrences
      list(ids = occ$occurrence_id[occ$target_expression == "util.sas"],
           origins = occ$resolution_origin)
    })
  }
  base_a <- withr::local_tempdir()
  base_b <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  driver_a <- build(base_a)
  driver_b <- build(base_b)

  here <- scan_from(driver_a, file.path(base_a, "proj"))
  away <- scan_from(driver_a, elsewhere)
  other <- scan_from(driver_b, elsewhere)

  expect_identical(length(here$ids), 1L)
  # resolution never consulted the working directory, so identity must not
  # either: a relative root is resolved against configuration, never getwd()
  expect_identical(away$origins, here$origins)
  expect_identical(away$ids, here$ids)
  # P1: byte-identical sources at a different absolute base mint the same ids
  expect_identical(other$ids, here$ids)
})

test_that("a caller-supplied relative include_roots resolves against the project root, not getwd()", {
  # the previous test exercises only the YAML route: sas_config() has already
  # resolved includes.roots against the configuration file's directory before
  # sas_project() ever sees it. A caller-supplied config never goes through
  # sas_config()'s resolution at all -- R/project.R alone is responsible for
  # anchoring it, the same rule autoexec already follows.
  build <- function(base) {
    dir.create(file.path(base, "proj"))
    dir.create(file.path(base, "shared"))
    writeLines("%include 'util.sas';", file.path(base, "shared", "setup.sas"))
    writeLines("data util; run;", file.path(base, "shared", "util.sas"))
    writeLines("%include '../shared/setup.sas';",
               file.path(base, "proj", "driver.sas"))
    file.path(base, "proj", "driver.sas")
  }
  scan_from <- function(driver, cwd, cfg) {
    withr::with_dir(cwd, {
      occ <- sas_project(driver, config = cfg)$include_graph$occurrences
      occ$occurrence_id[occ$target_expression == "util.sas"]
    })
  }

  base <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  driver <- build(base)

  # a plain list is merged onto sas_config()'s defaults inside sas_project()
  list_cfg <- list(include_roots = "../shared")
  here_list <- scan_from(driver, file.path(base, "proj"), list_cfg)
  away_list <- scan_from(driver, elsewhere, list_cfg)
  expect_identical(length(here_list), 1L)
  expect_identical(away_list, here_list)

  # a hand-built sas2r_config object is used as-is, bypassing sas_config()
  # entirely -- this is the shape the task brief's own Step-1 fixture uses
  s3_cfg <- structure(list(include_roots = "../shared"), class = "sas2r_config")
  here_s3 <- scan_from(driver, file.path(base, "proj"), s3_cfg)
  away_s3 <- scan_from(driver, elsewhere, s3_cfg)
  expect_identical(length(here_s3), 1L)
  expect_identical(away_s3, here_s3)

  # both caller-supplied shapes agree with each other too
  expect_identical(here_list, here_s3)
})

test_that("a config file directory that differs from the project root still anchors relative roots", {
  # every other end-to-end includes.roots test in this file puts _sas2r.yml in
  # the project root, where the configuration directory and the project root
  # are one and the same directory -- so R/project.R:184's own re-resolution
  # against `root` silently reproduces the right answer even if R/config.R's
  # config-directory resolution were missing entirely. Here they are two
  # different directories, so only the config-directory resolution can be
  # right.
  build <- function(base) {
    proj <- file.path(base, "proj"); dir.create(proj)
    programs <- file.path(proj, "programs"); dir.create(programs)
    shared <- file.path(base, "shared"); dir.create(shared)
    writeLines("%include 'util.sas';", file.path(shared, "setup.sas"))
    writeLines("data util; run;", file.path(shared, "util.sas"))
    # two levels up from the driver's own directory (programs/ -> proj/ ->
    # base/), then into shared/
    writeLines("%include '../../shared/setup.sas';",
               file.path(programs, "driver.sas"))
    # _sas2r.yml sits in proj/, one level above programs/; the project root
    # sas_project() computes for a single-file scan is programs/ itself
    writeLines(c("includes:", "  roots:", "    - ../shared"),
               file.path(proj, "_sas2r.yml"))
    file.path(programs, "driver.sas")
  }
  scan_from <- function(driver, cwd) {
    withr::with_dir(cwd, {
      occ <- sas_project(driver)$include_graph$occurrences
      occ$occurrence_id[occ$target_expression == "util.sas"]
    })
  }

  base_a <- withr::local_tempdir()
  base_b <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  driver_a <- build(base_a)
  driver_b <- build(base_b)

  here <- scan_from(driver_a, dirname(driver_a))
  away <- scan_from(driver_a, elsewhere)
  other_base <- scan_from(driver_b, elsewhere)

  expect_identical(length(here), 1L)
  # two working directories agree ...
  expect_identical(away, here)
  # ... and so do two different absolute bases holding byte-identical sources
  expect_identical(other_base, here)
})
