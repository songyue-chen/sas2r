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
  expect_identical(occ$parent_file, rep(include_normalize_path(file.path(sub, "driver.sas")), 3L))
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
