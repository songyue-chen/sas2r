demo <- function() system.file("examples", "demo_project", package = "sas2r")

test_that("project scans all files and renumbers units globally", {
  p <- sas_project(demo())
  expect_s3_class(p, "sas2r_project")
  expect_identical(nrow(p$files), 3L)  # macros/ dir is search path, not program dir
  expect_identical(anyDuplicated(p$units$unit_id), 0L)
})

test_that("lineage crosses files and orders units topologically", {
  p <- sas_project(demo())
  creates_srt <- p$lineage$unit_id[p$lineage$dataset == "work.adsl_srt" &
                                   p$lineage$role == "creates"]
  reads_srt <- p$lineage$unit_id[p$lineage$dataset == "work.adsl_srt" &
                                 p$lineage$role == "reads"]
  expect_true(all(match(creates_srt, p$order) < match(reads_srt, p$order)))
})

test_that("macro resolution and flags: derive_flag resolved, ghost_macro flagged", {
  p <- sas_project(demo())
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "derive_flag"], "resolved_path")
  expect_identical(res$status[res$name == "ghost_macro"], "unresolved")
  expect_true("unresolved_macro" %in% p$flags$kind)
})

test_that("single .sas file with no config works (zero-config path)", {
  f <- withr::local_tempfile(fileext = ".sas")
  writeLines("data a; set b; run;", f)
  p <- sas_project(f)
  expect_identical(nrow(p$files), 1L)
  expect_identical(p$units$unit_type, "data_step")
  expect_identical(p$librefs, tibble::tibble(libref = character(),
    action = character(), engine = character(), path_expression = character(),
    path = character(), line = integer(), unit_id = integer(),
    unit_type = character(), file = character()))
})

test_that("cycle falls back to file order with a flag", {
  dir <- withr::local_tempdir()
  writeLines("data a; set b; run;\ndata b; set a; run;", file.path(dir, "x.sas"))
  p <- sas_project(dir)
  expect_true("dependency_cycle" %in% p$flags$kind)
  expect_identical(p$order, p$units$unit_id)
})

test_that("empty directory returns valid 0-row project tibbles", {
  dir <- withr::local_tempdir()
  p <- sas_project(dir)
  expect_s3_class(p, "sas2r_project")
  expect_identical(nrow(p$files), 0L)
  expect_identical(nrow(p$statements), 0L)
  expect_identical(nrow(p$units), 0L)
  expect_identical(nrow(p$comments), 0L)
  expect_identical(names(p$statements), c("stmt_id", "text", "first_token", "type",
                                          "line_start", "line_end", "unit_id",
                                          "unit_type", "file", "origin"))
  expect_identical(names(p$units), c("file", "unit_id", "unit_type", "label",
                                     "line_start", "line_end", "n_stmts", "origin"))
  expect_identical(names(p$comments), c(
    "comment_id", "text", "kind", "line_start", "line_end", "char_start",
    "char_end", "unit_id", "placement", "file", "origin"
  ))
})

test_that("project comments attach to the next or containing translation unit", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "comments.sas")
  writeLines(c(
    "/* above first unit */",
    "data a;",
    "  x = 1;",
    "run;",
    "* before second unit;",
    "data b;",
    "  %* inside second unit;",
    "  y = 1 /* inline second unit */;",
    "run;",
    "%* after all units;"
  ), path)

  p <- sas_project(dir)

  expect_identical(p$comments$text, c(
    "/* above first unit */", "* before second unit;",
    "%* inside second unit;", "/* inline second unit */",
    "%* after all units;"
  ))
  expect_identical(p$comments$unit_id, c(1L, 2L, 2L, 2L, NA_integer_))
  expect_identical(p$comments$placement, c(
    "leading", "leading", "internal", "internal", "unattached"
  ))
  expect_identical(p$comments$file, rep(path, 5L))
  expect_identical(p$comments$origin, rep("program", 5L))
  expect_false(any(c("char_start", "char_end") %in% names(p$statements)))
})

test_that("comment-only files retain all comment forms without a unit", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "notes.sas")
  writeLines(c(
    "/* block only */",
    "* statement only;",
    "%* macro only;"
  ), path)

  p <- sas_project(dir)

  expect_identical(p$comments$text,
                   c("/* block only */", "* statement only;", "%* macro only;"))
  expect_identical(p$comments$kind, c("block", "statement", "macro"))
  expect_identical(p$comments$unit_id, rep(NA_integer_, 3L))
  expect_identical(p$comments$placement, rep("unattached", 3L))
  expect_identical(p$comments$file, rep(path, 3L))
  expect_identical(p$comments$origin, rep("program", 3L))
})

test_that("cached project scans preserve globally offset comment attachments", {
  dir <- withr::local_tempdir()
  writeLines("data a; run;", file.path(dir, "a.sas"))
  path <- file.path(dir, "b.sas")
  writeLines(c("/* b unit note */", "data b; run;"), path)

  p1 <- sas_project(dir, cache = TRUE)
  p2 <- sas_project(dir, cache = TRUE)

  expect_identical(p1$comments$unit_id, 2L)
  expect_identical(p1$comments$file, path)
  expect_identical(p1$comments, p2$comments)
})

test_that("recreated dataset in sequential code does not trigger false cycle", {
  src <- "data a; set src; run;\ndata b; set a; run;\ndata a; set b; run;"
  u <- sas_units(sas_statements(src))
  lin <- extract_dataset_refs(u)
  ord <- unit_order(lin, unique(u$unit_id))
  expect_identical(ord, c(1L, 2L, 3L))
})

test_that("sas_project with empty or non-existent path throws clear error", {
  expect_error(sas_project(""), "Path does not exist or is invalid")
  expect_error(sas_project("non_existent_dir_12345"), "Path does not exist or is invalid")
})

test_that("missing quoted include is flagged as unresolved_include", {
  dir <- withr::local_tempdir()
  writeLines("%include 'missing_setup.sas';", file.path(dir, "driver.sas"))
  p <- sas_project(dir)
  expect_true(any(grepl("missing_setup.sas", p$flags$detail[p$flags$kind == "unresolved_include"])))
})

test_that("existing quoted include outside top-level scanned files is resolved and scanned", {
  dir <- withr::local_tempdir()
  sub_dir <- file.path(dir, "macros"); dir.create(sub_dir)
  writeLines("%macro prep; %mend;", file.path(sub_dir, "prep.sas"))
  writeLines("%include 'macros/prep.sas'; %prep() data a; run;", file.path(dir, "driver.sas"))
  p <- sas_project(dir)
  expect_false("include_not_scanned" %in% p$flags$kind)
  expect_identical(sort(p$files$origin), c("included", "program"))
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "prep"], "resolved_project")
})

test_that("quoted include matching top-level file is not duplicated or cycled", {
  dir <- withr::local_tempdir()
  writeLines("data a; run;", file.path(dir, "setup.sas"))
  writeLines("%include 'SETUP.sas'; data b; run;", file.path(dir, "driver.sas"))
  p <- sas_project(dir)
  expect_false("include_cycle" %in% p$flags$kind)
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("config list overlays over discovered _sas2r.yml", {
  dir <- withr::local_tempdir()
  writeLines("data a; run;", file.path(dir, "a.sas"))
  writeLines("includes:\n  roots:\n    - /path/from/yaml", file.path(dir, "_sas2r.yml"))
  p <- sas_project(dir, config = list(provider = "custom_provider"))
  expect_identical(p$config$include_roots, "/path/from/yaml")
  expect_identical(p$config$provider, "custom_provider")
})

test_that("missing autoexec path generates autoexec_missing flag", {
  dir <- withr::local_tempdir()
  writeLines("data a; run;", file.path(dir, "a.sas"))
  p <- sas_project(dir, config = list(autoexec = "nonexistent_autoexec.sas"))
  expect_true("autoexec_missing" %in% p$flags$kind)
})

test_that("sasautos in program files is harvested with sasautos_from_program flag", {
  dir <- withr::local_tempdir()
  writeLines("options sasautos=('macros');\ndata a; run;", file.path(dir, "prog.sas"))
  p <- sas_project(dir)
  expect_true("sasautos_from_program" %in% p$flags$kind)
  expect_true("macros" %in% p$config$macro_search_path)
})

test_that("sas_project attaches dependency_facts and source_hashes", {
  dir <- withr::local_tempdir()
  writeLines("data a; set b; run;", file.path(dir, "prog.sas"))
  p <- sas_project(dir)
  expect_true(!is.null(p$dependency_facts))
  expect_true(!is.null(p$source_hashes))
  expect_type(p$dependency_facts, "list")
  expect_type(p$source_hashes, "character")
  expect_true(length(p$source_hashes) == 1L)
  expect_true(nzchar(p$source_hashes[1]))
})




test_that("a quoted include resolves through a unique case-insensitive sibling", {
  # SAS on Windows resolves include paths case-insensitively, so real programs
  # say %include 'SETUP.sas' against a file named setup.sas. On a
  # case-sensitive filesystem (Linux CI) the literal spelling misses; the
  # resolver must fall back to the unique case-insensitive sibling.
  dir <- withr::local_tempdir()
  writeLines("data a; run;", file.path(dir, "setup.sas"))

  # Platform-independent: a target that exists under no casing stays unresolved.
  res_missing <- resolve_include_target(
    "nothere.sas", including_file = file.path(dir, "driver.sas"),
    project_root = dir)
  expect_identical(res_missing$status, "unresolved")

  case_insensitive_fs <- file.exists(file.path(dir, "SETUP.SAS"))
  skip_if(case_insensitive_fs,
          "filesystem resolves case-insensitively already; Linux CI exercises the fallback")

  res <- resolve_include_target(
    "SETUP.sas", including_file = file.path(dir, "driver.sas"),
    project_root = dir)
  expect_identical(res$status, "resolved")
  expect_identical(basename(res$path), "setup.sas")
})
