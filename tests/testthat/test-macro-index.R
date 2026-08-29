test_that("bundle files are indexed: many macros, one file", {
  dir <- withr::local_tempdir()
  writeLines(c("%macro one; %mend;", "%macro two(x=); %mend;"),
             file.path(dir, "utilities.sas"))
  idx <- build_macro_index(dir)
  expect_setequal(idx$name, c("one", "two"))
  expect_identical(unique(basename(idx$file)), "utilities.sas")
})

test_that("resolution falls back to the content index", {
  dir <- withr::local_tempdir()
  mac <- file.path(dir, "macros"); dir.create(mac)
  writeLines("%macro hidden; %mend;", file.path(mac, "utilities.sas"))
  cfg <- structure(list(libraries = list(), macro_search_path = mac,
                        include_roots = character(), autoexec = character(),
                        source = NA_character_, raw = list()),
                   class = "sas2r_config")
  res <- resolve_macro_calls(tibble::tibble(name = "hidden", line = 1L),
                             tibble::tibble(name = character(), params = character(),
                                            line_start = integer(), line_end = integer()),
                             cfg, project_dir = dir)
  expect_identical(res$status, "resolved_content")
  expect_match(res$source, "utilities\\.sas")
})

test_that("the index cache is hit on unchanged files", {
  dir <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  writeLines("%macro a; %mend;", file.path(dir, "u.sas"))
  build_macro_index(dir, cache_dir = cache)
  expect_true(file.exists(file.path(cache, "macro_index.rds")))
  idx2 <- build_macro_index(dir, cache_dir = cache)   # second call: cache path
  expect_identical(idx2$name, "a")
})

test_that("empty or non-existent directories return empty tibble with correct schema", {
  idx <- build_macro_index(character())
  expect_identical(nrow(idx), 0L)
  expect_identical(names(idx), c("name", "file", "line_start", "line_end"))

  idx_nonexist <- build_macro_index("non_existent_dir_xyz")
  expect_identical(nrow(idx_nonexist), 0L)
  expect_identical(names(idx_nonexist), c("name", "file", "line_start", "line_end"))
})

test_that("resolution hierarchy: project defs > filename convention > content index", {
  dir <- withr::local_tempdir()
  mac <- file.path(dir, "macros"); dir.create(mac)
  # File named `my_macro.sas` defines `my_macro`
  writeLines("%macro my_macro; %mend;", file.path(mac, "my_macro.sas"))
  # File named `bundle.sas` also defines `my_macro`
  writeLines("%macro my_macro; %mend;", file.path(mac, "bundle.sas"))

  cfg <- structure(list(libraries = list(), macro_search_path = mac,
                        source = NA_character_, raw = list()),
                   class = "sas2r_config")

  # 1. Project defs win over filename convention and content index
  res_proj <- resolve_macro_calls(tibble::tibble(name = "my_macro", line = 1L),
                                  tibble::tibble(name = "my_macro", params = "",
                                                 line_start = 1L, line_end = 2L),
                                  cfg, project_dir = dir)
  expect_identical(res_proj$status, "resolved_project")

  # 2. Filename convention wins over content index
  res_file <- resolve_macro_calls(tibble::tibble(name = "my_macro", line = 1L),
                                  tibble::tibble(name = character(), params = character(),
                                                 line_start = integer(), line_end = integer()),
                                  cfg, project_dir = dir)
  expect_identical(res_file$status, "resolved_path")
  expect_match(res_file$source, "my_macro\\.sas")
})

test_that("content index resolution records shadowing across multiple files", {
  dir <- withr::local_tempdir()
  mac1 <- file.path(dir, "mac1"); dir.create(mac1)
  mac2 <- file.path(dir, "mac2"); dir.create(mac2)
  writeLines("%macro util; %mend;", file.path(mac1, "bundle1.sas"))
  writeLines("%macro util; %mend;", file.path(mac2, "bundle2.sas"))

  cfg <- structure(list(libraries = list(), macro_search_path = c(mac1, mac2),
                        source = NA_character_, raw = list()),
                   class = "sas2r_config")
  res <- resolve_macro_calls(tibble::tibble(name = "util", line = 1L),
                             tibble::tibble(name = character(), params = character(),
                                            line_start = integer(), line_end = integer()),
                             cfg, project_dir = dir)
  expect_identical(res$status, "resolved_content")
  expect_match(res$source, "bundle1\\.sas")
  expect_identical(res$n_matches, 2L)
  expect_match(res$shadowed, "bundle2\\.sas")
})

test_that("macro index recovers cleanly from corrupted cache file", {
  dir <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  writeLines("%macro healthy; %mend;", file.path(dir, "h.sas"))
  # Write invalid/corrupt data to cache file
  writeLines("CORRUPTED RDS DATA", file.path(cache, "macro_index.rds"))
  idx <- build_macro_index(dir, cache_dir = cache)
  expect_identical(idx$name, "healthy")
})
