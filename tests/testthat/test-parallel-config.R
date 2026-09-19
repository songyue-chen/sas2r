test_that("conflicting YAML settings stop preflight and translation before work", {
  root <- withr::local_tempdir()
  writeLines("data out; x=1; run;", file.path(root, "p.sas"))
  config <- file.path(root, "_sas2r.yml")
  yaml::write_yaml(list(migration = list(max_parallel_translations = 4),
    llm = list(provider = "deepseek", model = "offline", max_tries = 2)), config)
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  testthat::local_mocked_bindings(
    scan_project = function(...) stop("source scanning must not start"),
    sas_llm = function(...) stop("provider setup must not start"),
    sas_llm_probe = function(...) stop("provider calls must not start"))

  # Loading config alone remains valid: a function argument can override it.
  expect_identical(sas_config(config)$migration$max_parallel_translations, 4L)
  for (entry in list(sas_preflight, sas_translate)) {
    error <- tryCatch(entry(root, config = config, out_dir = file.path(root, "out")),
                      error = identity)
    expect_s3_class(error, "sas2r_parallel_config_error")
    message <- conditionMessage(error)
    for (text in c("migration.max_parallel_translations: 4", "llm.max_tries: 2",
      "set llm.max_tries to 1", "set migration.max_parallel_translations to 1",
      "Update _sas2r.yml", "No translation was started. No settings were changed.")) {
      expect_match(message, text, fixed = TRUE)
    }
  }
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE), before)
})

test_that("preflight accepts both valid alternatives and honors concurrency overrides", {
  root <- withr::local_tempdir()
  writeLines("data out; x=1; run;", file.path(root, "p.sas"))
  cfg <- list(migration = list(max_parallel_translations = 4L),
    llm = list(provider = "deepseek", model = "offline", max_tries = 2L))
  expect_identical(sas_preflight(root, config = cfg,
    max_parallel_translations = 1L)$max_parallel_translations, 1L)
  cfg$migration$max_parallel_translations <- 1L
  expect_identical(sas_preflight(root, config = cfg)$max_parallel_translations, 1L)
  expect_error(sas_preflight(root, config = cfg, max_parallel_translations = 4L),
    class = "sas2r_parallel_config_error")
  cfg$llm$max_tries <- 1L
  expect_identical(sas_preflight(root, config = cfg,
    max_parallel_translations = 4L)$max_parallel_translations, 4L)
  cfg$llm$max_tries <- NULL
  expect_identical(sas_preflight(root, config = cfg,
    max_parallel_translations = 4L)$max_parallel_translations, 4L)
})

test_that("translation honors overrides and permits either valid configuration", {
  root <- withr::local_tempdir()
  writeLines("data out; x=1; run;", file.path(root, "p.sas"))
  cfg <- list(migration = list(max_parallel_translations = 4L),
    llm = list(provider = "deepseek", model = "offline", max_tries = 2L))
  # Exercise startup and the deterministic pipeline without paid requests.
  testthat::local_mocked_bindings(sas_llm = function(...) NULL)
  serial <- sas_translate(root, out_dir = withr::local_tempdir(), config = cfg,
    max_parallel_translations = 1L, execute = FALSE)
  expect_identical(serial$diagnostics$parallel$effective, 1L)
  cfg$migration$max_parallel_translations <- 1L
  expect_error(sas_translate(root, out_dir = withr::local_tempdir(), config = cfg,
    max_parallel_translations = 4L, execute = FALSE), class = "sas2r_parallel_config_error")
  cfg$llm$max_tries <- 1L
  parallel <- sas_translate(root, out_dir = withr::local_tempdir(), config = cfg,
    max_parallel_translations = 4L, execute = FALSE)
  expect_identical(parallel$diagnostics$parallel$effective, 4L)
  expect_identical(parallel$diagnostics$parallel$backend, "callr")
})

test_that("explicit adapters use their own retry settings and cannot silently downgrade", {
  skip_if_not_installed("ellmer")
  root <- withr::local_tempdir()
  writeLines("data out; x=1; run;", file.path(root, "p.sas"))
  cfg <- list(migration = list(max_parallel_translations = 4L),
    llm = list(provider = "deepseek", model = "offline", max_tries = 1L))
  adapter_config <- cfg$llm
  adapter_config$max_tries <- 2L
  adapter <- sas_llm(adapter_config) # Constructor does not make network calls.
  expect_error(sas_translate(root, out_dir = file.path(root, "out"),
    config = cfg, llm = adapter, execute = FALSE), class = "sas2r_parallel_config_error")
  expect_false(dir.exists(file.path(root, "out")))
  state <- list(translator_llm = NULL, reviewer_llm = adapter, fixer_llm = NULL)
  expect_error(resolve_parallel_execution(state, 4L), class = "sas2r_parallel_config_error")
  expect_identical(resolve_parallel_execution(state, 1L)$effective, 1L)

  # Unused YAML retry settings must not reject an explicit replacement adapter.
  cfg$llm$max_tries <- 2L
  result <- sas_translate(root, out_dir = withr::local_tempdir(), config = cfg,
    llm = parallel_test_llm(list(reviewer = valid_program_review_response())), execute = FALSE)
  expect_identical(result$diagnostics$parallel$effective, 4L)
})
