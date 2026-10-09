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
    args <- list(path = root, config = config, out_dir = file.path(root, "out"))
    if (identical(entry, sas_preflight)) args$diagnose <- "off"
    error <- tryCatch(do.call(entry, args), error = identity)
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
  expect_identical(sas_preflight(root, diagnose = "off", config = cfg,
    max_parallel_translations = 1L)$max_parallel_translations, 1L)
  cfg$migration$max_parallel_translations <- 1L
  expect_identical(sas_preflight(root, diagnose = "off", config = cfg)$max_parallel_translations, 1L)
  expect_error(sas_preflight(root, diagnose = "off", config = cfg, max_parallel_translations = 4L),
    class = "sas2r_parallel_config_error")
  cfg$llm$max_tries <- 1L
  expect_identical(sas_preflight(root, diagnose = "off", config = cfg,
    max_parallel_translations = 4L)$max_parallel_translations, 4L)
  cfg$llm$max_tries <- NULL
  expect_identical(sas_preflight(root, diagnose = "off", config = cfg,
    max_parallel_translations = 4L)$max_parallel_translations, 4L)
})
