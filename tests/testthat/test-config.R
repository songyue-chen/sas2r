test_that("no config anywhere returns working defaults (zero-config path)", {
  dir <- withr::local_tempdir()
  cfg <- sas_config(start = dir)
  expect_s3_class(cfg, "sas2r_config")
  expect_identical(cfg$source, NA_character_)
  expect_identical(cfg$libraries, list())
  expect_identical(cfg$macro_search_path, character())
})

test_that("config is discovered upward from a nested directory", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "libraries:",
    "  adam: { path: data/adam, engine: sas7bdat, write: xpt }",
    "macros:",
    "  search_path:",
    "    - macros",
    "    - /org/macros"
  ), file.path(dir, "_sas2r.yml"))
  dir.create(file.path(dir, "data", "adam"), recursive = TRUE)
  dir.create(file.path(dir, "macros"))
  nested <- file.path(dir, "programs", "tlf"); dir.create(nested, recursive = TRUE)
  cfg <- sas_config(start = nested)
  expect_identical(basename(cfg$source), "_sas2r.yml")
  # every configured path is resolved against the configuration file's own
  # directory, so a scan launched from a nested directory names the same places
  expect_identical(cfg$libraries$adam$path,
                   normalizePath(file.path(dir, "data/adam"),
                                 winslash = "/", mustWork = FALSE))
  expect_identical(cfg$libraries$adam$engine, "sas7bdat")
  expect_identical(cfg$libraries$adam$write, "xpt")
  expect_identical(
    cfg$macro_search_path,
    c(normalizePath(file.path(dir, "macros"), winslash = "/", mustWork = FALSE),
      "/org/macros")
  )
})

test_that("SAS2R_CONFIG env var wins over discovery", {
  dir <- withr::local_tempdir()
  explicit <- file.path(dir, "special.yml")
  writeLines("libraries: {sdtm: {path: /s}}", explicit)
  withr::local_envvar(SAS2R_CONFIG = explicit)
  cfg <- sas_config(start = dir)
  expect_identical(cfg$source, explicit)
  expect_identical(cfg$libraries$sdtm$path, "/s")
})

test_that("unknown top-level keys warn but do not fail", {
  dir <- withr::local_tempdir()
  writeLines("bananas: true", file.path(dir, "_sas2r.yml"))
  expect_warning(sas_config(start = dir), "bananas")
})

test_that("0-byte config file returns raw = list()", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "_sas2r.yml"))
  cfg <- sas_config(start = dir)
  expect_s3_class(cfg, "sas2r_config")
  expect_identical(cfg$raw, list())
  expect_identical(cfg$libraries, list())
  expect_identical(cfg$macro_search_path, character())
})

test_that("budget config keeps policy modes separate from LLM identity", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "budget:",
    "  mode: strict",
    "  pricing_source: external",
    "  max_usd: 12.5",
    "  max_calls: 7",
    "  max_request_chars: 4096"
  ), file.path(dir, "_sas2r.yml"))

  cfg <- sas_config(start = dir)

  expect_identical(cfg$budget$mode, "strict")
  expect_identical(cfg$budget$pricing_source, "external")
  expect_equal(cfg$budget$max_usd, 12.5)
  expect_equal(cfg$budget$max_calls, 7)
  expect_equal(cfg$budget$max_request_chars, 4096)

  writeLines(c(
    "budget:",
    "  mode: observe",
    "  guessed_price_per_token: 0.1"
  ), file.path(dir, "_sas2r.yml"))
  expect_error(sas_config(start = dir), class = "sas2r_budget_config_error")
})

test_that("scalar macros config value does not crash sas_config", {
  dir <- withr::local_tempdir()
  writeLines("macros: mymacs", file.path(dir, "_sas2r.yml"))
  cfg <- sas_config(start = dir)
  # the shorthand spelling gets the same config-directory base as the mapping
  expect_identical(cfg$macro_search_path,
                   file.path(normalizePath(dir, winslash = "/",
                                           mustWork = FALSE),
                             "mymacs"))
})

test_that("cloud llm config defaults to ambient identity and rejects typos", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "llm:",
    "  provider: bedrock",
    "  profile: clinical-dev",
    "  region: us-west-2",
    "  model: us.anthropic.example"
  ), file.path(dir, "_sas2r.yml"))

  cfg <- sas_config(start = dir)
  expect_identical(cfg$llm$auth_mode, "ambient")
  expect_identical(cfg$llm$profile, "clinical-dev")
  expect_identical(cfg$llm$region, "us-west-2")

  writeLines(c(
    "llm:",
    "  provider: bedrock",
    "  profle: clinical-dev",
    "  region: us-west-2",
    "  model: us.anthropic.example"
  ), file.path(dir, "_sas2r.yml"))
  expect_error(sas_config(start = dir), class = "sas2r_llm_config_error")
})

test_that("cloud llm config validates provider-specific selectors", {
  dir <- withr::local_tempdir()
  cases <- list(
    c("llm:", "  provider: azure", "  model: deployment", "  api_version: '2025-04-01-preview'"),
    c("llm:", "  provider: vertex", "  model: gemini", "  location: us-central1"),
    c("llm:", "  provider: bedrock", "  model: anthropic.example")
  )
  for (lines in cases) {
    writeLines(lines, file.path(dir, "_sas2r.yml"))
    expect_error(sas_config(start = dir), class = "sas2r_llm_config_error")
  }
})

test_that("llm config rejects selectors for the wrong provider and secret-bearing URLs", {
  dir <- withr::local_tempdir()
  cases <- list(
    c(
      "llm:", "  provider: azure",
      "  endpoint: https://example.openai.azure.com",
      "  api_version: v1", "  model: deployment", "  profile: wrong-provider"
    ),
    c(
      "llm:", "  provider: bedrock", "  region: us-west-2", "  model: m",
      "  endpoint: https://wrong-provider.example"
    ),
    c(
      "llm:", "  provider: azure",
      "  endpoint: https://example.openai.azure.com?api_key=do-not-persist",
      "  api_version: v1", "  model: deployment"
    ),
    c(
      "llm:", "  provider: azure", "  endpoint: not-a-url",
      "  api_version: v1", "  model: deployment"
    ),
    c(
      "llm:", "  provider: ollama", "  auth_mode: ambient",
      "  base_url: http://127.0.0.1:11434", "  model: local-model"
    ),
    c(
      "llm:", "  provider: vertex", "  project_id: p",
      "  location: us-central1", "  model: gemini",
      "  capabilities:", "    structured_otput: native"
    )
  )
  for (lines in cases) {
    writeLines(lines, file.path(dir, "_sas2r.yml"))
    expect_error(sas_config(start = dir), class = "sas2r_llm_config_error")
  }
})

test_that("llm tiers accept only the documented frontier cheap and fast names", {
  base <- list(
    provider = "azure", endpoint = "https://example.openai.azure.com",
    api_version = "v1"
  )

  accepted <- normalize_llm_config(c(base, list(tiers = list(
    frontier = "deployment-a", cheap = "deployment-b", fast = "deployment-c"
  ))))
  expect_identical(names(accepted$tiers), c("frontier", "cheap", "fast"))

  expect_error(
    normalize_llm_config(c(base, list(tiers = list(turbo = "deployment-a")))),
    class = "sas2r_llm_config_error"
  )
  expect_error(
    normalize_llm_config(c(base, list(tiers = structure(
      list("deployment-a", "deployment-b"),
      names = c("cheap", "cheap")
    )))),
    class = "sas2r_llm_config_error"
  )
})

test_that("Bedrock selectors are unambiguous and use ellmer cache modes", {
  base <- list(
    provider = "bedrock", model = "anthropic.example", auth_mode = "ambient"
  )

  expect_identical(
    normalize_llm_config(c(base, list(region = "us-west-2", cache = "5m")))$cache,
    "5m"
  )
  expect_error(
    normalize_llm_config(c(base, list(region = "us-west-2", cache = TRUE))),
    class = "sas2r_llm_config_error"
  )
  expect_error(
    normalize_llm_config(c(base, list(region = "us-west-2.evil.invalid"))),
    class = "sas2r_llm_config_error"
  )
  expect_error(
    normalize_llm_config(c(base, list(
      region = "us-west-2", base_url = "https://bedrock.example.invalid"
    ))),
    class = "sas2r_llm_config_error"
  )
})

test_that("cloud API-key modes are limited to constructors that support them", {
  expect_silent(normalize_llm_config(list(
    provider = "azure", auth_mode = "api_key",
    endpoint = "https://example.openai.azure.com", api_version = "v1",
    model = "deployment", api_key = "azure-test-key"
  )))

  expect_error(normalize_llm_config(list(
    provider = "bedrock", auth_mode = "api_key", region = "us-west-2",
    model = "anthropic.example", api_key = "unsupported"
  )), "does not accept selector.*api_key", class = "sas2r_llm_config_error")
  expect_error(normalize_llm_config(list(
    provider = "vertex", auth_mode = "api_key", project_id = "project-a",
    location = "us-central1", model = "gemini-example", api_key = "unsupported"
  )), "does not accept selector.*api_key", class = "sas2r_llm_config_error")
  expect_error(normalize_llm_config(list(
    provider = "bedrock", auth_mode = "api_key", region = "us-west-2",
    model = "anthropic.example"
  )), "supports ambient authentication only", class = "sas2r_llm_config_error")
  expect_error(normalize_llm_config(list(
    provider = "vertex", auth_mode = "api_key", project_id = "project-a",
    location = "us-central1", model = "gemini-example"
  )), "supports ambient authentication only", class = "sas2r_llm_config_error")
})

test_that("a relative includes.roots entry resolves against the config file, not getwd()", {
  base <- withr::local_tempdir()
  cfg_dir <- file.path(base, "proj"); dir.create(cfg_dir)
  shared <- file.path(base, "shared"); dir.create(shared)
  nested <- file.path(cfg_dir, "programs"); dir.create(nested)
  writeLines(c("includes:", "  roots:", "    - ../shared", "    - /org/shared"),
             file.path(cfg_dir, "_sas2r.yml"))
  elsewhere <- withr::local_tempdir()

  roots_from <- function(cwd) {
    withr::with_dir(cwd, sas_config(start = nested)$include_roots)
  }
  here <- roots_from(cfg_dir)
  away <- roots_from(elsewhere)

  # the same configuration file names the same two directories from anywhere
  expect_identical(away, here)
  expect_identical(normalizePath(here[1], winslash = "/"),
                   normalizePath(shared, winslash = "/"))
  # an absolute entry is already anchored and is passed through untouched
  expect_identical(here[2], "/org/shared")
})

test_that("a ~-prefixed includes.roots entry is treated as already anchored", {
  dir <- withr::local_tempdir()
  writeLines(c("includes:", "  roots:", "    - ~/r", "    - shared"),
             file.path(dir, "_sas2r.yml"))

  cfg <- sas_config(start = dir)

  # R expands ~ against the user's home directory, not the working directory,
  # so prefixing a base onto it would corrupt the path rather than anchor it --
  # it must come back exactly as configured, with no base prefixed
  expect_identical(cfg$include_roots[1], "~/r")
  # a genuinely relative sibling entry still gets the config directory as its
  # base, so the ~ entry's pass-through is not just every entry going untouched
  expect_identical(cfg$include_roots[2],
                   file.path(normalizePath(dir, winslash = "/", mustWork = FALSE),
                             "shared"))
})

test_that("a relative includes.roots entry is stored already normalized", {
  base <- withr::local_tempdir()
  cfg_dir <- file.path(base, "proj"); dir.create(cfg_dir)
  shared <- file.path(base, "shared"); dir.create(shared)
  writeLines(c("includes:", "  roots:", "    - ../shared", "    - /org/shared"),
             file.path(cfg_dir, "_sas2r.yml"))

  cfg <- sas_config(start = cfg_dir)

  # the base (the configuration directory) was already canonical; the joined
  # relative entry is now canonical too, with no literal ".." segment left in
  # it -- harmless either way (the anchor builder and file.exists() both
  # normalize again), but this is what a human reading print(cfg) sees
  expect_identical(cfg$include_roots[1],
                   normalizePath(shared, winslash = "/", mustWork = FALSE))
  # an already-anchored entry is untouched, exactly as before --
  # config_resolve_paths() leaves it alone and there is nothing to normalize
  expect_identical(cfg$include_roots[2], "/org/shared")
})

test_that("autoexec and macro search path share the includes.roots base rule", {
  base <- withr::local_tempdir()
  cfg_dir <- file.path(base, "proj"); dir.create(cfg_dir)
  env_dir <- file.path(cfg_dir, "env"); dir.create(env_dir)
  mac_dir <- file.path(base, "shared", "macros"); dir.create(mac_dir, recursive = TRUE)
  nested <- file.path(cfg_dir, "programs"); dir.create(nested)
  writeLines(c("environment:", "  autoexec:", "    - env/autoexec.sas",
               "macros:", "  search_path:", "    - ../shared/macros"),
             file.path(cfg_dir, "_sas2r.yml"))
  writeLines("libname adam '/data/adam';", file.path(env_dir, "autoexec.sas"))

  cfg <- sas_config(start = nested)

  # both keys are resolved against the configuration file's directory, exactly
  # as includes.roots already is -- not against the project root and never
  # against getwd()
  expect_identical(normalizePath(cfg$autoexec, winslash = "/"),
                   normalizePath(file.path(env_dir, "autoexec.sas"),
                                 winslash = "/"))
  expect_identical(normalizePath(cfg$macro_search_path, winslash = "/"),
                   normalizePath(mac_dir, winslash = "/"))

  # and the project scanned from a directory below the configuration file finds
  # the configured autoexec rather than silently missing it
  writeLines("data w; set adam.adsl; run;", file.path(nested, "a.sas"))
  p <- sas_project(nested)
  expect_false("autoexec_missing" %in% p$flags$kind)
  expect_true("adam" %in% p$librefs$libref)
})

test_that("a ~-prefixed autoexec entry is anchored like a ~-prefixed include root", {
  dir <- withr::local_tempdir()
  writeLines(c("environment:", "  autoexec:",
               "    - ~/sas2r-no-such-autoexec.sas", "    - env/autoexec.sas"),
             file.path(dir, "_sas2r.yml"))

  cfg <- sas_config(start = dir)

  # one anchoring rule for every configured path: R expands ~ against the home
  # directory, so prefixing a base onto it would corrupt the path -- it must
  # not become "<base>/~/sas2r-no-such-autoexec.sas"
  expect_identical(cfg$autoexec[1], "~/sas2r-no-such-autoexec.sas")
  expect_identical(cfg$autoexec[2],
                   file.path(normalizePath(dir, winslash = "/", mustWork = FALSE),
                             "env/autoexec.sas"))
})

test_that("a caller-supplied relative library path is anchored on the project root", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data", "adam"), recursive = TRUE)
  writeLines("data w; set adam.adsl; run;", file.path(root, "a.sas"))

  # no configuration file exists, so getwd() must not be what answers "relative
  # to what?" -- the project root does
  p <- withr::with_dir(withr::local_tempdir(), {
    sas_project(root, config = list(libraries = list(adam = "data/adam")))
  })

  expect_identical(p$config$libraries$adam$path,
                   normalizePath(file.path(root, "data", "adam"),
                                 winslash = "/", mustWork = FALSE))
  expect_identical(p$config$libraries$adam$engine, "sas7bdat")
  expect_identical(p$config$libraries$adam$write, "rds")
})

test_that("a library entry without a usable path is a configuration error", {
  dir <- withr::local_tempdir()
  writeLines(c("libraries:", "  adam:", "    engine: sas7bdat"),
             file.path(dir, "_sas2r.yml"))
  expect_error(sas_config(start = dir), class = "sas2r_config_error")
})

test_that("a libraries mapping without its configuration file is refused", {
  # `sas_config()` only ever hands `normalize_library_config()` a mapping it
  # read out of a file, so there is no working-directory case: inventing one
  # would put invocation state back into a configured path.
  expect_error(
    normalize_library_config(list(adam = "data/adam"), NA_character_),
    class = "sas2r_config_error"
  )
  expect_identical(normalize_library_config(NULL, NA_character_), list())
})

test_that("one directory gets one spelling whichever configured group names it", {
  base <- withr::local_tempdir()
  real <- file.path(base, "real"); dir.create(real)
  link <- file.path(base, "link")
  skip_if_not(suppressWarnings(file.symlink(real, link)), "no symlink support")
  writeLines(c("libraries:", "  adam: data/adam",
               "includes:", "  roots:", "    - data/adam"),
             file.path(link, "_sas2r.yml"))
  cfg <- sas_config(file.path(link, "_sas2r.yml"))
  # `data/adam` does not exist, so nothing downstream can canonicalize it away:
  # the base has to have been canonical already, for libraries as for includes.
  expect_identical(cfg$libraries$adam$path, cfg$include_roots)
  expect_true(startsWith(cfg$libraries$adam$path,
                         include_normalize_path(real)))
})

test_that("a library write format the runtime cannot honour fails at config time", {
  # `lib_write()` writes rds or xpt. Anything else used to normalize cleanly,
  # reach the generated registry, and only fail at run time -- inside a bundle
  # a reviewer had already read -- on the first write to that library.
  base <- withr::local_tempdir()
  expect_error(
    normalize_library_entries(
      list(adam = list(path = "data/adam", write = "parquet")), base),
    class = "sas2r_config_error"
  )
  expect_error(
    normalize_library_entries(
      list(adam = list(path = "data/adam", write = "parquet")), base),
    "parquet"
  )
  # The two the runtime does honour, case-insensitively, still pass.
  for (fmt in c("rds", "XPT")) {
    entry <- normalize_library_entries(
      list(adam = list(path = "data/adam", write = fmt)), base)
    expect_identical(entry$adam$write, tolower(fmt))
  }
  # The default is unchanged.
  expect_identical(
    normalize_library_entries(list(adam = "data/adam"), base)$adam$write, "rds")
})

test_that("the shipped demo project configures a write format the runtime honours", {
  cfg <- sas_config(system.file("examples", "demo_project", "_sas2r.yml",
                                package = "sas2r"))
  expect_true(cfg$libraries$adam$write %in% c("rds", "xpt"))
})

test_that("sas_config parses comparison_rules and does not warn on migration key", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "comparison_rules:",
    "  tolerance:",
    "    numeric: 0.0001",
    "migration:",
    "  execute: true"
  ), file.path(dir, "_sas2r.yml"))

  expect_no_warning(cfg <- sas_config(start = dir))
  expect_identical(cfg$comparison_rules$tolerance$numeric, 1e-4)
})


test_that("a library read engine the runtime cannot read fails at config time", {
  # lib_read() resolves rds, sas7bdat, and xpt members. An engine like duckdb
  # used to normalize cleanly and do nothing.
  base <- withr::local_tempdir()
  expect_error(
    normalize_library_entries(
      list(adam = list(path = "data/adam", engine = "duckdb")), base),
    class = "sas2r_config_error"
  )
  expect_error(
    normalize_library_entries(
      list(adam = list(path = "data/adam", engine = "duckdb")), base),
    "duckdb"
  )
  for (eng in c("rds", "XPT", "sas7bdat")) {
    entry <- normalize_library_entries(
      list(adam = list(path = "data/adam", engine = eng)), base)
    expect_identical(entry$adam$engine, tolower(eng))
  }
  expect_identical(
    normalize_library_entries(list(adam = "data/adam"), base)$adam$engine,
    "sas7bdat")
})
