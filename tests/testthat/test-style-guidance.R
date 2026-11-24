test_that("the default package list includes the tidyverse packages agents should prefer", {
  default <- normalize_package_allowlist()
  expect_true(all(c("base", "dplyr", "tidyr", "ggplot2", "stringr", "forcats", "purrr",
    "lubridate", "tibble", "haven", "stats", "utils", "graphics", "grDevices", "grid") %in% default))
  expect_identical(normalize_package_allowlist("base, dplyr"), c("base", "dplyr"))
  privacy <- test_path("..", "..", "docs", "model-privacy.md")
  skip_if_not(file.exists(privacy), "source-tree documentation contract")
  expect_match(paste(readLines(privacy, warn = FALSE), collapse = "\n"),
    paste(default, collapse = ", "), fixed = TRUE)
})

test_that("tidyverse style prefers allowlisted installed packages and keeps the fallback and helper rules", {
  text <- render_style_guidance(list(dialect = "tidyverse",
    allowlist = c("base", "dplyr", "haven", "stats", "utils")))
  expect_length(text, 1L)
  expect_match(text, "tidyverse first", fixed = TRUE)
  if (requireNamespace("dplyr", quietly = TRUE)) {
    expect_match(text, "dplyr::* for", fixed = TRUE)
  } else {
    expect_match(text, "not installed here, so do not use: dplyr", fixed = TRUE)
  }
  expect_false(grepl("ggplot2", text, fixed = TRUE))
  expect_match(text, "use base R or the bundle helpers", fixed = TRUE)
  expect_match(text, "not a defect", fixed = TRUE)
  expect_match(text, "sas_sort", fixed = TRUE)
  expect_match(text, "sas_merge", fixed = TRUE)
  expect_match(text, "chr_cmp", fixed = TRUE)
})

test_that("allowlisted packages that are not installed are named as unavailable", {
  local_mocked_bindings(agent_package_facts = function(allowlist = NULL) list(
    r_version = "4.4.0", allowed = c("base", "dplyr", "ggplot2"),
    versions = c(base = "4.4.0", dplyr = "1.1.4", ggplot2 = "unknown")))
  text <- render_style_guidance(list())
  expect_match(text, "dplyr::* for", fixed = TRUE)
  expect_false(grepl("ggplot2::* for", text, fixed = TRUE))
  expect_match(text, "not installed here, so do not use: ggplot2", fixed = TRUE)
})

test_that("base and free-text dialects render without a tidyverse preference", {
  base <- render_style_guidance(list(dialect = "base"))
  expect_match(base, "base R first", fixed = TRUE)
  expect_false(grepl("tidyverse first", base, fixed = TRUE))
  expect_identical(render_style_guidance(list(dialect = "house rules v2")), "Style: house rules v2")
})

test_that("the style block reaches every writing role's prompt and not the reviewer's", {
  style <- render_style_guidance(list(dialect = "tidyverse"))
  for (file in c("translator.md", "translator-macro.md", "fixer.md")) {
    rendered <- render_prompt(file, list(style = style))
    expect_match(rendered, "tidyverse first", fixed = TRUE)
    expect_false(grepl("{{", rendered, fixed = TRUE))
  }
  reviewer <- paste(readLines(system.file("prompts", "reviewer.md", package = "sas2r")), collapse = "\n")
  expect_false(grepl("{{style}}", reviewer, fixed = TRUE))
})

test_that("the shared policy tells every role that a necessary base R choice is not a finding", {
  policy <- agent_guidance_policy()
  expect_match(policy, "not a finding", fixed = TRUE)
  expect_match(policy, "dplyr::mutate(score = NA_real_)", fixed = TRUE)
})

tidy_skill_example_code <- function(name) {
  lines <- strsplit(agent_skill_catalog()[[name]]$body, "\n", fixed = TRUE)[[1L]]
  start <- which(lines == "```r")
  end <- which(lines == "```")
  paste(unlist(lapply(start, function(i) lines[seq.int(i + 1L, min(end[end > i]) - 1L)])), collapse = "\n")
}

test_that("the tidyverse idioms skill routes to the writing roles for ordinary data work only", {
  contexts <- list(list(unit_type = "data_step"), list(unit_type = "proc_step", procs = "sql"),
    list(unit_type = "macro_def"), list(unit_type = "program", procs = "means"))
  for (ctx in contexts) {
    for (role in c("translator", "fixer")) {
      text <- render_agent_skills(route_agent_skills(c(list(agent = role), ctx)))
      expect_match(text, "tidyverse-idioms", fixed = TRUE)
      expect_match(text, "sas_sort", fixed = TRUE)
    }
    expect_false(grepl("tidyverse-idioms",
      render_agent_skills(route_agent_skills(c(list(agent = "reviewer"), ctx))), fixed = TRUE))
  }
  expect_false(grepl("tidyverse-idioms",
    render_agent_skills(route_agent_skills(list(agent = "translator", unit_type = "global"))), fixed = TRUE))
  catalog <- agent_skill_catalog()
  expect_lt(catalog[["tidyverse-idioms"]]$priority, catalog[["sas-missing-sort-semantics"]]$priority)
  expect_match(catalog[["sas-native-graphics"]]$body, "ggplot2 when it is allowlisted and installed", fixed = TRUE)
  expect_match(catalog[["sas-statistical-defaults"]]$body, 'geom_boxplot(stat = "identity")', fixed = TRUE)
})

test_that("the tidyverse idioms examples keep SAS order and draw supplied statistics", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggplot2")
  code <- tidy_skill_example_code("tidyverse-idioms")
  expect_false(any(lint_r_code(code)$kind == "disallowed_namespace"))
  expect_true(any(lint_r_code(code, allowlist = c("base", "stats"))$kind == "disallowed_namespace"))
  env <- new.env(parent = asNamespace("sas2r"))
  eval(parse(text = code), env)
  # SAS sorts missing first, so subject 01's first visit is the missing one and
  # its baseline is that row's value; dplyr::arrange alone would put it last.
  expect_identical(env$flagged$avisitn[env$flagged$first_visit], c(NA, 1))
  expect_identical(env$flagged$baseline, c(4, 4, 7, 7, 7))
  expect_identical(env$flagged$avisitn[env$flagged$last_visit], c(2, 3))
  drawn <- ggplot2::layer_data(env$figure, 1L)
  expect_equal(unname(unlist(drawn[1L, c("ymin", "lower", "middle", "upper", "ymax")])), c(0, 1, 3, 5, 5))
  expect_equal(ggplot2::layer_data(env$figure, 2L)$y, 20)
})

test_that("the example configuration and the guide document the style dialect and package list", {
  example <- test_path("..", "..", "inst", "examples", "_sas2r.example.yml")
  guide <- test_path("..", "..", "docs", "running-migrations.md")
  skip_if_not(file.exists(example) && file.exists(guide), "source-tree documentation contract")
  config <- doc_validate_config(example)
  expect_identical(config$dialect, "tidyverse")
  expect_match(paste(readLines(example, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    "# allowlist:", fixed = TRUE)
  guide_text <- paste(readLines(guide, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(guide_text, "## Code style and packages", fixed = TRUE)
  expect_match(guide_text, "dialect: base", fixed = TRUE)
  expect_match(guide_text, "not a finding", fixed = TRUE)
})

test_that("the report counts tidyverse calls against base data-frame operations", {
  counts <- code_style_counts(paste(
    "x <- lib_read('raw', 'input') |> dplyr::mutate(a = 1) |> dplyr::filter(a > 0)",
    "y <- x[x$a > 0, , drop = FALSE]", "z <- merge(x, y, by = 'a')", sep = "\n"))
  expect_identical(counts, list(tidyverse = 2L, base = 2L))
  expect_identical(code_style_counts("not r code ((("), list(tidyverse = 0L, base = 0L))
  fx <- repair_workflow_fixture(n = 2L, failures = integer())
  state <- fx$state
  state$selected_revisions$p01$r_code <- paste(
    "x <- lib_read('raw', 'input') |> dplyr::mutate(value = value + 1)",
    "lib_write(x, 'work', 'out1')", sep = "\n")
  write_migration_report(state)
  report <- read_json_record(state$paths$report_json)
  expect_identical(report$style_observation$tidyverse, 1L)
  expect_identical(report$style_observation$base, 0L)
  expect_identical(report$style_observation$components, 2L)
  md <- paste(readLines(state$paths$report_md), collapse = "\n")
  expect_match(md, "Style observation (advisory): tidyverse package calls 1; base data-frame operations 0; across 2 components.", fixed = TRUE)
})

test_that("dialect and allowlist in _sas2r.yml reach the project configuration and the style block", {
  root <- withr::local_tempdir()
  writeLines(c("dialect: base", "allowlist: [base, dplyr, stats]"), file.path(root, "_sas2r.yml"))
  writeLines("data work.out; x=1; run;", file.path(root, "p.sas"))
  cfg <- sas_config(file.path(root, "_sas2r.yml"))
  expect_identical(cfg$dialect, "base")
  expect_identical(normalize_package_allowlist(cfg$allowlist), c("base", "dplyr", "stats"))
  project <- sas_project(root, config = cfg)
  expect_identical(project$config$dialect, "base")
  expect_identical(normalize_package_allowlist(project$config$allowlist), c("base", "dplyr", "stats"))
  expect_match(render_style_guidance(project$config), "base R first", fixed = TRUE)
  expect_identical(agent_package_facts(project$config$allowlist)$allowed, c("base", "dplyr", "stats"))
  # A file without the keys keeps the defaults.
  writeLines("libraries: {}", file.path(root, "_sas2r.yml"))
  plain <- sas_config(file.path(root, "_sas2r.yml"))
  expect_null(plain$dialect)
  expect_null(plain$allowlist)
})

test_that("a configuration file with non-ASCII comments is read without warnings in any locale", {
  root <- withr::local_tempdir()
  path <- file.path(root, "_sas2r.yml")
  writeLines(c("# Active example: Gemini Flash — first evaluation", "dialect: tidyverse"), path, useBytes = FALSE)
  expect_no_warning(cfg <- sas_config(path))
  expect_identical(cfg$dialect, "tidyverse")
})

test_that("package facts take one line so a longer default list does not crowd the packet", {
  root <- withr::local_tempdir()
  writeLines("data work.out; x=1; run;", file.path(root, "p.sas"))
  project <- sas_project(root, config = list(allowlist = c("base", "haven", "notapkg9")))
  lines <- strsplit(build_agent_guidance(project, "p", config = project$config)$text, "\n", fixed = TRUE)[[1L]]
  facts <- lines[grepl("installation is not semantic support", lines, fixed = TRUE)]
  expect_length(facts, 1L)
  expect_match(facts, "base [0-9]")
  expect_match(facts, "haven [0-9]")
  expect_match(facts, "notapkg9 not installed", fixed = TRUE)
  expect_false(any(grepl("allowed by mechanical lint", lines, fixed = TRUE)))
})

# The pharmaverse dialect is opt-in. Package facts are mocked throughout: no
# test may assume a pharmaverse package is installed where the suite runs.
local_pharmaverse_facts <- function(allowed, installed, envir = parent.frame()) {
  local_mocked_bindings(agent_package_facts = function(allowlist = NULL) {
    versions <- stats::setNames(ifelse(allowed %in% installed, "1.0.0", "unknown"), allowed)
    list(r_version = "4.4.0", allowed = allowed, versions = versions)
  }, .env = envir)
}

test_that("the default style is unchanged and never mentions pharmaverse packages", {
  default <- render_style_guidance(list())
  expect_identical(render_style_guidance(list(dialect = "tidyverse")), default)
  expect_match(default, "^Style: tidyverse first, faithful always\\.")
  expect_false(grepl("pharmaverse|admiral|rtables|gtsummary", default, ignore.case = TRUE))
  # Allowlisting a pharmaverse package does not switch the dialect on.
  local_pharmaverse_facts(c("base", "dplyr", "admiral"), c("base", "dplyr", "admiral"))
  expect_false(grepl("pharmaverse|admiral", render_style_guidance(list()), ignore.case = TRUE))
  expect_false(grepl("admiral", render_style_guidance(list(dialect = "base")), fixed = TRUE))
})

test_that("the pharmaverse dialect keeps the tidyverse rules and adds faithful-first conditions", {
  local_pharmaverse_facts(
    allowed = c("base", "dplyr", "admiral", "tern"),
    installed = c("base", "dplyr", "admiral"))
  tidy <- render_style_guidance(list(dialect = "tidyverse"))
  text <- render_style_guidance(list(dialect = "pharmaverse"))
  expect_length(text, 1L)
  expect_identical(render_style_guidance(list(dialect = "Pharmaverse")), text)
  tidy_lines <- strsplit(tidy, "\n", fixed = TRUE)[[1L]]
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  # Everything the tidyverse dialect says after its heading is still said,
  # in the same order, before anything about pharmaverse packages.
  expect_identical(lines[seq_along(tidy_lines)][-1L], tidy_lines[-1L])
  expect_match(lines[1L], "pharmaverse only where it is exact, faithful always", fixed = TRUE)
  added <- paste(lines[-seq_along(tidy_lines)], collapse = "\n")
  expect_match(added, "the SAS program is the specification", fixed = TRUE)
  expect_match(added, "- admiral::* for ADaM derivation steps", fixed = TRUE)
  expect_match(added, "not installed here, so do not use: tern.", fixed = TRUE)
  # Not allowlisted, so not named at all.
  expect_false(grepl("gtsummary|rtables", added))
  expect_match(added, "set each argument that affects results explicitly", fixed = TRUE)
  expect_match(added, "not a defect", fixed = TRUE)
  expect_match(added, "Add nothing a package adds by convention, and drop nothing", fixed = TRUE)
  expect_match(added, "never replaces a bundle helper", fixed = TRUE)
  expect_match(text, "sas_merge", fixed = TRUE)
})

test_that("the pharmaverse dialect names no package that is not allowlisted and installed", {
  local_pharmaverse_facts(allowed = c("base", "dplyr"), installed = c("base", "dplyr"))
  text <- render_style_guidance(list(dialect = "pharmaverse"))
  expect_match(text, "No pharmaverse package is both allowlisted and installed here, so use none.", fixed = TRUE)
  expect_false(grepl("admiral|tern|rtables|gtsummary", text))
  expect_false(grepl("For every such call", text, fixed = TRUE))

  local_pharmaverse_facts(allowed = c("base", "admiral"), installed = "base")
  text <- render_style_guidance(list(dialect = "pharmaverse"))
  expect_match(text, "so use none.", fixed = TRUE)
  expect_match(text, "not installed here, so do not use: admiral.", fixed = TRUE)
  expect_false(grepl("admiral::", text, fixed = TRUE))
})

test_that("the pharmaverse dialect adds nothing to the allowlist and reaches only writing roles", {
  expect_false(any(names(STYLE_PHARMAVERSE_ROLES) %in% normalize_package_allowlist()))
  local_pharmaverse_facts(c("base", "dplyr", "admiral"), c("base", "dplyr", "admiral"))
  style <- render_style_guidance(list(dialect = "pharmaverse"))
  for (file in c("translator.md", "translator-macro.md", "fixer.md")) {
    expect_match(render_prompt(file, list(style = style)), "admiral::* for", fixed = TRUE)
  }
  reviewer <- paste(readLines(system.file("prompts", "reviewer.md", package = "sas2r")), collapse = "\n")
  expect_false(grepl("{{style}}", reviewer, fixed = TRUE))
})

test_that("pharmaverse calls are counted only when present", {
  expect_identical(code_pharmaverse_calls(paste(
    "adsl <- lib_read('adam', 'adsl') |> admiral::derive_vars_merged(dataset_add = ex, by_vars = admiral::exprs(USUBJID))",
    "t <- gtsummary::tbl_summary(adsl)", "# admiral::not_code()", "x <- 'tern::in_a_string'", sep = "\n")), 3L)
  expect_identical(code_pharmaverse_calls("x <- dplyr::mutate(y, a = 1)"), 0L)
  expect_identical(code_pharmaverse_calls("not r code ((("), 0L)

  plain <- migration_style_observation(list(list(r_code = "x <- dplyr::mutate(y, a = 1)")))
  expect_identical(names(plain), c("tidyverse", "base", "components"))
  expect_identical(migration_style_line(plain),
    "Style observation (advisory): tidyverse package calls 1; base data-frame operations 0; across 1 components.")
  used <- migration_style_observation(list(list(r_code = "x <- admiral::derive_vars_dt(y, new_vars_prefix = 'A', dtc = DTC)")))
  expect_identical(used$pharmaverse, 1L)
  expect_identical(migration_style_line(used),
    "Style observation (advisory): tidyverse package calls 0; base data-frame operations 0; pharmaverse package calls 1; across 1 components.")
})
