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
  expect_match(text, "dplyr::* for", fixed = TRUE)
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
