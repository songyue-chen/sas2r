review_source <- function(root, code) {
  file <- file.path(root, "main.sas")
  writeLines(code, file)
  file
}

test_that("global alignment hints do not impose keys on unreferenced summaries", {
  contract <- list(target_key = "adam.summary", required = TRUE, assertions = list())
  data <- data.frame(TRT = "A", N = 2)
  rules <- list(keys = c("STUDYID", "USUBJID"))
  result <- assess_dataset_target(contract, data, rules)
  expect_true(result$passed)
  expect_null(result$checks$keys_present)
  rules$unique_keys <- TRUE
  expect_false(assess_dataset_target(contract, data, rules)$checks$keys_present$passed)
})

test_that("R and YAML profiles inherit globals and explicit fields override them", {
  ref <- data.frame(USUBJID = c("01", "01"), AGE = c(50, 60))
  path <- withr::local_tempfile(fileext = ".rds"); saveRDS(ref, path)
  rules <- list(tolerances = list(AGE = list(abs = 1)), keys = "USUBJID", unique_keys = TRUE)
  candidate <- ref; candidate$AGE <- ref$AGE + 0.5
  profile <- qc_profile(required_columns = "USUBJID")
  expect_null(profile$tolerances)
  expect_null(profile$unique_keys)
  expect_false(is.object(profile))
  for (p in list(profile, list(required_columns = "USUBJID"))) {
    contract <- infer_output_contracts(NULL, list(profiles = list(subject = p),
      references = list("adam.adsl" = path), assertions = list("adam.adsl" = list(profile = "subject"))))
    result <- assess_dataset_target(contract, candidate, rules)
    expect_true(result$reference_passed)
    expect_false(result$checks$unique_keys$passed)
  }
  contract <- list(target_key = "adam.adsl", reference_path = path,
    assertions = qc_profile(unique_keys = FALSE, tolerances = list()))
  result <- assess_dataset_target(contract, candidate, rules)
  expect_false(result$reference_passed)
  expect_null(result$checks$unique_keys)
})

test_that("invalid rules, assertions and budget arguments fail before writes or model calls", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  testthat::local_mocked_bindings(sas_llm = function(...) stop("must not construct adapter"))
  bad <- list(list(required_columns = list("USUBJID")),
    list(required_columns = c("USUBJID", "usubjid")), list(tolerances = list(AGE = 0.01)),
    list(min_rows = "1"), list(keys = list("USUBJID")), list(unique_keys = TRUE))
  for (rules in bad) {
    yaml <- file.path(root, "bad.yml"); yaml::write_yaml(list(comparison_rules = rules), yaml)
    if (is.list(rules$required_columns) || is.list(rules$keys)) {
      # YAML sequences deserialize to valid character vectors.
      expect_no_error(sas_config(yaml))
    } else expect_error(sas_config(yaml), class = "sas2r_output_contract_error")
    expect_error(sas_translate(file, out_dir = file.path(root, "out"),
      config = list(comparison_rules = rules), execute = FALSE), class = "sas2r_output_contract_error")
    expect_false(dir.exists(file.path(root, "out")))
    expect_false(dir.exists(file.path(root, ".sas2r")))
  }
  for (args in list(list(budget_mode = "typo"), list(pricing_source = "typo"),
                   list(usage_limits = list(max_calls = -1)))) {
    expect_error(do.call(sas_translate, c(list(path = file, out_dir = file.path(root, "out")), args)))
    expect_false(dir.exists(file.path(root, "out")))
    expect_false(dir.exists(file.path(root, ".sas2r")))
  }
  # A failed resume must preserve its previous contracts and graph too.
  state <- file.path(root, "out", ".sas2r"); dir.create(state, recursive = TRUE)
  prior <- c("graph.json", "output-contracts.json")
  for (name in prior) writeLines("previous run", file.path(state, name))
  expect_error(sas_translate(file, out_dir = dirname(state), resume = TRUE, budget_mode = "typo"))
  for (name in prior) expect_identical(readLines(file.path(state, name)), "previous run")
  expect_false(dir.exists(file.path(root, ".sas2r")))
})

test_that("direct profiles survive translation contract serialization and scan reuse", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data adam.adsl; x=1; run;")
  overrides <- list(assertions = list("adam.adsl" = qc_profile(required_columns = "x")))
  check <- sas_preflight(file, config = list(libraries = list(adam = root)), outputs = overrides)
  testthat::local_mocked_bindings(sas_project = function(...) stop("must reuse the supplied project"))
  result <- sas_translate(check$project, out_dir = file.path(root, "out"), outputs = overrides,
    execute = FALSE, usage_limits = list(max_calls = 0))
  expect_true(file.exists(result$output_contracts_path))
  saved <- jsonlite::read_json(result$output_contracts_path)
  expect_true(length(saved) > 0L)
  expect_identical(check$project$config$libraries, translation_config(check$project, NULL)$libraries)
})

test_that("malformed metadata fails with the complete observed value", {
  for (field in c("label", "format.sas")) for (value in list(c("Age", "years"), character(), 8)) {
    data <- data.frame(AGE = 1)
    attr(data$AGE, field) <- value
    requirement <- if (field == "label") "labels" else "formats"
    assertions <- setNames(list(list(AGE = "Age")), requirement)
    result <- assess_dataset_target(list(target_key = "adam.adsl", assertions = assertions), data)
    expect_false(result$passed)
    expect_identical(result$checks[[requirement]]$actual$AGE, value)
  }
})

test_that("QC validates each metadata value and supports empty mappings and named order", {
  expect_error(qc_profile(formats = list(AVAL = 8, ADT = "DATE9.")), "quote YAML")
  expect_error(qc_profile(formats = list(AVAL = 8.2)), "quote YAML")
  for (empty in list(character(), list())) {
    p <- qc_profile(labels = empty, formats = empty, types = empty,
      column_order = c(subj = "USUBJID", age = "AGE"))
    expect_true(check_dataset_qc(data.frame(USUBJID = "01", AGE = 1), p)$column_order$passed)
  }
  raw <- yaml::yaml.load('labels: {"N": "Count"}\nformats: {AVAL: "8.", ADT: "DATE9."}\ntolerances: {AGE: {abs: 1e-6, rel: 0}}')
  p <- do.call(qc_profile, raw)
  expect_identical(p$labels, list(N = "Count"))
  expect_identical(p$tolerances$age$abs, 1e-6)
  for (a in list(list(row_cout = 1), list(profile = "subject", row_cout = 1))) {
    expect_error(validate_output_overrides(list(profiles = list(subject = qc_profile()),
      assertions = list("adam.adsl" = a))), "row_cout", class = "sas2r_output_contract_error")
  }
  expect_error(validate_output_overrides(list(profiles = list(subject = 1))),
    class = "sas2r_output_contract_error")
  expect_no_error(validate_output_overrides(list(assertions = list("table.html" = list(required_text = "Count")))))
})

test_that("physical type requirements remain distinct from comparison coercion", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(USUBJID = "01"), path)
  contract <- list(target_key = "adam.adsl", reference_path = path,
    assertions = qc_profile(types = c(USUBJID = "character")))
  result <- assess_dataset_target(contract, data.frame(USUBJID = factor("01")))
  expect_true(result$reference_passed)
  expect_false(result$checks$types$passed)
  expect_identical(result$checks$types$actual$USUBJID, "factor")
})

test_that("preflight reports unresolved dataset names and does not flag macro-valued filters", {
  root <- withr::local_tempdir()
  for (code in c("data out; set &lib..dm; run;", "data out; set raw.&ds; run;",
                 "proc sort data=&lib..dm; by x; run;", "data out; merge a raw.&ds; by x; run;")) {
    check <- sas_preflight(review_source(root, code))
    expect_identical(check$status, "needs_attention")
    expect_true("dynamic_dataset_reference" %in% check$findings$kind)
  }
  check <- sas_preflight(review_source(root, "data out; set raw.dm(where=(x=&limit)); run;"))
  expect_false("dynamic_dataset_reference" %in% check$findings$kind)
  check <- sas_preflight(review_source(root, 'data out; note="from &lib"; x=1; run;'))
  expect_identical(check$status, "ready_for_translation")
})

test_that("preflight distinguishes ordered in-file work, unknown producers and real cycles", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data stage; x=1; run; data out; set stage; run;")
  check <- sas_preflight(file)
  expect_identical(check$status, "ready_for_translation")
  expect_identical(check$inputs$status, "generated")
  expect_false("dependency_cycle" %in% check$findings$kind)
  expect_identical(check$references$status, character())
  expect_identical(check$schedule, check$project$schedule)
  testthat::local_mocked_bindings(build_dependency_graph = function(...) stop("must reuse the project graph"))
  expect_no_error(sas_preflight(check$project))
})

test_that("unknown WORK producers get a source remedy rather than a file remedy", {
  root <- withr::local_tempdir()
  for (code in c("data out; set nowhere; run;", "data out; set stage; run; data stage; x=1; run;")) {
    check <- sas_preflight(review_source(root, code))
    expect_identical(check$inputs$status, "no_producer")
    expect_identical(check$status, "needs_attention")
    expect_true(any(grepl("earlier step", check$next_actions)))
    expect_false(any(grepl("Supply missing input members", check$next_actions)))
  }
})

test_that("preflight retains cross-file cycles and unresolved setup findings", {
  root <- withr::local_tempdir()
  writeLines("data a; set b; run;", file.path(root, "a.sas"))
  writeLines("data b; set a; run;", file.path(root, "b.sas"))
  check <- sas_preflight(root)
  expect_true("dependency_cycle" %in% check$findings$kind)
  expect_identical(check$status, "needs_attention")
  for (code in c("data undeclared.out; x=1; run;", "%include &prog;",
                 "libname remote oracle user=test; data remote.out; x=1; run;")) {
    check <- sas_preflight(review_source(root, code))
    expect_identical(check$status, "needs_attention")
    expect_true(length(check$next_actions) > 0L)
  }
  for (i in 1:12) writeLines(if (i < 12) sprintf("%%include 'inc%d.sas';", i+1L) else "data out; x=1; run;",
    file.path(root, paste0("inc", i, ".sas")))
  check <- sas_preflight(file.path(root, "inc1.sas"))
  expect_true("include_depth_exceeded" %in% check$findings$kind)
  expect_identical(check$status, "needs_attention")
})

test_that("configured reference paths stay anchored across working directories", {
  root <- withr::local_tempdir(); elsewhere <- withr::local_tempdir()
  dir.create(file.path(root, "refs")); saveRDS(data.frame(x = 1), file.path(root, "refs", "out.rds"))
  file <- review_source(root, "data adam.out; x=1; run;")
  yaml::write_yaml(list(libraries = list(adam = "."), outputs = list(references = list("adam.out" = "refs/out.rds"))),
    file.path(root, "_sas2r.yml"))
  withr::local_dir(elsewhere)
  check <- sas_preflight(file)
  expect_identical(check$references$status, "available")
  expect_true(assess_dataset_target(check$outputs, data.frame(x = 1))$reference_passed)
  expect_identical(check$destinations$report_json, migration_paths(check$destinations$root, "<run_id>")$report_json)
})

test_that("preflight and deterministic emission use identical failure reasons", {
  root <- withr::local_tempdir()
  for (expression in c("scan(name, 2)", "a @@ b")) {
    file <- review_source(root, paste("data out; set raw.dm; x=", expression, "; run;"))
    check <- sas_preflight(file)
    result <- sas_transpile(check$project, out_dir = file.path(root, "out"))
    expect_true(check$unsupported$reason[1L] %in% result$manifest$reason)
  }
})

test_that("all output check shapes have a usable report reason", {
  result <- assess_dataset_target(list(target_key = "adam.missing"), list(outputs_dir = withr::local_tempdir()))
  expect_match(output_checks_reason(result$checks), "candidate_exists")
  result <- assess_tlf_target(list(target_key = "table.pdf"), list(outputs_dir = withr::local_tempdir()))
  expect_match(output_checks_reason(result$checks), "candidate_exists")
  expect_identical(output_checks_reason(NULL), "No output checks available")
})

test_that("documentation grammar includes empty and knitr R fences", {
  blocks <- doc_code_blocks(c("  ```r   ", "  ```", "```{r setup, include=FALSE}", "x <- 1", "```"))
  expect_identical(vapply(blocks, `[[`, character(1), "code"), c("", "x <- 1"))
  expect_no_error(parse(text = blocks[[1L]]$code))
})

test_that("SAS collection driver roster matches the manifest", {
  driver <- test_path("..", "..", "tools", "generate-semantic-references.sas")
  skip_if_not(file.exists(driver), "SAS driver is a source-repository tool")
  lines <- readLines(driver)
  calls <- lines[grepl("^%semantic_case\\(", lines)]
  ids <- sub("^%semantic_case\\(([^)]+)\\);.*$", "\\1", calls)
  cases <- jsonlite::read_json(test_path("fixtures", "semantic-reference", "manifest.json"))
  expect_identical(ids, vapply(cases, `[[`, character(1), "id"))
})

test_that("target tolerances and NULL profile fields follow one inheritance rule", {
  p <- validate_output_overrides(list(profiles = list(subject = qc_profile(row_count = 2)),
    assertions = list("adam.adsl" = list(profile = "subject", row_count = NULL))))
  expect_equal(p$assertions[[1]]$row_count, 2)
  path <- withr::local_tempfile(fileext = ".rds"); saveRDS(data.frame(x = 1), path)
  contract <- list(target_key = "adam.adsl", reference_path = path,
    assertions = qc_profile(numeric_tolerance = 0))
  expect_false(assess_dataset_target(contract, data.frame(x = 1.5),
    list(tol_abs = 1, tol_rel = 1))$reference_passed)
})

test_that("explicit inline configuration does not inherit a discovered provider", {
  root <- withr::local_tempdir()
  file <- review_source(root, "data out; x=1; run;")
  yaml::write_yaml(list(llm = list(provider = "openai", model = "unused")), file.path(root, "_sas2r.yml"))
  expect_null(translation_config(file, list(libraries = list(raw = root)))$llm)
  expect_identical(translation_config(file, NULL)$llm$provider, "openai")
})
