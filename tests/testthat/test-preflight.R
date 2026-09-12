test_that("preflight finds real files and missing inputs without model calls or writes", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "raw"))
  # Deliberately not a readable dataset: preflight must only check its path.
  writeLines("directory discovery only", file.path(root, "raw", "dm.rds"))
  writeLines(c("data work.stage; set raw.dm; age=age+1; run;",
               "data adam.adsl; set work.stage; run;",
               "data adam.adae; set raw.ae; retain x; run;"), file.path(root, "main.sas"))
  yaml::write_yaml(list(libraries = list(raw = "raw", adam = "adam"),
    llm = list(provider = "openai", model = "unused")), file.path(root, "_sas2r.yml"))
  testthat::local_mocked_bindings(
    sas_llm = function(...) stop("adapter must not be constructed"),
    lib_read = function(...) stop("datasets must not be read"),
    sas_translate = function(...) stop("translation must not run"))
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  check <- sas_preflight(root, out_dir = file.path(root, "migration"),
                         budget_usd = 5, usage_limits = list(max_calls = 12, max_request_bytes = 10000))
  expect_s3_class(check, "sas2r_preflight")
  expect_equal(check$model_calls, 0)
  expect_identical(check$inputs$status[check$inputs$dataset == "raw.dm"], "available")
  expect_identical(check$inputs$status[check$inputs$dataset == "raw.ae"], "missing")
  expect_identical(check$inputs$status[check$inputs$dataset == "work.stage"], "generated")
  expect_match(check$inputs$path[check$inputs$dataset == "raw.dm"], "raw/dm.rds$")
  expect_equal(check$budget$max_usd, 5)
  expect_equal(check$budget$max_calls, 12)
  expect_identical(check$budget$mode, "strict")
  expect_true(any(grepl("retain", check$unsupported$reason)))
  expect_setequal(check$outputs$target_key, c("adam.adsl", "adam.adae"))
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE), before)
  expect_identical(check$status, "needs_attention")
  expect_message(print(check), "0 model calls")
})

test_that("preflight keeps source binding changes and unresolved includes visible", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "first")); dir.create(file.path(root, "second"))
  saveRDS(data.frame(x = 1), file.path(root, "first", "dm.rds"))
  writeLines(c("libname raw 'first'; data a; set raw.dm; run;",
               "libname raw 'second'; data b; set raw.dm; run;",
               "%include 'absent.sas';"), file.path(root, "main.sas"))
  check <- sas_preflight(root)
  rows <- check$inputs[check$inputs$dataset == "raw.dm", ]
  expect_identical(rows$status, c("available", "missing"))
  expect_true(any(check$findings$kind == "unresolved_include"))
  expect_true(all(c("selected_path", "selection_origin", "status") %in% names(check$libraries)))
  expect_equal(nrow(check$unsupported), 0)
})

test_that("preflight and translation share budget validation and defaults", {
  root <- withr::local_tempdir()
  writeLines("data out; set absent.dm; run;", file.path(root, "main.sas"))
  check <- sas_preflight(root)
  expect_identical(check$inputs$status, "unresolved")
  expect_identical(check$budget$mode, "observe")
  expect_true(is.infinite(check$budget$max_calls))
  expect_error(sas_preflight(root, usage_limits = list(max_call = 2)), "usage_limits")
  expect_error(sas_preflight(root, budget_mode = "typo"), "budget_mode")
  expect_error(sas_translate(root, out_dir = file.path(root, "out"), budget_mode = "typo"), "budget_mode")
})

test_that("preflight reports missing references and rejects misspelled config paths", {
  root <- withr::local_tempdir()
  source <- file.path(root, "main.sas")
  writeLines("data out; set raw.dm; run;", source)
  ref <- file.path(root, "ref.rds")
  check <- sas_preflight(source, outputs = list(references = list("work.out" = ref)))
  expect_identical(check$references$status, "missing")
  expect_true(any(grepl("reference files", check$next_actions)))
  expect_identical(check$destinations$generated_outputs,
                   file.path(check$destinations$root, "<run_id>", "generated-outputs"))
  expect_error(sas_preflight(source, config = file.path(root, "typo.yml")), "Configuration file not found")
})

test_that("preflight does not treat a conditional library as an available input", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "raw"))
  saveRDS(data.frame(x = 1), file.path(root, "raw", "dm.rds"))
  writeLines(c("%macro never_called;", "libname raw 'raw';", "%mend;",
               "data out; set raw.dm; run;"), file.path(root, "main.sas"))
  check <- sas_preflight(root)
  expect_identical(check$inputs$status, "unresolved")
  expect_true(any(check$libraries$status == "conditionally_bound"))
  expect_true(any(check$unsupported$reason == "macro_deferred"))
})
