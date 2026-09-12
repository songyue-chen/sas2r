test_that("unbound producer ordering is retained without claiming available inputs", {
  root <- withr::local_tempdir()
  writeLines("data out; set undeclared.stage; run;", file.path(root, "a_use.sas"))
  writeLines("data undeclared.stage; x=1; run;", file.path(root, "b_make.sas"))
  check <- sas_preflight(root)
  expect_identical(check$schedule$component_id, c("b_make", "a_use"))
  expect_identical(check$inputs$status, "unresolved")
  expect_identical(check$status, "needs_attention")
})

test_that("invoked macro bodies are deferred while uncalled definitions stay inactive", {
  root <- withr::local_tempdir()
  definition <- "%macro mk(ds); data out; set &ds; run; %mend;"
  uncalled <- sas_preflight(review_source(root, definition))
  expect_identical(uncalled$status, "ready_for_translation")
  called <- sas_preflight(review_source(root, paste(definition, "%mk(raw.dm);")))
  expect_identical(called$status, "needs_attention")
  expect_true("macro_data_flow_deferred" %in% called$findings$kind)
  dir.create(file.path(root, "inc"))
  writeLines(definition, file.path(root, "inc", "macros.sas"))
  included <- sas_preflight(review_source(root, "%include 'inc/macros.sas'; %mk(raw.dm);"))
  expect_true("macro_data_flow_deferred" %in% included$findings$kind)
})

test_that("unsupported DATA-step dataset positions remain visible as deferred", {
  root <- withr::local_tempdir()
  for (stmt in c("if _n_ = 1 then set raw.totals;", "else set &d;", "getit: set raw.b;")) {
    check <- sas_preflight(review_source(root, paste("data out;", stmt, "run;")))
    expect_identical(check$status, "needs_attention", info = stmt)
    expect_true("dataset_statement_deferred" %in% check$findings$kind, info = stmt)
  }
  literal <- sas_preflight(review_source(root, "data 'out ds'n; set 'my data'n; run;"))
  expect_false("work.n" %in% literal$project$lineage$dataset)
  expect_true("dataset_statement_deferred" %in% literal$findings$kind)
})

test_that("PROC options share one tokenizer and APPEND can create its base", {
  root <- withr::local_tempdir()
  code <- "data new; x=1; run; proc append base=work.all data=work.new; run;"
  check <- sas_preflight(review_source(root, code))
  expect_identical(check$inputs$dataset, c("work.new", "work.all"))
  expect_identical(check$inputs$status, c("generated", "created_if_missing"))
  expect_true("work.all" %in% check$project$lineage$dataset[check$project$lineage$role == "creates"])
  expect_identical(check$status, "ready_for_translation")
  refs <- extract_dataset_refs(sas_units(sas_statements("proc compare base=a compare=b; run;")))
  expect_setequal(refs$dataset[refs$role == "reads"], c("work.a", "work.b"))
  expect_identical(eq_captures("proc sort data = raw.dm out = sorted;", "data"), "raw.dm")
  expect_identical(eq_captures("proc sort data=a(rename=(out=newout)) out=b;", "out"), "b")
  copy <- sas_preflight(review_source(root, "proc copy in=&lib out=work; run;"))
  expect_false("work.work" %in% copy$project$lineage$dataset)
  expect_true("dataset_statement_deferred" %in% copy$findings$kind)
  writeLines("proc append base=work.all data=work.new; run;", file.path(root, "a_append.sas"))
  writeLines("data all; x=1; run; data new; x=2; run;", file.path(root, "b_make.sas"))
  unlink(file.path(root, "main.sas"))
  expect_identical(sas_preflight(root)$schedule$component_id, c("b_make", "a_append"))
})

test_that("a bundle executes nested include modules once at their include sites", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inc"))
  saveRDS(data.frame(x = 1), file.path(root, "src.rds"))
  writeLines("data stage; set raw.src; x=x+1; run;", file.path(root, "inc", "prep.sas"))
  file <- review_source(root, "%include 'inc/prep.sas'; data out; set stage; x=x+1; run;")
  result <- sas_translate(file, execute = TRUE, outputs = "work.out",
    config = list(libraries = list(raw = list(path = root, engine = "rds"))))
  expect_equal(readRDS(file.path(result$outputs_dir, "work", "out.rds"))$x, 3)
  plan <- build_bundle_execution_plan(result$project$graph)
  expect_identical(plan$root_programs, "main")
  expect_true("prep" %in% plan$included_modules)
})
