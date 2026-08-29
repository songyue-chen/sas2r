test_that("output inventory contains terminal datasets and actual TLF sites", {
  project <- output_contract_fixture_project()
  contracts <- infer_output_contracts(project)
  expect_setequal(contracts$kind, c("dataset", "tlf"))
  expect_true(all(c("adam.adsl", "reports/t14-1.pdf", "reports/l14-2.rtf",
                    "reports/f14-3.png") %in% contracts$target_key))
  expect_false("work.stage" %in% contracts$target_key)
})

test_that("dynamic TLF paths remain visible and unresolved", {
  contracts <- infer_output_contracts(dynamic_tlf_fixture_project())
  row <- contracts[contracts$resolution == "dynamic", ]
  expect_true(nrow(row) == 1L && nzchar(row$source_file) && row$line > 0L)
})

test_that("output-contract tibble has canonical 13 columns", {
  project <- output_contract_fixture_project()
  contracts <- infer_output_contracts(project)
  expected_cols <- c(
    "target_id", "target_key", "kind", "logical_name", "path_expression",
    "resolution", "producer_node_id", "required", "reference_path",
    "assertions", "source_file", "line", "reason"
  )
  expect_identical(names(contracts), expected_cols)
  expect_s3_class(contracts, "tbl_df")
})

test_that("character vector overrides classify TLF extensions and datasets", {
  project <- output_contract_fixture_project()
  overrides <- c("adam.adsl", "reports/t14-1.pdf", "reports/custom.html", "adam.adae")
  contracts <- infer_output_contracts(project, overrides = overrides)

  adsl_row <- contracts[contracts$target_key == "adam.adsl", ]
  expect_equal(nrow(adsl_row), 1L)
  expect_identical(adsl_row$kind, "dataset")
  expect_true(adsl_row$required)

  html_row <- contracts[contracts$target_key == "reports/custom.html", ]
  expect_equal(nrow(html_row), 1L)
  expect_identical(html_row$kind, "tlf")
  expect_true(html_row$required)

  adae_row <- contracts[contracts$target_key == "adam.adae", ]
  expect_equal(nrow(adae_row), 1L)
  expect_identical(adae_row$kind, "dataset")
  expect_true(adae_row$required)
})

test_that("exact structured named list overrides merge by target_key", {
  project <- output_contract_fixture_project()
  overrides <- list(
    datasets = c("adam.adsl"),
    tlfs = c("reports/t14-1.pdf"),
    references = c(
      "adam.adsl" = "/reference/adam/adsl.xpt",
      "reports/t14-1.pdf" = "/reference/t14-1.pdf"
    ),
    assertions = list(
      "adam.adsl" = list(
        required_columns = c("USUBJID"),
        keys = c("USUBJID"),
        numeric_tolerance = 1e-8
      ),
      "reports/t14-1.pdf" = list(required_text = c("Table 14.1"))
    )
  )

  contracts <- infer_output_contracts(project, overrides = overrides)

  adsl_row <- contracts[contracts$target_key == "adam.adsl", ]
  expect_equal(nrow(adsl_row), 1L)
  expect_identical(adsl_row$reference_path, "/reference/adam/adsl.xpt")
  expect_identical(adsl_row$assertions[[1]]$required_columns, c("USUBJID"))
  expect_equal(adsl_row$assertions[[1]]$numeric_tolerance, 1e-8)

  tlf_row <- contracts[contracts$target_key == "reports/t14-1.pdf", ]
  expect_equal(nrow(tlf_row), 1L)
  expect_identical(tlf_row$reference_path, "/reference/t14-1.pdf")
  expect_identical(tlf_row$assertions[[1]]$required_text, c("Table 14.1"))
})

test_that("unknown override fields are rejected with error", {
  project <- output_contract_fixture_project()
  bad_overrides <- list(
    datasets = c("adam.adsl"),
    invalid_field = 123
  )
  expect_error(
    infer_output_contracts(project, overrides = bad_overrides),
    class = "sas2r_output_contract_error"
  )
})

test_that("intermediate persistent datasets are excluded from contracts", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  code <- c(
    "data adam.adsl_temp; set sdtm.dm; run;",
    "data adam.adsl; set adam.adsl_temp; run;"
  )
  writeLines(code, file.path(dir, "inter.sas"))
  writeLines(c("libraries:", "  adam: data/adam"), file.path(dir, "_sas2r.yml"))
  p <- sas_project(dir)

  contracts <- infer_output_contracts(p)
  expect_true("adam.adsl" %in% contracts$target_key)
  expect_false("adam.adsl_temp" %in% contracts$target_key)
})

test_that("bare filerefs pointing to TLFs are resolved", {
  dir <- withr::local_tempdir()
  code <- c(
    "filename outpdf 'reports/listing.pdf';",
    "ods pdf file=outpdf;",
    "proc print data=adam.adsl; run;",
    "ods pdf close;"
  )
  writeLines(code, file.path(dir, "fileref.sas"))
  p <- sas_project(dir)

  contracts <- infer_output_contracts(p)
  expect_true("reports/listing.pdf" %in% contracts$target_key)
  expect_identical(contracts$kind[contracts$target_key == "reports/listing.pdf"], "tlf")
})

test_that("GOPTIONS and PROC EXPORT artifact targets are discovered", {
  dir <- withr::local_tempdir()
  code <- c(
    "filename myplot 'reports/plot.png';",
    "goptions gsfname=myplot;",
    "proc gplot data=adam.adsl; run;",
    "proc export data=adam.adsl outfile='reports/export.csv' dbms=csv replace; run;"
  )
  writeLines(code, file.path(dir, "export.sas"))
  p <- sas_project(dir)

  contracts <- infer_output_contracts(p)
  expect_true("reports/plot.png" %in% contracts$target_key)
  expect_true("reports/export.csv" %in% contracts$target_key)
})

test_that("build_dependency_graph integrates output contracts as final_output nodes and edges", {
  project <- output_contract_fixture_project()
  contracts <- infer_output_contracts(project)
  graph <- build_dependency_graph(project, output_contracts = contracts)

  out_nodes <- graph$nodes[graph$nodes$type == "final_output", ]
  expect_true(nrow(out_nodes) >= 4L)
  expect_true("adam.adsl" %in% out_nodes$component_id)
  expect_true("reports/t14-1.pdf" %in% out_nodes$component_id)

  out_edges <- graph$edges[graph$edges$to %in% out_nodes$node_id, ]
  expect_true(nrow(out_edges) >= 4L)
})

test_that("empty or NULL project inputs return canonical empty contracts tibble", {
  contracts <- infer_output_contracts(NULL)
  expect_equal(nrow(contracts), 0L)
  expect_identical(names(contracts), c(
    "target_id", "target_key", "kind", "logical_name", "path_expression",
    "resolution", "producer_node_id", "required", "reference_path",
    "assertions", "source_file", "line", "reason"
  ))
})
