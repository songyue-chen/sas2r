test_that("public API has one translation workflow", {
  expected <- c(
    "sas_translate", "sas_code", "sas_write", "sas_config", "sas_llm",
    "sas_llm_models", "sas_llm_probe", "analyze_output_order",
    "as_digest_json", "compare_aligned_outputs", "compare_datasets",
    "compare_profile", "diff_digest", "passed", "read_comparison_report",
    "write_comparison_report"
  )
  expect_setequal(getNamespaceExports("sas2r"), expected)
})
