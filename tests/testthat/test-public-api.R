test_that("public API has one translation workflow plus the runtime helpers", {
  workflow <- c(
    "sas_translate", "sas_preflight", "qc_profile", "sas_code", "sas_write", "sas_config", "sas_llm",
    "sas_llm_models", "sas_llm_probe", "analyze_output_order",
    "as_digest_json", "compare_aligned_outputs", "compare_datasets",
    "compare_profile", "diff_digest", "passed", "read_comparison_report",
    "write_comparison_report"
  )
  # The runtime every translated program carries is exported package code
  # since 0.2.0 (ADR 0003, phase 2): the user-facing helpers have help pages
  # and are callable at the console; the operators and the plumbing stay
  # internal but still ship in every bundle.
  runtime <- c(
    "lib_read", "lib_write", "sas2r_libname_assign", "sas2r_libname_clear",
    "sas2r_resolve_registry", "sas2r_fold_names", "chr_cmp", "sas_if_else", "sas_sum", "sas_mean",
    "sas_min", "sas_max", "sas_round", "sas_length", "sas_substr",
    "sas_compress", "sas_display", "sas_sort", "sas_merge", "apply_format",
    "sas_put", "sas2r_source_include"
  )
  expect_setequal(getNamespaceExports("sas2r"), c(workflow, runtime))
  expect_true(all(runtime %in% sas2r:::SAS2R_HELPER_NAMES))
  expect_false(any(c("%+%", "%notin%", "split_ds", "sas2r_registry_env",
                     "sas2r_lib_entry", "sas2r_lib_member_path",
                     "sas2r_libref_stop") %in% getNamespaceExports("sas2r")))
})
