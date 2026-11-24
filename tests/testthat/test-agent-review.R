test_that("reviewer validates a closed finding schema", {
  spec <- load_agent_specs()$reviewer
  expect_false(is.null(spec))
  expect_identical(spec$output_schema, "program_review_v1")
  valid <- sas2r:::validate_output(good_review()$data, spec$output_schema)
  expect_true(valid$ok)

  invalid <- good_review()$data
  invalid$verdict <- "approve"
  expect_false(sas2r:::validate_output(invalid, spec$output_schema)$ok)

  invalid_status <- good_review()$data
  invalid_status$static_runnability <- "approved"
  expect_false(sas2r:::validate_output(invalid_status, spec$output_schema)$ok)

  invalid_conf <- good_review(status = "issue_found", codes = "sas_default_mismatch")$data
  invalid_conf$findings[[1]]$confidence <- 1.5
  expect_false(sas2r:::validate_output(invalid_conf, spec$output_schema)$ok)

  invalid_extra <- good_review()$data
  invalid_extra$extra_field <- "not allowed"
  expect_false(sas2r:::validate_output(invalid_extra, spec$output_schema)$ok)

  missing_field <- good_review()$data
  missing_field$verdict <- NULL
  expect_false(sas2r:::validate_output(missing_field, spec$output_schema)$ok)
})

test_that("reviewer spec tools are restricted and do not modify files or read datasets", {
  specs <- load_agent_specs()
  rev_spec <- specs$reviewer
  expect_false(is.null(rev_spec))
  # read_unit_context was removed: the reviewer prompt renders {{unit}} and
  # {{context}}, built from the same objects the tool reads, so it could only
  # repeat what the prompt already carried.
  allowed_tools <- c(
    "query_project_graph", "lookup_rulebook", "read_dependency_context", "search_skills", "read_skill"
  )
  expect_setequal(names(rev_spec$tools), allowed_tools)
  expect_null(rev_spec$tools$read_diff_evidence)
  expect_null(rev_spec$tools$lookup_cookbook)
  expect_null(rev_spec$tools$find_macro)
  expect_null(rev_spec$tools$get_macro_source)
  expect_null(rev_spec$tools$list_macro_files)
  expect_null(rev_spec$tools$search_docs)
})

test_that("unit_proc_names reads the proc off a PROC unit whose first_token is bare", {
  dir <- withr::local_tempdir()
  writeLines("proc sort data=work.a out=work.b; by x; run;", file.path(dir, "p.sas"))
  p <- sas_project(dir)
  uid <- p$statements$unit_id[1]
  us <- p$statements[p$statements$unit_id == uid, ]
  # The bare token is what every routing site sees; the proc name is only in
  # the statement text.
  expect_identical(us$first_token[1], "proc")
  expect_identical(unit_proc_names(us), "sort")
})

test_that("unit_proc_names is empty for a unit that runs no proc", {
  dir <- withr::local_tempdir()
  writeLines("data work.b; set work.a; x = 1; run;", file.path(dir, "p.sas"))
  p <- sas_project(dir)
  us <- p$statements[p$statements$unit_id == p$statements$unit_id[1], ]
  expect_identical(unit_proc_names(us), character(0))
})

test_that("unit_proc_names prefers an extractor-supplied proc column", {
  us <- tibble::tibble(
    unit_id = c(1L, 1L),
    unit_type = c("proc_step", "proc_step"),
    first_token = c("proc", "by"),
    text = c("proc sort data=work.a", "by x"),
    proc = c("SORT", NA_character_)
  )
  expect_identical(unit_proc_names(us), "sort")
})

test_that("program-level review routes ordering skills and gets a working tool context", {
  dir <- withr::local_tempdir()
  sas <- "proc sort data=adam.adsl out=adam.srt; by usubjid; run;"
  writeLines(sas, file.path(dir, "prog.sas"))
  proj <- sas_project(dir)

  captured <- list()
  llm <- recording_reviewer(function(context) {
    captured[[length(captured) + 1L]] <<- context
    valid_program_review_response(verdict = "reviewed_no_material_finding")
  })
  revision <- list(
    component_id = "prog", revision_id = "r1",
    r_code = "srt <- sas_sort(sas2r_fold_names(lib_read('adam','adsl')), by = 'usubjid')",
    contract = list(component_id = "prog", sas_text = sas)
  )
  res <- review_program_revision(
    revision = revision,
    context = list(component_id = "prog", project = proj, config = list()),
    llm = llm,
    paths = init_migration_paths(withr::local_tempdir())
  )

  expect_true(length(captured) > 0L)
  sys_txt <- request_task_text(captured[[1L]])
  expect_match(sys_txt, "sas-missing-sort-semantics")

  # query_project_graph must answer from the project lineage, not emptiness.
  tools <- captured[[1L]]$request$tools
  qpg <- NULL
  for (t in tools) if (identical(t$name, "query_project_graph")) qpg <- t
  expect_false(is.null(qpg))
  ans <- qpg$call(list(dataset = "adam.adsl"))
  expect_true(length(ans) > 0L)
})
