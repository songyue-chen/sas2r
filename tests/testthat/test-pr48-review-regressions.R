test_that("ordinary execution errors retain technical facts without record values", {
  snippets <- c(
    function_not_found = 'sas_fake_helper(1)',
    object_not_found = 'zz + 1',
    non_numeric_operand = '1 + value_from_record',
    missing_boolean = 'if (NA) 1',
    empty_argument = 'if (logical()) 1',
    differing_row_counts = 'data.frame(a = 1:3, b = 1:2)',
    undefined_columns = 'data.frame(x = 1)[, "missing_col"]',
    subscript_out_of_bounds = 'list(1)[[3]]',
    unused_argument = 'round(1, digits = 2, extra = value_from_record)',
    missing_argument = 'f <- function(required) required; f()')
  env <- new.env()
  env$value_from_record <- "RECORD_VALUE_997711"
  for (kind in names(snippets)) {
    code <- snippets[[kind]]
    error <- tryCatch(eval(parse(text = code), env), error = identity)
    expect_s3_class(error, "error")
    for (condition in list(error, execution_condition(error))) {
      facts <- agent_error_facts(condition, code)
      expect_identical(facts$kind, kind, info = code)
      expect_false(grepl("RECORD_VALUE_997711", jsonlite::toJSON(facts, auto_unbox = TRUE)))
    }
  }
})

test_that("real dplyr conditions and serialized causes preserve useful names", {
  skip_if_not_installed("dplyr")
  snippets <- c(
    object_not_found = 'dplyr::mutate(data.frame(x = 1), y = zz)',
    column_not_found = 'dplyr::select(data.frame(x = 1), missing_col)',
    filter_not_logical = 'dplyr::filter(data.frame(x = 1), value_from_record)',
    incompatible_types = 'dplyr::bind_rows(data.frame(x = 1), data.frame(x = value_from_record))')
  env <- new.env()
  env$value_from_record <- "RECORD_VALUE_997711"
  for (kind in names(snippets)) {
    code <- snippets[[kind]]
    error <- tryCatch(eval(parse(text = code), env), error = identity)
    # Smoke records retain the outer formatted message; callr uses its cause.
    records <- list(error, execution_condition(error),
      list(message = conditionMessage(error), class = class(error), call = conditionCall(error)))
    for (condition in records) {
      facts <- agent_error_facts(condition, code)
      expect_identical(facts$kind, kind, info = code)
      expect_false(grepl("RECORD_VALUE_997711", jsonlite::toJSON(facts, auto_unbox = TRUE)))
      if (kind == "object_not_found") expect_identical(facts$identifiers, "zz")
      if (kind == "column_not_found") expect_identical(facts$identifiers, "missing_col")
    }
  }
})

test_that("real subprocess library errors retain classed facts and full local diagnostics", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  code <- 'x <- lib_read("work", "missing")'
  revisions <- list(p01 = list(component_id = "p01", r_code = code, revision_id = "r1"))
  plan <- build_program_smoke_plan(state$graph, "p01", revisions, execute = TRUE)
  prep <- prepare_program_smoke(state, plan, tempfile("missing-library-"))
  result <- run_program_smoke(prep$plan, prep$runtime, prep$attempt_dir)
  expect_false(result$passed)
  expect_match(result$condition$message, "\nSearched:", fixed = TRUE)
  expect_match(result$condition$message, "\nExecution root:", fixed = TRUE)
  expect_true("sas2r_dataset_not_found" %in% result$condition$class)
  facts <- bounded_agent_diagnostics(result, source_code = code)
  expect_identical(facts$condition_kind, "dataset_not_found")
  expect_identical(facts$condition_identifiers, "work.missing")
  expect_false(grepl("Searched:|Execution root:", facts$condition_message))
  # Incorrect helper call keeps its canonical contract, never the data value.
  value <- "RECORD_VALUE_997711"
  e <- tryCatch(lib_write(value, "work", "out"), error = identity)
  expect_s3_class(e, "sas2r_lib_write_arguments")
  facts <- agent_error_facts(execution_condition(e), 'lib_write(value, "work", "out")')
  expect_identical(facts$kind, "lib_write_arguments")
  expect_false(grepl(value, facts$condition_message, fixed = TRUE))
})

review_order_project <- function(programs, macros = list()) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "programs"))
  dir.create(file.path(root, "macros"))
  for (name in names(programs)) writeLines(programs[[name]], file.path(root, "programs", paste0(name, ".sas")))
  for (name in names(macros)) writeLines(macros[[name]], file.path(root, "macros", paste0(name, ".sas")))
  sas_project(file.path(root, "programs"), config = list(
    macro_search_path = file.path(root, "macros"),
    migration = list(execution_order = paste0(names(programs), ".sas"))))
}

test_that("deferred WORK candidates exclude unrelated setup programs and prefer nearest writers", {
  p <- review_order_project(list(
    a = "%setup; data work.stage; x=1; run;",
    b = "%setup; data work.unrelated; x=1; run;",
    c = "%setup; data work.stage; x=2; run;",
    d = "%setup; data work.out; set work.stage; run;"),
    list(setup = "%macro setup; %put NOTE: setup; options mprint; %mend;"))
  read <- component_read_context(p$graph, "d")
  expect_identical(read$reads[[1]]$writer_status, "deferred")
  expect_identical(read$possible, c("c", "a"))
  ids <- c("a", "b", "c", "d", "macro__setup")
  selected <- stats::setNames(lapply(ids, function(id) list(revision_id = "r1", r_code = paste("#", id))), ids)
  bodies <- agent_dependency_bodies(p, "d", selected)
  expect_setequal(names(bodies), c("a", "c", "macro__setup"))
  before <- build_agent_guidance(p, "d", selected_revisions = selected)
  expect_lt(match("c", before$selected_dependencies), match("a", before$selected_dependencies))
  selected$b$r_code <- "# unrelated edit"
  expect_identical(build_agent_guidance(p, "d", selected_revisions = selected)$identity, before$identity)
})

test_that("literal macro writes and unknown earlier effects remain possible without future writers", {
  p <- review_order_project(list(
    unrelated = "%setup; data work.other; x=1; run;",
    literal = "%literal;",
    dynamic = "%dynamic;",
    unresolved = "%missing;",
    include = '%include "absent.sas";',
    reader = "%setup; data work.out; set work.stage; run;",
    future = "%dynamic;"),
    list(setup = "%macro setup; %put NOTE: setup; %mend;",
      literal_macro = "%macro literal; data work.stage; x=1; run; %mend;",
      dynamic_macro = "%macro dynamic; data &target; x=1; run; %mend;"))
  read <- component_read_context(p$graph, "reader")
  expect_identical(read$possible, c("include", "unresolved", "dynamic", "literal"))
  expect_identical(read$reads[[1]]$writer_status, "deferred")
})

test_that("large ordered studies retain the immediate possible writer in the packet", {
  n <- 200L
  programs <- stats::setNames(lapply(seq_len(n), function(i) paste0("%setup; data work.p", i, "; ",
    if (i > 1L) paste0("set work.p", i - 1L, "; "), "x=1; run;")), sprintf("p%03d", seq_len(n)))
  p <- review_order_project(programs, list(setup = "%macro setup; %put NOTE: setup; %mend;"))
  ids <- c(names(programs), "macro__setup")
  selected <- stats::setNames(lapply(ids, function(id) list(revision_id = "r1",
    r_code = paste(rep(paste0("x_", id, " <- 1"), 20L), collapse = "\n"))), ids)
  expect_setequal(names(agent_dependency_bodies(p, "p200", selected)), c("p199", "macro__setup"))
  packet <- build_agent_guidance(p, "p200", selected_revisions = selected)
  expect_true("p199" %in% packet$selected_dependencies)
  expect_lt(nchar(packet$text), 24000L)
})

test_that("a local possible writer does not add the current program as its own dependency", {
  p <- review_order_project(list(main = "%emit; data work.out; set work.stage; run;"),
    list(emitter = "%macro emit; data work.stage; x=1; run; %mend;"))
  facts <- component_read_context(p$graph, "main")
  expect_identical(facts$reads[[1]]$writer_status, "deferred")
  expect_length(facts$possible, 0L)
  packet <- build_agent_guidance(p, "main")
  expect_false("main" %in% packet$selected_dependencies)
  expect_true("macro__emit" %in% packet$selected_dependencies)
})

test_that("investigation projection contains categories rather than reference counts", {
  target <- discrepancy_fixture()
  metrics <- target$checks$reference_comparison$summary
  metrics$value[metrics$metric == "rows_base"] <- 254
  metrics$value[metrics$metric == "rows_comp"] <- 266
  metrics$value[metrics$metric == "rows_matched"] <- 254
  target$checks$reference_comparison$summary <- metrics
  summary <- reviewer_discrepancy_summary(target)
  expect_identical(summary$rows, "generated_more")
  expect_identical(summary$row_pairing, "partial")
  expect_false(any(vapply(unlist(summary, recursive = FALSE), is.numeric, logical(1))))
  numeric_leaves <- function(x) if (is.list(x)) any(vapply(x, numeric_leaves, logical(1))) else is.numeric(x)
  expect_false(numeric_leaves(summary))
  packet <- jsonlite::toJSON(summary, auto_unbox = TRUE)
  expect_false(grepl("254|266|rows_aligned|value_mismatches|compared|missing_differences", packet))
  expect_equal(dataset_discrepancy_summary(target)$reference$rows, 254)
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.result = target))
  requests <- list()
  state$reviewer_llm <- diagnosis_mock(list(explanations = list()),
    callback = function(request) requests[[length(requests) + 1L]] <<- request)
  migration_report_diagnosis(state)
  expect_false(grepl("254|266|value_mismatches|rows_aligned", request_task_text(requests[[1]])))
})

test_that("source chain facts ignore assignments and separate comparisons", {
  p <- review_order_project(list(main = c("data work.out;", "set work.in;",
    "flag = (x=1 and y=2);", "if x=1 then y=2;", "if (0<x)<5 then ok=1;",
    "if 0<x<=5 then chain=1;", "g = (a=b=c);", "if a=b=c then h=1;",
    "if 0<x<5<10 then long=1;", "run;")))
  facts <- paste(source_comparison_context(p, "main"), collapse = "\n")
  expect_match(facts, "if 0<x<=5 then chain=1", fixed = TRUE)
  expect_match(facts, "g = (a=b=c)", fixed = TRUE)
  expect_match(facts, "if a=b=c then h=1", fixed = TRUE)
  expect_match(facts, "if 0<x<5<10", fixed = TRUE)
  expect_false(grepl("flag =|if x=1 then y=2|if \\(0<x\\)<5", facts))
})

test_that("reports expose partial explanation coverage and group by warning kind", {
  fx <- repair_workflow_fixture(n = 1L, failures = integer())
  state <- fx$state
  state$assessment <- list(targets = list(work.first = discrepancy_fixture(), work.second = discrepancy_fixture()))
  state$histories$p01 <- record_completed_review(state$histories$p01, verdict = "repair_required",
    findings = list(list(severity = "material", affected_outputs = "work.first",
      sas_evidence = "x=1", r_evidence = "x <- 2")))
  result <- migration_report_diagnosis(state)
  expect_match(result$reason, "1 of 2 unresolved targets", fixed = TRUE)
  expect_length(state$reviewer_llm$requests(), 0L)
  groups <- run_advisory_groups(c(
    "input_missing: supply the input or correct its producer.",
    "macro_data_flow_deferred: review and execution must assess it.",
    "source_dependency: missing macro; review this source."))
  expect_match(groups[["Inputs and readiness"]], "input_missing", fixed = TRUE)
  expect_match(groups[["Dependencies"]], "macro_data_flow_deferred", fixed = TRUE)
  expect_false("Source reviews" %in% names(groups))
})
