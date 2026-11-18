test_that("a declared producer-first order runs a caller that has a setup macro first", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  dir.create("input")
  writeLines(c("%macro prep();", "data work.tmp; x=1; run;", "%mend;",
    "%macro copy(indata=);", "data work.out; set &indata; run;", "%mend;",
    "%prep();", "%copy(indata=raw.member);"), "a_consumer.sas")
  writeLines("data raw.member; value=2; run;", "z_producer.sas")
  responses <- list("translator:a_consumer" = valid_program_translation_response(
    paste("prep <- function() lib_write(data.frame(x = 1), 'work', 'tmp')",
      "copy <- function(indata = '') {", "  parts <- strsplit(indata, '.', fixed = TRUE)[[1L]]",
      "  lib_write(lib_read(parts[1L], parts[2L]), 'work', 'out')", "}", "prep()",
      "copy(indata = 'raw.member')", sep = "\n"), discovered_dependencies = "raw.member"),
    "translator:z_producer" = valid_program_translation_response(
      "lib_write(data.frame(value=2), 'raw', 'member')"), reviewer = valid_program_review_response())
  result <- sas_translate(root, out_dir = withr::local_tempdir(),
    config = list(libraries = list(raw = list(path = "input", engine = "rds")),
      migration = list(execution_order = c("z_producer.sas", "a_consumer.sas"))),
    outputs = list(datasets = c("work.out", "raw.member")),
    llm = parallel_test_llm(responses, delay = 0), max_program_repair_rounds = 0L,
    max_bundle_repair_rounds = 0L)
  expect_identical(result$diagnostics$dependency_findings$a_consumer$advisory, "raw.member")
  expect_null(result$diagnostics$deferred_components$a_consumer)
  expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "work", "out.rds"))$value, 2)
  expect_equal(readRDS(file.path(result$outputs_dir, "datasets", "raw", "member.rds"))$value, 2)
})

test_that("late inputs use the order and binding at each invocation", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  dir.create("input")
  writer <- "data raw.member; value=2; run;"
  writeLines(writer, "producer.sas")
  writeLines(c("%macro prep();", "data work.tmp; x=1; run;", "%mend;", "%prep();"), "setup.sas")
  reader <- c("%macro consume(indata=raw.member);", "data work.out; set &indata; run;", "%mend;")
  order <- c("producer.sas", "setup.sas", "caller.sas")
  project <- function(order) sas_project(root, config = list(
    libraries = list(raw = list(path = "input", engine = "rds")),
    migration = list(execution_order = order)))
  status <- function(p) unname(late_dataset_availability(p, "caller", "raw.member"))

  writeLines(c(reader, "%consume();"), "caller.sas")
  expect_identical(status(project(order)), "project_writer_needs_execution")
  early <- project(rev(order))
  expect_identical(status(early), "member_unavailable_in_bound_library")
  saveRDS(data.frame(value = 1), "input/member.rds")
  expect_identical(status(early), "available_in_bound_library")
  unlink("input/member.rds")
  writeLines(c(reader, "%consume(indata=work.alternate);"), "caller.sas")
  expect_identical(status(project(rev(order))), "scanned_dataset_reference")

  writeLines(c(reader, "%consume(", "  indata=work.alternate", ");"), "caller.sas")
  expect_identical(status(project(rev(order))), "scanned_dataset_reference")
  reader[1L] <- "%macro consume(id,indata=raw.member);"
  writeLines(c(reader, "%consume(1,indata=work.alternate);"), "caller.sas")
  expect_identical(status(project(rev(order))), "scanned_dataset_reference")

  # A non-first statement and an intervening macro both need runtime evidence.
  call <- "%consume(indata=raw.member);"
  reader[1L] <- "%macro consume(indata=);"
  writeLines(c(reader, "proc print data=sashelp.class;", call, "run;"), "caller.sas")
  expect_identical(status(project(order)), "project_writer_needs_execution")
  writeLines(c(reader, call), "caller.sas")
  p <- project(order)
  rebuilt <- p
  rebuilt$late_order <- NULL
  expect_identical(status(rebuilt), status(p))
  writeLines(c(writer, "proc datasets library=raw; delete other; quit;"), "producer.sas")
  expect_identical(status(project(order)), "project_writer_needs_execution")
  writeLines(c(writer, "proc datasets lib=raw; run; delete member; run; quit;"), "producer.sas")
  expect_identical(status(project(order)), "project_writer_order_unresolved")
  # Two component reconciliations must be able to reuse the scanned chronology.
  local_mocked_bindings(ordered_dataset_producers = function(...) stop("unexpected source walk"))
  expect_identical(status(p), "project_writer_needs_execution")
  expect_identical(status(p), "project_writer_needs_execution")
})
