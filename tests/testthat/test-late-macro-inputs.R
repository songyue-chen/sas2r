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
