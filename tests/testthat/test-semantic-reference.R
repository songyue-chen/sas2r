semantic_emit <- function(source) {
  units <- sas_units(sas_statements(source))
  units <- units[units$unit_type %in% c("data_step", "proc_step"), ]
  if (units$unit_type[1L] == "proc_step") return(emit_proc_sql(units))
  ir <- parse_data_step(units)
  if (ir$route == "merge") emit_merge_step(ir) else emit_data_step(ir)
}


test_that("semantic reference corpus preserves values or explicitly defers a whole unit", {
  root <- test_path("fixtures", "semantic-reference")
  cases <- jsonlite::read_json(file.path(root, "manifest.json"))
  for (case in cases) {
    dir <- file.path(root, case$id)
    code <- semantic_emit(paste(readLines(file.path(dir, "source.sas")), collapse = "\n"))
    expected <- semantic_frame(file.path(dir, "expected.csv"))
    if (case$status == "deferred") {
      expect_true(is.na(code$code), info = case$id)
      expect_true(length(code$flags) > 0L, info = case$id)
    } else {
      env <- new.env(parent = globalenv())
      sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), env)
      env$lib_read <- function(libref, member) semantic_frame(file.path(dir, paste0(member, ".csv")))
      actual <- NULL
      env$lib_write <- function(df, libref, member) { actual <<- df; invisible(df) }
      if (case$status == "runtime_deferred") {
        expect_error(eval(parse(text = code$code), env), "duplicate keys with shared non-key columns")
        expect_null(actual)
      } else {
        expect_no_error(eval(parse(text = code$code), env))
        expect_equal(as.data.frame(actual), expected, ignore_attr = TRUE, info = case$id)
        for (nm in intersect(names(expected), c("flag", "lt", "eq", "gt"))) {
          expect_type(actual[[nm]], "double")
        }
      }
    }
    # Optional real-SAS collection is an independent comparison, never generated
    # from our R implementation. Absence is documented rather than called parity.
    sas_reference <- file.path(root, "sas-generated", paste0(case$id, ".csv"))
    if (file.exists(sas_reference)) {
      expect_identical(semantic_reference_difference(sas_reference, file.path(dir, "expected.csv")),
                       TRUE, info = paste(case$id, "SAS reference"))
    }
  }
})

test_that("all variants of extra MERGE body statements defer", {
  bodies <- c("z = x + y;", "if x < 0 then delete;", "keep id;", "drop x;", "rename x=z;", "where x > 0;", "if ina; if inb;")
  for (body in bodies) {
    em <- semantic_emit(paste("data out; merge a(in=ina) b(in=inb); by id;", body, "run;"))
    expect_true(is.na(em$code), info = body)
    expect_identical(em$flags, "merge_body_deferred", info = body)
  }
})

test_that("SAS collection audit distinguishes missing, partial, failed and complete evidence", {
  root <- test_path("fixtures", "semantic-reference")
  generated <- withr::local_tempdir()
  audit <- semantic_reference_audit(root, generated)
  expect_identical(audit$status, "incomplete")
  expect_equal(sum(audit$coverage$status == "missing"), 14)
  cases <- jsonlite::read_json(file.path(root, "manifest.json"))
  for (case in cases) file.copy(file.path(root, case$id, "expected.csv"),
                                file.path(generated, paste0(case$id, ".csv")))
  # This deliberately synthetic collection exercises the verifier, not SAS.
  expect_identical(semantic_reference_audit(root, generated)$status, "incomplete")
  writeLines("synthetic provenance for verifier unit test", file.path(generated, "provenance.txt"))
  writeLines("synthetic log for verifier unit test", file.path(generated, "sas.log"))
  expect_identical(semantic_reference_audit(root, generated)$status, "passed")
  file <- file.path(generated, "missing_assignment.csv")
  csv <- readLines(file)
  writeLines(c("renamed,flag", csv[-1]), file)
  expect_identical(semantic_reference_audit(root, generated)$status, "failed")
  writeLines(csv, file)
  writeLines("ERROR: export failed", file.path(generated, "sas.log"))
  expect_identical(semantic_reference_audit(root, generated)$status, "failed")
  writeLines("synthetic log", file.path(generated, "sas.log"))
  writeLines(c("x,flag", "999,999"), file.path(generated, "missing_assignment.csv"))
  expect_identical(semantic_reference_audit(root, generated)$status, "failed")
})
