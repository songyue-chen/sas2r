semantic_emit <- function(source) {
  units <- sas_units(sas_statements(source))
  units <- units[units$unit_type %in% c("data_step", "proc_step"), ]
  if (units$unit_type[1L] == "proc_step") return(emit_proc_sql(units))
  ir <- parse_data_step(units)
  if (ir$route == "merge") emit_merge_step(ir) else emit_data_step(ir)
}

semantic_frame <- function(path) {
  frame <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                          na.strings = "", blank.lines.skip = FALSE)
  # All-missing numeric columns otherwise get inferred as logical by read.csv.
  for (nm in names(frame)) if (is.logical(frame[[nm]]) && all(is.na(frame[[nm]]))) frame[[nm]] <- as.numeric(frame[[nm]])
  frame
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
      reference <- semantic_frame(sas_reference)
      names(reference) <- tolower(names(reference))
      expect_equal(reference, expected, ignore_attr = TRUE, info = paste(case$id, "SAS reference"))
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
