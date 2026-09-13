population_fixture <- function(sas, inputs, envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  writeLines(sas, file.path(root, "arbitrary.sas"))
  project <- sas_project(root)
  specs <- source_population_specs(project, "arbitrary")$arbitrary
  env <- new.env()
  env$lib_read <- function(libref, member, ...) inputs[[paste(libref, member, sep = ".")]]
  env$lib_write <- function(df, libref, member, ...) invisible(df)
  observer <- observe_source_population(specs, env)
  list(specs = specs, env = env, observer = observer)
}

test_that("SET row preservation catches lost records but allows source filters", {
  input <- data.frame(id = 1:5)
  fx <- population_fixture("data work.out; set raw.source; value = id + 1; run;", list(raw.source = input))
  expect_error(fx$env$lib_write(input[1:4, , drop = FALSE], "work", "out"), "expected 5 rows, got 4", class = "sas2r_population_mismatch")
  fx$env$lib_write(input[5:1, , drop = FALSE], "work", "out")
  expect_identical(fx$observer$finish()[[1]]$status, "passed")
  for (body in c("where id > 2;", "if id = 2 then delete;", "output; output;", "retain total 0;")) {
    fx <- population_fixture(paste("data work.out; set raw.source;", body, "run;"), list(raw.source = input))
    expect_identical(fx$specs[[1]]$status, "unverified")
    expect_no_error(fx$env$lib_write(input[1, , drop = FALSE], "work", "out"))
    expect_identical(fx$observer$finish()[[1]]$status, "unverified")
  }
})

test_that("MERGE preserves repeated events and uses the SAS IN filter", {
  inputs <- list(raw.subjects = data.frame(id = c(1, 2)),
                 raw.events = data.frame(id = c(1, 1, 1, 3), event = 1:4))
  # Expected populations are hand-written, never computed with sas_merge.
  filters <- c("", "if a;", "if b;", "if a and b;", "if a and not b;", "if b and not a;")
  expected <- list(c(1, 1, 1, 2, 3), c(1, 1, 1, 2), c(1, 1, 1, 3), c(1, 1, 1), 2, 3)
  for (i in seq_along(filters)) {
    fx <- population_fixture(paste("data work.out; merge raw.subjects(in=a) raw.events(in=b); by id;", filters[i], "run;"), inputs)
    expect_no_error(fx$env$lib_write(data.frame(id = expected[[i]]), "work", "out"))
    expect_identical(fx$observer$finish()[[1]]$status, "passed")
  }
  fx <- population_fixture("data work.out; merge raw.subjects(in=a) raw.events(in=b); by id; if a and b; run;", inputs)
  expect_error(fx$env$lib_write(data.frame(id = 1), "work", "out"), "expected 3 rows, got 1")
  # Same total count is insufficient when a subject's records are replaced.
  expect_error(fx$env$lib_write(data.frame(id = c(1, 1, 2)), "work", "out"), "mismatched BY groups: 2")
})


test_that("BY counts handle missing, compound keys, and empty populations", {
  inputs <- list(raw.subjects = data.frame(id = c(NA, 1, 1), visit = c(1, 1, 2)),
                 raw.events = data.frame(id = c(NA, NA, 1), visit = c(1, 1, 2)))
  fx <- population_fixture("data work.out; merge raw.subjects(in=a) raw.events(in=b); by id visit; if a and b; run;", inputs)
  expect_no_error(fx$env$lib_write(inputs$raw.events, "work", "out"))
  expect_identical(fx$observer$finish()[[1]]$expected_rows, 3L)
  empty <- list(raw.subjects = data.frame(id = numeric()), raw.events = data.frame(id = numeric()))
  fx <- population_fixture("data work.out; merge raw.subjects raw.events; by id; run;", empty)
  expect_no_error(fx$env$lib_write(data.frame(id = numeric()), "work", "out"))
  expect_identical(fx$observer$finish()[[1]]$status, "passed")
})

test_that("inlined, repeated, and unsupported writes remain visibly unverified", {
  fx <- population_fixture("data work.out; set work.intermediate; run;", list())
  fx$env$lib_write(data.frame(id = 1:3), "work", "out")
  expect_identical(fx$observer$finish()[[1]]$reason, "source_input_unavailable")
  fx <- population_fixture(c("data work.out; set raw.source; run;",
    "proc sort data=work.out nodupkey; by id; run;"), list(raw.source = data.frame(id = c(1, 1))))
  expect_identical(fx$specs[[1]]$status, "unverified")
  expect_no_error(fx$env$lib_write(data.frame(id = 1), "work", "out"))
})

test_that("stale intermediates do not become source population evidence", {
  fx <- population_fixture(c("data work.stage; set raw.source; run;",
    "data work.out; set work.stage; run;"),
    list(raw.source = data.frame(id = 1:3), work.stage = data.frame(id = 1)))
  fx$env$lib_write(data.frame(id = 1), "work", "out")
  expect_identical(fx$observer$finish()[[2]]$reason, "source_input_unavailable")
  fx$env$lib_write(data.frame(id = 1:3), "work", "stage")
  expect_error(fx$env$lib_write(data.frame(id = 1), "work", "out"), "expected 3 rows, got 1")
})
