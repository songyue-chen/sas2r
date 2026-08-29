attempt_runtime_fixture <- function(envir = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = envir)
  input_dir <- file.path(base, "inputs", "adam")
  dir.create(input_dir, recursive = TRUE)
  input_file <- file.path(input_dir, "adsl.rds")
  saveRDS(data.frame(USUBJID = "01"), input_file)


  candidate_root <- file.path(base, "candidate_root")
  staged_dir <- file.path(base, "staged")
  dir.create(staged_dir, recursive = TRUE)

  prog_file <- file.path(base, "p.sas")
  writeLines("data work.x; set adam.adsl; run;", prog_file)
  project <- sas_project(prog_file, config = list(libraries = list(adam = input_dir)))

  lib_map <- build_attempt_library_map(project, candidate_root)
  write_registry(project, staged_dir, library_map = lib_map)
  write_helpers(staged_dir)
  before_objs <- ls(envir = globalenv(), all.names = TRUE)
  withr::defer({
    after_objs <- ls(envir = globalenv(), all.names = TRUE)
    new_objs <- setdiff(after_objs, before_objs)
    if (length(new_objs) > 0L) {
      rm(list = new_objs, envir = globalenv())
    }
  }, envir = envir)


  list(



    base = base,
    input_file = input_file,
    input_dir = input_dir,
    candidate_root = candidate_root,
    staged_dir = staged_dir,
    registry = file.path(staged_dir, "_sas2r_registry.R"),
    helpers = file.path(staged_dir, "sas2r-helpers.R")
  )
}

test_that("attempt runtime uses candidate-first copy-on-write libraries", {
  fx <- attempt_runtime_fixture()
  before <- unname(tools::md5sum(fx$input_file))
  source(fx$registry)
  source(fx$helpers)

  original <- lib_read("adam", "adsl")
  changed <- transform(original, TRT = "candidate")
  lib_write(changed, "adam", "adsl")

  expect_identical(lib_read("adam", "adsl")$TRT, "candidate")
  expect_identical(unname(tools::md5sum(fx$input_file)), before)
  expect_true(file.exists(file.path(fx$candidate_root, "adam", "adsl.rds")))
  expect_identical(split_ds("&plotds", macro_vars = c(plotds = "plot_ready")),
                   c(lib = "work", member = "plot_ready"))
})
