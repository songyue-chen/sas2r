test_that("named formats work through both helpers in caller scope", {
  catalog <- list(yn = list(values = c("0" = "NO", "1" = "YES")),
                  "$yn" = list(values = c("0" = "zero", "1" = "one")))
  bundled <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), bundled)
  for (helpers in list(list(apply_format = apply_format, sas_put = sas_put), bundled)) {
    scope <- new.env(parent = globalenv())
    scope$.sas2r_formats <- catalog
    scope$format <- helpers$apply_format
    scope$put <- helpers$sas_put
    expect_identical(evalq(format(c(0, 1), "YN."), scope), c("NO", "YES"))
    expect_identical(evalq(put(c("0", "1"), "$YN."), scope), c("zero", "one"))
    expect_identical(evalq(local({ f <- function(x) put(x, "yn"); f(c(1, 0)) }), scope),
                     c("YES", "NO"))
    expect_identical(helpers$sas_put(c(0, 1), "yn", catalog = catalog), c("NO", "YES"))
    expect_identical(helpers$sas_put(c(0, 1), catalog$yn), c("NO", "YES"))
    expect_identical(helpers$sas_put(c(1, NA), NULL), c("1", NA_character_))
    expect_error(helpers$sas_put(1, "absent", catalog = catalog), "SAS format not found.*absent")
    expect_error(helpers$sas_put(1, "$yn", catalog = catalog["yn"]), "SAS format not found")
    expect_error(helpers$sas_put(1, "yn", catalog = catalog["$yn"]), "SAS format not found")
    expect_error(helpers$sas_put(1, "yn", catalog = NULL), "SAS format not found")
    expect_error(helpers$sas_put(1, NA_character_, catalog = catalog), "one non-empty string")
    for (name in c("yn3.", "yn3.2", "$yn8."))
      expect_error(helpers$sas_put(1, name, catalog = catalog),
        "format widths and decimal specifications are not supported")
  }
})

test_that("named formats execute correctly in a normalized smoke runtime", {
  root <- withr::local_tempdir()
  writeLines("proc format; value yn 0='NO' 1='YES'; run;", file.path(root, "autoexec.sas"))
  writeLines("data work.result; flag=put(1,yn.); run;", file.path(root, "main.sas"))
  code <- "lib_write(data.frame(flag = sas_put(1, 'yn.')), 'work', 'result')"
  project <- sas_project(file.path(root, "main.sas"))
  # A caller supplying a plain state list must get the compiled catalog.
  state <- unclass(new_migration_state(project, withr::local_tempdir()))
  state$runtime <- NULL
  state <- normalize_migration_state(state)
  state$selected_revisions <- list(main = list(r_code = code))
  plan <- build_program_smoke_plan(state$graph, "main", state$selected_revisions)
  prepared <- prepare_program_smoke(state, plan, state$attempt$attempt_dir)
  expect_true(run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir)$passed)
  output <- file.path(prepared$runtime$output_dirs[["work"]], "result.rds")
  expect_identical(readRDS(output)$flag, "YES")
})

test_that("smoke and bundle execution both exclude selected setup assignments", {
  root <- withr::local_tempdir()
  writeLines("%let label = EXAMPLE;", file.path(root, "autoexec.sas"))
  writeLines('data work.result; label="&label"; run;', file.path(root, "main.sas"))
  project <- sas_project(file.path(root, "main.sas"))
  state <- new_migration_state(project, withr::local_tempdir())
  code <- list(setup = "invented_label <- 'EXAMPLE'",
    main = "lib_write(data.frame(label = invented_label), 'work', 'result')")
  state$selected_revisions <- lapply(names(code), function(id) list(component_id = id,
    r_code = code[[id]], staged_file = paste0(id, ".R"),
    contract = list(component_id = id, staged_file = paste0(id, ".R"))))
  names(state$selected_revisions) <- names(code)
  plan <- build_program_smoke_plan(state$graph, "main", state$selected_revisions)
  expect_false("setup" %in% plan$dependency_prefix)
  prepared <- prepare_program_smoke(state, plan, state$attempt$attempt_dir)
  smoke <- run_program_smoke(prepared$plan, prepared$runtime, prepared$attempt_dir)
  bundle <- run_bundle_attempt(state)
  expect_false(smoke$passed)
  expect_false(bundle$passed)
  expect_match(smoke$condition$message, "invented_label.*not found")
  expect_match(bundle$condition$message, "invented_label.*not found")
})

test_that("conflicting format definitions are withheld and defer execution", {
  root <- withr::local_tempdir()
  for (i in 1:2) writeLines(c(
    sprintf("proc format; value ord 1='%s'; run;", c("First", "Second")[i]),
    sprintf("data work.out%d; label=put(1,ord.); run;", i)), file.path(root, paste0("p", i, ".sas")))
  check <- sas_preflight(root, diagnose = "off")
  compiled <- compile_format_catalog(check$project)
  expect_null(compiled$catalog$ord)
  expect_true(all(compiled$flags$reason == "format_redefined:ord"))
  expect_length(unique(compiled$flags$unit_id), 2L)
  expect_identical(check$status, "needs_attention")
  expect_true("format_redefined" %in% check$findings$kind)
  warning <- Filter(function(x) x$kind == "format_redefined", check$readiness$warnings)
  expect_length(warning, 1L)
  expect_true(warning[[1L]]$blocks_execution)
  expect_true(all(c("p1", "p2") %in% warning[[1L]]$affected))
  # Repeating the same definition, including the character variant, is valid.
  for (i in 1:2) writeLines(c("proc format; value ord 1='First'; value $ord '1'='Text'; run;",
    sprintf("data work.out%d; label=put(1,ord.); run;", i)), file.path(root, paste0("p", i, ".sas")))
  repeated <- compile_format_catalog(sas_project(root))
  expect_equal(nrow(repeated$flags), 0L)
  expect_identical(sas_put(1, "ord.", catalog = repeated$catalog), "First")
  expect_identical(sas_put("1", "$ord.", catalog = repeated$catalog), "Text")
})

test_that("equivalent reordered format definitions remain executable", {
  root <- withr::local_tempdir()
  definitions <- c(
    "value ord 1='A' 2='B'; value $ord 'a'='First' 'b'='Second'; value age low-<18='Child' 18-high='Adult' other='Unknown';",
    "value ord 2='B' 1='A'; value $ord 'b'='Second' 'a'='First'; value age other='Unknown' 18-high='Adult' low-<18='Child';"
  )
  for (i in seq_along(definitions)) writeLines(c(
    paste("proc format;", definitions[i], "run;"),
    sprintf("data work.out%d; label=put(1,ord.); run;", i)), file.path(root, paste0("p", i, ".sas")))
  check <- sas_preflight(root, diagnose = "off")
  compiled <- compile_format_catalog(check$project)
  expect_equal(nrow(compiled$flags), 0L)
  expect_false("format_redefined" %in% check$findings$kind)
  expect_identical(check$status, "ready_for_translation")
  expect_identical(sas_put(c(2, 1), "ord.", catalog = compiled$catalog), c("B", "A"))
  expect_identical(sas_put(c("b", "a"), "$ord.", catalog = compiled$catalog), c("Second", "First"))
  expect_identical(sas_put(c(-1, 17.9, 18, 80, NA), "age.", catalog = compiled$catalog),
    c("Child", "Child", "Adult", "Adult", "Unknown"))

  # An endpoint change is still a real conflict after ordering is normalized.
  writeLines(paste("proc format;", sub("18-high", "18<-high", definitions[2], fixed = TRUE), "run;"),
    file.path(root, "p2.sas"))
  changed <- compile_format_catalog(sas_project(root))
  expect_null(changed$catalog$age)
  expect_true(all(changed$flags$reason == "format_redefined:age"))
  expect_identical(sas_put(1, "ord.", catalog = changed$catalog), "A")
})

test_that("startup loads one compiled catalog for programs and macro functions", {
  root <- withr::local_tempdir()
  writeLines("proc format; value yn 0='NO' 1='YES'; value $yn '0'='zero' '1'='one'; run;",
    file.path(root, "autoexec.sas"))
  writeLines("data work.result; flag=put(1,yn.); run;", file.path(root, "main.sas"))
  project <- sas_project(file.path(root, "main.sas"))
  out <- withr::local_tempdir()
  write_helpers(out)
  write_formats(compile_format_catalog(project)$catalog, out)
  write_autoexec(project, out)
  dir.create(file.path(out, "R", "macros"), recursive = TRUE)
  writeLines('format_flag <- function(x) sas_put(x, "yn.")',
    file.path(out, "R", "macros", "format_flag.R"))
  scope <- new.env(parent = globalenv())
  sys.source(file.path(out, "autoexec.R"), envir = scope, chdir = TRUE)
  expect_identical(evalq(sas_put(c(0, 1), "YN."), scope), c("NO", "YES"))
  expect_identical(scope$format_flag(c(1, 0)), c("YES", "NO"))
  expect_identical(evalq(sas_put(c("1", "0"), "$yn"), scope), c("one", "zero"))
  expect_false(exists("sas_formats", envir = scope, inherits = FALSE))

  selected <- list(setup = list(revision_id = "r1", r_code = "sas_formats <- list()"))
  context <- list(project = project, component_id = "main", selected_revisions = selected)
  dependency <- read_dependency_context(context, "setup", "r")
  expect_identical(dependency$code, selected$setup$r_code)
  expect_match(dependency$execution_note, "not executed as a program", fixed = TRUE)
  guidance <- build_agent_guidance(project, "main", selected_revisions = selected)
  expect_match(guidance$text, dependency$execution_note, fixed = TRUE)
})
