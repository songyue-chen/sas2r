# A variable a DATA step reads that no input supplies: read as missing and
# reported, as SAS does. Every dataset here is made up in the test.

# The runtime a bundle carries, loaded the way a bundle loads it.
note_runtime <- function() {
  h <- new.env(parent = globalenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), h)
  h$.sas2r_registry <- list(work = list(path = tempdir()))
  h
}

# A project of SAS programs reading `raw.dm`, which holds no RANDDTC, and the
# migration state of a run whose generated R is given per program.
note_fixture <- function(programs, r, envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  dir.create(file.path(root, "inputs"))
  saveRDS(data.frame(usubjid = c("01", "02", "03"), stringsAsFactors = FALSE), file.path(root, "inputs", "dm.rds"))
  for (name in names(programs)) writeLines(programs[[name]], file.path(root, paste0(name, ".sas")))
  config <- list(
    libraries = list(raw = list(path = file.path(root, "inputs"), engine = "rds"),
                     adam = list(path = file.path(root, "adam"), engine = "rds", write = "rds")),
    migration = list(execution_order = paste0(names(programs), ".sas")))
  project <- sas_project(root, config = config)
  state <- new_migration_state(project, file.path(root, "migration"), config = config, execute = TRUE)
  for (id in names(programs)) {
    code <- paste(r[[id]], collapse = "\n")
    state$selected_revisions[[id]] <- list(component_id = id, revision_id = "r1", r_code = code,
      staged_file = paste0(id, ".R"),
      contract = list(component_id = id, staged_file = paste0(id, ".R"),
                      sas_text = paste(programs[[id]], collapse = "\n")))
  }
  list(root = root, project = project, state = state)
}

adsl_sas <- c("data adam.adsl;", "  set raw.dm;", "  randdt = input(substr(RANDDTC, 1, 10), yymmdd10.);",
              "  if randdt ne . then ittfl = 'Y';", "  else ittfl = 'N';", "run;")
adsl_r <- c("dm <- lib_read('raw', 'dm') |> sas_uninitialized('randdtc')",
            "dm$randdt <- as.numeric(as.Date(substr(dm$randdtc, 1, 10)) - as.Date('1960-01-01'))",
            "dm$ittfl <- ifelse(!is.na(dm$randdt), 'Y', 'N')",
            "lib_write(dm, 'adam', 'adsl')")

test_that("an absent variable is read as missing, with the note SAS logs", {
  h <- note_runtime()
  dm <- data.frame(usubjid = c("01", "02"), Age = c(30, 41), stringsAsFactors = FALSE)

  seen <- character()
  out <- withCallingHandlers(
    h$sas_uninitialized(dm, c("USUBJID", "randdtc", "RANDDTC", "age")),
    sas2r_uninitialized_variable = function(m) {
      seen <<- c(seen, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  # Present in another letter case counts as present; a name is added once.
  expect_identical(names(out), c("usubjid", "Age", "randdtc"))
  expect_identical(out$randdtc, c(NA_real_, NA_real_))
  expect_identical(seen, "NOTE: Variable randdtc is uninitialized.\n")
  expect_message(h$sas_uninitialized(dm, "randdtc"), "NOTE: Variable randdtc is uninitialized.", fixed = TRUE)

  # A variable the step declares character is a character missing.
  typed <- suppressMessages(h$sas_uninitialized(out, "armcd", type = "character"))
  expect_identical(typed$armcd, c(NA_character_, NA_character_))

  # Nothing to add: the frame comes back as it was, with no note.
  expect_silent(same <- h$sas_uninitialized(dm, c("usubjid", "AGE")))
  expect_identical(same, dm)

  # It works on a frame with no rows and on a tibble.
  empty <- suppressMessages(h$sas_uninitialized(tibble::tibble(a = integer()), "b"))
  expect_identical(empty$b, numeric())
  expect_s3_class(empty, "tbl_df")

  # Each case is recorded where a run reads it.
  recorded <- get(".sas2r_log_notes", envir = h)
  expect_identical(vapply(recorded, `[[`, "", "variable"), c("randdtc", "randdtc", "armcd", "b"))
  expect_identical(recorded[[3L]], list(note = "uninitialized_variable", variable = "armcd", type = "character", rows = 2L))

  expect_error(h$sas_uninitialized(dm, "a b"), "names each variable as a string", fixed = TRUE)
  expect_error(h$sas_uninitialized(dm, NA_character_), "names each variable as a string", fixed = TRUE)
  expect_error(h$sas_uninitialized(dm, "x", type = "date"), "numeric", fixed = TRUE)
  expect_error(h$sas_uninitialized(list(a = 1), "x"), "data frame", fixed = TRUE)
})

test_that("the package form works with no registry loaded", {
  dm <- data.frame(usubjid = "01", stringsAsFactors = FALSE)
  expect_message(out <- sas_uninitialized(dm, "randdtc"), "uninitialized")
  expect_identical(out$randdtc, NA_real_)
})

test_that("only a plain variable name survives from what a program recorded", {
  note <- function(variable, rows = 3L, type = "numeric", kind = "uninitialized_variable") {
    list(note = kind, variable = variable, type = type, rows = rows)
  }
  kept <- run_log_notes(list(
    p01 = list(note("RANDDTC", 3L), note("randdtc", 60L), note("armcd", 2L, "character"),
               note("2024-02-29"), note("a b"), note(strrep("x", 33L)), note("x", kind = "other"),
               list(variable = "y"), "text"),
    p02 = list(note("not a name")),
    p03 = list()))
  expect_identical(names(kept), "p01")
  expect_identical(kept$p01, list(
    list(note = "uninitialized_variable", variable = "RANDDTC", type = "numeric", rows = 60L, times = 2L),
    list(note = "uninitialized_variable", variable = "armcd", type = "character", rows = 2L, times = 1L)))
  # Reading the result again changes nothing, in either shape a saved record
  # comes back in.
  expect_identical(run_log_notes(kept), kept)
  json <- jsonlite::toJSON(kept, auto_unbox = TRUE)
  expect_identical(run_log_notes(jsonlite::fromJSON(json, simplifyVector = FALSE)), kept)
  expect_identical(run_log_notes(jsonlite::fromJSON(json, simplifyVector = TRUE)), kept)
  expect_identical(run_log_notes(NULL), list())
  expect_identical(run_log_notes(list()), list())
})

test_that("a name written in the call must be one the SAS source uses", {
  fx <- note_fixture(list(p01 = adsl_sas), list(p01 = adsl_r))
  problems <- function(...) check_uninitialized_names(c(...), fx$project)

  expect_identical(problems(adsl_r), character())
  expect_identical(problems("dm <- sas_uninitialized(dm, c('RANDDTC', 'ittfl'))"), character())
  expect_identical(problems("dm <- dm %>% sas_uninitialized('randdtc', type = 'numeric')"), character())
  expect_identical(problems("dm <- sas2r::sas_uninitialized(vars = 'randdtc', df = dm)"), character())

  found <- problems("dm <- sas_uninitialized(dm, c('randdtc', 'aval2'))",
                    "dm <- dm |> sas_uninitialized('made_up')")
  expect_length(found, 2L)
  expect_match(found[[1L]], "unbacked_variable_name: sas_uninitialized() names aval2, which the SAS source never uses",
               fixed = TRUE)
  expect_match(found[[2L]], "names made_up", fixed = TRUE)

  # A name computed at run time, as inside a translated macro, is not read here.
  expect_identical(problems("f <- function(df, dtcvar) sas_uninitialized(df, dtcvar)"), character())
  expect_identical(problems("dm <- sas_uninitialized(dm, paste0(prefix, 'dt'))"), character())
  # Without the source there is nothing to check against.
  expect_identical(check_uninitialized_names("sas_uninitialized(dm, 'made_up')", NULL), character())
  expect_identical(problems("x <- 1"), character())

  # The revision check applies it, so an invented name is repair feedback.
  r_path <- withr::local_tempfile(fileext = ".R")
  writeLines(c("dm <- lib_read('raw', 'dm')", "dm <- sas_uninitialized(dm, 'made_up')"), r_path)
  checked <- check_program_revision(r_path, contract = list(component_id = "p01"), project = fx$project)
  expect_false(checked$pass)
  expect_true(any(grepl("unbacked_variable_name", checked$errors, fixed = TRUE)))
  writeLines(adsl_r, r_path)
  expect_true(check_program_revision(r_path, contract = list(component_id = "p01"), project = fx$project)$pass)
})

test_that("a check run and a bundle run both record the note for the program that ran", {
  fx <- note_fixture(
    list(p01 = adsl_sas, p02 = c("data adam.adtte;", "  set adam.adsl;", "  startdt = randdt;", "run;")),
    list(p01 = adsl_r, p02 = c("t <- lib_read('adam', 'adsl')", "stop('made-up failure')")))

  plan <- build_program_smoke_plan(fx$state$graph, "p01", fx$state$selected_revisions)
  smoke <- run_program_smoke(plan, fx$state$runtime, withr::local_tempdir())
  expect_true(smoke$passed)
  expect_identical(smoke$log_notes, list(p01 = list(
    list(note = "uninitialized_variable", variable = "randdtc", type = "numeric", rows = 3L, times = 1L))))

  # The second program stops; the first program's note is kept.
  bundle <- run_bundle_attempt(fx$state, sequence = 1L)
  expect_false(bundle$passed)
  expect_identical(bundle$condition$component_id, "p02")
  expect_identical(bundle$log_notes, smoke$log_notes)
  saved <- read_json_record(file.path(bundle$attempt_dir, "record.json"))
  expect_identical(run_log_notes(saved$log_notes), smoke$log_notes)

  # The program wrote what SAS would: the date missing, and the flag from it.
  written <- list.files(bundle$attempt_dir, pattern = "^adsl[.]rds$", recursive = TRUE, full.names = TRUE)
  expect_length(written, 1L)
  adsl <- readRDS(written)
  expect_identical(adsl$randdt, rep(NA_real_, 3L))
  expect_identical(adsl$ittfl, rep("N", 3L))
})

test_that("the report lists each note and marks a name the SAS source never uses", {
  fx <- note_fixture(list(p01 = adsl_sas),
    list(p01 = c(adsl_r[1:3], "dm <- sas_uninitialized(dm, paste0('zz', 'var'))", adsl_r[[4L]])))
  bundle <- run_bundle_attempt(fx$state, sequence = 1L)
  expect_true(bundle$passed)

  notes <- report_log_notes(fx$state, list(completed_attempts = list(bundle)))
  expect_identical(vapply(notes$p01, `[[`, "", "variable"), c("randdtc", "zzvar"))
  expect_identical(vapply(notes$p01, `[[`, NA, "named_in_source"), c(TRUE, FALSE))
  lines <- log_note_report_lines(notes)
  expect_identical(lines[[1L]], "## SAS log notes")
  expect_true("- p01: Variable randdtc is uninitialized. No input supplied it, so the program read it as missing in 3 rows." %in% lines)
  expect_true(any(grepl("p01: Variable zzvar is uninitialized.*The SAS source never names this variable", lines)))
  expect_identical(log_note_report_lines(list()), character())

  # The machine report, the Markdown report, the start-here page and the
  # outcome summary all carry it.
  write_migration_report(fx$state)
  report <- jsonlite::read_json(fx$state$paths$report_json)
  expect_identical(vapply(report$log_notes$p01, `[[`, "", "variable"), c("randdtc", "zzvar"))
  expect_true(any(grepl("SAS log notes: 2 uninitialized variables read as missing (randdtc, zzvar)",
                        unlist(report$outcome$details), fixed = TRUE)))
  for (path in c(fx$state$paths$report_md, fx$state$paths$start_here)) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(text, "SAS log notes", fixed = TRUE, info = path)
    expect_match(text, "p01: Variable randdtc is uninitialized. No input supplied it", fixed = TRUE, info = path)
    expect_match(text, "The SAS source never names this variable", fixed = TRUE, info = path)
  }

  # A selected attempt is the one reported, ahead of a later attempt.
  fx$state$selected_attempt <- list(log_notes = list())
  expect_identical(report_log_notes(fx$state, list(completed_attempts = list(bundle))), list())
  # With no bundle attempt, a program's own check run is reported.
  fx$state$selected_attempt <- NULL
  fx$state$selected_revisions$p01$smoke <- list(log_notes = bundle$log_notes)
  expect_identical(names(report_log_notes(fx$state, list(completed_attempts = list()))), "p01")
})

test_that("the rule reaches every role with the helper's own description", {
  policy <- agent_guidance_policy()
  expect_match(policy, "sas_uninitialized(df, \"name\")", fixed = TRUE)
  expect_match(policy, "Where SAS itself stops on an absent variable", fixed = TRUE)
  reference <- helper_reference()
  expect_match(reference, "sas_uninitialized(df, vars, type = \"numeric\")", fixed = TRUE)
  expect_match(reference, "NOTE: Variable X is uninitialized.", fixed = TRUE)
  expect_true("sas_uninitialized" %in% SAS2R_HELPER_NAMES)
})
