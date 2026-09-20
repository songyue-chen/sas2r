# These inputs are generated here; no existing study project or real data is read.
organization_fixture <- function(envir = parent.frame()) {
  root <- normalizePath(withr::local_tempdir(.local_envir = envir), winslash = "/")
  for (dir in c("programs", "include", "macros", "input")) dir.create(file.path(root, dir))
  saveRDS(data.frame(value = c(2, 4)), file.path(root, "input", "raw.rds"))
  writeLines(c("libraries:", "  raw:", "    path: input", "    engine: rds",
    "macros:", "  search_path: [macros]"), file.path(root, "_sas2r.yml"))
  writeLines("data work.stage; set raw.raw; value=value+1; run;", file.path(root, "programs", "prepare.sas"))
  writeLines(c('%include "../include/setup.sas";', '%scale();',
    "data raw.final; set work.stage; value=value*2; run;"), file.path(root, "programs", "report & review.sas"))
  writeLines("%let marker=1;", file.path(root, "include", "setup.sas"))
  writeLines("%macro scale(); %put scale; %mend;", file.path(root, "macros", "scale.sas"))
  project <- sas_project(file.path(root, "programs"))
  state <- new_migration_state(project, file.path(root, "migration"))
  modules <- transpile_staged_modules(project)
  for (i in seq_len(nrow(modules))) {
    source <- modules$file[[i]]
    cid <- unique(project$graph$nodes$component_id[project$graph$nodes$source_file %in% source])[[1L]]
    code <- if (basename(source) == "prepare.sas") {
      "x <- lib_read('raw','raw'); x$value <- x$value + 1; lib_write(x,'work','stage')"
    } else if (basename(source) == "setup.sas") "marker <- 10" else paste(
      sprintf('sas2r_source_include(%s)', deparse(modules$staged_file[basename(modules$file) == "setup.sas"])),
      "x <- scale(lib_read('work','stage')); stopifnot(marker == 10)",
      "lib_write(x,'raw','final'); writeLines('<html>synthetic</html>', 'summary & detail.html')", sep = "\n")
    state$selected_revisions[[cid]] <- list(component_id = cid, revision_id = "r1", r_code = code,
      staged_file = modules$staged_file[[i]])
  }
  state$selected_revisions$macro__scale <- list(component_id = "macro__scale", revision_id = "r1",
    staged_file = "R/macros/scale.R", r_code = "scale <- function(x) { x$value <- x$value*2; x }")
  attempt <- init_attempt(state$paths, "bundle")
  snapshot <- snapshot_selected_bundle(state, attempt)
  list(root = root, state = state, attempt = attempt, snapshot = snapshot)
}

run_manual_bundle <- function(dir) {
  callr::r(function(dir) {
    setwd(dir)
    before <- getwd()
    source("run.R")
    stopifnot(identical(getwd(), before))
    readRDS(file.path("output", "datasets", "raw", "final.rds"))
  }, args = list(dir = dir))
}

test_that("organized bundle moves with macros, includes, upstream data, and separate writes", {
  fx <- organization_fixture()
  bundle <- fx$state$paths$bundle
  original <- attempt_output_hashes(fx$snapshot)
  materialize_user_bundle(fx$snapshot, bundle, fx$state$project)
  expect_true(file.exists(file.path(bundle, "macros", "scale.R")))
  expect_true(file.exists(file.path(bundle, "runtime", "sas2r-helpers.R")))
  expect_false(file.exists(file.path(bundle, "output")))
  expect_equal(run_manual_bundle(bundle)$value, c(6, 10))
  expect_true(file.exists(file.path(bundle, "output", "tlf", "summary & detail.html")))
  expect_equal(readRDS(file.path(fx$root, "input", "raw.rds"))$value, c(2, 4))
  expect_identical(attempt_output_hashes(fx$snapshot), original)

  moved <- file.path(withr::local_tempdir(), "moved bundle & data")
  materialize_user_bundle(bundle, moved, fx$state$project)
  expect_false(dir.exists(file.path(moved, "output")))
  expect_equal(run_manual_bundle(moved)$value, c(6, 10))
  dir.create(file.path(moved, "input"))
  file.copy(file.path(fx$root, "input", "raw.rds"), file.path(moved, "input"))
  autoexec <- file.path(moved, "autoexec.R")
  text <- readLines(autoexec)
  text <- gsub(deparse(file.path(fx$root, "input")), '"input"', text, fixed = TRUE)
  writeLines(text, autoexec)
  unlink(file.path(fx$root, "input"), recursive = TRUE)
  expect_equal(run_manual_bundle(moved)$value, c(6, 10))
})

test_that("output contracts retain requested WORK and separate unrequested scratch", {
  fx <- organization_fixture()
  state <- fx$state
  attempt <- fx$attempt
  saveRDS(data.frame(value = 6), file.path(attempt$work_dir, "deliver.rds"))
  saveRDS(data.frame(value = 999), file.path(attempt$work_dir, "scratch.rds"))
  state$selected_attempt <- attempt
  state$selected_attempt$output_hashes <- attempt_output_hashes(attempt$attempt_dir)
  state$output_contracts <- infer_output_contracts(state$project,
    overrides = list(datasets = "work.deliver"))
  saved <- materialize_run_outputs(state)
  expect_equal(readRDS(file.path(state$paths$outputs_datasets, "work", "deliver.rds"))$value, 6)
  expect_false(file.exists(file.path(state$paths$outputs_datasets, "work", "scratch.rds")))
  expect_equal(readRDS(file.path(state$paths$work, "work", "scratch.rds"))$value, 999)
  expect_true("work.deliver" %in% names(saved))
})

test_that("partial navigation reports missing code and escapes local names and errors", {
  fx <- organization_fixture()
  state <- fx$state
  materialize_user_bundle(fx$snapshot, state$paths$bundle, state$project)
  state$bundle_dir <- state$paths$bundle
  state$status <- "blocked"
  state$status_reason <- 'problem "quoted" <value> & \'one\''
  state$schedule <- rbind(state$schedule, state$schedule[1L, , drop = FALSE])
  state$schedule$component_id[nrow(state$schedule)] <- "never_generated"
  write_migration_report(state)
  manifest <- read_json_record(state$paths$manifest)
  expect_false(manifest$components$never_generated$generated)
  expect_null(manifest$components$never_generated$code)
  expect_null(manifest$selected_attempt_id)
  html <- paste(readLines(state$paths$start_here), collapse = "\n")
  expect_match(html, "&quot;quoted&quot; &lt;value&gt; &amp; &#39;one&#39;", fixed = TRUE)
  expect_match(html, "report%20%26%20review.R", fixed = TRUE)
  links <- regmatches(html, gregexpr('href="[^"]+"', html))[[1L]]
  links <- sub('^href="(.*)"$', '\\1', links)
  expect_true("#components" %in% links)
  expect_match(html, 'id="components"', fixed = TRUE)
  links <- links[!startsWith(links, "#")]
  expect_true(all(file.exists(file.path(state$paths$run_root, utils::URLdecode(links)))))
})

test_that("manifest dependencies are arrays for zero, one and multiple providers", {
  root <- withr::local_tempdir()
  writeLines("data work.seed_a; value=1; run;", file.path(root, "seed_a.sas"))
  writeLines("data work.seed_b; value=2; run;", file.path(root, "seed_b.sas"))
  writeLines("data work.one; set work.seed_a; run;", file.path(root, "one.sas"))
  writeLines("data work.two; set work.seed_a work.seed_b; run;", file.path(root, "two.sas"))
  state <- new_migration_state(sas_project(root), withr::local_tempdir())
  write_migration_report(state)

  # Disable reader simplification so a JSON string cannot masquerade as an array.
  manifest <- jsonlite::read_json(state$paths$manifest, simplifyVector = FALSE)
  dependencies <- lapply(manifest$components, `[[`, "dependencies")
  expect_identical(dependencies$seed_a, list())
  expect_identical(dependencies$seed_b, list())
  expect_identical(dependencies$one, list("seed_a"))
  expect_true(is.list(dependencies$two))
  expect_setequal(unlist(dependencies$two), c("seed_a", "seed_b"))
})

test_that("handled preflight failure leaves a blocked page and keeps its original error", {
  root <- withr::local_tempdir()
  writeLines("/* no active source */", file.path(root, "program.sas"))
  out <- withr::local_tempdir()
  expect_error(sas_translate(root, out_dir = out, execute = FALSE), "No active source")
  pages <- list.files(out, pattern = "START_HERE.html", recursive = TRUE, full.names = TRUE)
  expect_length(pages, 1L)
  manifest <- read_json_record(file.path(dirname(pages), "manifest.json"))
  expect_identical(manifest$status, "blocked")
  expect_null(manifest$selected_attempt_id)
  expect_identical(manifest$outcome$severity, "error")
  expect_identical(manifest$outcome$title, "Run incomplete - failed")
  expect_match(paste(readLines(pages), collapse = "\n"), "Stopped during: preflight", fixed = TRUE)
})

test_that("manual output-root changes apply to every write and allow supplied upstream data", {
  fx <- organization_fixture()
  bundle <- fx$state$paths$bundle
  materialize_user_bundle(fx$snapshot, bundle, fx$state$project)
  expect_equal(run_manual_bundle(bundle)$value, c(6, 10))
  # Ordinary user configuration: keep the old manual results, give WORK an
  # external read directory, and run just the downstream report with fresh writes.
  input <- file.path(fx$root, "supplied")
  dir.create(input)
  saveRDS(data.frame(value = c(20, 40)), file.path(input, "stage.rds"))
  path <- file.path(bundle, "autoexec.R")
  text <- readLines(path)
  text <- gsub('file.path(.sas2r_bundle_root, "output")',
    'file.path(.sas2r_bundle_root, "output-second")', text, fixed = TRUE)
  text <- gsub('read_path = file.path(.sas2r_output_root, "work")',
    paste0('read_path = ', deparse(input)), text, fixed = TRUE)
  writeLines(text, path)
  value <- callr::r(function(bundle) {
    setwd(bundle)
    source("autoexec.R")
    dir.create(file.path(.sas2r_output_root, "tlf"), recursive = TRUE)
    setwd(file.path(.sas2r_output_root, "tlf"))
    source(file.path(.sas2r_bundle_root, "programs", "report & review.R"))
    lib_read("raw", "final")$value
  }, args = list(bundle = bundle))
  expect_equal(value, c(40, 80))
  expect_equal(readRDS(file.path(bundle, "output/datasets/raw/final.rds"))$value, c(6, 10))
  expect_equal(readRDS(file.path(bundle, "output-second/datasets/raw/final.rds"))$value, c(40, 80))
})

test_that("a selected attempt supplies matching code, identity, and saved output values", {
  fx <- organization_fixture()
  state <- fx$state
  state$output_contracts <- infer_output_contracts(state$project,
    overrides = list(datasets = "work.deliver"))
  for (i in 1:2) {
    attempt <- if (i == 1L) fx$attempt else init_attempt(state$paths, "bundle")
    state$selected_revisions$prepare$r_code <- sprintf("lib_write(data.frame(value=%d),'work','deliver')", i)
    snapshot_selected_bundle(state, attempt)
    saveRDS(data.frame(value = i), file.path(attempt$work_dir, "deliver.rds"))
    attempt <- complete_attempt(attempt, output_hashes = attempt_output_hashes(attempt$attempt_dir),
      passed = TRUE, execution_order = "prepare", executed_component_ids = "prepare")
    assessment <- list(status = "needs_review", passing_targets = "work.deliver")
    state$selected_attempt <- select_attempt(state$paths, attempt, assessment)
  }
  materialize_user_bundle(file.path(state$selected_attempt$attempt_dir, "bundle"), state$paths$bundle, state$project)
  state$saved_outputs <- materialize_run_outputs(state)
  state$bundle_dir <- state$paths$bundle
  state$status <- "needs_review"
  write_migration_report(state)
  expect_match(paste(readLines(file.path(state$paths$bundle_programs, "prepare.R")), collapse = "\n"), "value=2", fixed = TRUE)
  expect_equal(readRDS(file.path(state$paths$outputs_datasets, "work/deliver.rds"))$value, 2)
  expect_identical(read_json_record(state$paths$manifest)$selected_attempt_id, "bundle_attempt_002")
  prune_rejected_attempt_outputs(state$paths)
  expect_false(file.exists(file.path(fx$attempt$work_dir, "deliver.rds")))
  expect_true(file.exists(file.path(fx$snapshot, "prepare.R")))
})
test_that("navigation separates unresolved expressions from concrete output files", {
  fx <- organization_fixture()
  state <- fx$state
  state$output_contracts <- merge_output_overrides(empty_output_contracts(),
    list(tlfs = c("summary-&panel..html", "summary-one.html")))
  state$assessment <- assess_final_outputs(state$output_contracts,
    list(attempt_dir = fx$root, completed = TRUE, passed = TRUE))
  state$status <- state$assessment$status
  write_migration_report(state)
  html <- paste(readLines(state$paths$start_here), collapse = "\n")
  saved <- strsplit(strsplit(html, "<h2>Saved outputs</h2>", fixed = TRUE)[[1L]][2L],
                    "<h2>Unresolved output expressions</h2>", fixed = TRUE)[[1L]]
  expect_match(saved[1L], "summary-one.html", fixed = TRUE)
  expect_false(grepl("summary-&amp;panel", saved[1L], fixed = TRUE))
  expect_match(saved[2L], "summary-&amp;panel..html", fixed = TRUE)
  expect_match(saved[2L], "concrete outputs unknown", fixed = TRUE)
  report <- read_json_record(state$paths$report_json)
  expect_identical(report$output_assessments[["summary-&panel..html"]]$status, "unresolved_target")
})
