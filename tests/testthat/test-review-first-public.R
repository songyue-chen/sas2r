# Public regressions from the 2026-09-11 review. Provider calls are observable
# counters, so a plausible status can never substitute for proving reuse/limits.
counted_review_llm <- function(responses) {
  adapter <- mock_llm(responses)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  request <- adapter$request
  adapter$request <- function(payload) {
    calls$n <- calls$n + 1L
    request(payload)
  }
  list(llm = adapter, calls = calls)
}

review_public_fixture <- function(envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  inputs <- file.path(root, "input")
  dir.create(inputs)
  saveRDS(data.frame(x = c(1, 7)), file.path(inputs, "src.rds"))
  source <- file.path(root, "program.sas")
  writeLines("data work.out; set raw.src; do; x = x + 10; end; where x < 5; run;", source)
  list(root = root, source = source, inputs = inputs,
       config = list(libraries = list(raw = list(path = inputs, engine = "rds", write = "rds"))))
}

review_public_code <- paste(
  "out <- lib_read('raw', 'src')",
  "out <- out[out$x < 5, , drop = FALSE]",
  "out$x <- out$x + 10",
  "lib_write(out, 'work', 'out')", sep = "\n"
)

test_that("public request limits prevent provider calls and validate configuration", {
  fx <- review_public_fixture()
  for (limit in c("max_calls", "max_request_bytes", "max_request_chars", "max_input_tokens", "max_wall_time")) {
    adapter <- counted_review_llm(list())
    result <- sas_translate(fx$source, config = fx$config, execute = FALSE,
                            llm = adapter$llm, usage_limits = stats::setNames(list(0), limit))
    expect_identical(adapter$calls$n, 0L, info = limit)
    expect_equal(result$usage[[limit]], 0, info = limit)
    expect_identical(result$status, "needs_review")
  }
  expect_error(sas_translate(fx$source, usage_limits = list(max_call = 0)), class = "sas2r_budget_config_error")
  expect_error(sas_translate(fx$source, usage_limits = list(max_calls = -1)), class = "sas2r_budget_config_error")
  expect_error(sas_translate(fx$source, config = list(llm = list(provider = "invalid_provider"))),
               class = "sas2r_llm_config_error")
})

test_that("public resume uses actual saved revisions and makes no repeated provider calls", {
  fx <- review_public_fixture()
  out <- file.path(fx$root, "migration")
  adapter <- counted_review_llm(list(good_translation(review_public_code), good_review()))
  first <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = adapter$llm)
  expect_identical(adapter$calls$n, 2L)
  expect_equal(readRDS(file.path(first$outputs_dir, "work", "out.rds"))$x, 11)
  again <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = adapter$llm, resume = TRUE)
  expect_identical(adapter$calls$n, 2L)
  expect_identical(sas_code(first), sas_code(again))
  expect_identical(again$status, first$status)
  expect_length(again$diagnostics$resumed_components, 1L)
  expect_equal(readRDS(file.path(again$outputs_dir, "work", "out.rds"))$x, 11)

  # A changed source invalidates the saved translation/review as a whole.
  write("* changed source;", file = fx$source, append = TRUE)
  fresh <- counted_review_llm(list(good_translation(review_public_code), good_review()))
  changed <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = fresh$llm, resume = TRUE)
  expect_identical(fresh$calls$n, 2L)
  expect_null(changed$diagnostics$resumed_components)

  # Input bytes, not just their paths, participate in reuse.
  saveRDS(data.frame(x = c(2, 9)), file.path(fx$inputs, "src.rds"))
  fresh2 <- counted_review_llm(list(good_translation(review_public_code), good_review()))
  changed_input <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = fresh2$llm, resume = TRUE)
  expect_identical(fresh2$calls$n, 2L)
  expect_equal(readRDS(file.path(changed_input$outputs_dir, "work", "out.rds"))$x, 12)
})

test_that("public progress distinguishes unavailable review and deferred execution", {
  fx <- review_public_fixture()
  events <- list()
  result <- withCallingHandlers(sas_translate(fx$source, config = fx$config, execute = FALSE),
    sas2r_progress = function(e) events[[length(events) + 1L]] <<- e)
  text <- unlist(lapply(events, format_sas2r_progress))
  expect_true(any(grepl("review unavailable", text, fixed = TRUE)))
  expect_false(any(grepl(": reviewed$", text)))
  expect_true(any(grepl("deferred.*execute_disabled", text)))
  expect_false(any(grepl("bundle.*failed", text)))
  expect_true(any(grepl("Elapsed:", text)))
  expect_true(any(grepl("Effective limits", text)))
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "Elapsed:")
  expect_match(printed, "Effective limits")
  expect_match(printed, "0 / 1 components")
  reported <- jsonlite::read_json(result$report_json_path)$component_evidence[[1L]]
  expect_identical(reported$evidence_level, "pending")
  expect_identical(reported$review_status, "review_unavailable")
})

test_that("public exports include every library and TLF and run after moving", {
  fx <- review_public_fixture()
  # Name roots in reverse alphabetical order to exercise dependency order.
  writeLines("data work.mid; set raw.src; run;", file.path(fx$root, "z_first.sas"))
  writeLines("data adam.out; set work.mid; x = x + 1; run; ods html file='outputs/table.html'; proc print data=adam.out; run; ods html close;", file.path(fx$root, "a_second.sas"))
  unlink(fx$source)
  fx$config$libraries$adam <- list(path = fx$inputs, engine = "rds", write = "rds")
  second_code <- paste("out <- lib_read('work', 'mid')", "out$x <- out$x + 1",
    "lib_write(out, 'adam', 'out')",
    "dir.create('outputs', showWarnings = FALSE)",
    "writeLines('<html><body><table><tr><td>Results</td></tr></table></body></html>', 'outputs/table.html')",
    "writeLines('Summary', 'summary.txt')", sep = "\n")
  adapter <- counted_review_llm(list(good_review(), good_translation(second_code), good_review()))
  result <- sas_translate(fx$root, config = fx$config, out_dir = file.path(fx$root, "migration"), llm = adapter$llm)
  expect_identical(result$status, "migration_ready")
  for (rel in c("work/mid.rds", "adam/out.rds", "outputs/table.html", "summary.txt")) {
    expect_true(file.exists(file.path(result$outputs_dir, rel)), info = rel)
  }
  before <- attempt_output_hashes(dirname(result$bundle_dir))
  export <- file.path(fx$root, "export")
  sas_write(result, export)
  moved <- file.path(fx$root, "moved")
  expect_true(file.rename(export, moved))
  expect_true(file.exists(file.path(moved, "outputs/table.html")))
  expect_true(file.exists(file.path(moved, "README.md")))
  expect_true(file.exists(file.path(moved, "outputs-manifest.json")))
  # Delete copies to prove this execution creates fresh results in the moved folder.
  unlink(file.path(moved, c("work", "adam", "outputs")), recursive = TRUE)
  callr::r(function() source("run.R", chdir = TRUE), wd = moved)
  expect_equal(readRDS(file.path(moved, "adam/out.rds"))$x, c(2, 8))
  expect_true(file.exists(file.path(moved, "outputs/table.html")))
  expect_identical(attempt_output_hashes(dirname(result$bundle_dir)), before)
  expect_equal(readRDS(file.path(fx$inputs, "src.rds"))$x, c(1, 7))
})

test_that("coverage separates references and detects seeded value differences", {
  fx <- review_public_fixture()
  ref <- file.path(fx$root, "expected.rds")
  saveRDS(data.frame(x = 11), ref)
  overrides <- list(datasets = "work.out", references = list(work.out = ref))
  adapter <- counted_review_llm(list(good_translation(review_public_code), good_review()))
  result <- sas_translate(fx$source, config = fx$config, outputs = overrides, llm = adapter$llm)
  expect_identical(result$status, "validated")
  report <- jsonlite::read_json(result$report_json_path, simplifyVector = TRUE)
  expect_equal(report$coverage$outputs_produced, 1)
  expect_equal(report$coverage$outputs_reference_compared, 1)
  expect_equal(report$coverage$outputs_reference_passed, 1)
  expect_equal(report$coverage$components_independently_reviewed, 1)
  expect_identical(report$coverage$validated_targets, "work.out")
  expect_equal(report$usage$calls, 2)
  expect_true(report$usage$elapsed_seconds > 0)

  bad <- counted_review_llm(list(good_translation(sub("+ 10", "+ 20", review_public_code, fixed = TRUE)), good_review()))
  failed <- sas_translate(fx$source, config = fx$config, outputs = overrides, llm = bad$llm, max_bundle_repair_rounds = 0)
  expect_identical(failed$status, "blocked")
  coverage <- migration_coverage(failed$output_assessments, failed$component_evidence)
  expect_equal(coverage$outputs_produced, 1)
  expect_equal(coverage$outputs_reference_compared, 1)
  expect_equal(coverage$outputs_passed, 0)
  expect_length(coverage$validated_targets, 0)
})

test_that("public resume preserves a fixer revision's actual path and its review", {
  fx <- review_public_fixture()
  out <- file.path(fx$root, "migration")
  bad_code <- sub("+ 10", "+ 20", review_public_code, fixed = TRUE)
  adapter <- counted_review_llm(list(
    good_translation(bad_code),
    material_review_response(sas_evidence = "x increases by 10", r_evidence = "x increases by 20", affected_outputs = "work.out"),
    valid_program_fix_response(code = review_public_code),
    good_review()
  ))
  first <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = adapter$llm)
  expect_equal(adapter$calls$n, 4L)
  expect_equal(readRDS(file.path(first$outputs_dir, "work/out.rds"))$x, 11)
  checkpoint <- readRDS(file.path(out, ".sas2r/resume.rds"))
  selected <- checkpoint$selected_revisions[[1L]]
  expect_match(selected$r_path, "/revisions/rev_")
  expect_true(file.exists(selected$r_path))
  expect_identical(selected$revision_id, "r2")
  again <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = adapter$llm, resume = TRUE)
  expect_equal(adapter$calls$n, 4L)
  expect_identical(sas_code(again), sas_code(first))
  expect_equal(readRDS(file.path(again$outputs_dir, "work/out.rds"))$x, 11)

  # Lost revision files cause regeneration, rather than selecting empty code.
  unlink(selected$r_path)
  fresh <- counted_review_llm(list(good_translation(review_public_code), good_review()))
  recovered <- sas_translate(fx$source, config = fx$config, out_dir = out, llm = fresh$llm, resume = TRUE)
  expect_equal(fresh$calls$n, 2L)
  expect_equal(readRDS(file.path(recovered$outputs_dir, "work/out.rds"))$x, 11)
})

test_that("mixed reference coverage identifies exactly what contributes validation", {
  fx <- review_public_fixture()
  writeLines("data work.out work.extra; set raw.src; do; x = x + 10; end; where x < 5; run;", fx$source)
  ref <- file.path(fx$root, "expected.rds")
  saveRDS(data.frame(x = 11), ref)
  code <- paste(review_public_code, "lib_write(out, 'work', 'extra')", sep = "\n")
  adapter <- counted_review_llm(list(good_translation(code), good_review()))
  result <- sas_translate(fx$source, config = fx$config, llm = adapter$llm,
                          outputs = list(datasets = c("work.out", "work.extra"), references = list(work.out = ref)))
  coverage <- jsonlite::read_json(result$report_json_path, simplifyVector = TRUE)$coverage
  expect_identical(result$status, "validated")
  expect_equal(coverage$outputs_total, 2)
  expect_equal(coverage$outputs_produced, 2)
  expect_equal(coverage$outputs_reference_compared, 1)
  expect_equal(coverage$outputs_passed, 2)
  expect_identical(coverage$validated_targets, "work.out")
  expect_identical(coverage$unreferenced_targets, "work.extra")
})

test_that("public deterministic translation preserves WHERE timing and numeric flags", {
  fx <- review_public_fixture()
  saveRDS(data.frame(x = c(NA, -1, 2, 7)), file.path(fx$inputs, "src.rds"))
  writeLines("data work.out; set raw.src; flag=x<0; x=x+10; where x<5; run;", fx$source)
  result <- sas_translate(fx$source, config = fx$config, llm = mock_llm(list(good_review())))
  expect_identical(result$status, "migration_ready")
  actual <- readRDS(file.path(result$outputs_dir, "work/out.rds"))
  expect_identical(actual$x, c(NA_real_, 9, 12))
  expect_identical(actual$flag, c(1, 1, 0))
})

test_that("export entry point cannot overwrite a source program named run", {
  fx <- review_public_fixture()
  renamed <- file.path(fx$root, "run.sas")
  file.rename(fx$source, renamed)
  adapter <- counted_review_llm(list(good_translation(review_public_code), good_review()))
  result <- sas_translate(renamed, config = fx$config, llm = adapter$llm)
  expect_identical(result$status, "migration_ready")
  expect_match(sas_code(result), "x <- out\\$x \\+ 10")
  dest <- file.path(fx$root, "delivery")
  sas_write(result, dest)
  entrypoint <- jsonlite::read_json(file.path(dest, "run-order.json"))$entrypoint
  expect_identical(entrypoint, "_run.R")
  unlink(file.path(dest, "work"), recursive = TRUE)
  callr::r(function(entry) source(entry, chdir = TRUE), args = list(entrypoint), wd = dest)
  expect_equal(readRDS(file.path(dest, "work/out.rds"))$x, 11)
})
