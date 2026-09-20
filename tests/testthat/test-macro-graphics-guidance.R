skill_example_code <- function(name) {
  lines <- strsplit(agent_skill_catalog()[[name]]$body, "\n", fixed = TRUE)[[1L]]
  start <- which(lines == "```r")
  end <- which(lines == "```")
  paste(unlist(lapply(start, function(i) lines[seq.int(i + 1L, min(end[end > i]) - 1L)])), collapse = "\n")
}

test_that("macro guidance reaches translators, reviewers and fixers for callers and definitions", {
  for (source in c("%macro calculate(x); %local answer; %let answer=&x; %mend;",
                   "%calculate(1);", "data _null_; call symputx('answer', x); run;")) {
    for (role in c("translator", "reviewer", "fixer")) {
      skills <- route_agent_skills(list(agent = role, flags = skill_flags_from_sas(source)))
      text <- render_agent_skills(skills)
      expect_match(text, "sas-macro-execution", fixed = TRUE)
      expect_match(text, "Do not reset every variable", fixed = TRUE)
      expect_match(text, "program must also execute|must also execute", perl = TRUE)
    }
  }
  expect_false("macro_execution" %in% skill_flags_from_sas("data out; set in; run;"))
})

test_that("the shipped scope example replaces cleared results and retains intentional state", {
  env <- new.env(parent = baseenv())
  env$limits <- c(700, 900) # A different outer binding must not leak in.
  eval(parse(text = skill_example_code("sas-macro-execution")), env)
  panels <- env$build_panels(list(c(10, 20, NA), numeric(), c(100, 200)), per_page = 2L)
  expect_length(panels, 3L)
  expect_identical(lapply(panels, `[[`, "limits"), list(c(10, 20), numeric(), c(100, 200)))
  expect_identical(vapply(panels, `[[`, 0L, "page"), c(1L, 1L, 2L))
  expect_identical(env$limits, c(700, 900))
  expect_length(env$build_panels(list()), 0L)
  expect_identical(vapply(env$build_panels(rep(list(1), 3), per_page = 1L), `[[`, 0L, "page"), 1:3)
})

test_that("the shipped plot example sends source-defined whiskers to the renderer", {
  file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  withr::defer(grDevices::dev.off())
  received <- NULL
  draw <- graphics::bxp
  testthat::local_mocked_bindings(bxp = function(z, ...) {
    received <<- z
    draw(z, ...)
  }, .package = "graphics")
  env <- new.env(parent = baseenv())
  eval(parse(text = skill_example_code("sas-statistical-defaults")), env)
  # Literal acceptance values derived from the seven ordered observations;
  # assert the renderer's argument, not a separate summary calculation.
  expect_equal(as.numeric(received$stats), c(0, 1, 3, 5, 5))
  expect_equal(received$out, 20)
  expect_identical(received$group, 1L)
  expect_equal(received$n, 7)
  expect_identical(received$names, "Group A")
  expect_true(file.exists(file))
})

test_that("native graphics guidance reaches template definitions and callers in all roles", {
  for (source in c("proc template; define statgraph example; begingraph; endgraph; end; run;",
                   "ods pdf file='plot.pdf'; proc sgrender data=work.summary template=example; run; ods pdf close;",
                   "proc sgplot data=work.summary; scatter x=x y=y; run;",
                   "proc sgpanel data=work.summary; panelby group; scatter x=x y=y; run;",
                   "ods html; proc sgscatter data=work.summary; compare x=x y=y; run;",
                   "ods rtf; proc gplot data=work.summary; plot y*x; run;",
                   "proc gchart data=work.summary; vbar group; run;",
                   "proc boxplot data=work.summary; plot y*group; run;",
                   "ods graphics on;")) {
    for (role in c("translator", "reviewer", "fixer")) {
      text <- render_agent_skills(route_agent_skills(list(agent = role, flags = skill_flags_from_sas(source))))
      expect_match(text, "sas-native-graphics", fixed = TRUE)
      expect_match(text, "caller must capture and use it", fixed = TRUE)
      expect_match(text, "read_dependency_context", fixed = TRUE)
    }
  }
  for (source in c("ods pdf file='listing.pdf'; proc print data=work.listing; run; ods pdf close;",
                   "ods pdf file='table.pdf'; proc report data=work.summary; run; ods pdf close;")) {
    for (role in c("translator", "reviewer", "fixer")) {
      text <- render_agent_skills(route_agent_skills(list(agent = role, flags = skill_flags_from_sas(source))))
      expect_false(grepl("sas-native-graphics", text, fixed = TRUE))
    }
  }
  code <- skill_example_code("sas-native-graphics")
  expect_false(any(lint_r_code(code)$kind == "disallowed_namespace"))
  expect_true(any(lint_r_code(code, allowlist = c("base", "stats"))$kind == "disallowed_namespace"))
  expect_true(all(c("graphics", "grDevices", "grid") %in% agent_package_facts()$allowed))
})

test_that("a returned native template is consumed and preserves statistics on both PDF pages", {
  env <- new.env(parent = baseenv())
  eval(parse(text = skill_example_code("sas-native-graphics")), env)
  # Acceptance values from the same seven-observation definition-5 example
  # above. Rendering must consume these values rather than recompute hinges.
  box <- list(stats = matrix(c(0, 1, 3, 5, 5), ncol = 1), n = 7,
    conf = matrix(NA_real_, 2, 1), out = 20, group = 1L, names = "Treatment A")
  labels <- c("Treatment", "n", "Mean", "Std Dev", "Min", "Q1", "Median", "Q3", "Max")
  page <- list(box = box, notch = FALSE, means = 5, references = c(2, 8),
    ylim = c(0, 25), ylab = "Observed value", title = "Visit 1",
    table = cbind(labels, c("A", "7", "5", "6.83", "0", "1", "3", "5", "20")),
    footnote = "Source summary and IQR outliers")
  second <- page; second$title <- "Visit 2"
  received <- list(); original <- graphics::bxp
  testthat::local_mocked_bindings(bxp = function(z, ...) {
    received[[length(received) + 1L]] <<- z
    original(z, ...)
  }, .package = "graphics")
  path <- file.path(withr::local_tempdir(), "new-output-folder", "summary.pdf")
  device_before <- grDevices::dev.cur()
  env$draw_summary(list(page, second), path)
  expect_identical(grDevices::dev.cur(), device_before)
  expect_length(received, 2)
  for (z in received) {
    expect_identical(z, box)
    expect_equal(as.numeric(z$stats), c(0, 1, 3, 5, 5))
    expect_identical(z$out, 20)
  }
  expect_gt(file.info(path)$size, 1000)
  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_equal(pdftools::pdf_info(path)$pages, 2)
    pages <- pdftools::pdf_text(path)
    for (i in 1:2) {
      for (label in c(labels, "Observed value", "Source summary and IQR outliers", paste("Visit", i))) {
        expect_match(pages[[i]], label, fixed = TRUE)
      }
    }
  }
})
