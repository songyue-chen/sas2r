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
