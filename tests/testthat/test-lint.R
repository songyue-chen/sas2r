test_that("clean emitted code lints quietly", {
  code <- 'x <- lib_read("adam", "adsl") |>\n  dplyr::filter((is.na(age) | age < 65))'
  l <- lint_r_code(code)
  expect_identical(nrow(l[l$level == "error", ]), 0L)
  expect_false("unwrapped_comparison" %in% l$kind)
})

test_that("banned functions and parse failures are errors", {
  expect_identical(lint_r_code("system('rm -rf /')")$kind[1], "banned_function")
  expect_identical(lint_r_code("x <- ((")$kind[1], "parse_failure")
})

test_that("unwrapped comparisons are flagged", {
  l <- lint_r_code("y <- dplyr::filter(df, age < 65)")
  expect_true("unwrapped_comparison" %in% l$kind)

  # F19: is.na(other) does not silence unwrapped comparison on age
  l2 <- lint_r_code("dplyr::filter(df, is.na(other) & age < 65)")
  expect_true("unwrapped_comparison" %in% l2$kind)

  # is.na(age) | age < 65 is properly wrapped and not flagged
  l3 <- lint_r_code("dplyr::filter(df, is.na(age) | age < 65)")
  expect_false("unwrapped_comparison" %in% l3$kind)
})

test_that("unknown functions and foreign namespaces are surfaced", {
  l <- lint_r_code("z <- data.table::setDT(df); imaginary_fn(1)")
  expect_true("disallowed_namespace" %in% l$kind)
  expect_true("unknown_function" %in% l$kind)
})

test_that("all banned functions are flagged as errors", {
  for (fn in c("system", "system2", "shell", "download.file", "url",
               "unlink", "file.remove", "Sys.setenv", "source",
               "eval", "parse", "library", "require")) {
    code <- sprintf("%s('arg')", fn)
    res <- lint_r_code(code)
    expect_true("banned_function" %in% res$kind, info = fn)
    expect_identical(res$level[res$kind == "banned_function"], "error")
  }
})

test_that("empty code returns empty tibble with correct schema", {
  res <- lint_r_code("")
  expect_s3_class(res, "tbl_df")
  expect_identical(names(res), c("level", "kind", "detail"))
  expect_identical(nrow(res), 0L)
})

test_that("sas2r helper functions are recognized and not marked unknown", {
  code <- 'res <- sas_sum(1, 2) %+% sas_compress(" a b ") |> sas_round(0.1)'
  l <- lint_r_code(code)
  expect_false("unknown_function" %in% l$kind)
})

test_that("custom allowlist and helpers are respected", {
  code <- "custom_pkg::my_fn(1); my_helper(2)"
  l_default <- lint_r_code(code)
  expect_true("disallowed_namespace" %in% l_default$kind)
  expect_true("unknown_function" %in% l_default$kind)

  l_custom <- lint_r_code(code, allowlist = c("custom_pkg"), helpers = c("my_helper"))
  expect_identical(nrow(l_custom), 0L)
})

test_that("SAS2R_HELPER_NAMES is in sync with sas2r-helpers.R template", {
  # Set-equal in both directions on purpose, and the constant is the whole
  # template surface rather than only what emitted units call -- see
  # ?SAS2R_HELPER_NAMES for why the internal helpers belong in it. A name here
  # the template does not define allows a call nothing can satisfy; a name the
  # template defines and this omits is a call lint_r_code() would reject.
  e <- new.env(parent = baseenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e)
  expect_setequal(SAS2R_HELPER_NAMES, ls(e, all.names = TRUE))
})

test_that("every helper has a help page: its own if exported, the runtime topic otherwise", {
  # Documentation is the third leg of the contract: a helper cannot be added
  # to the source and the allowlist without a place in the reference. The
  # operators and the two S3 methods cannot be Rd aliases, so they are checked
  # in the runtime topic's text instead.
  src_man <- test_path("..", "..", "man")
  db <- if (dir.exists(src_man)) {
    tools::Rd_db(dir = test_path("..", ".."))
  } else {
    tools::Rd_db("sas2r")
  }
  rd_tags <- function(rd) vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
  aliases <- unlist(lapply(db, function(rd) {
    unlist(lapply(rd[rd_tags(rd) == "\\alias"], function(x) as.character(x[[1]])))
  }), use.names = FALSE)
  topic <- db[["sas2r_runtime.Rd"]]
  expect_false(is.null(topic))
  topic_text <- paste(unlist(lapply(topic, function(x) paste(unlist(x), collapse = ""))), collapse = "")

  not_aliasable <- c("%+%", "%notin%", "$.sas2r_dataset", "[[.sas2r_dataset")
  expect_true(all(setdiff(SAS2R_HELPER_NAMES, not_aliasable) %in% aliases))
  expect_true(grepl("%notin%", topic_text, fixed = TRUE))
  expect_true(grepl("%+%", topic_text, fixed = TRUE))
  expect_true(grepl("sas2r_dataset", topic_text, fixed = TRUE))
  # every exported helper has a page of its own (a usage section), and every
  # helper the runtime topic aliases is one the package deliberately keeps
  # internal
  exported <- intersect(SAS2R_HELPER_NAMES, getNamespaceExports("sas2r"))
  own_page <- vapply(exported, function(nm) {
    any(vapply(db, function(rd) {
      nm %in% unlist(lapply(rd[rd_tags(rd) == "\\alias"], function(x) as.character(x[[1]]))) &&
        any(rd_tags(rd) == "\\usage")
    }, logical(1)))
  }, logical(1))
  expect_true(all(own_page), info = paste(exported[!own_page], collapse = ", "))
  topic_aliases <- unlist(lapply(topic[rd_tags(topic) == "\\alias"], function(x) as.character(x[[1]])))
  expect_length(intersect(topic_aliases, exported), 0L)
})

test_that("the vendored runtime is the package's runtime, function for function", {
  # The bundle template is rendered from R/runtime-*.R; whatever the delivery
  # form, the code must be identical. removeSource() strips srcrefs so the
  # comparison is of code, not of comments or layout.
  e <- new.env(parent = baseenv())
  sys.source(system.file("templates", "sas2r-helpers.R", package = "sas2r"), e,
             keep.source = FALSE)
  ns <- asNamespace("sas2r")
  for (nm in SAS2R_HELPER_NAMES) {
    expect_identical(
      deparse(removeSource(get(nm, envir = e, inherits = FALSE))),
      deparse(removeSource(get(nm, envir = ns, inherits = FALSE))),
      info = nm
    )
  }
})

test_that("the committed template is what the runtime sources render to", {
  src <- test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "source-tree template contract")
  rendered <- runtime_template_lines(src)
  committed <- readLines(test_path("..", "..", "inst", "templates", "sas2r-helpers.R"), warn = FALSE)
  expect_identical(
    committed, rendered,
    info = "inst/templates/sas2r-helpers.R is stale: run Rscript tools/build-runtime-template.R"
  )
})

test_that("write_helpers() stamps the vendored runtime with the generating version", {
  out <- withr::local_tempdir()
  write_helpers(out)
  lines <- readLines(file.path(out, "sas2r-helpers.R"), warn = FALSE)
  expect_match(lines[[1]], "^# sas2r runtime helpers -- vendored by sas2r [0-9.]+")
  # the stamp replaces the template's own first line and changes nothing else
  template <- readLines(system.file("templates", "sas2r-helpers.R", package = "sas2r"), warn = FALSE)
  expect_identical(lines[-1], template[-1])
  e <- new.env(parent = baseenv())
  sys.source(file.path(out, "sas2r-helpers.R"), e)
  expect_setequal(SAS2R_HELPER_NAMES, ls(e, all.names = TRUE))
})

test_that("parenthesized negation !(is.na(v)) is recognized without false warning", {
  code <- "dplyr::filter(df, !(is.na(age)) & age > 65)"
  l <- lint_r_code(code)
  expect_false("unwrapped_comparison" %in% l$kind)
})

test_that("empty index arguments do not crash the linter", {
  # `arg <- e[[i]]` binds the empty symbol to a variable, and is.null(arg) then
  # forces that binding, raising "argument \"arg\" is missing, with no default".
  # The guard for the empty symbol sat second in the && and never ran. Any
  # generated R using x[, 1] or df[1, ] -- the most ordinary indexing there is,
  # and unavoidable when translating SAS -- aborted the lint gate, which is
  # what failed every macro unit in the validation project.
  for (code in c("y <- x[, 1]", "y <- df[1, ]", "y <- m[, , 2]",
                 "y <- df[df$a > 1, ]", "f <- function(a, b) a")) {
    expect_no_error(lint_r_code(code), message = code)
  }
})

test_that("a lint finding is still reported through an empty index argument", {
  # The walk must continue past the empty slot rather than stopping there.
  lint <- lint_r_code("y <- df[, system('rm -rf /')]")

  expect_true(nrow(lint) > 0L)
})

test_that("parse failures are still reported as errors", {
  lint <- lint_r_code("y <- (")

  expect_true(any(lint$level == "error"))
  expect_true(any(lint$kind == "parse_failure"))
})

test_that("non-canonical lib_read/lib_write calls are lint errors that teach the canonical form", {
  bad <- c(
    'lib_write("adam.adsl", adsl)',
    'lib_write(adsl, "adam.adsl")',
    'lib_read("adam.adsl")',
    'lib_read("adsl")',
    'lib_write(adsl, "adam")',
    'lib_write(adsl, "adam", dataset = "adsl")',
    'lib_read("adam", table = "adsl")'
  )
  for (code in bad) {
    res <- lint_r_code(code)
    hit <- res[res$kind == "helper_misuse", , drop = FALSE]
    expect_identical(nrow(hit), 1L, info = code)
    expect_identical(hit$level, "error", info = code)
    expect_match(hit$detail, 'lib_(read|write)\\((df, )?"lib", "member"\\)', info = code)
  }

  good <- c(
    'adsl <- lib_read("adam", "adsl")',
    'lib_write(adsl, "adam", "adsl")',
    'lib_write(adsl, "adam", member = "adsl")',
    'x <- lib_read(lib, mem)',          # symbolic arguments: judged at runtime
    'lib_write(df, "work", "b") |> invisible()'
  )
  for (code in good) {
    res <- lint_r_code(code)
    expect_false(any(res$kind == "helper_misuse"), info = code)
  }
})
