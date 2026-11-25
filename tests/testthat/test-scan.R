test_that("semicolon inside single-quoted string is masked as string", {
  sc <- sas_scan("x = 'a;b';")
  expect_identical(sc$mask[which(sc$chars == ";")[1]], "s")
  expect_identical(sc$mask[which(sc$chars == ";")[2]], "c")
})

test_that("doubled quotes stay inside the string", {
  sc <- sas_scan("y = 'it''s; fine';")
  semis <- which(sc$chars == ";")
  expect_identical(sc$mask[semis[1]], "s")
  expect_identical(sc$mask[semis[2]], "c")
})

test_that("block comments are masked k, including semicolons", {
  sc <- sas_scan("a /* x; y */ b;")
  expect_identical(sc$mask[which(sc$chars == ";")[1]], "k")
})

test_that("star comment at statement start runs to first semicolon", {
  sc <- sas_scan("* note; data a;")
  expect_identical(sc$mask[1], "k")
  expect_identical(sc$mask[which(sc$chars == ";")[1]], "c") # terminator returns to code
  expect_identical(sc$mask[which(sc$chars == "d")[1]], "c") # 'data' is code
})

test_that("percent-star macro comment is masked", {
  sc <- sas_scan("%* macro note; data a;")
  expect_identical(sc$mask[1], "k")
})

test_that("datalines content is masked d until lone semicolon line", {
  src <- "data a;\ninput x $;\ndatalines;\nfoo bar\nbaz\n;\nrun;"
  sc <- sas_scan(src)
  foo_pos <- regexpr("foo", src)[1]
  expect_identical(sc$mask[foo_pos], "d")
  run_pos <- regexpr("run", src)[1]
  expect_identical(sc$mask[run_pos], "c")
})

test_that("datalines4 content is masked d until the ;;;; line, semicolons included", {
  src <- "data a;\ninput x $;\ndatalines4;\nfoo;bar\nbaz\n;;;;\nrun;"
  sc <- sas_scan(src)
  foo_pos <- regexpr("foo", src)[1]
  expect_identical(sc$mask[foo_pos], "d")
  inner_semi <- foo_pos + 3
  expect_identical(sc$chars[inner_semi], ";")
  expect_identical(sc$mask[inner_semi], "d")
  run_pos <- regexpr("run", src)[1]
  expect_identical(sc$mask[run_pos], "c")
})

test_that("statements split only at code semicolons, comments stripped", {
  s <- sas_statements("data a; /* junk; */ x = 'a;b'; run;")
  expect_identical(nrow(s), 3L)
  expect_identical(s$text[2], "x = 'a;b'")
  expect_identical(s$first_token, c("data", "x", "run"))
})

test_that("line numbers are recorded", {
  s <- sas_statements("data a;\n  x = 1;\nrun;")
  expect_identical(s$line_start, c(1L, 2L, 3L))
})

test_that("trailing statement without semicolon is kept", {
  s <- sas_statements("data a; x = 1")
  expect_identical(nrow(s), 2L)
  expect_identical(s$text[2], "x = 1")
})

test_that("datalines rows come through as one datalines_data statement", {
  s <- sas_statements("data a;\ninput x $;\ndatalines;\nfoo bar\nbaz\n;\nrun;")
  expect_identical(nrow(s), 5L)
  expect_identical(s$type[4], "datalines_data")
  expect_identical(s$first_token[3], "datalines")
})

test_that("star comments produce no code statements", {
  s <- sas_statements("* just a note;\ndata a; run;")
  expect_identical(s$first_token[s$type == "code"], c("data", "run"))
})

test_that("macro call without trailing semicolon does not swallow next statement", {
  s <- sas_statements("%setup()\ndata a;\nx=1;\nrun;")
  expect_identical(s$first_token, c("%setup", "data", "x", "run"))
  expect_identical(s$line_start, c(1L, 2L, 3L, 4L))
  expect_identical(s$text[1], "%setup()")
  expect_identical(s$text[2], "data a")
})

test_that("datalines payload content is preserved in statement text", {
  s <- sas_statements("data a;\ninput x y;\ndatalines;\n1 2\n3 4\n;\nrun;")
  expect_identical(s$type[4], "datalines_data")
  expect_identical(s$text[4], "1 2\n3 4")
})

test_that("parmcards4 is recognized as datalines block", {
  s <- sas_statements("proc explode; parmcards4;\na;b\n;;;;\nrun;")
  expect_true(any(s$type == "datalines_data"))
  expect_identical(s$text[s$type == "datalines_data"], "a;b")
})

test_that("line_start is accurate for statements after blank lines", {
  s <- sas_statements("data a;\n\nx=1;\nrun;")
  expect_identical(s$line_start, c(1L, 3L, 4L))
  expect_identical(s$line_end, c(1L, 3L, 4L))
})

test_that("datalines rows have empty first_token and do not forge keywords", {
  s <- sas_statements("data a;\ninput w $ x;\ndatalines;\nproc 1\n%macro x\nlibname fake '/tmp/x'\n;\nrun;")
  dl_row <- s[s$type == "datalines_data", ]
  expect_identical(nrow(dl_row), 1L)
  expect_identical(dl_row$first_token, "")
  expect_match(dl_row$text, "proc 1")
})

test_that("split_macro_statement handles multi-line tails, chains, and nested parens", {
  s1 <- sas_statements("%setup()\ndata out1\n  out2; x=1; run;")
  expect_identical(s1$first_token, c("%setup", "data", "x", "run"))
  expect_identical(s1$text[2], "data out1\n  out2")

  s2 <- sas_statements("%a()\n%b()\ndata x; run;")
  expect_identical(s2$first_token, c("%a", "%b", "data", "run"))

  s3 <- sas_statements("%m(f(g(x)))\ndata a; run;")
  expect_identical(s3$first_token, c("%m", "data", "run"))

  s4 <- sas_statements("%setup()\n\n\ndata a; run;")
  expect_identical(s4$first_token, c("%setup", "data", "run"))
  expect_identical(s4$line_start, c(1L, 4L, 4L))
})

test_that("statement-form macro keywords are not falsely split across newlines", {
  s1 <- sas_statements("%include\n'/org/setup.sas';")
  expect_identical(nrow(s1), 1L)
  expect_identical(s1$first_token, "%include")
  expect_identical(s1$text, "%include\n'/org/setup.sas'")

  s2 <- sas_statements("%put\nNOTE all good;")
  expect_identical(nrow(s2), 1L)
  expect_identical(s2$first_token, "%put")

  s3 <- sas_statements("%copy\nmymac / source;")
  expect_identical(nrow(s3), 1L)
  expect_identical(s3$first_token, "%copy")
})

test_that("macro call with argument list wrapped onto next line is not split", {
  s <- sas_statements("%create_report\n  (data=adsl, out=t1);\nrun;")
  expect_identical(nrow(s), 2L)
  expect_identical(s$first_token, c("%create_report", "run"))
  expect_identical(s$text[1], "%create_report\n  (data=adsl, out=t1)")
})

test_that("source records retain exact SAS comments and private spans", {
  src <- paste(c(
    "/* leading block */",
    "data a;",
    "  x = '/* string, not comment */';",
    "  * statement note;",
    "  %* macro note;",
    "  y = 1 /* inline block */;",
    "run;"
  ), collapse = "\n")

  records <- sas2r:::sas_source_records(src)

  expect_identical(
    records$comments$text,
    c("/* leading block */", "* statement note;",
      "%* macro note;", "/* inline block */")
  )
  expect_identical(records$comments$kind,
                   c("block", "statement", "macro", "block"))
  expect_identical(records$comments$comment_id, 1:4)
  expect_true(all(records$comments$char_start <= records$comments$char_end))
  expect_true(all(records$statements$char_start <= records$statements$char_end))

  public <- sas_statements(src)
  expect_identical(
    names(public),
    c("stmt_id", "text", "first_token", "type", "line_start", "line_end")
  )
  expect_false(any(grepl("leading block|statement note|macro note|inline block",
                         public$text)))
})

test_that("comment-looking strings and datalines are not comment records", {
  src <- paste(c(
    "data a;",
    "  x = '/* not a block */';",
    "  y = \"%* not a macro comment;\";",
    "  input raw $;",
    "  datalines;",
    "/* payload */",
    ";",
    "run;"
  ), collapse = "\n")

  records <- sas2r:::sas_source_records(src)

  expect_identical(nrow(records$comments), 0L)
  expect_true(any(records$statements$type == "datalines_data"))
})

test_that("private spans preserve order across an unsemicoloned macro split", {
  src <- "%setup()\n/* for the data step */\ndata a; x = 1; run;"
  records <- sas2r:::sas_source_records(src)

  expect_identical(records$statements$first_token[1:2], c("%setup", "data"))
  expect_lt(records$statements$char_end[1], records$statements$char_start[2])
  expect_gt(records$comments$char_start[1], records$statements$char_end[1])
  expect_lt(records$comments$char_end[1], records$statements$char_start[2])
})

test_that("empty datalines payload has an integer source span", {
  src <- "data a;\ndatalines;\n;\nrun;"
  records <- sas2r:::sas_source_records(src)
  payload <- records$statements[records$statements$type == "datalines_data", ]

  expect_identical(payload$text, "")
  expect_identical(payload$char_start, 19L)
  expect_identical(payload$char_end, 19L)
})

test_that("adjacent comments retain distinct exact source records", {
  src <- "/* block */%* macro;\ndata a; run;"
  records <- sas2r:::sas_source_records(src)

  expect_identical(
    records$comments,
    tibble::tibble(
      comment_id = 1:2,
      text = c("/* block */", "%* macro;"),
      kind = c("block", "macro"),
      line_start = c(1L, 1L),
      line_end = c(1L, 1L),
      char_start = c(1L, 12L),
      char_end = c(11L, 20L)
    )
  )
})

# %str()/%nrstr() arguments ------------------------------------------------

statement_text <- function(src) sas_statements(src)$text

test_that("a semicolon inside %str() or %nrstr() does not end the statement", {
  expect_identical(statement_text("%let x=%str(a;b);\ndata y; set z; run;"),
    c("%let x=%str(a;b)", "data y", "set z", "run"))
  expect_identical(statement_text("%let p=%NRSTR  (&x; %y;);\nrun;"),
    c("%let p=%NRSTR  (&x; %y;)", "run"))
  # Only that semicolon is tagged; the argument text keeps its ordinary tags,
  # so it stays in the statement exactly as written.
  sc <- sas_scan("%let x=%str(a;b);")
  expect_identical(paste(sc$mask, collapse = ""), "cccccccccccccmccc")
  expect_null(sc$fallbacks)
})

test_that("parentheses inside the argument are counted", {
  expect_identical(statement_text("%let c=%str(if (a>1) then b=f(x););\ndata y; set z; &c run;"),
    c("%let c=%str(if (a>1) then b=f(x);)", "data y", "set z", "&c run"))
  expect_identical(statement_text("%let c=%str(a %str(;) b;);\nrun;"),
    c("%let c=%str(a %str(;) b;)", "run"))
})

test_that("marked delimiters are text: they neither count nor open a string", {
  # Before this, the marked quotation mark opened a string that swallowed the
  # code after it.
  expect_identical(statement_text("%let q=%str(it%'s);\ndata y; set z; run;\n%let r='x';"),
    c("%let q=%str(it%'s)", "data y", "set z", "run", "%let r='x'"))
  expect_identical(statement_text("%let q=%str(say %\"hi;);\nrun;"),
    c("%let q=%str(say %\"hi;)", "run"))
  expect_identical(statement_text("%let p=%str(a%);b);\nrun;"), c("%let p=%str(a%);b)", "run"))
  expect_identical(statement_text("%let p=%str(a%(;b);\nrun;"), c("%let p=%str(a%(;b)", "run"))
  # %% is a percent sign, so the parenthesis after it closes the argument.
  expect_identical(statement_text("%let p=%str(20%%);\nx=1;"), c("%let p=%str(20%%)", "x=1"))
  # A matched string inside the argument is still a string.
  expect_identical(statement_text("%let p=%str('a);b' c;);\nrun;"), c("%let p=%str('a);b' c;)", "run"))
})

test_that("only %str and %nrstr are followed", {
  expect_identical(statement_text("%let p=%strx(a;b);\nrun;"), c("%let p=%strx(a", "b)", "run"))
  expect_identical(statement_text("%let p=%bquote(a;b);\nrun;"), c("%let p=%bquote(a", "b)", "run"))
  expect_identical(statement_text("x = \"%str(a;b)\"; run;"), c("x = \"%str(a;b)\"", "run"))
  expect_identical(statement_text("/* %str( */ a; b;"), c("a", "b"))
})

test_that("an unclosed argument falls back to the previous splitting and is reported", {
  src <- "%let p=%str(a;b;\ndata y; set z; run;\nproc print; run;"
  expect_identical(sas_scan(src)$mask, legacy_sas_scan(src)$mask)
  expect_identical(statement_text(src), c("%let p=%str(a", "b", "data y", "set z", "run", "proc print", "run"))
  fallbacks <- sas_scan(src)$fallbacks
  expect_identical(fallbacks$kind, "macro_quote_unclosed")
  expect_identical(fallbacks$position, as.integer(regexpr("%str", src, fixed = TRUE)))
  findings <- sas_source_records(src)$findings
  expect_identical(findings$kind, "scanner_fallback")
  expect_identical(findings$line, 1L)
  expect_match(findings$reason, "not closed", fixed = TRUE)

  # A closed argument earlier in the same source still benefits.
  src <- "%let a=%str(x;y);\n%let p=%str(a;b;\nrun;"
  expect_identical(statement_text(src), c("%let a=%str(x;y)", "%let p=%str(a", "b", "run"))
  expect_identical(sas_source_records(src)$findings$line, 2L)
  expect_identical(nrow(sas_source_records("%let a=%str(x;y);")$findings), 0L)
})

test_that("a source with many unclosed constructs is read entirely by the previous rules", {
  src <- paste(c("%let ok=%str(a;b);", rep("%let p=%str(a;b;", SCAN_FALLBACK_PASS_LIMIT + 5L), "run;"), collapse = "\n")
  sc <- sas_scan(src)
  expect_identical(sc$mask, legacy_sas_scan(src)$mask)
  expect_identical(utils::tail(sc$fallbacks$kind, 1L), "scan_rules_disabled")
  expect_lte(nrow(sc$fallbacks), SCAN_FALLBACK_PASS_LIMIT + 1L)
})

# Instream data ------------------------------------------------------------

test_that("plain instream data ends at the first line holding a semicolon", {
  # No lone semicolon line: before this, everything after `datalines;` was data.
  s <- sas_statements("data a; input x y; datalines;\n1 2\n3 4\nrun;\nproc print data=a; run;\ndata b; set a; run;")
  expect_identical(s$text, c("data a", "input x y", "datalines", "1 2\n3 4", "run",
    "proc print data=a", "run", "data b", "set a", "run"))
  expect_identical(s$type[4], "datalines_data")
  expect_identical(s$first_token[5:6], c("run", "proc"))
  expect_identical(s$line_start[4:6], c(2L, 4L, 5L))

  # The ending line may be indented and may hold several statements.
  expect_identical(statement_text("data a; input x; cards;\n1\n2\n  proc print data=a; run;"),
    c("data a", "input x", "cards", "1\n2", "proc print data=a", "run"))

  # A later data block keeps its own data.
  s <- sas_statements("data a; input x; datalines;\n1\nrun;\ndata b; input y; datalines;\n2\n;\nrun;")
  expect_identical(s$text[s$type == "datalines_data"], c("1", "2"))
  expect_identical(sum(s$first_token == "run"), 2L)
})

test_that("datalines4 ends only at a line beginning with ;;;;", {
  s <- sas_statements("data a; input x $; datalines4;\n;x\nc;d\n ;\n;;;;\nrun;\nproc print; run;")
  expect_identical(s$text, c("data a", "input x $", "datalines4", ";x\nc;d\n ;", "run", "proc print", "run"))
  # An indented terminator is accepted, as it was before.
  expect_identical(trimws(statement_text("data a; input x $; cards4;\na;b\n  ;;;;\nrun;")),
    c("data a", "input x $", "cards4", "a;b", "run"))
})

test_that("a datalines4 block with no ;;;; line falls back and is reported", {
  src <- "data a; input x $; datalines4;\na;b\n;\nrun;\nproc print; run;"
  expect_identical(sas_scan(src)$mask, legacy_sas_scan(src)$mask)
  expect_identical(statement_text(src), c("data a", "input x $", "datalines4", "a;b", "run", "proc print", "run"))
  expect_identical(sas_scan(src)$fallbacks$kind, "datalines4_unterminated")
  findings <- sas_source_records(src)$findings
  expect_identical(findings$line, 1L)
  expect_match(findings$reason, ";;;;", fixed = TRUE)
})

test_that("macro call scanning sees a macro-quoted semicolon as text", {
  units <- sas_units(sas_statements("%let x=%str(a; %inner(1););\n%outer(2);"))
  expect_identical(units$text, c("%let x=%str(a; %inner(1);)", "%outer(2)"))
})

test_that("scanner fallbacks reach the project findings with their file and line", {
  dir <- withr::local_tempdir()
  writeLines(c("%let ok=%str(a;b);", "%let p=%str(a;b;", "data y; set z; run;"), file.path(dir, "p.sas"))
  writeLines("data q; set y; run;", file.path(dir, "q.sas"))
  flags <- sas_project(dir, cache = FALSE)$flags
  hit <- flags[flags$kind == "scanner_fallback", ]
  expect_identical(nrow(hit), 1L)
  expect_match(hit$detail, "p.sas:2: ", fixed = TRUE)
  expect_false("scanner_fallback" %in% preflight_blocking_findings())

  # A cached scan serves another file with the same content under its own name.
  file.copy(file.path(dir, "p.sas"), file.path(dir, "r.sas"))
  cached <- sas_project(dir)$flags
  cached <- sas_project(dir)$flags
  details <- cached$detail[cached$kind == "scanner_fallback"]
  expect_length(details, 2L)
  expect_true(any(grepl("p.sas:2: ", details, fixed = TRUE)))
  expect_true(any(grepl("r.sas:2: ", details, fixed = TRUE)))
})

