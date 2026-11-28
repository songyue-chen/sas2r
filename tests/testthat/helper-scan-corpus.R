# Synthetic SAS-like source for scanner tests. Nothing here is real study code.
#
# scan_corpus_plain(): token soup with strings, comments, macro syntax and
#   unbalanced quotes, but no %str()/%nrstr() and no instream data.
# scan_corpus_instream(): well-formed programs whose instream data blocks end
#   the conventional way (a lone semicolon line, or ;;;; for the "4" forms).
# scan_corpus_triggers(): everything, including %str() text with marked and
#   unmarked delimiters, data blocks ended by code lines, and unclosed forms.
#
# The first two are, by construction, sources the newer scanner rules do not
# apply to. Callers set the seed.
scan_corpus_tokens <- c(
  "data", "set", "run", "proc sql", "quit", "a", "b", "x1", "total", "work.t",
  "=", "+", ",", "1", "2.5", " ", " ", " ", "\n", "\n", ";", ";", ";",
  "(", ")", "'", "\"", "'it''s; here'", "\"x;y\"", "/* note; */", "/*", "*/",
  "* remark", "*", "%", "%let v=", "%macro m(p=1)", "%mend", "%put &=v",
  "%* macro note", "&x", "%m(a, b)", "%if &x %then %do", "%end", "%sysfunc(today())"
)

scan_corpus_plain <- function(n, max_tokens = 40L) {
  vapply(seq_len(n), function(i) {
    paste(sample(scan_corpus_tokens, sample.int(max_tokens, 1L), replace = TRUE), collapse = "")
  }, character(1))
}

scan_corpus_code_block <- function() {
  statements <- c(
    "data a; set b; x = 'p;q'; run;", "proc sort data=a; by x; run;",
    "/* block; comment */", "* statement comment;", "%let v=1;",
    "%macro m(p=1); data _null_; put \"&p;\"; run; %mend;", "%m(p=2)",
    "proc sql;\n  create table t as select * from a\n  ;\nquit;", "x = (1 + 2);"
  )
  paste(sample(statements, sample.int(3L, 1L), replace = TRUE), collapse = "\n")
}

scan_corpus_data_block <- function() {
  word <- function() paste(sample(c("1", "22", "abc", "x y", "3.5", "N/A"), sample.int(3L, 1L), replace = TRUE), collapse = " ")
  indent <- sample(c("", "", "  ", "\t"), 1L)
  if (sample.int(2L, 1L) == 1L) {
    opener <- sample(c("datalines", "cards", "parmcards", "DATALINES"), 1L)
    lines <- replicate(sample.int(4L, 1L), word())
    paste0("data d; input a $ b $; ", opener, ";\n", paste(lines, collapse = "\n"), "\n", indent, ";\nrun;")
  } else {
    opener <- sample(c("datalines4", "cards4", "parmcards4", "Cards4"), 1L)
    # Semicolons may sit inside a line but never begin one.
    lines <- replicate(sample.int(4L, 1L), paste0(word(), sample(c("", ";", "; tail", ";;"), 1L)))
    paste0("data d; input a $ b $; ", opener, ";\n", paste(lines, collapse = "\n"), "\n", indent, ";;;;\nrun;")
  }
}

scan_corpus_instream <- function(n) {
  vapply(seq_len(n), function(i) {
    blocks <- replicate(sample.int(4L, 1L),
      if (sample.int(2L, 1L) == 1L) scan_corpus_data_block() else scan_corpus_code_block())
    paste(blocks, collapse = "\n")
  }, character(1))
}

scan_corpus_triggers <- function(n, max_tokens = 40L) {
  tokens <- c(scan_corpus_tokens,
    "%str(", "%str(", "%NRSTR (", "%nrstr(", "%)", "%(", "%'", "%\"", "%%", "%str(a;b)",
    "%str(it%'s)", "%str(f(x);)", "datalines;\n", "cards;\n", "datalines4;\n",
    "cards4;\n", "\n;;;;\n", "\n;\n", "\nrun;\n", "\n;x\n", "1 2\n", "a;b\n")
  vapply(seq_len(n), function(i) {
    paste(sample(tokens, sample.int(max_tokens, 1L), replace = TRUE), collapse = "")
  }, character(1))
}
