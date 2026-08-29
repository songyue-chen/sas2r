helpers_env <- function() {
  e <- new.env(parent = globalenv())
  f <- system.file("templates", "sas2r-helpers.R", package = "sas2r")
  if (!nzchar(f) || !file.exists(f)) {
    stop("Template file 'inst/templates/sas2r-helpers.R' not found", call. = FALSE)
  }
  sys.source(f, e)
  e
}

test_that("numeric helpers reproduce SAS semantics", {
  h <- helpers_env()
  # SAS SUM()/MEAN() are row-wise across arguments; one argument passes through
  # elementwise (SAS sum(x) is x, missing stays missing). Column aggregation is
  # emitted inline by the PROC MEANS / PROC SQL emitters, never through these.
  expect_identical(h$sas_sum(c(1, NA, 2)), c(1, NA, 2))
  expect_identical(h$sas_sum(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
  expect_identical(h$sas_mean(c(1, NA, 3)), c(1, NA, 3))
  expect_identical(h$sas_mean(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
  expect_identical(h$sas_round(0.5), 1)                 # half away from zero
  expect_identical(h$sas_round(-0.5), -1)
  expect_identical(h$sas_round(2.5), 3)                 # not banker's 2
  expect_identical(h$sas_round(-2.5), -3)
  expect_identical(h$sas_compress(" a b c "), "abc")
  expect_identical(h$sas_compress("12-34", "-"), "1234")
})

test_that("sas_sort puts missings first and is stable", {
  h <- helpers_env()
  df <- data.frame(x = c(2, NA, 1), tag = c("a", "b", "c"))
  out <- h$sas_sort(df, by = "x")
  expect_identical(out$tag, c("b", "c", "a"))
  out2 <- h$sas_sort(df, by = "x", descending = "x")
  expect_identical(out2$x[1], 2)                        # SAS descending: missing last
  expect_true(is.na(out2$x[3]))

  # Character columns
  df_char <- data.frame(k = c("b", NA, "a"), v = 1:3)
  out_char <- h$sas_sort(df_char, by = "k")
  expect_identical(out_char$v, c(2L, 3L, 1L))
  out_char_desc <- h$sas_sort(df_char, by = "k", descending = "k")
  expect_identical(out_char_desc$v, c(1L, 3L, 2L))
})

test_that("sas_merge: match-merge, right overwrites overlap, loud on m:m", {
  h <- helpers_env()
  a <- data.frame(k = c(1, 2), v = c("a1", "a2"), only_a = TRUE)
  b <- data.frame(k = c(2, 3), v = c("b2", "b3"))
  out <- h$sas_merge(a, b, by = "k", keep = "left")
  expect_identical(out$v[out$k == 2], "b2")             # right wins on overlap
  expect_identical(nrow(out), 2L)

  inner <- h$sas_merge(a, b, by = "k", keep = "both")
  expect_identical(inner$k, 2)

  right <- h$sas_merge(a, b, by = "k", keep = "right")
  expect_identical(right$k, c(2, 3))

  left_only <- h$sas_merge(a, b, by = "k", keep = "left_only")
  expect_identical(left_only$k, 1)

  right_only <- h$sas_merge(a, b, by = "k", keep = "right_only")
  expect_identical(right_only$k, 3)

  full <- h$sas_merge(a, b, by = "k", keep = "full")
  expect_identical(nrow(full), 3L)

  mm_a <- data.frame(k = c(1, 1), x = 1:2)
  mm_b <- data.frame(k = c(1, 1), y = 3:4)
  expect_error(h$sas_merge(mm_a, mm_b, by = "k"), "many-to-many")
})

test_that("string and math helpers reproduce SAS semantics", {
  h <- helpers_env()
  # Substr
  expect_identical(h$sas_substr("abcdef", 4, 3), "def")
  expect_identical(h$sas_substr("abcdef", 4), "def")
  expect_identical(h$sas_substr("abc", 2, 10), "bc")

  # Length
  expect_identical(h$sas_length("abc  "), 3L)
  expect_identical(h$sas_length(""), 1L)
  expect_identical(h$sas_length(NA_character_), 1L)

  # Min and Max with NAs
  expect_identical(h$sas_min(1, NA, 3), 1)
  expect_identical(h$sas_min(c(1, 5), c(2, NA)), c(1, 5))
  expect_identical(h$sas_max(c(1, 5), c(2, NA)), c(2, 5))
  expect_identical(h$sas_min(NA_real_, NA_real_), NA_real_)
  # A single argument is elementwise like SAS MIN(x)/MAX(x), never a column
  # aggregate that would collapse a mutate() to one recycled scalar.
  expect_identical(h$sas_min(c(2, NA)), c(2, NA))
  expect_identical(h$sas_max(c(2, NA)), c(2, NA))
})

test_that("sas_sum and sas_mean are row-wise across arguments like SAS SUM()/MEAN()", {
  h <- helpers_env()
  expect_identical(h$sas_sum(c(1, NA), c(2, 3)), c(3, 3))
  expect_identical(h$sas_sum(c(NA_real_, 1), c(NA_real_, 2)), c(NA_real_, 3))
  expect_identical(h$sas_sum(c(1, 2), 10), c(11, 12))   # scalar arg recycles
  expect_identical(h$sas_mean(c(2, NA), c(4, 4)), c(3, 4))
  expect_identical(h$sas_mean(1, 2, 3), 2)
  # Regression: mutate(z = sas_sum(a, b)) must not collapse two columns into
  # one grand total recycled down every row.
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_identical(h$sas_sum(df$a, df$b), c(4, 6))
})

test_that("sas_sort handles case-insensitivity, missing by vars, and per-column NA descending", {
  h <- helpers_env()
  df <- data.frame(USUBJID = c("b", "a"), V = 1:2)
  # Case insensitivity of by variable
  out <- h$sas_sort(df, by = "usubjid")
  expect_identical(out$USUBJID, c("a", "b"))

  # Error on missing by variable
  expect_error(h$sas_sort(df, by = "nonexistent"), "not found in dataset")

  # Per-column descending NA placement (F16)
  df_mixed <- data.frame(grp = c("A", "A", "A"), val = c(5, NA, 9))
  out_mixed <- h$sas_sort(df_mixed, by = c("grp", "val"), descending = "val")
  expect_identical(out_mixed$val, c(9, 5, NA))   # NA comes last on descending
})

test_that("sas_merge preserves Date class during overlap resolution", {
  h <- helpers_env()
  a <- data.frame(k = 1:2, d = as.Date(c("2020-01-01", "2020-01-02")), v = "a")
  b <- data.frame(k = 2:3, d = as.Date(c("2020-02-02", "2020-02-03")), v = "b")
  res <- h$sas_merge(a, b, by = "k", keep = "full")
  expect_s3_class(res$d, "Date")
  expect_identical(res$d[res$k == 2], as.Date("2020-02-02"))
})

test_that("concat operator and apply_format work", {
  h <- helpers_env()
  expect_identical(h$`%+%`("a", "b"), "ab")
  expect_identical(h$`%+%`("a", NA), "a")
  expect_identical(h$`%+%`(NA, "_POP"), "_POP")

  fmt <- list(values = c("1" = "MALE", "2" = "FEMALE"), ranges = NULL, other = "UNK")
  expect_identical(h$apply_format(c(1, 2, 9), fmt), c("MALE", "FEMALE", "UNK"))
  expect_identical(h$sas_put(c(1, 2, 9), fmt), c("MALE", "FEMALE", "UNK"))

  # Raw value fallback when other is NULL and large numeric keys (F15)
  fmt_no_other <- list(values = c("1" = "MALE", "100000" = "LARGE"), ranges = NULL, other = NULL)
  expect_identical(h$apply_format(c(1, 3, 100000), fmt_no_other), c("MALE", "3", "LARGE"))

  # R2-5: Mixed-decimal vectors do not pad integer keys with .0
  fmt_mixed <- list(values = c("1" = "MALE", "2" = "FEMALE"), ranges = NULL, other = NULL)
  expect_identical(h$apply_format(c(1, 2.5), fmt_mixed), c("MALE", "2.5"))

  # Format with ranges
  fmt_range <- list(
    values = c("99" = "Missing"),
    ranges = list(
      list(lo = 0, hi = 17, label = "<18"),
      list(lo = 18, hi = 65, label = "18-65"),
      list(lo = 66, hi = 120, label = ">65")
    ),
    other = "Other"
  )
  expect_identical(h$apply_format(c(10, 25, 70, 99, 150), fmt_range),
                   c("<18", "18-65", ">65", "Missing", "Other"))
})

test_that("lib_read and lib_write work with registry", {
  h <- helpers_env()
  tmp <- withr::local_tempdir()
  h$.sas2r_registry <- list(
    testlib = list(read_path = tmp, write_path = tmp, engine = "rds", write = "rds")
  )
  df <- data.frame(A = 1:3, B = c("x", "y", "z"))
  h$lib_write(df, "testlib", "mydata")
  expect_true(file.exists(file.path(tmp, "mydata.rds")))

  # lib_write supports flipped arguments lib_write("testlib.mydata2", df)
  h$lib_write("testlib.mydata2", df)
  expect_true(file.exists(file.path(tmp, "mydata2.rds")))
  expect_identical(h$lib_read("testlib.mydata2")$A, 1:3)

  # chr_cmp supports 2-argument comparison returning -1, 0, 1
  expect_identical(h$chr_cmp("ALT", "ALT"), 0L)
  expect_identical(h$chr_cmp("ALT", "AST"), -1L)
  expect_identical(h$chr_cmp("AST", "ALT"), 1L)
  expect_identical(h$chr_cmp(NA, "ALT"), -1L)
  expect_identical(h$chr_cmp("ALT", NA), 1L)
  expect_identical(h$chr_cmp(NA, NA), 0L)

  # emit_proc_sort alias works
  df_sort <- data.frame(x = c(3, 1, 2))
  expect_identical(h$emit_proc_sort(data = df_sort, by = "x")$x, c(1, 2, 3))

  # sas_display helper formats strings and numbers
  expect_identical(h$sas_display(c("a", NA)), c("a", ""))
  expect_identical(h$sas_display(c(1, NA)), c("1", "."))
})

test_that("write_helpers copies template to output directory", {
  out <- withr::local_tempdir()
  dest <- write_helpers(out)
  expect_true(file.exists(dest))
  expect_identical(basename(dest), "sas2r-helpers.R")
})

test_that("registry writer emits a sourceable registry with WORK", {
  p <- sas_project(system.file("examples", "demo_project", package = "sas2r"))
  out <- withr::local_tempdir()
  write_registry(p, out)
  e <- new.env(); sys.source(file.path(out, "_sas2r_registry.R"), e)
  expect_true("adam" %in% names(e$.sas2r_registry))
  expect_true(dir.exists(e$.sas2r_registry$work$write_path))
})


test_that("sas_merge output keeps SAS column order and BY row ordering", {
  h <- helpers_env()
  # Dataset a lists x before its key; SAS keeps contributing-dataset order.
  a <- data.frame(x = c(10, 30), k = c(1, 3))
  b <- data.frame(k = c(3, 1, NA), y = c("c", "a", "m"))
  m <- h$sas_merge(a, b, by = "k", keep = "full")
  expect_identical(names(m), c("x", "k", "y"))
  # Rows follow the BY ordering with missing first, like SAS collation.
  expect_identical(m$k, c(NA, 1, 3))
  expect_identical(m$y, c("m", "a", "c"))
})

test_that("sas_if_else keeps classes and treats a missing condition as false", {
  h <- helpers_env()
  d <- as.Date(c("2020-01-01", "2020-06-01", "2020-12-31"))
  cond <- c(TRUE, NA, FALSE)
  out <- h$sas_if_else(cond, d + 10, d)
  expect_s3_class(out, "Date")
  expect_identical(out, as.Date(c("2020-01-11", "2020-06-01", "2020-12-31")))
  # New-variable form: an NA false branch keeps the yes branch's class.
  out2 <- h$sas_if_else(cond, d, NA)
  expect_s3_class(out2, "Date")
  expect_identical(is.na(out2), c(FALSE, TRUE, TRUE))
})
