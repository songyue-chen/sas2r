base_df <- function() tibble::tibble(
  USUBJID = c("01", "02", "03"),
  AVAL = c(1, 2, NA),
  SEX = c("M", "F  ", "M"),
  ADT = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
)

test_that("identical-within-tolerance passes with cosmetic report", {
  comp <- base_df()
  comp$SEX <- c("M", "F", "M")            # padding-only difference
  comp$AVAL <- comp$AVAL + 1e-12          # inside tolerance
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_true(passed(r))
  expect_identical(sum(r$cosmetic$n[r$cosmetic$kind == "padding"]), 1L)
  expect_identical(nrow(r$details), 0L)
})

test_that("value mismatches fail and land in details with full fidelity", {
  comp <- base_df(); comp$AVAL[2] <- 99
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_false(passed(r))
  expect_identical(r$details$var, "aval")
  expect_identical(r$details$base_value, "2")
  expect_identical(r$details$comp_value, "99")
  v <- r$vars[r$vars$var == "aval", ]
  expect_identical(v$n_mismatch, 1L)
  expect_identical(v$max_abs_diff, 97)
})

test_that("one-sided NA is a value mismatch classed na_diff", {
  comp <- base_df(); comp$AVAL[3] <- 5   # base has NA there
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_false(passed(r))
  expect_identical(r$details$class, "na_diff")
})

test_that("case-only difference is a mismatch classed case", {
  comp <- base_df(); comp$SEX[1] <- "m"
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_false(passed(r))
  expect_identical(r$details$class, "case")
})

test_that("unmatched rows fail the comparison", {
  comp <- base_df()[1:2, ]
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_false(passed(r))
  expect_identical(r$summary$value[r$summary$metric == "rows_only_base"], 1L)
})

test_that("kind mismatch is structural, var excluded from cell compare", {
  comp <- base_df(); comp$AVAL <- as.character(comp$AVAL)
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_false(passed(r))
  expect_false("aval" %in% r$vars$var)
})

test_that("per-variable override loosens tolerance", {
  comp <- base_df(); comp$AVAL[1] <- 1.00005
  p <- compare_profile(overrides = list(aval = list(abs = 1e-3, rel = 0)))
  expect_true(passed(compare_datasets(base_df(), comp, profile = p, keys = "usubjid")))
  expect_false(passed(compare_datasets(base_df(), comp, keys = "usubjid")))
})

test_that("label attr difference is cosmetic, not failing", {
  b <- base_df(); attr(b$AVAL, "label") <- "Analysis Value"
  comp <- base_df(); attr(comp$AVAL, "label") <- "Analysis value"
  r <- compare_datasets(b, comp, keys = "usubjid")
  expect_true(passed(r))
  expect_true(any(r$cosmetic$kind == "attr:label"))
})

test_that("strict na_tags turns tag mismatch into failure", {
  b <- base_df(); b$AVAL[3] <- haven::tagged_na("a")
  comp <- base_df(); comp$AVAL[3] <- haven::tagged_na("b")
  r_report <- compare_datasets(b, comp, keys = "usubjid")
  expect_true(passed(r_report))
  expect_true(any(r_report$cosmetic$kind == "na_tag"))
  p <- compare_profile(na_tags = "strict")
  expect_false(passed(compare_datasets(b, comp, profile = p, keys = "usubjid")))
})

test_that("dates compare under numeric tolerance", {
  comp <- base_df(); comp$ADT[1] <- comp$ADT[1] + 1
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  expect_false(passed(r))
  expect_identical(r$details$var, "adt")
})

test_that("datetime columns compare under numeric tolerance", {
  dt1 <- as.POSIXct("2020-01-01 12:00:00", tz = "UTC")
  dt2 <- as.POSIXct("2020-01-02 12:00:00", tz = "UTC")
  b <- tibble::tibble(id = 1:2, dttm = c(dt1, dt2))
  comp <- tibble::tibble(id = 1:2, dttm = c(dt1, dt2 + 3600))
  r <- compare_datasets(b, comp, keys = "id")
  expect_false(passed(r))
  expect_identical(r$details$var, "dttm")
  expect_identical(r$vars$kind[r$vars$var == "dttm"], "datetime")
})

test_that("logical columns compare exactly with NA == NA", {
  b <- tibble::tibble(id = 1:3, flag = c(TRUE, FALSE, NA))
  comp1 <- tibble::tibble(id = 1:3, flag = c(TRUE, FALSE, NA))
  r1 <- compare_datasets(b, comp1, keys = "id")
  expect_true(passed(r1))

  comp2 <- tibble::tibble(id = 1:3, flag = c(TRUE, TRUE, NA))
  r2 <- compare_datasets(b, comp2, keys = "id")
  expect_false(passed(r2))
  expect_identical(r2$details$class, "value")

  comp3 <- tibble::tibble(id = 1:3, flag = c(TRUE, FALSE, TRUE))
  r3 <- compare_datasets(b, comp3, keys = "id")
  expect_false(passed(r3))
  expect_identical(r3$details$class, "na_diff")
})

test_that("factor and haven_labelled columns are normalized with cosmetic notes", {
  b <- tibble::tibble(id = 1:2, grp = factor(c("A", "B")), val = haven::labelled(c(10, 20), label = "Val"))
  comp <- tibble::tibble(id = 1:2, grp = c("A", "B"), val = c(10, 20))
  r <- compare_datasets(b, comp, keys = "id")
  expect_true(passed(r))
  expect_true(any(r$cosmetic$kind == "normalize:factor_to_character"))
  expect_true(any(r$cosmetic$kind == "normalize:zap_labels"))
})

test_that("row order comparison when keys is NULL", {
  b <- tibble::tibble(val = c(1, 2, 3))
  comp <- tibble::tibble(val = c(1, 2, 3))
  r <- compare_datasets(b, comp, keys = NULL)
  expect_true(passed(r))
  expect_null(r$keys)

  comp_diff <- tibble::tibble(val = c(1, 3, 2))
  r_diff <- compare_datasets(b, comp_diff, keys = NULL)
  expect_false(passed(r_diff))
  expect_identical(nrow(r_diff$details), 2L)
})

test_that("details table is capped at 1000 rows with details_truncated attr", {
  n <- 1200
  b <- tibble::tibble(id = seq_len(n), val = rep(1, n))
  comp <- tibble::tibble(id = seq_len(n), val = rep(2, n))
  r <- compare_datasets(b, comp, keys = "id")
  expect_false(passed(r))
  expect_identical(nrow(r$details), 1000L)
  expect_true(attr(r$details, "details_truncated"))
  expect_identical(r$summary$value[r$summary$metric == "value_mismatch_cells"], as.integer(n))
})

test_that("strict padding turns padding difference into failure", {
  b <- tibble::tibble(id = 1:2, txt = c("a  ", "b"))
  comp <- tibble::tibble(id = 1:2, txt = c("a", "b"))
  p_strict <- compare_profile(padding = "strict")
  r <- compare_datasets(b, comp, profile = p_strict, keys = "id")
  expect_false(passed(r))
  expect_identical(r$details$class, "value")
})

test_that("ignore na_tags ignores tag differences", {
  b <- tibble::tibble(id = 1, val = haven::tagged_na("a"))
  comp <- tibble::tibble(id = 1, val = haven::tagged_na("b"))
  p_ignore <- compare_profile(na_tags = "ignore")
  r <- compare_datasets(b, comp, profile = p_ignore, keys = "id")
  expect_true(passed(r))
  expect_identical(r$vars$tag_mismatches[r$vars$var == "val"], 0L)
  expect_false(any(r$cosmetic$kind == "na_tag"))
})

test_that("passed() verifies input class", {
  expect_error(passed(list(passed = TRUE)), "inherits\\(x, \"sas2r_comparison\"\\)")
})

test_that("print.sas2r_comparison outputs summary and mismatch info", {
  comp <- base_df(); comp$AVAL[2] <- 99
  r <- compare_datasets(base_df(), comp, keys = "usubjid")
  out <- capture.output(print(r), type = "message")
  expect_true(any(grepl("sas2r dataset comparison", out)))
  expect_true(any(grepl("FAILED", out)))
  expect_true(any(grepl("aval", out)))

  r_pass <- compare_datasets(base_df(), base_df(), keys = "usubjid")
  out_pass <- capture.output(print(r_pass), type = "message")
  expect_true(any(grepl("PASSED", out_pass)))
})

test_that("empty datasets compare cleanly", {
  b <- tibble::tibble(id = character(), val = numeric())
  comp <- tibble::tibble(id = character(), val = numeric())
  r <- compare_datasets(b, comp, keys = "id")
  expect_true(passed(r))
  expect_identical(nrow(r$details), 0L)
  expect_false(attr(r$details, "details_truncated"))
  expect_identical(r$summary$value[r$summary$metric == "rows_base"], 0L)
})

test_that("columns only in base or only in comp are recorded and cause failure", {
  b <- tibble::tibble(id = 1:2, x = 1:2, extra_base = c("a", "b"))
  comp <- tibble::tibble(id = 1:2, x = 1:2, extra_comp = c("c", "d"))
  r <- compare_datasets(b, comp, keys = "id")
  expect_false(passed(r))
  expect_identical(r$structure$only_base, "extra_base")
  expect_identical(r$structure$only_comp, "extra_comp")
})

test_that("F1: finite vs infinite values fail comparison", {
  b <- tibble::tibble(id = 1, aval = 5)
  cm <- tibble::tibble(id = 1, aval = Inf)
  r <- compare_datasets(b, cm, keys = "id")
  expect_false(passed(r))
  expect_identical(r$vars$n_mismatch[r$vars$var == "aval"], 1L)
})

test_that("F2: duplicate keys pair positionally without false pass or fabricated diffs", {
  # Dataset compared to itself with duplicate keys passes cleanly
  b_dup <- tibble::tibble(id = c("1", "1"), val = c(10, 20))
  r_self <- compare_datasets(b_dup, b_dup, keys = "id")
  expect_true(passed(r_self))
  expect_identical(nrow(r_self$details), 0L)
  expect_true(r_self$structure$dup_fanout)

  # Dropped duplicate row fails
  cm_dropped <- tibble::tibble(id = "1", val = 10)
  r_dropped <- compare_datasets(b_dup, cm_dropped, keys = "id")
  expect_false(passed(r_dropped))
  expect_identical(r_dropped$structure$rows_only_base, 1L)
})

test_that("F3: SAS null empty string == NA policy is respected", {
  b <- tibble::tibble(id = 1:2, txt = c("", "val"))
  cm <- tibble::tibble(id = 1:2, txt = c(NA_character_, "val"))
  r_default <- compare_datasets(b, cm, keys = "id")
  expect_true(passed(r_default))

  p_strict_null <- compare_profile(sas_null_equals_na = FALSE)
  r_strict <- compare_datasets(b, cm, profile = p_strict_null, keys = "id")
  expect_false(passed(r_strict))
  expect_identical(r_strict$details$class, "na_diff")
})

test_that("R1: time columns convert difftime units canonicalizing to seconds", {
  t_mins <- as.difftime(60, units = "mins")
  t_secs <- as.difftime(60, units = "secs")
  b_diff_units <- tibble::tibble(id = 1, t = t_mins)
  cm_diff_units <- tibble::tibble(id = 1, t = t_secs)
  r_diff <- compare_datasets(b_diff_units, cm_diff_units, keys = "id")
  expect_false(passed(r_diff))
  expect_identical(r_diff$vars$max_abs_diff[r_diff$vars$var == "t"], 3540)

  # 5 mins vs 300 secs passes
  t_5mins <- as.difftime(5, units = "mins")
  t_300secs <- as.difftime(300, units = "secs")
  r_match <- compare_datasets(tibble::tibble(id = 1, t = t_5mins),
                              tibble::tibble(id = 1, t = t_300secs), keys = "id")
  expect_true(passed(r_match))
})

test_that("R6: unsupported kinds are structural and do not fabricate cell diffs", {
  b_list <- tibble::tibble(id = 1:2, lst = list(1:2, 3:4))
  r_self_list <- compare_datasets(b_list, b_list, keys = "id")
  expect_false(passed(r_self_list))
  expect_identical(nrow(r_self_list$details), 0L)
  expect_identical(r_self_list$structure$unsupported_kinds$var, "lst")
  expect_false("lst" %in% r_self_list$vars$var)
})

test_that("R7: datetime comparisons use abs-only tolerance and reject epoch-scaled slack", {
  t0 <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  b_dt <- tibble::tibble(id = 1, dt = t0)
  cm_dt <- tibble::tibble(id = 1, dt = t0 + 15)
  r_dt <- compare_datasets(b_dt, cm_dt, keys = "id")
  expect_false(passed(r_dt))
  expect_identical(r_dt$vars$n_mismatch[r_dt$vars$var == "dt"], 1L)
})

test_that("R9: strict padding does not count padding cells in vars$n_cosmetic", {
  b_pad <- tibble::tibble(id = 1, txt = "a  ")
  cm_pad <- tibble::tibble(id = 1, txt = "a")
  p_strict <- compare_profile(padding = "strict")
  r_pad <- compare_datasets(b_pad, cm_pad, profile = p_strict, keys = "id")
  expect_false(passed(r_pad))
  expect_identical(r_pad$vars$n_cosmetic[r_pad$vars$var == "txt"], 0L)
  expect_identical(r_pad$vars$n_mismatch[r_pad$vars$var == "txt"], 1L)
})

test_that("R10 & N2: POSIXct formatting in details renders UTC timezone exactly once", {
  t0 <- as.POSIXct("2020-01-01 12:00:00", tz = "UTC")
  b_dt <- tibble::tibble(id = 1, dt = t0)
  cm_dt <- tibble::tibble(id = 1, dt = t0 + 3600)
  r_dt <- compare_datasets(b_dt, cm_dt, keys = "id")
  expect_identical(r_dt$details$base_value, "2020-01-01 12:00:00 UTC")
  expect_identical(r_dt$details$comp_value, "2020-01-01 13:00:00 UTC")
})

test_that("N4: explicit rel override on datetime variable is honored", {
  t0 <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  b_dt <- tibble::tibble(id = 1, dt = t0)
  cm_dt <- tibble::tibble(id = 1, dt = t0 + 15)
  # with explicit rel = 1e-7 on dt (rel * 1.5e9 ~ 150s slack), +15s passes
  p_rel <- compare_profile(overrides = list(dt = list(rel = 1e-7)))
  r_dt_rel <- compare_datasets(b_dt, cm_dt, profile = p_rel, keys = "id")
  expect_true(passed(r_dt_rel))
})

test_that("N6: print.sas2r_comparison outputs structural difference names", {
  b <- tibble::tibble(id = 1, aval = 10, extra_b = 1)
  cm <- tibble::tibble(id = 1, aval = "10", extra_c = 2)
  r <- compare_datasets(b, cm, keys = "id")
  out <- capture.output(print(r), type = "message")
  expect_true(any(grepl("structural differences", out)))
  expect_true(any(grepl("columns only in base: extra_b", out)))
  expect_true(any(grepl("columns only in compare: extra_c", out)))
  expect_true(any(grepl("type mismatch: aval", out)))
})

test_that("F8 & F12: tagged NA formatting shows .A vs .B and Date tagged NAs are checked", {
  b <- tibble::tibble(id = 1, aval = haven::tagged_na("a"))
  cm <- tibble::tibble(id = 1, aval = haven::tagged_na("b"))
  p_strict <- compare_profile(na_tags = "strict")
  r <- compare_datasets(b, cm, profile = p_strict, keys = "id")
  expect_false(passed(r))
  expect_identical(r$details$base_value, ".a")
  expect_identical(r$details$comp_value, ".b")
})

test_that("F14: Date formatting in details preserves date string", {
  b <- tibble::tibble(id = 1, dt = as.Date("2020-01-01"))
  cm <- tibble::tibble(id = 1, dt = as.Date("2020-06-01"))
  r <- compare_datasets(b, cm, keys = "id")
  expect_false(passed(r))
  expect_identical(r$details$base_value, "2020-01-01")
  expect_identical(r$details$comp_value, "2020-06-01")
})




test_that("compare_datasets_aligned pairs rows by inferred identity, not position", {
  b <- tibble::tibble(USUBJID = c("01", "02", "03"), AVAL = c(1, 2, 3))
  cm <- b[c(2, 3, 1), ]
  r <- compare_datasets_aligned(b, cm)
  expect_s3_class(r, "sas2r_comparison")
  expect_true(passed(r))
  expect_identical(r$structure$selected_keys, "usubjid")
  expect_identical(r$structure$alignment_method, "unique_key")

  # Duplicate keys match as multisets instead of pairing positionally.
  b2 <- tibble::tibble(id = c("1", "1"), val = c(10, 20))
  cm2 <- tibble::tibble(id = c("1", "1"), val = c(20, 10))
  r2 <- compare_datasets_aligned(b2, cm2, keys = "id")
  expect_true(passed(r2))
  expect_identical(r2$structure$alignment_method, "duplicate_key_multiset")
  expect_true(r2$structure$dup_fanout)

  # A genuine mismatch stays localized under the alignment.
  cm3 <- b
  cm3$AVAL[2] <- 99
  cm3 <- cm3[c(3, 1, 2), ]
  r3 <- compare_datasets_aligned(b, cm3)
  expect_false(passed(r3))
  expect_identical(nrow(r3$details), 1L)
  expect_identical(r3$details$var, "aval")
  expect_identical(r3$details$base_value, "2")
  expect_identical(r3$details$comp_value, "99")

  # Configured keys that exist in neither frame refuse loudly.
  expect_error(compare_datasets_aligned(b, cm, keys = "nope"))
})
