---
name: tidyverse-idioms
description: Prefer allowlisted, installed tidyverse packages for DATA steps, PROC SQL, summaries, reshaping and figures when they express the SAS behavior faithfully, and keep the bundle helpers for order, merge and missing-value semantics.
metadata:
  sas2r:
    version: 1
    agents: [translator, fixer]
    priority: 50
    triggers:
      unit_types: [data_step, proc_step, macro_def, program]
      procs: [sql, means, summary, freq, transpose, sort, univariate]
      flags: [by_group, order_dependent, native_graphics, statistical_defaults]
    tools: [lookup_rulebook, read_dependency_context]
---

# Tidyverse first, faithful always

Use an allowlisted, installed tidyverse package whenever its functions express
the SAS behavior faithfully. Fall back to base R or a bundle helper when they
do not; that fallback is correct and is not a finding. Never change SAS
behavior to fit an idiom. Qualify every package function, use the base `|>`
pipe and never call `library()`.

| SAS construct | Preferred R | Keep in mind |
|---|---|---|
| DATA step assignments, IF/THEN/ELSE, WHERE, IF ... DELETE | `dplyr::mutate`, `dplyr::if_else`, `dplyr::case_when`, `dplyr::filter` | Comparisons that may see missing values go through `chr_cmp()` or explicit `is.na()` guards: SAS missing is below every value in `<` and `>` tests. |
| PROC SORT, BY-group FIRST./LAST., RETAIN within a group | `sas_sort()` first, then `dplyr::group_by` with `dplyr::row_number()`, `dplyr::n()`, `dplyr::lag()`, `cumsum()` | `dplyr::arrange` puts NA last; SAS sorts missing first. NODUPKEY is `dplyr::distinct` only after `sas_sort`. |
| MERGE ... BY | `sas_merge()` | A many-to-many MERGE has no join equivalent; keep the helper's refusal visible. |
| PROC SQL joins and GROUP BY | `dplyr::inner_join`, `dplyr::left_join`, `dplyr::full_join`, `dplyr::summarise` | `COUNT(DISTINCT x)` excludes missing: `dplyr::n_distinct(x, na.rm = TRUE)`. Check how the source treats missing join keys before choosing `na_matches`. |
| PROC MEANS, SUMMARY, FREQ | `dplyr::summarise`, `dplyr::count` | Keep `sas_mean()` and `sas_sum()` where SAS missing handling differs from `na.rm`; percentile definitions follow the statistical-defaults skill. |
| PROC TRANSPOSE | `tidyr::pivot_longer`, `tidyr::pivot_wider` | Preserve the ID and BY order and the `_NAME_` column when the source uses it. |
| Character functions | `stringr::str_sub`, `str_trim`, `str_detect`, `str_replace_all`, `str_pad` | SAS character variables have declared lengths; `sas_substr()` and `sas_length()` reproduce padding-sensitive behavior. |
| FORMAT and PUT on categories | `forcats::fct_relevel`, `forcats::fct_recode` from the format table, or `apply_format()` | Keep the source order of levels; never sort levels alphabetically by default. |
| Dates and intervals | `lubridate::ymd`, `lubridate::interval`, `lubridate::%m+%` | SAS dates count days from 1960-01-01; keep the INTNX alignment (`beginning`, `sameday`) explicit. |
| SGPLOT, SGRENDER, GTL | `ggplot2` layers with `stat = "identity"` on source-computed statistics | Compute the statistics as the source does; the figure only draws them. |

## BY-group logic after a SAS-faithful sort

```r
visits <- data.frame(usubjid = c("01", "01", "02", "02", "02"),
                     avisitn = c(2, NA, 1, 3, 2), aval = c(5, 4, 7, 9, 8))
ordered <- sas_sort(visits, by = c("usubjid", "avisitn"))
flagged <- ordered |>
  dplyr::group_by(usubjid) |>
  dplyr::mutate(first_visit = dplyr::row_number() == 1L,
                last_visit = dplyr::row_number() == dplyr::n(),
                baseline = aval[1L]) |>
  dplyr::ungroup()
```

The missing visit sorts first, as in SAS, so it is the baseline row for
subject 01. `dplyr::arrange(usubjid, avisitn)` would have made visit 2 the
baseline.

## A figure that draws supplied statistics

```r
box <- data.frame(group = "Group A", ymin = 0, lower = 1, middle = 3, upper = 5, ymax = 5)
outliers <- data.frame(group = "Group A", value = 20)
figure <- ggplot2::ggplot() +
  ggplot2::geom_boxplot(data = box, stat = "identity",
    ggplot2::aes(x = group, ymin = ymin, lower = lower, middle = middle,
                 upper = upper, ymax = ymax)) +
  ggplot2::geom_point(data = outliers, ggplot2::aes(x = group, y = value)) +
  ggplot2::labs(x = NULL, y = "Observed value")
```

The quartiles, whiskers and outliers come from the source's own definition,
not from `ggplot2::stat_boxplot`. Write a multi-page PDF by opening
`grDevices::pdf()`, calling `print()` on each page's figure, then
`grDevices::dev.off()`. Required tables, labels and pagination limits from the
source remain part of the output.
