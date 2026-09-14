---
name: sas-statistical-defaults
description: Preserve source statistical definitions through summaries, rounding and calculations inside plots.
metadata:
  sas2r:
    version: 1
    agents: [translator, reviewer, fixer]
    priority: 90
    triggers:
      flags: [statistical_defaults]
    tools: [lookup_rulebook, search_docs]
---

# Statistical definitions through the final output

Use the source procedure and its explicit options to choose the calculation.
For ordinary unweighted percentiles, SAS definition 5 corresponds to
`stats::quantile(..., type = 2)`, while R defaults to type 7. Other definitions,
weights and missing-value rules need their own source-grounded treatment.
Consult the existing rulebook's quantile domain for unresolved cases.

A plotting function can recompute statistics from the raw data. Base R boxplots
use Tukey hinges; these need not equal the source percentiles. Preserve the
source quartiles, fences, whiskers and outlier membership through the plotting
step, using explicit statistics when necessary. Do not assume a correct summary
table makes a separately computed plot correct. Preserve requested statistical
features; leave visual polish to human review.

For example, for unweighted values `c(0, 1, 2, 3, 4, 5, 20)`, definition 5 gives
quartiles 1 and 5, fences -5 and 11, whiskers 0 and 5, and outlier 20.
Base R hinges are 1.5 and 4.5. Small groups and values near fences expose defaults
that larger, regular samples may hide. These are acceptance examples, not a
requirement to introduce a new statistical helper.

For SAS ROUND, use the existing `sas_round()` contract rather than base R's
ties-to-even rule. Check the source's actual numeric format for displayed
values; do not assume `round()` or `sprintf()` preserves every SAS format.
Reuse the provided helper reference and rulebook before requesting more tools.

Review group membership, missing values, calculation definitions and final
consumers independently. If the source definition cannot be established, record
that uncertainty instead of assuming the R default is equivalent.

References:
- [R quantile definitions, including the SAS mapping](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html)
- [R boxplot statistics and hinges](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/boxplot.stats.html)
- [SAS percentile definitions](https://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/statug_boxplot_sect018.htm)
