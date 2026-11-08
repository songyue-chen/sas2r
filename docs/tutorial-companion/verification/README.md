# Live tutorial verification — 0.6.0 beta

The video reconstructs OpenAI run `run_20261108T221151Z_f3aca9b7`, using
[PR #64](https://github.com/songyue-chen/sas2r/pull/64), commit
`d3cdf57d60d40a7108909a7516f44435e0f7aa5b`. The package metadata is 0.6.0;
this is a beta preview, not the CRAN release. Versions and usage are in summary.json.

The initial draft passed smoke execution. Review identified a missing Obs column
in default PROC PRINT output. One fixer invocation restored it. Revision two
passed review, smoke execution, bundle execution, and required output checks.

The PDF was saved at `outputs/tlf/table1.pdf`, with no second outputs directory
beneath tlf and no write into the source project's outputs folder. Dataset values,
missingness, flags, row order, source-derived column order, all five subject IDs,
and expected PDF text passed. AVAL_FLAG precedes input columns because the SAS
LENGTH declaration precedes SET. No independent SAS reference is supplied;
PDF layout equivalence is not established. The generated PDF is shown unchanged.

The standalone export passed the same checks in a fresh R process without an
OpenAI key or sas2r loaded. The runtime created `output/tlf/`; no folder workaround
was needed. Manual execution does not update the original migration report.

A separate initial beta run also passed without repairs (8 provider calls,
approximately $0.0386 estimated usage). AI generation and repair sequences vary.

live-console.txt abbreviates local tutorial paths. selected-code.R and table1.pdf
are unchanged generated artifacts. replay-summary.txt is retained historical
PR #63 evidence, not evidence from this beta run.
