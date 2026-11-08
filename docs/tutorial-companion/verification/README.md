# Live tutorial verification — 0.6.0 beta

The main walkthrough reconstructs OpenAI run `run_20261108T222306Z_d7870d6a`,
using [PR #64](https://github.com/songyue-chen/sas2r/pull/64), final commit
`ba21d25578f075fd5408220dc30445517f7c75d5`. Metadata is 0.6.0; this is a beta,
not the CRAN release. Versions and estimated usage are in summary.json.

The selected code passed review, smoke execution, bundle execution and required
output checks without repair. The PDF was saved at `outputs/tlf/table1.pdf`, with
no second outputs directory beneath tlf and no write into the source project.
Dataset values, missingness, flags, row order, source-derived column order, all
five subject IDs, and expected PDF text passed. AVAL_FLAG precedes input columns
because the SAS LENGTH declaration precedes SET.

The standalone export passed the same checks without an API key or sas2r loaded.
The runtime created `output/tlf/`; no folder workaround was needed. Manual
execution does not update the original migration report.

The video separately shows the [earlier beta repair example](repair-example/README.md),
where review identified a missing Obs column and the fixer restored it. The
current final-commit run needed no repair. AI generation and repair sequences vary.

No independent SAS reference is supplied. SAS PROC PRINT layout equivalence is
not established; inspect formatting separately. The actual generated PDF is shown
unchanged. live-console.txt abbreviates local paths; selected-code.R and table1.pdf
are unchanged generated artifacts. replay-summary.txt is historical PR #63 evidence.
