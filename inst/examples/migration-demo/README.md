# Synthetic migration demo

This example derives flags from five synthetic observations, sorts them, writes
`adam.final_ds`, and requests a PDF table at `outputs/table1.pdf`. It needs no
licensed SAS installation. Its macro, conditional and reporting statements need
translation assistance; it is not an offline rule-only translation example.

## Copy and prepare

Run this in R after installing sas2r. It creates a fresh writable copy; the
installed package remains unchanged. Use your own destination instead of
`tempfile()` if you want to keep the project between R sessions.

<!-- sas2r-example: offline installed-migration-setup -->
```r
library(sas2r)
installed_demo <- system.file("examples", "migration-demo", package = "sas2r")
demo <- tempfile("sas2r-migration-demo-")
dir.create(demo)
file.copy(list.files(installed_demo, full.names = TRUE), demo, recursive = TRUE)
source(file.path(demo, "make-input.R"), chdir = TRUE)
check <- sas_preflight(demo, diagnose = "off")
check$inputs
check$readiness
```

`make-input.R` creates `data/input_ds.rds` in the copied demo when sourced with
`chdir = TRUE`. From a terminal, either run `Rscript make-input.R` inside the
copied demo, or supply the destination explicitly:

```sh
Rscript /path/to/migration-demo/make-input.R /path/to/migration-demo/data
```

Running without a destination from another directory fails with setup guidance.
This explicit offline preflight makes no model calls. The default `diagnose = "auto"`
can send bounded SAS statements, paths and findings to a configured LLM. With
data prepared it can still identify source
features needing translation/review; this is expected, not a missing API key.

## Translate and inspect

Add an `llm:` block to the copied `_sas2r.yml` using a
[provider profile](https://github.com/songyue-chen/sas2r/blob/main/docs/llm-providers.md#2-configuration-examples-_sas2ryml),
and configure that provider's credentials. The next step can make paid model
calls. Inspect the configured limits before running it.

<!-- sas2r-example: network installed-migration-run -->
```r
result <- sas_translate(
  demo,
  config = file.path(demo, "_sas2r.yml"),
  out_dir = file.path(demo, "migration-output"),
  execute = TRUE
)
result$status
result$status_reason
result$outputs_dir
result$report_path
```

Inspect the returned status and report before using any output. When execution
succeeds, look under `result$outputs_dir` for
`datasets/adam/final_ds.rds` and `tlf/outputs/table1.pdf`.
`AVAL_FLAG` should mark subject 02 missing; `HIGH_FLAG` should be 1 for subjects
01, 03 and 05, and 0 for 02 and 04. Rows should be sorted by `TRTP`, then `AVISITN`.
The PDF should show the resulting table, not a plot.

The package's regression test supplies a fixed translator and reviewer and checks
these synthetic data plus PDF content when PDF text extraction is available.
That exercises the workflow; it does not establish live-model quality or
independent SAS equivalence. No SAS reference output is bundled with this demo.
