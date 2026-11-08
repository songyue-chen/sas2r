# sas2r tutorial companion

**[Download the corrected tutorial ZIP](https://raw.githubusercontent.com/songyue-chen/sas2r/tutorial-companion-v1/docs/tutorial-companion/sas2r-tutorial-companion.zip)**

Revision 3 supplies a corrected, self-contained tutorial example for the tested
[PR #63 build](https://github.com/songyue-chen/sas2r/pull/63). This fix is not yet the CRAN release. Extract the ZIP, open `companion/`, and copy **all its contents**, including
`example/`, into a **new, empty RStudio project**. Open `demo.R` and run it section
by section. Keep earlier runs for diagnosis; use a fresh project for this revision.

## What changed

The original package example uses a macro-based dataset reference that 0.5.8 can
miss during dependency discovery. It also leaves `AVAL_FLAG` at the seven-character
length established by `'MISSING'`, truncating `'RECORDED'`. The corrected tutorial
uses `set raw.input_ds;` and `length AVAL_FLAG $8;` explicitly. These are documented
source changes, not changes to the installed package or a workaround that hides
translation differences. The pinned package build also reconciles successful bundle execution with earlier smoke failures for the same selected revision.

`demo.R` now checks that `raw.input_ds` is available **before paid provider calls**,
allows up to two component repair rounds, preserves existing YAML settings, and checks the generated dataset and PDF before
export. The commented OpenAI YAML template remains at the end of `demo.R`.

## Which files come from where?

CRAN still includes the **original** `demo.sas`, `_sas2r.yml`, `make-input.R`, and
`README.md` under `examples/migration-demo`:

```r
system.file("examples", "migration-demo", package = "sas2r")
```

For this walkthrough, use the **corrected example supplied in this download**.
Do not overwrite it with the original CRAN example.

| File | Purpose |
| --- | --- |
| [demo.R](companion/demo.R) | Setup, configuration pause, input preflight, probe, translation, inspection, verification and export. |
| [preflight-llm.R](companion/preflight-llm.R) | Real provider probe and latest log entry. |
| [read-llm-log.R](companion/read-llm-log.R) | Model activity from the returned run's log. |
| [verify-output.R](companion/verify-output.R) | Dataset and PDF-content checks after execution. |
| [run-export.R](companion/run-export.R) | Prepares the example PDF directory and runs the standalone export in a fresh session. |
| [example/](companion/example) | Corrected SAS, base YAML, input generator, expected values, and provenance notes. |
| [README.txt](companion/README.txt) | Step-by-step setup and interpretation. |

The scripts and corrected example are separate tutorial materials on this branch;
`main` remains reserved for the CRAN package.

## Run the tutorial

```r
install.packages(c("remotes", "ellmer", "pdftools"))
remotes::install_github("songyue-chen/sas2r", ref = "e9d120613d1254ae3772f26d9eb7bac23cba5636")
```

Restart R after installation if sas2r was already loaded. This commit belongs to
PR #63 targeting `develop`; it has not been merged or submitted to CRAN by this tutorial.


1. Install the pinned PR build and supporting packages using the commands below.
   Store your OpenAI credential in your user `.Renviron`, save, and restart R.
   Never print or share the key.
2. Copy **all** extracted companion contents into a new project, including `example/`.
3. Run the setup section of `demo.R` to copy the corrected source and generate inputs.
4. At the pause, copy the **11 commented YAML lines** at the end of `demo.R` into
   the **same `_sas2r.yml`**, below `outputs`. Remove only the leading `# ` from
   each pasted line. Preserve indentation; align `llm` with `libraries` and
   `outputs`. Update an existing `llm` section instead of duplicating it. Save.
5. Run offline preflight and confirm `raw.input_ds` is **available**.
6. Run the model probe, then translation. These make provider requests and may
   incur charges. Confirm model access and billing/credits. The supplied profile
   does not impose a total spending cap.
7. Inspect status, generated R, `START_HERE.html`, and the model log. Run the output
   checks; inspect the PDF visually. Export after checking the results.
8. In a fresh session in the tutorial project, run `source("run-export.R")` to
   prepare this example's relative PDF directory and execute the exported code.
   This uses no sas2r namespace or model connection and does not update the original report.

## Verification and limits

See [live verification](verification/README.md) for the real OpenAI test and its
saved evidence. The video reconstructs interfaces; it is not a screen recording.
The latest real run repaired two execution errors and passed review, smoke,
bundle and tutorial output checks. A separate saved-code replay verifies the
stale runtime-evidence fix without making model requests.
A successful probe alone does not verify a full migration or tool calling.

`example/expected-final.csv` contains tutorial expectations derived from the
corrected source and synthetic input. It is **not independently generated by SAS**,
is checked only after translation, and is not configured as a SAS reference.
Successful execution and these checks **do not establish SAS-reference equivalence**.

[CRAN package](https://CRAN.R-project.org/package=sas2r) ·
[Provider configuration](https://github.com/songyue-chen/sas2r/blob/main/docs/llm-providers.md)
