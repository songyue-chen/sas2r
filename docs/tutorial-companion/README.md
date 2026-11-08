# sas2r tutorial companion scripts

Companion download v1 for the sas2r 0.5.8 tutorial.

**[Download sas2r-tutorial-companion.zip](https://raw.githubusercontent.com/songyue-chen/sas2r/tutorial-companion-v1/docs/tutorial-companion/sas2r-tutorial-companion.zip)**

Extract the ZIP, open `companion/`, and copy the three `.R` scripts into a new,
empty RStudio project. Open `demo.R` and run it section by section. The included
`README.txt` supplies setup instructions and the OpenAI configuration block.

## Which files come from where?

The **CRAN package includes** `demo.sas`, `_sas2r.yml`, `make-input.R`, and
`README.md` under `examples/migration-demo`. Find the installed example in R:

```r
installed_demo <- system.file("examples", "migration-demo", package = "sas2r")
cat(installed_demo, "\n")
cat(list.files(installed_demo), sep = "\n")
```

The **tutorial companion scripts are provided separately**:

| Script | Purpose |
| --- | --- |
| [demo.R](companion/demo.R) | Follow the tutorial: copy the bundled example, generate synthetic inputs, preflight, translate, inspect, and export. |
| [preflight-llm.R](companion/preflight-llm.R) | Test the configured model with `sas_llm_probe()` and inspect the latest probe log entry. |
| [read-llm-log.R](companion/read-llm-log.R) | Read model activity from a returned migration run's diagnostic log. |

These three scripts are not installed by CRAN. `sas2r-demo` is simply the project
folder name chosen in the video. The companion ZIP contains the three scripts
plus [README.txt](companion/README.txt); the example itself is copied from your
installed package by `demo.R`.

## Run the tutorial

1. Install `sas2r` and `ellmer`. Store your OpenAI credential in your user
   `.Renviron`, save, and restart R, as shown in the video.
2. Extract the companion ZIP and copy its three R scripts into your project.
3. Open `demo.R`. Run its setup section to copy the bundled example and generate
   the five synthetic observations.
4. At the marked pause, add the OpenAI `llm` block from `README.txt` to the copied
   `_sas2r.yml`, preserving its library and output settings. Keep credentials out
   of the YAML file. The selected model must be available to your account.
5. Continue section by section: connection probe, offline source preflight,
   translation, log inspection, output review, and export.

The probe and translation make provider requests when you run the scripts and
may incur charges. The example profile does not impose a total spending cap.

## Verification and interpretation

The scripts were checked against an isolated CRAN sas2r 0.5.8 installation with
deterministic offline test adapters. Probe success, FALSE, and error/log paths
were exercised, along with translator/reviewer/fixer activity and output checks.
The video uses reconstructed interfaces; it is not a recorded live OpenAI run.
A successful probe checks a structured ping, not a full migration or tool call.
No SAS reference output is included, so these checks do not establish SAS
reference equivalence. A fixer is invoked when needed, not in every migration.

## Video description

Suggested link text: **Download the sas2r tutorial companion scripts**.
Link to this README for context, or to the ZIP download above for a direct download.

[CRAN package](https://CRAN.R-project.org/package=sas2r) ·
[Provider configuration](../llm-providers.md)
