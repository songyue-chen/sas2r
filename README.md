# sas2r: SAS to R Translation & Migration Evidence for Clinical Programming

> **Use R to get R.**

<!-- badges: start -->
<!-- badges: end -->

**sas2r** is an open-source R package for **clinical statistical programmers and biostatisticians** in pharmaceutical, biotech, and CRO organizations. It runs a **coordinated multi-agent workflow** that moves clinical trial data pipelines (SDTM, ADaM, Tables, Listings, and Figures) from SAS to R — an AI translator, an independent AI reviewer, and an AI fixer, each with a defined role inside a deterministic process — and shows you the evidence for every step it took.

`sas2r` does not require SAS to translate or execute the generated R. Dataset processing and comparison run on your own infrastructure; a configured AI provider receives the evidence described below. Reference-based validation requires outputs from the corresponding SAS programs, using the same inputs and parameters.

> **Also check out [sas2r.ai](https://sas2r.ai)** — a web-based companion tool for quick, browser-based SAS to R code translation. While sas2r.ai currently uses direct model translation for rapid code conversions, we plan to bring this R package's multi-agent workflow and dataset QC capabilities to the cloud platform in the future!

---

## Why Statistical Programmers Use sas2r

- **Built for clinical data work.** The rule-based translator handles the bread-and-butter patterns on its own — DATA step derivations and filters, `MERGE (in=a in=b)`, `PROC SORT`, `MEANS`, `FREQ`, `FORMAT`, and simple `PROC SQL`. It also knows SAS habits R does not share: how missing values sort and compare, trailing blanks in character values, and case-insensitive variable names all behave the SAS way in the translated code.
- **Honest about what it can't do.** SAS patterns the rules cannot prove — `RETAIN`, `FIRST.` / `LAST.` logic, `OUTPUT` statements, `PROC TRANSPOSE`, macros — are never silently guessed. Each one is clearly marked and handed to an AI translator, and a second, independent AI reviewer reads the result against your original SAS before it is accepted.
- **Automated dataset QC.** If you provide reference SAS datasets (`.sas7bdat`, `.xpt`, or `.rds`), `sas2r` compares each generated R dataset against them: it lines up rows even when their order differs, understands duplicate key values, applies SAS missing-value and blank-padding rules, and checks numbers to configurable tolerances.
- **Your source data is never touched.** Input libraries are opened read-only, and every run writes into its own separate working copy (copy-on-write), so a failed attempt can never contaminate your data or a previous good result.
- **Standalone R programs you can take anywhere.** Export plain R scripts, `autoexec.R`, runtime files, and a dependency-ordered launcher with `sas_write()`. The exported guide lists required R packages and external input libraries; `sas2r` itself is not required to run the bundle. The runtime is documented and versioned: see `?sas2r_runtime` and `vignette("runtime-helpers")`.
- **Repairs with evidence, not guesswork.** When a translated program errors or an output doesn't match its reference, an AI fixer receives a focused summary of what went wrong, patches the one program responsible, and the whole pipeline re-runs from scratch to prove the patch actually helped.

---

## How a Migration Runs: a Coordinated Multi-Agent Workflow

`sas2r` is agentic where judgment helps and deterministic where trust is required. The AI agents — translator, independent reviewer, fixer — exercise real judgment inside their steps: each decides which of its tools to consult (macro sources, the dependency graph, the rulebook, bounded comparison evidence) within a fixed call budget. But the process around them is code, not model choice: the pipeline sequence, the repair-round limits, the execution of every program, and the final status are all decided deterministically, and no agent ever grades its own work.

The workflow runs in two stages: first each program is translated and checked on its own, then the whole pipeline runs end to end and the outputs are judged together. Each stage has its own repair loop.

```text
┌────────────────────────────────────────────────────────────┐
│                     Your SAS programs                      │
│         (.sas files, macros, %include scripts)             │
└─────────────────────────────┬──────────────────────────────┘
                              ▼
   STAGE 1 — one program at a time, in dependency order
┌────────────────────────────────────────────────────────────┐
│  Rule-based translation                                    │
│  Reliable patterns become R directly; anything uncertain   │
│  is marked and handed to the AI translator — never guessed │
└─────────────────────────────┬──────────────────────────────┘
                              ▼
┌────────────────────────────────────────────────────────────┐
│  Independent AI review + trial run of the program          │
│  The reviewer reads the R against your SAS; the program    │
│  is also executed on its own to catch runtime errors       │
└──────────┬─────────────────────────────────┬───────────────┘
           │ problem found                   │ program is sound
           ▼                                 │
┌─────────────────────────┐                  │
│  AI fixer patches the   │── re-reviewed ──►│
│  program (the immediate │    and re-run    │
│  repair loop)           │                  │
└─────────────────────────┘                  ▼
   STAGE 2 — the whole pipeline together
┌────────────────────────────────────────────────────────────┐
│  Full pipeline run in an isolated working copy             │
│  Every program executes in order against real data         │
└─────────────────────────────┬──────────────────────────────┘
                              ▼
┌────────────────────────────────────────────────────────────┐
│  Output check                                              │
│  Each required dataset and TLF is compared against the     │
│  reference files you configured                            │
└──────────┬─────────────────────────────────┬───────────────┘
           │ error or mismatch               │ everything passes
           ▼                                 │
┌─────────────────────────┐                  │
│  AI fixer patches the   │                  │
│  ONE responsible program│── fresh full ───►│
│  (the bundle repair     │    re-run        │
│  loop)                  │                  ▼
└─────────────────────────┘   ┌─────────────────────────────┐
                              │  Final R bundle + report     │
                              │  with one of four statuses   │
                              └─────────────────────────────┘
```

A repaired run only replaces a previous one if it is genuinely better — a patch that makes things worse is discarded, and the earlier attempt stays selected.

---

## Quickstart

### 1. Install

<!-- sas2r-example: network install -->
```r
# install.packages("remotes")
remotes::install_github("songyue-chen/sas2r")
```

### 2. Describe your study in `_sas2r.yml`

Create one small file, `_sas2r.yml`, in your project directory. It says where your data lives, which outputs matter, and which AI model to use:

```yaml
project: my_clinical_study

libraries:            # your SAS librefs and where their data files live
  sdtm:
    path: data/sdtm
    engine: xpt       # how to read members: xpt, sas7bdat, or rds
  adam:
    path: data/adam
    engine: xpt
    write: rds        # how translated programs save results: rds or xpt

outputs:              # what the migration must produce
  datasets:
    - adam.adsl
    - adam.adae
  references:         # optional: gold-standard SAS outputs to compare against
    adam.adsl: data/reference/adsl.xpt
    adam.adae: data/reference/adae.xpt

llm:                  # the AI model (see the provider guide below)
  provider: anthropic
  model: claude-sonnet-4-6
```

### 3. Check the setup offline

<!-- sas2r-example: offline readme-preflight -->
```r
library(sas2r)
check <- sas_preflight(
  "programs/", config = "_sas2r.yml", out_dir = "migration_output",
  usage_limits = list(max_calls = 20)
)
print(check)
check$inputs       # availability and producer-order status
check$budget       # effective limits; no model calls are made
```

Preflight reports setup findings before translation. It does not read dataset
contents or test model credentials. `check$project` preserves its configuration
and output requirements for translation. A plain `config` list supplied with
`check$project` updates only the named top-level settings; explicit `NULL` clears
a setting. The saved project uses absolute source paths and can be reused from
another working directory. Rescan the source path when sources or library
bindings change. [The preflight and QC guide](docs/clinical-qc-preflight.md)
has a self-contained example and reusable profiles for labels, formats, types,
column order, keys, uniqueness, row counts, and per-variable tolerances.

### 4. Run it

<!-- sas2r-example: network migration -->
```r
library(sas2r)

result <- sas_translate(
  path = check$project,            # reuse the unchanged source scan
  out_dir = "migration_output",
  usage_limits = list(max_calls = 20),
  execute = TRUE,                  # actually run the translated programs
  max_program_repair_rounds = 1,   # immediate repair attempts per program
  max_bundle_repair_rounds = 2,    # full-pipeline repair attempts
  agent_evidence = "code_only"     # what repair evidence the AI may see
)

# What you get back
result$status        # one of the four statuses below
result$bundle_dir    # the finished, standalone R programs
result$outputs_dir   # selected outputs, e.g. adam/adsl.rds or outputs/table.html
result$report_path   # a readable report of everything that happened

# Read a translated program
cat(sas_code(result, 1))

# Export the selected code, generated outputs, reports, and run guide
sas_write(result, "r_production/")
```

The export includes `run.R`, `run-order.json`, `outputs-manifest.json`, and a
README describing its inputs and R dependencies. From the exported folder, run
`Rscript run.R`, or use `source("run.R", chdir = TRUE)` in R. If a source program
already occupies `run.R`, `run-order.json` identifies the renamed launcher.
Input data is not copied: update `autoexec.R` and any explicit source LIBNAME
paths if those inputs move. A manual rerun does not update the exported report.

### The four statuses

Every run ends in exactly one status. It is decided from what actually executed and what the output checks found — never from what an AI model claims:

- **`blocked`** — something required went wrong: a program failed to run, or a required output is missing or doesn't match. The report names the program and shows the evidence.
- **`needs_review`** — execution was deferred, or required review/lineage evidence remains incomplete or blocked. Programs may still have run successfully; read the reported reason.
- **`migration_ready`** — bundle execution, required output checks, and lineage requirements passed, without passing reference evidence for a required target. This can mean only existence/readability checks were available.
- **`validated`** — those requirements passed and at least one required target supplied a passing reference comparison. Other outputs can remain unreferenced.

Check coverage alongside the status: `print(result)` and the reports distinguish
outputs produced, reference-compared, passed, and reference-passed. The JSON
report lists `validated_targets` and `unreferenced_targets` under `coverage`.
For example, one passing referenced output and two unreferenced outputs can
produce `validated`; it does not mean all three were compared.

**THIS IS NOT PARITY.** `validated` means your configured reference comparisons passed under the tolerances you declared. It is not proof of SAS equivalence, and it does not replace double programming or independent statistical QC.

### Reading the progress log

| Log message | What it establishes |
| --- | --- |
| `reviewer ...: ok, 3 tool calls` | The reviewer agent call completed successfully. This is not its semantic verdict. The tool count describes tool use, not findings. |
| `coordinator ...: mechanical checks passed` | Generated code passed syntax and mechanical contract checks. |
| `coordinator ...: reviewed` | A review result was recorded. Inspect its verdict and findings; it can still require repair. |
| `coordinator ...: review unavailable` | No usable review was obtained. The reason is recorded, and smoke execution may continue. |
| `smoke ...: passed` | The program executed in its smoke attempt. This does not establish agreement with SAS reference data. |
| `bundle ... assessed -- migration_ready` | The selected bundle met the requirements described above. Read reference coverage separately. |

Review findings can trigger a fixer even after mechanical checks and smoke tests
pass. A repaired revision is checked and reviewed again. The elapsed-time and
usage summary distinguishes known spend from unknown-cost calls: `$0.0000`
known spend with unknown-cost calls does not mean the run was free.

### Reference differences: translation error or different specification?

Use references generated by the same SAS programs, input snapshot, formats,
macros, and parameters. A similarly named ADaM dataset is not sufficient: a
reference may exclude subjects the source retains, derive additional variables,
or use a different analysis visit definition.

When a comparison fails, inspect schema differences and unmatched rows before
cell examples, then trace the affected derivations back to SAS and the inputs.
Verify that row keys have the same meaning on both sides. An independent R
reconstruction can help diagnose a discrepancy, but it is not a SAS execution.
Resolve source/reference mismatches before using them to drive translation
repairs. See the [comparison guide](docs/output-evidence.md) for saved-output
checks and ambiguous alignment.

### Resume and limit provider calls

Repeat the same `sas_translate()` call with `resume = TRUE` to reuse saved
translation revisions and completed reviews when sources, inputs, QC requirements,
model settings, runtime, and worker prompts still match. Transport timeouts, retry
limits, and run budget changes alone do not invalidate completed revisions. Changed or missing artifacts regenerate;
smoke execution and full output checks rerun in fresh attempts. An unavailable
review is retried.

Use `usage_limits = list(max_calls = 20)` to cap provider requests, or
`usage_limits = list(max_calls = 0)` to prevent them. Limits and usage are
reported explicitly; the usage ledger is cumulative across resumed runs.
See `?sas_translate` and the [migration evidence guide](docs/migration-evidence.md)
for the complete limits and reuse contract.

---

## Connecting an AI Model

`sas2r`'s AI connection is built on [ellmer](https://ellmer.tidyverse.org), the tidyverse package that speaks to every major AI provider. `sas2r` never talks to a provider directly — every call goes through ellmer's official connectors — so in principle, any provider ellmer supports is within reach of this design. From that family, this release validates and ships **twelve providers**, each checked when your configuration loads: a typo in a provider name or setting stops the run immediately instead of failing halfway through. As ellmer's connector family grows, further providers can join the validated list once they have been exercised with the migration workflow.

Full live migrations have been run end to end with these models:

| Provider | Model used in live runs |
|---|---|
| `anthropic` | `claude-sonnet-4-6` |
| `openai` | `gpt-5.6-terra` |
| `gemini` | `gemini-3.7-flash` |
| `deepseek` | `deepseek-v4-pro` |

Model names change often. `sas_llm_models()` lists what your account can actually use, and `sas_llm_probe()` confirms your sign-in works before you start a long run.

For new DeepSeek Flash configurations, use `deepseek-flash`. As of September 12,
2026, it serves DeepSeek-V4.1-Flash; `deepseek-v4-flash` remains a temporary alias.
`deepseek-v4-pro` is still available and is the model recorded in the earlier
live runs above. See [DeepSeek's current model documentation](https://api-docs.deepseek.com/quick_start/pricing/).

### Every available setting, in one example

A provider and model identify the connection. Agent translation also needs the
capability declarations shown below. For translation, start with high reasoning
and an explicit output allowance on supported models:

```yaml
llm:
  provider: anthropic          # one of the twelve provider names below
  model: claude-sonnet-4-6     # or name models per tier instead:
  tiers:
    frontier: claude-opus-4-6  # optional override for the active agent tier;
                               # every active tier must support these settings

  auth_mode: api_key           # how sas2r signs in; each provider's
                               # choices and default are listed below
  timeout_seconds: 300         # patience per request (default 300)
  max_tries: 1                 # transport attempts per request (default 1;
                               # sas2r retries brief outages on its own)

  reasoning_effort: high       # adaptive thinking for this Claude model
  max_output_tokens: 32768     # starting allowance per response; increase for
                               # long programs within the model's limit

  cache: 1h                    # prompt-cache lifetime (anthropic, posit,
                               # bedrock). 1h is the default: migration
                               # turns are minutes apart, so a 5m cache
                               # would expire between them

  capabilities:
    structured_output: fallback
    tool_calling: native
```

**Never write an API key into `_sas2r.yml`.** Leave keys out of the file and set the provider's environment variable instead (listed below); `sas2r` finds it there. Files get shared and committed — environment variables don't.

### Recommended `_sas2r.yml` profiles

Choose **one complete `llm:` block** below; replace the previous block when
switching providers. These examples target ellmer 0.4.2's parameter mappings.
Keep the rest of your study configuration unchanged. The full template is
[inst/examples/_sas2r.example.yml](inst/examples/_sas2r.example.yml).

`max_output_tokens: 32768` is an initial allowance, not a quality guarantee or a
whole-run budget. Reasoning can consume part of it. Increase it for long programs
within the model's supported output limit. Omitting it uses connector/provider
defaults, which can be smaller than the model's maximum.

At startup, `sas_translate()` probes explicitly configured model settings before
agent work. Unknown reasoning support is checked with an invalid level followed
by the requested level. A connector that drops the setting, or an endpoint that
ignores it, stops the run. You no longer need `reasoning_effort: supported` or
`max_output_tokens: supported` flags for automatic verification. Explicit
`unsupported` flags remain authoritative.

Checks share the run's budget and appear as `settings` / `probe` entries in
`<out_dir>/.sas2r/llm_log.jsonl`. Successful checks are reused within the same
adapter session for the exact endpoint, model, connector version and settings.
`sas_preflight()` remains offline. The probe verifies request compatibility;
it does not establish SAS-to-R correctness or measure the model's reasoning.

**OpenAI — `OPENAI_API_KEY`**

```yaml
llm:
  provider: openai
  auth_mode: api_key
  model: gpt-5.6-terra
  reasoning_effort: high
  max_output_tokens: 32768
  capabilities:
    structured_output: native
    tool_calling: native
  timeout_seconds: 900
  max_tries: 1
```

**Anthropic — `ANTHROPIC_API_KEY`**

```yaml
llm:
  provider: anthropic
  auth_mode: api_key
  model: claude-sonnet-4-6
  reasoning_effort: high
  max_output_tokens: 32768
  capabilities:
    structured_output: fallback
    tool_calling: native
  cache: 1h
  timeout_seconds: 900
  max_tries: 1
```

**Google Gemini — `GEMINI_API_KEY` or `GOOGLE_API_KEY`**

```yaml
llm:
  provider: gemini
  auth_mode: api_key
  model: gemini-3.8-flash
  reasoning_effort: high
  max_output_tokens: 32768
  capabilities:
    structured_output: fallback
    tool_calling: native
  timeout_seconds: 900
  max_tries: 1
```

**DeepSeek — `DEEPSEEK_API_KEY`**

```yaml
llm:
  provider: deepseek
  auth_mode: api_key
  model: deepseek-flash              # or deepseek-v4-pro
  # DeepSeek currently defaults to thinking enabled, high effort.
  # ellmer 0.4.2 does not forward reasoning_effort on this route.
  max_output_tokens: 32768
  capabilities:
    structured_output: fallback
    tool_calling: native
    reasoning_effort: unsupported    # connector limitation; thinking is not disabled
  timeout_seconds: 900
  max_tries: 1
```

OpenAI forwards `high` to its reasoning setting; Gemini maps it to
`thinkingLevel`; Claude enables adaptive thinking with high effort. These
profiles retain provider-specific structured-output modes; schema adherence does
not establish translation correctness. See the official
[OpenAI model documentation](https://developers.openai.com/api/docs/models/gpt-5.6-terra),
[Gemini thinking guide](https://ai.google.dev/gemini-api/docs/generate-content/thinking),
and [Claude thinking guide](https://platform.claude.com/docs/en/build-with-claude/thinking-steering-and-cost).

DeepSeek's current API defaults to thinking enabled at high effort. With ellmer
0.4.2, sas2r relies on that server default: setting `reasoning_effort: high` and
marking it supported now stops the run when the connector tries to drop it. Omitting the field
does **not** mean thinking is off. This default is documented by
[DeepSeek](https://api-docs.deepseek.com/guides/thinking_mode/); it is not evidence
of the reasoning used in a particular historical response.

**Other provider routes**

| Provider | Reasoning configuration with ellmer 0.4.2 |
|---|---|
| `vertex` | Gemini thinking models use `high` with startup verification, as above; use ADC and your project/location. |
| `posit` | The Claude route supports the Claude profile's reasoning settings; the OpenAI-compatible route does not forward effort. |
| `azure`, `bedrock`, `databricks`, `snowflake` | These connectors do not forward `reasoning_effort`; endpoint/model defaults apply. They cannot explicitly enforce high reasoning through this YAML setting. |
| `ollama` | Reasoning support depends on the served model; the connection example does not establish thinking support. |
| `github` | Retired upstream; not recommended for new runs. |

Full connection examples for all twelve providers, including required cloud
selectors, are in [docs/llm-providers.md](docs/llm-providers.md). Use
`sas_llm_models()` to check model availability and `sas_llm_probe()` to test the
connection; neither certifies translation accuracy.

After a run, inspect `<out_dir>/.sas2r/llm_log.jsonl`: `requested_parameters`,
`effective_parameters`, and `withheld_parameters` distinguish requested settings
from what sas2r handed to ellmer. Connector warnings about dropping an explicitly
required setting now stop the run. A missing effort setting does
not establish that the provider disabled reasoning.

---

## What the AI Model Sees — and What It Never Sees

By default, the AI model receives your SAS code, the translated R code, column names and types, and — when outputs differ from references — a summary of the differences: which variables, how many cells, how large the gaps are. Not the data itself.

Two optional features can share small, capped extracts, and only if you turn them on:

- The reviewer's bounded comparison report may quote a handful of example differences, including row numbers, key values, and the differing cell values (which can include subject identifiers when your key columns identify subjects).
- Setting `agent_evidence = "bounded"` adds capped output summaries with short previews to repair evidence. The default, `agent_evidence = "code_only"`, shares neither.

All data reading, program execution, and output comparison happen in your local R session. Before enabling any provider, confirm the endpoint you configure meets your organization's data residency requirements.

---

## Handy Checks Before a Long Run

<!-- sas2r-example: network connectivity -->
```r
library(sas2r)

# Is my _sas2r.yml valid? (paths, provider names, settings)
sas_config("_sas2r.yml")

# Which models can my account use? (name any model to identify yourself;
# the answer lists everything your account serves)
sas_llm_models(list(provider = "anthropic", model = "claude-sonnet-4-6"))

# Does my sign-in actually work?
sas_llm_probe(list(provider = "anthropic", model = "claude-sonnet-4-6"))

```

<!-- sas2r-example: offline readme-comparison -->
```r
# Compare saved outputs with known unique subject keys. This makes no AI calls.
# result$outputs_dir is the selected output snapshot for this run;
# named-library datasets retain their library subdirectory.
comparison <- compare_datasets(
  base = haven::read_xpt("data/reference/adsl.xpt"),
  comp = readRDS(file.path(result$outputs_dir, "adam", "adsl.rds")),
  keys = c("STUDYID", "USUBJID"),
  profile = compare_profile(abs = 1e-8, rel = 1e-8)
)
passed(comparison)
write_comparison_report(comparison, file = "adsl-comparison.md")
stopifnot(passed(comparison))
```

`compare_datasets()` uses row order when keys are omitted and pairs duplicate
keys by occurrence. For repeated records or inferred keys, use
`compare_aligned_outputs()` as shown in the [comparison guide](docs/output-evidence.md).
These standalone checks produce comparison evidence without changing the saved
migration status or rerunning the bundle.

---

## For Regulated Submissions

> [!NOTE]
> `sas2r` is an **accelerator for statistical programming and migration work**. In regulated submissions (FDA, EMA, PMDA), automated translation does not replace formal double programming, independent code review, or your quality control procedures. Review all migrated code according to your organization's SOPs.

---

## License

Apache License 2.0. See [LICENSE.md](LICENSE.md) for details.
