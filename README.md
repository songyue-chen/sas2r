# sas2r: SAS to R Translation & Migration Evidence for Clinical Programming

> **Use R to get R.**

<!-- badges: start -->
<!-- badges: end -->

**sas2r** is an open-source R package for **clinical statistical programmers and biostatisticians** in pharmaceutical, biotech, and CRO organizations. It runs a **coordinated multi-agent workflow** that moves clinical trial data pipelines (SDTM, ADaM, Tables, Listings, and Figures) from SAS to R — an AI translator, an independent AI reviewer, and an AI fixer, each with a defined role inside a deterministic process — and shows you the evidence for every step it took.

`sas2r` does not require SAS. No license, no installation, no connection to a SAS server. Everything runs in R, on your own computer or your organization's own infrastructure.

> **Also check out [sas2r.ai](https://sas2r.ai)** — a web-based companion tool for quick, browser-based SAS to R code translation. While sas2r.ai currently uses direct model translation for rapid code conversions, we plan to bring this R package's multi-agent workflow and dataset QC capabilities to the cloud platform in the future!

---

## Why Statistical Programmers Use sas2r

- **Built for clinical data work.** The rule-based translator handles the bread-and-butter patterns on its own — DATA step derivations and filters, `MERGE (in=a in=b)`, `PROC SORT`, `MEANS`, `FREQ`, `FORMAT`, and simple `PROC SQL`. It also knows SAS habits R does not share: how missing values sort and compare, trailing blanks in character values, and case-insensitive variable names all behave the SAS way in the translated code.
- **Honest about what it can't do.** SAS patterns the rules cannot prove — `RETAIN`, `FIRST.` / `LAST.` logic, `OUTPUT` statements, `PROC TRANSPOSE`, macros — are never silently guessed. Each one is clearly marked and handed to an AI translator, and a second, independent AI reviewer reads the result against your original SAS before it is accepted.
- **Automated dataset QC.** If you provide reference SAS datasets (`.sas7bdat`, `.xpt`, or `.rds`), `sas2r` compares each generated R dataset against them: it lines up rows even when their order differs, understands duplicate key values, applies SAS missing-value and blank-padding rules, and checks numbers to configurable tolerances.
- **Your source data is never touched.** Input libraries are opened read-only, and every run writes into its own separate working copy (copy-on-write), so a failed attempt can never contaminate your data or a previous good result.
- **Standalone R programs you can take anywhere.** The result is a directory of plain R scripts plus one small helper file. They run on Posit Workbench, a laptop, a server, or a batch system — without `sas2r` installed.
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

### 3. Run it

```r
library(sas2r)

result <- sas_translate(
  path = "programs/",              # one .sas file or a whole directory
  out_dir = "migration_output",
  config = "_sas2r.yml",
  execute = TRUE,                  # actually run the translated programs
  max_program_repair_rounds = 1,   # immediate repair attempts per program
  max_bundle_repair_rounds = 2,    # full-pipeline repair attempts
  agent_evidence = "code_only"     # what repair evidence the AI may see
)

# What you get back
result$status        # one of the four statuses below
result$bundle_dir    # the finished, standalone R programs
result$outputs_dir   # the datasets the selected run produced
result$report_path   # a readable report of everything that happened

# Read a translated program
cat(sas_code(result, 1))

# Copy the finished bundle and report to your production folder
sas_write(result, "r_production/")
```

### The four statuses

Every run ends in exactly one status. It is decided from what actually executed and what the output checks found — never from what an AI model claims:

- **`blocked`** — something required went wrong: a program failed to run, or a required output is missing or doesn't match. The report names the program and shows the evidence.
- **`needs_review`** — code was produced but nothing has demonstrated the outputs yet (for example, you set `execute = FALSE`, or the AI review could not be completed).
- **`migration_ready`** — every program ran cleanly and every required output passed its checks. No reference comparison was involved.
- **`validated`** — on top of `migration_ready`, the outputs were compared against the reference SAS datasets you configured, and the comparison passed within your tolerances.

**THIS IS NOT PARITY.** `validated` means your configured reference comparisons passed under the tolerances you declared. It is not proof of SAS equivalence, and it does not replace double programming or independent statistical QC.

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

### Every available setting, in one example

Only `provider` and `model` are required. Everything else below is optional, shown with example values:

```yaml
llm:
  provider: anthropic          # one of the twelve provider names below
  model: claude-sonnet-4-6     # or name models per tier instead:
  tiers:
    frontier: claude-opus-4-6     # the tier today's workflow uses -- put your
                                  # strongest model here (an Opus-class model;
                                  # Sonnet-class models are the mid tier)
    cheap: claude-sonnet-4-6      # accepted and validated; reserved for
    fast: claude-haiku-4-5        # roles that opt into cheaper tiers

  auth_mode: api_key           # how sas2r signs in; each provider's
                               # choices and default are listed below
  timeout_seconds: 300         # patience per request (default 300)
  max_tries: 1                 # transport attempts per request (default 1;
                               # sas2r retries brief outages on its own)

  temperature: 0               # model settings, passed through when the
  top_p: 1                     # provider supports them
  reasoning_effort: high       # max_output_tokens is deliberately unset here:
                               # sas2r then uses the model's own maximum. Cap it
                               # only if you must -- a small ceiling is spent on
                               # reasoning before any answer text emerges, and
                               # the translation comes back truncated

  cache: 1h                    # prompt-cache lifetime (anthropic, posit,
                               # bedrock). 1h is the default: migration
                               # turns are minutes apart, so a 5m cache
                               # would expire between them

  capabilities:                # only if you need to override what sas2r
    structured_output: fallback   # detects about a model's abilities
    tool_calling: native
```

**Never write an API key into `_sas2r.yml`.** Leave keys out of the file and set the provider's environment variable instead (listed below); `sas2r` finds it there. Files get shared and committed — environment variables don't.

### What each provider needs

Add these provider-specific lines to the `llm:` block. Sign-in styles: **api_key** (an environment variable holds your key), **ambient** (your machine's existing cloud sign-in is used, e.g. AWS or Google credentials), **none** (no sign-in, e.g. a local model).

```yaml
# anthropic — sign-in: api_key. Key from ANTHROPIC_API_KEY.
provider: anthropic
model: claude-sonnet-4-6
# base_url: https://your-company-gateway.example.com   # only if IT routes traffic
# cache: 1h                                            # default 1h; also 5m or none

# openai — sign-in: api_key (default) or ambient. Key from OPENAI_API_KEY.
provider: openai
model: gpt-5.6-terra
# base_url: https://your-company-gateway.example.com/v1

# gemini — sign-in: ambient (default) or api_key. Key from GOOGLE_API_KEY or GEMINI_API_KEY.
provider: gemini
model: gemini-3.7-flash

# deepseek — sign-in: api_key. Key from DEEPSEEK_API_KEY.
provider: deepseek
model: deepseek-v4-pro

# azure — sign-in: ambient (default) or api_key (AZURE_OPENAI_API_KEY).
provider: azure
model: my-gpt-deployment          # your deployment name, chosen in Azure
endpoint: https://my-resource.openai.azure.com
api_version: 2024-10-21

# bedrock — sign-in: ambient (your AWS credentials / AWS_PROFILE).
provider: bedrock
model: us.anthropic.claude-sonnet-4-6-v1:0    # a Bedrock model id from your account
region: us-east-1
base_url: https://bedrock-runtime.us-east-1.amazonaws.com
# profile: my-aws-profile
# cache: auto

# vertex — sign-in: ambient (GOOGLE_APPLICATION_CREDENTIALS).
provider: vertex
model: gemini-3.7-flash
project_id: my-gcp-project
location: us-central1

# databricks — sign-in: ambient (DATABRICKS_TOKEN or workspace sign-in).
provider: databricks
model: databricks-claude-sonnet-4-6   # an endpoint name from your workspace
workspace: https://my-workspace.cloud.databricks.com

# github — sign-in: api_key. Key from GITHUB_PAT.
provider: github
model: gpt-5.6-terra

# ollama — sign-in: none. A model running on your own machine.
provider: ollama
model: llama3.3
base_url: http://localhost:11434

# posit — sign-in: ambient (Posit Connect credentials).
provider: posit
model: claude-sonnet-4-6
# cache: 1h                           # default 1h; also 5m or none

# snowflake — sign-in: ambient (SNOWFLAKE_TOKEN or key pair).
provider: snowflake
model: claude-sonnet-4-6              # a model your Snowflake account serves
account: my-account-identifier
```

The example model names for providers outside the live-run table are placeholders in the right shape — your account's own list (from `sas_llm_models()`) is the source of truth, and `sas2r` validates your choice when the configuration loads. The complete reference — every setting, sign-in mode, and troubleshooting — is in [docs/llm-providers.md](docs/llm-providers.md).

---

## What the AI Model Sees — and What It Never Sees

By default, the AI model receives your SAS code, the translated R code, column names and types, and — when outputs differ from references — a summary of the differences: which variables, how many cells, how large the gaps are. Not the data itself.

Two optional features can share small, capped extracts, and only if you turn them on:

- The reviewer's bounded comparison report may quote a handful of example differences, including row numbers, key values, and the differing cell values (which can include subject identifiers when your key columns identify subjects).
- Setting `agent_evidence = "bounded"` adds capped output summaries with short previews to repair evidence. The default, `agent_evidence = "code_only"`, shares neither.

All data reading, program execution, and output comparison happen in your local R session. Before enabling any provider, confirm the endpoint you configure meets your organization's data residency requirements.

---

## Handy Checks Before a Long Run

```r
library(sas2r)

# Is my _sas2r.yml valid? (paths, provider names, settings)
sas_config("_sas2r.yml")

# Which models can my account use? (name any model to identify yourself;
# the answer lists everything your account serves)
sas_llm_models(list(provider = "anthropic", model = "claude-sonnet-4-6"))

# Does my sign-in actually work?
sas_llm_probe(list(provider = "anthropic", model = "claude-sonnet-4-6"))

# Compare any two datasets directly, the same way the pipeline does.
# (result$outputs_dir points into the selected attempt of the latest run --
# each run gets its own run_<timestamp>_<hash>/ folder in the output
# directory, with the translated programs and report at its top, so reruns
# never overwrite each other.)
report <- compare_datasets(
  base = haven::read_xpt("data/reference/adsl.xpt"),
  comp = readRDS(file.path(result$outputs_dir, "adsl.rds")),
  keys = c("STUDYID", "USUBJID")
)
print(report$passed)
```

---

## For Regulated Submissions

> [!NOTE]
> `sas2r` is an **accelerator for statistical programming and migration work**. In regulated submissions (FDA, EMA, PMDA), automated translation does not replace formal double programming, independent code review, or your quality control procedures. Review all migrated code according to your organization's SOPs.

---

## License

Apache License 2.0. See [LICENSE.md](LICENSE.md) for details.
