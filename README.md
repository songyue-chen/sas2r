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

AI translation requires **ellmer 0.4.2 or newer**. We recommend **ellmer 0.5.0**
for new installations; both versions pass our offline compatibility tests.
Deterministic workflows can run without ellmer.

<!-- sas2r-example: network install -->
```r
# install.packages("remotes")
install.packages("ellmer")
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
  max_bundle_repairs_per_component = 2, # bundle repairs per component
  max_bundle_repair_rounds = NULL, # optional overall cap on bundle fixer calls
  agent_evidence = "code_only"     # what repair evidence the AI may see
)

# What you get back
result$status        # one of the four statuses below
result$bundle_dir    # all available selected programs and macros, including failed code
result$outputs_dir   # saved deliverables: datasets/<library>/ and tlf/ (NULL if none)
result$report_path   # a readable report of everything that happened

# Read a translated program
cat(sas_code(result, 1))

# Export the selected code, generated outputs, reports, and run guide
sas_write(result, "r_production/")
```

Open `migration_output/<run_id>/START_HERE.html` first. It links the selected
scripts, errors, output comparisons, and instructions for running or editing code.

```text
<run_id>/
  START_HERE.html
  manifest.json
  bundle/                 # all available selected code, including failed code
    README.md
    run.R
    autoexec.R
    programs/
    macros/
    runtime/
    output/               # manual reruns only; created when needed
  outputs/                # saved automated deliverables
    datasets/<library>/
    tlf/
  report/                 # translation.md, report.json, comparison-details/
  diagnostics/            # attempts, revisions, smoke tests, logs, scratch data
```

A component that produced no code is labelled **not generated**. A completed
execution is separate from reference equivalence; saved outputs can be partial
or unvalidated. Requested WORK datasets are deliverables too. Unrequested scratch
files stay in diagnostics, and previous attempts retain their own evidence.

From `result$bundle_dir`, run `Rscript run.R` or `source("run.R")` in a fresh R
session. The launcher loads the runtime and macros, then runs programs in
order. Manual writes use `.sas2r_output_root` in `autoexec.R`, initially
`bundle/output/`; they do not overwrite saved automated outputs. Change that
root to keep separate manual iterations. Relative report files are written below
`output/tlf/`; review explicit paths or other relative file access in edited code.

`sas_write()` copies this editable bundle and its guide. Saved automated outputs
are exported separately under `saved-outputs/`, and reports under `report/`.
Copy the whole bundle when moving it, install the R packages listed in its guide,
and either copy inputs or configure accessible external data paths. No LLM key or
sas2r installation is needed to run the included runtime. Failed scripts remain
available for human repair; edits and reruns require new QC.

Relative paths inside translated LIBNAME calls resolve against
`.sas2r_execution_root` in `autoexec.R`, initially the source project directory.
Relative paths in the registry itself remain relative to the bundle folder.
Rebinding a known physical library retains its separate write folder, so later
programs can read earlier generated datasets. New library assignments receive
separate output folders; an explicit `write_path` still takes effect. Missing
input errors list the paths searched and execution root.

Input data is not copied: update `.sas2r_execution_root`, the registry in
`autoexec.R`, and any explicit source LIBNAME
paths if those inputs move. A manual rerun does not update the exported report.

Bundle repair defaults to **two fixer calls per component**, in addition to the
immediate program-repair allowance. `max_bundle_repair_rounds = NULL` lets that
bounded allowance scale with the number of components; supply a number to cap
total bundle fixer calls, or `0` to disable them. `usage_limits` still bounds the
whole run. Failed and ineffective repair calls consume their component allowance.

The authoritative bundle run stops at the first execution error. sas2r then
checks unvisited independent branches in fresh smoke environments, queues known
mechanical and output failures by component, and repairs independent causes
before rerunning the full bundle. Downstream failures caused by an upstream
blocker wait for fresh evidence. An identical patch or failed fixer is deferred;
other independent components can continue. A shared-helper patch requires a
fresh run before further repairs. Diagnostic outputs never replace the complete
bundle acceptance check, and lost execution or output coverage rejects a repair.
Repair counts, deferred components, and diagnostic records appear in the report.

Each translator, reviewer, or fixer invocation has a **30-tool-call shared
allowance** by default. Individual tools inherit that allowance, so several
useful rule or macro lookups do not exhaust a separate small quota. A project
can adjust a role in `.sas2r/agents/translator.yml` (likewise `reviewer.yml` and
`fixer.yml`):

<!-- sas2r-example: agent translator -->
```yaml
tool_call_limit: 45
tools:
  search_skills: { max_calls: 6 } # optional narrower quota for this tool
```

The shared ceiling and any explicit tool quotas remain enforced; they reset on
the next agent invocation. The run-wide `usage_limits$max_tool_calls` is separate.
The model sees its remaining allowance. At exhaustion, native ellmer gathering
stops and the runner requests a final answer with tools closed and the evidence
already collected. Final-answer retries remain bounded; an incomplete answer
still cannot pass validation. Expected tool refusals and invalid arguments return
feedback, while unexpected tool errors remain visible. Completing an agent
request does not mean that every requested lookup succeeded.

Before generation, the console shows loaded R, sas2r and ellmer versions and the
effective agent and repair limits. The same information is saved in the run report
and usage ledger. Tool records include component, revision, request and invocation
identifiers; completion messages identify denied or failed lookups.

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
| `reviewer ...: review completed: repair_required, 3 tool calls` | The request completed and the review found a material issue. The tool count describes tool use, not findings. Older logs use `ok` for request completion alone. |
| `coordinator ...: mechanical checks passed` | Generated code passed syntax and mechanical contract checks. |
| `coordinator ...: review completed -- reviewed_no_material_finding` | The review found no material issue. Execution and reference comparison remain separate checks. |
| `coordinator ...: review unavailable` | No usable review was obtained. The reason is recorded, and smoke execution may continue. |
| `smoke ...: passed` | The program executed and any applicable source population checks passed. Inspect unverified checks separately; this does not establish agreement with SAS reference data. |
| `smoke ...: failed -- blocked by upstream: ...` | Execution failed in the named dependency. The consumer is recorded as blocked and is not sent to the fixer for that upstream crash. |
| `bundle ... assessed -- migration_ready` | The selected bundle met the requirements described above. Read reference coverage separately. |

Review findings can trigger a fixer even after mechanical checks and smoke tests
pass. A repaired revision is checked, reviewed, and executed before selection.
A repair that loses passing mechanical, execution, review, or source population
evidence is rejected; the previous revision and its unresolved findings remain.
Errors include the underlying R condition and saved execution logs. If a current
run is blocked while an older selected bundle remains on disk, the progress log
identifies that older selection explicitly.

`max_program_repair_rounds` is the total immediate-repair allowance per
component for the run, including revisits. A deferred component is retried when
its relevant dependencies change or a specifically awaited caller becomes
available. Completed reviews are reused within a run when the code, dependency
code, helper runtime and review configuration are unchanged.

Each fixer invocation can make one additional, budgeted request to correct a
parse or lint error in its proposed code. Persistent mechanical failures keep
the previous selected revision and save the rejected candidate's diagnostics.
An identical patch means no change was proposed; it does not clear the failure.
Bundle repair can still address a separately documented downstream code defect
after an upstream no-op. Downstream mismatches inherited from unresolved inputs
wait for those inputs to be resolved.

Unexpanded output expressions, such as `figure-&group..pdf`, appear separately
from concrete output targets in the report and starting page. Finding matching
files does not establish that the entire expected family was produced. An
unresolved required expression prevents readiness; its filenames and coverage
need source-grounded review.

For debugging a failed bundle, use `sas_translate(..., keep_raw_attempts = TRUE)`
to retain each component smoke execution's separate library folders and `run.R`
replay script. The component's `smoke_execution` in `report.json` lists the
output paths, hashes, configured-input hashes from run startup and replay path. These are partial
execution artifacts, not reference-validated final datasets. By default, raw
unselected outputs are pruned; smoke records and logs remain available under
`smoke_attempt_001/logs/` in the run folder.

Translator, reviewer, and fixer receive runtime signatures, argument rules,
return values, limitations, and examples directly from the package's helper
reference. Mechanical checks reject invented helper arguments before execution.
For equality, use `chr_cmp(a, b, op = "==")`; `op = "="` raises an error.
Omitting `op` returns an ordering result (`-1`, `0`, `1`), so an explicit operator
is required when using the result as a Boolean condition.

Source population checks use parsed SAS and local inputs, independently of the
agent's declared contract. Supported checks cover row counts for single-input
SET steps without row filters and BY-group counts for two-input MERGE steps with
simple IN filters. For example, a matched subject with three events must retain
three records. Filters, unsupported step bodies, repeated member writes, source library
assignments within a program, and unavailable intermediates remain `unverified`; see `source_population` in the
Markdown report and `source_population_checks` in component JSON evidence.
Incompatible BY types between source inputs leave the population check
`unverified` with a diagnostic. When the source keys are compatible but the
translation changes the output to an incompatible type, the check fails.
These checks cover population behavior, not all derived values or SAS parity.

The elapsed-time and
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

## Called macros in separate folders

Configure macro directories in `_sas2r.yml`; relative paths are resolved from
that configuration file:

```yaml
macros:
  search_path:
    - macros
    - shared/macros
```

When a program calls `%my_macro(...)`, sas2r finds its definition in those
folders and translates it into `bundle/macros/my_macro.R`. It also follows calls
from that macro to other macros. Each called definition has one reusable R
function, even when several programs use it or several definitions share a SAS
file. Uncalled library macros are not sent for translation.

Macros are translated before their callers. Agents receive upstream function
contracts, and the bundle's `autoexec.R` loads the standalone functions before
programs run. `sas_write()` exports the macro scripts and generated
`tests_macros/test-<name>.R` interface tests. Those tests check the function name,
parameters and known defaults; execution and SAS-reference comparison provide
separate evidence about behavior.

Resolved project macro calls are tracked separately from runtime helpers.
Repairs refresh helper-use metadata without treating project functions as
unknown runtime helpers. Standalone smoke tests use an available caller's first
top-level call with literal arguments, preserving multiline calls. A caller
that has not been generated reports `caller_not_generated`; calls requiring
prior setup, variables, loops or other enclosing context report
`caller_context_required` and run as part of their containing program. A smoke
pass for that program does not establish that every conditional macro ran.

Programs that define and invoke internal macros must retain that invocation.
A mechanical check catches definitions-only R when the source invokes a macro
outside its definitions. Shared guidance also covers returned values, nested
macro scopes and repeated calls, including intentional clearing or retention of
state. Graphics guidance preserves source statistical definitions through the
renderer, requested summary content, axis behavior and pagination settings;
visual polish remains a human task.

For explicit dataset cleanup, generated functions can use
`lib_delete("work", c("scratch_a", "scratch_b"))`. This removes stored datasets;
garbage collection is not a substitute. Dataset-name ranges, prefix lists and
`_ALL_` need expansion into explicit names. Deleting a member backed by a
separate input directory remains unsupported, with input files preserved.
Unresolved dynamic expressions remain explicit limitations; agents must not
bypass them with `eval()`/`parse()` or silently drop meaningful operations.

Inspect `sas_preflight(...)$called_macros` for the discovered definitions and
planned file paths. Macro translation requires an AI provider. Dynamic macro
names, nested macro definitions, `%INCLUDE` inside an autocall macro, and library
files with executable initialization outside their macro definitions remain
unresolved. Translation
stops before model calls and reports the macro name, source file and line. If a
definition is missing, configure `macros.search_path` or supply the definition
in the scanned sources, then run preflight again. Preflight returns dependency
findings for inspection; malformed source such as an unterminated macro comment
raises a parse error with its location. sas2r does not expand arbitrary SAS macro
code. Top-level `%LET`, `OPTIONS`, and other initialization in an autocall file
are not silently discarded: they can change macro values, execution, or output.

Dependency detection classifies percent-prefixed syntax before looking up user
macros. Definitions, control statements, built-in functions (including documented
NLS macro functions and SAS-supplied NLS autocall macros), `%INCLUDE`, `%LIST`,
`%RUN`, and `%label:` declarations are not user calls. Macro/block comments and
single-quoted literals do not introduce calls; double-quoted text can. Simple
`%NRSTR(...)` text is treated as literal. Computed names and quoting that requires
expansion (such as `%UNQUOTE(%NRSTR(%generated_call()))`) stop with an explicit analysis
finding, rather than a missing-macro-file diagnosis. Macro text inside ordinary
SAS `* comment;` statements also requires expansion and is reported separately.
Unquoting a variable alone, such as `%UNQUOTE(&condition)` in a WHERE expression,
is advisory: it does not prove that a user macro is called. Such generated text
remains unverified; offline mapping does not fully execute the macro language.

For literal SQL patterns, use single quotes, for example `like '%Total%'`.
In `like "%Total%"`, SAS attempts to invoke `%Total`, even without parentheses.
sas2r stops if it cannot resolve that name: assuming literal text would also hide
a genuine call whose macro folder was not configured. Use SAS macro quoting
when literal percent text must coexist with macro expansion.

SAS requires matching quotation marks within `%* ...;` comments; a semicolon
inside matched quotes does not end the comment. Use `/* Don't run this step */`
for prose with an unmatched apostrophe. `%STR` and `%NRSTR` require a preceding
percent sign for unmatched quotes or parentheses, for example
`%nrstr(Don%'t modify this table)`. Ordinary `* ...;` comments can still execute
macro statements; use block comments for inactive macro text.
See SAS's [macro comment rules](https://support.sas.com/documentation/cdl/en/mcrolref/61885/HTML/default/a000543665.htm)
and [macro quoting rules](https://support.sas.com/documentation/cdl/en/mcrolref/61885/HTML/default/a001061290.htm).

Statement splitting does not fully support macro-quoted semicolons, such as
`%exec_sql(query=%str(select a; select b;))`. Simplify these forms or supply
expanded source before translation; a discovered macro name does not prove
that its argument or statement boundaries were parsed correctly. Similarly,
`call execute('%my_macro(' || id || ')')` constructs code at runtime, so its
single-quoted text is not mapped as a static call. Provide expanded source and
the required macro definitions for such programs.

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
switching providers. Version-specific notes below describe ellmer 0.4.2's
parameter mappings as the minimum supported baseline, not a requirement to
install that exact version. Ellmer 0.5.0 is also supported and recommended for
new installations. Startup verification checks the settings against your
installed connector and selected model.
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
  comp = readRDS(file.path(result$outputs_dir, "datasets", "adam", "adsl.rds")),
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
