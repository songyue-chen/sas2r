# What your AI provider can receive

[Back to the README](../README.md#privacy-what-your-model-provider-can-receive).

Dataset reading, generated R execution and output comparison run on the machine
where you run sas2r, including its local R subprocesses. When AI is enabled,
translation, review and repair send prompts and tool results through ellmer to
your configured provider endpoint. The normal workflow does not attach dataset
files or TLF files to model requests, but **local data processing does not mean
that no sensitive information can reach the model**.

With the default `agent_evidence = "code_only"`, a request can contain:

| Information | Examples |
|---|---|
| SAS source and comments | Program statements, macro definitions and calls, included source, attached comments, and literal values written in the source |
| Generated R and review evidence | Current or proposed R code, shared helpers, syntax/lint failures and source-supported review findings |
| Project context | Program and dataset names, column names and types inferred from code, dependencies, macro arguments, filenames, library paths and the execution root |
| Execution diagnostics | Recognized error kinds, standard condition classes, failed component identifiers, source-verified object/dataset/column names, source call locations and local log paths; no raw messages or log contents |
| Investigation/report summaries only | Relative row/column sizes, complete/partial row pairing, type differences, some/all/missing-only variable differences and enumerated patterns; no exact counts, difference amounts or per-record values |
| Guidance and lookup results | Helper interfaces, installed-package versions, translation rules, registered skills and enabled local documentation mirrors |

Both `code_only` and the legacy `bounded` label exclude dataset previews and raw
runtime messages. Raw, reference and patient records, row numbers, subject
identifiers, key values, cell values, min/max/quantile statistics and distinct
values are not included.
This is not source-code anonymization: literal values or identifiers already in
SAS/R code, comments or project paths can still reach the provider.

The translator, fixer and candidate acceptance reviewer receive no reference-derived
summaries. The focused mismatch reviewer may investigate approved aggregate patterns
and must return a source-grounded SAS/R contradiction before a repair is justified.
Comparison detail files and their tool handles are unavailable to all these roles.
A SAS input that is also configured as a reference keeps its local execution role;
that does not authorize forwarding its records to a model.

See the [field types and origins](discrepancy-evidence.md) for the closed summary contract.

The report uses the same measured summary with human-only difference amounts.
It reuses current source-review evidence where available. Otherwise
`migration.report_diagnosis: auto` permits one bounded, budget-accounted explanatory
request with code and approved patterns, announces the provider, and reports
unavailable/incomplete diagnosis without changing validation status. Set
`migration.report_diagnosis: off` to disable these explanations. Report prose never
feeds repair or acceptance requests.

All authoring/review roles can retrieve paged SAS and selected R for related
upstream/downstream dependencies, including indirectly called macros. Scheduling-only
`execution_before` edges do not make every earlier program a code dependency.
Deferred WORK reads can expose possible preceding programs with a matching static/macro write or unknown dataset effect, nearest first and explicitly labeled
unconfirmed. Paging uses existing tool limits; repeated unavailable page requests
stop within the invocation. Review identities include all retrievable selected code,
so an indirect dependency change invalidates cached evidence.

The configured `allowlist` (a comma-separated string or YAML list) is used
consistently in prompts, package facts, helper checks and lint. An explicit list
replaces the default `base, dplyr, tidyr, ggplot2, stringr, forcats, purrr, lubridate, tibble, haven, stats, utils, graphics, grDevices, grid`.
Packages must be installed where the bundle executes; the style guidance names
only allowlisted packages that are installed.
Installed package versions are descriptive context: a package update alone does
not discard saved translations on resume. Execution checks still run with the
current environment.

The translation report includes advisory dependency-symbol, direct-file-I/O and
candidate-file byte-change notices. File-change notices are human-only and do
not drive repairs or selection. Byte changes can reflect timestamps or a valid
source correction. **Generated R is not a filesystem sandbox:** validation of
model-written helper patches and direct-I/O notices do not prevent arbitrary
local file reads. Full runtime filesystem isolation is not provided.

Token summaries label known total input/output usage separately from cached,
cache-creation and reasoning categories. Reasoning is already included in total
output; unknown usage is reported rather than treated as a complete bill.
Nonlocal-assignment notices flag explicit enclosing/global writes for source-scope
review. They are advisory and do not trigger a fixer on their own.

Only a source-grounded finding can turn a reference mismatch into a code repair.
A correction can be retained despite an inconsistent reference; a failed
required reference still reports `blocked`. These controls protect repair
decisions and do not prove arbitrary SAS/R equivalence.

Before using AI with confidential programs or clinical data:

- Use an endpoint approved by your organization. Confirm its data residency,
  retention, access and model-training terms for your account. sas2r does not
  set those provider policies.
- Review source, comments, paths and custom guidance for sensitive content before
  a run. Neither evidence-policy label enables dataset previews.
- For offline inspection, use `sas_preflight(diagnose = "off")` or the standalone comparison
  functions. In `sas_translate()`, `usage_limits = list(max_calls = 0)` prevents
  model requests, including automatic settings probes. `llm = NULL` alone can
  still use the provider in `_sas2r.yml`; `execute = FALSE` disables execution,
  not AI calls. Without model calls, unsupported translations and AI reviews
  can remain incomplete.
- Treat the local run folder as confidential too: scripts, reports, diagnostic
  logs and saved outputs can contain sensitive information. The
  `<out_dir>/.sas2r/llm_log.jsonl` and `usage.jsonl` files record model settings,
  calls and usage metadata, not a complete transcript of everything sent. Review
  local artifacts before sharing them.

See the [output-evidence guide](output-evidence.md#4-data--model-privacy-boundary)
for local comparison and audit details. This section describes the R package;
it does not describe the separate sas2r.ai website.
