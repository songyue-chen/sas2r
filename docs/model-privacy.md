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
| Execution diagnostics | Error messages, stack traces, capped stderr excerpts, failed component identifiers and source-derived checks, including expected/actual row counts or counts of mismatched BY groups |
| Guidance and lookup results | Helper interfaces and limits, observed installed-package versions, translation rules, registered skills, and matching documentation from an enabled local documentation mirror |

**`code_only` omits explicit dataset-row and output previews; it is not an
anonymization setting.** For example, a subject ID in a SAS filter, a name in a
comment, or a patient value printed in an error can appear in a request. Paths
can reveal usernames, study names or internal folder structure. Credential
redaction in audit/error handling is not general removal of clinical identifiers
from prompts, source code or execution logs.

Setting `agent_evidence = "bounded"` permits capped candidate-output summaries
and previews in execution diagnostics where available. These may contain
row numbers, key values, cell values and subject identifiers. A cap limits the
amount of information; it does not de-identify it.

Reference comparison values, subject IDs, counts, difference hints and reports
are excluded from translator, reviewer and fixer requests and tools, including
project tool overrides. A focused source review can receive the affected output
names and the fact that a mismatch occurred. It receives no reference answers
to imitate. A dataset that the SAS legitimately reads retains its input role
even if it is also configured as a reference; that does not make its source
usage or execution diagnostics confidential to the local process.

Each role receives the same source policy and bounded selected direct-dependency
SAS/R bodies (up to 6,000 characters per body and 24,000 characters per packet).
Missing or truncated context is identified. The read-only `read_dependency_context`
tool lets all three roles retrieve bounded pages of SAS source and selected R
code for direct dependencies and consumers, up to 12,000 characters per call.
Calls use the existing tool budget. They add no dataset or reference-output
access. Missing-context and capability findings remain visible;
labels alone do not cancel a proven execution or translation failure.

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
- Keep `agent_evidence = "code_only"` unless sharing candidate previews is
  approved. Review source, comments, paths, custom guidance and error-producing
  code for sensitive content before a run.
- For offline inspection, use `sas_preflight()` or the standalone comparison
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
