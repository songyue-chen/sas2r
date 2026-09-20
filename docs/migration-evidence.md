# Migration Evidence & Verification Guide

`sas2r` provides a deterministic, evidence-grounded migration framework for translating SAS clinical programming assets into validated R bundles. This document describes the component evidence ladder, bundle verification statuses, reviewer verdicts, attempt contracts, and data residency boundary.

## Component Evidence Ladder

Newly translated components start without an evidence level. A successful smoke
attempt is recorded separately; it does not substitute for a completed review.
The four evidence levels are:

1. **`reviewed_only`**: An independent reviewer evaluated the candidate R code and contract against the source SAS, with no material findings.
2. **`runtime_verified`**: The reviewed revision has execution coverage from an isolated subprocess run or smoke attempt with its upstream dependency chain.
3. **`output_verified`**: The component's output requirements passed the migration gate.
4. **`reference_validated`**: The output lineage has passing reference-comparison evidence within the configured tolerances. Reference provenance must be established separately.

Evidence is strictly immutable per revision binding: any source, code, or helper modification resets evidence for the new revision.

## Authoritative Bundle Statuses

When `sas_translate()` processes a project or SAS file, the final bundle is assigned one of four canonical states:

- **`blocked`**: Bundle execution failed, or a required output is missing, unreadable, or fails its checks, including a configured reference comparison. Dependency cycles and deferred dependency branches also prevent execution.
- **`needs_review`**: Execution was deferred (`execute = FALSE`), or required review/lineage evidence is incomplete or blocked. This can occur even when smoke execution succeeds; the status reason identifies the missing evidence.
- **`migration_ready`**: Bundle execution, required output checks, and lineage requirements passed, without passing reference evidence for a required target. Existence and readability checks alone do not establish matching dataset values.
- **`validated`**: Required output checks and lineage requirements pass, and at least one required target has a passing reference comparison. Other targets can remain unreferenced. This status does not mean every output was compared.

## Reviewer Outcomes & Authority

Independent LLM review is designed to assist human engineers, not to grant unearned certification:

- **`reviewed_no_material_finding`**: The reviewer reported no material issues in the candidate code and contract. This remains an AI review outcome, not an independent SAS execution.
- **`repair_required`**: Reviewer identified material issues (e.g. inverted logic, missing condition, incorrect library mapping) that trigger immediate component repair.
- **`review_unavailable`**: No usable review was obtained, for example because no LLM is configured, a request failed, or a budget limit was reached. Smoke verification can continue, but required lineage with unavailable review keeps the final bundle at `needs_review` unless an execution/output failure makes it `blocked`.

### Reading progress messages

`reviewer ...: ok` records successful completion of the agent call; it is not the
review verdict. `coordinator ...: reviewed` means the coordinator recorded a
review result, which can still contain `repair_required` findings. The report
and component review history contain the verdict and findings.

Mechanical checks, reviews, smoke execution, and bundle output checks establish
different facts. A smoke pass does not clear a mechanical failure or missing
review. A later successful review resolves prior review-specific blockers while
preserving their history. Repairing code creates a new revision whose evidence
must be collected again.

## Dual Repair Loops & Attempt Isolation

`sas2r` uses two separate, bounded repair loops:

1. **Component-Level Immediate Repair**: Fast feedback loop (`max_program_repair_rounds = 1L`) fixing syntax, lint errors, and initial reviewer findings on individual components before full execution.
2. **Bundle-Level Causal Repair**: Each component receives up to `max_bundle_repairs_per_component = 2L` fixer calls across bundle attempts. `max_bundle_repair_rounds = NULL` scales the overall allowance with the component count; an explicit number caps total bundle fixer calls, and zero disables bundle repair. Failed and identical patches count as calls. Run-wide usage limits still apply.

After a bundle stops at an execution failure, isolated smoke checks diagnose
unvisited independent branches. Known mechanical, execution, and output failures
are grouped by component. Independent repairs can share a batch; dependent
repairs wait for fresh evidence after upstream repairs. A shared-helper patch
also requires a fresh run before further repairs. Failed or identical fixes are
deferred without consuming another component's allowance. Diagnostic records
remain separate from authoritative bundle results. Every changed batch is
followed by a fresh complete bundle attempt before selection or acceptance.
The report records repair counts, deferrals, and isolated diagnostics.

A saved selection from an older run does not stop a new run's repair loop.
It remains selected until a new attempt meets the replacement criteria; repair
and usage limits still apply. A regression against an attempt selected within
the current pipeline stops further repair and retains that attempt.

Under the default `agent_evidence = "code_only"` policy, agents receive source code, inferred schemas, project paths and execution diagnostics without explicit dataset previews. Code, comments and errors can still contain patient values or identifiers; this policy does not de-identify them. See the [full privacy explanation](model-privacy.md).

### Source-based repair and selection

A reference mismatch alone does not authorize code changes. It can request one focused source review per unchanged component context, grouping affected outputs. The reviewer traces SAS and R operations without reference counts, values or mismatch-variable hints. An unavailable investigation consumes its opportunity; reference-only changes and resume do not reset it. Actual source, input, dependency or reviewer-context changes may justify a new review within finite budgets.

An inconclusive extra review is recorded separately and retains any completed review of the unchanged code. Only an actionable material source finding updates its active review evidence. An unexplained upstream reference mismatch does not establish that downstream differences are inherited: independent source review remains available within the same per-component cap. Known upstream repair findings take precedence.

Mechanical failures, attributable runtime errors, malformed/missing artifacts and source-grounded findings still enter bounded repair. Candidates are reviewed before subsequent repairs use them. Shared-helper changes are checked with their consumers using candidate files; rejected code, helpers and review records remain local diagnostics while earlier active code is retained. Fresh execution then protects established execution and source-check coverage before selection.

Selection compares individual components and required non-reference checks. Better reference agreement cannot compensate for a source regression, and losing an inconsistent reference match does not veto a source-supported correction. Required reference failures still produce `blocked`; the output assessment separately records `reference_issue` and `source_evidence`. Unverified source checks remain unverified, and a clean static review is not a proof of semantic equivalence.

Across runs, source-review and population comparisons apply to components still present with unchanged source. Population check identities use component and output names, so project-wide statement renumbering does not invalidate them. Required output checks are compared within unchanged source lineage. Removing or editing SAS programs changes that comparison scope; it does not make an older source task permanently selected. A missing external input remains an input problem. A missing intermediate with a uniquely identified, completed upstream writer is routed to that writer for repair, retaining the reader's actual execution error.

### Repair reasons and evidence records

| Reason, event or field | Meaning |
|---|---|
| `no_source_grounded_repair` | Differences remain, but no source-supported defect authorizes another repair. The code and comparison results are retained. |
| `repair_review_regressed` | A candidate's review or mechanical checks regressed, or candidate review raised an error; the retained revision stays active. |
| `source_input_unavailable: <dataset>` | SAS reads a missing dataset that no bundle component is declared to produce. Supply or configure the input; the missing file alone does not authorize a rewrite. |
| `execution_timeout; no translation defect established` | Execution timed out; that observation alone does not establish a code defect. |
| `bundle_repair_rejected` | Progress event identifying the rejected component, attempt and reason. |
| `bundle_source_review_completed` | Progress event reporting the focused review's outcome, including an unavailable review. |
| `source_mismatch_review` | Component-history event storing the attempted context, target identities, review basis and outcome. Inconclusive results do not replace an existing completed verdict. |
| `diagnostics.rejected_repairs` | Rejected fixer revision and paths; `revisions` maps each affected component's artifact revision ID, evidence revision ID and code path. History events link both revision IDs. |
| `diagnostics.selection_rejections` | Per-attempt replacement-rejection details; the retained-selection progress message also includes the reason and previous attempt location. |
| `reference_issue` | An unresolved reference mismatch, separate from code correctness. |
| `source_evidence` | Existing component review verdicts, evidence identifiers and supported source-check counts for an output's lineage. |

After focused review, lineage and status are refreshed from the same attempt's saved target observations; unchanged datasets are not compared a second time.

### Copy-on-Write Attempt Isolation

Each run has a timestamp-first folder directly in the output directory. Attempts are isolated beneath `<run_id>/diagnostics/bundle_attempts/`; revisions, smoke executions, and logs have separate diagnostics folders. Open `START_HERE.html` for navigation. The selected editable code lives in `bundle/`, saved contract-declared deliverables in `outputs/`, and reports in `report/`. Missing code and partial results remain explicit. Manual runs write beneath `bundle/output/` without changing automated evidence. The shared `.sas2r/` folder retains resume state and a copy of the latest report.
In `manifest.json`, every component's `dependencies` field is a JSON array,
including `[]` for no dependencies and `["provider_id"]` for a single dependency.

- Source inputs are never mutated (protected by copy-on-write library registries).
- Attempt outputs, logs, and `record.json` are captured atomically.
- Deterministic selection ensures newer attempts are selected only if they improve upon or maintain previous pass criteria without regressions.

## Parallel coordination and evidence

`migration.max_parallel_translations` defaults to 1 and limits simultaneous
program or called-macro workflows. The matching `sas_translate()` argument
overrides YAML. It does not count translator, reviewer and fixer roles
separately. See the [provider guide](llm-providers.md#recommended-starting-settings)
for suggested model settings and gradual concurrency increases.

The coordinator schedules dependency-ready components and owns selected
revisions, shared request/tool/spending limits, and checkpoints. Separate R
processes use the existing role context builders and tools with snapshots of
the selected upstream revisions. Initial translation and review may overlap.
Smoke execution and complete repair transactions, including affected-consumer
checks, proceed one at a time. Concurrently completed drafts wait for that
transaction to finish before being selected. Final reviews use a fixed
code/helper selection and can overlap; findings from that pass enter the bundle
repair queue.
Whole-bundle execution and repair remain serial.

Translator, reviewer and fixer share a read-only dependency-code reader. When
the initial context omits a needed body, they can retrieve pages of its SAS
source and selected R implementation within the existing tool-call budget.
This does not provide dataset or reference-output access. Generated literal
library assignments are also checked against the paths resolved during preflight.

After an execution failure, the start page names the last failed component,
shows the actual exception and links its logs. Outstanding static reviews are
listed separately: a later stopping stub may exist without having executed.
When an upstream dataset was recorded but a reader cannot find it, repair
investigates the reader's library binding before blaming the producer.

The report's `diagnostics.parallel` records requested and effective concurrency,
the backend and any fallback reason. For a parallel run, its `observed` field
includes peak process count, sampled process memory and coordinator admission
latency. These are run
observations, not CPU allocation or provider capacity guarantees. A custom
adapter without process reconstruction support, or ellmer older than 0.5.0,
falls back to one workflow and reports why. Requesting parallel translation with
`llm.max_tries` above 1 instead stops preflight and translation before provider
calls. The error shows both effective settings and asks you to set either
`llm.max_tries` or `migration.max_parallel_translations` to 1. Settings are not
changed automatically; function argument overrides are honored.

Each process job keeps `job.json`, `stdout.log` and `stderr.log` under
`<run_id>/diagnostics/workers/<job_id>/`. The job record identifies its component
and phase. `completed` means the process returned; `accepted` means the coordinator
merged its result. Neither means the component or bundle passed all quality
gates. Component histories, final reviews and bundle output assessments remain
the acceptance evidence. Selected revisions remain in these durable diagnostic
folders; each manifest component's `revision_id` and `revision_path` map to the
original revision file. Its `code` field points to the editable user bundle.

An ordinary component or worker failure is recorded with its log paths while
other translation work continues. Configuration, provider access, accounting
and artifact persistence failures stop new dispatch; active sibling jobs finish
and their completed work is checkpointed before the error is raised. Saved
drafts are not promoted to passed quality checks. An explicit user interruption
still cancels the run.

Only standalone identifiers, dataset names (`lib.member`) and macro names
(`%macro`) are interpreted as dependency findings. Descriptive sentences stay
in the translation contract for review. When an identifier cannot be reconciled
with the known graph, the coordinator emits a `dependency_warning`. Available
source keeps translating, including affected downstream drafts with the finding
in their context. `diagnostics.dependency_findings` records those findings and
their affected components; `diagnostics.execution_deferred` records why the
full bundle could not execute. Such a run requires review (`needs_review`);
component or mechanical-check failures can instead make it `blocked`. Independent
components can still receive their smoke checks. Automatic graph correction,
task reassignment and partial-bundle execution are separate design items.
Resolve required dependencies before expecting complete execution evidence.

All processes draw from one usage ledger. An interrupted admitted request may
already have reached the provider, so it remains accounted for with unknown
outcome/cost where appropriate; it is not treated as a free cancelled call.
Increasing concurrency does not increase the configured budget or repair limits.
Workers measure the complete request, including native reasoning history, before
sending accounting messages. Only its size measurements travel with the public
request fields; the additional native history stays in the worker conversation.
The coordinator still applies the shared size, token and spending limits.
Parallel mode remains opt-in pending paired live quality and performance
validation; common prompts and offline checks alone do not establish equal
translation quality.

## Data Residency & Model Boundary

`sas2r` enforces a declared, bounded boundary between local data and remote language models:

- **Default Model Evidence**: Source and generated code, input schema metadata, helper interfaces and execution diagnostics. Reference comparison summaries, digests, values and reports do not enter code-writing requests or tools; project overrides cannot restore the comparison tool.
- **Bounded Candidate Evidence**: `agent_evidence = "bounded"` permits capped candidate-output summaries and previews in execution diagnostics. These may contain row numbers, key values, cell values and subject identifiers. `code_only` omits these previews; source code and error messages may themselves contain data. Complete reference comparisons remain in local reports and the public comparison API. Source inputs retain their input role even when also used as references.
- **Data Residency**: Dataset reading, execution and comparison run locally; confirm your configured endpoint meets your enterprise data residency obligations.

## Regulatory Review & Validation Disclaimer

> [!WARNING]
> **THIS IS NOT PARITY**: Automated translation and reviewer checks do not replace regulated validation. Review and independent output verification are required before production use.

## Coverage, limits, and reuse

The JSON report's `coverage` separates `outputs_total`, `outputs_produced`,
`outputs_reference_compared`, `outputs_passed`, and `outputs_reference_passed`.
It also reports `components_independently_reviewed` out of `components_total`.
`validated_targets` names the required targets that supplied passing reference
evidence; `unreferenced_targets` lists targets without a completed comparison.
A completed review with a material finding counts as reviewed, not as passing.
The Markdown report and printed result expose these same counts.

`usage` includes elapsed seconds, provider calls, known/billed/estimated spend,
unknown-cost calls, and the effective limits. Unknown cost is not zero spend.
Configure non-dollar ceilings using, for example,
`usage_limits = list(max_calls = 10, max_request_bytes = 100000)`.
`usage_limits = list(max_calls = 0)` prevents all provider requests.
The accepted names are documented in `?sas_translate`; misspellings fail.

For shipped adapters on ellmer 0.5.0+, `max_calls` counts each admitted request:
initial gathering, every tool-result continuation, and structured or JSON
finalization. For example, two tool batches followed by a gathered answer and
finalization use four calls within one agent invocation. This applies with one
or several workflows. ellmer 0.4.2 uses the native serial tool loop with legacy
phase-level admission, so that example uses two admissions; parallel execution
falls back to one workflow. Review ceilings configured for the older counting
unit when upgrading. With `max_tries > 1`, connector-internal HTTP retries are
not separately admitted; parallel mode requires `max_tries: 1`. Agent-level
retries remain individually admitted. Custom adapters retain their own request
boundary.

The usage ledger records request IDs and their `invocation_id` for grouping;
`llm_log.jsonl` keeps the existing gathering/finalization entries rather than
adding a runner entry or user prompt for every tool batch.

`resume = TRUE` uses `.sas2r/resume.rds` to reuse exact selected revision records
and completed reviews when source, input data, configuration, runtime, and worker
prompts still match. Changing concurrency alone does not invalidate this reuse.
It does not reconstruct program paths from report labels.
Missing or edited code files, or changed input bytes, cause regeneration.
Older runs without a checkpoint regenerate. Passing or deferred smoke results
can be reused when their complete context still matches; changed code, helpers,
input identity or callable paths require fresh checks. Full-bundle execution and
output checks use fresh attempts. The usage ledger remains cumulative across
resumed runs; elapsed time describes this invocation. A previously unavailable
review is retried within the remaining budget.

Parallel checkpoints distinguish drafts from components that have completed
immediate processing. A saved draft must finish its checks and repair before
releasing dependent work. Completed final reviews are saved individually.
Repair invocations and component revisit counts are preserved, including an
interrupted repair invocation. Checkpoint format 9 imports compatible format-8
checkpoints with their repair counts intact. Missing historical revisit counts
stay unknown and do not grant additional automatic revisits. Other incompatible
checkpoints regenerate with an explicit reason.

For a compatible checkpoint, completed reviews additionally require the same
review request context. Changed helper documentation, rulebook content or
installed dependency facts can therefore trigger a fresh review of retained
translations. Reviews saved without a request identity are refreshed with an
explicit progress reason. Attempt log locations and execution IDs are excluded
from the static reviewer packet; their observed errors and log content still
participate in review identity. Full paths remain in local execution records.

Shared worker prompts, skills, policy and runtime helper code also participate
in the checkpoint identity. Changes to them can regenerate translations, not
just reviews. Version 0.4.5 includes a shared policy change. A package version
number alone does not invalidate a checkpoint.

## Checking figure content

A valid PDF and a completed model review do not prove that every annotation is
visible. Inspect the rendered pages. For required labels or headings, configure
the existing text assertions in `_sas2r.yml`:

```yaml
outputs:
  tlfs: [outputs/summary.pdf]
  assertions:
    outputs/summary.pdf:
      required_text: [Mean, Median, "Std Dev"]
```

With `pdftools` installed, the output gate checks extracted PDF text and rejects
missing required text. This checks presence across the PDF; it does not verify
each page, layout or statistical values. Those still need review or suitable
reference evidence.

## Moving a deliverable

`result$outputs_dir` contains all generated files from the selected execution,
including WORK, named libraries, and TLFs, under their relative paths:
`work/out.rds`, `adam/adsl.rds`, and `outputs/table.html`, for example.
The selected attempt's file inventory also drives reports and export.

`sas_write(result, "delivery")` writes the selected code and runtime, all generated
files, reports, `outputs-manifest.json`, a dependency/input guide, and `run.R` (renamed if a source program already uses that name; see `run-order.json`).
It rebuilds `autoexec.R` so generated library outputs belong to the destination.
After moving the folder, run `Rscript run.R` from it, or in R use
`source("run.R", chdir = TRUE)`. Programs run in dependency order; included modules
are invoked by their parents. The original attempt is preserved.

Input libraries are external dependencies, not copied study data. Update paths
in `autoexec.R` and any explicit source LIBNAME assignments when inputs move.
The exported report describes the selected execution, not any later manual rerun.

## Semantic regression corpus

The 14 cases in `tests/testthat/fixtures/semantic-reference` cover missing
operands, stored flags, computed predicates, WHERE timing, string literals, SQL
case folding, shared MERGE variables, and unsupported multi-statement bodies.
Expected CSVs are independently written from documented SAS behavior; they have
not been SAS-executed. `tools/generate-semantic-references.sas` exports the same
cases and SAS provenance for independent verification in a SAS environment.
Offline tests check actual R data values and explicit deferral without claiming
a project-wide accuracy rate or SAS parity.
