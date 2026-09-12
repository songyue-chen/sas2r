# Migration Evidence & Verification Guide

`sas2r` provides a deterministic, evidence-grounded migration framework for translating SAS clinical programming assets into validated R bundles. This document describes the component evidence ladder, bundle verification statuses, reviewer verdicts, attempt contracts, and data residency boundary.

## Component Evidence Ladder

Every component in a SAS project traverses an evidence hierarchy based on accumulated static, mechanical, and execution evidence:

1. **`transpiled_only`**: The component has been mechanically parsed or translated into R code and behavioral contracts, but has not yet undergone independent static review or isolated smoke testing.
2. **`reviewed_only`**: An independent reviewer agent has evaluated the candidate R code and contract against the source SAS, with no blocking semantic or runnability defects identified.
3. **`runtime_verified`**: The component executed as part of an isolated subprocess run or smoke verification step with its upstream dependency chain.
4. **`output_verified`**: The component and its generated candidate outputs were evaluated and satisfied at the migration gate.
5. **`reference_validated`**: The component outputs have been compared against authentic SAS reference datasets within specified numerical and structural tolerances.

Evidence is strictly immutable per revision binding: any source, code, or helper modification resets evidence for the new revision.

## Authoritative Bundle Statuses

When `sas_translate()` processes a project or SAS file, the final bundle is assigned one of four canonical states:

- **`blocked`**: Hard syntax/lint errors remain, unresolvable circular dependencies exist, or execution failed without a working candidate bundle.
- **`needs_review`**: The bundle is statically valid and ready for inspectability, but execution was disabled (`execute = FALSE`), reviewer findings require human sign-off, or non-blocking warnings remain.
- **`migration_ready`**: All required candidate datasets and TLF outputs were successfully produced and structurally validated across copy-on-write attempt directories.
- **`validated`**: Required output checks and lineage requirements pass, and at least one required target has a passing reference comparison. Other targets can remain unreferenced. This status does not mean every output was compared.

## Reviewer Outcomes & Authority

Independent LLM review is designed to assist human engineers, not to grant unearned certification:

- **`reviewed_no_material_finding`**: Reviewer confirms the R code faithfully implements the SAS logic and complies with target conventions.
- **`repair_required`**: Reviewer identified material issues (e.g. inverted logic, missing condition, incorrect library mapping) that trigger immediate component repair.
- **`review_unavailable`**: When no LLM is configured or budget limits are reached, review is recorded as unavailable. This is treated honestly as an observability notice and does not block smoke verification or claim unverified equivalence.

## Dual Repair Loops & Attempt Isolation

`sas2r` uses two separate, targeted repair loops configured by `max_program_repair_rounds` and `max_bundle_repair_rounds`:

1. **Component-Level Immediate Repair**: Fast feedback loop (`max_program_repair_rounds = 1L`) fixing syntax, lint errors, and initial reviewer findings on individual components before full execution.
2. **Bundle-Level Causal Repair**: Multi-component repair loop (`max_bundle_repair_rounds = 2L`) diagnosing execution and output failures across the whole dependency graph.

Under the default `agent_evidence = "code_only"` policy, agents receive source code, AST context, and execution summaries without raw patient data.

### Copy-on-Write Attempt Isolation

Each attempt runs in an isolated directory, grouped per run (`<run_id>/bundle_attempt_NNN/`, where run folders sit directly in the output directory and their timestamp-first ids sort chronologically) so repeated runs into the same output directory never collide. A run's folder puts the deliverable first and keeps its evidence beneath: the selected translated programs are materialized at its top next to `report.md` and `report.json`, while the attempts and the per-component program revisions and reviews (`<run_id>/programs/`) sit below (the state-level `.sas2r/report.json` additionally tracks the latest run for `resume`):
- Source inputs are never mutated (protected by copy-on-write library registries).
- Attempt outputs, logs, and `record.json` are captured atomically.
- Deterministic selection ensures newer attempts are selected only if they improve upon or maintain previous pass criteria without regressions.

## Data Residency & Model Boundary

`sas2r` enforces a declared, bounded boundary between local data and remote language models:

- **Default Payload**: SAS source code, macro structures, aggregated column metadata, and redacted comparison digests (variable names, mismatch counts, difference magnitudes, pattern hints — never the mismatching cells) are what reach the model. Bundle-repair evidence carries the digest in place of the raw mismatch table.
- **Bounded Opt-In Surfaces**: The reviewer's bounded comparison report may quote a small, capped set of example differences — including row numbers, key values, and differing cell values (subject identifiers among them when key columns identify subjects) — and `agent_evidence = "bounded"` adds capped output metadata with short previews to repair evidence. The default `agent_evidence = "code_only"` sends neither.
- **Data Residency**: Clinical datasets and outputs stay in the local R process on your infrastructure; confirm your configured endpoint meets your enterprise data residency obligations.

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

`resume = TRUE` uses `.sas2r/resume.rds` to reuse exact selected revision records
and completed reviews when source, input data, configuration, runtime, and worker
prompts still match. It does not reconstruct program paths from report labels.
Missing or edited code files, or changed input bytes, cause regeneration.
Older runs without a checkpoint regenerate. Smoke and full output checks always
rerun in fresh attempts. The usage ledger remains cumulative across resumed runs;
elapsed time describes this invocation. A previously unavailable review is retried.

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
