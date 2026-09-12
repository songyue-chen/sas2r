# Output Evidence & Comparison Guide

`sas2r` provides a suite of standalone, SAS-free comparison and alignment functions to evaluate candidate R dataset outputs against reference SAS datasets and verify structural and numerical equivalence.

---

## 1. Supported Use Cases & Observable Outputs

### Use Case 1: Source Only (No LLM, No Data)
- **Inputs**: SAS source files (`.sas`). No data files, no configuration, no LLM required.
- **Internal Work**: Scanner parses source, isolates translation units (DATA steps, PROCs, macros), extracts lineage, and applies deterministic transpiler rules. Generates baseline R scripts and helper runtimes.
- **Observable Outputs**: Staged R scripts, standalone runtime files (`autoexec.R`, `sas2r-helpers.R`, `_sas2r_formats.R`), and execution logs.

### Use Case 2: Source Plus LLM (No Data)
- **Inputs**: SAS source files plus an active LLM configuration (`_sas2r.yml`). No customer data required.
- **Internal Work**: Translator agent generates candidate R code under strict tool budgets, while the reviewer agent audits candidate code against semantic rules and domain guidelines.
- **Observable Outputs**: Repaired R code units, audit summaries, and usage ledger logs (`<out_dir>/.sas2r/usage.jsonl`).

### Use Case 3: Saved Final Outputs
- **Inputs**: SAS source, configured library directories containing authentic SAS outputs (`.sas7bdat` / `.xpt`), and candidate R output datasets (`.rds` / `.xpt`).
- **Internal Work**: The output-review inventory discovers dataset targets from AST lineage and pairs accessible files. Standalone comparison functions instead take data frames directly. Migration reference checks use the configured `outputs.references` paths and optional assertions for keys and tolerances.
- **Observable Outputs**: Structured target plans and bounded comparison reports under `<out_dir>/.sas2r/output-review/<run_id>/`. The separate migration summary is saved in the run folder as `report.json` and `report.md`; `.sas2r/report.json` also tracks the latest run.

### Use Case 4: TLF (Table, Listing, Figure) Preparation Data
- **Inputs**: SAS programs producing intermediate datasets that feed report procedures (`PROC REPORT`, `PROC TABULATE`, `PROC PRINT`, `PROC SGPLOT`, `PROC SGPANEL`, `PROC SGRENDER`).
- **Internal Work**: AST lineage identifies intermediate preparation datasets consumed by reporting PROCs. This dataset-review path compares structured preparation data; rendered artifact checks belong to the separate migration output gate.
- **Observable Outputs**: Comparison reports and target plans designating role `tlf_preparation_data`.

### Use Case 5: Equal Content with SAS-Missing-First vs. R-NA-Last Ordering
- **Inputs**: Saved datasets with identical row sets but differing sort orders due to SAS placing missing values first while R sorts `NA` last.
- **Internal Work**: Row alignment separates content matching from sort order analysis. Applies explicit SAS sort collation semantics to verify content equivalence independently of row sequence.
- **Observable Outputs**: `order_equivalent = FALSE` can accompany equal content. Without a source ordering contract the reason is `incidental_reorder`; when order is meaningful, inspect the order diagnostics as well as content differences.

### Use Case 6: Duplicate Keys Reordered Within a Group
- **Inputs**: Datasets with non-unique key values where rows within a key group appear in different relative positions.
- **Internal Work**: Executes exact multiset matching on normalized rows, followed by bounded minimum-cost residual assignment via Hungarian matching (`clue::solve_LSAP`) up to safety limits.
- **Observable Outputs**: Preserves exact multiset row counts and reports ambiguous tie groups without exponential combinatorial search.

### Use Case 7: Evidence Missing, Ambiguous, or Resource-Limited
- **Inputs**: Partial output folders, missing candidate files, or datasets exceeding fixed evidence limits.
- **Internal Work**: Fails closed safely per target. A target with no candidate output stays `missing_candidate` and one with no reference stays `missing_reference`; unresolvable ambiguous candidates stay `ambiguous`.
- **Observable Outputs**: Translation results remain intact; output review reports explicit diagnostic statuses without halting or corrupting code generation.

---

## 2. Comparison APIs & Functions

The two comparison APIs share cell-comparison rules but differ in row alignment
and result type:

| Function | Contract |
| --- | --- |
| `compare_profile(abs = 1e-8, rel = 1e-8)` | Constructs numeric tolerance, missing-value, padding, and attribute policy. It does not take datasets or calculate a statistical profile. |
| `compare_datasets(base, comp, profile, keys)` | Compares reference `base` with candidate `comp`. Without keys it pairs by row order; duplicate keys pair by occurrence. Returns a `sas2r_comparison`. |
| `passed(comparison)` | Returns the pass/fail result of a `sas2r_comparison`. |
| `compare_aligned_outputs(reference, candidate, target, context = NULL, profile = compare_profile())` | Infers and checks candidate row keys, then uses duplicate-key or keyless multiset alignment when needed. Returns a bounded `sas2r_comparison_report`. It does not require pre-aligned input. |
| `analyze_output_order(reference, candidate, pairs, context = NULL)` | Evaluates row order using a pairing table with reference_row and candidate_row columns and optional source ordering context. |
| `diff_digest(comparison, label = "dataset")` | Builds a redacted summary from a `sas2r_comparison`, including names, counts, and magnitudes. |
| `as_digest_json(digest)` | Serializes that digest to JSON. |
| `write_comparison_report(x, file = ...)` | Writes Markdown for a `sas2r_comparison`, or JSON for a `sas2r_comparison_report`. |
| `read_comparison_report(report_id, registry)` | Retrieves a registered report from a named list or environment. It does not read a file path. |

### Compare datasets with known unique keys

This example runs entirely offline:

<!-- sas2r-example: offline keyed-comparison -->
```r
library(sas2r)

reference <- data.frame(USUBJID = c("01", "02"), AVAL = c(10, 20))
candidate <- reference[c(2, 1), ]
comparison <- compare_datasets(
  base = reference,
  comp = candidate,
  keys = "USUBJID",
  profile = compare_profile(abs = 1e-8, rel = 1e-8)
)
passed(comparison)
write_comparison_report(comparison, file = "dataset-comparison.md")
stopifnot(passed(comparison), file.exists("dataset-comparison.md"))
```

For a saved migration, substitute
`haven::read_xpt("data/reference/adsl.xpt")` and
`readRDS(file.path(result$outputs_dir, "adam", "adsl.rds"))`.
Named-library output files retain their library subdirectory. This reads the
saved outputs and writes a separate comparison report; it does not rerun the
bundle or change its recorded status.

### Compare repeated records or infer row keys

<!-- sas2r-example: offline aligned-comparison -->
```r
reference <- data.frame(USUBJID = c("01", "01", "02"), AVAL = c(10, 11, 20))
candidate <- reference[c(2, 3, 1), ]
target <- list(
  target_id = "adam.example",
  logical_dataset = "adam.example",
  role = "output",
  contributing_unit_ids = integer()
)
report <- compare_aligned_outputs(reference, candidate, target = target)
print(report)
report$structure
report$alignment
report$mismatches$total_mismatch_cells
report$resource_state
report$truncated_fields
write_comparison_report(report, file = "aligned-comparison.json")
stopifnot(report$mismatches$total_mismatch_cells == 0L,
          file.exists("aligned-comparison.json"))
```

`passed()` does not accept this report class. Inspect missing/extra columns,
column types, unmatched rows, mismatching cells, meaningful ordering, and
comparison completeness together. Zero reported cell differences alone do not
establish a match. Ambiguous pairing, unsupported types, or resource limits leave
evidence unresolved. `truncated_fields` distinguishes capped examples from
omitted structural or other evidence. Use `jsonlite::read_json()` to inspect a
saved JSON file as a list; `read_comparison_report()` is for registered objects.

### Establish that the references are comparable

Use SAS references from the same source programs, input snapshot, formats,
macros, and parameters. An `.xpt` filename alone does not establish provenance.
Confirm population and output definitions, then choose row keys that mean the
same thing on both sides. Different analysis visit numbering, for example, can
make identically named key columns unsuitable for matching.

Start a discrepancy investigation with missing/extra variables and row counts,
then inspect value differences and the responsible SAS derivations. If a source
KEEP statement requests 18 variables but the reference has 55, determine which
specification is intended before asking a fixer to add the missing variables.
Do not drop those columns merely to obtain a passing comparison. An independent
R reconstruction can help attribute differences, but it is not a SAS execution.

---

## 3. Source Authority & Library Discovery

1. **Point-of-Use Source Authority**: An accessible `LIBNAME` statement in SAS source code is authoritative for its scope.
2. **Configured Fallback**: `libraries:` definitions in `_sas2r.yml` supply bindings only when a source libref is absent or unresolvable.
3. **No Member Fall-Through**: A missing dataset member under an accessible source root does not fall through to a configured fallback directory.
4. **Inventory Roots**: Configured library paths serve as search roots for automatic output review. The migration gate separately accepts explicit reference paths and per-target key/tolerance assertions in `_sas2r.yml`.

---

## 4. Data & Model Privacy Boundary

`sas2r` enforces strict data containment to protect proprietary and clinical trial data:

- **Complete Datasets Stay Local**: Complete saved dataset objects remain entirely within the local R process.
- **Default Model Evidence**: With `agent_evidence = "code_only"`, agents receive source/code context, metadata, and redacted difference digests: names, counts, and magnitudes, without mismatching cells.
- **Bounded Opt-In Surfaces**: The reviewer's bounded comparison report may contain capped examples with row numbers, subject identifiers, key values, and differing cell values. Setting `agent_evidence = "bounded"` adds output summaries with short previews to repair evidence. The default `code_only` policy sends neither. Reports are serialized and capped.
- **Data Residency Compliance**: Organizations must ensure configured model endpoints comply with their enterprise data residency and privacy obligations.

---

## 5. Audit Mode & Metering

Nothing has to be taken on trust: the privacy boundary is inspectable after the fact:

- **Audit Mode**: The standalone comparison functions above read data and create reports without an LLM or model calls. In `sas_translate()`, `llm = NULL` can still use the provider from configuration; use `usage_limits = list(max_calls = 0)` to prevent provider requests.
- **Caps & Truncation Flag**: Every diagnostic field carries a fixed cap. Whenever a cap omits detail, `truncated = TRUE` is set in the serialized report.
- **Metadata-Only Usage Ledger**: Model interactions append request records to `<out_dir>/.sas2r/usage.jsonl` recording token counts and spend without saving prompt bodies.
- **Unknown Cost**: Reported known spend excludes requests without usable pricing information. Check unknown-cost calls and effective limits alongside the dollar total.

---

## 6. Regulatory Review & Validation Disclaimer

> [!WARNING]
> **THIS IS NOT PARITY**: Output comparison reports evaluate equivalence against supplied reference files under specified tolerances. Successful comparison does not replace required clinical programming double-programming or regulatory validation procedures.

For required metadata and structure checks in a migration, see
[clinical QC profiles and preflight](clinical-qc-preflight.md).
