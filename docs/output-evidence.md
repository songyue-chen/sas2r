# Output Evidence & Comparison Guide

`sas2r` provides a suite of standalone, SAS-free comparison and alignment functions to evaluate candidate R dataset outputs against reference SAS datasets and verify structural and numerical equivalence.

---

## 1. Supported Use Cases & Observable Outputs

### Use Case 1: Source Only (No LLM, No Data)
- **Inputs**: SAS source files (`.sas`). No data files, no configuration, no LLM required.
- **Internal Work**: Scanner parses source, isolates translation units (DATA steps, PROCs, macros), extracts lineage, and applies deterministic transpiler rules. Generates baseline R scripts and helper runtimes.
- **Observable Outputs**: Staged R scripts, standalone runtime helpers (`sas2r-helpers.R`, `_sas2r_registry.R`), and execution logs.

### Use Case 2: Source Plus LLM (No Data)
- **Inputs**: SAS source files plus an active LLM configuration (`_sas2r.yml`). No customer data required.
- **Internal Work**: Translator agent generates candidate R code under strict tool budgets, while the reviewer agent audits candidate code against semantic rules and domain guidelines.
- **Observable Outputs**: Repaired R code units, audit summaries, and usage ledger logs (`<out_dir>/.sas2r/usage.jsonl`).

### Use Case 3: Saved Final Outputs
- **Inputs**: SAS source, configured library directories containing authentic SAS outputs (`.sas7bdat` / `.xpt`), and candidate R output datasets (`.rds` / `.xpt`).
- **Internal Work**: Discovers final dataset targets from AST lineage, and pairs candidate outputs automatically without user-configured dataset mappings or tolerances. Aligns rows, computes normalized cell differences, and builds versioned comparison reports.
- **Observable Outputs**: Normalized comparison reports written under `<out_dir>/.sas2r/report.json`, structured target plans, and candidate verification metrics.

### Use Case 4: TLF (Table, Listing, Figure) Preparation Data
- **Inputs**: SAS programs producing intermediate datasets that feed report procedures (`PROC REPORT`, `PROC TABULATE`, `PROC PRINT`, `PROC SGPLOT`, `PROC SGPANEL`, `PROC SGRENDER`).
- **Internal Work**: AST lineage identifies intermediate preparation datasets consumed by reporting PROCs. Compares structured preparation data; never attempts to parse rendered PDF, RTF, HTML, or image artifacts.
- **Observable Outputs**: Comparison reports and target plans designating role `tlf_preparation_data`.

### Use Case 5: Equal Content with SAS-Missing-First vs. R-NA-Last Ordering
- **Inputs**: Saved datasets with identical row sets but differing sort orders due to SAS placing missing values first while R sorts `NA` last.
- **Internal Work**: Row alignment separates content matching from sort order analysis. Applies explicit SAS sort collation semantics to verify content equivalence independently of row sequence.
- **Observable Outputs**: Alignment marked as content-equivalent with `order_equivalent = FALSE`, diagnosed as `order_only_difference` rather than a translation defect.

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

`sas2r` provides standalone functions for programmatic output evaluation:

- `compare_datasets(candidate, reference)`: Primary comparison between two data frames or tibbles.
- `compare_aligned_outputs(candidate, reference)`: Evaluates pre-aligned tables.
- `compare_profile(candidate, reference)`: Statistical profiling and distribution comparison.
- `analyze_output_order(candidate, reference)`: Diagnostic sorting collation analysis.
- `as_digest_json(comparison)`: Standardized JSON serialization for structural digests.
- `write_comparison_report(report, path)` & `read_comparison_report(path)`: Report persistence.
- `diff_digest(digest_a, digest_b)` & `passed(comparison)`: Structural diffing and pass/fail evaluation.

---

## 3. Source Authority & Library Discovery

1. **Point-of-Use Source Authority**: An accessible `LIBNAME` statement in SAS source code is authoritative for its scope.
2. **Configured Fallback**: `libraries:` definitions in `_sas2r.yml` supply bindings only when a source libref is absent or unresolvable.
3. **No Member Fall-Through**: A missing dataset member under an accessible source root does not fall through to a configured fallback directory.
4. **Inventory Roots**: Configured library paths serve as inventory search roots. Users provide no manual dataset mappings or tolerances.

---

## 4. Data & Model Privacy Boundary

`sas2r` enforces strict data containment to protect proprietary and clinical trial data:

- **Complete Datasets Stay Local**: Complete saved dataset objects remain entirely within the local R process.
- **Bounded Diagnostic Reports**: When agent diagnostics are enabled during migration, only bounded summaries are transmitted to remote language models.
- **Protected Elements**: Bounded reports are serialized and capped. While complete datasets never leave the local R process, diagnostic summaries may carry row numbers, subject identifiers, key values, and differing cell values necessary for defect diagnosis.
- **Data Residency Compliance**: Organizations must ensure configured model endpoints comply with their enterprise data residency and privacy obligations.

---

## 5. Audit Mode & Metering

Nothing has to be taken on trust: the privacy boundary is inspectable after the fact:

- **Audit Mode**: Running without an LLM (`llm = NULL`) evaluates outputs, creates comparison reports, and contacts no external model.
- **Caps & Truncation Flag**: Every diagnostic field carries a fixed cap. Whenever a cap omits detail, `truncated = TRUE` is set in the serialized report.
- **Metadata-Only Usage Ledger**: Model interactions append request records to `<out_dir>/.sas2r/usage.jsonl` recording token counts and spend without saving prompt bodies.

---

## 6. Regulatory Review & Validation Disclaimer

> [!WARNING]
> **THIS IS NOT PARITY**: Output comparison reports evaluate equivalence against supplied reference files under specified tolerances. Successful comparison does not replace required clinical programming double-programming or regulatory validation procedures.
