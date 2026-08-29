# sas2r 0.2.0

### Dependency-Aware Migration Architecture
* **Single Authoritative Entry Point**: `sas_translate()` orchestrates end-to-end migration of single SAS programs or complete multi-file pipelines into modular, reproducible R bundles.
* **Dependency Graph & Topological Scheduling**: Automatically parses `%include` trees, macro calls, and dataset lineages to construct an authoritative Directed Acyclic Graph (DAG) and deterministic topological execution schedule.
* **Copy-on-Write Attempt Isolation**: All execution attempts execute inside isolated candidate directories (`attempts/bundle_attempt_NNN/`) with copy-on-write library containment, ensuring input datasets are never mutated.
* **Dual Repair Loops**:
  - *Immediate Component Repair*: Fast-feedback loop fixing syntax, linting, and initial reviewer findings on individual translation units.
  - *Bundle Causal Repair*: Multi-round causal repair loop diagnosing execution and output discrepancies across the whole dependency graph.
* **Four Authoritative Bundle Statuses**: Standardized gate statuses (`blocked`, `needs_review`, `migration_ready`, `validated`) indicating artifact completeness and validation depth.
* **Component Evidence Ladder**: Immutable evidence tracking per revision binding (`transpiled_only`, `reviewed_only`, `smoke_verified`, `runtime_verified`).
* **Output Verification Gate**: Evaluates candidate datasets and Table/Listing/Figure (TLF) outputs against formal structural contracts and optional SAS reference datasets.
* **Deterministic Selection & Reporting**: Selects non-regressive attempts and exports machine-readable (`.sas2r/report.json`) and Markdown (`report.md`) audit documentation.
* **Model Privacy & Data Residency**: By default only SAS source code, macro structures, column name metadata, and redacted comparison digests reach remote models — never the mismatching cells. Bounded, capped opt-in surfaces (the reviewer's comparison report examples and `agent_evidence = "bounded"` previews) may carry row numbers, key values, differing cell values, and subject identifiers; the default `code_only` policy sends neither. Users remain responsible for confirming their configured endpoint meets enterprise data residency obligations.
* **Removed Legacy Approvals & Synthetic Ladder**: Replaced legacy approval ledger commands and single-unit synthetic ladders with the dependency-aware migration pipeline and output verification gate.

# sas2r 0.1.0

* Worker prompts now receive retained SAS comments only as fallback evidence.
  Comments between translation units attach forward to the next unit; executable
  SAS takes precedence, and source approval identity remains code-only.

* Generated macro functions now deterministically validate the current macro's
  public interface before emission.

* Ellmer HTTP deadlines and retry counts now have stable sas2r defaults. The
  usage ledger records the absolute-attempt policy, request duration, and safe
  terminal failure class/reason without copying provider error text.

* Ellmer-backed agents now always gather with registered tools before a
  tool-free structured finalization request. A generic downgrade-only
  transport constraint prevents model or project capability overrides from
  bypassing this public ellmer limitation; provider cache-token accounting and
  stateful tool wrappers are unchanged.

Initial release of `sas2r`, providing transparent, verifiable SAS-to-R translation for regulated analytics environments with integrated dataset comparison capabilities.
