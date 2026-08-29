---
name: sas-dataset-row-alignment
description: Align SAS and R output rows by source-derived identity, duplicate-group multisets, and explicit order semantics before diagnosing differences.
metadata:
  sas2r:
    version: 1
    agents: [translator, reviewer, fixer]
    priority: 90
    triggers:
      procs: [sort, report, tabulate, print, sgplot, sgpanel, sgrender]
      flags: [duplicate_keys, keyless_output, order_dependent]
      comparison_reasons: [duplicate_key_order, keyless_reorder, missing_order_difference]
    tools: [lookup_rulebook, query_project_graph]
---

# SAS Dataset Row Alignment Semantics

## Core Principles

When evaluating differential evidence or comparing SAS and R output datasets, row alignment must be established through principled identity rules rather than naive positional comparison.

### 1. Physical Position Is Not Row Identity

- Physical row position (i.e. row number `1, 2, ..., N`) does not constitute inherent row identity.
- Comparing row `i` of SAS output to row `i` of R output is only sound when an explicit, deterministic ordering contract is established by the source code.
- Without an explicit order contract, positional differences are ambiguous and may conflate harmless permutation with actual data divergence.

### 2. Unique Keys Take Precedence

- If a dataset possesses a declared or inferred unique primary key (or set of candidate keys), row identity must be established exclusively by matching on those key values.
- Key-based alignment maps corresponding SAS and R rows deterministically regardless of their physical placement in the respective tables.

### 3. Duplicate Groups as Multisets

- When candidate keys contain duplicates (i.e. multiple rows share the same key combination), rows within each duplicate key group must be treated as a multiset (bag).
- Identical rows within the key group are matched first.
- Only remaining unmatched rows (residuals) within the key group are reported as content differences.
- Within-group row order differences are diagnosed as order-specific variations (`duplicate_key_order`) rather than missing or extra rows.

### 4. Keyless Datasets Use Full-Row Multisets

- In keyless tables (where no subset of columns forms a valid key), the entire row tuple serves as the multiset element.
- The alignment compares full-row multiset counts between SAS and R.
- If multiset counts match but row sequences differ, the difference is classified as keyless reordering (`keyless_reorder`) rather than data corruption.

### 5. Positional Matching Requirements

- Positional 1-to-1 row alignment is permitted only when:
  1. The source program defines a strict, deterministic sort order encompassing all rows (e.g. `PROC SORT` with tie-breaking keys); OR
  2. The unit is keyless and explicitly order-dependent by specification.
- If neither condition holds, positional comparison is invalid and must not be used to assert translation failure.

### 6. Separation of Content and Order Diagnostics

- Data content correctness and row sequence correctness are orthogonal properties:
  - **Content Equivalence**: The SAS and R datasets contain the exact same multiset of rows and column values.
  - **Order Equivalence**: The rows appear in the exact same sequence.
- Diagnostic reporting must keep content findings and order findings separate to enable targeted fixes (e.g. adjusting `arrange()` vs. fixing data step logic).
