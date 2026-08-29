---
name: sas-missing-sort-semantics
description: Apply and review SAS missing-value and sort-order behavior when translating SORT, BY-group, merge, or order-dependent code to R.
metadata:
  sas2r:
    version: 1
    agents: [translator, reviewer, fixer]
    priority: 100
    triggers:
      semantic_rules: [procs.sort]
      procs: [sort]
      flags: [by_group, order_dependent]
      comparison_reasons: [missing_order_difference]
    tools: [lookup_rulebook, query_project_graph]
---

# SAS Missing-Value and Sort-Order Semantics

## Core Principles

When translating SAS code to R or reviewing translated output, the agent must account for fundamental differences in missing value representation and sorting order between SAS and R.

### 1. Numeric Missing Value Hierarchy

In SAS, numeric missing values form an ordered hierarchy that sorts **before** all regular nonmissing numbers in ascending order:

$$\text{._} < \text{.} < \text{.A} < \text{.B} < \dots < \text{.Z} < \text{all nonmissing numbers}$$

- Special missing values: `._` is the lowest possible numeric value in SAS.
- Standard missing value: `.` (period) is greater than `._` and less than `.A`.
- Letter missing values: `.A` through `.Z` sort in alphabetical order after `.` and before any finite number (including negative numbers like `-1e300`).
- Nonmissing numbers: All finite real numbers (negative, zero, positive) sort strictly after all missing representations in ascending order.

### 2. Character Missing Values

In SAS, character missing values are represented by blank strings (`""` or fixed-width spaces):

$$\text{"" (blank/whitespace)} < \text{all nonblank character values}$$

- In an ascending sort, blank character values sort before all nonblank character values.

### 3. Descending Sort Reversal

When a descending sort is specified in SAS (e.g., `PROC SORT; BY DESCENDING var;`):

- Numeric: Regular nonmissing numbers appear first (in descending numerical order), followed by `.Z` down to `.A`, then `.`, and finally `._`.
- Character: Nonblank character strings appear first (in descending alphabetical/collational order), followed by blank strings at the very end.

### 4. R Default Differences and Pitfalls

- R represents missing numeric and character data as `NA` (or `NA_real_`, `NA_character_`, `NaN`).
- R's `order()`, `sort()`, and `dplyr::arrange()` place `NA` values **last** by default in ascending order (`na.last = TRUE`).
- In contrast, SAS places missing values **first** in ascending order.
- Direct translation to `order(x)` or `arrange(x)` without explicit `NA` handling causes missing values to appear at the bottom rather than the top, producing row-order mismatches in downstream operations.
- Special missing values (`._`, `.A`-`.Z`) collapse to `NA_real_` in R unless explicitly mapped to domain-specific indicators or separate columns.

### 5. Collation and Session Locale Uncertainty

- SAS sorting order for character variables depends on the active collation sequence (e.g., standard ASCII, EBCDIC, or locale-specific ICU collation via `SORTSEQ` option).
- R string sorting depends on the operating system locale and C library collation (`stringi`, `stringr`, or base `Sys.getlocale("LC_COLLATE")`).
- When diagnosing sort differences, account for potential collation sequence variations across uppercase, lowercase, punctuation, and non-ASCII characters.

## Review Checkpoints

When translating or reviewing units involving sorting and order dependencies:

1. **PROC SORT Translation**:
   - Verify that `BY` variables respect ascending vs. `DESCENDING` flags.
   - Ensure missing values are explicitly placed at the beginning for ascending sorts and at the end for descending sorts to match SAS behavior.
   - For `NODUPKEY` and `DUPOUT=`, verify that deduplication retains the first record per BY-group according to the established order.

2. **BY-Group Processing**:
   - In SAS DATA steps, `BY` statements create automatic `FIRST.variable` and `LAST.variable` flags based on sorted groups.
   - Confirm that the R equivalent (e.g., grouped operations with `dplyr::group_by()` or custom indexing) reproduces the exact group boundaries and preserves the underlying sort order.

3. **Merge Operations**:
   - SAS DATA-step `MERGE` requires datasets to be pre-sorted by the `BY` variables.
   - Verify that R joins (`dplyr::full_join`, `merge`) account for missing key alignment and preserve expected group multiplicities.

4. **Order-Dependent Operations**:
   - Functions like `LAG()`, `DIF()`, cumulative sums, and `RETAIN` statements strictly depend on row ordering.
   - Verify that the input data frame has been aligned to the exact SAS sort order prior to evaluating order-dependent expressions.
