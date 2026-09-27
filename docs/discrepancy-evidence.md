# Discrepancy evidence contract

`dataset_discrepancy_summary()` builds the canonical local summary from results
already computed by the local comparator. It never opens a dataset, log or detail
file. `reviewer_discrepancy_summary()` projects categorical differences without counts or measured offset amounts for focused
investigation and report explanation requests. Fixers and candidate acceptance
reviews receive neither projection. This is an explicit field projection, not a
redaction pass over arbitrary comparison JSON.

The following fields are human-report fields, not the provider-bound projection.

| Field | Type | Origin and meaning |
|---|---|---|
| `schema_version` | string | Summary contract version |
| `target`, `kind`, `status` | strings | Existing output assessment identity and status |
| `reference_present` | boolean | Existing assessment; presence does not establish reference provenance |
| `generated`, `reference` | objects with numeric `rows`, `columns` | Comparator dimensions; generated dimensions can also come from local candidate assessment |
| `rows_aligned`, `columns_common`, `value_mismatches` | numbers or unavailable | Paired-row, shared-name and unequal-cell counts from the comparator |
| `variables` | array | Only variables with mismatches; each has `name`, `kind`, numeric `compared`, `mismatches`, `missing_differences` and enumerated `patterns` |
| `type_differences` | array | Column name, generated type and reference type from structural comparison |
| `alignment` | string | Comparator's alignment state, or `unknown` |

Unavailable numeric measurements serialize as JSON null. Permitted variable
patterns are `CONSTANT_OFFSET`, `NA_PATTERN_DIFF`, `CASE_ONLY_DIFF` and
`PADDING_ONLY`; the current variable list retains mismatches only. A human-only
`absolute_offset` may describe a constant difference among mismatching numeric
values. It does not establish direction, all-row coverage, a date epoch or a cause.
A Date/numeric type difference is separate from a measured numeric difference.
No conversion is automatically applied to make values agree.

Explicitly excluded: full check messages, reference paths and file handles,
comparison cells, record identifiers, keys, examples/previews, distinct values,
minimum/maximum/quantile statistics and raw runtime log text. Reference paths and
full comparison details remain available to the human in local reports.

The provider-bound projection retains identity/status, column names/types and
enumerated patterns. Row and column sizes become same, generated_more,
generated_fewer or unknown; pairing becomes complete, partial or unknown.
Variable differences become some, all or missing_only. It contains no exact
dimensions, pairing totals, mismatch counts, offset magnitudes or numeric targets.
The same projection serves focused investigation and terminal report explanation.

Execution diagnostics use a separate execution-facts-v3 projection. Recognized
missing-dataset/object/column/function and argument errors retain names already
present in supplied code. Multi-line dplyr causes are recognized; arithmetic,
condition, indexing, row-shape and type failures become fixed technical categories.
Library helper failures carry classed conditions with technical identifier fields.
No runtime values, row sizes or arbitrary messages are forwarded. Other messages
become an unclassified error. Calls must occur in the supplied code tree.
Known technical classes, component IDs and local log paths remain available.
Unknown messages and the complete diagnostics remain local for programmer review.
All legacy evidence-policy labels enforce the same no-record boundary.

Focused review may locate a source/R contradiction using approved patterns. Its
findings cite source operations, rather than reference targets. Reference
inconsistency remains advisory; it cannot authorize changing SAS logic. Human
report explanations are terminal output, never subsequent agent input, and never
change comparison results. Categorical patterns are bound into investigation cache
identity; all retrievable dependency code is bound into source context identity.
Old checkpoints are invalidated when this evidence contract changes.
