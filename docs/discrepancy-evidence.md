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
comparison cells, record identifiers, key values, examples/previews, distinct values,
minimum/maximum/quantile statistics and raw runtime log text. Reference paths and
full comparison details remain available to the human in local reports.

The provider-bound projection retains identity/status, column names/types and
enumerated patterns. Row and column sizes become same, generated_more,
generated_fewer or unknown; pairing becomes complete, partial or unknown.
Variable differences become some, all or missing_only. It contains no exact
dimensions, pairing totals, mismatch counts, offset magnitudes or numeric targets.
The same projection serves focused investigation, terminal report explanation
and AI bug triage.

The projection is checked against this closed format each time it is built:
only the fields above, only the listed values for sizes, pairing, differences
and patterns, short single-line names, no number anywhere and no reference
path. A projection that fails the check is not sent. The run continues without
it, a warning names the target and the fields at fault (never a value), and
the omission is recorded as `discrepancy_summary_omitted` in the run
diagnostics or in the diagnosis record. Such a warning means a defect in
sas2r, not in your programs; please report it.

## Bug triage attribution facts

The opt-in AI bug triage (`migration.bug_reports: true`) adds a second closed
projection to its one request. It is built only when the option is on and a
request can follow, after every repair and status decision has been made. It
never feeds a repair, a candidate choice or the run status.

For each failed dataset target, up to eight differing columns are described.
The limit falls to two as more targets share the request; further column names
are listed without detail. The comparison summary of such a target is sent
without its own `variables` and `type_differences` lists, since each described
column carries the same facts.

| Field | Values | Origin and meaning |
|---|---|---|
| `name`, `checks`, `difference` | column name; check names; `type`, `values`, `missing_in_generated`, `extra_in_generated`, `attribute` | The failed checks that name the column |
| `reference_type`, `generated_type`, `required_type` | type words such as `numeric`, `date`, `character`, `integer` | From the comparison, or from a configured `types` requirement |
| `extent`, `patterns` | as in the comparison summary above | Copied from that summary for a value difference |
| `sas_source` | `assigned`, `named_not_assigned`, `not_named`, `unknown` | A scan of names in the SAS of the upstream programs. It is not a SAS parser: `unknown` is reported when macro text could build the name |
| `generated_code` | `assigned`, `passed_through`, `named_not_assigned`, `not_named`, `unknown` | A scan of the parsed selected R, never run. `passed_through` needs every upstream program parsed and none changing columns without naming them |
| `assigned_in`, `helpers` | program names; runtime helper names | Where the R assigns the column, and the helpers whose calls name it or produce its value |
| `translator_told` | input dataset name and `numeric` or `character` | The type sas2r inferred from the source and gave the translator |
| `representation_only`, `conversion` | `yes`, `no`, `not_checked`; the rule tested | Whether a type difference holds the same values. See below |

Each target also carries `helpers_called`, `external_inputs` and `decisions`:
how rows were paired (`method`, `keys_used`, `keys_from`, `keys_configured`,
`row_order`), the numeric tolerance (`package_default` or `configured`, `zero`
or `above_zero`, and the differing columns with their own tolerance), the file
types of the reference and the generated dataset, and for a differing column
any recorded date decision (stored as a date or left as a SAS day number, its
basis, the SAS format name and the stated reason). The request as a whole
carries `programs` (who wrote each program shown: rules, model or repair),
`runtime_helpers` (the package's own, or patched during the run) and
`shared_patterns` (a type difference, value pattern or failed check found in
two or more failed datasets, the targets and columns involved, and whether
they share an upstream program or input).

`representation_only` is the one fact that needs the data. The gate does not
compare a column whose types differ. For triage, sas2r reads the generated
dataset and the reference again on your machine, converts the side SAS would
store as a number (a date as days from 1960-01-01, a datetime or time as
seconds, true/false as 1/0) and compares the two with the gate's row pairing
and tolerance. A failed `types` requirement between `integer` and `double` is
answered from the generated values alone. Only `yes`, `no` or `not_checked`
and the name of the rule are sent. Trailing blanks are already treated as
equal by the gate and never fail a check, so they need no flag.

This projection contains column, dataset, program, helper and check names,
type words and the fixed values above. It contains no number, no cell or key
value, no count and no path. The names of key columns are sent; key values are
not. A column name that is not a plain SAS or R name is left out. A plain name
is sent as it is, including one that a transpose built from a data value, as
it already is in the comparison summary and in the generated code. Every block is checked against this
format before the request is built. A block that fails is not sent, a warning
names the target and the fields at fault, and the omission is recorded as
`package_bug_facts_omitted` in the diagnosis record.

The request puts these facts first, then the documentation of up to four
runtime helpers, then the SAS and selected R of the programs involved. It is
limited to 24,000 characters. Code keeps a reserved share of at most 8,000
characters, and the diagnosis record names each section that was cut
(`context_truncated_sections`).

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
