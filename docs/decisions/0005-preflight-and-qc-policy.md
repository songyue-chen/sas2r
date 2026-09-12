# Preflight planning and QC inheritance

Status: accepted.

## Context

Preflight must describe the same inputs, output requirements, and dependency
plan that translation uses, without provider calls, execution, or writes. A
saved project is a public handoff, including when the working directory changes.
Clinical QC requirements need explicit inheritance and observable evidence.

## Decisions

Source paths and configured paths are absolute. Scan-setting comparisons
normalize both configurations and disregard library mapping order. A plain list
on a reused project replaces supplied top-level fields; omitted fields inherit,
and explicit NULL clears a field. A source path with an explicit list, a YAML
file, or a `sas2r_config` object uses a complete configuration. Changed bindings
require a rescan. Sources must remain unchanged when a scanned project is reused.

QC uses whole-field inheritance: target assertions override a named profile,
which overrides global rules. Omitted/NULL fields inherit; false and empty fields
override. Variable-specific tolerances remain a separate field from default
tolerances. Keys align rows; only `unique_keys = TRUE` requires nonmissing unique
keys, checked after inheritance. Types describe physical R columns, including
factors. Metadata checks alone do not establish SAS equivalence.

Reference keys use SAS dataset naming: case-insensitive, with WORK for an
unqualified name. Explicit output references take precedence over per-target
comparison references, which precede the global fallback. The selected reference
is stored in the contract. Duplicate keys and unknown comparison targets fail.
Configuration uses a single YAML document and true/false booleans to preserve
clinical metadata keys such as N and Y.

## Alternatives

- Recursive configuration merging makes explicit resets ambiguous. Whole-field
  updates retain omitted settings without inventing a second merge policy.
- Comparing raw path spellings falsely rejects equivalent bindings. Normalizing
  both sides accepts filesystem aliases without accepting actual setting changes.
- Basenames alone lose colliding programs. A suffix derived from the existing
  root-relative staging identity preserves distinct components without making
  every component identifier a path or changing noncolliding names.
- An existing file cannot justify a read before its producer in the same source.
  This remains a backward-dependency finding. Unknown library paths retain
  name-based ordering, but cannot count as available inputs.
- Expanding arbitrary macro/control-flow forms inside preflight would create a
  second SAS interpreter. Unsupported data flow is conservatively deferred.
- Converting all YAML boolean-looking words would corrupt N/Y metadata keys.
  True/false is the configuration vocabulary; incompatible writer output must
  be corrected before loading.

## Consequences

File-level scheduling can still require review for interleaved include handoffs.
Included files retain their staged paths and run at include sites in bundles.
Changed QC/model semantics invalidate saved revisions; transport limits alone
retain them, while execution and output checks rerun. Cache/checkpoint schema
changes regenerate old products. Actual SAS reference execution remains a
separate validation milestone. Review transcripts and responses stay local;
this tracked record and public guides describe supported policy.
