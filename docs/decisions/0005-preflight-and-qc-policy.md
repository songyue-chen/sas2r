# Preflight planning and QC inheritance

Status: accepted.

Preflight and translation share configuration normalization, output contracts,
bound dataset identities, and producer selection. A returned project retains the
complete plan. Reusing it assumes unchanged sources; changed scan settings require
rescanning. A later same-file producer is an explicit backward dependency, and
files with identical basenames remain distinct. Scheduling still operates on
files, so include handoffs that require interleaving can need manual review.

A plain configuration list supplied with a reused project updates whole top-level
fields: omitted fields retain project settings, while explicit NULL/empty values
replace them. A source path with an explicit list, a YAML file, or a
`sas2r_config` object uses a complete configuration. This project-update rule is
distinct from the assertion inheritance policy below.

QC uses whole-field inheritance: target assertions override a named profile,
which overrides global comparison rules. Omitted/NULL fields inherit; false and
empty fields override. Keys align reference rows; only `unique_keys = TRUE`
requires nonmissing unique keys, checked after inheritance. Passing metadata or
row-count requirements does not establish SAS reference equivalence.

Configuration uses true/false YAML booleans to preserve metadata keys such as N
and Y. Review transcripts and responses remain local working material; NEWS and
user guides record supported behavior and limitations.
