# Semantic reference corpus

The checked-in expected CSV files are independently written, documentation-derived expectations. They have **not** been generated or verified by a SAS execution. Tests do not derive these values from sas2r's translators or runtime.

Each case includes the complete SAS setup, SAS program, input CSV files, expected output CSV, and an explicit supported/deferred classification in manifest.json. Duplicate-key MERGE with shared variables is deferred when runtime cardinality becomes known. Other unsupported shapes defer before execution.

Primary expectation sources:

- SAS expression operators (missing values compare below nonmissing numbers; results are numeric 0/1): https://support.sas.com/documentation/cdl/en/lrcon/65287/HTML/default/p00iah2thp63bmn1lt20esag14lh.htm
- WHERE selects input observations before executable DATA-step statements: https://support.sas.com/documentation/cdl/en/lestmtsref/63323/HTML/default/n1xbr9r0s9veq0n137iftzxq4g7e.htm
- One-to-many MERGE with shared variables: https://support.sas.com/kb/48/705.html

Collection remains **pending: no SAS executable is available**. The 14 offline
fixture checks are not a substitute for this step.

When a SAS environment becomes available, run from the repository root:

```sh
Rscript tools/run-semantic-references.R --sas=/path/to/sas
```

The command creates a fresh `sas-generated/` collection, saves `sas.log` and SAS
version/platform/time in `provenance.txt`, and checks all 14 outputs against the
independent expectations. It requires a new empty directory to prevent stale
outputs from an interrupted run being reused; choose a different `--output`
path for each later collection. The SAS driver stops on an error and clears the
previous case's WORK.OUT before executing the next case. It never reads the
expected CSV files.

For references collected on another machine (copy the complete directory):

```sh
Rscript tools/run-semantic-references.R --verify-only --output=/path/to/collection
```

Exit status is 0 for complete matching evidence, 1 for differences or SAS log
errors, and 2 for missing/partial evidence. The full SAS log and provenance are
required. A passing comparison alone does not establish that a supplied file was
produced by SAS: review the log and source/input provenance. Preserve those
artifacts with the benchmark before promoting it into release evidence.

The optional fixture test can still compare files in `sas-generated/`. The
explicit collection verifier is the completeness gate; ordinary offline tests
remain useful without SAS. Synthetic files used to test that verifier are only
unit-test inputs and are never stored as SAS reference evidence.
