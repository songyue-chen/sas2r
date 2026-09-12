# Semantic reference corpus

The checked-in expected CSV files are independently written, documentation-derived expectations. They have **not** been generated or verified by a SAS execution. Tests do not derive these values from sas2r's translators or runtime.

Each case includes the complete SAS setup, SAS program, input CSV files, expected output CSV, and an explicit supported/deferred classification in manifest.json. Duplicate-key MERGE with shared variables is deferred when runtime cardinality becomes known. Other unsupported shapes defer before execution.

Primary expectation sources:

- SAS expression operators (missing values compare below nonmissing numbers; results are numeric 0/1): https://support.sas.com/documentation/cdl/en/lrcon/65287/HTML/default/p00iah2thp63bmn1lt20esag14lh.htm
- WHERE selects input observations before executable DATA-step statements: https://support.sas.com/documentation/cdl/en/lestmtsref/63323/HTML/default/n1xbr9r0s9veq0n137iftzxq4g7e.htm
- One-to-many MERGE with shared variables: https://support.sas.com/kb/48/705.html

To collect independently executed SAS references, create `sas-generated/` here and run `tools/generate-semantic-references.sas` in SAS from the repository root. It exports each output and records SAS version/platform/time. Then run the semantic-reference test with that folder present: it compares SAS-generated outputs against the checked-in expectations as well as checking the R translation. Preserve the SAS log and provenance when promoting generated references into a release benchmark. This repository's offline suite does not claim SAS parity or model accuracy.
