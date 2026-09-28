# Release tests

`main` contains the smaller test suite shipped to CRAN. `develop` retains the
complete suite, including the scenarios omitted here. This selection is made in
the release branch: no `skip_on_cran()` calls or `NOT_CRAN` switches select tests
in this directory.

## Retained coverage

| Feature | Representative coverage retained on main |
| --- | --- |
| Scanning, includes, macros and dependencies | Scanner, macro, include, graph, schedule and execution-order tests; called-macro translation and execution |
| Deterministic translation and runtime helpers | Transpilation, expressions, DATA steps, SQL, merge, means, frequency, source populations and helper semantics |
| Public workflows | Translation results, disabled execution, code retrieval, export, reports, resume and the dataset/TLF migration demo |
| Agent translation, review and repair | Generation, review/fix, smoke checks, immediate repair, fresh bundle reruns and runtime macro attribution |
| Source-faithful repair | The complete `test-source-faithful-repair.R`, seeded semantic defects and cross-component diagnosis; reference matches cannot authorize source regressions |
| Parallel work and incomplete inputs | Configuration and real two-worker execution with missing inputs; partial execution and readiness/status gates |
| Autoexec and library bindings | Static scope/order, CLEAR and fallback checks; direct and conditional-fallback inputs through smoke, bundle and moved exports; static macro and dynamic-name advisories |
| Outputs and evidence | Dataset comparisons, rows, cells, alignment, output contracts, TLF requirements, QC, lineage/evidence levels and reports |
| Providers and accounting | Offline provider/configuration contracts, request handling, usage ledger, privacy and budget checks |

The extended acceptance, concurrency and accumulated integration regression
matrices remain on `develop`. Main also selects fewer repeated repair, review,
macro-attribution and startup scenarios. Retained tests preserve their correctness
assertions; fixtures use synthetic data and no paid provider calls.

## Full release validation

`.github/development-test-ref` records the development commit containing the
complete tests for this release. Both installed-package CI jobs install the release
package first, restore that commit's `tests/` into the CI checkout, and run the full
suite with `NOT_CRAN=true`. Thus later changes on `develop` cannot silently change
an older release's validation. The CRAN check jobs run the smaller shipped suite.

Run this suite from the repository's `tests/` directory against an installed package
with `Rscript -e 'testthat::test_check("sas2r")'`. Check the built archive with
`NOT_CRAN=false R CMD check --as-cran sas2r_0.5.8.tar.gz`.

See [the release workflow](../docs/releasing.md) before preparing the next version.
