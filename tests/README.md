# Release tests

`main` contains the smaller test suite shipped to CRAN. `develop` retains the
complete suite, including the scenarios omitted here. This selection is made in
the release branch: no `skip_on_cran()` calls or `NOT_CRAN` switches select tests
in this directory.

## Retained coverage

| Feature | Representative coverage retained on main |
| --- | --- |
| Scanning and dependencies | Scanner, include, graph, schedule and execution-order tests; called-macro discovery, unresolved dependencies and standalone execution gates |
| Translation and runtime helpers | Transpilation, expressions, DATA steps, SQL, merge, means, frequency, source populations and helper semantics |
| Public workflows | Translation results, disabled execution, code retrieval, export, reports and resume; related assertions share small workflow fixtures |
| Source-faithful repair | The complete `test-source-faithful-repair.R`, unchanged; reference matches cannot authorize source regressions, and rejected repairs retain the previous runtime |
| Parallel work and incomplete inputs | Concurrency configuration, worker schemas, component readiness, critical-error propagation and source-based input recognition |
| Startup and formats | Ordered includes, CLEAR and configured fallback; movable generated bundles; real named-format smoke execution, conflicts, equivalent definitions and setup exclusion |
| Outputs and evidence | Dataset comparisons, rows, cells, alignment, output targets, TLF contracts, evidence levels and reports |
| Providers and agents | Offline provider/configuration contracts, schema validation and repair, tool budgets and installed worker schemas |
| CRAN review regressions | Temporary default audit logs, explicit demo destinations, file-writing side effects and documentation contracts |

The extended acceptance, concurrency, provider/accounting and accumulated
integration regression matrices remain on `develop`. This includes the separate
parallel missing-input, called-macro bundle, startup export, partial execution
and multi-program ordering workflows. Their lighter contracts remain here.
Public result, export and resume assertions share one migration; transpilation
and manifest assertions share one fixture. Fixtures use small synthetic data
and no paid provider calls. The check log prints the ten slowest test files so
remote timing regressions can be identified without another profiling upload.

## Full release validation

`.github/development-test-ref` records the development commit containing the
complete tests for this release. Both installed-package CI jobs install the release
package first, restore that commit's `tests/` into the CI checkout, and run the full
suite with `NOT_CRAN=true`. Thus later changes on `develop` cannot silently change
an older release's validation. The CRAN check jobs run the smaller shipped suite,
including on Windows R release and R-devel.

Run this suite from the repository's `tests/` directory against an installed package
with `Rscript -e 'testthat::test_check("sas2r")'`. Check the built archive with
`NOT_CRAN=false R CMD check --as-cran sas2r_0.5.8.tar.gz`.

See [the release workflow](https://github.com/songyue-chen/sas2r/blob/develop/docs/releasing.md) before preparing the next version.
