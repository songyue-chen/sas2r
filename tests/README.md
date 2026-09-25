# Test profiles

CRAN checks use `NOT_CRAN=false`. They retain unit tests and representative
public translation, review, repair, resume, export, dependency, output-validation,
and one/two-worker execution cases. They require no SAS installation, provider
credentials, paid requests, or fixture-package installation.

The independent-program concurrency fixture uses two programs on CRAN, one per
available worker, with exact request/cost accounting, real overlap, and complete
output-value assertions. Full CI uses four programs across one to four workers
to additionally cover queue turnover. Other dependency and failure cases remain
in the CRAN profile. Symbolic-link scenarios skip only when the OS cannot create
the link; worker tests wait for actual request admission before interruption.

The `installed-tests` and `no-sas-tarball` CI jobs use `NOT_CRAN=true` and run
the complete test suite against installed packages. This includes:

- The extended public-workflow regression suite in `test-review-first-public.R`.
- Twenty-component repair and repeated-helper-edit scenarios.
- Complete late-dependency retrieval across all agent roles.
- Parallel crash/resume and helper-rollback integrations.
- Environment-observation execution and dependency reassessment after resume.
- All one-to-four-worker combinations and the full drafting/execution cross-product.
- The bundled ellmer installation fixture.

These process-heavy scenarios are marked with `skip_on_cran()` to leave check-time
margin on CRAN. Smaller tests for the same mechanisms remain in the CRAN profile;
the extended cases remain required CI gates. Mock transport retries retain their
attempts and assertions but use a zero backoff because no external service is called.

Run all source-tree tests with `NOT_CRAN=true Rscript -e 'testthat::test_local()'`.
Check a built archive with `NOT_CRAN=false R CMD check --as-cran sas2r_*.tar.gz`.
