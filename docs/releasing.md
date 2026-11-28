# Release and development branches

Reserve `main`, the default branch, for CRAN submissions only. It can contain
a reviewed candidate ahead of the version published on CRAN. Start ordinary
code, test and documentation changes from `develop` and target them there.
The [CRAN package page](https://CRAN.R-project.org/package=sas2r) identifies
the published version.

The release branch ships a smaller set of tests covering the public API and
package features. Extended integration scenarios stay on `develop`; their
absence from the CRAN archive does not make them optional release checks.

## Preparing a release

1. Select a `develop` commit with the intended package version and implementation.
   Make functional fixes on `develop` first. Keep its complete tests.
2. Prepare a submission pull request against `main` from the reviewed changes.
   Preserve release-only CI and test settings. Include representative tests for
   each feature and the source-faithful repair checks. Align DESCRIPTION, NEWS,
   help, installation instructions and guide links with the candidate version.
   Label the candidate and identify the version currently published on CRAN.
3. Set `.github/development-test-ref` on the release branch to the full commit ID
   from step 1. Keep that file absent on `develop`, where CI tests its own current
   sources. The fixed ID prevents future development tests from being run against
   an older release accidentally.
   If a release-only correction needs updated full tests while `develop` has
   moved on, use a compatible test snapshot and retain it with a permanent
   `release-tests-*` tag. Record its commit ID in the pin and its tag in
   `cran-comments.md`. Keep the tag after deleting its working branch so CI
   can still fetch the commit; restore only `tests/` from that snapshot.
4. Run `R CMD build` and check the archive with
   `NOT_CRAN=false R CMD check --as-cran`, including CRAN incoming checks.
   The release workflow builds one source archive on Linux with R release.
   Download the `sas2r-source-candidate` artifact and verify its contents
   against the included `SHA256SUMS`. The platform checks and full installed
   tests use that archive. Retain Windows R-devel coverage and Linux PDF manual checks. Record test time
   and overall package check time separately. The previous CRAN review requested
   an overall check under ten minutes on Windows and Linux.
5. Install that same archive and run the complete tests from the recorded
   development commit against the installed package. The `installed-tests` and
   `no-sas-tarball` CI jobs restore only `tests/` after installation, so the runtime
   under test remains the release package. They set `NOT_CRAN=true` to include
   the development suite's existing extended cases. Use a disposable checkout
   when reproducing those restore steps locally.
6. Resolve release-blocking findings and record the candidate's measured results
   and relevant reviewer responses in `cran-comments.md`. Merge the reviewed
   submission changes to `main`. Verify that the package contents match the
   tested candidate. Submit that tested R source archive through the CRAN form
   and retain its source commit, SHA-256 and check logs.
7. After CRAN publishes the update, tag the accepted source commit and update
   the release status in the documentation on both branches.

Release-only test reductions and `.github/development-test-ref` must not be merged
back into `develop`. Bring shared documentation and CI improvements across
separately. For each subsequent release, start from the chosen development
commit and reassess the representative suite; do not blindly merge away tests.
