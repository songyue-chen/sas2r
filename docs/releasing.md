# Release and development branches

`main` is the default branch and represents the stable CRAN release. Until the
first acceptance, it holds the release candidate and the README identifies its
pending status. `develop` holds ongoing work and the complete regression suite.
Start feature branches from `develop` and target development pull requests there.

The release branch ships a smaller set of tests covering the public API and
package features. Extended integration scenarios stay on `develop`; their
absence from the CRAN archive does not make them optional release checks.

## Preparing a release

1. Select a `develop` commit with the intended package version and implementation.
   Make functional fixes on `develop` first. Keep its complete tests.
2. Prepare the release changes from that commit. Retain representative tests for
   every feature and the source-faithful repair checks. Do not change package
   implementation merely to shorten checks.
3. Set `.github/development-test-ref` on the release branch to the full commit ID
   from step 1. Keep that file absent on `develop`, where CI tests its own current
   sources. The fixed ID prevents future development tests from being run against
   an older release accidentally.
4. Run `R CMD build`, then `R CMD check --as-cran` on the resulting archive. Check
   total elapsed time as well as test time on Windows and Linux, with comfortable
   margin below the limit CRAN gave in its review.
5. Install that same archive and run the complete tests from the recorded
   development commit against the installed package. The `installed-tests` and
   `no-sas-tarball` CI jobs restore only `tests/` after installation, so the runtime
   under test remains the release package. They set `NOT_CRAN=true` to include
   the development suite's existing extended cases. Use a disposable checkout
   when reproducing those restore steps locally.
6. Update `cran-comments.md` with the response to CRAN and measured results.
   Submit the tested source archive through the CRAN submission form. Preserve
   its checksum and check logs. GitHub's automatic source downloads are not a
   replacement for the archive produced by `R CMD build`.
7. After acceptance, promote the accepted release to `main`, tag its source commit
   (for example `v0.5.8`), and update the README's release status. For this first
   submission, the explicitly labelled candidate may already be on `main`.

Release-only test reductions and `.github/development-test-ref` must not be merged
back into `develop`. Bring shared documentation and CI improvements across
separately. For each subsequent release, start from the chosen development
commit and reassess the representative suite; do not blindly merge away tests.
