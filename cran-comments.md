## Resubmission: sas2r 0.5.8

This resubmission reduces the Windows check time reported by the incoming
checks (20 minutes overall, including 16 minutes in tests). A first reduction
passed GitHub Windows checks but still took 782 seconds on win-builder, with
550 seconds in tests. After reducing repeated integration work further, the
revised source archive now completes win-builder R-devel checks in 375 seconds
(6 minutes 15 seconds), including 155 seconds in tests. Installation took
41 seconds. The result has 0 errors, 0 warnings and only the New submission NOTE.

The shipped suite contains 89 representative test files instead of 130.
Public result, export and resume checks share one migration, and transpilation
checks share one fixture. Separate parallel, macro-bundle, startup-export and
multi-program execution scenarios remain in the complete development suite;
their smaller configuration and dependency contracts remain in the archive.
The test log now prints the ten slowest files to make remote timings visible.
Package runtime behavior is unchanged by this timing reduction.

The complete source-faithful repair suite is unchanged. Format smoke execution,
setup exclusion, conflicting format definitions and file-writing regressions
remain in the submitted tests. Full development tests from commit
`7ae0f92d17239799c57630f20c79867b4ce1884a` run against the installed release
archive in CI. Windows R-devel is included alongside Windows release.

The earlier manual-review corrections are also retained:

- Removed examples from all seven internal helper help topics, including
  `sas2r_lib_member_file` and `split_ds`. These functions remain internal.
- Internal audit logging now defaults to the R session temporary directory.
  The internal lockfile writer requires an explicit destination.
- Sourcing the installed demo input script only defines its input generator.
  Both the generator and command-line script require an explicit destination.
- README and vignette writing examples use temporary destinations. The runtime
  vignette runs a temporary copy of an exported bundle and restores the caller's
  working directory through `source(..., chdir = TRUE)`.
- Added checks for default log destinations and the demo's explicit destination.
  Regenerated help and the runtime helper reference together.

Additional fixes align format loading and startup behavior in smoke and bundle
execution, report unsupported format definitions and widths, and keep unrelated
execution failures from taking priority in independent repairs. Equivalent
format definitions remain supported when their values or numeric ranges are
declared in a different order.

The smaller representative submission test suite is retained. The complete
suite remains on the development branch and is checked against the installed
release package using `.github/development-test-ref`. Tests use small synthetic
fixtures, need no SAS installation or provider credentials, and use at most two
translation workers.

## Validation

The revised source archive passes local `R CMD check --as-cran --no-manual`
on macOS with R 4.4.0:

- 0 errors, 0 warnings.
- 2 NOTEs: "New submission" and "unable to verify current time".
- Shipped tests: 3,260 assertions passed; 0 failures and 0 test warnings.
- Test elapsed time: 35 seconds, down from 58 seconds for the first reduction
  and 137 seconds for the original submitted archive.
- Examples and both rebuilt vignettes passed; no temporary-directory detritus.
- Archive inspection confirms that runtime code, help, examples and vignettes
  are unchanged; only tests and the packaging timestamp differ.

The complete development suite at the pinned test commit passed locally
against this installed archive: 10,777 assertions, no failures or test warnings,
and two expected skips. All 11 CI checks passed, including all six platform
checks, both installed-package development-suite jobs and all seven acceptance
gates. A development-test assertion was corrected to distinguish reference
contents from incidental digits in paths; this does not change the release
archive.

Win-builder R-devel (2026-10-08 r90650 ucrt) checked the same archive:

- Installation: 41 seconds; overall check: 375 seconds; tests: 155 seconds.
- 0 errors, 0 warnings, 1 NOTE: "New submission".
- 3,277 assertions passed; 0 failures, 0 test warnings, 12 source-only skips.
- Examples, rebuilt vignettes, and the PDF and HTML manuals passed.

Win-builder logs: https://win-builder.r-project.org/UcsR5RJUEQxU/

CI results: https://github.com/songyue-chen/sas2r/actions/runs/37887051787

Source archive SHA-256:
`9cfd43d8cd9552a87f02360fcb808b7c492f9ce0f5e6a8cab1cf16fbc22856ce`.

Release PR: https://github.com/songyue-chen/sas2r/pull/55

Version 0.5.8 remains a first-submission candidate, not an accepted CRAN release.
