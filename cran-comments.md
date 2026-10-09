## Resubmission: sas2r 0.5.8

This resubmission reduces the Windows check time reported by the incoming
checks (20 minutes overall, including 16 minutes in tests). The shipped suite
now contains 89 representative test files instead of 130. Repeated public
workflow setups share small fixtures, and extended integration and provider
matrices remain in the complete development suite. Package runtime behavior
is unchanged by this timing reduction.

The complete source-faithful repair suite and the recent format and file-writing
regressions remain in the submitted tests. Full development tests from commit
`e43392b32d93291175fa3f6671e78d3b8450f8cf` run against the installed release
archive in CI. Windows R-devel is now included alongside Windows release.

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
- Shipped tests: 3,911 assertions passed; 0 failures and 0 test warnings.
- Test elapsed time: 58 seconds, down from 137 seconds for the previous archive.
- Examples and both rebuilt vignettes passed; no temporary-directory detritus.

GitHub Windows checks pass with only the expected "New submission" NOTE:

- R release: 3 minutes 46 seconds overall; 128 seconds in tests.
- R-devel: 5 minutes 15 seconds overall; 174 seconds in tests.

These are package check durations, excluding CI setup and dependency installation.
The previous Windows R-release check took 8 minutes 46 seconds overall, including
394 seconds in tests. A separate win-builder R-devel check has been requested.

The complete pinned development suite passed against the installed source
archive: 10,775 assertions, 0 failures, 0 test warnings and 2 expected skips.
The documentation runner executed 18 offline examples, parsed 6 network examples
without provider calls, and validated 23 configurations. All seven local
migration acceptance checks passed against the installed archive.

The release PR tracks the platform checks, PDF manual checks, and complete
development-suite checks in CI:

Release PR: https://github.com/songyue-chen/sas2r/pull/55

Version 0.5.8 remains a first-submission candidate, not an accepted CRAN release.
