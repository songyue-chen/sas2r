## Resubmission: sas2r 0.5.8

This resubmission addresses the manual review requesting removal of examples
for unexported functions and correction of file-writing destinations.

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
execution failures from taking priority in independent repairs.

The smaller representative submission test suite is retained. The complete
suite remains on the development branch and is checked against the installed
release package using `.github/development-test-ref`. Tests use small synthetic
fixtures, need no SAS installation or provider credentials, and use at most two
translation workers.

## Validation

The corrected source archive passes local `R CMD check --as-cran --no-manual`
on macOS with R 4.4.0:

- 0 errors, 0 warnings.
- 2 NOTEs: "New submission" and "unable to verify current time".
- Shipped tests: 7,420 assertions passed; 0 failures and 0 test warnings.
- Test elapsed time: 136 seconds.
- Examples and both rebuilt vignettes passed; no temporary-directory detritus.

The documentation runner executed 18 offline examples, parsed 6 network examples
without provider calls, and validated 23 configurations. All seven local
migration acceptance checks passed against the installed archive.

An additional 1,084 focused assertions passed against the installed archive
from the source checkout, with 0 failures, 0 warnings, and 0 skips.

The release PR tracks the current platform checks, PDF manual checks, and
complete development-suite checks in CI:

Release PR: https://github.com/songyue-chen/sas2r/pull/54

Version 0.5.8 remains a first-submission candidate, not an accepted CRAN release.
