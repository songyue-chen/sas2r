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

The smaller representative submission test suite is retained. The complete
suite remains on the development branch and is checked against the installed
release package using `.github/development-test-ref`. Tests use small synthetic
fixtures, need no SAS installation or provider credentials, and use at most two
translation workers.

## Validation

Fresh source-archive and complete-suite validation results are recorded after
checking this revision. Earlier submission timings are not reused as evidence
for the corrected archive.
