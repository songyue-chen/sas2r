## sas2r 0.6.2

This update follows the published 0.5.8 release. The candidate uses the reviewed
package implementation and complete development tests at
`4e5bddd7a9f591840247ac2f00a0a5539a6f3d71`.

## Earlier review feedback

- Routine progress can be silenced. The one new note, for a variable that no
  input supplies, is a `message()`. No function added since 0.5.8 writes to the
  console with `cat()` or `print()` outside a print method. Execution failures
  keep their diagnostics.
- Runtime state stays local. No function writes to the global workspace.
  `sas_output_path()` compares the calling environment with `globalenv()` only
  to stop its search for the output root; it assigns nothing. Each `<<-`
  updates a variable already initialized in an enclosing local scope, as
  described in the CRAN Cookbook.
- Examples use explicit or temporary output directories, and internal helpers
  have no runnable help examples. All 39 help topics with examples document
  exported functions.
- The archive ships a representative offline test suite. Full development
  tests run separately against the installed package. The previous review
  requested an overall check time under ten minutes on Windows and Linux.

## Submission archive and checks

The archive contains 103 representative test files. The package code,
namespace, help and runtime sources match the reviewed development commit. This
submission record is excluded from the archive.

Not yet recorded. These come from the checks of the release pull request, on
the one archive that will be submitted, and are entered here before submission:

- the submission file's name, size, SHA-256, build R version and release commit;
- overall check time and test time on each platform, including Windows and
  Linux R-devel with incoming checks, and the PDF and HTML manuals on Linux;
- the complete development suite against the installed submission file;
- the reverse-dependency listing on the day of submission.

## Rehearsal before the pull request

A rehearsal archive was built on macOS with R 4.4.0 from this tree, with the
README links pointing at the preceding development commit. Its package code,
help and tests are the ones in this candidate.

- `NOT_CRAN=false R CMD check --as-cran --no-manual`: 0 errors, 0 warnings and
  one NOTE, `unable to verify current time`, which comes from the check host.
  154 seconds overall and 72 seconds for tests, with 5,809 passing assertions
  and 12 source-checkout skips.
- The shipped tests pass against the installed archive with the global
  workspace locked (`lockEnvironment(globalenv())`): 6,091 assertions, no
  failures.

These are not the submission's results.
