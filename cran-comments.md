## sas2r 0.6.0

This update follows the published 0.5.8 release. The candidate uses the reviewed
package implementation and complete development tests at
`21803ae4ad638378cc2f7a1473d1d43834a9c3b6`.

## Earlier review feedback

- Routine progress can be silenced. Execution failures keep their diagnostics.
- Runtime state stays local. Examples use explicit or temporary output
  directories, and internal helpers have no runnable help examples.
- The archive ships a representative offline test suite. Full development
  tests run separately against the installed package. The previous review
  requested an overall check time under ten minutes on Windows and Linux.

## Validation

Candidate archive measurements will be recorded here before submission.
