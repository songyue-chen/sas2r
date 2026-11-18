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

## Submission archive and checks

The submission file is `sas2r_0.6.0.tar.gz` (776,662 bytes). It was built with
R 4.6.2 on Linux from release commit
`f812e2c62566c357a0ee6d1f59d7d209402792f1`. Its SHA-256 is
`f2fb66c19f09d3271b09d5603b3f7f0b353d24fadee5d5ae9473c0785d6ddcdb`.
The package code, namespace, help and runtime sources match the reviewed
development commit. This submission record is excluded from the archive.

Each platform checked the same file with `R CMD check --as-cran` and reported
zero errors, warnings and NOTEs. Windows and Linux R-devel included incoming
checks. The Linux release and R-devel jobs checked both PDF and HTML manuals.

| Platform | Overall check | Tests |
| --- | ---: | ---: |
| Windows R-devel (r90926) | 198 s | 94 s |
| Windows R release | 140 s | 66 s |
| Linux R-devel (r90926) | 186 s | 78 s |
| Linux R release | 113 s | 49 s |
| Linux R 4.1.3 | 167 s | 74 s |
| macOS R release | 173 s | 75 s |

Times are elapsed seconds and exclude runner and dependency setup. The archive
contains 96 representative test files. Each platform passed 3,666 assertions
with 12 source-checkout skips. All 38 help topics with examples document
exported functions.

An additional macOS R 4.4.0 check, including incoming checks and both manuals,
took 148 seconds overall and 50 seconds for tests. Its only NOTE was
`unable to verify current time`. The complete development suite against the
locally installed submission file passed 11,392 assertions with three
environment-dependent skips and no failures or test warnings. Offline
documentation examples and all seven synthetic migration acceptance checks
passed. The CRAN package index listed no reverse dependencies, including
Suggests and Enhances, on November 18, 2026.

[Platform checks and source archive](https://github.com/songyue-chen/sas2r/actions/runs/44354234517)
