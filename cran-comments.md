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

The submission file is `sas2r_0.6.2.tar.gz` (866,949 bytes). It was built with
R 4.6.2 on Linux from release commit
`a3aa39438bb124fa63d748b4520d54a0a4e2785e`. Its SHA-256 is
`781f156651c6f94d8b53fbc21da55ff6ec19183927da464f2e2f3734ae9f92d6`.
The package code, namespace, help and installed files match the reviewed
development commit. This submission record is excluded from the archive.

Each platform checked the same file with `R CMD check --as-cran` and reported
zero errors, warnings and NOTEs. Windows and Linux R-devel included incoming
checks. The Linux release and R-devel jobs checked both PDF and HTML manuals.

| Platform | Overall check | Tests |
| --- | ---: | ---: |
| Windows R-devel (r90926) | 333 s | 188 s |
| Windows R release (4.6.2) | 201 s | 119 s |
| Linux R-devel (r90998) | 228 s | 119 s |
| Linux R release (4.6.2) | 224 s | 123 s |
| Linux R 4.1.3 | 227 s | 124 s |
| macOS R release (4.6.2) | 125 s | 74 s |

Times are elapsed seconds and exclude runner and dependency setup. The archive
contains 103 representative test files. Each platform passed 5,809 assertions
with 12 source-checkout skips.

The complete development suite, pinned to the development commit, ran against
the installed submission file and passed 13,465 assertions with two
environment-dependent skips and no failures or test warnings. The offline
documentation examples and the synthetic migration acceptance run passed
against the same installed file. The shipped tests also pass against it with
the global workspace locked (`lockEnvironment(globalenv())`).

CRAN listed no reverse dependencies, including Suggests and Enhances, on
November 28, 2026.

[Platform checks and source archive](https://github.com/songyue-chen/sas2r/actions/runs/46114525073)
