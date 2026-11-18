# sas2r 0.6.0 development status

The published version is [sas2r 0.5.8](https://CRAN.R-project.org/package=sas2r).
Version 0.6.0 is under review on `develop`. The
[0.5.8 submission record](https://github.com/songyue-chen/sas2r/blob/e28af41d73e5b577c9ed4c628539c635a3340c20/cran-comments.md)
contains the earlier release's checks and correspondence.

## Development checks

All ten [CI jobs for development commit 48aa30b8](https://github.com/songyue-chen/sas2r/actions/runs/43790121436)
passed. The package check took 13 min 18 s on Linux release and 18 min 51 s on
Windows release. Both exceed the ten-minute overall check time requested in the
previous CRAN review. These timings include the tests shipped by `develop`.

## Before submission

Development regression tests cover distinct report destinations through
assessment and export, report writers inside translated macros, PDF text
encoding, and late macro inputs with a project writer. Unsupported PDF text
fails before the destination is opened. Projects with an ambiguous producer
order can declare the intended sequence in `migration.execution_order`.

Prepare the representative CRAN suite and pin its compatible full development
tests. Record the exact submission archive and its Windows and Linux results
here after checking it.
The [release workflow](docs/releasing.md) describes those steps.
