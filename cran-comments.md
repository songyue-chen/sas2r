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

The development review reproduced four issues with synthetic inputs.

- Distinct SAS report destinations such as `table.html` and `outputs/table.html`
  can overwrite one file and both pass output assessment.
- A translated reporting function that calls `sas_output_path()` from its own
  frame can fail even when the enclosing bundle runtime has a valid output root.
- The plain PDF listing helper can finish with conversion warnings and replace
  Greek letters or Chinese characters with dots in the saved document.
- A readable library member can clear a reported macro input dependency even
  when a project program writes the member and is scheduled after the caller.

Resolve these findings, prepare the representative CRAN suite and pin its
compatible full development tests. Record the exact
submission archive and its Windows and Linux results here after checking it.
The [release workflow](docs/releasing.md) describes those steps.
