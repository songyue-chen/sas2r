## Resubmission: sas2r 0.5.8

This resubmission addresses the manual review of 0.5.5, which reported an overall
check time of 14 minutes, including 12 minutes for tests.

The submitted source contains a smaller representative test suite. Extended
integration matrices and repeated translation, review, repair and startup
scenarios remain on the public `develop` branch and run in CI against the installed
release package. The submission tests cover package features using small synthetic
fixtures, including actual translation, execution, repair, resume, exports,
parallel execution, reference comparison and source-faithful repair. Selecting the
smaller suite does not change package implementation.

Examples, vignettes and tests require neither a SAS installation nor provider
credentials or paid API calls. Checks use at most two translation workers.

Version 0.5.8 also includes the documented fixes since 0.5.5: source-based repair
and diagnosis improvements, consistent autoexec library initialization across
preflight/execution/export, and reduced repeated scanner work. See NEWS.md.

## Validation

Final archive check results and measured timings will be recorded here before
submission. CRAN acceptance has not been received.
