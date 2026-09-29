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
preflight/execution/export, and reduced repeated scanner work. Help, vignettes and the shared helper
reference also clarify output paths, runtime behavior and provider support.
See NEWS.md.

## Validation

The final source archive was built with R 4.6.1 and checked on Ubuntu using
`R CMD check --as-cran`, including the PDF manual and vignettes:

- Total check time: 3 minutes 38 seconds; tests: 143 seconds elapsed.
- 0 errors, 0 warnings, 1 NOTE: "New submission".

The same package implementation and test selection also passed on Windows
(8 minutes 43 seconds overall), macOS (3 minutes 35 seconds), R-devel
(5 minutes 23 seconds) and R 4.1.3 (5 minutes 52 seconds). These checks used
`--as-cran`; Windows, macOS and R 4.1.3 used `--no-manual`. These results include the
final help, vignette and generated helper-reference corrections. Times are
elapsed measurements on GitHub-hosted runners.

The complete development suite passed against the installed release package
in CI (10,645 assertions, 0 failures, 0 test warnings), including the separate
clean-library tarball and migration acceptance checks. The local release test
profile decreased from 362 to 118 seconds. No package implementation was changed
to achieve this reduction. CRAN acceptance has not been received.

All 10 jobs passed for source commit 8c9117d0077f65b885909216745c38d835d66f9d:
https://github.com/songyue-chen/sas2r/actions/runs/36509994153

Submitted source archive SHA-256:
562b086c942a5c1547b7c0160614e3e434744239c7f3753f6f4dfe2ee8b2ac0d
