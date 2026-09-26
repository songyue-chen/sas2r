This is the first CRAN submission of sas2r, version 0.5.5.

The package provides dependency-aware translation of SAS programs to R, execution of generated code, comparison with supplied reference datasets, and reporting of unresolved translation and validation findings. Rule-based translation works offline without a SAS installation. Optional language-model translation, review, and repair use a user-configured provider through ellmer and may require an account and API credentials.

The source archive was built with R 4.6.1 and checked with R CMD check --as-cran on Linux. The same archive passed win-builder checks on Windows R-release 4.6.1 and R-devel (2026-09-25 r90590), including PDF and HTML manuals:

R-release: https://win-builder.r-project.org/JFdR234d25IO/
R-devel: https://win-builder.r-project.org/zb9rqkmw4Rnj/

All three checks report 0 errors, 0 warnings, and one NOTE:

    New submission

Checks of the same source revision also passed on Linux R-devel, Windows and macOS R 4.6.1, and the declared minimum R 4.1.3. The full installed-package suite and offline ellmer integration checks pass in CI.

Examples, vignettes, and automated tests require neither a SAS installation nor provider credentials or paid API calls. Checks use at most two translation workers. Extended process-heavy scenarios run in CI; the CRAN test profile retains unit tests and representative translation, execution, review, repair, resume, dependency, and output-validation coverage.

On win-builder, the complete checks took 938 seconds (R-release) and 924 seconds (R-devel). The test suites took 810 and 789 seconds elapsed, respectively; both reported 8,862 passing assertions, no failures, and no test warnings. The longest individual example took under two seconds.
