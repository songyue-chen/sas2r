## Submission

This is a new submission.

`sas2r` translates SAS programs and projects to R using dependency-aware
orchestration, program smoke execution, and final-output evidence. It requires
no SAS installation and no SAS licence.

<!-- Before submitting: run the win-builder and R-hub checks below, replace the
     "(not yet run)" markers with their results, and delete this comment. -->

## Test environments

* local: macOS 26.6.2 (aarch64-apple-darwin20), R 4.4.0
* win-builder: R-devel and R-release — (not yet run)
* R-hub: linux, macos, windows — (not yet run)

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Songyue Chen <chuckknow@gmail.com>'

New submission
```

This is the package's first submission to CRAN.

One further NOTE appears on the local machine only:

```
* checking for future file timestamps ... NOTE
unable to verify current time
```

That is an artifact of the local check machine being unable to reach a time
server, not a property of the package.

The local check was run with `--no-manual` because LaTeX is not installed on
that machine; the PDF manual is exercised by the win-builder and R-hub runs.

## Notes for the reviewer

* **No network access or credentials are needed to check this package.** The
  deterministic rule-based translator and the dataset comparator run entirely
  offline, and those are what the runnable examples exercise.

* **Optional LLM features are in Suggests.** AI-assisted translation, review,
  and repair are provided through `ellmer`. Constructing an adapter with
  `sas_llm()` contacts no network and reads no credentials, so that example is
  runnable (guarded by `requireNamespace("ellmer")`). Every example that would
  actually contact a model provider — `sas_llm_probe()`, `sas_llm_models()`,
  and the agent-assisted branch of `sas_translate()` — is wrapped in
  `\dontrun{}`.

* **Filesystem use.** All examples, tests, and vignette code write only inside
  `tempdir()`. The package writes elsewhere only to a destination the user
  passes explicitly, e.g. `sas_write(x, dir)`.

* **Test time.** The test suite takes roughly 107 seconds elapsed on the local
  machine. Tests requiring optional packages are guarded with
  `skip_if_not_installed()`.
