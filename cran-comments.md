## Resubmission: sas2r 0.5.8

This revision addresses the manual review about console output and writes to
the global workspace.

- Progress and preflight updates now use `message()`. They remain visible by
  default and can be silenced with `suppressMessages()`. The existing
  `sas2r.progress` option still controls progress separately.
- The executor's error diagnostic uses `message()` instead of `cat()`.
  Worker stdout/stderr files, structured conditions and saved reports are
  retained even when the calling migration is silenced.
- Executor callbacks now update explicitly allocated local environments
  rather than using `<<-`.
- Package runtime helpers no longer fall back to a registry in `.GlobalEnv`.
  Interactive examples load the runtime into a dedicated environment.
  Standalone generated scripts still use the runtime explicitly loaded into
  their own execution environment, including direct `Rscript` execution.
- Replay scripts source code locally and restore the working directory on
  success or error. Tests no longer source helpers into, assign sentinels to,
  or remove objects from the global workspace. An unused global sentinel
  helper was removed.
- Help, the runtime template, the helper reference and the vignette were
  updated together. Regression tests cover message suppression, retained
  smoke/bundle error logs, local library changes and replay isolation.

Other uses of `<<-` update variables already initialized in enclosing local
scopes, as described in the CRAN Cookbook; they do not create global bindings.

The release retains 89 representative test files and all previous corrections
for unexported examples, temporary file destinations and check timing. Complete
release regression tests are pinned at
`744510a74289f947cc14e746651f93fe3669cf13` and run against the installed release
archive. Development-only features are not included in this revision.

## Current validation

The current archive passes `R CMD check --as-cran --no-manual` on macOS with
R 4.4.0: 0 errors, 0 warnings and two NOTEs (New submission and unable to verify
current time). The shipped tests take 40 seconds locally. Examples and rebuilt
vignettes pass, with no temporary-directory detritus.

All seven offline migration acceptance gates pass against the installed archive.
The complete release regression suite and platform CI checks are still running.
The updated archive has not been submitted to CRAN or win-builder.

Source archive SHA-256:
`98bb6020bfddd43fbbc17e1d112aa4a20e64f7c6a55b3a7a0050e2e9f09d5167`.

## Previous Windows measurement

The earlier timing-only archive passed win-builder R-devel in 375 seconds
overall, including 155 seconds in tests, with 0 errors, 0 warnings and one
New submission NOTE. That result belongs to the previous archive and is not a
Windows timing measurement of this revision.

Version 0.5.8 remains a first-submission candidate, not an accepted CRAN release.
