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
  or remove objects from the test runner's global workspace. An unused global
  sentinel helper was removed. A new regression seeds a workspace registry
  only in a disposable subprocess and confirms package calls leave it untouched.
- Help, the runtime template, the helper reference and the vignette were
  updated together. Regression tests cover message suppression, retained
  smoke/bundle error logs, local library changes and replay isolation.

Other uses of `<<-` update variables already initialized in enclosing local
scopes, as described in the CRAN Cookbook; they do not create global bindings.

The release retains 89 representative test files and all previous corrections
for unexported examples, temporary file destinations and check timing. Complete
release regression tests are pinned at
`44313af07a58d22e69176c08624f7dc0a553c05f` and run against the installed release
archive. The permanent tag `release-tests-0.5.8-20261021` retains that snapshot
after its working branch is deleted. Development-only features are not included in this revision.

## Current validation

The current archive passes `R CMD check --as-cran --no-manual` on macOS with
R 4.4.0: 0 errors, 0 warnings and two NOTEs (New submission and unable to verify
current time). All 3,285 shipped assertions pass in 34 seconds locally. Examples and rebuilt
vignettes pass, with no temporary-directory detritus.

The unchanged runtime implementation also passed all seven offline migration
acceptance gates and the complete release regression suite against an installed
archive: 1,779 cases, 10,788 passing assertions, no failures or test warnings,
and three documented skips. This final archive additionally includes the
subprocess workspace regression. The complete suite and platform checks are
rerun by release PR #60:
https://github.com/songyue-chen/sas2r/pull/60

The final archive was uploaded to win-builder R-devel on 2026-10-21 at 16:36 UTC.
The web form confirmed the filename and 736,131-byte size; the Windows check
result is pending. No CRAN resubmission has been made.

Source archive SHA-256:
`2f7dd8d3b65b60e0ac0774976b82ac3f8950f8ca676fe70a9633a734b19ce417`.

## Previous Windows measurement

The earlier timing-only archive passed win-builder R-devel in 375 seconds
overall, including 155 seconds in tests, with 0 errors, 0 warnings and one
New submission NOTE. That result belongs to the previous archive and is not a
Windows timing measurement of this revision.

Version 0.5.8 remains a first-submission candidate, not an accepted CRAN release.
