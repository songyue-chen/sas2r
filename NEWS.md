# sas2r 0.5.8

- Correct internal help examples and keep default audit logs and example outputs in temporary directories.

- Improve startup library handling across preflight, execution and exports.
  Usable SAS startup paths take precedence over configured library fallbacks.
- Speed up SAS project scanning.
- Bug fixes for dependency handling, startup warnings and macro error diagnosis.
- Clarify saved output paths, runtime behavior and provider documentation.

# sas2r 0.5.7

- Add clearer dataset discrepancy reports with optional AI explanations and
  repair decisions.
- Improve repairs across programs and macros using related SAS and R code.
  SAS source remains authoritative; dataset row numbers, key values, cell values,
  subject identifiers and raw runtime messages stay out of AI diagnostic prompts.
- Improve SAS comparison-chain handling and translation guidance for DATA-step
  retention and dplyr style.
- Bug fixes for dataset dependency tracking, macro error diagnosis and review
  reuse; add advisory regex checks.
- Older saved checkpoints are invalidated by the updated evidence format.

# sas2r 0.5.6

- Add `migration.execution_order` for programs sharing a WORK session, with
  dataset reads following the latest preceding write.
- Add optional AI diagnosis of preflight failures, including setup suggestions
  and issue-report guidance. Use `diagnose = "off"` for offline inspection.
- Bug fixes for ordered dataset dependencies, macro effects, missing-input
  detection, execution timeouts and diagnosis budget handling.

# sas2r 0.5.5

- Improve installed examples, configuration help and run-status explanations;
  remove the PHUSE benchmark mode.
- Readiness requires at least one mandatory output and checks every scheduled
  component and unresolved lineage. Unsupported SAS behavior remains deferred.
- Bug fixes for SAS missing values, comparisons, rounding, character ordering,
  formats, MERGE behavior and include handling.
- Numeric comparisons now use absolute tolerance by default; relative tolerance
  is opt-in. Output checks respect configured ordering and date/time policies.
- Improve resume, repair diagnostics, budget enforcement and translation
  performance. Prevent concurrent writes to the same migration directory.
- Exports require an empty destination or explicit replacement of an earlier
  sas2r export. Standalone bundles additionally require `cli` and `vctrs`.
- Limit credentials and raw execution diagnostics available to agents.
  Untrusted generated R still requires external isolation.

# sas2r 0.5.4

- Prepare package metadata, documentation links and tests for CRAN submission.
- Store parsing and macro-index caches in temporary storage to leave input
  projects unchanged.
- Support quiet runs with `options(sas2r.progress = FALSE)` and
  `suppressMessages()`, while retaining saved diagnostics and returned results.
- Bug fixes for Windows paths, parallel-worker startup and platform-specific
  tests. Oversized worker configurations report the serial-run alternative.

# sas2r 0.5.3

- Prefer installed, allowlisted tidyverse packages for faithful translations;
  `dialect: tidyverse` is the default and `dialect: base` selects base R preference.
- Add tidyverse and graphics guidance, plus advisory code-style observations
  in translation reports.
- Simplify examples and make budget limits optional and explicit.
- Updated prompts and policies invalidate earlier resume checkpoints.

# sas2r 0.5.2

- Let translation, review and repair retrieve additional SAS and R dependency
  code; improve guidance for native R graphics and PDF output.
- Allow independent programs to execute when other branches have unresolved
  dependencies. Partial runs cannot be reported as migration-ready or validated.
- Improve runtime failure summaries, output coverage and missing-context reports.
- Bug fixes for library paths, dependency resolution, false missing-source
  findings and reader-versus-producer error diagnosis.
- Checkpoints from 0.5.1 or earlier are invalidated; regeneration can incur new
  model calls.

# sas2r 0.5.1

- Continue translating available source despite missing inputs, dependencies or
  cycles, while deferring execution that cannot be supported.
- Preserve completed work after individual component failures and let independent
  programs continue. Provider-wide, configuration and accounting failures remain terminal.
- Improve preflight recognition of SAS metadata, automatic macro variables and
  configured libraries; make unresolved dependencies and incomplete work clearer.
- Bug fixes for resume readiness and custom-adapter worker startup, with guidance
  for oversized configurations and serial execution.

# sas2r 0.5.0

- Add opt-in parallel translation and final reviews through
  `max_parallel_translations`, defaulting to one workflow. Dependency order,
  component review, repair and bundle validation remain enforced.
- Add preflight pipeline coverage checks and consistent console, log and HTML
  run summaries. Simplify the quickstart and provider guides.
- Coordinate parallel budgets, tool requests and usage accounting. With ellmer
  0.5.0+, `max_calls` counts individual requests, including tool continuations;
  existing budgets may need adjustment.
- Older ellmer and custom adapters without a process factory fall back to one
  workflow. `max_parallel_translations` and `max_tries` cannot both exceed one.
- Save component phases, review progress and repair allowances for resume;
  retain compatible version-8 revisions in version-9 checkpoints.
- Bug fixes for worker startup, relative output paths, dependency findings,
  crash recovery and manifest consistency.

# sas2r 0.4.5

- Consolidate reviews of unchanged components before bundle execution and reuse
  completed reviews and smoke checks when their context is unchanged.
- Preserve review progress and repair allowances across interrupted runs.
- Improve shared SAS semantic guidance, integration review and source-derived
  KEEP/DROP context; distinguish representation differences from lost behavior.
- Bug fixes for shared-helper repair checks and bundle-attempt reporting.
- Older checkpoints may regenerate. Review-context changes can refresh reviews;
  shared prompt, policy or runtime changes can also regenerate translations.

# sas2r 0.4.4

- Add registry-aware `lib_members()` and `lib_exists()` helpers and improve macro,
  merge, initialization and runtime guidance shared by all agents.
- Improve shared-helper repairs and investigation of missing outputs, preserving
  the retained runtime when a candidate is rejected.
- Avoid regenerating saved translations solely because the package version
  changes; distinguish focused reviews from full reviews.
- Add clearer repair progress, token-usage summaries and advisory code/output
  observations. Output-change summaries stay out of model requests.
- Bug fixes for helper declarations, package allowlists, macro defaults and
  missing-context handling. Generated R is not a filesystem sandbox.

# sas2r 0.4.3

- Keep reference comparison answers out of translation, review and repair
  requests; only source-grounded findings authorize code changes.
- Preserve correct source translations when references disagree, while keeping
  required reference failures unresolved.
- Bug fixes for repair regressions, shared-helper consistency, missing generated
  intermediates and preservation of completed review evidence.
- Clarify that source code, comments, paths, schemas and diagnostics may reach
  models. Omitting dataset previews is not automatic de-identification.

# sas2r 0.4.2

- Add an offline `START_HERE.html`, an editable bundle and separate outputs,
  reports and diagnostics, including access to failed or incomplete translations.
- Make manual bundle runs use `.sas2r_output_root` (default `bundle/output/`);
  `sas_write()` keeps automated results separately under `saved-outputs/`.
- Add one budgeted correction request for fixer parse/lint failures and improve
  shared guidance for macros, graphics and runtime helpers.
- Bug fixes for repair allowances, pending findings and downstream repairs.
  Clarify unresolved output names and output-family completeness.

# sas2r 0.4.1

- Provide exact source-derived macro interfaces to all agents and improve
  guidance for statistical defaults.
- Increase the default shared tool allowance to 30 per agent invocation,
  preserving explicit overrides and run-wide limits.
- Improve library reassignment and relative-path handling, with clearer
  missing-dataset diagnostics.
- Bug fixes for mechanical repair routing and tool-argument handling; report
  effective settings, package versions and tool outcomes more clearly.

# sas2r 0.4.0

- Share each agent's tool allowance across lookups by default.
- Default to two bundle-repair calls per component, subject to run-wide limits.
  Diagnose independent branches separately and rerun the full bundle for acceptance.
- Improve repair selection and reporting, preserving completed execution and
  diagnostic logs when temporary outputs are removed.

# sas2r 0.3.2

- Translate statically called macros from configured search directories, including
  their dependencies, into separate R files with generated interface tests.
  These tests do not establish SAS semantic equivalence.
- Stop before model calls when a called macro cannot be resolved; unsupported
  dynamic and nested macro forms remain explicit findings.
- Add `lib_delete()` for named datasets in writable libraries. Deletion that
  would expose an original input remains unsupported.
- Isolate smoke runs and preserve diagnostics; `keep_raw_attempts = TRUE` also
  retains partial datasets and replay scripts.
- Bug fixes for comparison-report tools, helper metadata, deferred retries,
  macro parsing, quoting and built-in macro recognition.
- Corrected parsing invalidates older scan caches.

# sas2r 0.3.1

- Give fixers structured execution diagnostics and distinguish upstream failures
  from defects in the current component.
- Share documented runtime helper signatures and return values across agents;
  reject unsupported arguments and comparison operators.
- Add source-derived population checks for supported SET and MERGE patterns,
  reporting unsupported behavior as unverified.
- Bug fixes for repair selection, dependency blockers and progress reporting;
  retain stronger evidence and unresolved findings when a repair is rejected.

# sas2r 0.3.0

- Add offline `sas_preflight()` and reusable `qc_profile()` requirements for
  dataset metadata, keys, row counts and comparison tolerances.
- Verify configured model settings before agent work; verification calls share
  the run budget and required settings cannot be silently dropped.
- Improve project reuse, configuration validation, library resolution and output
  planning. Changed scan settings require a source rescan.
- Improve portable exports with selected outputs, a dependency-ordered `run.R`,
  manifests and input guidance. Reports distinguish execution, review and
  reference-validation coverage.
- Bug fixes for SAS missing-value semantics, WHERE filtering, SQL identifiers,
  macro interfaces, dataset dependencies, reference maps and include execution.
  Unsupported MERGE/SQL behavior remains deferred or explicitly rejected.
- Resume preserves selected revisions and reviews, while rerunning execution and
  output checks. Older planning checkpoints and scan caches regenerate.
- Expand documentation and offline semantic fixtures with reference-generation
  scripts; the saved fixture expectations have not been executed in SAS.

# sas2r 0.2.0

- Introduce dependency-aware `sas_translate()` workflows with component and
  bundle repair, output verification and reproducible exports.
- Add editable `autoexec.R` startup configuration and portable library paths.
  Package, document and version the runtime helpers shared with generated bundles.
- Standardize outcomes as `blocked`, `needs_review`, `migration_ready` and
  `validated`, with per-revision evidence and saved audit reports.
- Improve agent progress, tool finalization and ellmer tool-result compatibility.
- The `github` provider is unavailable with ellmer 0.5.0+; it remains registered
  for ellmer 0.4.2–0.4.x following the upstream provider retirement.
- Replace legacy approval workflows and remove the runtime-only `emit_proc_sort`
  alias; previously generated bundles retain their own runtime copies.
- At this release, `code_only` excludes dataset previews, while opt-in bounded
  evidence can contain data values and identifiers. Endpoint privacy and data
  residency still require user assessment.

# sas2r 0.1.0

- Initial release with SAS-to-R translation and integrated dataset comparison.
- Preserve SAS comments as fallback context, with executable source taking
  precedence, and validate generated macro interfaces.
- Add stable provider deadlines, retry accounting and tool-based agent gathering
  followed by structured finalization.
