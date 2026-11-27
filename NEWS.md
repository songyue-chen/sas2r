# sas2r 0.6.2

- Send requests to Anthropic as a stream, so the connection stays active while
  the model works. A network that closes a silent connection after a fixed
  time no longer ends every long request. Requests for native structured
  output are still sent whole. With ellmer 0.5.0 or older, the provider's
  reasoning-token count is not recorded for a streamed request; output-token
  totals are unchanged.

- Report when several model requests fail at the same elapsed time. That
  pattern points to a time limit on the network or at the provider, not to
  random failures, and the run report, the start-here page and the end-of-run
  summary now say so.

- Read a variable no input supplies as missing, and report it, as SAS does.
  A DATA step that reads such a variable does not stop in SAS; it logs
  `NOTE: Variable X is uninitialized.` Generated programs now call the new
  runtime helper `sas_uninitialized()` for that case. Before, a generated
  program either stopped or filled the missing values without saying so. The
  run report, the start-here page and the end-of-run summary list each note
  under "SAS log notes". A name written in the call must be one the SAS source
  uses. Where SAS stops on an absent variable (`BY`, `WHERE`, `KEEP=`,
  `DROP=`, `RENAME=`, procedure variable lists), the generated R still stops.

- Store a SAS date as an R `Date` in the datasets a migration writes. SAS keeps
  a date as a day number with a date format; generated programs still compute
  with that number, so date arithmetic is unchanged. The generated program
  states the format where it writes the dataset, as the SAS `FORMAT` statement
  did: `lib_write(df, "lib", "member", formats = c(visitdt = "date9."))`. A
  column given a date format is stored as a date, and a later program that
  reads the dataset with `lib_read()` receives the day number again. The
  result is the same with or without a reference dataset: a reference is
  compared afterwards and decides nothing. A declaration must be written out
  in the program and must be a format the SAS source gives that variable, or
  the program fails its checks. A new skill, `sas-formats-and-dates`, tells
  the translator, reviewer and fixer how SAS formats are inherited and lost.
  The run report lists each column stored as a date, each declared column left
  as a number with the reason, and each column a reference stores as a date
  that the program does not declare. A reference comparison of a date column
  now checks the calendar date; before, the column failed on type and its
  values were not compared. Datetime and time formats are not covered and stay
  numbers.

- Fix `lib_write()` for a library written as XPT. Every write failed with
  "A provided name contains an illegal character".

- Pair rows on the comparison keys you configure. The migration gate checked
  that configured keys (`comparison_rules$keys` or a target's `keys`
  assertion) were present and then paired rows on keys taken from the source
  or guessed from column names. Configured keys now come first. If they
  repeat, rows are paired inside each key group and are never paired across
  different key values. Inside a group, rows pair on a key from the source or
  from column names when it tells the group's rows apart on both sides, and
  the record then lists the configured keys followed by that key. Otherwise
  the rows of the group are matched by content. The comparison record now
  names the keys rows were paired on and, separately, the keys that were
  configured. A dataset compared with configured keys can get a different
  result than before. `compare_datasets()` is unchanged.

- Reduce repeated model requests during translation, review and repair, and
  keep request content stable between calls so providers can reuse cached
  prompts.

- Give the translator more context for programs that are only partly
  translated by rule. Requests for these programs can be larger, so existing
  request-size or token ceilings may need raising.

- Bring `sas_merge()` and `sas_sort()` closer to how SAS compares and orders BY
  values, including blank and missing values. Both now stop on inputs they
  cannot handle reliably instead of guessing.

- Parse model output more strictly. This raises the minimum jsonlite version.

- Give repair, triage and the run report a clearer description of execution
  failures in generated code, including row-size failures that were previously
  reported as unclassified. A failure is described the same way whether it is
  found when one program is checked or when the whole project is run.

- Remove settings and internal code that had no effect. A configuration file
  that sets `verification` is now rejected, with a message saying what to use
  instead.

- Split SAS statements more faithfully. A semicolon inside a `%str()` or
  `%nrstr()` argument no longer ends the statement, and a marked quotation mark
  such as `%'` no longer hides the code after it. Plain instream data
  (`datalines`, `cards`, `parmcards`) ends at the first line that contains a
  semicolon, as in SAS, so a block ended by `run;` no longer absorbs the rest
  of the file. `datalines4` and its relatives end only at `;;;;`. Sources with
  none of these constructs are split exactly as before. Where an argument or a
  `datalines4` block is never closed, the previous splitting is used and
  preflight lists an advisory `scanner_fallback` finding.

- Check the comparison summary a model may see against its closed format
  before every request that carries it. A summary that does not fit is
  withheld with a warning and recorded in the run diagnostics; the run
  continues.

- Give opt-in AI bug triage (`migration.bug_reports: true`) the evidence to
  judge whether a failed output is the package's doing. For each differing
  column of a failed dataset the request now says whether the SAS source and
  the generated R assign it or only pass it through, which runtime helpers
  handled it, what type sas2r told the translator it had, and whether a type
  difference holds the same values in another representation (a SAS day
  number against a date, for example). It also carries the comparison's own
  decisions (row keys and where they came from, tolerance, file types), who
  wrote each program (rules, model or repair), and difference classes that
  repeat across outputs. A finding now names the package step it suspects.
  These facts are column, program and helper names and fixed words: no data
  values, counts or paths. The compared datasets are read a second time on
  your machine for this. Triage is still one request, and runs with the
  option off are unchanged.

- Show the issue draft for a possible package bug on the start-here page, and
  build it from what the run recorded. For a finding about an output the
  draft gives the output's name, the failed check, the comparison keys you
  configured and the keys rows were paired on, the pairing method and the row
  counts. "Records paired" and "row matching ran to its end" are separate
  facts: matching can finish and still leave rows unpaired. What sas2r
  recorded and what the AI suggested are under separate headings.
  `sas_bug_report()` saves the same draft as a Markdown file. The draft is
  built on your computer from the saved report: it is sent nowhere, asks no
  model and runs no example. A finding saved by an earlier version exports
  what it has and says that the comparison details could not be linked.

- Show more cost detail in the run reports and the printed summary: the share
  of input tokens read from the provider's prompt cache, completed requests and
  tokens for each role, and how many answers were accepted before the tool
  phase ended. `report.json` carries the same figures under `usage`.

- Correct the guides on resuming after a package upgrade. A checkpoint is
  reused only by the sas2r version that saved it, so `resume = TRUE` after an
  upgrade regenerates translations with new model calls. The guides previously
  said a version change alone did not. The behaviour has been this way since
  0.5.5; only the documentation changes.

- Remove the unused `read_comparison_report` agent tool. No translator,
  reviewer or fixer could be given it. A project skill that lists it under
  `tools` is now rejected as naming an unknown tool; delete the entry. The
  `read_comparison_report()` function is unchanged.

# sas2r 0.6.0 beta

- Reconcile late dataset reports against source-bound libraries before deferring
  execution, including literal dataset names passed to SAS macros. When a
  project program writes the same bound member, an existing file alone cannot
  clear the dependency. Set `migration.execution_order` to distinguish a read
  of the original input from a read of the regenerated member. After a declared
  writer, uncertain macro effects are recorded as advisories and the caller
  can execute. Components reuse the source scan's order information.

- Discover providers through the installed ellmer package for translation, review,
  repair and AI diagnosis. Existing provider IDs and configuration remain valid.
  Additional connectors use their ellmer constructor name without `chat_` as
  the provider ID.

- Add opt-in AI triage of possible package bugs (`migration.bug_reports: true`).
  The configured reviewer uses the run budget to explain suspected package
  defects and propose synthetic R examples where possible. `sas_bug_report()`
  writes the saved findings to Markdown with explicit reproduction status.
  The installed sample demonstrates draft export without SAS or an API key.
  Report diagnosis includes its response schema in the prompt for fallback
  JSON providers. Bug triage reports schema failures in the saved run report
  and includes dependency deferrals with the available member evidence.
  Missing-variable failures in generated functions now retain code-confirmed
  function and column names in AI evidence and saved triage reports, even
  when the reviewer finds no grounded package-bug suspicion.

- Reject non-`.pdf` destinations in `sas_print_table()` before opening a device
  or writing a file, including in exported runtimes. Upper- and mixed-case PDF
  extensions remain accepted. In headings and cells each tab becomes one space,
  and LF, CRLF and CR each start a new line. Text outside the PDF renderer's
  printable Windows-1252 encoding raise an error before the file is opened.
  Use a renderer and font that support the source text for these listings.
  Translation, review and repair guidance preserves
  all source-required PDF and non-PDF formats, including RTF, DOCX and HTML;
  changing a filename is not format conversion. Update saved translations that
  used this PDF helper for other formats. Saved bundles retain their embedded runtime;
  regenerate affected bundles before exporting to receive the new guard.
  RTF checks reject mismatched binary formats before attempting text extraction.
- Recognize DOCX report targets in output configuration as well as ODS WORD
  statements. Reports without built-in format validators, including DOCX, now
  require review instead of passing on file existence alone. Their format,
  content and layout remain unverified; this does not add a DOCX renderer.

- Add `sas_print_table()` for plain PDF listings with shared column positions,
  fixed line spacing and repeated page headers. Generated bundles carry the
  same renderer for standalone execution. Translation and review guidance now
  distinguishes broken header/value alignment, clipping and overlap from
  cosmetic styling. Existing saved translations must be regenerated or edited
  to use the helper; arbitrary PDF layout and SAS styling equivalence still
  require review.

- Normalize Gemini Interactions token usage with ellmer 0.5.1, keeping cached
  input, visible output, and reasoning tokens distinct. Exercise both current
  and earlier Gemini protocols in the offline provider contract checks.

- Preserve full relative SAS report paths under `outputs/tlf/`. For example,
  `table.html` and `outputs/table.html` are saved separately. Equivalent paths
  such as `./outputs/table.html` and `outputs/table.html` share one saved file
  and retain their individual contract assessments. Programs that
  consume reports from an earlier 0.6.0 development build should include the
  source `outputs/` prefix in the new path. Regenerate affected bundles and
  rerun them to recover reports that previously overwrote each other.
  Translated macros can now use `sas_output_path()` from a nested function.
- Check source column order for supported literal LENGTH declarations before SET,
  and supply a source-based repair diagnostic when generated columns are appended
  incorrectly. Unsupported declaration forms remain unverified.

- A successful bundle execution now supersedes earlier smoke failures for the
  same executed component binding. Historical failures remain visible, while
  outstanding review findings and output/reference checks still gate readiness.

- Preserve printing and invalid-argument execution diagnostics in reviewer and
  fixer evidence, including `invalid printing width`. Argument values and raw
  logs remain local.

- Give bundle repairs the actual attempt's startup snapshot, execution scope,
  and revision identities, distinguishing attempted code from the current
  selection. Related caller/callee context now carries the same revision labels.
- Retain applied and rejected repair outcomes, diagnoses, and rejection reasons
  in local run diagnostics and reports; make bounded prior outcomes available to
  subsequent repair attempts without treating them as established facts.
- Expand development regression coverage for repair context, execution evidence,
  and successful bundle reconciliation. Preserve the separate CRAN and full
  development test profiles and clarify reference-blind repair documentation.

# sas2r 0.5.8

- Make progress messages suppressible while preserving execution logs, and keep runtime state out of the global workspace.
- Package calls such as top-level `sas2r::lib_read()` now error when only a
  workspace registry is available. Source the bundle's `autoexec.R` into
  `runtime <- new.env()` with `local = runtime`, then use `runtime$lib_read()`.
- Resolve named formats from the bundle catalog and clarify startup and merge behavior.
- Preserve actual execution failures when preparing independent code repairs.
- Align smoke startup with bundle execution and report conflicting format definitions and unsupported widths.
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
