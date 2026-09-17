# sas2r 0.4.5

* Consolidate semantic reviews of earlier unchanged components into a review-only
  checkpoint before bundle execution. Shared-helper repairs check earlier
  consumers locally and reject execution regressions without repeated model
  reviews. Keep initial and actual-repair reviews, current-context evidence,
  source-grounded bundle repairs, and existing repair limits.
* Reuse passing/deferred smoke results only for an unchanged execution context.
  Save component and final-review progress with repair counts so interrupted
  work can resume without repeating completed reviews or resetting allowances.
  Older checkpoint formats regenerate with an explicit explanation.
* Share a source-visible semantic policy across translation, review and repair;
  distinguish harmless representation differences from lost calculations,
  statistics, labels or other required effects.
* Give bundle repair and subsequent full review an explicit integration focus
  using existing bounded executor diagnostics. Reuse completed reviews through
  the evidence history only when their complete request context matches;
  per-attempt log locations and execution IDs do not force another review.
* Explain when a saved review predates request identities and needs refreshing.
  Document upgrade costs: review-only context changes refresh compatible saved
  reviews; shared worker policy, prompt or runtime changes can regenerate
  translations through existing checkpoint invalidation.
* Supply conservative source-only column exclusions for simple, uniquely bound
  KEEP/DROP writers. Unsupported syntax remains unknown and does not suppress
  independent consumer defects or authorize reference-driven changes. Literal
  macro setup outside the writer and unrelated input-library declarations do
  not suppress these facts; indirect writers and output rebinding remain unknown.
* Hash smoke code and outputs by file contents. Report every recorded bundle
  attempt separately from matching current-revision execution and selection,
  including failed attempts and earlier snapshots.

# sas2r 0.4.4

* Assemble named helper edits over the retained runtime and use one candidate
  snapshot for checks, review, execution and handoff. Nested edits retain their
  complete parent function; rejected repairs preserve the retained runtime.
* Investigate missing outputs with observed empty candidate inputs before
  artifact-driven repair, preserving independent source-grounded repairs and
  existing caps. Prioritize attributable execution blockers.
* Pair SAS/R dependency context, record focused versus full review scope and
  recover unavailable full reviews only through completed current full reviews.
* Add registry-based `lib_members()`, source-specific merge/initialization and
  required-effect guidance, advisory nonlocal-assignment notices and explicit
  repair-candidate progress wording. Token summaries use canonical totals and
  report cache/reasoning categories and unknown usage separately.

* Preserve saved translations across installed-package version changes. Apply
  configured package allowlists consistently across worker context and checks,
  and avoid stale prose helper declarations blocking ordinary dynamic calls.
* Reconcile helper declarations from parsed R without rewriting valid code;
  preserve raw declarations for audit. Ordinary trigger-free macro text defaults
  now share one source-owned interpretation across translation and review.
* Give translator, reviewer and fixer shared source guidance, bounded selected
  direct-dependency code and observed package facts. Add optional review finding
  categories with narrow, scoped missing-context handling; independent execution
  failures retain repair paths and all existing budgets remain unchanged.
* Check model-written helper patches before execution/selection, with unchanged
  packaged runtime expressions exempted. The same mechanical correction allowance
  covers both program and helper errors. Changed low-level helpers may need human repair.
* Add registry-aware `lib_exists()`, distinct from reading or counting rows. Extend
  existing rules for distinct nonmissing counts, macro quoting, changing loop
  inputs, dependency delegation and numeric-format limitations.
* Reports retain retry provenance and add advisory dependency-symbol, direct-I/O
  and comparable candidate-file byte-change observations. These observations do
  not prove equivalence; output-change summaries stay out of model requests.
  Generated R is not a filesystem sandbox.

# sas2r 0.4.3

* Keep reference comparison answers out of translation and repair requests and tools. Unexplained differences receive one bounded source-focused review per unchanged component context; only source-grounded findings authorize code repair.
* Reject source-review regressions before bundle repairs are applied, retain coherent shared-helper revisions, and compare individual source/non-reference checks before selecting freshly executed bundles. Correct source translations can be retained despite inconsistent references, whose required failures remain blocked.
* Preserve focused-review history across reference-only configuration changes and resume, and report source evidence separately from unresolved reference differences.
* Retain completed review evidence when an extra review is inconclusive, scope cross-run checks to unchanged sources, and route missing generated intermediates to their source-declared writers. Preserve all artifact errors and rejected revision identifiers, and reuse target observations after focused review.
* Clarify the README privacy boundary: model requests can include source and generated code, comments, project paths, inferred schemas, tool results and execution diagnostics. The default omits dataset previews, but is not automatic de-identification; reference comparison answers stay outside translation, review and repair requests.

# sas2r 0.4.2

* New runs can use their bounded bundle repair allowance when an older run
  remains selected. A worse attempt cannot replace that selection; regressions
  against an attempt selected within the current pipeline still stop repair.
* Bundle repair carries pending static findings forward and can fix a downstream
  code defect after an upstream no-op, without treating unresolved comparisons
  as passed. Rejected mechanical candidates retain their code and diagnostics.
* Fixers get one budgeted correction request for parse/lint failures. Mechanical
  checks catch executable SAS macro calls translated into definitions alone.
* Shared agent guidance covers macro return values, scope, repeated calls and
  source graphics behavior, with executable synthetic acceptance examples.
* Reports distinguish unresolved output expressions from concrete missing files.
  Filename matches alone cannot establish output-family completeness.
* Runtime helper guidance now describes internal return structures, including
  `split_ds()` and library-registry helpers. Translator, reviewer, and fixer
  receive these details through the same packaged helper reference.
* Migration runs have an offline `START_HERE.html`, an editable `bundle/` with
  separate programs, macros, and runtime, saved `outputs/`, `report/`, and
  `diagnostics/`. All available selected code remains accessible, including
  failed code; missing components are explicitly labelled.
* Manual runs write under the existing configurable `.sas2r_output_root`,
  defaulting to `bundle/output/`. `sas_write()` keeps saved automated outputs
  separate under `saved-outputs/`. The generated guide explains running,
  editing, supplying upstream data, and moving the bundle.

# sas2r 0.4.1

- Give translator, reviewer and fixer exact source-derived macro interfaces,
  including empty-string defaults, before they act. Route mechanically invalid
  revisions to repair before semantic review.
- Route focused statistical-default guidance to relevant components, including
  differences between summary calculations and calculations inside plots.
- Resolve relative LIBNAME assignments against the project execution root.
  Preserve generated members and separate output locations across reassignment;
  missing-dataset errors list the searched paths and execution root.
- Increase the shared tool allowance to 30 per agent invocation. Preserve
  explicit overrides, report remaining calls, and close native ellmer gathering
  at the limit while retaining evidence for bounded finalization. Invalid tool
  arguments return corrective feedback; unexpected failures remain visible.
- Record loaded R, sas2r and ellmer versions, effective agent/repair limits and
  component/revision identifiers for tool outcomes. Completion messages identify
  denied and failed lookups separately from request completion.

# sas2r 0.4.0

* Share each agent's tool allowance across lookups by default, while preserving
  explicit per-tool and run-wide limits.
* Give each component two bundle repair calls by default. The optional overall
  cap remains a hard limit. Diagnose independent branches in isolation, queue
  known failures by cause, and retain full fresh bundle execution for acceptance.
* Preserve completed execution when selecting repaired bundles and report repair
  counts, deferrals and isolated diagnostic evidence.
* Keep isolated diagnostics with their owning bundle attempt and retain their
  logs when temporary execution outputs are pruned.

# sas2r 0.3.2

* Deliver validated comparison reports as JSON-compatible tool results, fixing
  failed `read_comparison_report` calls during agent repair.

* Reconcile runtime helper use against resolved macro interfaces and refresh
  metadata after repairs. Unknown declared helpers still fail mechanical checks.
* Retry deferred components only after relevant changes, share the immediate
  repair allowance across revisits, and reuse unchanged completed reviews.
* Read revision code consistently during smoke planning. Preserve multiline
  calls and defer calls that need their enclosing program's execution context.
* Add `lib_delete()` for explicit dataset names in writable libraries. Deletion
  that would expose a separate original input remains explicitly unsupported.
* Isolate smoke executions and retain their output hashes and diagnostics.
  `keep_raw_attempts = TRUE` also preserves partial datasets and replay scripts.

* Translate statically called macros from configured macro search directories,
  including their transitive macro dependencies, before their calling programs.
  Uncalled definitions remain outside the translation plan, including definitions
  sharing a library file with a called macro.
* Emit each called macro as `R/macros/<name>.R` with a generated interface test
  under `tests_macros/`. Bundle startup loads these functions for callers;
  standalone files contain only their function definition. Export preserves
  macro files and tests. Interface tests do not establish SAS semantic equivalence.
* Preflight lists called macro names, source files and planned R paths. Caller
  agents receive translated upstream contracts. Dynamic calls, missing macro
  definitions, nested definitions, macro-body includes and library files requiring
  top-level initialization remain explicit unresolved findings.
* Translation stops before model calls when a called macro cannot be resolved,
  reporting names, source locations, searched folders and configuration guidance.
  Preflight still returns its findings for inspection.
* Recognize `%QSYSFUNC` and the built-in macro execution/existence functions
  during dependency mapping instead of reporting them as missing user macros.
  Include documented NLS macro names, using `%QKLOWCAS` for quoted lowercase.
* Stop with a source location when an unterminated macro comment would hide
  subsequent code. Preserve SAS's matched-quote and quoted-semicolon rules.
* Parse macro parameter lists up to their matching closing parenthesis so
  description options do not corrupt the interface. Preserve nested defaults
  and quoted paths. Invalidate older scan caches for the corrected parser.
* Distinguish macro labels and percent-prefixed SAS statements from user calls.
  Preserve real calls in double-quoted text and macro defaults; mask simple
  `%NRSTR` literals. Quoting that needs expansion and active macro text in SAS
  statement comments produce explicit analysis findings, not missing-file errors.
  Computed names such as `%prefix&suffix` remain dynamic.

# sas2r 0.3.1

* Fixers receive bounded, structured smoke and bundle diagnostics, including the
  underlying R condition, exit status and log paths. Dependency failures name
  the failing upstream program and block consumer repair for that crash.
  Call locations are formatted once and missing calls remain absent. Smoke
  blockers distinguish upstream dependencies from local failures and reconcile
  when execution is rerun.
* All three agents receive a shared runtime reference generated from helper
  documentation and actual signatures. Mechanical gates reject unsupported
  helper arguments even when the agent omits its helper declaration.
  A missing reference reports an actionable installation error.
  The shared reference includes argument rules and return values for all
  documented helpers. `chr_cmp()` documents equality as `op = "=="` and rejects
  unsupported operators instead of silently producing incorrect flags.
* Source-derived population checks detect row loss in supported SET and MERGE
  patterns during both smoke and bundle execution. Unsupported source behavior
  and unavailable intermediates remain explicitly unverified.
  Incompatible source BY types remain unverified with diagnostics; output key
  type changes fail with a structured population mismatch. Factor labels and
  integer/double representations use the same BY comparison rules.
* Repaired candidates cannot replace revisions with better mechanical, runtime,
  review or source population evidence. Rejected candidates retain their history
  and the selected revision retains unresolved review findings.
* Progress names review verdicts, underlying execution errors and a retained
  older bundle after a blocked run. Exact attempt lookup avoids the spurious
  missing `attempt_dir` warning caused by partial matching against `attempts`.

# sas2r 0.3.0

### Model settings

* Translation now verifies explicit model settings before agent work, probes
  unknown reasoning support with an invalid level and the requested level, and
  caches successful checks per adapter and exact deployment/settings profile.
  Probe attempts share the translation budget and audit trail; offline preflight
  stays offline. Required settings cannot be silently dropped by ellmer or
  removed during a capability retry.
* The connectivity probe now sends configured reasoning, sampling and output
  settings. Recommended provider profiles use automatic verification instead of
  manual reasoning/output capability flags.

### Preflight and QC

* Add `sas_preflight()` to inspect source/library resolution, input availability,
  deterministic limitations, outputs, and effective budgets with zero model
  calls and no migration artifacts.
* Add reusable `qc_profile()` output requirements for labels, formats, types,
  column order, keys, uniqueness, row counts, and variable-specific tolerances.

* Partial configuration lists on reused projects update supplied top-level fields
  without discarding library, output, or provider settings. Explicitly changing
  or clearing scan settings still requires rescanning the source path.
* Configured TLF references that are missing or directories now fail the output
  gate with their paths retained in diagnostics. Existing TLF references remain
  uncompared and do not count as reference-validation evidence.

* Preflight and translation now share normalized configuration and a reusable
  output/dependency plan. Relative references remain stable across project reuse,
  global reference fallbacks are inspected, and invalid configuration fails
  before migration/cache writes. Changed scan settings require a source rescan.
* QC profiles inherit omitted fields from global rules; explicit false/empty
  fields override them. Keys guide alignment unless uniqueness is requested.
  Validate inherited uniqueness requirements, accept factor types and scientific
  row counts, preserve YAML metadata keys, and require `true`/`false` booleans.
* Dataset detection distinguishes names from expressions and uncalled macro
  bodies. Preflight reports unknown WORK producers and backward dependencies;
  duplicate filenames retain separate components. Graph and preflight share
  library-aware producer selection without redundant historical write edges.
* JSON and Markdown output assessments share reasons and distinguish unavailable
  checks from failures. Resume explains incompatible checkpoints before new
  provider calls; checkpoints from the previous planning policy are regenerated.

* Execute documented offline R and YAML examples against the installed package
  in CI. Add a complete SAS fixture collection verifier; actual SAS execution
  remains pending because no SAS runtime is available.

* README and evidence guides now distinguish agent completion, semantic review,
  execution, and reference coverage. Examples use library-qualified output paths
  and the correct comparison APIs; export, resume, and reference-provenance
  guidance reflects the current migration workflow.

### Migration and runtime

* Mechanical interface checks compare declared parameters with the component's
  named function, never an unrelated helper. Scripts with external path inputs
  no longer fail because they define a helper such as `is_not_missing(x)`;
  explicit SAS macro interfaces remain enforced.

* Program reviews receive the component's actual SAS source on initial and repair
  passes. Translator prompts prohibit package attachment and mechanical retries
  include the failed code and named lint findings. Persistent mechanical failures
  remain blockers, with their details visible in progress and reports. Smoke
  results are recorded independently of unavailable reviews.

* Deterministic translation preserves quoted values, applies WHERE to input rows,
  and uses SAS missing-value comparisons in predicates and numeric derived flags.
  SQL identifiers follow the same case folding as input frames.
* MERGE bodies and PROC SQL units with unsupported extra statements defer as a
  whole. Runtime MERGE refuses duplicate keys with shared non-key columns.
* `sas_translate(usage_limits = ...)` enforces the canonical request limits and
  reports invalid configured LLM providers instead of silently disabling them.
* Resume saves the actual selected revision records and their independent reviews.
  Unchanged resume avoids repeated provider calls but reruns execution and output
  checks. Input hashes now include the actual configured library data.
* `outputs_dir` contains all selected outputs under their library/relative paths.
  `sas_write()` rebuilds the destination registry and includes all generated files,
  a dependency-ordered `run.R`, output manifest, and dependency/input guide.
* Reports and printed summaries separate produced, reference-compared, passing,
  and reference-passing targets from independently reviewed components. They list
  contributing validation targets, elapsed time, effective limits, and known,
  estimated, billed, and unknown-cost usage. Deliberately skipped execution is
  reported as deferred; unavailable review is not reported as completed.
* Added 14 semantic fixtures with independent documented expectations and a SAS
  reference-generation script, plus public regressions for budgets, resume,
  repaired revisions, changed inputs, moved exports, and seeded output defects.
  The saved expectations have not yet been executed in SAS.

* Project reuse anchors source files and harvested SASAUTOS paths. Equivalent
  library ordering and directories created after scanning do not require a
  rescan; explicit NULL clears output requirements. Unknown R configuration
  keys and invalid output paths fail before writes with classed errors.
* Reference maps use case-insensitive dataset keys, reject duplicates and unknown
  comparison targets, and prefer per-target references over the global fallback.
  Contracts persist resolved references as an array of records with full numeric
  precision. Directory references fail with an actionable explanation.
* Called macro data flow and unsupported dataset statements remain visible as
  deferred findings. APPEND bases may be created if absent; unbound libraries
  retain producer ordering while their inputs remain unresolved.
* Scan-cache schema 4.0 replaces older entries; successful scans prune obsolete
  content entries. Preflight never writes macro indexes. Configuration accepts
  one YAML document, validates named mappings, and warns about the unused
  legacy tolerance field.
* Bundle snapshots retain staged include paths and execute included files at
  their call sites. Resume ignores transport-only changes and removes stale
  invalidation messages after successful reuse. Older checkpoints regenerate
  under the updated planning policy.

### Provider documentation

* DeepSeek examples use the current `deepseek-flash` API name, with links to
  the provider's model list and guidance on legacy aliases.

# sas2r 0.2.0

### Runtime
* **`autoexec.R`: one file a person maintains, and a short program header**: every bundle now carries `autoexec.R` in place of `_sas2r_registry.R` -- the bundle's counterpart of a SAS autoexec, generated by the translation and yours to maintain afterwards. Its LIBRARIES section lists each libref's `read_path`, `write_path`, `engine`, and `write` as plain strings, where a relative path means inside the bundle folder and an absolute path is used as is, so a run folder moved or renamed as a whole keeps working and only moved source data needs an edit; its LOAD section sources `sas2r-helpers.R`, resolves those paths with the new exported helper `sas2r_resolve_registry()` (which also refuses an uncommented `<FILL>` placeholder with the libref named), and sources `_sas2r_formats.R`. A translated program opens with one guarded line, `source("autoexec.R", chdir = TRUE)`, instead of a 55-line block that located and loaded three files inline: a program runs from its folder, or after `autoexec.R` was sourced once in the session, exactly as a SAS program runs with its autoexec; run from an unrelated working directory before that, it fails on that first line, and the comment above it says what to do. The environment model is unchanged: a program sourced into a sandbox keeps its runtime there. Sourcing `autoexec.R` by hand once per session, with `chdir = TRUE`, is the R counterpart of running a SAS autoexec -- programs then find the runtime loaded and skip their own bootstrap. The bundle executor loads a bundle through its `autoexec.R` too.
* **One runtime, two delivery forms**: the helpers every translated program calls now live in `R/runtime-*.R` as package code -- 21 of them exported with a help page and runnable examples each (`?lib_read`, `?sas_merge`, ...) -- and `inst/templates/sas2r-helpers.R`, the copy every bundle carries, is rendered from those files. Tests fail if the vendored runtime differs from the package's by a single function or if the template is stale. Registry lookup goes through `sas2r_registry_env()` so the same code serves a bundle, a test harness, and the console. The runtime's `emit_proc_sort` compatibility alias is removed: it collided with the package's PROC SORT emitter of the same name, and no generated code ever called it (earlier bundles keep their own frozen copy).
* **The runtime helpers are documented and versioned**: `?sas2r_runtime` documents every function a translated program's `sas2r-helpers.R` carries (each helper name, `?lib_read` for instance, is an alias of that topic), `vignette("runtime-helpers")` explains how to run programs and use the runtime interactively, every vendored `sas2r-helpers.R` now states the sas2r version that generated it, and a stability policy is in force.

### Agents
* **The console says which agent is working, and on what**: progress lines now read `reviewer  demo (r1, round 1): reviewing`, `reviewer  demo (r1, round 1): ok, 3 tool calls`, `coordinator  demo (r1): mechanical checks passed`, `smoke  demo: failed [smoke_attempt_001] -- Dataset not found: work.stg1`, `bundle  round 1: bundle_attempt_001 assessed -- blocked`, instead of one `coordinator 1/1` per lifecycle event. the agent runner signals a `sas2r_agent_event` condition when an agent starts and when it finishes, carrying its audit context, and the renderer draws an identical consecutive line once.
* **The model is told when its tools close**: the finalization request -- structured output, which ellmer cannot combine with registered tools -- now says "the tools are now closed and cannot be called again" before asking for the final answer, on both routes into it. A model that had just been calling `lookup_rulebook` would otherwise ask for one more and get ellmer's "Unknown tool" warning and a wasted round trip.
* **Tool result serialization at ellmer boundary**: Tool results handed to `ellmer` are now serialized to JSON using `jsonlite::toJSON(auto_unbox = TRUE)` at the tool-wrapper boundary. This preserves wire format byte-identically across both `ellmer` 0.4.2 and 0.5.0+ while preventing deprecation warnings on `ellmer` >= 0.5.0 where complex list returns are deprecated.

### Providers
* **GitHub Models retired upstream**: `ellmer` 0.5.0 made `chat_github()` and `models_github()` defunct (GitHub Models was retired on 2026-07-30). The `github` provider stays registered for `ellmer` 0.4.2-0.4.x; on newer `ellmer`, `sas_llm()` and `sas_llm_models()` refuse it with a `sas2r_llm_provider_retired` error before any request is built, and the real-ellmer contract verifies that refusal instead of exercising the retired provider.

### Dependency-Aware Migration Architecture
* **Single Authoritative Entry Point**: `sas_translate()` orchestrates end-to-end migration of single SAS programs or complete multi-file pipelines into modular, reproducible R bundles.
* **Dependency Graph & Topological Scheduling**: Automatically parses `%include` trees, macro calls, and dataset lineages to construct an authoritative Directed Acyclic Graph (DAG) and deterministic topological execution schedule.
* **Copy-on-Write Attempt Isolation**: All execution attempts execute inside isolated candidate directories (`attempts/bundle_attempt_NNN/`) with copy-on-write library containment, ensuring input datasets are never mutated.
* **Dual Repair Loops**:
  - *Immediate Component Repair*: Fast-feedback loop fixing syntax, linting, and initial reviewer findings on individual translation units.
  - *Bundle Causal Repair*: Multi-round causal repair loop diagnosing execution and output discrepancies across the whole dependency graph.
* **Four Authoritative Bundle Statuses**: Standardized gate statuses (`blocked`, `needs_review`, `migration_ready`, `validated`) indicating artifact completeness and validation depth.
* **Component Evidence Ladder**: Immutable evidence tracking per revision binding (`transpiled_only`, `reviewed_only`, `smoke_verified`, `runtime_verified`).
* **Output Verification Gate**: Evaluates candidate datasets and Table/Listing/Figure (TLF) outputs against formal structural contracts and optional SAS reference datasets.
* **Deterministic Selection & Reporting**: Selects non-regressive attempts and exports machine-readable (`.sas2r/report.json`) and Markdown (`report.md`) audit documentation.
* **Model Privacy & Data Residency**: By default only SAS source code, macro structures, column name metadata, and redacted comparison digests reach remote models — never the mismatching cells. Bounded, capped opt-in surfaces (the reviewer's comparison report examples and `agent_evidence = "bounded"` previews) may carry row numbers, key values, differing cell values, and subject identifiers; the default `code_only` policy sends neither. Users remain responsible for confirming their configured endpoint meets enterprise data residency obligations.
* **Removed Legacy Approvals & Synthetic Ladder**: Replaced legacy approval ledger commands and single-unit synthetic ladders with the dependency-aware migration pipeline and output verification gate.

# sas2r 0.1.0

* Worker prompts now receive retained SAS comments only as fallback evidence.
  Comments between translation units attach forward to the next unit; executable
  SAS takes precedence, and source approval identity remains code-only.

* Generated macro functions now deterministically validate the current macro's
  public interface before emission.

* Ellmer HTTP deadlines and retry counts now have stable sas2r defaults. The
  usage ledger records the absolute-attempt policy, request duration, and safe
  terminal failure class/reason without copying provider error text.

* Ellmer-backed agents now always gather with registered tools before a
  tool-free structured finalization request. A generic downgrade-only
  transport constraint prevents model or project capability overrides from
  bypassing this public ellmer limitation; provider cache-token accounting and
  stateful tool wrappers are unchanged.

Initial release of `sas2r`, providing transparent, verifiable SAS-to-R translation for regulated analytics environments with integrated dataset comparison capabilities.
