# Code review: `feat/clinical-qc-preflight`

**Target:** branch `feat/clinical-qc-preflight` at `50e265f` ("feat: add offline preflight and clinical QC profiles"), diffed against its merge-base with `main` (`6a0f2b7`). 27 files, +1295 / −177.
**Method:** nine independent finder angles (line-by-line, removed behaviour, cross-file, R pitfalls, wrapper correctness, reuse, simplification, efficiency, altitude), one verifier vote per candidate, then a fresh gap sweep. Every finding marked CONFIRMED was reproduced by running the branch under `pkgload::load_all()`, in most cases side by side with the merge-base. No files were edited.
**Date:** 2026-09-12

## Summary

The feature is well tested for the shapes its own tests use, but two design choices leak into behaviour a study would hit on day one:

1. **Global `comparison_rules` are now merged wholesale into the per-dataset QC assertions** (`R/migration-gate.R:232-234`). That turns a study-wide `keys:` alignment hint into a hard "keys present" requirement on every dataset target (findings 1), makes any malformed global rule abort the migration inside the output gate after model spend instead of at config load (2), and lets a profile's materialised defaults silently override global tolerances and uniqueness (8).
2. **Preflight's readiness verdict is derived from the wrong signals**: a regex over scanner flag kinds that both over-matches (a within-file `dependency_cycle` artefact blocks ordinary single-file programs, 5) and under-matches (four "cannot resolve" kinds are missed, 11), while inputs referenced through macro variables leave no trace at all and produce a clean bill of health (4).

Fifteen correctness findings survive verification, plus two narrower correctness items, fourteen design/duplication items, five efficiency items, and one test-coverage gap. The list below is ranked most severe first within each section.

---

## A. Correctness (confirmed)

### 1. Global `comparison_rules$keys` now fails every dataset target that lacks the key columns
- **Where:** `R/migration-gate.R:232-234` (merge), `R/qc-profile.R:172-176` (`keys_present`).
- **What:** `qc_assertions <- comparison_rules; qc_assertions[names(assertions)] <- assertions; checks <- c(checks, check_dataset_qc(cand_data, qc_assertions))` is unconditional. At merge-base the global key was read only inside the reference-comparison branch (`assertions$keys %||% comparison_rules$keys`, line ~302) to steer row alignment.
- **Reproduction:** `comparison_rules = list(keys = c("STUDYID","USUBJID"))`, target `adam.summary` with columns `TRT, N`, no reference, no assertions. Branch: `passed = FALSE, status = "failed", reason = "keys_present: Missing key columns: studyid, usubjid"`. Merge-base: `passed = TRUE`. A required summary target flips the whole migration gate. No test exercises `comparison_rules$keys`.
- **Fix direction:** pass the gate only the QC-shaped subset of global rules (or make global defaults a named profile), and keep `keys` alignment-only unless a target opts into `unique_keys`.

### 2. Unvalidated `comparison_rules` now abort the migration inside the output gate, after model spend
- **Where:** `R/qc-profile.R:125` (`validate_qc_assertions` inside `check_dataset_qc`), reached from `R/migration-gate.R:232`; call chain is bare: gate `:767` → `R/orchestrate.R:762` → `R/translate.R:175`, and it runs after `run_bundle_attempt`.
- **What:** `comparison_rules` is stored raw (`R/config.R:402`) and never validated at load. The old gate coerced (`tolower(as.character(req_cols))`, `as.integer(min_rows)`) or ignored the field.
- **Reproduction (branch aborts, merge-base passes):** `required_columns = list("USUBJID")`, `required_columns = c("USUBJID","usubjid")`, `tolerances = list(AGE = 0.01)` (scalar, `rlang_error`), `min_rows = "1"` (string from YAML), `keys = list("USUBJID")`, `unique_keys = TRUE` without keys. Via `assess_final_outputs()` the error propagates out of the run. Also: `validate_qc_assertions` builds and discards a `compare_profile()` on every candidate evaluation.
- **Fix direction:** validate `comparison_rules` once at config load with the same schema as profiles, and never let the gate throw for a config shape.

### 3. A `qc_profile()` object used directly as a target assertion crashes `sas_translate()` but passes `sas_preflight()`
- **Where:** `R/qc-profile.R:114-119` (`unclass()` only in the `profile =` branch); crash at `R/translate.R:118` → `R/output-contracts.R:641` (`jsonlite::toJSON(unclass(contracts))`, outer `unclass` strips only the tibble class).
- **Reproduction:** `outputs = list(assertions = list("adam.adsl" = qc_profile(keys = "USUBJID")))`, the shape `tests/testthat/test-qc-profile.R:120-121` uses. `sas_preflight()` → `ready_for_translation`; `sas_translate(execute = FALSE)` → `No method asJSON S3 class: sas2r_qc_profile`. The docs tell users to reuse preflight's arguments in translate.
- **Fix direction:** unclass (or `as.list`) every assertion in `resolve_qc_profiles`, or drop the class entirely (see B.5).

### 4. Inputs referenced through macro variables vanish from `inputs` and the run is reported `ready_for_translation`
- **Where:** `R/preflight.R:84` (`reads <- lineage[lineage$role == "reads", ]`), status at `:63`.
- **What:** the scanner records no `reads` row and raises no flag for `set &lib..dm;`, `set raw.&ds;`, `proc sort data=&lib..dm`. Preflight has nothing to resolve.
- **Reproduction:** `%let lib=nowhere; data adam.a; set &lib..dm; run;` with no such library → `status = "ready_for_translation"`, `nrow(inputs) == 0`, no findings, `adam.a` listed as an output. The only trace is an `unsupported` row with reason `macro_residue`, which the docs classify as advisory. The `&sdtm..dm` idiom is ubiquitous in clinical programs.
- **Fix direction:** treat `macro_residue` in a dataset position as an unresolved input, or have the scanner emit a flag for unresolved dataset references.

### 5. The `dependency_cycle` flag is a within-file artefact, so ordinary single-file programs never reach `ready_for_translation`
- **Where:** `R/preflight.R:51` (`grepl("...|cycle|...", findings$kind)`), `:52`, action at `:73`.
- **What:** `R/dependency-graph.R:53-58` makes components per file; `:688-691` records a self-loop for any edge whose ends are in the same file; `:760` calls that a cycle; `R/project.R:792-796` raises `dependency_cycle`. So "step 1 writes `adam.adsl`, step 2 reads it" in one file is a "cycle".
- **Reproduction:** `data adam.adsl; set raw.dm; run; proc sort data=adam.adsl; by x; run;` with `raw/dm.rds` present → `needs_attention` + "Resolve the reported include, macro, or dependency findings". The same two steps in two files → `ready_for_translation`. Also triggered by a `work.stage` hand-off and by defining and calling a macro in one file. `tests/testthat/test-preflight.R:6-8` has this shape and passes for a different reason (`raw.ae` missing), masking the artefact.
- **Fix direction:** do not treat `dependency_cycle` as blocking (it is informational and already resolved by "using file order"), or make the scanner distinguish real cross-file cycles.

### 6. A non-scalar `label`/`format.sas` attribute on a candidate column aborts the whole run at the gate
- **Where:** `R/qc-profile.R:149-150` (`vapply(..., character(1))` over `as.character(attr(v, ...) %||% NA_character_)`); no `tryCatch` above it (gate `:234`, `:767`; `R/orchestrate.R:762`; `R/translate.R:175`).
- **Reproduction:** `attr(df$AGE, "label") <- c("Age", "years")` with a profile `labels = c(AGE = "Age")` → unclassed `simpleError: values must be length 1, but FUN(X[[1]]) result is length 2`; same for a length-0 attribute. Exposure: RDS or in-memory candidates (whatever generated R code set); haven paths yield scalar labels.
- **Fix direction:** take `[1]` / `paste()` the attribute, and fail the check rather than the run.

### 7. Budget/pricing argument validation now runs after side effects, including a scan cache written into the user's source tree
- **Where:** `R/translate.R:126` calling `translation_budget()` (`R/translation-setup.R:30-43`), after `dir.create(paths$state)` (`:102`), `sas_project(cache = TRUE)` (`:111`), `write_output_contracts()` (`:118`), `atomic_write_json(graph)` (`:122`). At merge-base (`R/translate.R:93-108`) the checks ran before "1. Output directory setup".
- **Reproduction:** `sas_translate(f, out_dir = out, budget_mode = "typo", execute = FALSE)` → same error class, but now `out/.sas2r/graph.json`, `out/.sas2r/output-contracts.json` and `<source dir>/.sas2r/scan_cache.rds` exist; merge-base created nothing. With `resume = TRUE` the prior run's graph and contracts are rewritten before the abort. `tests/testthat/test-translate.R:71-74` asserts only the condition class.
- **Fix direction:** call `translation_budget()` (or at least its validation half) before step 1; add `expect_length(list.files(out, recursive = TRUE, all.files = TRUE), 0)` to the test.

### 8. A selected profile silently drops global tolerances and uniqueness that a plain assertion inherits
- **Where:** `R/qc-profile.R:36-37` (`as.list(environment())` keeps `unique_keys = FALSE`, `tolerances = list()`), `R/migration-gate.R:309` (`assertions$tolerances %||% comparison_rules$tolerances`, and `list()` is not `NULL`), `:233` (`unique_keys = FALSE` overwrites a global `TRUE`).
- **Reproduction:** reference `AGE = 50, 60`, candidate `50.5, 60.5`, `comparison_rules = list(tolerances = list(AGE = list(abs = 1)), keys = "USUBJID", unique_keys = TRUE)`. Plain `assertions = list(required_columns = "USUBJID")` → reference passes and duplicate keys are caught. Same target selecting `qc_profile(required_columns = "USUBJID")` → reference fails (tolerance lost) and no `keys_nonmissing`/`unique_keys` check is emitted. A hand-written YAML profile that omits the two fields inherits the globals, so R profiles and YAML profiles behave differently (see B.5).
- **Fix direction:** make `qc_profile()` drop unset fields (or treat `list()`/`FALSE` defaults as "unset" in the merge), and encode override precedence once (B.4).

### 9. YAML-typed keys and values in `labels`/`formats`/`types` pass validation and produce expectations that can never match
- **Where:** `R/qc-profile.R:57` (`unlist()` of a mixed list coerces to character), consumed at `:151` (`actual == expected`).
- **Reproduction:** `formats: {AVAL: 8., ADT: DATE9.}` loads with `AVAL` as numeric `8`; `labels: {N: Count}` loads with key `FALSE` (YAML 1.1 booleans: `N/Y/n/y/no/yes/on/off`). A candidate carrying exactly `format.sas = "8."` and `label = "Count"` on `N` fails `formats` and `labels` on every round. With only numeric values (`formats: {AVAL: 8.2}`) the config instead hard-errors at load, so behaviour depends on whether a string sibling is present. `w.d` formats are the most common SAS formats and the docs show the unquoted style.
- **Fix direction:** reject non-character values and non-character keys before `unlist()`, and document quoting.

### 10. A misspelled inline assertion key is silently dropped, while the same typo in a profile aborts
- **Where:** `R/qc-profile.R:97` (`assert_exact_names` runs only over `profiles`) vs `:117` (`validate_qc_assertions(a)` checks values of known fields only).
- **Reproduction:** `assertions: adam.adsl: {row_cout: 306}` (R or YAML) → accepted, target passes with `candidate_exists, candidate_readable` only. `profiles: subject: {row_cout: 306}` → "Unknown or unsupported field". A target that selects a profile still leaks typos (`profile: subject, row_cout: 306` accepted).
- **Fix direction:** apply one per-kind assertion schema (dataset: `qc_profile` formals + `profile`) to every target.

### 11. The readiness regex misses four "cannot resolve" scanner kinds
- **Where:** `R/preflight.R:51`.
- **What:** `libref_undeclared` (write-only use), `libref_engine_unsupported` (write-only), `dynamic_include`, `include_depth_exceeded` (`R/project.R:827, 836, 537, 569`) do not match `missing|unresolved|ambiguous|cycle|truncat`; `ambiguous` matches no kind at all. Read-side libref cases are caught only incidentally through `inputs$status == "unresolved"`.
- **Reproduction:** `data sdtm.out; set raw.dm; run;` (no `sdtm`) → `ready_for_translation`, finding `libref_undeclared`, no actions. `%include &prog;` → `ready_for_translation`, and the target is never scanned so its inputs are absent. Include chain deeper than 10 → same.
- **Fix direction:** give flags a `blocking` property where they are raised, and let preflight read it.

### 12. A factor column always fails a `types` requirement that the comparator in the same gate satisfies
- **Where:** `R/qc-profile.R:146` (`class(v)[1L]` → `"factor"`); comparator `R/normalize.R:11` (`is.factor → "character"`), `R/row-alignment.R:153-155` (`factor_to_character`).
- **Reproduction:** candidate `USUBJID = factor(...)`, character reference, `types = c(USUBJID = "character")` → `reference_passed = TRUE`, `checks$types$passed = FALSE`, `status = "failed"`. No admissible `types` value (`:62-63`) accepts a factor.
- **Fix direction:** derive the QC vocabulary from `col_kind()` and refine integer/double with `typeof()`.

### 13. YAML `abs: 1e-6` in `tolerances` is rejected while `numeric_tolerance: 1e-6` is accepted as a string
- **Where:** `R/qc-profile.R:83-84` → `R/profile.R:61-66` (coerces character scalar `abs`) vs `:95-104` (rejects character `overrides$abs/rel`).
- **Reproduction:** the `yaml` package parses `1e-6` (no decimal point) as `"1e-6"`. `tolerances: {AGE: {abs: 1e-6, rel: 0}}` → "Override `abs` for variable "age" must be a single non-negative number" at config load; `numeric_tolerance: 1e-6` → accepted, stored as a string, coerced later at `R/migration-gate.R:292`. The docs example uses `0.01` and is unaffected; the vignette's `numeric_tolerance: 1e-6` is on the accepted path.
- **Fix direction:** coerce numeric-looking strings in `compare_profile()` overrides the same way scalars are coerced, or reject both consistently with a message that names the YAML rule.

### 14. A `work.*` read with no known producer is reported as a missing file with an empty search list
- **Where:** `R/preflight.R:88, 99-106`; action at `:70`; vocabulary at `:13` and `docs/clinical-qc-preflight.md:66-74`.
- **Reproduction:** `data out; set nowhere; run;` → `work.nowhere | <session work> | missing`, `searched_paths = character()`, next action "Supply missing input members or correct their library paths; inspect $inputs$searched_paths". A dataset created inside a macro invocation (`%macro mk; data work.stage; ...; %mend; %mk;`) hits the same path because lineage has no `creates` row for the macro body. The docs define `missing` as a missing file; there is no "no known producer" status.
- **Fix direction:** add a status such as `no_producer` with its own action, and skip the file remedy for `<session work>`.

### 15. A named `column_order` vector can never pass
- **Where:** `R/qc-profile.R:159` (`identical(columns, tolower(assertions$column_order))`; `tolower()` keeps names; `:49-52` neither strips nor rejects them).
- **Reproduction:** `qc_profile(column_order = c(subj = "USUBJID", age = "AGE"))` against columns `USUBJID, AGE` → `passed = FALSE` with `details` showing identical expected and actual lists. R-only shape.
- **Fix direction:** `unname()` in validation, or compare with `identical(unname(...), ...)`.

### 16. An empty `labels`/`formats`/`types` mapping is rejected as malformed
- **Where:** `R/qc-profile.R:57-58` (`unlist(list())` is `NULL`), fed by `:39`.
- **Reproduction:** `qc_profile(labels = character())`, `qc_profile(labels = list())`, YAML `labels: {}` and `formats: {}` → "QC labels must be a named character mapping"; the vector fields (`required_columns = character()`) accept empty input.
- **Fix direction:** treat an empty mapping as "no requirement".

### 17. Preflight's `unsupported$reason` uses a different vocabulary from the transpile manifest for the same unit
- **Where:** `R/transpile.R:877-883` (`deterministic_unit_translation` covers only data/proc), `:628-636` (condition → reason mapping stays in `transpile_source_file`), `R/preflight.R:134` (`error = function(e) conditionMessage(e)`), `:130` (`macro_deferred` duplicated), `:132-133` vs `:639-644` (flag derivation differs).
- **Reproduction:** `x = scan(name, 2);` → preflight ``unmapped SAS function in expression: `scan` ``, manifest `unmapped_function:scan`; `y = a @@ b;` → preflight ``unsupported syntax or characters in expression: `a @@ b` ``, manifest `expr_parse_failed`.
- **Fix direction:** move the `tryCatch` and reason derivation into the shared function so both callers get the ledger vocabulary.

---

## B. Design, duplication, and simplification (confirmed unless marked)

### B.1 `outputs.references` are resolved against the working directory, contrary to the documented config rule
- `R/preflight.R:44-45` reports `config_resolve_paths(..., getwd())` and checks `file.exists()` on the unresolved string; `normalize_outputs_config(raw_outputs, config_file)` (`R/config.R:319-322`) never uses `config_file`; references are stored verbatim (`R/output-contracts.R:215-216`). `R/config.R:343-345` promises every relative configured path resolves against the config file's directory. Live: `adsl: refs/adsl.rds` beside `_sas2r.yml`, run from another cwd → preflight reports `missing`. Pre-existing for the gate; preflight is the first place a user sees it.

### B.2 Free-text `reason` exists only for dataset targets that reach the final return
- `R/migration-gate.R:367-375` joins a reason from checks; `assess_tlf_target` (`:400-706`) returns none, and the dataset early returns at `:163` (missing candidate) and `:206` (unreadable) also omit it; `R/migration-report.R:283` prints `ass$reason %||% "(none)"` for every target. A TLF with no candidate prints "(none)" although `checks$candidate_exists$details` holds the answer.

### B.3 `deterministic_unit_translation()` is shared, but its failure mapping is not (see A.17)

### B.4 Override precedence is encoded twice in the gate
- `R/migration-gate.R:232-233` (`x[names(a)] <- a`) versus `:289`, `:302`, `:309` (`a$f %||% g$f`). They agree only while no assertion holds `NULL` or an empty default, which is exactly what A.8 breaks.

### B.5 YAML profiles and R profiles have different shapes, and the `sas2r_qc_profile` class does nothing
- `R/qc-profile.R:94-116` validates YAML mappings by hand while R profiles come from `qc_profile()` (always carrying `unique_keys = FALSE`, `tolerances = list()`, list-coerced mappings). `methods(class = "sas2r_qc_profile")` finds nothing; the class is stripped at `:114` and is the direct cause of A.3. `profiles[[name]] <- do.call(qc_profile, p)` would give one canonical shape. Secondary: `R/output-contracts.R:172` `length(assertions) > 0L` treats a bare `qc_profile()` (length 2) and a YAML `{}` (length 0) differently.

### B.6 Redundant guard with three different error classes in one function
- `R/qc-profile.R:96` unclassed abort duplicates `assert_exact_names()`'s first predicate (`R/config.R:244-246`, class `sas2r_config_error`); `:94` and `:106` use `sas2r_output_contract_error`; `:110` (TLF profile) is unclassed. No test pins any of the mapping messages.

### B.7 Preflight destinations re-spell paths that `migration_paths()` owns
- `R/preflight.R:57-59` hand-spells `generated-outputs` (`R/translate.R:199`), `<attempt>/bundle` (`R/migration-attempts.R:86-87`) and `report.json` in the run folder, which is the secondary copy (`R/migration-report.R:241-244`); the authoritative `paths$report_json` (`<out_dir>/.sas2r/report.json`, what `sas_translate` returns) and `paths$report_md` are omitted although `migration_paths()` provides both.

### B.8 `build_dependency_graph()` runs only to fill an undocumented field
- `R/preflight.R:38` feeds `:67` `schedule` only. `schedule`, `configured_libraries`, `notes` (and `status`) are returned but absent from `man/sas_preflight.Rd` `\value`, from `print.sas2r_preflight`, from tests, and from docs.

### B.9 Budget limit names are hard-coded in a third place
- `R/preflight.R:39-41` versus `R/translation-setup.R:58-59` (derived from `formals(new_usage_budget)`) and `R/migration-summary.R:34-35`. All agree today on the eight `max_*` names; a new formal would be accepted by translation but vanish from preflight's `$budget` and the usage summary. (All 11 names exist on the budget object; no NULL entries.)

### B.10 Dataset format set is spelled a fourth time
- `R/preflight.R:108` versus `LIBRARY_READ_ENGINES` (`R/config.R:94`, a set), `R/migration-gate.R:55` (`rds, xpt, sas7bdat`, attempt dirs), `R/runtime-data.R:279-281` (`rds, sas7bdat, xpt`, input libraries). Preflight matches `lib_read()`'s order, so the operational drift is narrower than "preflight vs gate". The bypass of `sas2r_lib_member_path()`'s unsafe-member refusal is real in code but unreachable: the token regex at `R/extract.R:147` admits no `/`.

### B.11 `translation_config()` promotes a pre-existing drift into a shared helper (PLAUSIBLE as new cost; drift itself pre-exists)
- `R/translation-setup.R:10-11` wraps a list config bare; `sas_project()` (`R/project.R:147-158`) merges a list over the discovered `_sas2r.yml`. So `sas_translate(path, config = list(libraries = ...))` loses the file's `llm`/`outputs` while `sas_project(path, config = list(...))` keeps them, and with a `sas2r_project` as `path` both callers re-run discovery from `project_dir` and ignore `project$config`. The identical inline block exists at merge-base (`R/translate.R:122-137`). The only new behaviour: a nonexistent character `config` now aborts (`sas2r_config_error`) instead of falling through to discovery, which is a deliberate, tested tightening.

### B.12 `paths_for()` re-derives the binding join (PLAUSIBLE: waste, not drift)
- `R/preflight.R:89-95` matches `(kind, use_file, use_line, libref)` although `lineage$binding_id`/`binding_status` (`R/libref-registry.R:1146-1158`, `R/project.R:763`) already answer it. By construction the tuple cannot disagree with the id join (same key at `:1184-1192`).

### B.13 Two markdown fence grammars over the same five documents
- `tools/run-doc-examples.R:16` accepts only exact ```` ```r ````/```` ```yaml ```` openers; `doc_r_chunks()` (`tests/testthat/test-documentation-contracts.R:51-53`) also accepts whitespace and ```` ```{r ...} ````. Both vignettes' only `{r` chunks today are knitr setup chunks at line 12, so the current impact is trivial, but the CI step (`.github/workflows/check.yml`) and the contract test now disagree on what a chunk is. Two latent bugs in the script: `:25` `seq.int(start + 1L, end - 1L)` reverses on an immediately closed fence (code becomes the fence lines; `parse()` errors), and `:92` `stopifnot(env$check$model_calls == 0L)` passes vacuously if the README block ever stops assigning `check`.

### B.14 The semantic-reference audit and the in-test comparison disagree on tolerance and structure
- `tests/testthat/helper-semantic-reference.R:20-21` (`identical(names)` + `all.equal(tolerance = 1e-8, check.attributes = FALSE)`) versus `tests/testthat/test-semantic-reference.R:40-43` (`expect_equal(ignore_attr = TRUE)`, tolerance ≈ 1.49e-8, ignores names). A renamed column passes the test and fails the audit; a 1.2e-8 relative difference likewise. The fixture README (`:42-44`) names the audit as the completeness gate. Related: the 14 `%semantic_case` calls (`tools/generate-semantic-references.sas:30-43`), `manifest.json`, the fixture README (`:13, :23`) and `test-semantic-reference.R:62` all hard-code the count; the driver `tools/run-semantic-references.R:18` sources a `tests/testthat/helper-*.R`, so the audit never reaches the installed package or tarball.

### B.15 `references$status` changes type with cardinality
- `R/preflight.R:47-48`: `ifelse()` on a zero-length condition yields `logical(0)`, so the column is `logi(0)` with no references and `chr` otherwise. Small impact; `character()` default fixes it.

### B.16 The quick-start preflight budget is not the budget the quick-start translation runs with
- `README.md:132` (`usage_limits = list(max_calls = 20)`) versus `:150-158` (`sas_translate()` without `usage_limits`); same in `vignettes/dependency-aware-migration.Rmd:85` vs `:112`. `docs/clinical-qc-preflight.md:53` and preflight's own `notes` say to reuse the same arguments.

---

## C. Efficiency (confirmed, timings from the verifier's runs)

### C.1 Preflight recomputes contracts, graph, and schedule that `sas_project()` just computed
- `R/preflight.R:34, 38, 67` versus `R/project.R:787-791` (graph and schedule are then discarded; `output_contracts` is kept at `:881`, and `identical(check$outputs, project$output_contracts)` is `TRUE` when `outputs` is `NULL`). On a synthetic 150-program study this is roughly 105 s of a 276 s preflight. `sas_translate()` (`R/translate.R:116-123`) has the same duplicate.

### C.2 The documented flow scans every source twice, and the emitter output is thrown away
- `R/preflight.R:33` uses `cache = FALSE` and returns no project; `README.md:130` then `:150` both pass `"programs/"`, although `sas_translate()` accepts a `sas2r_project`. `preflight_unsupported()` (`:126-139`) runs the full deterministic emitter over every unit and keeps only `reason`/flags; `sas_transpile()` regenerates the code. Returning `project` from preflight and documenting `sas_translate(check$project, ...)` removes the second scan.

### C.3 `preflight_inputs()` is quadratic in programs
- `R/preflight.R:95-103`: per read row, a subset of all writers plus a tibble row-subset per producer, evaluated fully (`any(vapply())`, no early exit). ~30 µs per subset; 100 programs × 10 datasets ≈ 3 s, ~10× per 4× programs. Resolve each writer once, or reduce `work.*` to `%in%`.

### C.4 One-row tibbles per read and per unit
- `R/preflight.R:112-115` and `:138-139`: 5,000 one-row `tibble()` calls take 2.6 s; one vectorised `tibble()` takes 0.003 s.

### C.5 `keys_nonmissing` runs `trimws(as.character(x))` on numeric and date keys after normalisation already blanked padding
- `R/qc-profile.R:181`; `normalize_output_frame()` (`R/row-alignment.R:176-183`) already maps `"  "`/`"01 "` to `NA`/`"01"`. 1 s per million-row numeric key versus 0.001 s for `sum(is.na(x))`, per key, per target, per gate round. Keep a character-only branch if tab/newline-only values must still count.

---

## D. Test coverage

### D.1 The test for the moved argument validation pins only the error class
- `tests/testthat/test-translate.R:71-74`; nothing asserts that `out` stays empty, so A.7 is invisible to the suite. `expect_false(dir.exists(file.path(out, ".sas2r")))` after the `expect_error` would pin the original invariant.

---

## Checked and cleared

- Budget/pricing/usage-limit validation bodies moved verbatim; `translation_config`/`translation_budget` reproduce the old normalisation for both callers; `cache = FALSE` writes no scan cache; no ledger I/O with a `NULL` ledger.
- `NAMESPACE`, man pages, and `test-public-api.R` agree on the two new exports and the print method; `check.yml` references an existing script and passes under `--source` (11 offline blocks, 16 YAML configs).
- `deterministic_unit_translation()` preserves `target_ds`/`reason` semantics; `reason` can be `NULL` on success and the manifest builder handles it (`%||% NA_character_`); the removed `ir` initialiser has no remaining reference.
- `semantic_frame` moved to a helper both testthat and the driver load; the SAS generator keeps the same 14 cases, `WORK.OUT` export, and provenance line; `%include` inside the macro and `%abort abend` are valid.
- Classed profiles survive the resume fingerprint (`canonicalize_migration_value()` rebuilds lists); `qc_profile()` field order is deterministic; `migration_md_table()` escapes `|` and newlines so the new `reason` cannot break `report.md`.
- `print.sas2r_preflight()` handles zero flags/inputs; no user data reaches cli as a glue template; nothing on the preflight path touches an LLM adapter or pricing lookup; `as.list(budget)[limits]` has no `NULL` entries.
- `lib_read()` probes the same extensions in the same order as preflight's `available` check.
- The affected test files all pass on the branch under pkgload; none of the findings above is caught by the existing suite.
