# Offline preflight and clinical QC profiles

Run `sas_preflight()` before a migration to inspect resolved source files,
point-of-use library bindings, missing input paths, deterministic translation
limitations, output targets, destinations, and effective usage limits. It does
not construct an AI adapter, make model calls, read dataset contents, execute
programs, or write a scan cache or migration folder.

This complete example runs offline with a small synthetic input:

<!-- sas2r-example: offline preflight-qc -->
```r
library(sas2r)

study <- tempfile("qc-study-")
dir.create(study)
dir.create(file.path(study, "raw"))
saveRDS(data.frame(USUBJID = c("01", "02"), AGE = c(50, 60)),
        file.path(study, "raw", "dm.rds"))
writeLines("data adam.adsl; set raw.dm; run;", file.path(study, "adsl.sas"))

subject_qc <- qc_profile(
  required_columns = c("USUBJID", "AGE"),
  labels = c(AGE = "Age in years"),
  types = c(USUBJID = "character", AGE = "numeric"),
  column_order = c("USUBJID", "AGE"),
  keys = "USUBJID", unique_keys = TRUE,
  row_count = 2,
  numeric_tolerance = 0,
  tolerances = list(AGE = list(abs = 0.01, rel = 0))
)
outputs <- list(
  profiles = list(subject = subject_qc),
  assertions = list("adam.adsl" = list(profile = "subject"))
)
check <- sas_preflight(
  study, out_dir = file.path(study, "migration"),
  config = list(libraries = list(raw = "raw", adam = "adam")),
  outputs = outputs
  # Optional limits, commented out by default. Uncomment to enforce them; the
  # run stops admitting requests when a limit is reached.
  # Catalog costs are estimates; an in-flight request may exceed the threshold.
  # , budget_usd = 5, budget_mode = "soft", usage_limits = list(max_request_bytes = 200000)
)
print(check)
check$libraries
check$outputs$assertions
check$budget
stopifnot(check$model_calls == 0L,
          check$inputs$status[check$inputs$dataset == "raw.dm"] == "available",
          !dir.exists(file.path(study, "migration")))
```

The example declares an AGE label requirement; preflight records that requirement
but does not inspect the data to check it. The migration output gate evaluates
it on each candidate. `sas_translate(check$project, ...)` reuses the scan,
configuration, and output requirements, including explicit `outputs` overrides.
Supply the same budget arguments again. Run preflight on the source path if
sources or library settings change; changing scan settings on an existing
project is rejected with a rescan instruction. `config$budget` is not consumed by
these entry points; their explicit budget arguments determine the effective
limits. Preflight displays planned locations; run and attempt IDs are assigned
when translation starts.

Preflight also reports `max_parallel_translations`, from its explicit argument
or the default of 1. It describes the requested workflow limit. Because preflight does
not construct a model adapter or launch translation processes, it does not test
parallel adapter support, measure available CPU/memory, or check provider quotas.
The translation report records the effective concurrency and any fallback reason.
If you override this value in the preflight call, supply the same override to
`sas_translate()`; a call-specific preflight override does not change the project
configuration.

`inputs$status` distinguishes an existing file (`available`), a missing file,
a library that could not be resolved (`unresolved`), a WORK member with no known
earlier producer (`no_producer`), a read whose only producer occurs later in
the same file (`backward_dependency`), and data with a known in-project producer
(`generated`). An APPEND base with no existing input is `created_if_missing`: SAS
creates that base on its first append. Other WORK members need an earlier
creation step, not a disk file. `no_producer` and `backward_dependency` are
static-analysis warnings, not proof that an input is unavailable: a translated
macro may create it. They allow execution to test that behavior. A failed file
lookup (`missing`) or unavailable library binding (`unresolved`) still defers
affected execution. Generated inputs still depend on that
producer running successfully. `unsupported` lists constructs the deterministic
emitter defers; the AI workflow may translate them. Runtime-only restrictions,
data values, reference comparability, and model credentials remain unchecked.
Macro variables in dataset names produce a `dynamic_dataset_reference` finding;
preflight does not expand them or certify those inputs as available. Invoked
project macros whose data flow needs expansion raise `macro_data_flow_deferred`.
Conditional or labelled dataset statements, name literals, and library-level
COPY/DATASETS operations raise `dataset_statement_deferred`; preflight does not
invent dataset names for these unsupported forms. Unresolved
includes and library bindings also require attention. Within-file step handoffs
are ordered normally; a later write cannot supply an earlier read, even if an
old output file exists. Files with identical basenames retain separate identities.
Cycles between files remain findings that need attention. The file-level
scheduler can also flag a valid include handoff as a cycle when the parent
writes before an include and consumes its output afterward; this case requires
manual scheduling review. Macro definitions are deferred constructs, not executed
input reads. Preflight does not expand macro calls. The migration demo therefore
has unresolved dynamic inputs and can report `needs_attention`.
`status` summarizes these setup findings. `schedule` shows the file order,
`configured_libraries` the configured seeds, and `notes` the inspection limits.
`destinations$report_json` and `$report_md` identify the authoritative reports.
An empty unsupported list is not an accuracy assessment.

For a missing input, inspect `searched_paths` and correct the library binding or
supply the listed member. Inspect `libraries$selection_origin` to see whether
SAS source or configuration selected a directory. Resolve missing includes and
macros at the reported source location. Call `sas_llm_probe()` separately when
you deliberately want to test provider connectivity.

The same profile can be written directly in `_sas2r.yml`:

<!-- sas2r-example: config clinical-qc -->
```yaml
outputs:
  profiles:
    subject:
      required_columns: [USUBJID, AGE, ADT]
      labels: {AGE: Age in years}
      formats: {ADT: "DATE9."}
      types: {USUBJID: character, AGE: numeric, ADT: Date}
      column_order: [USUBJID, AGE, ADT]
      keys: [USUBJID]
      unique_keys: true
      min_rows: 1
      max_rows: 10000
      numeric_tolerance: 0
      tolerances:
        AGE: {abs: 0.01, rel: 0}
  references:
    adam.adsl: data/reference/adsl.xpt
  assertions:
    adam.adsl:
      profile: subject
      row_count: 306
```

A target assertion overrides the entire corresponding profile field; profile
fields override `comparison_rules` defaults. R and YAML profiles contain only
supplied fields: omitted/NULL fields inherit, while explicit `FALSE` or empty
mappings override. `unique_keys` is checked against inherited keys after these
fields are combined. Unknown fields and non-mapping assertion sequences fail
before translation; a NULL target or profile entry is an empty mapping. Empty
metadata mappings impose no requirement. Column names are case-insensitive.
The config loader preserves YAML keys such as `N`, `Y`, `yes`, and `no` as
strings. Use `true` and `false` for boolean settings; `yes`/`no`/`on`/`off` are
strings rather than boolean aliases. When generating YAML with the R `yaml`
package, use a logical handler that emits literal `true`/`false`. Quote metadata
string values such as `formats: {AVAL: "8."}` so YAML does not turn them into
numbers. Scientific notation is accepted for finite, nonnegative whole row counts.
Non-character metadata values are rejected, including mixed mappings. Labels and `format.sas` attributes must match exactly
when declared. `numeric` accepts integer or double columns, while `Date` and
`POSIXct` require those classes. These are physical R type requirements:
`factor` accepts ordinary and ordered factors; factors fail `character` even
if the comparator can match their text values.
Malformed metadata attributes fail QC and retain the observed values in the
report. `column_order` specifies the complete ordered column list.
Keys guide reference alignment; without a reference they impose no requirement
unless `unique_keys: true` is set. That requires the columns to exist and have
nonmissing, nonblank values and a unique combined key, using the comparator's
SAS trailing-space normalization. Declaring alignment keys alone still permits
missing values and repeated keys. Row count requirements can be exact or bounded.
Checks report expected/actual metadata, affected columns, missing key counts,
and duplicate counts in the target's `checks` record.

Per-variable tolerances use `compare_profile()`'s absolute and relative policy
and apply only during reference comparison. A profile with no reference checks
structure and metadata only. Metadata remains optional for other targets and
cosmetic in the ordinary comparator unless its policy says otherwise. Named
profiles are explicit study contracts, not built-in claims of ADaM or SDTM
compliance. Passing them alone does not produce reference-validated evidence.

CI runs the marked offline R and configuration examples directly from these
documents using `Rscript tools/run-doc-examples.R --installed`. Study-dependent
saved-output examples use synthetic fixtures; provider and installation examples
are parsed but are not advertised as offline execution tests.

With a source path, an explicit R configuration list supplies the complete
configuration; it does not inherit a discovered model provider. With a reused
project, a plain list updates only the supplied top-level fields. For example,
`sas_preflight(check$project, config = list(comparison_rules = list(min_rows = 10)))`
replaces the global comparison rules while retaining libraries, output
requirements, and model settings. Nested fields are not merged; an explicit
`NULL` resets that top-level field, and `list()` leaves the project unchanged.
A YAML path or `sas2r_config` object always supplies a complete configuration.
To edit file configuration, start with `sas_config()` and modify that object.
Changed library, include, macro-search, or autoexec settings, including explicit
clearing, require rescanning the source path.

Configured reference paths resolve against the YAML file directory;
paths in R configuration lists resolve against the project directory. Direct
`outputs` argument paths are anchored to the calling working directory. Stored
paths are absolute so project reuse does not prefix them again. Preflight and
output gates both honor target references and the `comparison_rules$reference_path`
or `comparison_rules$references` fallback. The precedence is an explicit
`outputs$references` entry, then a per-target `comparison_rules$references` entry,
then the global `comparison_rules$reference_path`. The global fallback applies
to all targets, including TLFs; prefer per-target references for mixed bundles.
The resolved path is stored in each output contract. Dataset keys are
case-insensitive and unqualified names mean WORK, never an inferred library.
Duplicate target spellings are rejected, and a comparison reference naming an
unknown target must be corrected or declared in `outputs`. A configured reference must be a file;
a missing path or directory fails the output gate, including for tables,
listings, and figures (TLFs). TLF reference content comparison remains unavailable:
an existing reference is recorded as not compared and supplies no equivalence
evidence.

All output assessment records carry a `reason` in JSON and Markdown. Unavailable
checks are labeled as not evaluated, separately from failures. `output-contracts.json`
is always an array of target records, including for zero or one target, and
preserves the configured numeric precision. Matching checkpoints reuse completed
revisions; the parallel checkpoint format also imports compatible version-8
checkpoints. Incompatible checkpoints regenerate, and resume reports the reason
before new provider calls. See the [resume contract](migration-evidence.md#coverage-limits-and-reuse)
for retained reviews, smoke results and repair allowances.

Default numeric tolerances use `tol_abs` before `numeric_tolerance` when both
appear in global rules, with `tol_rel` as the relative default. An explicit
profile or target `numeric_tolerance` replaces those default aliases and sets
the relative default to zero. Per-variable `tolerances` is a separate inherited
field and overrides the defaults; set `tolerances = list()` to clear it. The
historical `comparison_rules$tolerance` field is ignored with a warning.

Use a single YAML document and `true`/`false` booleans. Some YAML writers emit
`yes`/`no` by default; convert those boolean values to `true`/`false` before
loading the configuration. Metadata keys such as `N` and `Y` remain strings.

## Warnings and translation-only work

Preflight is a readiness report. Missing input files, source includes/macros and
uncertain dependency order normally produce warnings while available source
continues translating. Inspect `check$readiness$warnings` for affected components,
source locations, consequences and suggested actions. Documented SAS metadata
reads have input status `environment`; they do not require a study data file.

Use `sas_translate(..., execute = FALSE)` when you only want translated code.
Leaving references unconfigured does not select that mode: programs can run and
undergo output checks without SAS reference comparisons. Missing source cannot be
invented; affected translations and downstream assumptions remain provisional.
Missing execution prerequisites defer execution, while missing configured
references defer comparison. No active source, unusable configuration and
unexplained pipeline omissions still stop the run before translation.
