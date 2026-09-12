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
  outputs = outputs, budget_usd = 5,
  usage_limits = list(max_calls = 20, max_request_bytes = 200000)
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
it on each candidate. Use the same `config`, `outputs`, and budget arguments
when calling `sas_translate(check$project, ...)` on your study. The returned
project reuses the scan; run preflight again if sources change. `config$budget` is not consumed by
these entry points; their explicit budget arguments determine the effective
limits. Preflight displays planned locations; run and attempt IDs are assigned
when translation starts.

`inputs$status` distinguishes an existing file (`available`), a missing file,
a library that could not be resolved (`unresolved`), a WORK member with no known
earlier producer (`no_producer`), and data with a known in-project producer
(`generated`). WORK members need an earlier creation step, not a disk file. Generated inputs still depend on that
producer running successfully. `unsupported` lists constructs the deterministic
emitter defers; the AI workflow may translate them. Runtime-only restrictions,
data values, reference comparability, and model credentials remain unchecked.
Macro variables in dataset names produce a `dynamic_dataset_reference` finding;
preflight does not expand them or certify those inputs as available. Unresolved
includes and library bindings also require attention. Within-file step handoffs
are ordered normally; cycles between files remain findings that need attention.
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
mappings override. Unknown assertion fields fail before translation. Empty
metadata mappings impose no requirement. Column names are case-insensitive.
Quote YAML metadata keys and string values, for example `labels: {"N": "Count"}`
and `formats: {"AVAL": "8."}`. YAML otherwise converts `N` to a boolean key and
`8.` to a number; the original spelling cannot be recovered after parsing.
Non-character metadata values are rejected, including mixed mappings. Labels and `format.sas` attributes must match exactly
when declared. `numeric` accepts integer or double columns, while `Date` and
`POSIXct` require those classes. These are physical R type requirements:
factors fail `character` even if the comparator can match their text values.
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

An explicit R configuration list is a complete configuration for preflight and
translation; it does not silently inherit a discovered model provider. To keep
file configuration while making changes, start with `sas_config()` and edit that
object. A supplied project retains its own configuration unless explicitly
overridden. Configured reference paths resolve against the YAML file directory;
paths in R configuration lists resolve against the project directory. Direct
`outputs` argument paths remain relative to the calling working directory.
