# Installed sas2r examples

These files are included when you install sas2r; no GitHub checkout is needed.
Locate them from R:

<!-- sas2r-example: offline installed-examples -->
```r
examples <- system.file("examples", package = "sas2r")
list.files(examples)
```

- `migration-demo/`: a five-row synthetic study with dataset derivations and a
  PDF table. Copy it to a writable directory and follow its README. Translation
  of its macro and reporting statements needs a configured language model.
- `bug-report-demo/`: an offline example for the package-bug reporting beta.
  It uses a synthetic report based on a historical PDF writer regression.
- `_sas2r.example.yml`: an annotated configuration reference. Adapt the paths,
  output names and provider/model settings before using it. It is not an
  immediately runnable study; its active model profile needs credentials.

Input library engines are `sas7bdat`, `xpt` and `rds`; output formats are `rds`
and `xpt`. Supply credentials through your provider's supported authentication,
not by putting secrets into the YAML file.

For an offline first run, see the package's `dependency-aware-migration`
vignette (`vignette("dependency-aware-migration", package = "sas2r")`).
Do not generate data or migration outputs inside the installed package directory.

## Try the package-bug reporting beta

The demo report represents a run whose bundled PDF writer put PDF bytes in an
HTML file. Use the installed example to create a local draft. This takes no SAS
installation, API key, or study project.

<!-- sas2r-example: offline bug-report-demo -->
```r
demo <- system.file("examples", "bug-report-demo", "report.json", package = "sas2r")
draft <- sas2r::sas_bug_report(demo, file = tempfile("sas2r-issue-", fileext = ".md"))
cat(readLines(draft), sep = "\n")
```

Copy the R block from the draft into a fresh R session and run it. On the
current beta it should say that the helper rejected the destination before
writing a file. The report represents an earlier runtime where the same
assertion fails. The demo is for practice and does not require a GitHub issue.
