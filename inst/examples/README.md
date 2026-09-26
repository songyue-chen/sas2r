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
- `_sas2r.example.yml`: an annotated configuration reference. Adapt the paths,
  output names and provider/model settings before using it. It is not an
  immediately runnable study; its active model profile needs credentials.

Input library engines are `sas7bdat`, `xpt` and `rds`; output formats are `rds`
and `xpt`. Supply credentials through your provider's supported authentication,
not by putting secrets into the YAML file.

For an offline first run, see the package's `dependency-aware-migration`
vignette (`vignette("dependency-aware-migration", package = "sas2r")`).
Do not generate data or migration outputs inside the installed package directory.
