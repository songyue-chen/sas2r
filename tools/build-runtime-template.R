# Regenerate inst/templates/sas2r-helpers.R from R/runtime-*.R.
# Run from the package root:  Rscript tools/build-runtime-template.R
pkgload::load_all(".", quiet = TRUE)
lines <- sas2r:::runtime_template_lines("R")
writeLines(lines, file.path("inst", "templates", "sas2r-helpers.R"))
cat("wrote inst/templates/sas2r-helpers.R (", length(lines), "lines )\n")
