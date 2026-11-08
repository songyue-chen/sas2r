# Run in a fresh R session from the tutorial project, after sas_write().
# No sas2r package or provider connection is used here.
local({
  project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  export_dir <- file.path(project_dir, "exported-r")
  previous_dir <- setwd(export_dir)
  on.exit(setwd(previous_dir), add = TRUE)
  runtime <- new.env()
  sys.source("autoexec.R", runtime)
  # sas_output_path() creates output/tlf/ for generated reports.
  sys.source("run.R", new.env())
  message("Export executed. Inspect ", runtime$.sas2r_output_root,
          "; manual execution does not update the original migration report.")
})
