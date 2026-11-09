sas2r tutorial companion — 0.6.0 beta (revision 6)

Use a NEW, writable RStudio project for this revision. Extract the ZIP, open
companion/, and copy ALL its contents into the project, including example/.
Do not copy only the R scripts. Open demo.R and run it section by section.
If you used an earlier download, keep that run for diagnosis and start fresh.

Files and provenance
- CRAN sas2r 0.5.8 includes its original example under examples/migration-demo.
  Locate it with system.file("examples", "migration-demo", package = "sas2r").
- This download supplies a corrected tutorial-specific example in example/.
  It uses SET raw.input_ds and LENGTH AVAL_FLAG $8. This avoids the original
  macro-input discovery problem and the seven-character truncation of RECORDED.
- demo.R copies this corrected example into your project, preserving existing
  files and edited YAML. It stops if demo.sas differs from this revision.
- preflight-llm.R probes the provider and reads its log.
- read-llm-log.R prints model activity from the returned run.
- verify-output.R checks actual outputs against example/expected-final.csv.
  These expectations are derived from the corrected source and synthetic input.
  They are NOT SAS-generated reference results and do not prove SAS equivalence.

Steps
1. Install the tested PR #65 build based on merged develop (0.6.0 beta; not yet CRAN):
   install.packages(c("remotes", "ellmer", "pdftools"))
   remotes::install_github("songyue-chen/sas2r", ref = "22b34abc27f0a73b679f124f51ce9cee4fa39aef")
   https://github.com/songyue-chen/sas2r/pull/65
   Restart R if sas2r was already loaded.
2. Store OPENAI_API_KEY in your user .Renviron. Save and restart R.
   Check nzchar(Sys.getenv("OPENAI_API_KEY")); never print the credential.
3. Run demo.R's setup to copy the corrected example and generate five rows.
4. PAUSE at configuration. Copy the 11 commented YAML lines at the end of
   demo.R into the SAME _sas2r.yml, below outputs. Remove only the leading
   "# " from each pasted line; preserve indentation. Keep libraries and outputs.
   Align llm at their top level; update an existing llm block, never duplicate it.
   Save. Leave the template commented in demo.R. Keep the key out of YAML.
5. Run offline sas_preflight(..., diagnose = "off"). Confirm raw.input_ds is
   available. The script stops before provider calls if it is not available.
6. Run preflight-llm.R. A TRUE probe checks a structured ping, not the complete
   migration. Inspect the log and fix any error before translating.
7. Run sas_translate() with up to two component repair rounds. Probe and translation make real provider requests and
   may incur charges. This example profile has no total spending cap. Confirm
   account billing/credits and access to the configured model before running.
8. Inspect status, START_HERE.html, generated R, and the model activity log.
9. Run verify-output.R and verify_demo_output(result, project_dir). It requires
   migration_ready, five expected rows, correct values/missingness/flags/order,
   PDF text, and this demo's one-page table alignment and row spacing. It stops
   on a mismatch. Inspect the PDF visually too; these checks are not general PDF validation.
10. After the checks pass, export into a new folder with sas_write(). Read the
    exported README.md. In a fresh session in your project, run
    source("run-export.R"). The runtime creates the report folder automatically
    and runs exported-r/run.R without any provider calls. Then inspect the outputs.
    The standalone runner does not change the original migration status.

Important interpretation
An input file, an expected-value check, and an independently generated SAS
reference are different things. No SAS installation or SAS reference output
is included. An AI run can require repair or fail; inspect every run's evidence.
The video uses reconstructed interfaces and synthetic data. Its main workflow
is backed by real OpenAI tests. The latest run used the shared plain-table
renderer and passed review and execution without repairs. Its columns and row
spacing passed independent demo checks and visual inspection. The fixer segment
is a separate earlier beta run, explicitly labeled in the video.
See the GitHub verification notes for the tested versions, outcome and limits.

Downloads and verification
https://github.com/songyue-chen/sas2r/tree/tutorial-companion-v1/docs/tutorial-companion
https://CRAN.R-project.org/package=sas2r
https://github.com/songyue-chen/sas2r/blob/develop/docs/llm-providers.md

Output locations in this beta
- Run: migration-output/<run_id>/outputs/tlf/table1.pdf
- Standalone export: exported-r/output/tlf/table1.pdf
There is no extra outputs/ beneath tlf/. The runtime creates parent directories.
The checker also verifies column order: AVAL_FLAG comes first because LENGTH
precedes SET in the SAS source. This is a source-derived tutorial check, not
independent SAS output or formatting equivalence.
