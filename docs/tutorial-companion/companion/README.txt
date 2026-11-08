sas2r tutorial companion scripts

These scripts are supplied separately with the tutorial. They are NOT files
installed by install.packages("sas2r"). The folder name sas2r-demo in the video
is simply the name chosen for the viewer's RStudio project.

Included here (inside companion/):
  demo.R           Follow the tutorial step by step in RStudio.
  preflight-llm.R   Probe the configured model; print the latest probe log entry.
  read-llm-log.R    Inspect model activity after sas_translate() returns result.

The example itself IS included with CRAN sas2r 0.5.8:
  demo.sas, _sas2r.yml, make-input.R, README.md
Find those installed files in R:

  installed_demo <- system.file("examples", "migration-demo", package = "sas2r")
  cat(installed_demo, "\n")
  cat(list.files(installed_demo), sep = "\n")

Getting started:
1. Install sas2r and ellmer, configure OPENAI_API_KEY in your user .Renviron,
   save and restart R as shown in the video.
2. Create a new, empty RStudio project at a location you choose.
3. Extract sas2r-tutorial-companion.zip and open its companion folder.
4. Copy its three .R files into the RStudio project. Open demo.R.
5. Run demo.R interactively, section by section. It copies the bundled example
   into the project and creates the five synthetic input observations.
6. At PAUSE, add the following llm block below outputs in the same _sas2r.yml.
   It is also provided as a commented template at the end of demo.R: remove
   only the leading "# " when copying it to YAML; keep the indentation.
   Keep it commented in R. Preserve libraries and outputs, update an existing
   llm section instead of duplicating it, and put no API key in YAML.

llm:
  provider: openai
  auth_mode: api_key
  model: gpt-5.6-luna
  reasoning_effort: high
  max_output_tokens: 128000
  capabilities:
    structured_output: native
    tool_calling: native
  timeout_seconds: 900
  max_tries: 1

7. Save the configuration and continue demo.R. The connection probe and
   translation use your provider account and may incur charges. The selected
   model must be accessible to your account; this profile is not a spending cap.
8. Review status, diagnostics, generated code, datasets, and PDF before export.

The video uses reconstructed interfaces and deterministic offline test adapters.
No live OpenAI authentication or translation was recorded. The separate repair
example seeds a +20 draft for SAS +10; the corrected outputs were 11 and 17.
The probe demonstrates a structured ping, not full tool-calling or migration
validation. No SAS reference output is supplied, so equivalence is not established.

Package: https://CRAN.R-project.org/package=sas2r
Documentation: https://github.com/songyue-chen/sas2r
