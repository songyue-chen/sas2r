# Live OpenAI verification — 2026-11-08

The corrected tutorial and downloadable scripts were run in a fresh project with
**CRAN sas2r 0.5.8, ellmer 0.5.0, R 4.4.0, and OpenAI `gpt-5.6-luna`**.
The test used the configured provider and real API requests, not a deterministic
translator or reviewer. Only RStudio GUI actions (`View` and opening the report)
were replaced with terminal output by the test harness.

- Offline preflight found `raw.input_ds` available.
- The real provider probe returned `TRUE`.
- Translation and review completed; the successful run needed no fixer call.
- The bundle executed and returned `migration_ready`.
- Companion checks passed for all five rows, values, missingness, flags, column
  names, row order, and PDF text.
- The exported R was run in a fresh subprocess with `OPENAI_API_KEY` removed.
  Its dataset and PDF passed the same checks.
- The actual PDF was inspected: all columns and five rows are present. Its row
  spacing is loose and the observation heading is close to the first heading.
  The PDF is a content demonstration, not a claim of matching SAS formatting.

The successful translation reported 119.7 seconds, 8 provider calls and $0.0295
known estimated spend, with one call of unknown cost. This is not a billing
receipt or a complete cost total including the separate connection probe.

The first corrected-source trial executed but retained a sort review finding
after one repair. It was **not accepted as successful**. The final tutorial adds
explicit synthetic-input schema comments and allows up to two component repair
rounds. The successful fresh run used neither round. Actual model runs can differ.

## Saved evidence

- [Summary](summary.json)
- [Actual console output](live-console.txt) — local project prefix replaced with `<project>`
- [Selected generated R](selected-code.R)
- [Generated dataset as CSV](final-dataset.csv)
- [Actual generated PDF](table1.pdf)

These files are the selected OpenAI run's output. They are **not SAS-generated
references**. The separate `companion/example/expected-final.csv` is a set of
hand-authored tutorial expectations. Neither proves independent SAS equivalence.
No API key, credential file, serialized model adapter, or full provider request
payload is included in this evidence.
