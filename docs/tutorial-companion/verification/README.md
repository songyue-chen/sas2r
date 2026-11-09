# Live tutorial verification — 0.6.0 beta, PR #65

The main walkthrough reconstructs OpenAI run `run_20261109T015807Z_91f7b381` using tested commit
`72e0b3f4c33a13921f1529138f0d9b9acce68c6b` from [PR #65](https://github.com/songyue-chen/sas2r/pull/65),
based on merged develop `8802bc57`. This is a beta, not the CRAN release.
See summary.json for the versions and estimated usage; actual AI runs can differ.

Review, smoke execution and the first bundle attempt passed without repairs.
Dataset values, missingness, flags, source row/column order and PDF text passed.
The generated code uses `sas_print_table()` with `sas_output_path()`.
The actual PDF has aligned headings/values and compact row spacing; both
independent demo geometry checks and visual inspection passed. The same
companion check rejects the earlier malformed PDF. This is a narrowly scoped
one-page demo check, not arbitrary PDF layout or SAS styling certification.

The PDF is saved at `outputs/tlf/table1.pdf`, without another outputs directory.
The standalone export passed the same checks without an API key or sas2r loaded.
Its output is under `output/tlf/`; manual execution does not update the original
migration report. No independent SAS reference is supplied.

The [separate earlier repair example](repair-example/README.md) is retained and
explicitly labeled in the video. It is not this run's execution trace.
`selected-code.R`, `selected-demo.R` and `table1.pdf` are unchanged generated
artifacts. Local project paths are abbreviated in console logs. Other v10,
PR #63 and prior beta notes are historical and do not describe this run.

Final install commit `22b34abc27f0a73b679f124f51ce9cee4fa39aef` adds only the public-API test expectation.
Its runtime, prompts and documentation are identical to the OpenAI-tested commit above.
