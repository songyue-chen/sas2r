Corrected tutorial example for sas2r 0.5.8

This is a tutorial-specific revision, supplied separately from CRAN.
It replaces SET &plotds with SET raw.input_ds so 0.5.8 can identify the input,
and declares LENGTH AVAL_FLAG $8 so RECORDED is not truncated to seven characters.
The original example remains installed under examples/migration-demo.

expected-final.csv contains hand-authored expectations derived from this
corrected SAS and make-input.R. It was not produced by SAS and must not be
represented or configured as an independent SAS reference. Verification is
performed after translation; it is not supplied as an answer to the translator.
