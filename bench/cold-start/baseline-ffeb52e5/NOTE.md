# Baseline suite caveat

The `cold` and `warm` rows for `minimal.scm` in `summary.tsv` are valid.

`bootstrap` sample 1 and the whole `readonly` state are invalid: a pipeline
source (`goldfish/expander/lib/install.scm`) was edited while this suite ran,
which changed the content-addressed cache version directory mid-run and forced
a spurious cold rebuild.  `bootstrap` samples 2-3 used the new version and are
consistent with each other.  The affected states were re-measured cleanly on
the optimized binary (see `../optimized-46a5f649/`).
