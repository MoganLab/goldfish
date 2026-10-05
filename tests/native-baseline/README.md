# Native test baseline archive

The original `full-run.log`, `discovered.manifest` and `results.tsv` are frozen
historical evidence. Later targeted runs do not rewrite that verdict. Use
`dispositions.tsv` to see which original failures were resolved and
[`../NATIVE-FOLLOWUPS.tsv`](../NATIVE-FOLLOWUPS.tsv) for the current active
follow-up list.

The original 131 failures included the million-element set test. That case is
now resolved; its current native result is recorded in
[`set-million-recheck.log`](set-million-recheck.log) with peak memory and wall
time in [`set-million-recheck.metrics`](set-million-recheck.metrics). Other
targeted rechecks remain in this archive where they support the disposition
record. Do not interpret the historical full-suite totals as a current full
suite result.
