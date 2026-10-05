# Native verification

## Routine gates

Run the fixed native workflow with:

```sh
sh tools/test-native-workflow.sh
sh tools/check-native-manifest.sh
```

For normal changes, run the affected test file or directory. Run
`./bin/gf test --all` only for scheduled full-suite checks or changes to core
semantics. On a non-main branch, `./bin/gf test` means
`--changed-since=main`, not the full suite.

Current deferred and excluded tests are listed in
[`tests/NATIVE-FOLLOWUPS.tsv`](tests/NATIVE-FOLLOWUPS.tsv). This is the live
list; historical test dispositions remain in the baseline archive.

## Native full-suite record

The latest archived full-suite run is a historical baseline, not a current
pass claim: revision `08354213` discovered 1,576 files, with 1,445 passing and
131 failing. The full transcript, frozen manifest, per-file results and
dispositions are in [`tests/native-baseline/`](tests/native-baseline/). The
current native full suite has not been rerun since that baseline.

The set-size failure from that run has since been resolved. The original test
now passes with both the 1,000,000- and 999,999-element sets; the targeted
recheck took 130.40 seconds and peaked at 704,768 KiB. See the
[test log](tests/native-baseline/set-million-recheck.log) and
[scale report](bench/native-million-set/README.md).

## R7RS-small audit

[`R7RS-COMPATIBILITY.tsv`](R7RS-COMPATIBILITY.tsv) is the clause-organized
compatibility matrix. [`R7RS-SEMANTIC-AUDIT.tsv`](R7RS-SEMANTIC-AUDIT.tsv)
tracks standard-library obligations and their direct probes. Run the focused
gates with:

```sh
sh tools/test-r7rs-audit.sh
sh tools/test-r7rs-semantics.sh
sh tools/test-r7rs-semantics.sh --verify-recorded
```

The semantic snapshot is in [`tests/r7rs/semantic-results.tsv`](tests/r7rs/semantic-results.tsv).
Recorded passes are evidence for their individual probes, not a claim of full
R7RS-small conformance. Remaining gaps and unverified clauses stay in the two
matrices.

## Extension and performance evidence

The SRFI 151 arbitrary-integer tests are an extension gate, not an R7RS-small
requirement:

```sh
./bin/gf test tests/liii/bitwise/
./bin/gf test tests/srfi/srfi-151-test.scm
```

The current scale target and its reproducible records are in
[`bench/native-million-set/`](bench/native-million-set/). It matches the
original two-set test and completes in about 129 seconds with about 690 MiB
peak RSS. JSON parsing, serialization, lookup and key enumeration have separate
benchmarks and profiles in [`bench/json-phases/`](bench/json-phases/).

Earlier startup, vector-access and hash-table investigations remain in their
respective benchmark reports as historical evidence. Their old scale trials
and runner-development captures have been removed where the final benchmark
supersedes them. [`bench/README.md`](bench/README.md) is the index for active
performance records.
