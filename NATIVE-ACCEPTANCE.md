# Native runtime verification

## Routine gate

Run:

```sh
sh tools/test-native-workflow.sh
```

This builds the native `gf`, checks warm and cold bootstrap, exercises `-e`,
file execution and a stateful REPL, then runs the fixed workflow corpus in
`tests/native-workflow.manifest` including an isolated-cache cross-library workflow.

Validate test ownership and paths with:

```sh
sh tools/check-native-manifest.sh
```

## Acceptance

- The native build and bootstrap workflow completes.
- Every file in `tests/native-workflow.manifest` passes.
- Each deferred or excluded test has one current decision in
  `tests/NATIVE-FOLLOWUPS.tsv`.
- Core semantic, bootstrap, reader or library-loader changes also pass the
  affected test subset and this native workflow gate.
- Full-suite runs are scheduled separately because of their cost.

## R7RS compatibility audit

[R7RS-COMPATIBILITY.tsv](R7RS-COMPATIBILITY.tsv) follows the clause numbering
of the [corrected R7RS-small report](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-1.html).
`audited` records passing focused regressions, not complete conformance for
the clause. `partial` identifies a known incompatibility. `inventory` records
existing coverage without certifying it in this audit; `unverified` identifies
areas still needing systematic review. Evidence paths and boundaries accompany
every row. Historical host/S7 results are not native conformance evidence.

Run the macro hygiene, tail-call, continuation, promise and library-loading
audit with:

```sh
sh tools/test-r7rs-audit.sh
```

The gate checks matrix paths, executes native continuation-frame and eval-machine
checks, and runs the seven-file corpus with both cold and warm bootstrap.
It always uses a fresh isolated cache: the current native bootstrap can select
an older complete version after source edits. A successful loop alone does not
establish proper tail recursion; native tests also compare continuation frame
counts at different recursion depths, including promise forwarding.

Known failures remain separate, standard-expected probes:

```sh
sh tools/test-r7rs-audit.sh --gaps
```

This command currently returns nonzero. Its six probes cover
ordinary `...` under a custom ellipsis
marker, numeric library names, `include-ci`,
`include-library-declarations`, declaration-level `cond-expand`, and
`cond-expand` library availability requirements. They are neither passing
coverage nor CI skips. Library order checks require dependencies to precede
consumers; they do not impose an implementation-independent load-once rule.

Follow-up priorities from the audit:

1. Normalize library declarations before body expansion, including declaration
   splicing, case-folded includes and numeric name components; support library
   requirements in `cond-expand`.
2. Preserve ordinary `...` identifiers when another ellipsis marker is selected,
   and validate bootstrap cache freshness before loading a complete cache.

The numeric extension gate uses `./bin/gf test tests/liii/bitwise/` and
`./bin/gf test tests/srfi/srfi-151-test.scm`. It checks all 39 SRFI 151
procedures, arbitrary exact integers, infinite two's-complement behavior,
negative shifts and lengths, empty fields, and boolean conversions. SRFI 151
is an extension rather than a requirement of R7RS-small.

Run `./bin/gf benchmarks/bitwise-scale.scm 4096` for repeated monotonic
timings with checked results. Timing begins after bootstrap and compilation;
the benchmark also exercises a 200000-iteration tail loop.
