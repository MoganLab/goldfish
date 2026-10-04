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
It uses a fresh isolated cache to exercise source bootstrap, then replays that
cache warm. A successful loop alone does not
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
2. Preserve ordinary `...` identifiers when another ellipsis marker is selected.

Native bootstrap selects the current content-addressed pipeline directory,
including runtime executable bytes and bootstrap sources. It validates every
required artifact's envelope, source/kernel stamp and recorded dependency
stamps before loading any cached library, including the deferred base library.
An invalid cache falls back to source bootstrap. Truncated or multi-record
cache files are cache misses and can be rebuilt automatically.

`./bin/gf --bootstrap-cache-directory` prints the current cache directory without
bootstrapping Scheme libraries. `./bin/gf --check-bootstrap-cache` validates it
and returns nonzero for a stale, incomplete or malformed cache. The warming
tool uses these checks and rebuilds invalid caches in isolation. The native
gate includes `tests/runtime/native-cache-test.cpp` and
`tools/test-bootstrap-cache.sh`, covering content changes with preserved file
size/time, missing dependencies, runtime changes, fingerprint parity and
automatic recovery from a damaged deferred base artifact.

The broader `tests/expander/lib-cache-all-libs-test.scm` probe currently loads
107 of 115 libraries. Eight failures involving `match`, JSON exports and
their consumers were reproduced on the pre-fix `1070cfa2` baseline as well.
This probe remains failing; the bootstrap freshness gate does not certify
all extension-library loading.

The numeric extension gate uses `./bin/gf test tests/liii/bitwise/` and
`./bin/gf test tests/srfi/srfi-151-test.scm`. It checks all 39 SRFI 151
procedures, arbitrary exact integers, infinite two's-complement behavior,
negative shifts and lengths, empty fields, and boolean conversions. SRFI 151
is an extension rather than a requirement of R7RS-small.

Run `./bin/gf benchmarks/bitwise-scale.scm 4096` for repeated monotonic
timings with checked results. Timing begins after bootstrap and compilation;
the benchmark also exercises a 200000-iteration tail loop.

`tests/liii/bitwise/scale-test.scm` checks 100000-bit integers, 4096-bit
list/vector round trips and 200000 tail iterations. SRFI 217 integer sets
retain their exact signed 64-bit fixnum domain; out-of-domain integers are
rejected rather than passed into the fixed-width trie.

[Recorded samples](benchmarks/bitwise-scale-results.tsv) compare the Scheme
scans at `48721b75` with native scans on an AMD Ryzen 7 7840HS, x86_64,
release build. Each sample measures 16 calls after bootstrap. At 4096 bits,
median dense population count fell from 483.19 ms to 0.575 ms; positive and
negative integer length fell from 242.03/252.19 ms to 0.534/0.570 ms.
These measurements describe this machine and workload. The 100000-bit
samples also validate their checksums; timings can vary under concurrent tests.
The compiled SRFI 151 wrappers call the registered native `bit-count` and
`integer-length` primitives directly.

The native library-source gate replaces a shared exported 4096-pair list,
then collects garbage. Module metadata and promoted environment bindings
must release the old value. The baseline retained 4104 objects; the regression
allows at most 128 incidental objects above the pre-allocation count.
