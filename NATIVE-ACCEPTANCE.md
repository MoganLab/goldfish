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

## Native full-suite baseline

The completed native run on 2026-10-05 (Asia/Shanghai), at source revision
`08354213`, discovered 1576 files: 1445 passed and 131 failed (exit 255).
It ran from 15:12:31 to 18:04:40 UTC on October 4, taking 2h 52m 9s.
The runtime binary stayed fixed; the locally modified bounded test driver is
identified separately by its SHA-256 in
[metadata.tsv](tests/native-baseline/metadata.tsv).

The native full-suite record lives in `tests/native-baseline/`: the discovery
manifest freezes the corpus, `results.tsv` links every verdict to the complete
run transcript, and `dispositions.tsv` accounts for failures, environment
guards and earlier migration decisions. The archived C3 table preserves the
old decisions rather than treating them as current exclusions.

[Results](tests/native-baseline/results.tsv) preserve the original verdicts.
[Dispositions](tests/native-baseline/dispositions.tsv) account for all 131
failures: 78 deferred repairs or test migrations and 53 exclusions for native
njson handles, advanced subprocess capture, hooks and signature reflection.
Fifteen earlier file-level exclusions/deferrals are retired after passing.
Eight HTTP files passed their opt-out guards; a ninth failed on JSON import
before reaching its guard. None establishes live HTTP coverage.

The next repairs are usable `match`, JSON and R5RS re-export bindings;
numeric association/membership equivalence; circular list traversal; missing
Scheme operations and typed-vector families; fractional sleep and consistent
error contracts. Legacy byte-index, callable-container and multiple-value
splicing expectations have separate test-migration decisions. The existing
19 R7RS semantic gaps remain tracked in their independent audit.

Incomplete workers recovered through isolated runs. Million-element set
construction and circular list conversion remained bounded failures. A large
Unicode worker exhausted its chunk budget, but all four isolated files passed
(7921 assertions); `char-alphabetic?` separately passed 2800 assertions while
taking about 11 minutes in its worker. Profile loading and worker costs before
treating a budget failure as evidence of an incorrect character predicate.
The original transcript retains buffered/interleaved output as emitted;
its final per-file summary supplies the authoritative verdicts.

```sh
GOLDFISH_CACHE_DIR=/tmp/goldfish-full-cache sh tools/warm-bootstrap-cache.sh
GOLDFISH_CACHE_DIR=/tmp/goldfish-full-cache GOLDFISH_TEST_TIMEOUT=300 ./bin/gf test --all --jobs=8
sh tools/check-native-baseline.sh
```

The timeout includes isolated process startup. Worker chunks receive the
per-file limit multiplied by chunk size and fall back to individually bounded
runs if incomplete. Timeouts are failures. `NATIVE-FOLLOWUPS.tsv` records
ownership decisions; the full runner does not use it to suppress files.
HTTP tests require `GOLDFISH_TEST_HTTP` and cannot count as live HTTP coverage
when the variable is unset. The SimpleTex API assertions have a separate
credential guard. This baseline covers discovered `tests/**/*-test.scm` files,
not C++ targets, tests under `tools/`, or the separately named R7RS semantic
audit corpus. Those gates retain their own evidence and known gaps.

The historical host/S7 result of 1555 passing files belongs to the executable
before native migration. It is not a result from this native run, and its
corpus differs from the current discovery manifest.

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
checks, and runs the fifteen-file corpus with both cold and warm bootstrap.
It uses a fresh isolated cache to exercise source bootstrap, then replays that
cache warm. A successful loop alone does not
establish proper tail recursion; native tests also compare continuation frame
counts at different recursion depths, including promise forwarding.

Known failures, when present, remain separate, standard-expected probes:

```sh
sh tools/test-r7rs-audit.sh --gaps
```

The six original compatibility probes now pass in the regular audit corpus;
their original gap manifest is empty and this command returns success without running tests.
This does not establish complete R7RS conformance. Library order checks require dependencies to precede
consumers; they do not impose an implementation-independent load-once rule.

The standard-library and basic-semantics audit is recorded separately in
[R7RS-SEMANTIC-AUDIT.tsv](R7RS-SEMANTIC-AUDIT.tsv), with individual obligations,
standard references, direct probe files and explicit boundaries. Its
[cold/warm snapshot](tests/r7rs/semantic-results.tsv) contains 128 observations:
109 pass and 19 gaps. The export fixture comes from the report's Appendix A,
independently of implementation exports. All required exports are present in
15 of 16 libraries; `(scheme r5rs)` lacks `...`, `=>`, `_`, `else` and
`syntax-rules`. Extra base/time exports are reported separately. Available
bindings alone do not certify procedure semantics.

```sh
sh tools/test-r7rs-semantics.sh
sh tools/test-r7rs-semantics.sh --verify-recorded
```

The first command keeps standard expectations and returns failure for known
gaps. The second verifies that both statuses and actual results reproduce in
cold and warm runs; its success is a reproducibility check, not a conformance
pass. `--record` updates the snapshot only after the two runs agree and still
returns failure when gaps remain. Circular `list?` is isolated with a 60-second
timeout; a timeout counts only after the probe reaches its target operation.
The matrix checker rejects known gaps marked `audited` and requires direct
probe evidence for every linked clause.

The next fixes are grounded in these probes:

1. Terminating circular `list?`, overlapping bytevector copying, and internal
   multiple-value/record definitions through standard `eval`.
2. Reader numeric prefixes, string continuations and read-error classification;
   numeric exactness in `equal?` and signed-zero behavior in `eqv?`.
3. Unicode full case folding and digit values; port close idempotence,
   multiple-value forwarding, CR line endings, EOF, optional flush arguments
   and file-error classification; the missing R5RS syntax exports.

This corpus samples numeric boundaries, expressions, quasiquote, data and
ports. It does not certify every procedure, record-definition context,
invalid UTF-8 input, complex branch cut, clock or process-context operation.
Unspecified outcomes are not forced to a particular result, and textual and
binary port categories are allowed to overlap. No native full-suite run is
claimed for this audit.

Custom ellipsis regressions cover ordinary `...` pattern variables, literal
identifiers and free template references, empty and nested repetitions, vectors,
dotted tails, multiple rules and generated macros. Variables are renamed per
rule; inactive marker suppression is removed when producing generated syntax.

Library declarations support nonnegative exact integer name components, nested
`include-library-declarations`, declaration `cond-expand` and library availability
requirements. Includes search the containing file's directory before the load
path, and `include-ci` uses the reader's `#!fold-case` behavior. Cache records
track included sources and availability queries, including transitive macro
providers. `tools/test-library-declaration-cache.sh` checks warm replay, include
edits/deletion and changes in optional library availability across processes.

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

The last recorded run of `tests/expander/lib-cache-all-libs-test.scm` loaded
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
