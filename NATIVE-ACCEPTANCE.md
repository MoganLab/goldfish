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
failures. Initially these were 78 deferred repairs or test migrations and 53 exclusions for native
njson handles, advanced subprocess capture, hooks and signature reflection.
Fifteen earlier file-level exclusions/deferrals are retired after passing.
Eight HTTP files passed their opt-out guards; a ninth failed on JSON import
before reaching its guard. None establishes live HTTP coverage.

The subsequent [targeted recheck](tests/native-baseline/migration-recheck.log)
ran ten proposed test migrations: eight passed and two failed (driver exit 255).
The eight passing changes are committed: character-based string slices,
padding and Han alphabetic expectations; default-hash contract assertions;
and inspection of the string raised by `vector-sorted?`. The two proposed
`string-take` edits initially exposed native `substring` error-tag failures
after fixing their Unicode offsets. The later primitive repair now passes
both files; the original tested [patch](tests/native-baseline/migration-tested.patch)
and failed recheck remain archived.
Current decisions are 45 locally verified fixes, 33 deferrals and 53
exclusions. Subsequent repairs restore match and JSON definitions, JSON numeric
classification and UTF-8 output, R5RS re-export order, and compiler assertions.
The default changed-since-main gate passes 143 files; the JSON directory passes
20 files. Direct IR checks now end with check-report so assertion failures
propagate to the runner. Evidence is retained in json-recheck.log and
library-default-recheck.log. The cached library recheck loads all 115 libraries,
passes both match suites and eval-when, and reaches the ninth HTTP opt-out
guard. A separate empty-cache aggregate attempt hit its 600-second budget
after passing six consumers; it is not counted as a complete passing gate.
No second full run is claimed.

The prerequisite primitive repairs are recorded in
[primitives-recheck.log](tests/native-baseline/primitives-recheck.log): 37 files
and 788 assertions pass. They share exactness- and signed-zero-sensitive
numeric equality across predicates and lookups, reject circular cdr chains
with constant auxiliary space, and preserve character-index substring error
tags. Numeric output preserves inexact zero imaginary parts so component
exactness and signed zeros survive reader and cache round trips. Real-domain
atan/log and inexact conversion use the real representation; inexact inputs
retain their observable components; JSON encodes real numeric
values without complex notation. Adjacent numeric test migrations preserve
required exactness and use numerical comparison only where exactness is
implementation-dependent. The [tested source patch](tests/native-baseline/primitives-tested.patch)
and executable hash identify the local runtime used for these checks.
The final [default gate](tests/native-baseline/primitives-default.log) passes
159 files; the [native workflow](tests/native-baseline/primitives-workflow.log)
passes reader, cache recovery, CLI, ten corpus files and cold cross-library
execution. The [audit recheck](tests/native-baseline/primitives-audit.log)
reproduces 113 passes and 15 known gaps in cold and warm caches.

Remaining repairs include missing Scheme operations and typed-vector families;
fractional sleep and consistent
error contracts. Legacy byte-index, callable-container and multiple-value
splicing expectations have separate test-migration decisions. The remaining
15 R7RS semantic gaps remain tracked in their independent audit.

In the original run, incomplete workers recovered through isolated runs.
Million-element set construction and circular list conversion were bounded
failures; the circular-list cases now pass their targeted recheck. A large
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
113 pass and 15 gaps. The export fixture comes from the report's Appendix A,
independently of implementation exports. All required exports are present in
all 16 libraries, including the restored R5RS syntax exports. Extra base/time exports are reported separately. Available
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

1. Overlapping bytevector copying and internal
   multiple-value/record definitions through standard `eval`.
2. Reader numeric prefixes, string continuations and read-error classification.
3. Unicode full case folding and digit values; port close idempotence,
   multiple-value forwarding, CR line endings, EOF, optional flush arguments
   and file-error classification.

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

The original `tests/expander/lib-cache-all-libs-test.scm` probe loaded
107 of 115 libraries. After repairing match and JSON definitions, the
[cached recheck](tests/native-baseline/library-cached-recheck.log) loads
all 115 libraries. This is focused library-loading evidence; it does not
certify every extension-library procedure.

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

## Performance and scale preparation

Build `gf-native`, finish the affected semantic regressions, and run:

```sh
sh tools/bench-native-scale.sh --smoke --output=/tmp/goldfish-scale-smoke
sh tools/bench-native-scale.sh --samples=3 --output=/tmp/goldfish-scale-quick
sh tools/bench-native-scale.sh --full --samples=3 --timeout=300 --output=/tmp/goldfish-scale-full
```

The output directory must be empty. The runner uses GNU time and GNU timeout
on Linux. Each sample starts a new native process and records elapsed/user/
system seconds, peak RSS in KiB, exit code and semantic check result, with
separate stdout/stderr. Metadata identifies the executable and input hashes,
source revision and local patch, machine, optimization level and GC setting.
Bootstrap-cache preparation is outside the measurements; timed workloads
include process startup and their checks. Run benchmarks alone on an idle
machine and compare individual samples as well as their medians.

| Case | Quick size | Full size | Checked behavior |
| --- | ---: | ---: | --- |
| Cold/warm startup | 1 | 1 | Evaluate `(+ 1 1)` |
| Cold/warm compilation | 32 definitions | 512 definitions | Compile the same generated source into a lowered program artifact |
| Allocation / GC | 50 × 1,000 vector slots | 50 × 100,000 vector slots | Retained allocation contents |
| Long list | 1,000 elements | 1,000,000 elements | Proper-list detection, length and reverse |
| Wide vector | 4,096 slots | 1,000,000 slots | Fill, copy and final element |
| Deep structure | 64 levels | 2,048 levels | Independent nested vectors compare equal and preserve depth |
| Set construction | 10 elements | 1,000,000 elements | Cardinality, present and absent membership |

Cold startup uses an empty Goldfish cache; warm startup uses a complete cache.
This does not flush the operating system's page cache. Cold compilation copies
the prepared bootstrap and workload cache for each sample; warm compilation reuses
that sample's generated artifact. A failed cold compilation blocks its paired
warm sample. Compilation checks artifact shape and executes the generated source outside
the measured interval; broader compiler semantics remain
covered by their separate regression tests. Allocation runs use the selected
GC mode without forcing a collection, so these measurements characterize
allocation with that collector rather than isolated GC pause times.

The [recorded smoke](bench/native-scale-baseline/results.tsv) passes all nine
cases and both compiled-program execution checks. Its metadata and individual
logs retain the measured executable, input hashes and time/RSS samples.
Workloads are libraries with explicitly imported entry points; cache preparation
imports them without running their bodies. One sample per case verifies the
runner, not statistical performance or million-element behavior.

A separate [entry probe](bench/native-scale-baseline/entry-probes.tsv) using
100 set elements reached its 180-second limit; a later ten-element probe
passed. These checks include library import and are not isolated construction
measurements. The quick profile uses ten elements; the full profile retains
one million. Profile import/compilation and collection work separately before
attributing the timeout to a particular implementation hotspot.

Timeouts and semantic failures remain failed rows and make the runner return
nonzero. The smoke profile checks the harness and small workloads; it is not
million-element evidence or an optimization result. The full profile is the
next dedicated performance run. Choose optimizations from measured hotspots
and keep the affected semantic regressions green for every change.

The runner also accepts `--case=NAME`, `--size=N`, `--setup-timeout=N`
(default 300 seconds), `--run-timeout=N` (default 1800 seconds), and
`--cache=DIR` to copy a seed cache into the isolated run. Every preparation
command is bounded, including copying compile caches. An outer deadline covers
preparation, generation, samples and validation; termination gets a five-second
kill grace. Selecting warm compilation also runs its required cold sample.
Only the selected workload libraries are prepared. `stages.tsv`,
`current-stage.txt` and `run-status.tsv` distinguish preparation, sample and
overall termination. A timeout may exit 124, or 137 after forced termination.
[Bound controls](bench/native-scale-bounds/controls.tsv) exercise all three
limits and a passing selected 32-element list. Partial logs remain available
when the outer deadline interrupts a stage.

For startup/import/execution attribution, use:

```sh
sh tools/bench-native-scale.sh --smoke --case=million-set --size=10 --phases --timeout=40 --setup-timeout=60 --run-timeout=150 --output=/tmp/goldfish-set-phases
sh tools/bench-native-scale.sh --smoke --case=compile-warm --size=4 --phases --timeout=50 --setup-timeout=60 --run-timeout=180 --output=/tmp/goldfish-compile-phases
```

`--cache=DIR` can copy an existing bootstrap cache, but stamps are still
validated. `--phases` instruments set and compilation samples. It feeds one
form at a time to the stateful native REPL so import expansion happens between
timer markers, rather than before a whole program's first expression.
Phase samples use the `r7rs` REPL with explicit timer imports; ordinary
compilation samples use `liii`. Compare their inner compiler intervals rather
than treating the two process totals as measurements of identical startup.
`phases.tsv` uses the existing monotonic nanosecond clock. The startup row
includes process launch and importing the timer; other rows measure import,
list generation, set construction/checks, or compilation. Peak RSS remains a
whole-process measurement. Failed phases retain a flushed begin marker and
an `incomplete` row when the sample timeout returns. An overall interruption
can leave only raw phase markers in the stdout log.

`--import-state=source` removes just the selected benchmark library artifact
before each phase sample, including repeated samples and paired compilation.
Its dependency and timer artifacts stay warm. This is a source-versus-cache
comparison for the benchmark library, not a completely cold dependency graph.
Preparation, timer/control overhead and semantic validation are distinguished
from the inner compile/construct intervals. The ordinary uninstrumented path
remains available for later performance comparisons.

The [phase evidence](bench/native-scale-phases/set10/phases.tsv) shows a
10-element sample spending 20.30 seconds on startup/timer import, 3.45 seconds
on importing the set benchmark, and 0.00128 seconds on construction. The
[100-element sample](bench/native-scale-phases/set100/phases.tsv) completes
startup (21.66 seconds), import (3.55 seconds), and list generation (0.00041
seconds), then times out in construction at the 40-second process limit.
These single samples locate the failure; they are not throughput estimates.
The [compile pair](bench/native-scale-phases/compile/phases.tsv) separates
4.40 seconds of cold compilation from 0.0758 seconds of cached reading;
both compiled programs execute successfully. Two
[source-import samples](bench/native-scale-phases/source/phases.tsv) separately
reset the selected artifact and complete import in 4.29 and 4.50 seconds.

The [native resize probe](bench/native-scale-phases/resize/native-output.log)
reports five keys but eleven stored bucket entries after growing a two-bucket
table. Reading the current `%s7-ht-resize!` definition shows `bucket-loop`
inside `cell-loop`, and publishing the new vector inside `bucket-loop`.
Repeated suffix traversal duplicates entries. A separate read-only Guile
algorithm probe transfers eight original entries into 255 bucket entries.
It is supporting algorithm evidence; the native probe and bounded construction
are the runtime evidence. This is a repair prerequisite, not merely a request
for a faster million-element implementation. The original set-size full-run
failure remains deferred, with this concrete diagnosis. Repair rehash traversal
and uniqueness before increasing collection sizes; then measure growth and
profile any remaining cost. Startup/cache replay is also a measured fixed cost,
but is not the cause of this construction timeout.

The deadline supervisor cleans the entire worker process group after it exits,
including preparation descendants whose shell ended first, and handles explicit
interruption. A [heartbeat control](bench/native-scale-phases/descendants/heartbeat-control.tsv)
uses a TERM-ignoring preparation child and verifies that it cannot continue
writing after the runner returns. All investigation runs stay below a total
budget; no million-element test was repeated.
