# C3 native migration readiness gate

Status: complete (2026-09-27). C3 follows the C2/M3 closeout and the first
measured native performance batches. It is a readiness gate before the R4
default-runtime switch, not the switch itself.

## Goal and scope

C3 verifies that the native runtime can carry a representative, documented
Goldfish workflow from a clean build through library loading and evaluation.
Host/s7 served as a differential oracle during migration; it was retired on
2026-09-28 after the paired checks documented below.

In scope:

- Native cold bootstrap, library-source bootstrap, and warm-cache execution.
- A fixed C3 workflow corpus spanning `(scheme base)`, selected SRFIs, and
  selected `(liii ...)` libraries, including module imports, mutation,
  exceptions, multiple values, and file/port use.
- Historical C2 strict parity for the supported float-free corpus and M3
  lowered program guard. Every excluded file has an explicit disposition.
- Native CLI entry modes used by scripts (`-e`, file execution, imports) and
  the current REPL smoke path.
- Repeatable native performance baselines for evaluator calls, allocation/GC,
  and selected library hot paths. Measurements are regression evidence, not
  claims about all workloads.

Out of scope for C3:

- The default switch and gf0 evaluator retirement were completed during R4.
  The S7 host/oracle target has since exited; deleting the remaining bridge
  and vendored source is a separate R4 step with its own review.
- Reproducing s7-only hooks, signature introspection, rootlet mutation, or
  other APIs explicitly rejected by `RUNTIME_CONTRACT.md`.
- Requiring every deferred numeric-tower feature before C3. Float/inexact,
  bignum, and ratio work follows the ordering already recorded in the runtime
  contract; the C3 corpus must state which numeric surface it exercises.

## Entry criteria

1. The C2 manifest and skip manifest validate with
   `sh tools/check-c2-manifest.sh`.
2. The latest full C2 aggregate and every post-change strict slice are recorded
   with their exact scope; no claim treats a bucketed test as a pass.
3. The M3 gf0/S7 comparison passed before the gf0 evaluator was retired in R4;
   its result is retained as historical migration evidence, not a current gate.
4. The native performance probes emit nonzero raw monotonic samples and
   deterministic workload results. Timing variance is characterized on the
   same machine; a measurement that truncates to zero is invalid evidence.

## Exit criteria

1. `tools/test-native.sh` passes, including native reader, source bootstrap,
   and cold-bootstrap checks. The exact cache state and commands are recorded.
2. Every C3 workflow test passes natively. Differential tests have zero missing
   verdicts and zero unexplained divergences. R7RS-required behavior is checked
   against the specification when s7 differs.
3. Each C2 skip has one reviewed disposition: migrate before R4, explicitly
   exclude from the native surface, or defer with a named follow-up milestone.
   There are no unowned or reasonless exclusions.
4. CLI script execution, `-e` evaluation, module imports, and REPL smoke tests
   work through the native driver without accidentally falling back to s7.
5. Performance changes include the workload, raw repeated samples, profiler
   evidence, and correctness checks. A regression over 10% on a dedicated,
   comparable machine blocks the relevant optimization; noisy shared-machine
   measurements are exploratory and must not be used as a release threshold.
6. R4 switch prerequisites are recorded: how the local build/release process
   supplies bootstrap caches without host `gf`, what compatibility surface is
   removed, and which deferred features remain unsupported.

## Current baseline

- C2's recorded float-free sweep plus the later strict call/cc and
  match-capability slices:
  1,277 agreement passes and 54 explicitly bucketed files, with no recorded
  divergence; see `tests/C2-ACCEPTANCE.md`. This is recorded evidence, not a
  fresh full C2 run.
- Post-change C2 regression slice: 36/36 strict agreement passes; the six
  environment/evaluator cases affected by the subsequent lookup optimization
  add 6/6 strict agreement passes (42/42 combined).
- M3 lowered-program guard: all 12 `m2a` files passed on 2026-09-27.
- Native performance: UTF-8 conversion and evaluator-loop baselines exist;
  an allocation/GC probe now records five deterministic allocation samples
  and collector stats. All performance measurements so far were taken on a
  shared, variable-load machine, so they are exploratory rather than
  release-grade; see `bench/native/README.md`.

## C3 kickoff record (2026-09-27)

- `sh tools/check-c2-manifest.sh`: passed; 1,331 in-scope files and 58
  bucketed files validated.
- `sh tools/diff-gf0-m2a.sh`: passed all 12 lowered-program cases before its
  R4 retirement; see `tests/C2-ACCEPTANCE.md`.
- `sh tools/test-native.sh`: passed the native reader, library-source, and
  cold-bootstrap checks. The warm-cache phase used
  `/home/jinser/.cache/goldfish/ccache/v240655d0cf85/`. The cold check used an
  isolated empty cache under `/tmp`; its `native bootstrap cache not found`
  diagnostic is expected and exercises the source-bootstrap fallback. Cold
  `-e` evaluation returned 42, followed by source loading of the bootstrap,
  CLI, and environment fixtures.

At kickoff, these checks alone did not establish the workflow corpus or close
the 58 skip dispositions. The corpus, skip review, CLI/REPL evidence, and
subsequent C3 closeout results are recorded below.

## Fixed workflow corpus (2026-09-27)

The fixed corpus is `tests/c3-native.manifest` (9 test files, including one
cross-library workflow):

- `(scheme base)`: call-with-port, multiple values, exception handlers, and
  call-with-input-file.
- `(scheme eval)`: eval and imported environments.
- `(srfi srfi-13)`: Unicode-aware prefix operations.
- `(liii path)` and `(liii string)`: path composition and string joining.
- `tests/c3/native-workflow.scm` ties together explicit imports from all
  four library families, mutation, exception handling, multiple values,
  Unicode string operations, and path composition. File-backed port behavior
  is exercised by `call-with-input-file-test.scm`.

`sh tools/test-native-c3.sh` builds the `gf-native` target (executable
`bin/gf`), runs the native bootstrap
gate, checks `-e`, file execution, and a stateful REPL session, runs the corpus
through `gf --each-file`, and loads the cross-library workflow from an
isolated empty cache. On 2026-09-27 every step passed; all 9 files passed
(118 checks in the 9-file corpus, plus 7 checks in the isolated cold-cache
run). Before oracle retirement, the same 9-file scope passed strict host/native
comparison in one batch: 9 agree-pass, 0 agree-fail, 0 divergences, and 0
missing verdicts. The host side loaded each test in a fresh `gf -m liii -e`
process because its CLI had no `test` subcommand. Raw summaries are retained at
`/tmp/c3-native-final.log` and `/tmp/c3-strict-final.log` on the validation
machine. This is a reproducible baseline for the selected workflow, not a
substitute for the full C2 manifest.

## C2 skip dispositions (2026-09-27)

Every row in `tests/c2-skip.tsv` now has an individual disposition in
`tests/C3-SKIP-DISPOSITIONS.tsv`; `sh tools/check-c3-manifest.sh` verifies
one-to-one path coverage, non-empty rationale and milestone fields, and
valid corpus paths. Current review assigns 2 explicit S7-only surfaces to
exclude and 52 cases to named follow-up milestones. No current disposition row
is marked `migrate-before-R4`; the former call/cc exclusions were implemented
and added to the strict parity slice.

- `R4-removal`: remove S7 hook invocation and procedure-signature metadata;
  these are explicitly outside the native contract.
- The slow match-capability test's strict paired host/native pass is recorded
  in `tests/C2-ACCEPTANCE.md`; it is historical and no longer an R4 audit item.
- `R5-numeric-tower`, `R5-random`, `R5-reader-extensions`,
  `R5-platform-extensions`, and `R5-scale-and-GC`: own the remaining numeric,
  PRNG, raw-string syntax, optional OS-backed libraries, and million-element
  stress coverage after the initial native cutover. Their exact files and
  required work are listed row-by-row in the disposition ledger.

The ledger is a reviewed scope decision, not evidence that deferred
functionality already works. The slow-case paired verdict is now complete;
the 52 deferred cases and 2 exclusions are the
documented initial native cutover boundary.

## Native bootstrap cache (2026-09-27)

`tools/warm-bootstrap-cache.sh` now uses the default native `gf` and
`compile-file-cached` to build the workflow's transitive library cache in an
isolated cache directory, verifies every artifact listed by
`src/runtime/bootstrap.cpp`, then installs it under the content-addressed
directory for the running native binary. The C3 gate exercises this path from
an unavailable warm cache and then exercises source bootstrap from an empty
cache. Native artifacts use the separate
`~/.cache/goldfish/native-ccache` root; the host's `ccache` remains independent.
On this run the complete cache was
`/home/jinser/.cache/goldfish/native-ccache/va89bf51c78ee` (56 artifacts).
The cache-generation prerequisite is therefore implemented and validated
without host `gf`; CI/release integration still needs to call this script or
preserve its generated cache artifact when R4 changes the default runtime.

## R4 closeout record

- The long match-capability paired verdict was obtained on 2026-09-28 and is
  archived in `tests/C2-ACCEPTANCE.md`; it is no longer pending.
- `sh tools/test-native-c3.sh` remains the local native workflow gate. It builds the native
  driver, generates and verifies the bootstrap cache with
  `tools/warm-bootstrap-cache.sh`, and checks cold bootstrap without host `gf`.
- Repeat performance probes on a reserved, comparable machine before using
  their exploratory numbers as release thresholds.

Do not mark C3 complete by inheriting the C2 aggregate. C3 requires the entry
and exit evidence above against the then-current native driver and corpus.
The paired results in this file and `tests/C2-ACCEPTANCE.md` are historical
migration evidence. Oracle retirement means they can no longer be rerun.
