# C3 native migration readiness gate

Status: scope defined; C3 has not started. C3 follows the C2/M3 closeout and
the first measured native performance batches. It is a readiness gate before
the R4 default-runtime switch, not the switch itself.

## Goal and scope

C3 verifies that the native runtime can carry a representative, documented
Goldfish workflow from a clean build through library loading and evaluation,
with the host/s7 implementation serving only as a differential oracle where
the language contract calls for one.

In scope:

- Native cold bootstrap, library-source bootstrap, and warm-cache execution.
- A fixed C3 workflow corpus spanning `(scheme base)`, selected SRFIs, and
  selected `(liii ...)` libraries, including module imports, mutation,
  exceptions, multiple values, and file/port use.
- C2 strict parity for the supported float-free corpus and the M3 lowered
  program guard. Every excluded file must have an explicit disposition.
- Native CLI entry modes used by scripts (`-e`, file execution, imports) and
  the current REPL smoke path.
- Repeatable native performance baselines for evaluator calls, allocation/GC,
  and selected library hot paths. Measurements are regression evidence, not
  claims about all workloads.

Out of scope for C3:

- Switching `gf`'s default implementation or deleting s7/gf0. Those remain R4
  changes and require their own rollback-ready review.
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
3. The M3 `tests/gf0/m2a-*.scm` guard passes, or each failure is classified as
   a documented R7RS/s7-oracle difference or an actionable regression.
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
6. R4 switch prerequisites are recorded: how build/CI supplies bootstrap
   caches without host `gf`, what compatibility surface is removed, and which
   deferred features remain unsupported.

## Current baseline

- C2's recorded float-free sweep: 1,273 agreement passes, 58 explicitly
  bucketed files, no recorded divergence; see `tests/C2-ACCEPTANCE.md`.
- Post-change C2 regression slice: 36/36 strict agreement passes; the six
  environment/evaluator cases affected by the subsequent lookup optimization
  add 6/6 strict agreement passes (42/42 combined).
- M3 lowered-program guard: all 12 `m2a` files passed on 2026-09-27.
- Native performance: UTF-8 conversion and evaluator-loop baselines exist;
  measurements so far were taken on a shared, variable-load machine, so they
  are exploratory rather than release-grade.

Do not mark C3 complete by inheriting the C2 aggregate. C3 requires the entry
and exit evidence above against the then-current native driver and corpus.
