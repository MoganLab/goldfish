# Runtime contract and roadmap

Goldfish uses the native evaluator as its only runtime. Scheme defines language
libraries and policy; C++ provides values, memory management, control flow,
evaluation, reading, primitive calls, and platform interfaces.

## Runtime boundary

C++ owns:

- `Value`, object representation, heap/GC, symbol interning and environments;
- the lowered core evaluator, multiple values, exceptions, continuations and
  `dynamic-wind`;
- the source reader, ports, artifact loading and primitive ABI;
- OS and platform operations that cannot be implemented portably in Scheme.

Scheme owns:

- derived syntax, macros, records, guards and error-object behavior;
- library imports, module policy, compiler passes and cache orchestration;
- R7RS, SRFI and `(liii ...)` libraries.

New C++ primitives require a runtime or platform need that cannot reasonably
be expressed in Scheme. Optimize evaluator or library paths only after
measurement identifies a material bottleneck.

## Current implementation

- Native cold and warm bootstrap, source and cached library loading, CLI and REPL
  use `bin/gf`.
- The evaluator implements the 14 forms listed in `tools/substrate-baseline.txt`.
  Front-end lowering handles derived forms and named `let`.
- Native `call/cc` is multi-shot and preserves multiple values and shared
  environments. `dynamic-wind`, exception handlers, Scheme callbacks in the
  supported iteration primitives, and resource-backed port callbacks run
  through evaluator control frames. See `CONTROL-STACK.md`.
- Kernel sources under `goldfish/expander/kernel/` generate
  `goldfish/expander/kernel-combined.scm` with `sh tools/build-kernel.sh`.
- `sh tools/test-native-workflow.sh` is the representative native workflow gate.
  `sh tools/check-native-manifest.sh` validates its corpus and the follow-up ledger.

The native surface does not provide S7 inlet mutation forms, `*s7*`, hook
invocation, or procedure signature reflection. The two excluded test cases are
listed in `tests/NATIVE-FOLLOWUPS.tsv`.

## Active work

The test paths and decisions are tracked in `tests/NATIVE-FOLLOWUPS.tsv`.
This ordered plan defines the work:

1. **Bootstrap cleanup:** remove migration-only aliases, rejecting stubs and
   `LegacyLet` support after confirming generated artifacts and cold bootstrap
   no longer depend on them. Preserve only interfaces with an explicit runtime
   contract.
2. **Numeric tower:** first make integer overflow explicit; then complete
   inexact and complex behavior; then add bignum and ratio support with defined
   exactness, comparison and reader/writer rules. Use R7RS and independent
   implementations as references.
3. **Randomness:** implement native random state and seeding; keep SRFI-27 and
   `(liii random)` composition in Scheme; specify reproducibility and ranges.
4. **Reader extensions:** decide whether raw-string syntax is part of the
   supported `(liii ...)` surface. Implement and test it, or remove its exports
   and examples.
5. **Platform libraries:** decide support individually for subprocess, njson
   and UUID. Keep policy in Scheme and add only the necessary native platform
   operations.
6. **Scale and performance:** establish repeatable workload and memory
   baselines; optimize only measured bottlenecks. Run scale tests in a dedicated
   time slot.

A phase is complete when its follow-up rows are resolved, affected tests pass,
and the native workflow gate passes for changes to core evaluator, bootstrap,
reader or library loading. The full suite is reserved for explicit requests or
core semantic milestones.
