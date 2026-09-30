# Runtime contract

Goldfish uses the native evaluator as its only runtime. Scheme defines language
libraries and policy; C++ provides values, memory management, control flow,
evaluation, ports, primitive calls, and platform interfaces.

## Runtime boundary

C++ owns:

- `Value`, object representation, heap/GC, symbol interning and environments;
- the lowered core evaluator, multiple values, exceptions, continuations and
  `dynamic-wind`;
- port and reader primitives, artifact loading and the primitive ABI;
- OS and platform operations that cannot be implemented portably in Scheme.

Scheme owns:

- derived syntax, macros, records, guards and error-object behavior;
- source syntax reading, library imports, module policy, compiler passes and
  cache orchestration;
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

Deferred and excluded native tests are tracked in
`tests/NATIVE-FOLLOWUPS.tsv`.
