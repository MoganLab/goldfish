# Runtime layers

```text
CLI / loader → native evaluator → compiler → expander libraries → kernel → core IR
```

- **Runtime** (`src/runtime/`): values, heap/GC, environments, reader, core
  evaluator, continuations, primitive calls, artifacts and bootstrap.
- **Core IR** (`goldfish/core/ir.scm`): defines the 14 forms in
  `tools/substrate-baseline.txt`. Regenerate that inventory with
  `tools/freeze-substrate.sh` after changing the core contract.
- **Kernel** (`goldfish/expander/kernel/`): Scheme implementation of the
  expander foundation. Rebuild `kernel-combined.scm` with
  `sh tools/build-kernel.sh` after kernel source changes.
- **Expander and compiler** (`goldfish/expander/`, `goldfish/compiler/`): Scheme
  transformations that lower programs to core IR without depending on runtime
  internals.
- **Libraries** (`goldfish/scheme/`, `goldfish/srfi/`, `goldfish/liii/`): Scheme
  implementations of R7RS, SRFI and Goldfish APIs.

C++ implements engine mechanisms and platform boundaries. Scheme implements
language policy, macros, compiler passes and library behavior.

## Validation

- `sh tools/test-native-workflow.sh` checks native build, cold/warm bootstrap, CLI,
  REPL and the representative workflow corpus.
- `sh tools/check-native-manifest.sh` validates the corpus and
  `tests/NATIVE-FOLLOWUPS.tsv`.
- `sh tools/lint-layer.sh` checks core-form inventory and source dependencies.
