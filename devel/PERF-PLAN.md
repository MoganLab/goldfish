# Goldfish performance program

Performance is treated as one engineering discipline, not a set of one-off
patches: shared instruments, one regression gate per area, explicit phase
exits. Work proceeds one major phase at a time; a phase is finished only when
its exit gate is met, then the next phase starts.

## 0. Principles

These constrain every optimization (not negotiable per workstream):

1. **One cache, one compile.** Standard libraries and user libraries go
   through the same cache and the same compiler. No special-casing.
2. **No performance pre-expansion of non-kernel modules.** The kernel is
   pre-expanded only because bootstrap requires it; every other module is
   cached through the normal mechanism.
3. **Distribution precompiles the cache** (Guile `.go` model): the shipped
   cache is produced by that same mechanism, not a separate format.
4. **Semantics first.** An optimization that changes behavior, or that cannot
   be A/B measured, is reverted. Cache-validity and recovery behavior are part
   of the contract.

## Working agreement

- One major phase active at a time. Within it, items land as separate commits,
  each A/B measured, gated, and reverted if flat.
- Gates before every landing: affected native tests; the changed-since gate
  (`GOLDFISH_CACHE_DIR=<isolated> ./bin/gf test`) whenever core
  expander/library code changed; `bench/cold-start/run-suite.sh` +
  `compare.sh` for anything touching boot or compile; `--all` only when
  landing core semantics.
- Every binary rebuild changes the content-addressed cache version: re-warm
  before measuring (cold run ~40 s, then warm ~3.5 s), or use an isolated
  `GOLDFISH_CACHE_DIR` so a stale cache cannot mask a change.
- `bench/` records keep raw logs, metadata and checksums.

## Reference point

Pre-native: ~200 ms cold / ~60 ms warm. Native, measured 2026-10-06 after
Phase 1: cold ~62 s (empty cache), warm ~3.5 s, warm RSS ~32 MiB, cold peak
RSS ~91-103 MiB (Phase 0 measurement; cold peak rose ~5-9% on two workloads
with Phase 1, warm RSS unchanged).

## Targets

| metric | now | near (exit Ph 3) | mid (exit Ph 5) | stretch (Ph 6) |
|---|---:|---:|---:|---:|
| warm startup | 3.5 s | < 1 s | < 300 ms | < 60 ms |
| cold, shipped cache | n/a | ≈ warm | ≈ warm | ≈ warm |
| cold, no cache | ~62 s | < 10 s | < 3 s | — |
| warm RSS | 32 MiB | ≤ 32 MiB | ≤ 32 MiB | — |
| runtime throughput | unmeasured | baseline + gate | −10% time | — |

## Program map

| phase | goal | status |
|---|---|---|
| 0. Measurement & gates | instruments before optimization | **DONE** 2026-10-06 |
| 1. Compile pipeline | reader / expander / optimizer / serializer | **DONE** 2026-10-06 |
| 2. Startup structure | installer cacheable; shipped-cache cold ≈ warm | **ACTIVE** (2a done; 2c step 2 next; 2b folded into 3) |
| 3. Evaluator & runtime | runtime throughput; warm < 1 s | SCOPED (profile + levers ready; Appendix B) |
| 4. Memory | peak/steady RSS | later |
| 5. Cache unification & IO | one cache front door, one bundle kind | later |
| 6. Runtime image | design doc + feasibility prototype | research, later |

## Phase 0 — Measurement and regression infrastructure [DONE]

Exit met 2026-10-06: one command produces startup + micro-benchmark numbers
and flags regressions against a stored baseline. Instruments, all committed:

- Startup suite: `sh bench/cold-start/run-suite.sh OUTDIR N` (fixed workloads
  × cache states, medians).
- Regression gate: `sh bench/cold-start/compare.sh BASE CAND [tol%]` — per-
  (workload,state) wall/RSS medians; non-zero exit on regression or failed run.
- Micro-benchmarks: `sh bench/micro/run.sh OUT` (reader, serialize/
  deserialize, eval, alloc; interleaved min-of-N). Expander/optimizer splits
  come from `GOLDFISH_DEBUG=timing` on a program compile.
- Symbol-resolved profiler: `sh tools/build-prof.sh` → `bin/gf-prof`;
  `sh tools/prof.sh bin/gf-prof <args>`.
- Per-library load ledger: `bench/cold-start/lib-timing.sh` (+ LIB-LEDGER.md);
  the timing marks perturb boot, so re-add them only for a measurement run.
- Distribution warm-up: `sh tools/warm-bootstrap-cache.sh` (boot set +
  optimizer artifacts; see CCACHE_NOTES.md).

## Phase 1 — Shared compile pipeline [DONE]

Exit met 2026-10-06: every stage has a micro-benchmark; four landed changes
moved cold compile −11..−12%, warm −20..−32%, bootstrap −17..−44%, readonly
−17..−43% (records in Appendix A). The same pipeline serves cold startup,
first compile of any library, fingerprinting and program compilation.

Key finding (Appendix B): the expander and optimizer are Scheme, so pipeline
cost is evaluator/GC cost — Phase 1 and Phase 3 converge on the evaluator.
The landed int fast paths serve both; the remaining levers are structural and
wait for Phase 3.

## Phase 2 — Startup structure [ACTIVE — current major phase]

Goal: make the bootstrap installer cacheable so warm boot drops to the
cached-replay floor (~1.5-1.7 s), and a distribution-precompiled cache makes a
fresh install's cold ≈ warm. The remaining distance to warm < 1 s is evaluator
work (2b below), so that number closes in Phase 3 — by design, not by gap.

### 2a — Distribution precompile [DONE]

`tools/warm-bootstrap-cache.sh` also requires the optimizer pipeline
(`native_precompile_artifacts`, 7 files) alongside the boot set (12), so a
shipped cache gives a warm first program compile. The optimizer artifacts are
not in the boot-required set (validating them every start would re-parse the
large compiler artifacts). Ship/install procedure: `CCACHE_NOTES.md`.

### 2b — Cached-replay throughput [FOLDED into Phase 3]

`load-cached-runtime` + `standard-library` + `mode-imports` (~1.6 s) profile
as evaluator work: deserialize/eval/macro-rebuild and binding re-import. No
standalone change exists; it lands with the Phase 3 levers and is measured
there as the warm-boot line.

### 2c — Cacheable bootstrap installer [ACTIVE]

Warm boot is ~3.5 s; `load-install-scm` is ~2.3 s (65%), all re-expanding
`expander/lib/install.scm`, because that file defines the cache backend
(`install-library-file!`, `cache-file-for`, the gfo helpers) every other cache
path depends on — it cannot use the cache it defines. Design (option B from
Appendix C): a bootstrap-only capture/load pair. Cold boot captures the file's
top-level definitions as a module bundle; warm boot replays the bundle instead
of re-expanding. Normal cache, no pre-expansion, no layering change.

- **Step 1 [DONE, `6b18c922`]** — module-bundle replay also defines toplevel
  bindings in the expander module environment (`load_bundle_gfo_file` module
  branch).
- **Step 2 [NEXT]** — give the replay the native-source-unit semantics that
  `load-source-file`'s `internal_source` branch has: (1) create/restore the
  `(native-source)` exp-library and link it to the base library
  (`exp-library-add-use!`); (2) evaluate the bundle's lowered definitions in
  `(module-eval-environment (the-expander-library))`; (3) publish its toplevel
  bindings to global. Standalone change over the existing bootstrap artifacts
  with zero behavior change, gate-backed — step 1 alone left
  `install-library-forms!` unbound during the next expand (root cause and
  history in Appendix C).
- **Step 3** — re-apply the split + capture + warm replay (spec in
  Appendix C): definitions-only `install.scm` (899 lines) + new
  `expander/lib/install-boot.scm` (macro-layer boot block, boot-only helpers,
  and the 18 top-level `module-define!` publish calls); the
  `(load-source-file "core/gfo.scm")` seed load moves into `native_main`;
  cold capture via `install-library-file!`, warm replay via
  `bootstrap.load_artifact` with a `load_source` fallback, then
  `install-boot.scm`.
- **Step 4** — acceptance: `load-install-scm` ~2.3 s → ≈0.5 s and warm boot
  ≈1.5-1.7 s; changed-since gate green; cold/warm/bootstrap/readonly states
  via run-suite + compare; cache-miss and corruption recovery; no warm-RSS
  regression.

Exit: steps 2-4 green. Phase 3 then starts.

## Phase 3 — Evaluator and runtime throughput [NEXT]

The ultimate metric — how fast programs run after startup — plus the folded
2b warm-boot work. The profile is known (Appendix B); bounded variants were
measured neutral and must not be retried; the profitable changes are
structural.

1. **Runtime benchmark set + gate first.** Establish baseline programs, wire
   `compare.sh` for them the same way as startup. Nothing lands before this.
2. **Structural evaluator levers**, one gate-backed commit each (each is
   call/cc + dynamic-wind critical, so the dedicated gate cycle includes
   those tests):
   - KontFrame slimming: merge the three winder vectors, shrink the frame
     (today 2 Values, 2 shared_ptrs, a Values vector, 4 winder vectors,
     moved on every push; ~17% of samples).
   - Avoid per-call `vector<Value>` construction/move (~9%).
   - Non-atomic `EnvironmentPtr` (ownership/threading audit first).
   - GC allocation-rate reduction (~18%; pairs with Phase 4).
3. **Cached-replay credit (2b).** Re-measure deserialize/eval/re-import after
   the levers; target warm startup < 1 s.

Exit: runtime gate with baseline and measured gains; warm startup < 1 s.

## Phase 4 — Memory [LATER]

- Peak RSS (cold ~91-103 MiB) and steady state; GC tuning.
- Allocation reduction in the compile pipeline and evaluator.
- Track per-stage RSS alongside time.

Exit: cold RSS below target with no throughput regression.

## Phase 5 — Cache unification and IO [LATER]

- One front door: the bootstrap plain-file path and the `define-library` path
  have separate orchestration and bundle kinds over one shared backend.
  Unify orchestration and bundle kind so the whole runtime is cached
  libraries through one path (also removes the `case-lambda`
  libraries-vs-module path ambiguity).
- `gfo` format, atomic write, concurrency, cross-machine portability.

Exit: one cache path, one bundle kind; recovery/concurrency gates green.

## Phase 6 — Runtime image [RESEARCH, LATER]

A `.fasl`-style image (serialized booted state) is the path to < 200 ms and
needs a design before implementation: serialization boundary, transformer
closures, gensym stability, invalidation, relationship to the `gfo` cache,
trust/portability. Deliver a design doc and a feasibility prototype only.

## Appendix A — Phase 1 records (2026-10-06)

Landed changes (interleaved min-of-5 micro-benchmarks):

| change | commit | micro-benchmark |
|---|---|---|
| small-integer `+ - *` in int64 | `52d96dda` | eval/sum −53%, fib −46% |
| small-integer `< <= > >=` in int64 | `64841f54` | eval/sum −28%, fib −33% |
| serialize: pair/vector tested first | `f32ceaed` | serialize −12.7% |
| deserialize: dispatch on pair head | `7a66dec2` | deserialize −12.1% |

Suite medians (`bench/cold-start/phase1-6dd0760c`, 3 samples, compare.sh OK):

- cold compile: minimal −12.0%, small-real −11.4%, large-scheme −10.9%
- warm startup: minimal 4.28→3.24 s (−24%), small-real −32%, large −20%
- bootstrap: minimal 7.46→4.18 s (−44%), small-real −41%, large −17%
- readonly: −43% (minimal/small-real), −17% (large)
- reader benchmark (high-iteration): −11%
- gates: changed-since 162/162 after each change; compare.sh OK
- caveat: cold peak RSS rose ~5-9% on two workloads; warm RSS unchanged

## Appendix B — Evaluator profile and neutral list (2026-10-06)

Symbol-resolved profile of arithmetic and warm-boot runs is dominated by:

- `Evaluator::run_machine` ~11.6%
- `KontFrame` construct/move/destroy ~17% (heavy frame: 2 `Value`, 2
  `shared_ptr`, a `Values` vector, 4 winder vectors; moved on every push)
- GC (mark/free/malloc) ~18%
- `std::vector<Value>` move/realloc ~9% (per-call argument vectors)
- `Environment::lookup` ~5%
- `RealNumber::exact` ~5% in pre-fast-path runs (fixed by the int
  comparisons landed in Phase 1)

Measured neutral, reverted (do not retry as-is): `state.frames.reserve(1024)`;
`return_values` assign-instead-of-move.

## Appendix C — 2c design record and split spec (2026-10-06)

Options considered for caching the installer:

- **A. Fold `install.scm` into the kernel pre-expansion.** Rejected: it is
  lib-layer; `build-kernel` expands kernel sources only; folding mixes layers.
- **B. Cache it as a module bundle loaded by a bootstrap-only C++ path.**
  Chosen. Uses the normal cache, adds only a bootstrap capture/load pair.
- **C. Pre-expand the seed into a loadable file.** Rejected: a special
  pre-expansion, same layering objection as A.

Risk/gate for B: the replay must reproduce binding kinds (toplevel vs macro)
and library home exactly, or every later cache path breaks; the cache version
invalidates by content hash when `install.scm` or the pipeline changes.

Prototype (2026-10-06, reverted; split files preserved under `/tmp`):
cold boot fine, capture produced a 48 KB bundle, warm `load-install-scm`
2.3 s → 540 ms, but the driver then failed with
`unbound symbol: install-library-forms!`. Root cause, refined: the
module-bundle replay targets the BASE library, while `load-source-file` treats
the file as an `internal_source` unit — fresh `(native-source)` exp-library
linked to base, defs evaluated in the expander's module environment, toplevel
aliases published to global. Even with the step-1 alias publication and the
lowered defs evaluated in the expander environment, the expander could not
resolve `install-library-forms!` while expanding the next source file; the
macro-layer artifact replay is not sufficient for a native-source unit. Hence
Phase 2c step 2 (native-source-unit replay semantics) precedes the re-apply.

Split spec (line numbers into the original 1007-line `install.scm`; re-derive
with sed if the `/tmp` files are gone):

- line 22 `(load-source-file "core/gfo.scm")` → moves into `native_main`
- boot block lines 669-732 → `install-boot.scm`
- top-level `module-define!` publish calls at 734-736, 869-891, 1007 →
  `install-boot.scm`
- remainder = definitions-only `install.scm` (899 lines)
- `/tmp` artifacts (volatile): `install.orig.scm` (1007 lines),
  `install-boot-body.scm` (boot block + publish calls), `install-md.scm`

## Appendix D — Key code locations

- Boot sequence: `src/runtime/native_main.cpp` ~604-660
- Source loader: `src/runtime/standard_primitives.cpp` — `load-source-file`
  installed at :1461; seed (`core/gfo.scm`) branch :1517-1629;
  `internal_source` branch :1660-1836 (`source_library` =
  `make-exp-library '(native-source)`; `eval_environment` =
  `module-eval-environment (the-expander-library)`; defs evaluated at
  :1778-1806; toplevel aliases published to global at :1807-1835)
- Artifact loader: `src/runtime/artifact.cpp` — `load_bundle_gfo_file`,
  module branch :471-~560 (step 1: also defines toplevel bindings in the
  expander module environment); `load_library_gfo_file` :303-445
- Bootstrap: `src/runtime/bootstrap.cpp` — `native_bootstrap_artifacts` (boot
  set, 12), `native_precompile_artifacts` (optimizer, 7), `load_cached_runtime`
  / `load_artifact` / `load_cached_source`
- Bootstrap installer: `goldfish/expander/lib/install.scm` (1007 lines)
