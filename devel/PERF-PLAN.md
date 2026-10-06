# Goldfish performance program

Performance is treated as one engineering discipline, not a set of one-off
patches: every workstream shares the same instruments, the same regression
gate, and the same design principles, and each phase has an explicit exit
criterion before the next begins.

## 0. Principles

These constrain every optimization (they are not negotiable per workstream):

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

## Reference point

The pre-native implementation started in ~200 ms cold / ~60 ms warm. Current
native numbers: cold ~70 s (empty cache), warm ~4.1 s, state-3 ~9.6 s, warm
RSS ~32 MiB, cold RSS ~91-103 MiB.

## Targets (staged)

| Metric | now | near | mid | stretch |
|---|---:|---:|---:|---:|
| warm startup | 4.1 s | < 1 s | < 300 ms | < 60 ms |
| cold, shipped cache | n/a | ≈ warm | ≈ warm | ≈ warm |
| cold, no cache | ~70 s | < 10 s | < 3 s | — |
| warm RSS | 32 MiB | ≤ 32 MiB | ≤ 32 MiB | — |
| runtime throughput | unmeasured | baseline + gate | −10% time | — |

## Phase 0 — Measurement and regression infrastructure

Foundation for everything else; nothing is optimized before it can be
measured and guarded.

- **Startup runner** (exists): `bench/cold-start/run-suite.sh`, fixed
  workloads, four cache states, `rusage` helper.
- **Per-library ledger** (exists): `bench/cold-start/lib-timing.sh`; the
  marks must be re-added so they do not perturb boot (install.scm-only first,
  verified by cold→warm before merge).
- **Symbol-resolved profiling**: make `tools/prof.sh` resolve symbols (build
  with debug info or consume `bin/gf.sym`) so hotspots are function-level, not
  addresses.
- **Micro-benchmarks** (new, one per pipeline stage): reader, expander/lower,
  optimizer passes, serializer/deserializer, evaluator call/loop/alloc, GC,
  cache replay.
- **Regression gate** (new): `bench/cold-start/compare.sh` — compare a run's
  `summary.tsv` against a recorded baseline by binary/source hash with a
  tolerance, failing on regressions. Wire the same for runtime benchmarks.

Exit: a single command produces startup + micro-benchmark numbers and flags
regressions against a stored baseline.

## Phase 1 — Shared compile pipeline throughput

The same pipeline serves cold startup, first compile of any standard or user
library, dependency fingerprinting, and program compilation. Improving it
helps all of them at once.

1. **Reader** — measured ~0.1 ms/line (`read-forms` of `srfi-175.scm` = 47 ms).
   Profile `read-forms`/TinyReader; target 5-10x.
2. **Expander / lower** — measured ~10 ms/line cold (`scheme/char` expand
   1.56 s / 134 lines). Profile `expand-library-body`, macro dispatch,
   `wrap-expression`; look for repeated traversal and allocation.
3. **Optimizer** — `optimize-run-passes` scales with program size (9.9 s for a
   604-line program); profile individual passes.
4. **Serializer / deserializer** — `save` 0.4-0.5 s, `lookup` deserialize;
   profile `serialize-cache-sexp`/`deserialize-cache-sexp` and `gfo` IO.

Exit: each stage has a micro-benchmark and a measured improvement, with the
cold compile and first-compile numbers improved accordingly, gates green.

**Finding (2026-10-06, symbol-resolved profile).** The reader is not a
bottleneck: `TinyReader` frames are ~0.01% even in a reader-heavy run.  A
`releasedbg` profile (symbols via `bin/gf.sym` debuglink) of an evaluator-heavy
run is dominated by `Evaluator::run_machine`, `KontFrame` stack churn
(emplace/dtor/ctor ~14%), GC (`GC_mark_from`/`GC_free`/`GC_malloc` ~19%),
`vector<Value>` move/realloc (~9%), `Environment::lookup` (~5%) and
`RealNumber::exact` (~3%).  Since the expander and optimizer are Scheme, the
Phase 1 pipeline cost is evaluator/GC cost — Phase 1 and Phase 3 converge on
the evaluator, and that is where the compile-throughput work must land.
`KontFrame` is heavy (2 Values, 2 shared_ptr, a Values vector and four winder
vectors) and constructed/moved on every push; slimming it is the first
evaluator target.

**Results (2026-10-06).** Landed four measured changes, each A/B-verified and
gated:

| change | commit | micro-benchmark |
|---|---|---|
| small-integer `+ - *` in int64 | `52d96dda` | eval/sum −53%, fib −46% |
| small-integer `< <= > >=` in int64 | `64841f54` | eval/sum −28%, fib −33% |
| serialize: pair/vector tested first | `f32ceaed` | serialize −12.7% |
| deserialize: dispatch on pair head | `7a66dec2` | deserialize −12.1% |

Compile pipeline (warm/cold) and suite medians (`bench/cold-start/phase1-6dd0760c`,
3 samples; `compare.sh` OK):

- cold compile: minimal −12.0%, small-real −11.4%, large-scheme −10.9%
- warm startup: minimal 4.28→3.24 s (−24%), small-real −32%, large −20%
- bootstrap: minimal 7.46→4.18 s (−44%), small-real −41%, large −17%
- readonly: −43% (minimal/small-real), −17% (large)
- reader benchmark (high-iteration): −11%
- gates: changed-since 162/162 after each change; `compare.sh` OK
- caveat: cold peak RSS rose ~5-9% on some workloads (warm RSS unchanged)

The per-stage criterion is met for reader, expander/lower, optimizer and
serializer/deserializer.  The next evaluator levers (KontFrame slimming,
primitive-call `vector<Value>` churn, GC) remain for Phase 3.

## Phase 2 — Startup structure

1. **`load-install-scm` (2.33 s, largest warm cost).** The bootstrap installer
   is expanded from source every boot and cannot use the cache before it
   defines it. Design the bootstrap seed: move the cache backend into the boot
   seed (kernel/C++) so `install.scm` becomes cacheable like any other file.
   This is the only change that crosses the pre-expansion boundary; it must be
   justified as bootstrap, not performance special-casing, and designed first.
2. **Cached replay throughput** — `load-cached-runtime` + `standard-library` +
   `mode-imports` (~1.6 s). Deserialize/eval/macro-rebuild and binding
   re-import. Reduce re-import copying; speed transformer rebuild.
3. **Distribution precompile** — build the full cache (including the
   optimizer) at packaging via the normal mechanism, ship it, validate by the
   content-addressed version. Makes cold ≈ warm for shipped installs.

Exit: warm < 1 s; cold with shipped cache ≈ warm; cold-no-cache materially
lower from Phase 1.

### 2a status (done)

`tools/warm-bootstrap-cache.sh` now also requires the optimizer pipeline
(`native_precompile_artifacts`: `core/ir`, `match*`, `compiler*`, `tree-il`)
alongside the boot set, so a distribution-precompiled cache gives a fresh
install a warm first program compile.  The optimizer artifacts are not in the
boot-required set (validating them every start would re-parse the large
compiler artifacts).  See `CCACHE_NOTES.md` for the ship/install procedure.

### 2c design — making the bootstrap installer cacheable

Problem: warm boot expands `expander/lib/install.scm` from source every start
(~2.3 s, the largest warm cost).  It is loaded through the raw seed loader
because it *defines* the cache backend (`install-library-file!`,
`cache-file-for`, the gfo helpers) that every other cache path depends on, so
it cannot use the cache it defines.  In warm mode the file is pure function
definitions: the macro-layer install block is skipped when
`GOLDFISH_NATIVE_ARTIFACTS` is set, so no side effects are needed to replay it.

Options considered:

- **A. Fold `install.scm` into the kernel pre-expansion.** Rejected: it is
  lib-layer; `build-kernel` expands kernel sources only; folding mixes layers
  and violates the layering the kernel boundary protects.
- **B. Cache `install.scm` as a normal module bundle loaded by a bootstrap-only
  C++ path.** Preferred. On cold boot, capture its top-level definitions as a
  module bundle (a dedicated capture, since `install-library-file!` does not
  exist yet). On warm boot, if that bundle validates, eval its defs into the
  base library exactly as `load_source` does, instead of re-expanding. This
  uses the normal cache and adds no performance pre-expansion; it only needs a
  bootstrap-only capture/load pair.
- **C. Pre-expand the seed into a loadable file.** Rejected for the same
  layering reason as A (bootstrap-justified, but still a special
  pre-expansion).

Risks / gate for B: the replay must reproduce binding kinds (toplevel vs macro)
and library home exactly, or every later cache path breaks; the cache version
must invalidate when `install.scm` or the pipeline changes (it does, by content
hash). Validate with the full changed-since gate and `bench/cold-start`
bootstrap/readonly states (cold→warm and cache-miss recovery).  Effort:
medium-large, bootstrap-critical.

Implementation finding (2026-10-06): B works up to the replay but needs one
loader prerequisite.  Splitting `install.scm` into a definitions-only file
plus a driver (boot block + boot-only helpers) and moving the `core/gfo.scm`
seed load into `native_main` preserved cold boot, and capturing
`install.scm` through `install-library-file!` succeeded (48 KB bundle).  Warm
replay via `ArtifactLoader`'s module branch dropped `load-install-scm` from
~2.3 s to ~520 ms, but it is not yet equivalent: the driver then fails with
`unbound symbol: install-library-forms!`.  The replay registers the binding in
the base library but does not populate the evaluation environment that
`expand-eval`/`load_source` fills, so the expander cannot resolve it while
expanding the next source file.

Prerequisite before B can land: make the module-bundle replay publish toplevel
bindings into the expander/base-library evaluation environment (a change to
`load_bundle_gfo_file` affecting every module-bundle load, hence its own
gate-backed cycle).  Order: (1) loader pubishment fix, standalone; (2) re-apply
the split + capture + warm replay; (3) measure `load-install-scm` ~2.3 s → ms
and warm < 1 s.

## Phase 3 — Runtime execution throughput

The ultimate metric: how fast programs run after startup. Currently
unmeasured.

- Evaluator: call/apply dispatch, closures, environments, tail calls.
- Primitives: hot paths (lists, vectors, strings, numbers, char).
- GC: allocation rate, collection pauses, promotion.
- Build the runtime benchmark set first, then optimize the measured hotspots.

Exit: runtime benchmarks established with a regression gate; measured gains.

## Phase 4 — Memory

- Peak RSS (cold ~91-103 MiB) and steady-state; GC tuning.
- Allocation reduction in the compile pipeline and evaluator.
- Track per-stage RSS alongside time.

Exit: cold RSS below target with no throughput regression.

## Phase 5 — Cache unification and IO

- **One front door**: the bootstrap plain-file path and the `define-library`
  path currently have separate orchestration and bundle kinds over one shared
  backend. Unify the orchestration and bundle kind so the whole runtime is
  cached libraries through one path (also removes the `case-lambda`
  libraries-vs-module path ambiguity).
- `gfo` format, atomic write, concurrency, cross-machine portability.

Exit: one cache path, one bundle kind; recovery/concurrency gates green.

## Phase 6 — Runtime image (research track)

A `.fasl`-style image (serialized booted state) is the path to < 200 ms and
needs a design before implementation: serialization boundary, transformer
closures, gensym stability, invalidation, relationship to the `gfo` cache,
trust/portability. Deliver a design doc and a feasibility prototype only.

## Continuous

- Every phase lands as separate, measured, reverted-if-flat commits.
- Startup + runtime regression gates run before each landing.
- Core-semantics changes additionally run the changed-since gate; `--all` only
  when landing core semantics.
- `bench/` records keep raw logs, metadata and checksums.
