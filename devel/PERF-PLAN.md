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
with Phase 1, warm RSS unchanged). After Phase 2 (2026-10-07): warm boot
total ~1.8 s, warm wall 1.8-1.9 s.

## Targets

| metric | now | near (exit Ph 3) | mid (exit Ph 5) | stretch (Ph 6) |
|---|---:|---:|---:|---:|
| warm startup | 1.8 s | < 1 s | < 300 ms | < 60 ms |
| cold, shipped cache | n/a | ≈ warm | ≈ warm | ≈ warm |
| cold, no cache | ~64 s | < 10 s | < 3 s | — |
| warm RSS | 32 MiB | ≤ 32 MiB | ≤ 32 MiB | — |
| runtime throughput | unmeasured | baseline + gate | −10% time | — |

## Program map

| phase | goal | status |
|---|---|---|
| 0. Measurement & gates | instruments before optimization | **DONE** 2026-10-06 |
| 1. Compile pipeline | reader / expander / optimizer / serializer | **DONE** 2026-10-06 |
| 2. Startup structure | installer cacheable; shipped-cache cold ≈ warm | **DONE** 2026-10-07 (2a + 2c; 2b folded into 3) |
| 3. Evaluator & runtime | runtime throughput; warm < 1 s | **NEXT** (profile + levers ready; Appendix B) |
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

## Phase 2 — Startup structure [DONE — 2026-10-07]

Goal: make the bootstrap installer cacheable so warm boot drops to the
cached-replay floor (~1.5-1.7 s), and a distribution-precompiled cache makes
a fresh install's cold ≈ warm. The remaining distance to warm < 1 s is evaluator
work (2b below), so that number closes in Phase 3 — by design, not by gap.

Exit met 2026-10-07: installer replayed from cache (load-install-scm ~2.3 s →
~20 ms), warm boot total 1.79 s, changed-since 162/162, recovery paths green,
no warm-RSS regression. Records: Appendix C/E.

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

### 2c — Cacheable bootstrap installer [DONE]

Warm boot was ~3.5 s; `load-install-scm` was ~2.3 s (65%), all re-expanding
`expander/lib/install.scm`, because that file defines the cache backend
(`install-library-file!`, `cache-file-for`, the gfo helpers) every other cache
path depends on — it cannot use the cache it defines. Design (option B from
Appendix C): a bootstrap-only capture/load pair. Cold boot captures the file's
top-level definitions as a module bundle; warm boot replays the bundle instead
of re-expanding. Normal cache, no pre-expansion, no layering change.

- **Step 1 [DONE, `6b18c922`]** — module-bundle replay also defines toplevel
  bindings in the expander module environment (`load_bundle_gfo_file` module
  branch).
- **Step 2 [DONE, `fabaa0dc`]** — `load_source_unit_gfo_file`: replay with
  native-source-unit semantics — a dedicated `(native-source "<key>")`
  library linked to the base library, lowered definitions evaluated in the
  expander module environment, binding table restored into that unit,
  toplevel values aliased to the root evaluator. NOT into the base library:
  that bakes qualified toplevel names into other files' cached artifacts,
  which replay in the root environment where those names are absent (the
  divergence found mid-implementation, see Appendix C).
- **Step 3 [DONE, `6e420d2e`]** — split `install.scm` into a definitions-only
  file (796 lines) and `expander/lib/install-boot.scm` (boot block, publishes,
  internal-surface registrations — these must run after module.scm installs,
  so they belong to the driver half); the `core/gfo.scm` seed load moves to
  `native_main` ahead of the installer; cold boots load the source and
  capture through the cached-source path, warm boots replay, missing/stale/
  corrupt bundles fall back to the source load and re-capture.
- **Step 4 [DONE]** — acceptance, Appendix E.

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
module-bundle replay targeted the BASE library, while `load-source-file` treats
the file as an `internal_source` unit — fresh `(native-source)` exp-library
linked to base, defs evaluated in the expander's module environment, toplevel
aliases published to global. Even with the step-1 alias publication and the
lowered defs evaluated in the expander environment, the expander could not
resolve `install-library-forms!` while expanding the next source file; the
macro-layer artifact replay is not sufficient for a native-source unit.

Resolution (2026-10-07, landed): the replay is a dedicated C++ entry with the
full cached-source-unit semantics (`load_source_unit_gfo_file`), and the
capture goes through the cached-source path into a dedicated
`(native-source "expander/lib/install.scm")` unit. A first implementation
restored the bindings into the BASE library and failed warmly with
`unbound symbol: native-debug-enabled?@(goldfish):0`: toplevel names embed
their allocating library (`store-alloc-name` builds `name@(lib):N`), so with
the installer's bindings in base, later boot files' cached artifacts resolved
their references to qualified names that only exist in the expander
environment — unreachable from the root environment those artifacts replay
in. Keeping the installer out of base (bare references + root aliases,
exactly the unsplit cold path) removed the divergence. Two more resolution
facts: unresolved free identifiers in library context stay bare for runtime
resolution (expand.scm, "no ambient base"), which is how the installer
references the gfo seed's names; and the internal-surface registration scans
must run after module.scm installs, hence they live in the driver half.

Split (landed): `install.scm` = lines 1-661 + 737-867 of the original
(definitions only, 796 lines, 40 top-level defines); `install-boot.scm` =
lines 662-736 + 869-1007 (boot block, publishes, registrations), 228 lines.
Re-derive with sed on those ranges if ever needed.

## Appendix D — Key code locations

- Boot sequence: `src/runtime/native_main.cpp` (~604-680; gfo-seed load,
  installer replay/capture, install-boot load)
- Source loader: `src/runtime/standard_primitives.cpp` — `load-source-file`
  installed at :1461; seed (`core/gfo.scm`) branch :1517-1629;
  `internal_source` branch :1660-1836 (`source_library` =
  `make-exp-library '(native-source)`; `eval_environment` =
  `module-eval-environment (the-expander-library)`; defs evaluated at
  :1778-1806; toplevel aliases published to global at :1807-1835)
- Artifact loader: `src/runtime/artifact.cpp` — `load_bundle_gfo_file`,
  module branch (step 1: also defines toplevel bindings in the expander
  module environment); `load_source_unit_gfo_file` (2c step 2 replay);
  `load_library_gfo_file`
- Bootstrap: `src/runtime/bootstrap.cpp` — `native_bootstrap_artifacts` (boot
  set, 12), `native_precompile_artifacts` (optimizer, 7), `load_cached_runtime`
  / `load_artifact` / `load_cached_source` / `load_cached_installer` /
  `capture_installer`
- Bootstrap installer: `goldfish/expander/lib/install.scm` (definitions
  only, 796 lines) + `goldfish/expander/lib/install-boot.scm` (driver half)
- Toplevel naming: `expander/kernel/store.scm` `store-alloc-name` builds
  `name@(lib):N`; unresolved library-context identifiers stay bare
  (`expander/kernel/expand.scm`, resolve-identifier / expand-atom)

## Appendix E — Phase 2c records (2026-10-07)

Suite medians, 3 samples, `compare.sh` over
`bench/cold-start/phase2c-base-c385cb89` (HEAD `6b18c922`, bin `c385cb897a43`)
vs `bench/cold-start/phase2c-cand-b915422a` (bin `b915422a49bb`):

- warm: minimal 3.16→1.92 s (−39.4%), small-real 3.21→1.82 s (−43.2%),
  large 3.78→3.57 s (−5.6%)
- bootstrap: minimal −27.3%, small-real −25.5%, large −8.4%
- readonly: minimal −27.0%, small-real −27.4%, large −7.4%
- cold: large +4.0%, small-real +5.9% (the installer's second expansion
  during capture, ~2-3 s once per cache generation)
- warm RSS: −1.4% (minimal) to −20% (small-real); bootstrap/readonly RSS
  mixed (+0.7..+22%)
- `load-install-scm`: 2.3 s → ~20 ms warm; boot total 1.79 s
  (gfo-seed 0.5 s is the visible new stage)
- caveat: the full suite's minimal-cold cell read +10.6% (over the 10%
  gate); a targeted cold-only rerun (`phase2c-coldonly-*`) measured +3.1%
  (62.26→64.15 s) — the first run's optimizer-compile stage carried
  single-sample noise (+2.7 s on a ~28 s stage). Cold cost is the capture's
  second expansion, verified by `GOLDFISH_DEBUG=timing` stage attribution.
- gates: changed-since 162/162; corruption/stale/missing bundle recovery
  (fallback + re-capture) verified by hand; `tools/build-kernel.sh` flow
  green; `tools/warm-bootstrap-cache.sh` ships the installer bundle (the
  isolated run captures it and the script copies the whole directory).
