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
| warm startup | ~0.9-1.06 s | < 1 s ✓ | < 300 ms | < 60 ms |
| cold, shipped cache | n/a | ≈ warm | ≈ warm | ≈ warm |
| cold, no cache | ~41-50 s | < 10 s | < 3 s | — |
| warm RSS | ~30 MiB | ≤ 32 MiB | ≤ 32 MiB | — |
| runtime throughput | baseline + gate ✓ | −10% time (Phase 4) | −10% time | — |

## Program map

| phase | goal | status |
|---|---|---|
| 0. Measurement & gates | instruments before optimization | **DONE** 2026-10-06 |
| 1. Compile pipeline | reader / expander / optimizer / serializer | **DONE** 2026-10-06 |
| 2. Startup structure | installer cacheable; shipped-cache cold ≈ warm | **DONE** 2026-10-07 (2a + 2c; 2b folded into 3) |
| 3. Evaluator & runtime | runtime throughput; warm < 1 s | **DONE** 2026-10-07 — gate + 5 levers; the < 1 s exit closed by Phase 5's interface builder (Appendix F) |
| 4. Memory | peak/steady RSS | later |
| 5. Cache unification & IO | one cache front door, one bundle kind | **ACTIVE** — attribution + native interface builder landed (Appendix G) |
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

## Phase 3 — Evaluator and runtime throughput [ACTIVE]

The ultimate metric — how fast programs run after startup — plus the folded
2b warm-boot work.

1. **Runtime benchmark set + gate [DONE, `7fa11efd`].** `bench/runtime/`:
   five fixed workloads (fib, sum, nqueens, nested dynamic-wind+call/cc
   escapes, list pipeline) over a warm cache with output verification;
   summary.tsv matches the startup suite's format so `compare.sh` gates it
   unchanged. Baseline: `bench/runtime/phase3-base-378e8608`.
2. **Structural levers, one gate-backed commit each [4 landed]:**
   - KontFrame slimming [DONE, `d2c5d626`]: the three winder vectors moved
     into a lazily allocated KontWind payload (common frames carry one null
     word). Runtime suite: winders -9.3%, sum -3.6%.
   - Per-call argument vectors [DONE, `1cd10241`]: core forms read arguments
     into eight inline slots (heap spill only past eight, preserving
     semantics for oversized forms); closure formals walked twice instead of
     collected. fib -9.4%, nqueens -8.0%, sum -7.9%.
   - HOF shadowing pathology [DONE, `481000ec`, `e0f5444a`]:
     base-functions.scm's Scheme member/assoc/map/for-each shadowed the
     machine loops, paying a closure invocation per element (assoc measured
     ~3 us/step). Dropped the shadows; equal? marked Kind::Equal so
     member/assoc inline their default comparator; equal() gained a
     non-aggregate fast path. assoc probe 50.1 s → 7.3 s; lists -18.4%,
     fib -9.7%, sum -10.3%; peak RSS ~-20%.
   - Non-atomic EnvironmentPtr [DONE, ref_ptr.hpp]: `EnvironmentPtr` is now
     an intrusive `RefPtr<Environment>` with a plain (non-atomic) refcount —
     the runtime is single-threaded, so every frame push/pop and environment
     copy was paying an atomic RMW for no synchronization.  Audited first:
     no site rebuilds ownership from a raw pointer, no use_count/weak_ptr.
     Runtime suite: all five workloads −11.0..−12.5% (double the 4-5%
     estimate); warm boot 1.13 → ~1.08 s.
3. **2b credit [PARTIAL].** The gfo cache seed now replays from its captured
   bundle like the installer [DONE, `ce7c0c9e`]: gfo-seed stage 466 → ~13 ms.
   Warm startup: **1.11 s** (minimal) / 1.20 s (small-real) / 1.62 s
   (large) — the < 1 s target needs the remaining replay stages
   (standard-library 330 ms, load-cached-runtime 220 ms, mode-imports
   ~250 ms) to get faster via a small-size Values type or Phase 5's unified
   replay; attribution and the full ledger are in Appendix F.

Exit: runtime gate with baseline and measured gains [met]; warm startup
< 1 s [met 2026-10-07 by Phase 5's native interface builder — warm
~0.9-1.06 s; see Phase 5 and Appendix F].

## Phase 4 — Memory [LATER]

- Peak RSS (cold ~91-103 MiB) and steady state; GC tuning.
- Allocation reduction in the compile pipeline and evaluator.
- Track per-stage RSS alongside time.

Exit: cold RSS below target with no throughput regression.

## Phase 5 — Cache unification and IO [ACTIVE — attribution done 2026-10-07]

Warm-boot attribution (GOLDFISH_DEBUG=timing, boot total 1.24 s, warm cache):

- **Import binding-copying ≈ 944 ms (76%)**: `import-into-library!`
  physically copies every imported library's binding table into the
  importing library.  C++ side 365 ms (scheme/base 261, case-lambda 104 —
  a one-macro library whose only import is (goldfish), the whole
  implementation library); Scheme side 579 ms across the mode-import
  library chain.  Every `(import (goldfish))` re-copies thousands of
  bindings.
- install-boot.scm ~113 ms: still source-expanded (publishes +
  internal-surface registration scans).
- Everything else is small: text parse ~30 ms total (TinyReader +
  lib-cache-read), defs eval ~65 ms, transformer rebuild ~24 ms, kernel
  ~39 ms, seed+installer replay ~30 ms.

Conclusions for the unified design:

1. **No format change needed** — text gfo + TinyReader parse is ~3% of
   boot; binary/mmap would buy nothing.  The unification is about
   orchestration and bundle kind, not serialization.
2. **The structural lever is the import-view construction, not copying.**
   The views already exist (`add-import-view!` / `import-view`); the cost
   is building the mapped interface table — a Scheme per-entry loop over
   the source's full export list (thousands of `exp-library-ref` +
   `exp-library-define!` calls, ~35 us/entry interpreted) for every
   distinct import set, paying again whenever the source's export list
   changed between importers (cache key includes the pairs).

   **Landed — the `%interface-table` primitive**: a C++ bulk builder that
   materializes the source's bindings once (`exp-library-bindings` over
   own + uses, newest-first first-wins — exactly `exp-library-ref`
   semantics) and issues one kernel `exp-library-define!` per visible
   name, keeping every record-layout detail inside the kernel.  Raises
   are ThrownValue payloads shaped byte-for-byte like the interpreted
   `(error 'import "~a..." arg ...)` — load-library-guard's detail
   formatting is unchanged (the import-sets test exercises both collision
   shapes).  scheme/base's artifact 290 → 115 ms; **warm boot 1.11 s →
   ~0.9-1.06 s — the < 1 s target is met**; cold unchanged (fair
   same-load A/B −1.0%); runtime suite unchanged (compare OK).  Logged in
   %internal-names.
3. install-boot.scm's publishes/registrations fold into the unified
   replay naturally once one front door exists.

Measured, reverted (do not retry as-is): registering the base library
directly as the view for bare `(goldfish)` imports — warm −11% (minimal
0.99 s) but cold +6..17%: with base's full own table visible, cold
capture from source resolves previously-bare identifiers to base
bindings, changing the expanded output and every downstream artifact.
Also a thread_local reuse of equal()'s cycle-guard vector: segfaults
cold boot under the conservative collector.

Steps: (a) unify orchestration and bundle kind over one backend, with
the native interface builder as the replay semantics — boot-file order
and native-source unit semantics must stay byte-equivalent, gated like
2c; (b) gfo format hardening: atomic write, concurrency, cross-machine
portability, each its own gate-backed commit.  (b) is done — the
contract lives in `tools/test-cache-concurrency.sh`; (a)'s design —
surface-growth semantics, the boot manifest, kind taxonomy and the D1-D6
decisions — is recorded in `devel/PHASE5-UNIFICATION.md`; its step 1
(in-place interface extension, D2) is next.

Exit: one manifest-driven boot chain + one loader path (the D3/D4
contract), recovery/concurrency gates green.

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

## Appendix F — Phase 3 records (2026-10-07)

Runtime suite (n=3, min-held medians via compare.sh), each lever vs its
predecessor's record:

| lever | commit | fib | sum | nqueens | winders | lists |
|---|---|---:|---:|---:|---:|---:|
| KontWind payload | `d2c5d626` | −1.5% | −3.6% | −1.6% | −9.3% | −1.1% |
| FormArguments | `1cd10241` | −9.4% | −7.9% | −8.0% | −1.3% | −6.5% |
| member/assoc unshadow + inline comparator | `481000ec` | −6.0% | +1.2% | −2.4% | +0.2% | −0.6% |
| map/for-each unshadow | `e0f5444a` | −9.7% | −10.3% | −7.9% | −7.6% | −18.4% |
| non-atomic EnvironmentPtr (RefPtr) | ref_ptr.hpp | −11.3% | −12.4% | −11.2% | −12.5% | −10.9% |

Cumulative vs `phase3-base-378e8608`: fib −24%, sum −19%, nqueens −18%,
winders −17%, lists −25%. assoc micro (20k lookups × 2000-entry alist):
50.1 s → 7.3 s. Changed-since 162/162 after every lever; every suite run
verifies program output.

Startup suite, cumulative Phase 3 vs the Phase 2 exit record
(`bench/cold-start/phase3-final-3a138fac` vs `phase2c-cand-b915422a`,
3 samples, compare.sh OK):

- warm: minimal 1.92→1.11 s, small-real 1.82→1.20 s, large 3.57→1.62 s
- bootstrap: −32.9/−31.7/−64.6% (large 22.4→7.9 s)
- readonly: −31.9/−31.1/−65.3%
- cold: −37.9/−33.5/−41.7% (large 85.0→49.6 s) — the evaluator levers
  accelerate cold compilation itself
- peak RSS: down on 11 of 12 cells (−4.6..−34.2%); small-real warm +0.0%

Lever C's own A/B (`bench/cold-start/phase3-leverc-d8ca389e` vs
`phase3-final-3a138fac`): warm −1.8/+1.8/−2.3% (boot is artifact-replay
dominated, not environment-copy dominated); cold minimal −4.9%; the
runtime suite is where the refcount removal shows (−11% across the board).
small-real cold +9.3% in that A/B is shared-box noise (minimal −4.9%,
large +0.5% in the same run).

Warm-boot stage attribution at the plateau (minimal, `GOLDFISH_DEBUG=timing`):
standard-library ~330 ms, load-cached-runtime ~220 ms, mode-imports ~250 ms,
install-boot ~130 ms, native-scheme-surface ~65 ms, kernel ~35 ms, seed+installer
replay ~25 ms — all artifact-deserialize + evaluator work.

Do-not-retry list (measured harmful, reverted): the thread_local reuse of
equal()'s cycle-guard vector segfaults cold boot under the conservative
collector; per Appendix B, `frames.reserve(1024)` and return-value
assign-instead-of-move.

Environment notes: `xmake` builds must run inside `nix develop -c` when the
toolchain detection cache under `build/` is cold (no gcc on the bare PATH);
`tools/build-prof.sh` currently fails — xmake 3.0.9's releasedbg config
errors with "target(lint-layer): toolchain not found", so perf profiling is
unavailable in this environment (code-level attribution was used instead).

## Appendix G — Phase 5 records (2026-10-07)

Attribution instrumentation (`0e53c2f4`): per-artifact and per-phase marks
under the timing key across both bundle kinds, plus Scheme-side
lib-cache-read / lib-import / lib-bindings / lib-macros / lib-restore /
lib-eval-defs marks.  Findings: import-view construction ~76% of a 1.24 s
warm boot (scheme/base 261 ms, case-lambda 104 ms for a one-macro library);
text parsing ~3%; defs eval ~5%.

Landed: the `%interface-table` primitive (bulk import-interface builder,
kernel-API-only — `exp-library-bindings` materialization + one
`exp-library-define!` per visible name; ThrownValue raises matching the
interpreted `(error 'import ...)` payload).  Two implementation lessons are
embedded in its comments: symbols are not pointer-interned across cache
readers (compare names, not Object pointers), and `exp-library-uses`
entries are `(view . level)` pairs.

Measured (records under `bench/cold-start/phase5-itab-9c9f5841` and
`bench/runtime/phase5-itab-9c9f5841`, compared against the
`phase3-leverc-d8ca389e` startup record):

- warm boot total: 1.11 → ~0.9-1.06 s (minimal 1.06 s, small-real 1.06 s
  under a loaded box; quiet-box direct timing 0.88-0.93 s).  **Warm < 1 s:
  met.**
- scheme/base artifact 290 → 115 ms; case-lambda 104 → ~0 ms; C++-side
  bundle-import total 365 → 132 ms.
- runtime suite: compare OK (compute paths untouched; load-inflated
  sum/winders cells within tolerance).
- cold: fair same-load A/B (`phase5-itab-coldbase` vs `coldcand`, minimal
  only): 50.73 → 50.24 s (−1.0%, compare OK).  The full-suite cold cells
  read +15..27% against the quiet-window records — a load-4.6 box
  inflates long runs ~24%; the same-load A/B is the honest comparison.
- gates: changed-since 162/162 (import-sets included — it failed with the
  first ErrorObject-shaped raise and passed once the raise was a
  ThrownValue matching `(error 'import ...)` byte-for-byte).

Environment note: a load-average-4+ box inflates cold-run wall ~24% and
runtime medians up to ~17%; when in doubt, measure BOTH sides in the same
window (`phase5-itab-coldbase/coldcand` demonstrate the pattern).

Follow-up (same day, reverted): memoizing import-set-pairs (identity
mappings) and shortcutting add-import-view!'s conflict scan for
use-less libraries measured only ~35 ms of warm boot and broke the cold
libraries-bundle replay (case-lambda raised user-raised value through
load_cached_runtime; root cause not pinned before revert).  Two memo
variants were tried and both missed: pair-shaped keys are never eq? to
their lookups, and registry records are re-created per query (no object
identity survives between imports) — a working memo must key on the
library name with value equality.  The remaining ~540 ms of Scheme-side
lib-import needs the per-spec breakdown (ispec instrumentation sketch
existed during the attempt; not kept) before the next cut.

install-boot cacheable (same day): the cold-start macro-layer install
moved to `expander/lib/install-macrolayer.scm` (loaded by the native
driver only when the bootstrap cache is unavailable), and
install-boot.scm became definitions-only — the expander-module
publishes wrapped in one define-with-side-effects plus the (unchanged)
registration scans — replayed from its captured bundle on warm boots
and expanded + captured on cold ones through the ordinary
load_cached_source path.  install-boot stage 116-130 ms → 35-41 ms;
startup suite all green (warm minimal 0.89 s, small-real 0.98 s; cold
-5.7..-16.6%; bootstrap/readonly improved; runtime suite all cells
improved); changed-since 162/162.

Records: `bench/cold-start/phase5-bootcache-e9febd7f`,
`bench/runtime/phase5-bootcache-e9febd7f`.

Root cause of the pairs-memo failures (corrects the note above): the
identity-mapping rebuild per importer is SEMANTICALLY REQUIRED, not
waste — the implementation library's export surface grows across boot
stages (reader/writer/native-scheme-surface registrations land in the
base library after the first importers), so every importer must snapshot
the exports at its own import time.  Memoizing the pairs freezes the
first importer's snapshot: later programs fail with unbound identifiers
for names registered after the freeze (observed: write-roundtrip in the
cache-roundtrip test).  Both memo variants failed for this reason; a
working memo would require freezing the implementation library's export
surface by design — a Phase 5 unification decision, not a local cache.
The scan shortcut/iface-bindings memo broke the cold libraries-bundle
replay independently (case-lambda raised user-raised value through
load_cached_runtime) and was reverted without root-causing; do not
reintroduce without a cold-cache gate in the loop.

Robustness contract (same day): audit found the gfo cache's guarantees
already implemented — pid-qualified tmp files + atomic `g_rename`
(concurrent writers cannot interleave; readers see old-or-new complete
records), `create_directories` tolerates the concurrent-mkdir race, and
validity stamps are content-based so copied caches on other machines
just recompile.  One gap fixed: a failed rename leaked the tmp file and
silently dropped the entry — gfo-write! now deletes it.  The guarantees
are enforced by `tools/test-cache-concurrency.sh`: four parallel cold
boots sharing one directory, a kill-mid-boot recovery, truncated-
artifact recovery, and a readonly boot from a copied cache.
