# Cold-start startup

This workstream measures and reduces the wait before a program runs, with the
cold-cache first run as the primary target and warm starts held as a
non-regression gate.  The plan is `/tmp/goldfish-cold-start-perf-plan.md`; this
directory holds the runner, the fixed workloads, the raw per-process logs and
the before/after report.

## Environment

Single machine, 2026-10-06: AMD Ryzen 7 7840HS (16 threads), 27 GiB RAM, Linux
(nix).  Every number below is a sample from this machine, not a performance
guarantee.  Processes are timed with GNU `time`-equivalent semantics via the
bundled `rusage` helper (`wait4` + `RUSAGE_CHILDREN`, peak RSS = `ru_maxrss`).

| Binary | Role | sha256 (first 12) |
|---|---|---|
| revision `ffeb52e5` | Phase 1 baseline (uninstrumented) | `ed1629d1bb28` |
| instrumented | Phase 2/3 ledger, A/B "before" | `4799e2efda7b` |
| optimized | Phase 4/6 "after" | `46a5f64966df` |

## Method

`run-suite.sh` drives the fixed workload set through four cache states with a
private `GOLDFISH_CACHE_DIR` per run, `GOLDFISH_DEBUG=timing`, and a bounded
deadline.  It never touches the user's default cache.

Workloads (`workloads/`): `minimal.scm` (`(display 'ok)`), `small-real.scm`
(standard-library calls plus a little computation), `large-scheme.scm` (604
lines, 300 definitions).

States: `cold` (fresh writable cache, first run), `warm` (same cache, repeat),
`bootstrap` (bootstrap cache present but the user program is not compiled),
`readonly` (bootstrap cache, program uncompiled, writes disabled).

```sh
cc -O2 -o bench/cold-start/rusage bench/cold-start/rusage.c
sh bench/cold-start/run-suite.sh bench/cold-start/optimized-run 3
```

## Phase 1 baseline (minimal.scm)

| State | wall s (3 samples) | median | peak RSS MiB |
|---|---|---:|---:|
| cold | 73.24 / 72.60 / 73.18 | 73.18 | 90.9 / 102.9 / 90.6 |
| warm | 7.26 / 7.28 / 7.42 | 7.28 | 32.5 / 32.1 / 31.8 |

The isolated cold run matches the plan's ~72 s; peak RSS is ~90-103 MiB
(the 80 MiB user report is in this band; the earlier 31 MiB figure was the
warm cache).  A separate state-3 probe (bootstrap warm, new program) was
10.5 s / 40.4 MiB.

## Phase 2/3 ledger and attribution

A fresh cold run of `minimal.scm` under the instrumented binary closes the
plan's ~35 s blind spot.  `[timing]` stages are additive and sum to external
wall:

```
wall 69.83 s = boot 36.02 s + install-fork-runner 0.00 s + user-file 33.76 s
user-file    = cache-lookup 0.001 + read-source 0.000 + expand 0.004
               + compile-optimize 33.75 + cache-write 0.002
compile-optimize = load-compiler 30.23 + load-tree-il 3.52 + run-passes 0.002
```

So the post-boot cost is not the user program at all: it is the **first lazy
load of the optimizer**, i.e. compiling `(goldfish compiler)` and its
dependencies (`core/ir`, `match`, `compiler/passes`, `compiler/patterns`,
`expander/tree-il`) from source.  Running the passes on `(display 'ok)` is
2 ms.  Cache mtimes independently confirm each optimizer artifact is written
exactly once during that phase.

Cold boot itself breaks down as: `mode-imports` 11.8 s, `load-install-scm`
10.6 s, `load-source-reader` 9.5 s, `native-scheme-surface` 2.8 s,
`standard-library` 0.9 s, kernel 0.05 s.

With the optimizer artifacts cached (state 3), `compile-optimize` is still
3.9 s, of which `load-compiler` is 3.0 s — the compiler library is expensive
to replay even warm.

## Phase 4 optimizations

### 1. Reuse the install cache for the native Scheme surface

`base-functions.scm`, `native-hash-adapter.scm` and `native-abi.scm` were
loaded with the uncached `load_source` on every start (~2.8-3.1 s).  They are
plain top-level definitions and load safely through the same
`load_cached_source` path already used for the reader and writer.

### 2. Check the program cache before reading source

`compile-file-cached-in-unit` read and parsed the whole source file before
looking up the cache, so every cache hit paid for a parse it discarded.  The
lookup now runs first; only a miss reads the file.

### Effect

| Measurement | before | after |
|---|---:|---:|
| warm `native-scheme-surface` stage | 3.00-3.07 s | 0.063-0.065 s |
| warm `minimal.scm` wall (median of 3) | 7.28 s | 4.28 s |
| warm `small-real.scm` wall (median of 3) | — | 4.74 s |
| state 3 `small-real.scm` wall | 12.39 s | 9.60 s |
| warm user-file on cache hit | read + parse | 0 ms (`compile-cache-hit`) |
| cold `minimal.scm` wall | 69.8-73.2 s | 71.1 s |

Warm peak RSS is unchanged (~32 MiB).  Cold is unchanged, as expected: a
genuinely empty cache still expands these files once.  The optimization turns
that one-time work into reusable artifacts, so every later start is fast.

## Optimized suite (3 samples each, medians)

| Workload | cold | warm | bootstrap | readonly |
|---|---:|---:|---:|---:|
| minimal.scm | 71.05 s / 91 MiB | 4.28 s / 32 MiB | 7.46 s / 41 MiB | 7.57 s / 40 MiB |
| small-real.scm | 72.33 s / 91 MiB | 4.74 s / 32 MiB | 9.42 s / 41 MiB | 9.64 s / 40 MiB |
| large-scheme.scm | 91.96 s / 99 MiB | 6.15 s / 41 MiB | 29.67 s / 49 MiB | 28.88 s / 49 MiB |

`bootstrap` and `readonly` (bootstrap cache present, program uncompiled) are
already inside the plan's 15 s target for the minimal and small workloads.
Cold first runs remain ~71-92 s and are dominated by one-time source
compilation of the runtime and optimizer; the large workload adds ~20 s of
program expansion plus ~10 s of pass execution (`large-scheme` bootstrap:
expand 8.7 s, run-passes 9.9 s).

## Phase 5 gates

- `tests/expander/`: 21/21 pass.
- changed-since gate (`./bin/gf test`, 162 files): 162/162 pass.
- `tools/test-native-startup-cache.sh`: replay, all five modes, corruption,
  stale digest and read-only fallback pass.
- `tools/test-native-cold-bootstrap.sh`: cold bootstrap pass.
- Corrupt program artifact: detected as a miss, recompiled, correct output,
  artifact rewritten.
- Two processes initializing the same fresh cache concurrently: both exit 0
  with correct output.

## Scope

Cold first runs cannot reach 15 s without shipping prebuilt artifacts: the
~36 s boot compile and the ~34 s optimizer compile are intrinsic source
compilation on an empty cache.  The measured improvements are on the warm and
"bootstrap cache present" paths, which is what a user sees after the first
run.  Remaining follow-ups: replaying `(goldfish compiler)` warm costs ~3 s
per program; `large-scheme` optimization is proportional to program size; and
`load` still reads a program's source before the cache check for library-name
detection.
