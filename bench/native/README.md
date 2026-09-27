# Native performance probes

Run each probe with the native runtime:

```sh
./bin/gf-native -m liii bench/native/string-to-utf8.scm
./bin/gf-native -m liii bench/native/evaluator-loops.scm
./bin/gf-native -m liii bench/native/continuations.scm
GOLDFISH_DEBUG=gc ./bin/gf-native -m liii bench/native/allocation-gc.scm
GOLDFISH_PROF_OUT=/tmp/native-perf.data \
  tools/prof.sh ./bin/gf-native -m liii bench/native/evaluator-loops.scm
```

The probes report raw monotonic nanoseconds. They do not use `(liii timeit)`:
native currently has an exact-only numeric tower, so converting elapsed
nanoseconds to fractional seconds with integer `/` truncates small timings to
zero. Each timed case has a warmup and five samples; compare medians for the
same binary and workload, and keep the profiler workload unchanged.

`continuations.scm` compares the same tail loop with repeated `call/cc`
capture, then measures one capture with 500 pending non-tail frames. This is a
cost probe, not a host/native comparison; run it on an otherwise idle machine
before making a snapshot representation decision.

## Continuation snapshot probe (2026-09-27)

One local run of `continuations.scm` produced these medians (five timed
samples, raw samples below):

| Workload | Median | Raw samples (ns) |
| --- | ---: | --- |
| Tail loop, 2,000 steps | 2,270,360 ns | 2,354,015; 2,270,360; 2,264,550; 2,255,954; 2,316,480 |
| Same loop with 2,000 captures | 3,680,373 ns | 3,638,610; 3,656,271; 3,703,563; 3,697,082; 3,680,373 |
| Nested return, depth 500 | 564,211 ns | 557,631; 566,966; 564,211; 561,216; 570,012 |
| Capture at depth 500 | 579,749 ns | 587,042; 583,956; 571,424; 578,136; 579,749 |
| Nested return, depth 5,000 | 5,193,023 ns | 5,190,078; 5,189,688; 5,249,532; 5,193,023; 5,228,546 |
| Capture at depth 5,000 | 5,392,380 ns | 5,392,380; 5,479,722; 5,359,904; 11,589,942; 5,307,463 |

Repeated shallow capture cost about 1.62x the tail-loop sample per step in
this run. Single captures were about 2.8% above a same-depth return at depth
500 and 3.8% at depth 5,000. One large outlier remains in the deeper capture
samples; background load and GC can dominate these measurements. This does
not justify adding COW complexity yet, but it sets a concrete baseline for
future reserved-machine profiling.

## Exploratory result (2026-09-27)

These measurements were taken on a shared machine with variable background
load. They indicate direction only and are not release-grade thresholds:

| Native workload | Before | After | Change |
| --- | ---: | ---: | ---: |
| `string->utf8`, 16-byte ASCII | 2.49 us/op | 2.09 us/op | about 16% faster |
| `string->utf8`, 1024 CJK chars | 87.5 us/op | 78.8 us/op | about 10% faster |
| tail lexical loop, 20,000 steps | 16.64 ms | 15.33 ms | about 8% faster |
| recursive `fib(24)` | 103.3 ms | 96.5 ms | about 7% faster |

The UTF-8 fast path avoids building a character-offset vector when no slice is
requested. The evaluator probe's profile initially attributed about 5.9% of
self time to `Environment::lookup`; after changing parent-chain lookup to an
iterative walk, it no longer appeared among the top twelve sampled frames.
GC and evaluator dispatch remain the largest costs. Repeat these probes on a
reserved machine before using the percentages as a release decision.

Raw `ns/op` samples behind the medians above:

| Workload | Before samples | After samples |
| --- | --- | --- |
| UTF-8, 16-byte ASCII | 2369, 2405, 2998, 2487, 2986 | 2088, 2063, 2473, 2063, 2515 |
| UTF-8, 1024 CJK chars | 88364, 84794, 86066, 87493, 96394 | 79676, 78810, 77640, 77569, 80793 |
| Tail lexical loop, 20,000 steps | 16706205, 16636498, 16684095, 16523133, 16582971 | 15231765, 15372483, 15325096, 15359209, 15294701 |
| Recursive `fib(24)` | 102961263, 104781119, 102920640, 103296613, 113164702 | 96064951, 96543567, 96252765, 97614586, 96703941 |

The evaluator profiles are retained locally as
`/tmp/gf-native-evaluator-before.data` and
`/tmp/gf-native-evaluator-after.data`; the UTF-8 profiles are
`/tmp/gf-native-utf8-before.data` and `/tmp/gf-native-utf8-final.data`.
These profiler files are machine-local and are not repository artifacts.

## Allocation and GC baseline (2026-09-27)

`allocation-gc.scm` allocates and converts 10,000 256-byte strings per sample;
every run returns 2,560,000 bytes. On the shared validation machine, five raw
samples were 110130281, 111698938, 121605984, 110020431, and 111080671 ns.
With `GOLDFISH_DEBUG=gc`, the process reported 285 collections and a maximum
reported heap of 70,584 KiB. Raw output and collector diagnostics are retained
locally at `/tmp/c3-allocation-gc-2026-09-27.out` and
`/tmp/c3-allocation-gc-2026-09-27.err`. These are reproducible workload and
collector observations, but timing remains exploratory because the machine
had variable background load.
