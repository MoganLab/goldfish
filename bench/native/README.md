# Native performance probes

Run each probe with the native runtime:

```sh
./bin/gf-native -m liii bench/native/string-to-utf8.scm
./bin/gf-native -m liii bench/native/evaluator-loops.scm
GOLDFISH_PROF_OUT=/tmp/native-perf.data \
  tools/prof.sh ./bin/gf-native -m liii bench/native/evaluator-loops.scm
```

The probes report raw monotonic nanoseconds. They do not use `(liii timeit)`:
native currently has an exact-only numeric tower, so converting elapsed
nanoseconds to fractional seconds with integer `/` truncates small timings to
zero. Each timed case has a warmup and five samples; compare medians for the
same binary and workload, and keep the profiler workload unchanged.

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
