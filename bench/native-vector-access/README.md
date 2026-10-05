Vector access now reads checked vector storage directly. `vector-ref` and
`vector-length` retain their type, arity and bounds checks without copying the
whole vector. `Evaluator::vector_values` remains a snapshot API for artifact
serialization. The optimization follows the prior
[perf diagnosis](../native-perf-rehash/ANALYSIS.md).

The [tested patch](tested.patch) and [metadata](metadata.tsv) identify the exact
source and executable. A fresh content-addressed bootstrap cache was prepared
for the rebuilt binary. No Scheme implementation library changed.

## Verification

The native affected-file run covers vector access, modification, copying,
iteration and conversion, small set operations, SRFI-125 and repeated hash
growth. Of 22 files, 21 pass. The additional `make-vector-test.scm` constructor
check fails on its already-deferred negative-length error tag, recorded in
`tests/native-baseline/dispositions.tsv`; constructor code and its expectation
are unchanged. The full run is retained as failed, not relabeled green.
The modified vector-ref file and all files directly exercising the changed
primitives pass, including error checks and new alias/identity regressions.
See [raw results](regression/affected.log).

The auxiliary C++ evaluator test has pre-existing assertion-gate problems.
Release defines NDEBUG, and forcing assertions uncovers stale code and
expectations. Its apparent release success is not counted as assertion
coverage; details are recorded in [the follow-up note](regression/CXX-CHECK-NOTE.txt).
Temporary investigative C++ test edits were discarded.

## Same-input performance confirmation

The existing 20,000-access vector probe completes all six checks. Times include
the common loop, sum and final check:

| Width | vector-ref before / after | vector-length before / after |
|---:|---:|---:|
| 8 | 0.11191 / 0.07839 seconds | 0.10871 / 0.07335 seconds |
| 8,192 | 0.22633 / 0.07822 seconds | 0.19594 / 0.07637 seconds |
| 65,536 | 1.16665 / 0.07895 seconds | 0.93339 / 0.07820 seconds |

The within-process width dependence disappears. Before/after numbers are
exploratory single samples, not controlled statistical speedup estimates.

The same 50,000-element profile input and FIFO-controlled interval complete in
6.30822 seconds versus the previous 11.95513 seconds. All cardinality and
membership checks pass; 624 samples are recorded, none lost. The
`Evaluator::vector_values` path is absent from the full inclusive report;
`memmove` self CPU share falls from 17.92% to 2.56%. Remaining hotspots include
the evaluator, environment lookup, transient allocation and GC. The perf
capture excludes startup, imports and list generation, and includes checks.

Reproduction uses the commands in
[the prior method](../native-perf-rehash/METHOD.txt), with a fresh cache matching
the new executable. The vector input and the set-wide input are unchanged.
The profile uses `cpu-clock:u`, 99 Hz, DWARF 8,192-byte stacks and monotonic
time. The symbol executable is relinked from the current objects; its .text is
verified byte-identical to bin/gf. The set target deadline remains 90 seconds
and the recorder's outer deadline 120 seconds, each with 5-second kill grace.
No semantic full suite or million-element run is included in this repair gate.

## Bounded scale recheck

After committing the repair as `e71831ad`, the original benchmark's
100,000-element sample completes: construction 12.49798 seconds, list generation
0.24921 seconds, startup/timer import 15.88730 seconds and benchmark import
2.68156 seconds. All checks pass. Whole-process wall time is 31.34 seconds,
peak RSS 82,672 KiB, exit code 0; the preceding attempt timed out at 60 seconds
without completing construction. The two runs' different fixed costs prevent
assigning the entire improvement to the vector change.

Command: `sh tools/bench-native-scale.sh --smoke --case=million-set --size=100000
--phases --timeout=60 --setup-timeout=90 --run-timeout=240
--cache=/tmp/gf-vector-access-fix/cache
--output=/tmp/gf-vector-access-fix/scale100000` (join into one shell line).
Raw phase, whole-process and setup records are in [scale100000](scale100000/).
Peak RSS describes this completed process, not just the construction phase;
the earlier timeout's RSS is not comparable as a full-workload memory result.

Applying the existing scheduling guard (fixed time plus variable phases scaled
by ten) projects 146.07 seconds for one million elements. This exceeds the
60-second sample budget, so the ladder stops without running that level. It is
a scheduling estimate, not a measured million-element result. The original
test creating two near-million-element sets remains deferred. Startup/source
installation and remaining interpreter overhead are the next measured targets.
