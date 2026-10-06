# Runtime throughput suite

Fixed compute workloads over a warm cache — the "how fast do programs run
after startup" metric, plus a semantics guard: every program's stdout is
checked against its `.expected` file, so an evaluator change that breaks
behavior fails the suite even when it gets faster.

## Usage

    sh bench/runtime/run.sh OUTDIR [samples]     # summary.tsv + logs
    sh bench/cold-start/compare.sh BASE CAND     # same gate as the startup suite

`summary.tsv` matches `bench/cold-start/run-suite.sh`'s format (state
"run"), so the startup regression gate compares runtime runs unchanged.  A
shared cache root is prepared by one cold pass over every program (bootstrap
artifacts + each program's compiled artifact), so sampled runs are pure warm
start + replay: boot improvements and evaluator improvements both move these
numbers.  Per-program stdout must match `progs/<name>.expected` or the run
fails immediately.

## Programs

| program | probes |
|---|---|
| fib.scm   | naive Fibonacci 27 — deep two-way recursion, closure calls, small ints |
| sum.scm   | tail loop over 2e6 — tightest eval-loop / int arithmetic dispatch |
| nqueens.scm | backtracking over a vector — mixed tail/non-tail recursion |
| winders.scm | 250k escapes through nested dynamic-wind + call/cc — the KontFrame winder path |
| lists.scm | 300k cons + filter/map/fold + assq lookups — allocation and HOF pressure (peak RSS ~190 MiB: the GC probe) |

## Notes

- The `lists.scm` pipeline originally used `assoc` over a 2000-entry table:
  ~3 µs per cons step (59 s for 20k lookups) versus `assq` at ~5.5 ns —
  recorded as a measured primitive hotspot for the Phase 3 levers, not
  encoded in the suite.
- Baselines live as `phase3-*/` directories (summary.tsv, metadata.tsv with
  revision + binary sha256, raw logs); add their checksums to
  `../cold-start/checksums.sha256` when landing a record.
