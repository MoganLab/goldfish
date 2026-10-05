# Million-element set workload

The scale case mirrors `tests/liii/set/set-size-test.scm`: it creates the
1,000,000-element list and set, retains them while creating the 999,999-element
list and set, and checks both sizes and boundary membership. The final run uses
the same reverse-built `range` function as the test. Each sample has a 300-second
hard timeout; the recorder has a 420-second outer deadline.

## Result

The original set test now passes on the native runtime. Its recorded run took
130.40 seconds and peaked at 704,768 KiB. The matching standalone workload took
128.64 seconds and peaked at 689,432 KiB; all construction phases and checks
completed. The process-level difference comes from the test runner and its
reporting overhead.

| Sample | Elements per set | Set construction | Whole process | Peak RSS | Result |
|---|---:|---:|---:|---:|---|
| Before insertion change | 100,000 / 99,999 | 25.80 s | 35.28 s | 91,168 KiB | pass |
| After insertion change | 100,000 / 99,999 | 9.97 s | 19.54 s | 90,336 KiB | pass |
| Before insertion change | 250,000 / 249,999 | 64.60 s | 74.89 s | 193,820 KiB | pass |
| After insertion change | 500,000 / 499,999 | 50.15 s | 62.14 s | 355,660 KiB | pass |
| Final, test-matching workload | 1,000,000 / 999,999 | 113.08 s | 128.64 s | 689,432 KiB | pass |

The 100,000-element before/after samples use the same workload and input shape.
They show a 61% reduction in set-construction time; peak memory is effectively
unchanged. The 250,000 and 500,000 rows are scale-ladder samples, not a direct
A/B comparison. Results are single samples, so treat the exact percentages as
indicative rather than a benchmark distribution.

## Change

Set insertion now calls the existing low-level hash-table setter directly from
the private set implementation. The set always owns a valid table, so this
avoids constructing and validating the general variadic `hash-table-set!`
argument list for each element. Comparator-based hashing and equality remain in
the same table operation. The entire 47-file set test directory passed, as did
the original million-element set-size test on its own. The changed-since-main
native gate also passed: 162 test files, 0 failures.

## Reproduction and records

Run from the repository root with a prepared cache:

```sh
sh tools/bench-native-scale.sh --smoke --case=million-set --size=1000000 \
  --phases --timeout=300 --setup-timeout=180 --run-timeout=420 \
  --cache=/path/to/cache --output=/tmp/gf-million-set
```

The cache must match the current runtime. `baseline-100k` and `baseline-250k`
record the pre-change ladder; `fast-100k` and `fast-500k` record the candidate
ladder; `exact-1m` records the final benchmark using the test's list-building
behavior. Each directory contains the input, phase output, process metrics and
run metadata. The original test's output and resource measurement are in
`../../tests/native-baseline/set-million-recheck.log` and
`../../tests/native-baseline/set-million-recheck.metrics`.
