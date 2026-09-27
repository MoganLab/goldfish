# C2 parity acceptance

Date: 2026-09-27

## Result

The current float-free manifest contains 1,331 files. The completed C2 sweep
reported 1,273 host/native agreement passes and no remaining divergences;
58 files were excluded by the explicit bucket list in `c2-skip.tsv`.
The per-file output from that sweep was not retained, so this is an aggregate
record, not a reproducible raw log. The final known shared failure from the
earlier slice (`scheme/base/vector-copy-test.scm`) was rerun on both hosts on
2026-09-27 and passes on each.

The 58 bucketed files are grouped as follows:

| Bucket | Files | Scope |
| --- | ---: | --- |
| `native-ffi` | 38 | njson, subprocess, and UUID still rely on host-only glue |
| `random` | 8 | native PRNG/random-state work is pending |
| `reader` | 3 | TinyReader does not implement the custom raw-string syntax |
| `engine-callcc` | 3 | native continuation support is pending |
| `time` | 2 | known slow/stress cases exceed the current sweep budget |
| `s7-compat` | 1 | S7 hook invocation is not part of the native surface |
| `introspection` | 1 | S7 procedure signature metadata is unavailable natively |
| `float/numeric` | 1 | complex construction depends on the float workstream |
| `numeric-tower` | 1 | the minimum int64 literal needs the bignum reader |
| **Total** | **58** | |

These are exclusions, not passes. Revisit each bucket when its owning runtime
workstream lands; do not remove an entry merely to increase the pass count.
For `match-capability-test.scm`, a 60-second host profile was dominated by the
legacy S7 evaluator and association-list lookups. The sample does not yet
identify which individual Scheme form accounts for the cost. The full host
suite's shared worker passed the test, and a direct native `--each-file` run
passed. C2 still lacks a paired verdict because its host per-file run exceeds
the 300-second timeout.

## Post-acceptance regression check

After the native compatibility changes in `dacbc3d6`, UTF-8 fast path in
`a08ec000`, and iterative environment lookup, affected in-scope C2 tests were
rerun with `C2_STRICT=1`: 42 agree-pass, 0 agree-fail, 0 divergences, and 0
missing verdicts across two slices. Captured summaries are
`/tmp/c2-post-change-slice-2026-09-27.log` (36 files) and
`/tmp/c2-environment-post-lookup-2026-09-27.log` (6 files) on the validation
machine. These are regression checks for the subsequent changes, not a rerun
of the full manifest.

The M3 lowered-program guard also passed all 12 `tests/gf0/m2a-*.scm` cases
on 2026-09-27 (`tools/diff-gf0-m2a.sh`; captured at
`/tmp/m3-m2a-guard-2026-09-27.log`).

## Repeatable gate

```sh
sh tools/check-c2-manifest.sh
C2_STRICT=1 sh tools/c2-compare.sh
```

The manifest checker verifies unique/in-scope paths and non-empty skip
reasons. Strict compare mode returns failure for any divergence, missing
verdict, or same-failure result. For routine changes, run a narrow slice with
`C2_STRICT=1` rather than repeating the full sweep.
