# C2 parity acceptance

Date: 2026-09-28

## Result

The current float-free manifest contains 1,331 files. The completed C2 sweep
reported 1,273 host/native agreement passes and no remaining divergences;
58 files were excluded by the explicit bucket list in `c2-skip.tsv`. A later
strict parity slice covered the three former call/cc exclusions: all three
agree, bringing the in-scope total to 1,276 passes and the remaining exclusions
to 55. On 2026-09-28, a dedicated strict paired run of
`tests/liii/match-capability-test.scm` also passed on both hosts. The current
record is therefore 1,277 agreement passes and 54 remaining exclusions. This
is not a rerun of the full manifest.
The per-file output from that sweep was not retained, so this is an aggregate
record, not a reproducible raw log. The final known shared failure from the
earlier slice (`scheme/base/vector-copy-test.scm`) was rerun on both hosts on
2026-09-27 and passes on each.

The 54 remaining bucketed files are grouped as follows:

| Bucket | Files | Scope |
| --- | ---: | --- |
| `native-ffi` | 38 | njson, subprocess, and UUID still rely on host-only glue |
| `random` | 8 | native PRNG/random-state work is pending |
| `reader` | 3 | TinyReader does not implement the custom raw-string syntax |
| `time` | 1 | million-element set stress belongs to a dedicated scale run |
| `s7-compat` | 1 | S7 hook invocation is not part of the native surface |
| `introspection` | 1 | S7 procedure signature metadata is unavailable natively |
| `float/numeric` | 1 | complex construction depends on the float workstream |
| `numeric-tower` | 1 | the minimum int64 literal needs the bignum reader |
| **Total** | **54** | |

The remaining bucketed files are exclusions, not passes. Revisit each bucket
when its owning runtime workstream lands; do not remove an entry merely to
increase the pass count.
The paired match-capability audit used
`C2_STRICT=1 C2_SKIP_MANIFEST=/dev/null C2_HOST_TIMEOUT=900 C2_TIMEOUT=900 sh tools/c2-compare.sh tests/liii/match-capability-test.scm`.
It reported 1 agree-pass, 0 agree-fail, 0 divergences, and 0 missing verdicts.
The host side took about eight minutes; its earlier 60-second profile was
dominated by the legacy S7 evaluator and association-list lookups.

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

The call/cc slice can be repeated independently:

```sh
C2_STRICT=1 sh tools/c2-compare.sh \
  tests/scheme/base/call-slash-cc-test.scm \
  tests/scheme/base/call-with-current-continuation-test.scm \
  tests/srfi/srfi-158-test.scm
```
