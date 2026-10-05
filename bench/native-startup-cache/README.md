# Native startup source-install cache

This change reuses the Scheme `install-library-file!` cache for the native
reader and writer. A dedicated source library retains forward references and
local transformers; its value bindings are published as the same global
aliases used by the source loader. Other bootstrap sources still expand on
each startup. No host-produced artifact or new cache format is required.

The cache keeps the existing source/kernel stamps and pipeline/runtime
fingerprint. Missing, unreadable, corrupt or stale entries expand from source.
Read-only caches use the source path without attempting writes. Startup timing
now separates post-standard collection, mode imports, reader and writer.

## Measurement

The baseline is the binary at `d5902985`, with the vector-access repair already
present. Both runs use a validated warm native bootstrap cache, the same
`r7rs` mode and expression `(+ 20 22)`, and GNU time's peak RSS. Cache preparation
is outside the measurement. Both return 42. The original before/after sample
logs are retained, including the existing baseline timing label that groups
mode imports, collection, reader and writer together.

| Measurement | Before | After, first sample |
|---|---:|---:|
| Whole process | 16.62 s | 6.21 s |
| Boot diagnostic total | 16.597 s | 6.201 s |
| Peak RSS | 48,560 KiB | 31,396 KiB |
| Reader, writer, imports and preceding collection | 10.214 s | 0.169 s |

The after interval consists of collection 4 ms, imports 71 ms, reader 92 ms,
writer 2 ms. This comparison is exploratory, not a statistical performance
guarantee. First-time native warming remains a separate operation: the recorded
isolated warm took 111.86 seconds and includes source bootstrap and compiler
work; its reader source expansion took 10.275 seconds.

After the regression gate finished, three further warm samples give:

| Mode | Whole-process seconds | Peak RSS, KiB |
|---|---|---|
| R7RS | 6.48, 6.46, 6.41 | 31,748, 28,620, 32,208 |
| Liii | 7.73, 7.01, 7.00 | 31,808, 31,828, 31,904 |

The R7RS median is 6.46 seconds; only one pre-change baseline sample was
captured. Full logs, binary hash and cache version are under `repeated/`.

Reproduce preparation and repeated measurements with a private cache:

```sh
GOLDFISH_CACHE_DIR=/tmp/gf-startup-sample sh tools/warm-bootstrap-cache.sh
sh bench/native-startup-cache/measure.sh /tmp/gf-startup-sample /tmp/gf-startup-results
GOLDFISH_CACHE_DIR=/tmp/gf-startup-sample sh tools/test-native-startup-cache.sh
GOLDFISH_CACHE_DIR=/tmp/gf-startup-sample sh tools/test-native-cold-bootstrap.sh
```

Every benchmark process has a deadline. The regression script copies only the
selected cache into a temporary directory and mutates that copy. It checks
missing entries, unchanged cache mtimes on replay, all five explicit modes,
reader corruption, writer source-digest mismatch and read-only fallback. Its
semantic checks include case directives, escaped symbols, rationals, Unicode
strings and cyclic vector read/write round trips.

## Regression limits

The affected `tests/liii/reader/` aggregate remains **failed**: one file passes,
the existing deferred `reader-test.scm` fails numeric prefixes. Its source
success path is inside `unless`, and reloading that source reproduces the
failure. This defect is recorded in `tests/NATIVE-FOLLOWUPS.tsv` and the
original native baseline; it is not introduced or fixed by this optimization.
Keep this failed aggregate separate from the successful targeted checks.

The native cold-bootstrap script, startup-cache regression, five writer files,
and four reader/program-cache files pass, along with the 161-file changed-since
gate. The four-file log contains check
counts as well as process results. Release C++ assertion-only tests are not
counted as evidence. No full native suite or million-element workload is run
for this change. Remaining startup cost is mostly installation of `install.scm`
and the native Scheme surface; larger collection budgets still depend on
construction time, not just startup time.

## Bounded collection recheck

The same 100,000-element phase probe passes in 22.30 seconds, peak RSS
65,760 KiB. Startup/timer import takes 6.654 seconds, benchmark import 2.694,
list generation 0.255, construction 12.675, and checks 0.000205. Compared with
the preceding 31.34-second sample, the improvement is in startup; construction
remains about 12.5–12.7 seconds. These are single exploratory samples.

Reproduce with:

```sh
sh tools/bench-native-scale.sh --smoke --case=million-set --size=100000 --phases \
  --timeout=60 --setup-timeout=90 --run-timeout=240 \
  --cache=/tmp/gf-startup-sample --output=/tmp/gf-startup-scale100000
```

The decade scheduling estimate is `wall + 9 * (list-build + set-build + check)`:
138.676 seconds for one million elements, above the 60-second budget. Stop here;
the million-element workload is not executed and its original two-set test
remains deferred. Raw input, phases, timings, cache preparation, binary/source
metadata and the tested source patch are archived under `scale100000/`, excluding
the copied cache directory.
