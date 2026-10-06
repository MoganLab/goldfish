#!/bin/sh
# Run the stage micro-benchmarks.
#
#   sh bench/micro/run.sh [OUTFILE]
#
# Environment:
#   GF_BIN        binary under test (default bin/gf)
#   MICRO_CACHE   cache directory (default /tmp/gf-micro)
#   MICRO_REPEATS process repetitions, min taken (default 5)
#
# Covers reader, serializer/deserializer, evaluator and allocation.
# Expander / optimizer / cache-write splits come from GOLDFISH_DEBUG=timing
# on a program compile (see bench/cold-start/LIB-LEDGER.md); the expander
# cannot be driven from user code without resetting the program library.
#
# Output is TSV: name<TAB>min_total_ms<TAB>iters.  The box is shared, so the
# minimum over several process runs is the stable statistic.
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$project_dir"

GF_BIN=${GF_BIN:-bin/gf}
MICRO_CACHE=${MICRO_CACHE:-/tmp/gf-micro}
MICRO_REPEATS=${MICRO_REPEATS:-5}
mkdir -p "$MICRO_CACHE"

i=1
while [ "$i" -le "$MICRO_REPEATS" ]; do
    GOLDFISH_CACHE_DIR="$MICRO_CACHE" "$GF_BIN" bench/micro/core.scm \
        2>/dev/null | awk -F'\t' 'NF==3'
    i=$((i + 1))
done | awk -F'\t' '
    { if (!($1 in best) || $2 < best[$1]) { best[$1] = $2; it[$1] = $3 } }
    END { for (name in best) print name "\t" best[name] "\t" it[name] }
' | sort | tee "${1:-/dev/stdout}"
