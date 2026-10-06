#!/bin/sh
# Run the stage micro-benchmarks.
#
#   sh bench/micro/run.sh [OUTFILE]
#
# Environment:
#   GF_BIN       binary under test (default bin/gf)
#   MICRO_CACHE  cache directory (default /tmp/gf-micro)
#
# Covers reader, serializer/deserializer, evaluator and allocation.
# Expander / optimizer / cache-write splits come from GOLDFISH_DEBUG=timing
# on a program compile (see bench/cold-start/LIB-LEDGER.md); the expander
# cannot be driven from user code without resetting the program library.
#
# Output is TSV: name<TAB>total_ms<TAB>iters.
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$project_dir"

GF_BIN=${GF_BIN:-bin/gf}
MICRO_CACHE=${MICRO_CACHE:-/tmp/gf-micro}
mkdir -p "$MICRO_CACHE"

GOLDFISH_CACHE_DIR="$MICRO_CACHE" "$GF_BIN" bench/micro/core.scm \
    | awk -F'\t' 'NF==3' | tee "${1:-/dev/stdout}"
