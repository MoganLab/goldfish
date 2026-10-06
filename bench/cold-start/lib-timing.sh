#!/bin/sh
# Per-library cache ledger.
#
# Isolates one library's warm restore and cold compile in a warm bootstrap
# cache, so standard-library and user-library costs can be compared on the
# same shared cache mechanism.
#
#   sh bench/cold-start/lib-timing.sh CACHE_ROOT LIB_PATH [SAMPLES] [MODE]
#
# LIB_PATH is repo-relative, e.g. liii/base.scm or scheme/char.scm.
# Prints the [timing] lib-* marks for that path on stdout, with a TSV
# summary line per sample.
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$project_dir"

test "$#" -ge 2 || { echo "usage: sh $0 CACHE_ROOT LIB_PATH [SAMPLES] [MODE]" >&2; exit 2; }
cache=$1
lib=$2
samples=${3:-3}
mode=${4:-r7rs}
expr="(import ($(printf '%s' "$lib" | sed 's/\.scm$//' | tr '/' ' ')))"

# A warm bootstrap cache plus the target library compiled once (cold if
# absent); the prepare run's cost is not recorded.
GOLDFISH_CACHE_DIR="$cache" timeout -k 5 300 bin/gf -m "$mode" -e "$expr" \
    >/dev/null 2>&1
version=$(GOLDFISH_CACHE_DIR="$cache" bin/gf --bootstrap-cache-directory)
artifact="$version/$lib-o2.gfo"
test -f "$artifact" || { echo "lib-timing: no artifact at $artifact" >&2; exit 2; }

run() {
    GOLDFISH_CACHE_DIR="$cache" GOLDFISH_DEBUG=timing \
        timeout -k 5 120 bin/gf -m "$mode" -e "$expr" \
        > /dev/null 2> "$cache/lib-timing.stderr" || {
            echo "lib-timing: import failed" >&2; cat "$cache/lib-timing.stderr" >&2; exit 1; }
}

printf 'state\tsample\tsegments\n'
n=1
while [ "$n" -le "$samples" ]; do
    run
    segs=$(grep -F " $lib " "$cache/lib-timing.stderr" | tr '\n' ';' || true)
    printf 'warm\t%s\t%s\n' "$n" "$segs"
    n=$((n + 1))
done

n=1
while [ "$n" -le "$samples" ]; do
    rm -f "$artifact"
    run
    segs=$(grep -F " $lib " "$cache/lib-timing.stderr" | tr '\n' ';' || true)
    printf 'cold\t%s\t%s\n' "$n" "$segs"
    n=$((n + 1))
done
