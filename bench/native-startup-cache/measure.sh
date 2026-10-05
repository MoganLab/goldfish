#!/bin/sh
# Warm-cache startup samples; no collection workload or unbounded process.
set -eu
project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$project_dir"
test "$#" = 2 || { echo "usage: sh $0 CACHE OUTPUT" >&2; exit 2; }
export GOLDFISH_CACHE_DIR=$1
output=$2
test ! -e "$output" || { echo "output must not already exist" >&2; exit 2; }
mkdir -p "$output"
bin/gf --check-bootstrap-cache > "$output/cache-version.txt"
sha256sum bin/gf > "$output/binary.sha256"
for mode in r7rs liii; do
    # Prepare optional source-install and mode-import caches outside samples.
    timeout -k 5 90 bin/gf -m "$mode" -e '(+ 20 22)' > "$output/$mode-prepare.stdout"
    for sample in 1 2 3; do
        timeout -k 5 60 env GOLDFISH_DEBUG=timing time -v \
            bin/gf -m "$mode" -e '(+ 20 22)' \
            > "$output/$mode-$sample.stdout" 2> "$output/$mode-$sample.stderr"
        test "$(cat "$output/$mode-$sample.stdout")" = 42
    done
done
