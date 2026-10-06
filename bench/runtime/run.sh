#!/bin/sh
# Runtime throughput suite: fixed compute workloads over a warm cache.
#
#   sh bench/runtime/run.sh OUTDIR [samples]
#
# Environment:
#   GF_BIN        binary under test (default bin/gf)
#   RUNTIME_CACHE cache directory (default OUTDIR/caches/shared, prepared once)
#   RUNTIME_TIMEOUT per-process seconds (default 300)
#
# Each program runs with a warm bootstrap cache (the shared root is prepared
# by one cold run); wall time therefore includes the constant warm boot, so
# boot improvements (Phase 2/3) and evaluator improvements both move these
# numbers.  summary.tsv matches bench/cold-start/run-suite.sh's format
# (state "run"), so bench/cold-start/compare.sh compares two runs unchanged.
#
# Program stdout is checked against progs/<name>.expected; a mismatch fails
# the suite immediately (the gate must catch semantics, not just timing).
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$project_dir"

test "$#" -ge 1 || { echo "usage: sh $0 OUTDIR [samples]" >&2; exit 2; }
output=$1
samples=${2:-3}
test ! -e "$output" || { echo "output must not already exist: $output" >&2; exit 2; }

GF_BIN=${GF_BIN:-bin/gf}
RUNTIME_TIMEOUT=${RUNTIME_TIMEOUT:-300}
cache_root=${RUNTIME_CACHE:-}
rusage="$project_dir/bench/cold-start/rusage"
test -x "$rusage" || { echo "run: build bench/cold-start/rusage first" >&2; exit 2; }
test -x "$GF_BIN" || { echo "run: no binary at $GF_BIN" >&2; exit 2; }

programs=$(ls "$project_dir"/bench/runtime/progs/*.scm)
test -n "$programs" || { echo "run: no programs under bench/runtime/progs/" >&2; exit 2; }

mkdir -p "$output/logs" "$output/caches"
log_root="$output/logs"

if [ -z "$cache_root" ]; then
    cache_root="$output/caches/shared"
    rm -rf "$cache_root"
fi
mkdir -p "$cache_root"

# Run a program, capturing stdout/stderr/rusage; fail on a timeout or a
# non-zero exit, like run-suite's compare gate would.
run_one() { # workload abs-path tag
    local tag=$3
    if ! GOLDFISH_CACHE_DIR="$cache_root" \
        timeout -k 5 "$RUNTIME_TIMEOUT" \
        "$rusage" "$project_dir/$GF_BIN" "$2" \
        > "$log_root/$tag.stdout" 2> "$log_root/$tag.stderr" \
        3> "$log_root/$tag.rusage"
    then
        echo "run: $tag failed -- see $log_root/$tag.stderr" >&2
        exit 1
    fi
}

check_output() { # workload tag
    local expected="$project_dir/bench/runtime/progs/$(basename "$1" .scm).expected"
    if ! diff -q "$expected" "$log_root/$2.stdout" >/dev/null; then
        echo "run: $2 produced wrong output -- expected $(cat "$expected")" >&2
        exit 1
    fi
}

emit() { # workload sample rusage-file
    eval "$(tr ' ' '\n' < "$3" | sed 's/^/r_/')"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(basename "$1")" "run" "$2" \
        "$r_wall" "$r_user" "$r_sys" "$r_maxrss_kib" "$r_exit" >> "$output/summary.tsv"
}

printf 'workload\tstate\tsample\twall\tuser\tsys\tmaxrss_kib\texit\n' > "$output/summary.tsv"
{
    printf 'field\tvalue\n'
    printf 'revision\t%s\n' "$(git rev-parse HEAD)"
    printf 'binary_sha256\t%s\n' "$(sha256sum "$GF_BIN" | cut -d' ' -f1)"
    printf 'samples\t%s\n' "$samples"
    printf 'cpu\t%s\n' "$(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2- | sed 's/^ //')"
    printf 'date\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$output/metadata.tsv"

# One cold pass over every program: the shared cache captures the bootstrap
# artifacts and each program's compiled artifact, so every sampled run is a
# pure warm start + replay.
for prog in $programs; do
    name=$(basename "$prog" .scm)
    run_one "$prog" "$prog" "prepare-$name"
    check_output "$prog" "prepare-$name"
done

for prog in $programs; do
    name=$(basename "$prog" .scm)
    n=1
    while [ "$n" -le "$samples" ]; do
        tag="$name-$n"
        run_one "$prog" "$prog" "$tag"
        check_output "$prog" "$tag"
        emit "$prog" "$n" "$log_root/$tag.rusage"
        n=$((n + 1))
    done
done

# Cache manifest for the shared root, then drop the heavy tree.
version=$(GOLDFISH_CACHE_DIR="$cache_root" "$GF_BIN" --bootstrap-cache-directory)
(cd "$version" && find . -type f -printf '%s\t%p\n' | sort -k2) \
    > "$output/caches/manifest-shared.tsv"
rm -rf "$cache_root"
echo "run: results in $output/summary.tsv"
