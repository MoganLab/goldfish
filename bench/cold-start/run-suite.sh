#!/bin/sh
# Cold/warm startup suite for the cold-start workstream.
#
# Measures a native gf binary over the fixed workload set and cache states
# described in the cold-start plan.  Each process runs under a private
# GOLDFISH_CACHE_DIR and a bounded deadline; the rusage helper records wall,
# user, system and peak RSS.  Nothing here touches the user's default cache.
#
# Usage:
#   sh bench/cold-start/run-suite.sh OUTDIR [samples]
#
# Environment:
#   GF_BIN        binary under test        (default bin/gf)
#   GOLDFISH_OPT_LEVEL  cache level        (default 2, matching -o2)
#   SUITE_STATES  space list of states     (default "cold warm bootstrap readonly")
#   SUITE_WORKLOADS space list of .scm     (default all in workloads/)
#   SUITE_TIMEOUT per-process seconds      (default 300)
#
# Output: OUTDIR/summary.tsv, OUTDIR/metadata.tsv, OUTDIR/logs/*,
#         OUTDIR/caches/* (manifest only; caches are pruned after use).
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$project_dir"

test "$#" -ge 1 || { echo "usage: sh $0 OUTDIR [samples]" >&2; exit 2; }
output=$1
samples=${2:-3}
test ! -e "$output" || { echo "output must not already exist: $output" >&2; exit 2; }

GF_BIN=${GF_BIN:-bin/gf}
export GOLDFISH_OPT_LEVEL=${GOLDFISH_OPT_LEVEL:-2}
SUITE_STATES=${SUITE_STATES:-"cold warm bootstrap readonly"}
SUITE_WORKLOADS=${SUITE_WORKLOADS:-"bench/cold-start/workloads/minimal.scm bench/cold-start/workloads/small-real.scm bench/cold-start/workloads/large-scheme.scm"}
SUITE_TIMEOUT=${SUITE_TIMEOUT:-300}

rusage="$project_dir/bench/cold-start/rusage"
test -x "$rusage" || { echo "run-suite: build bench/cold-start/rusage first" >&2; exit 2; }
test -x "$GF_BIN" || { echo "run-suite: no binary at $GF_BIN" >&2; exit 2; }

mkdir -p "$output/logs" "$output/caches"
log_root="$output/logs"

workload_key() { # absolute path -> cache artifact key
    case "$1" in /*) printf '%s' "${1#/}";; *) printf '%s' "$1";; esac
}
abs_path() { CDPATH= cd -- "$(dirname -- "$1")" && printf '%s/%s' "$(pwd)" "$(basename -- "$1")"; }

cache_dir_for() { # cache root -> version dir
    GOLDFISH_CACHE_DIR="$1" "$GF_BIN" --bootstrap-cache-directory
}

# run_one <cache-root> <workload> <tag> [extra env via RUN_ENV]
# Writes logs/<tag>.{stdout,stderr,rusage} and echoes nothing.
run_one() {
    cache_root=$1
    workload=$2
    tag=$3
    GOLDFISH_CACHE_DIR="$cache_root" GOLDFISH_DEBUG=timing \
        env ${RUN_ENV:-} \
        timeout -k 5 "$SUITE_TIMEOUT" \
        "$rusage" "$project_dir/$GF_BIN" "$workload" \
        > "$log_root/$tag.stdout" 2> "$log_root/$tag.stderr" 3> "$log_root/$tag.rusage"
}
# Note: RUN_ENV is intentionally word-split by `env` (e.g. GOLDFISH_CACHE_READONLY=1).

artifact_for() { # cache-root workload -> artifact path
    version=$(cache_dir_for "$1")
    printf '%s/%s-o%s.gfo' "$version" "$(workload_key "$2")" "$GOLDFISH_OPT_LEVEL"
}

# Prepare one clean bootstrap-bearing cache by running the minimal workload
# cold once; its artifact for each workload is removed before the bootstrap
# measurement so only the user-file compile cost varies.
prepare_bootstrap() { # cache-root
    root=$1
    run_one "$root" "$project_dir/bench/cold-start/workloads/minimal.scm" "prepare"
}

emit() { # workload state sample rusage-file cross-check
    rfile=$5
    eval "$(tr ' ' '\n' < "$rfile" | sed 's/^/r_/')"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(basename "$1")" "$2" "$3" \
        "$r_wall" "$r_user" "$r_sys" "$r_maxrss_kib" "$r_exit" >> "$output/summary.tsv"
}

printf 'workload\tstate\tsample\twall\tuser\tsys\tmaxrss_kib\texit\n' > "$output/summary.tsv"
{
    printf 'field\tvalue\n'
    printf 'revision\t%s\n' "$(git rev-parse HEAD)"
    printf 'binary_sha256\t%s\n' "$(sha256sum "$GF_BIN" | cut -d' ' -f1)"
    printf 'opt_level\t%s\n' "$GOLDFISH_OPT_LEVEL"
    printf 'samples\t%s\n' "$samples"
    printf 'states\t%s\n' "$SUITE_STATES"
    printf 'cpu\t%s\n' "$(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2- | sed 's/^ //')"
    printf 'date\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$output/metadata.tsv"

bootstrap_cache="$output/caches/bootstrap"
prepare_bootstrap "$bootstrap_cache"

for workload in $SUITE_WORKLOADS; do
    wl_abs=$(abs_path "$workload")
    name=$(basename "$workload" .scm)
    for state in $SUITE_STATES; do
        n=1
        while [ "$n" -le "$samples" ]; do
            tag="$name-$state-$n"
            case "$state" in
            cold)
                root="$output/caches/cold-$name-$n"
                rm -rf "$root"
                run_one "$root" "$wl_abs" "$tag"
                emit "$workload" "$state" "$n" "" "$log_root/$tag.rusage"
                rm -rf "$root"
                ;;
            warm)
                root="$output/caches/warm-$name"
                if [ "$n" = 1 ]; then
                    rm -rf "$root"
                    run_one "$root" "$wl_abs" "warm-prepare-$name"
                fi
                run_one "$root" "$wl_abs" "$tag"
                emit "$workload" "$state" "$n" "" "$log_root/$tag.rusage"
                ;;
            bootstrap)
                artifact=$(artifact_for "$bootstrap_cache" "$wl_abs")
                rm -f "$artifact"
                run_one "$bootstrap_cache" "$wl_abs" "$tag"
                emit "$workload" "$state" "$n" "" "$log_root/$tag.rusage"
                ;;
            readonly)
                root="$output/caches/readonly-$name-$n"
                rm -rf "$root"
                cp -a "$bootstrap_cache" "$root"
                artifact=$(artifact_for "$root" "$wl_abs")
                rm -f "$artifact"
                RUN_ENV="GOLDFISH_CACHE_READONLY=1" run_one "$root" "$wl_abs" "$tag"
                emit "$workload" "$state" "$n" "" "$log_root/$tag.rusage"
                rm -rf "$root"
                ;;
            *)
                echo "run-suite: unknown state '$state'" >&2; exit 2
                ;;
            esac
            n=$((n + 1))
        done
    done
    # Cache manifest for the warm root, then drop the heavy trees.
    if [ -d "$output/caches/warm-$name" ]; then
        version=$(cache_dir_for "$output/caches/warm-$name")
        (cd "$version" && find . -type f -printf '%s\t%p\n' | sort -k2) \
            > "$output/caches/manifest-warm-$name.tsv"
        rm -rf "$output/caches/warm-$name"
    fi
done
rm -rf "$bootstrap_cache"
echo "run-suite: results in $output/summary.tsv"
