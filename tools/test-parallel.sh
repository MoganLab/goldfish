#!/bin/sh
# test-parallel.sh -- run gf test files across parallel workers.
#
# The built-in `gf test` runner is serial; the full suite (1500+ files)
# is a dedicated time slot.  This wrapper shards the same files across N
# `gf test FILE` workers and reports the failures.  Workers share the
# gfo cache: one small file is run first (serially) to warm the shared
# library caches, which keeps the fan-out from racing on cold-cache
# compiles of the same library.
#
# Usage: tools/test-parallel.sh [-j N] [PATH|PATTERN]
#   -j N      worker count (default: nproc, or GF_TEST_JOBS)
#   PATH      directory under tests/ (default: tests), or a name filter

set -eu
cd "$(dirname "$0")/.."

if [ ! -x bin/gf ]; then
    echo "test-parallel: bin/gf not found -- run 'xmake b' first" >&2
    exit 1
fi

jobs="${GF_TEST_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"
filter=""
while [ $# -gt 0 ]; do
    case "$1" in
        -j) jobs="$2"; shift 2 ;;
        *) filter="$1"; shift ;;
    esac
done

root="tests"
if [ -n "$filter" ] && [ -d "$root/$filter" ]; then
    root="$root/$filter"
    pattern=""
elif [ -n "$filter" ]; then
    pattern="$filter"
else
    pattern=""
fi

if [ -n "${pattern:-}" ]; then
    files=$(find "$root" -name '*-test.scm' | grep "$pattern" | sort)
else
    files=$(find "$root" -name '*-test.scm' | sort)
fi

[ -n "$files" ] || { echo "test-parallel: no *-test.scm files under $root" >&2; exit 1; }

total=$(printf '%s\n' "$files" | wc -l | tr -d ' ')
echo "test-parallel: $total files, $jobs workers"

workdir=$(mktemp -d /tmp/kilo/test-parallel.XXXXXX)
trap 'rm -rf "$workdir"' EXIT
results="$workdir/results"
: > "$results"

# Warm the shared library caches with one file before fanning out, so the
# workers do not race each other cold-compiling the same imports.
first=$(printf '%s\n' "$files" | head -1)
if ./bin/gf test "$first" > "$workdir/$(printf %s "$first" | tr / _).out" 2>&1; then
    echo "PASS $first"
    echo "PASS $first" >> "$results"
else
    echo "FAIL $first"
    echo "FAIL $first" >> "$results"
fi

rest=$(printf '%s\n' "$files" | tail -n +2)
if [ -n "$rest" ]; then
    printf '%s\n' "$rest" | WORKDIR="$workdir" xargs -P "$jobs" -I FILE sh -c '
        out="$WORKDIR/$(printf %s "$1" | tr / _).out"
        if ./bin/gf test "$1" > "$out" 2>&1; then
            echo "PASS $1"
        else
            echo "FAIL $1"
        fi' sh FILE | tee -a "$results"
fi

pass=$(grep -c '^PASS ' "$results" || true)
fail=$(grep -c '^FAIL ' "$results" || true)
echo ""
echo "=== Summary ==="
echo "  Files:  $total"
echo "  Passed: $pass"
echo "  Failed: $fail"

if [ "$fail" -gt 0 ]; then
    echo ""
    echo "=== Failed files ==="
    grep '^FAIL ' "$results" | cut -d' ' -f2- | while read -r f; do
        echo "  $f"
    done
    echo ""
    echo "=== Failure details ==="
    grep '^FAIL ' "$results" | cut -d' ' -f2- | while read -r f; do
        out="$workdir/$(printf %s "$f" | tr / _).out"
        echo "--- $f"
        grep -a -B1 -A4 "Failed Test Files:" "$out" | head -8 || tail -8 "$out"
    done
    exit 1
fi
