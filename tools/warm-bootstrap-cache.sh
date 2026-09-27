#!/bin/sh
# tools/warm-bootstrap-cache.sh -- make the native bootstrap cache complete.
#
# Warm the native bootstrap cache with the native compiler. The seed workflow
# imports the standard bootstrap libraries plus case-lambda, so one native
# compile produces every artifact NativeBootstrap needs. This must stay
# independent of host gf/s7 so it remains usable after the R4 removal.

set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

# The required set has one source of truth: the C++ array.
required=$(awk '/native_bootstrap_artifacts = \{/ {f=1; next} f && /};/ {exit} f {print}' \
    src/runtime/bootstrap.cpp | sed -n 's/.*"\([^"]*\)".*/\1/p')

if [ -z "$required" ]; then
    echo "warm-bootstrap-cache: cannot read native_bootstrap_artifacts" >&2
    exit 1
fi

cache_root=${GOLDFISH_CACHE_DIR:-}
if [ -z "$cache_root" ]; then
    if [ -n "${XDG_CACHE_HOME:-}" ]; then
        cache_root=$XDG_CACHE_HOME/goldfish/native-ccache
    else
        cache_root=${HOME:-/tmp}/.cache/goldfish/native-ccache
    fi
fi

if [ ! -x bin/gf ]; then
    echo "warm-bootstrap-cache: bin/gf (native runtime) not built" >&2
    exit 1
fi

# Ask this binary for its content-addressed directory.  A different version
# can contain all required filenames while still being stale for this binary.
complete_dir=$(GOLDFISH_CACHE_DIR="$cache_root" GOLDFISH_OPT_LEVEL=2 \
    bin/gf -m liii -e '(gfo-dir)' | sed 's/^"//; s/"$//')

find_complete() {
    [ -n "$complete_dir" ] || return 1
    for artifact in $required; do
        [ -f "$complete_dir/$artifact" ] || return 1
    done
    return 0
}

report_missing() {
    for artifact in $required; do
        found=0
        if [ -f "$complete_dir/$artifact" ]; then found=1; fi
        if [ "$found" = 0 ]; then
            echo "warm-bootstrap-cache: missing $artifact" >&2
        fi
    done
}

if ! find_complete; then
    temp_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-native-cache.XXXXXX")
    trap 'find "$temp_root" -mindepth 1 -delete 2>/dev/null || true; rmdir "$temp_root" 2>/dev/null || true' EXIT HUP INT TERM
    temp_cache=$temp_root/ccache
    mkdir -p "$temp_cache"
    echo "warm-bootstrap-cache: building native cache in isolation"
    GOLDFISH_CACHE_DIR="$temp_cache" GOLDFISH_OPT_LEVEL=2 \
        bin/gf -m liii -e \
        '(compile-file-cached "tests/c3/native-workflow.scm")' >/dev/null
    temp_dir=$(GOLDFISH_CACHE_DIR="$temp_cache" GOLDFISH_OPT_LEVEL=2 \
        bin/gf -m liii -e '(gfo-dir)' | sed 's/^"//; s/"$//')
    complete=1
    for artifact in $required; do
        if [ ! -f "$temp_dir/$artifact" ]; then
            echo "warm-bootstrap-cache: isolated cache missing $artifact" >&2
            complete=0
        fi
    done
    if [ "$complete" != 1 ]; then
        exit 1
    fi
    mkdir -p "$complete_dir"
    cp -a "$temp_dir"/. "$complete_dir"/
    if ! find_complete; then
        report_missing
        echo "warm-bootstrap-cache: cache still incomplete under $complete_dir" >&2
        exit 1
    fi
    find "$temp_root" -mindepth 1 -delete
    rmdir "$temp_root"
    trap - EXIT HUP INT TERM
fi

echo "warm-bootstrap-cache: cache complete at $complete_dir"
