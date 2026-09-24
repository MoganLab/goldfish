#!/bin/sh
# tools/warm-bootstrap-cache.sh -- make the native bootstrap cache complete.
#
# The native runtime only READS a prebuilt cache: NativeBootstrap requires
# every entry of native_bootstrap_artifacts (src/runtime/bootstrap.cpp) in
# one version directory before find_cache_version will select it.  Any run
# of the host driver warms the startup chain, but a library nothing imports
# at startup (scheme/case-lambda) is left out -- and a single missing file
# makes the whole directory look absent, so the native tests abort with
# "native bootstrap cache not found".
#
# R4 deletes the host driver: once it is gone this warm-up has to become a
# cache the build/CI installs instead (GOLDFISH_CACHE_DIR).

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
        cache_root=$XDG_CACHE_HOME/goldfish/ccache
    else
        cache_root=${HOME:-/tmp}/.cache/goldfish/ccache
    fi
fi

complete_dir=

find_complete() {
    complete_dir=
    for candidate in "$cache_root" "$cache_root"/*/; do
        [ -d "$candidate" ] || continue
        complete=1
        for artifact in $required; do
            if [ ! -f "$candidate$artifact" ]; then
                complete=0
                break
            fi
        done
        if [ "$complete" = 1 ]; then
            complete_dir=$candidate
            return 0
        fi
    done
    return 1
}

report_missing() {
    for artifact in $required; do
        found=0
        for candidate in "$cache_root" "$cache_root"/*/; do
            if [ -f "$candidate$artifact" ]; then
                found=1
                break
            fi
        done
        if [ "$found" = 0 ]; then
            echo "warm-bootstrap-cache: missing $artifact" >&2
        fi
    done
}

if ! find_complete; then
    if [ ! -x bin/gf ]; then
        echo "warm-bootstrap-cache: bin/gf not built (needed to warm $cache_root)" >&2
        exit 1
    fi
    echo "warm-bootstrap-cache: warming $cache_root"
    # Host startup compiles the bootstrap chain, scheme/base and liii/reader;
    # case-lambda is only compiled when something asks for it.
    bin/gf -e '(import (scheme case-lambda))' >/dev/null
    if ! find_complete; then
        report_missing
        echo "warm-bootstrap-cache: cache still incomplete under $cache_root" >&2
        exit 1
    fi
fi

echo "warm-bootstrap-cache: cache complete at $complete_dir"
