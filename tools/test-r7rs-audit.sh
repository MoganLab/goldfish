#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"

case "${1:-}" in
    '') manifest=tests/r7rs/audit.manifest ;;
    --gaps) manifest=tests/r7rs/gaps.manifest ;;
    *) echo "usage: sh tools/test-r7rs-audit.sh [--gaps]" >&2; exit 2 ;;
esac
sh tools/check-r7rs-matrix.sh
if [ "${1:-}" = --gaps ] &&
   [ -z "$(sed '/^[[:space:]]*#/d; /^[[:space:]]*$/d' "$manifest")" ]; then
    echo "R7RS audit: the original compatibility gap manifest is empty; see the separate semantic audit"
    exit 0
fi
xmake build gf-native
xmake build native-evaluator-test
./bin/native-evaluator-test

# Exercise source bootstrap in isolation, then replay the same corpus warm.
audit_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-r7rs-audit.XXXXXX")
trap 'rm -rf "$audit_root"' EXIT HUP INT TERM
export GOLDFISH_CACHE_DIR="$audit_root/ccache"
mkdir -p "$GOLDFISH_CACHE_DIR"
files=$(sed '/^[[:space:]]*#/d; /^[[:space:]]*$/d' "$manifest")

if [ "${1:-}" = --gaps ]; then
    sh tools/warm-bootstrap-cache.sh
    # These probes assert standard behavior and deliberately report failures.
    # shellcheck disable=SC2086
    ./bin/gf -m liii -I tests/r7rs/fixtures --each-file $files
else
    echo "R7RS audit: cold bootstrap"
    # shellcheck disable=SC2086
    ./bin/gf -m liii -I tests/r7rs/fixtures --each-file $files
    sh tools/warm-bootstrap-cache.sh
    sh tools/test-library-declaration-cache.sh
    echo "R7RS audit: warm bootstrap"
    # shellcheck disable=SC2086
    ./bin/gf -m liii -I tests/r7rs/fixtures --each-file $files
fi

if [ "${1:-}" != --gaps ]; then
    echo "R7RS audit: native warm worker"
    GOLDFISH_CHECK_NO_EXIT=1 ./bin/gf -m liii tools/test/liii/worker.scm -- \
        tests/r7rs/library-declarations-test.scm tests/r7rs/conditional-library-test.scm
fi
