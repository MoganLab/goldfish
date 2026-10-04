#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"

case "${1:-}" in
    '') manifest=tests/r7rs/audit.manifest ;;
    --gaps) manifest=tests/r7rs/gaps.manifest ;;
    *) echo "usage: sh tools/test-r7rs-audit.sh [--gaps]" >&2; exit 2 ;;
esac
sh tools/check-r7rs-matrix.sh
xmake build gf-native
xmake build native-evaluator-test
./bin/native-evaluator-test

# A previously complete cache can hide source changes during bootstrap.
# Always start this audit in isolation, then replay the same corpus warm.
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
    echo "R7RS audit: warm bootstrap"
    # shellcheck disable=SC2086
    ./bin/gf -m liii -I tests/r7rs/fixtures --each-file $files
fi
