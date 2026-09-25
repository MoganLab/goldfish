#!/bin/sh
# One-shot CPU profile of a command: perf record + hottest frames.
#
#   tools/prof.sh <command...>
#   tools/prof.sh -n 199 ./bin/gf-native -m liii some-file.scm
set -eu
freq=99
if [ "${1:-}" = "-n" ]; then
    freq=$2
    shift 2
fi
out=${GOLDFISH_PROF_OUT:-/tmp/gf-prof.data}
if ! perf record -F "$freq" -g -o "$out" "$@" >/dev/null 2>&1; then
    echo "prof: perf record failed (perf unavailable? command error)" >&2
    exit 1
fi
echo "=== hottest (self %):"
perf report -i "$out" --stdio --no-children 2>/dev/null \
    | grep -E '^\s+[0-9]' | head -12
echo "=== top call paths (children %):"
perf report -i "$out" --stdio --children --percent-limit 10 2>/dev/null \
    | grep -E '^\s+[0-9]' | head -8
