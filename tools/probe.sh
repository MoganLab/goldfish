#!/bin/sh
# One-line behavior probe against the native runtime, memory-capped,
# with the expression wrapped in a catch so unbound/raise results print
# instead of aborting the probe.
#
#   sh tools/probe.sh "(eval 'foo (environment '(goldfish)))"
#   sh tools/probe.sh -m r7rs "(car 1)"
#   GOLDFISH_PROBE_MEM=4194304 sh tools/probe.sh '...'    # KB, default 6GB
set -eu
project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
mode=liii
if [ "${1:-}" = "-m" ]; then
    mode=$2
    shift 2
fi
expr=${1:?usage: probe.sh [-m mode] '<scheme expr>'}
ulimit -v "${GOLDFISH_PROBE_MEM:-6291456}"
timeout "${GOLDFISH_PROBE_TIMEOUT:-120}" "$project_dir/bin/gf" \
    -m "$mode" -e "
(import (goldfish))
(write (catch #t (lambda () $expr)
              (lambda (tag . info) (list 'caught tag info))))
(newline)"
