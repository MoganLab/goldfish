#!/bin/sh
# measure-vm.sh -- answer "what does the bytecode VM layer buy us?"
#
# Default execution is plain s7 eval of the lowered core forms; setting
# GOLDFISH_VM_DEFS=1 opts into the bytecode VM.  This script measures the
# opt-in against the default on the tracked paths.  Minimum-of-k everywhere:
# this box has noisy background load.
#
# Verdict (2026-08, pre-flip measurements): call-heavy fib(26) ran ~19%
# slower through the VM, library import ~14% slower, warm/cold start even.
# See LAYER.md 演进债.
set -eu
cd "$(dirname "$0")/.."
CCACHE="${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"

run() { # run vm|default <cmd> [args...]
  m=$1; shift
  if [ "$m" = vm ]; then GOLDFISH_VM_DEFS=1 "$@"; else env -u GOLDFISH_VM_DEFS "$@"; fi
}

ms() { m=$1; shift
  t0=$(date +%s%N); run "$m" "$@" >/dev/null 2>&1; t1=$(date +%s%N)
  echo $(( (t1-t0)/1000000 )); }

minof() { k=$1; m=$2; shift 2
  best=9999999; i=0
  while [ $i -lt $k ]; do
    v=$(ms "$m" "$@"); [ "$v" -lt "$best" ] && best=$v
    i=$((i+1)); done
  echo "$best"; }

echo "== 1. warm start, examples/or.scm (ms, min of 5) =="
for m in default vm; do echo "  $m: $(minof 5 $m ./bin/gf examples/or.scm)"; done

echo "== 2. cold cache, full rebuild + or.scm (ms, single shot) =="
for m in default vm; do rm -rf "$CCACHE"; echo "  $m: $(ms $m ./bin/gf examples/or.scm)"; done

echo "== 3. call-heavy micro, fib(26) (ms total incl. ~250ms startup, min of 3) =="
for m in default vm; do echo "  $m: $(minof 3 $m ./bin/gf -m liii benchmarks/micro-call.scm)"; done

echo "== 4. library import, liii/string (ms: cold single / warm min5) =="
for m in default vm; do
  rm -rf "$CCACHE"
  cold=$(ms $m ./bin/gf -m liii -e "(import (liii string))")
  warm=$(minof 5 $m ./bin/gf -m liii -e "(import (liii string))")
  echo "  $m: cold=${cold} warm=${warm}"
done

echo "== baseline --version (ms, min of 5) =="
echo "  $(minof 5 x ./bin/gf --version)"
