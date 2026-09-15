#!/bin/sh
# tools/bench-gf0-s7.sh -- dual-engine time baseline (s7 vs gf0, same opt).
#
# Reuses the diff-gate runner templates: compile once via
# compile-file-cached, evaluate the identical `opt' on each side.
# Usage: sh tools/bench-gf0-s7.sh tests/gf0 FILE...   (3 iters + 1 warmup
# each side, reports median real/user/sys).
#
# Baseline (2026-09-15, <machine>, commit <sha>, clean ccache):
#   (fill after first run; 6 numbers, no analysis here)
#
# Rules: run only when no other gf/ccache user is active; numbers are
# informational (regression tripwire at ~10%+, not a benchmark suite).

dir="$1"; shift
run=/tmp/kilo/bench-run.scm
mk_s7 () {
  {
    echo '(import (goldfish) (scheme base) (scheme eval))'
    echo "(define cc-mrefs (eval 'collect-cache-module-refs the-expander-library))"
    echo "(define load-lib! (eval 'load-library! the-expander-library))"
    echo "(define rt-reg? (eval 'runtime-registered? the-expander-library))"
    echo "(define opt (compile-file-cached \"$prog\"))"
    echo '(for-each (lambda (lib) (if (not (rt-reg? lib)) (load-lib! lib))) (cc-mrefs opt))'
    echo '(eval opt the-expander-library)'
  } > "$run"
}
mk_gf0 () {
  {
    echo '(import (goldfish) (scheme base) (scheme eval))'
    echo "(define cc-mrefs (eval 'collect-cache-module-refs the-expander-library))"
    echo "(define load-lib! (eval 'load-library! the-expander-library))"
    echo "(define rt-reg? (eval 'runtime-registered? the-expander-library))"
    echo "(define gf0-import (eval 'g_gf0-import-inlet (rootlet)))"
    echo "(define gf0-evalv (eval 'g_gf0-eval-values (rootlet)))"
    echo "(define opt (compile-file-cached \"$prog\"))"
    echo '(gf0-import the-expander-library #f)'
    echo '(for-each (lambda (lib) (if (not (rt-reg? lib)) (load-lib! lib))) (cc-mrefs opt))'
    echo '(gf0-import the-expander-library)'
    echo '(gf0-evalv opt)'
  } > "$run"
}
median3 () { printf '%s\n' "$1" "$2" "$3" | sort -n | sed -n '2p'; }
for prog in "$@"; do
  base=$(basename "$prog")
  for side in s7 gf0; do
    if [ "$side" = s7 ]; then mk_s7; else mk_gf0; fi
    ./bin/gf -I "$dir" "$run" > /dev/null 2>&1 || true  # warmup, discard
    t1=$( (time -p ./bin/gf -I "$dir" "$run" > /dev/null 2>&1) 2>&1 | awk '/^real/ {print $2}')
    t2=$( (time -p ./bin/gf -I "$dir" "$run" > /dev/null 2>&1) 2>&1 | awk '/^real/ {print $2}')
    t3=$( (time -p ./bin/gf -I "$dir" "$run" > /dev/null 2>&1) 2>&1 | awk '/^real/ {print $2}')
    echo "$base $side median-real=$(median3 "$t1" "$t2" "$t3")s ($t1 $t2 $t3)"
  done
done
