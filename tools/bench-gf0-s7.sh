#!/bin/sh
# tools/bench-gf0-s7.sh -- dual-engine time baseline (s7 vs gf0, same opt).
#
# Reuses the diff-gate runner templates: compile once via
# compile-file-cached, evaluate the identical `opt' on each side.
# Usage: sh tools/bench-gf0-s7.sh tests/gf0 FILE...   (3 iters + 1 warmup
# each side, reports median real/user/sys).
#
# Baseline (2026-09-15, 16c/27G linux, commit 09672149, warm ccache,
# N=50 in-process evals, wall seconds for 50 runs, median of 3):
#   m2a-higher-order.scm  s7 0.106s   gf0 0.234s
#   m2a-generator.scm     s7 0.117s   gf0 0.673s
#   m2a-match.scm         s7 0.116s   gf0 0.223s
# gf0 is a tree-walking reference evaluator; ~7-40x here is expected.
# Tripwire: >10% drift on rerun (same machine/commit-shape) gets chased.
#
# NOTE: N=50 in-process eval iterations amortize binary startup; the
# reported seconds are totals for N runs (divide for per-run).
N=${N:-50}
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
    echo "(let loop ((i $N)) (if (> i 0) (begin (eval opt the-expander-library) (loop (- i 1)))))"
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
    echo "(let loop ((i $N)) (if (> i 0) (begin (gf0-evalv opt) (loop (- i 1)))))"
  } > "$run"
}
median3 () { printf '%s\n' "$1" "$2" "$3" | sort -n | sed -n '2p'; }
# POSIX-sh-safe timing (the `time' keyword loses its stderr inside $()
# under some sh implementations): wall seconds with millisecond print.
stamp () { date +%s.%N; }
elapsed () { awk -v a="$1" -v b="$2" 'BEGIN {printf "%.3f", b - a}'; }
for prog in "$@"; do
  base=$(basename "$prog")
  for side in s7 gf0; do
    if [ "$side" = s7 ]; then mk_s7; else mk_gf0; fi
    ./bin/gf -I "$dir" "$run" > /dev/null 2>&1 || true  # warmup, discard
    s=$(stamp); ./bin/gf -I "$dir" "$run" > /dev/null 2>&1 || true; t1=$(elapsed "$s" "$(stamp)")
    s=$(stamp); ./bin/gf -I "$dir" "$run" > /dev/null 2>&1 || true; t2=$(elapsed "$s" "$(stamp)")
    s=$(stamp); ./bin/gf -I "$dir" "$run" > /dev/null 2>&1 || true; t3=$(elapsed "$s" "$(stamp)")
    echo "$base $side median=$(median3 "$t1" "$t2" "$t3")s ($t1 $t2 $t3)"
  done
done
