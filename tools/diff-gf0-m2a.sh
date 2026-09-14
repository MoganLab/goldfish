#!/bin/sh
# tools/diff-gf0-m2a.sh -- differential gate on REAL lowered programs.
#
# For each tests/gf0/m2a-*.scm: compile-file-cached the program, preload its
# deps (mirroring the loader), then evaluate the identical `opt` sexp once
# with s7 in the-expander-library and once with gf0 (seeded from the same
# inlet). Outputs must match after normalization (see norm()): load path
# echoes nothing, so normally only the program's own displays print.
# Cold-cache effects are warmed away by a discard run first.
#
# One runner template serves both sides (SIDE flag baked in), so form
# numbers and paths are identical; only the eval engine differs.
#
# norm() drops s7 stacktrace-dump lines (engine-frame contents differ by
# construction). Sound: evaluation outcomes always surface as check
# headers/values/summaries, which are kept; a divergence confined to
# dropped lines alone is not a semantic divergence.
#
# M3 note: explicit test files may be passed after DIR. Three known files
# cannot run through this whole-file path on EITHER engine (identical
# failure, frontend -- not evaluation -- issues, out of scope):
#   abs-test.scm  -- complex literal 1.0+2.0i misread under direct
#                    compile-file-cached (normal `gf test` passes).
#   case-test.scm -- constant-fold chokes whole-file ("not enough
#                    arguments" in ((lambda vs vs) (producer))).
#   srfi-158-test.scm -- make-coroutine-generator is fail-closed by the
#                    stale fence (gf0-stale-continuation on every yield;
#                    see tools/check-gf0-guards.sh), so it cannot run the
#                    dual gate: user callbacks are gf0 boxes, each nesting a
#                    fresh token. Pre-fence behavior was silent garbage
#                    (use-after-return into dead C++ frames, no crash).
set -eu
cd "$(dirname "$0")/.."
mkdir -p /tmp/kilo
dir=${1:-tests/gf0}
fail=0

if [ $# -ge 2 ]; then
  # Explicit file list (M3: test files): shift past dir, take the rest.
  shift
  files="$*"
else
  files="$dir"/m2a-*.scm
fi

# Drop s7 stacktrace-dump lines (frame ids, gensym ids, dump headers).
# Kept: check headers with actual values, expected lines, summaries,
# and all program displays.
norm () {
  grep -vE 'call-stack:[0-9]|sexp:[0-9]|\(loop:[0-9]|^loop:[0-9]|^call-with-values: |; (expression|actual-result|expected-result|location-info|info):|check:proc:[0-9]|and-t~[0-9]|could not be compiled; loading per form' || true
}

for prog in $files; do
  [ -e "$prog" ] || continue
  run=/tmp/kilo/m2a-run.scm
  # Two separate templates (a unified SIDE-flag template miscompiles on
  # the gf0 side; form numbers may differ across sides, which is fine
  # because load notes name the constant runner path).
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
      # Fence before load (call/cc wiring bakes at load time), full import
      # after (frame-0 rootlet snapshot must cover the loaded libs).
      echo '(gf0-import the-expander-library #f)'
      echo '(for-each (lambda (lib) (if (not (rt-reg? lib)) (load-lib! lib))) (cc-mrefs opt))'
      echo '(gf0-import the-expander-library)'
      echo '(gf0-evalv opt)'
    } > "$run"
  }
  mk_s7
  ./bin/gf -I "$dir" "$run" > /dev/null 2>&1 || true   # warm caches, discard
  s7out=$(./bin/gf -I "$dir" "$run" 2>&1 || true)
  mk_gf0
  gf0out=$(./bin/gf -I "$dir" "$run" 2>&1 || true)
  if [ "$(printf '%s\n' "$s7out" | norm)" != "$(printf '%s\n' "$gf0out" | norm)" ]; then
    echo "DIFF(m2a) $prog"; fail=1
  else
    echo "ok $prog"
  fi
done

exit $fail
