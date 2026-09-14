#!/bin/sh
# tools/check-gf0-guards.sh -- stale-continuation fence gate.
#
# Asserts the s7call-boundary fence (g_gf0-import-inlet shadow +
# g_gf0-s7call-token): s7 side runs the coroutine program natively
# (10, 20, eof); gf0 side must fail CLOSED with gf0-stale-continuation on
# every yield -- never silent garbage (the old use-after-return mode).
# Import precedes load (wiring bakes at load time); re-import after.
set -eu
cd "$(dirname "$0")/.."
prog=tests/gf0/guard-stale-cc.scm
run=/tmp/kilo/guard-run.scm
mk () {
  side=$1
  {
    echo '(import (goldfish) (scheme base) (scheme eval))'
    echo "(define cc-mrefs (eval 'collect-cache-module-refs the-expander-library))"
    echo "(define load-lib! (eval 'load-library! the-expander-library))"
    echo "(define rt-reg? (eval 'runtime-registered? the-expander-library))"
    if [ "$side" = gf0 ]; then
      echo "(define gf0-import (eval 'g_gf0-import-inlet (rootlet)))"
      echo "(define gf0-evalv (eval 'g_gf0-eval-values (rootlet)))"
    fi
    echo "(define opt (compile-file-cached \"$prog\"))"
    if [ "$side" = gf0 ]; then
      echo '(gf0-import the-expander-library #f)'
    fi
    echo '(for-each (lambda (lib) (if (not (rt-reg? lib)) (load-lib! lib))) (cc-mrefs opt))'
    if [ "$side" = gf0 ]; then
      echo '(gf0-import the-expander-library)'
      echo '(gf0-evalv opt)'
    else
      echo '(eval opt the-expander-library)'
    fi
  } > "$run"
}
mk s7
./bin/gf -I tests/gf0 "$run" > /dev/null 2>&1 || true
s7out=$(./bin/gf -I tests/gf0 "$run" 2>&1 || true)
mk gf0
gf0out=$(./bin/gf -I tests/gf0 "$run" 2>&1 || true)
fail=0
for want in 10 20 '#<eof>'; do
  printf '%s\n' "$s7out" | grep -qx "$want" || { echo "GUARD(s7) missing: $want"; fail=1; }
done
n=$(printf '%s\n' "$gf0out" | grep -c '^gf0-stale-continuation$' || true)
[ "$n" = 3 ] || { echo "GUARD(gf0) stale count=$n, want 3"; fail=1; }
if printf '%s\n' "$gf0out" | grep -qx '10'; then
  echo "GUARD(gf0) leaked a live yield"; fail=1
fi
[ $fail = 0 ] && echo "ok guards/stale-cc"
exit $fail
