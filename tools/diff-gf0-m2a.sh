#!/bin/sh
# tools/diff-gf0-m2a.sh -- differential gate on REAL lowered programs.
#
# For each tests/gf0/m2a-*.scm: compile-file-cached the program, preload its
# deps (mirroring the loader), then evaluate the identical `opt` sexp once
# with s7 in the-expander-library and once with gf0 (seeded from the same
# inlet). stdout must match byte-for-byte (load path echoes nothing; only
# the program's own displays print). Cold-cache effects are warmed away by
# a discard run first.
#
# M3 note: explicit test files may be passed after DIR. Three known files
# cannot run through this whole-file path on EITHER engine (identical
# failure, frontend -- not evaluation -- issues, out of scope):
#   abs-test.scm  -- complex literal 1.0+2.0i misread under direct
#                    compile-file-cached (normal `gf test` passes).
#   case-test.scm -- constant-fold chokes whole-file ("not enough
#                    arguments" in ((lambda vs vs) (producer))).
#   srfi-158-test.scm -- make-coroutine-generator checks fail structurally:
#                    s7-native call/cc captured inside the library cannot
#                    survive gf0's s7call C++ frame lifetime (use-after-
#                    return on later invoke; garbage values, luckily no
#                    crash). All non-coroutine checks pass. See
#                    CORE-SEMANTICS.md continuation boundary note.
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

for prog in $files; do
  [ -e "$prog" ] || continue
  s7r=$(mktemp /tmp/kilo/m2a-s7-XXXX.scm)
  gf0r=$(mktemp /tmp/kilo/m2a-gf0-XXXX.scm)
  {
    echo '(import (goldfish) (scheme base) (scheme eval))'
    echo "(define cc-mrefs (eval 'collect-cache-module-refs the-expander-library))"
    echo "(define load-lib! (eval 'load-library! the-expander-library))"
    echo "(define rt-reg? (eval 'runtime-registered? the-expander-library))"
    echo "(define opt (compile-file-cached \"$prog\"))"
    echo '(for-each (lambda (lib) (if (not (rt-reg? lib)) (load-lib! lib))) (cc-mrefs opt))'
    echo '(eval opt the-expander-library)'
  } > "$s7r"
  {
    echo '(import (goldfish) (scheme base) (scheme eval))'
    echo "(define cc-mrefs (eval 'collect-cache-module-refs the-expander-library))"
    echo "(define load-lib! (eval 'load-library! the-expander-library))"
    echo "(define rt-reg? (eval 'runtime-registered? the-expander-library))"
    echo "(define gf0-import (eval 'g_gf0-import-inlet (rootlet)))"
    echo "(define gf0-evalv (eval 'g_gf0-eval-values (rootlet)))"
    echo "(define opt (compile-file-cached \"$prog\"))"
    echo '(for-each (lambda (lib) (if (not (rt-reg? lib)) (load-lib! lib))) (cc-mrefs opt))'
    echo '(gf0-import the-expander-library)'
    echo '(gf0-evalv opt)'
  } > "$gf0r"
  ./bin/gf -I "$dir" "$s7r" > /dev/null 2>&1 || true   # warm caches, discard
  s7out=$(./bin/gf -I "$dir" "$s7r" 2>&1 || true)
  gf0out=$(./bin/gf -I "$dir" "$gf0r" 2>&1 || true)
  rm -f "$s7r" "$gf0r"
  if [ "$s7out" != "$gf0out" ]; then
    echo "DIFF(m2a) $prog"; fail=1
  else
    echo "ok $prog"
  fi
done

exit $fail
