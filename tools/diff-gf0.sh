#!/bin/sh
# tools/diff-gf0.sh -- differential gate: pure-core programs, s7 vs gf0.
#
# agree-*.scm:   single (begin ...) datum ending in (display ...) (newline).
#                gf0 prints one value line per datum plus a final count line;
#                s7 prints one last-value echo. Drop those (2 vs 1 trailing
#                lines), compare the rest byte-for-byte, then require
#                gf0_valueline == "($s7_valueline)".
#                The s7 side runs default `eval` (expander online): agree
#                programs are pure core, expansion is identity, evaluation
#                stays s7. (`-m s7` lacks let-values and can't serve here.)
# diverge-*.scm: compared against full per-engine expectation files
#                (.s7.expected / .gf0.expected); the two R7RS-strict points.
# module-ref is out of scope here (needs the boot registry; covered by
# direct eval-gf0 probes, see M1b notes).
set -eu
cd "$(dirname "$0")/.."
dir=${1:-tests/gf0}
fail=0

run_s7 () { ./bin/gf eval "$1" 2>&1; }
run_gf0 () { ./bin/gf eval-gf0 "$1" 2>&1; }

for f in "$dir"/agree-*.scm; do
  [ -e "$f" ] || continue
  code=$(cat "$f")
  s7out=$(run_s7 "$code")
  gf0out=$(run_gf0 "$code")
  # shellcheck disable=SC3043
  s7body=$(printf '%s\n' "$s7out" | sed '$d')
  gf0body=$(printf '%s\n' "$gf0out" | sed '$d' | sed '$d')
  s7last=$(printf '%s\n' "$s7out" | tail -n 1)
  gf0last=$(printf '%s\n' "$gf0out" | tail -n 2 | head -n 1)
  if [ "$s7body" != "$gf0body" ]; then
    echo "DIFF(body) $f"; fail=1
  elif [ "$gf0last" != "($s7last)" ]; then
    echo "DIFF(tail) $f: s7='$s7last' gf0='$gf0last'"; fail=1
  else
    echo "ok $f"
  fi
done

for f in "$dir"/diverge-*.scm; do
  [ -e "$f" ] || continue
  code=$(cat "$f")
  base=${f%.scm}
  if [ "$(run_s7 "$code")" != "$(cat "$base.s7.expected")" ]; then
    echo "DIFF(s7) $f"; fail=1
  elif [ "$(run_gf0 "$code")" != "$(cat "$base.gf0.expected")" ]; then
    echo "DIFF(gf0) $f"; fail=1
  else
    echo "ok $f"
  fi
done

exit $fail
