#!/bin/sh
# Emit the float-free manifest -- the C2 comparison scope: every test
# file that contains no float/complex/rational literal dependency.
# Regenerate and commit when the reader gains numeric forms (the
# shrinking list is itself the float workstream's progress bar).
set -eu
project=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project"
out=${1:-tests/float-free.manifest}
all=$(mktemp)
float=$(mktemp)
trap 'rm -f "$all" "$float"' EXIT

find tests -name '*-test.scm' | LC_ALL=C sort > "$all"
# Literal shapes: decimals (1.5 / .5), exponent form (1e-3), complex
# suffix (+2i), exactness/radix prefixes (#e1/2 #x10).  Deliberately
# conservative: a comment mentioning a float costs coverage, never a
# false pass.
grep -rlE '(^|[^A-Za-z0-9_-])[0-9]+\.[0-9]|(^|[^A-Za-z0-9_.])[.][0-9]|[0-9]+e[-+]?[0-9]|\+[0-9]*\.?[0-9]*i([^A-Za-z0-9]|$)|#[ie][#\\]?[0-9]|(^|[^A-Za-z0-9_.]) [0-9]+/[0-9]+' \
    $(cat "$all") 2>/dev/null | LC_ALL=C sort -u > "$float" || true

LC_ALL=C comm -23 "$all" "$float" > "$out"
total=$(wc -l < "$all")
excluded=$(wc -l < "$float")
scope=$(wc -l < "$out")
echo "manifest: $out (scope=$scope, float-excluded=$excluded, total=$total)"
