#!/bin/sh
# tools/freeze-substrate.sh -- print the canonical C++/Scheme substrate inventory.
#
# The three frozen tables (LAYER.md L0/T2, host-ABI handoff for a future engine):
#   A: gf:: forwarding shims  (src/gf_forwards.def) -- the object-manipulation surface
#   B: GF_GLUE platform prims (src/*.cpp)           -- declarative scalar/vector glue
#   C: all Scheme-visible C++ primitives            -- every g_*/g-* literal + iota/read/version
#   D: core-language heads      (goldfish/core/ir.scm) -- the eval contract for a new engine
#
# Output is checked into tools/substrate-baseline.txt; tools/lint-layer.sh
# regenerates and diffs.  Any change must be deliberate: update the baseline
# in the same commit and explain why in the commit message.
set -e
cd "$(dirname "$0")/.." # repo root

echo "== A gf-forwards =="
grep -E '^GF_FWD' src/gf_forwards.def \
  | sed -E 's/^GF_FWD_VOID\( *([A-Za-z0-9_]+).*/\1/; s/^GF_FWD\( *[^,]+, *([A-Za-z0-9_]+).*/\1/' \
  | sort
echo "count=$(grep -c -E '^GF_FWD' src/gf_forwards.def)"

echo "== B GF_GLUE =="
grep -hE 'GF_GLUE(_0)? *\(' src/*.cpp \
  | sed -E 's/^[^(]*\( *//; s/[ ,].*//' | tr -d '"' \
  | sort
echo "count=$(grep -hE 'GF_GLUE(_0)? *\(' src/*.cpp | wc -l | tr -d ' ')"

echo "== C scheme-primitives =="
grep -rhoE '"g[-_][a-z0-9_?!*<>=/-]+"' src/*.cpp src/*.hpp | tr -d '"' | sort -u
# Non-g_ names registered from C++ (each verified present at its site below).
for spec in "iota:goldfish.hpp" "read:liii_reader.cpp" "version:goldfish.hpp"; do
  name=${spec%%:*}; file=${spec##*:}
  grep -q "\"$name\"" "src/$file" || { echo "missing non-g_ primitive: $name" >&2; exit 1; }
  echo "$name"
done | sort -u
echo "count=$({ grep -rhoE '"g[-_][a-z0-9_?!*<>=/-]+"' src/*.cpp src/*.hpp | tr -d '"' | sort -u; echo iota; echo read; echo version; } | wc -l | tr -d ' ')"

echo "== D core-language =="
core_rows () {
  sed -n '/(define core-language/,/core-form?/p' goldfish/core/ir.scm \
    | grep -v -e core-language -e core-form? \
    | grep -oE "^[ ]+'?\(+[a-z!*+/-]+" | sed -E "s/^[^a-z!*+/-]*//" | sort -u
}
core_rows
echo "count=$(core_rows | wc -l | tr -d ' ')"
