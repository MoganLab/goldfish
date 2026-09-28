#!/bin/sh
set -e
fail=0

if grep -R --include="*.scm" "define (gfo-" goldfish --include="*.scm" 2>/dev/null \
    | grep -v "goldfish/core/gfo.scm" | grep -q .; then
    echo "layer violation: gfo defined outside core/gfo.scm"
    fail=1
fi

if grep -Rn '#include.*s7[.]h' src --include="*.cpp" --include="*.h" --include="*.hpp" 2>/dev/null | grep -q .; then
    echo "runtime dependency violation: native source includes s7.h"
    fail=1
fi
if grep -RnE 's7_(pointer|scheme|int)' src --include="*.cpp" --include="*.h" --include="*.hpp" 2>/dev/null | grep -v '^[^:]*:[0-9]*:[[:space:]]*//' | grep -q .; then
    echo "runtime dependency violation: native source exposes S7 types"
    fail=1
fi

if grep -R "goldfish/compiler" goldfish/expander/kernel --include="*.scm" 2>/dev/null | grep -q .; then
    echo "layer violation: L3 kernel depends on compiler"
    fail=1
fi
if grep -n "goldfish/compiler" goldfish/expander/kernel-combined.scm 2>/dev/null | grep -q .; then
    echo "layer violation: L3 artifact depends on compiler"
    fail=1
fi
if grep -R "goldfish/compiler" goldfish/expander/lib goldfish/expander/tree-il.scm \
    goldfish/liii/reader.scm goldfish/core --include="*.scm" 2>/dev/null \
    | grep -v "goldfish/core/ir" | grep -q .; then
    echo "layer violation: L4 must not import compiler (core/ir is shared)"
    fail=1
fi
if grep -RE "goldfish/core|goldfish/expander/lib" goldfish/compiler/ --include="*.scm" 2>/dev/null \
    | grep -v "goldfish/core/ir" | grep -q .; then
    echo "layer violation: L5 must not import core/lib (except core/ir)"
    fail=1
fi

if ! sh tools/freeze-substrate.sh 2>/dev/null | diff -u tools/substrate-baseline.txt - > /tmp/lint-substrate.diff 2>/dev/null; then
    echo "layer violation: core-language contract changed; review /tmp/lint-substrate.diff and update tools/substrate-baseline.txt deliberately"
    fail=1
fi
if [ "$fail" -ne 0 ]; then exit "$fail"; fi
