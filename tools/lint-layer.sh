#!/bin/sh
set -e
fail=0
if grep -r "s7_" goldfish/compiler/*.scm 2>/dev/null | grep -v "^Binary" | grep -q .; then echo "layer violation: compiler imports s7"; fail=1; fi
if grep -R --include="*.scm" "s7_" goldfish/compiler/ 2>/dev/null | grep -q .; then echo "layer violation: compiler imports s7"; fail=1; fi
if grep -q '#include.*s7\.h' src/gf.h 2>/dev/null; then echo "layer violation: gf.h includes s7.h"; fail=1; fi
if grep -q "s7_pointer\|s7_int\|s7_scheme" src/goldfish_vm.cpp 2>/dev/null | grep -v "^.*//" | grep -q .; then echo "layer violation: vm spells s7 types"; fail=1; fi
if grep -R --include="*.scm" "define (gfo-" goldfish --include="*.scm" 2>/dev/null | grep -v "goldfish/cache/gfo.scm" | grep -q .; then echo "layer violation: gfo defined outside cache/gfo.scm"; fail=1; fi
exit $fail
