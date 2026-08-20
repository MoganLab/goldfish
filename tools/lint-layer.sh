#!/bin/sh
set -e
fail=0
if grep -r "s7_" goldfish/compiler/*.scm 2>/dev/null | grep -v "^Binary" | grep -q .; then echo "layer violation: compiler imports s7"; fail=1; fi
if grep -R --include="*.scm" "s7_" goldfish/compiler/ 2>/dev/null | grep -q .; then echo "layer violation: compiler imports s7"; fail=1; fi
if grep -q '#include.*s7\.h' src/gf.h 2>/dev/null; then echo "layer violation: gf.h includes s7.h"; fail=1; fi
if grep -q "s7_pointer\|s7_int\|s7_scheme" src/goldfish_vm.cpp 2>/dev/null | grep -v "^.*//" | grep -q .; then echo "layer violation: vm spells s7 types"; fail=1; fi
if grep -R --include="*.scm" "define (gfo-" goldfish --include="*.scm" 2>/dev/null | grep -v "goldfish/cache/gfo.scm" | grep -q .; then echo "layer violation: gfo defined outside cache/gfo.scm"; fail=1; fi
if grep -Rn '#include.*s7\.h' src --include="*.cpp" --include="*.h" --include="*.hpp" 2>/dev/null | grep -v "src/gf.cpp" | grep -v "src/gf_glue.hpp" | grep -v "src/s7" | grep -q .; then echo "layer violation: non-L0 includes s7.h"; fail=1; fi
if grep -Rn "s7_pointer\|s7_int\|s7_scheme\|s7_double\|s7_function" src --include="*.cpp" --include="*.h" --include="*.hpp" 2>/dev/null | grep -v "src/s7" | grep -v "src/gf.h" | grep -v "src/gf.cpp" | grep -v "src/gf_glue.hpp" | grep -v "^.*//" | grep -q .; then echo "layer violation: non-L0 spells s7 types"; fail=1; fi
if grep "expander" src/liii_reader.cpp 2>/dev/null | grep -v "//" | grep -q .; then echo "layer violation: L1 tiny reader depends on expander"; fail=1; fi
if ! head -n 160 goldfish/liii/boot.scm 2>/dev/null | grep -q 'load-source-file "cache/gfo.scm"'; then echo "layer violation: L1 boot must load cache/gfo.scm first"; fail=1; fi
if grep -R "goldfish/cache\|goldfish/compiler" goldfish/expander/kernel --include="*.scm" 2>/dev/null | grep -q .; then echo "layer violation: L2 kernel depends on cache/compiler"; fail=1; fi
if grep -q "goldfish/cache\|goldfish/compiler" goldfish/expander/kernel-combined.scm 2>/dev/null | grep -q .; then echo "layer violation: L2 artifact depends on cache/compiler"; fail=1; fi
exit $fail
