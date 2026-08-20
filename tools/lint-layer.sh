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
# L2 single source: load-kernel.scm manifest must match kernel.scm includes
k_includes=$(grep -o '"expander/kernel/[^"]*"' goldfish/expander/kernel.scm 2>/dev/null | tr -d '"')
l_includes=$(grep -o '"expander/kernel/[^"]*"' goldfish/expander/kernel/load-kernel.scm 2>/dev/null | tr -d '"')
if [ "$k_includes" != "$l_includes" ]; then echo "layer violation: load-kernel.scm out of sync with kernel.scm"; echo "kernel.scm: $k_includes"; echo "load-kernel.scm: $l_includes"; fail=1; fi
if grep -R "goldfish/compiler" goldfish/expander/lib --include="*.scm" goldfish/liii/reader.scm goldfish/cache --include="*.scm" 2>/dev/null | grep -v "^.*:.*;;;" | grep -q .; then echo "layer violation: L3 must not import compiler"; fail=1; fi
if grep -R --include="*.scm" "s7_" goldfish/compiler/ goldfish/expander/syntax-ir.scm 2>/dev/null | grep -q .; then echo "layer violation: L4 must be pure no s7"; fail=1; fi
if grep -R --include="*.scm" "goldfish/cache\|goldfish/expander/lib" goldfish/compiler/ goldfish/expander/syntax-ir.scm 2>/dev/null | grep -q .; then echo "layer violation: L4 must not import cache/lib"; fail=1; fi
if grep -R --include="*.h" --include="*.hpp" --include="*.cpp" "goldfish/" src/goldfish_vm.cpp 2>/dev/null | grep -v "^.*//" | grep -q .; then echo "layer violation: L5 vm must not include Scheme files"; fail=1; fi
if grep -E '#include.*expander|#include.*compiler|\(import.*goldfish/compiler' src/goldfish.hpp 2>/dev/null | grep -q .; then echo "layer violation: L6 loader must not include expander/compiler"; fail=1; fi
# L0 glue minimal: keep g_xxx primitives only, business logic in Scheme (liii/*)
# current baseline 64, keep from growing; move new business to Scheme
if [ "$(grep -c "glue_" src/goldfish.hpp 2>/dev/null)" -gt 64 ]; then echo "layer violation: L0 glue too many, move business logic to Scheme"; fail=1; fi
exit $fail
