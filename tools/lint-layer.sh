#!/bin/sh
set -e
fail=0
if grep -R --include="*.scm" "s7_" goldfish/compiler/ 2>/dev/null | grep -q .; then echo "layer violation: compiler imports s7"; fail=1; fi
if grep -q '#include.*s7\.h' src/gf.h 2>/dev/null; then echo "layer violation: gf.h includes s7.h"; fail=1; fi
if grep -R --include="*.scm" "define (gfo-" goldfish --include="*.scm" 2>/dev/null | grep -v "goldfish/core/gfo.scm" | grep -q .; then echo "layer violation: gfo defined outside core/gfo.scm"; fail=1; fi
if grep -Rn '#include.*s7\.h' src --include="*.cpp" --include="*.h" --include="*.hpp" 2>/dev/null | grep -v "src/gf.cpp" | grep -v "src/gf_glue.hpp" | grep -v "src/s7" | grep -q .; then echo "layer violation: non-L0 includes s7.h"; fail=1; fi
if grep -Rn "s7_pointer\|s7_int\|s7_scheme\|s7_double\|s7_function" src --include="*.cpp" --include="*.h" --include="*.hpp" 2>/dev/null | grep -v "src/s7" | grep -v "src/gf.h" | grep -v "src/gf.cpp" | grep -v "src/gf_glue.hpp" | grep -v "^.*//" | grep -q .; then echo "layer violation: non-L0 spells s7 types"; fail=1; fi
if grep "expander" src/liii_reader.cpp 2>/dev/null | grep -v "//" | grep -q .; then echo "layer violation: L1 tiny reader depends on expander"; fail=1; fi
if ! head -n 160 goldfish/liii/boot.scm 2>/dev/null | grep -q 'load-source-file "core/gfo.scm"'; then echo "layer violation: L1 boot must load core/gfo.scm first"; fail=1; fi
if grep -R "goldfish/compiler" goldfish/expander/kernel --include="*.scm" 2>/dev/null | grep -q .; then echo "layer violation: L3 kernel depends on compiler"; fail=1; fi
if grep -n "goldfish/compiler" goldfish/expander/kernel-combined.scm 2>/dev/null | grep -q .; then echo "layer violation: L3 artifact depends on compiler"; fail=1; fi
if grep -R "goldfish/compiler" goldfish/expander/lib goldfish/expander/tree-il.scm --include="*.scm" goldfish/liii/reader.scm goldfish/core --include="*.scm" 2>/dev/null | grep -v "^.*:.*;;;" | grep -q .; then echo "layer violation: L4 must not import compiler"; fail=1; fi
if grep -R --include="*.scm" "s7_" goldfish/compiler/ 2>/dev/null | grep -q .; then echo "layer violation: L5 must be pure no s7"; fail=1; fi
if grep -R --include="*.scm" "goldfish/core\|goldfish/expander/lib" goldfish/compiler/ 2>/dev/null | grep -v "goldfish/core/ir" | grep -q .; then echo "layer violation: L5 must not import core/lib (except core/ir)"; fail=1; fi
if grep -E '#include.*expander|#include.*compiler|\(import.*goldfish/compiler' src/goldfish.hpp 2>/dev/null | grep -q .; then echo "layer violation: L7 loader must not include expander/compiler"; fail=1; fi
# L0 glue minimal: keep g_xxx primitives only, business logic in Scheme (liii/*)
# current baseline 64, keep from growing; move new business to Scheme
# target 60 after migrating find_function_libraries / load_gfproject to (liii project)
if [ "$(grep -c "glue_" src/goldfish.hpp 2>/dev/null)" -gt 64 ]; then echo "layer violation: L0 glue too many, move business logic to Scheme"; fail=1; fi
# L0 business leakage: find_function / load_gfproject should live in Scheme
if grep -q "find_function_libs_in_load_path\|load_gfproject" src/goldfish.hpp 2>/dev/null; then echo "layer warning: L0 still contains find_function/load_gfproject business, should migrate to (liii project) pure Scheme"; fi
exit $fail
