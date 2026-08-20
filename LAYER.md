# Layers

L0 host: src/gf.h, src/gf.cpp, src/gf_glue.hpp — sole s7.h inclusions; gf::host_version/date
L1 tiny: src/liii_reader.cpp (bootstrap subset only), goldfish/liii/boot.scm (first load cache/gfo.scm)
L2 expander-rt: goldfish/expander/kernel-combined.scm (self-contained, via build-combined.scm; kernel.scm includes ↔ load-kernel.scm manifest lint-synced)
L3 expander-lib: goldfish/expander/lib/*, goldfish/liii/reader.scm, goldfish/cache/gfo.scm (single gfo source; no compiler import, vm via host primitive fallback)
L4 compiler: goldfish/compiler/*, goldfish/compiler.scm, goldfish/expander/syntax-ir.scm — pure, no VM/s7/cache/lib
L5 vm: src/goldfish_vm.cpp — gf:: only, per-program VM, no Scheme includes, pre-decoded dispatch
L6 loader: src/goldfish.hpp — CLI/REPL/load-path dispatch only, no expander/compiler, no s7.h

Dependency: Ln -> L_{<n} only.
Invariants: L0 sole s7.h, L1 bootstrap-only, L2 self-contained + manifest synced, L3 gfo single source, L4 pure, L5 isolated, L6 loader-only; all machine-checked via tools/lint-layer.sh
