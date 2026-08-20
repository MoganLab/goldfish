# Layers

L0 host: src/gf.h, src/gf.cpp, src/gf_glue.hpp — sole s7.h inclusions; gf::host_version/date
L1 tiny: src/liii_reader.cpp (bootstrap subset only), goldfish/liii/boot.scm (first load cache/gfo.scm)
L2 expander-rt: goldfish/expander/kernel-combined.scm (self-contained, via build-combined.scm)
L3 expander-lib: goldfish/expander/lib/*, goldfish/liii/reader.scm, goldfish/cache/gfo.scm (single gfo source)
L4 compiler: goldfish/compiler/*, goldfish/expander/syntax-ir.scm — pure, no VM/s7
L5 vm: src/goldfish_vm.cpp — spells gf::pointer/int_/scheme only
L6 loader: src/goldfish.hpp — no s7.h, via gf::

Dependency: Ln -> L_{<n} only.
Invariants: compiler no s7_, gf.h opaque, no non-L0 s7.h, no non-L0 s7 types, gfo single source, L4 no VM.
