# Layers

L0 host: src/gf.h, src/gf.cpp — sole s7.h inclusion
L1 tiny: src/liii_reader.cpp, goldfish/liii/boot.scm
L2 expander-rt: goldfish/expander/kernel-combined.scm
L3 expander-lib: goldfish/expander/lib/*, goldfish/liii/reader.scm, goldfish/cache/gfo.scm (single gfo source)
L4 compiler: goldfish/compiler/*, goldfish/expander/syntax-ir.scm — pure, no VM
L5 vm: src/goldfish_vm.cpp — spells gf::pointer/int_/scheme only
L6 loader: src/goldfish.hpp

Dependency: Ln -> L_{<n} only.
Invariants: compiler no s7_, gf.h opaque, vm no s7 types, gfo single source.
