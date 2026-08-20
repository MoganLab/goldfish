# Layers

L0 host: src/gf.h, src/gf.cpp
L1 tiny: src/liii_reader.cpp, goldfish/liii/boot.scm
L2 expander-rt: goldfish/expander/kernel-combined.scm
L3 expander-lib: goldfish/expander/lib/*, goldfish/liii/reader.scm, goldfish/cache/gfo.scm
L4 compiler: goldfish/compiler/*, goldfish/expander/syntax-ir.scm
L5 vm: src/goldfish_vm.cpp
L6 loader: src/goldfish.hpp

Dependency: Ln -> L_{<n} only.
