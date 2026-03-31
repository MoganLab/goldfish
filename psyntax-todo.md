# Psyntax TODO

1. fix goldfish.hpp, that if file does not exist, it will `'./bin/goldfish a' terminated by signal SIGSEGV (Address boundary error)`
2. adapt all ./goldfish/ scheme base from s7 native define-macro to psyntax syntax-case system, using `define-macro` shim or use syntax-case rewrite it.
   NOTE: the ./goldfish/liii/oop.scm will need to be drop
