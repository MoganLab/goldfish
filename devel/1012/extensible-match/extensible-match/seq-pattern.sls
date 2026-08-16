(library (extensible-match seq-pattern)
  (export seq-pattern-expand/nfa
          seq/unordered-pattern-expand)
  (import (rnrs (6))
          (extensible-match ast)
          (extensible-match seq-pattern nfa)
          ;; We will probably only ever have one strategy for
          ;; unordered patterns
          (extensible-match seq-pattern unordered)))
