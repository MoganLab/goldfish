;; A7 fixture: builtin-fn 为内置风格过程，任何库都没有本地 define。
(define-library (liii a7builtin)
  (import (scheme base))
  (export builtin-fn)
) ;define-library
