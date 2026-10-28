;; A7 fixture: dep-fn 的实现库，该库有对应的 hint 库。
(define-library (liii a7dep)
  (import (scheme base))
  (export dep-fn)
  (begin

    (define (dep-fn x) x)
  ) ;begin
) ;define-library
