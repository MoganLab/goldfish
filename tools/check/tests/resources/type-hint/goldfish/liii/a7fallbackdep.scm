;; A7 fixture: fb-fn 的实现库，该库没有 hint 库。
(define-library (liii a7fallbackdep)
  (import (scheme base))
  (export fb-fn)
  (begin

    (define (fb-fn x) x)
  ) ;begin
) ;define-library
