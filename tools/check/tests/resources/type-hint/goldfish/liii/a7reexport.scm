;; A7 fixture: own-fn 本地实现；dep-fn 再导出自 (liii a7dep)。
(define-library (liii a7reexport)
  (import (scheme base) (liii a7dep))
  (export own-fn dep-fn)
  (begin

    (define (own-fn x) x)
  ) ;begin
) ;define-library
