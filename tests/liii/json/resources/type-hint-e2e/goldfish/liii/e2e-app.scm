;; e2e fixture: e2e-app-fn 本地实现；e2e-shared-fn 再导出自 (srfi srfi-1)。
(define-library (liii e2e-app)
  (import (scheme base) (srfi srfi-1))
  (export e2e-app-fn e2e-shared-fn)
  (begin

    (define (e2e-app-fn x) x)
  ) ;begin
) ;define-library
