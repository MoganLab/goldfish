;; e2e fixture: e2e-shared-fn 的实现库。
(define-library (srfi srfi-1)
  (import (scheme base))
  (export e2e-shared-fn)
  (begin

    (define (e2e-shared-fn x) x)
  ) ;begin
) ;define-library
