(define-library (r7rs-audit provider)
  (import (scheme base) (r7rs-audit state))
  (export counter read-counter bump! hygienic-plus)
  (begin
    (record! 'provider)
    (define counter 10)
    (define (read-counter) counter)
    (define (bump!) (set! counter (+ counter 1)))
    (define (private-plus x) (+ counter x))
    (define-syntax hygienic-plus
      (syntax-rules () ((_ x) (private-plus x))))))
