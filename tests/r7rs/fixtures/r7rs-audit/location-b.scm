(define-library (r7rs-audit location-b)
  (import (scheme base))
  (export counter read-counter bump!)
  (begin
    (define counter 100)
    (define (read-counter) counter)
    (define (bump!) (set! counter (+ counter 1)))))
