(define-library (r7rs-audit location-a)
  (import (scheme base))
  (export counter read-counter bump!)
  (begin
    (define counter 10)
    (define (read-counter) counter)
    (define (bump!) (set! counter (+ counter 1)))))
