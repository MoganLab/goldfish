(define-library (r7rs-audit macro-location-a)
  (import (scheme base) (r7rs-audit state-factory))
  (export counter read-counter bump!)
  (begin (define-state counter read-counter bump! 10)))
