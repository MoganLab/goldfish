(import (liii vector))

(define v (float-vector 1.0 2.0 3.0))
(define (foo v i)
  (float-vector-set! v i (if #t 1.0 (float-vector-ref v i))))
(foo v 100000000)
