(import (scheme base))
(define (bench-f-0 x) (+ x 0))
(define (bench-f-1 x) (+ x 1))
(define (bench-f-2 x) (+ x 2))
(define (bench-f-3 x) (+ x 3))
(unless (and (= (bench-f-0 42) 42) (= (bench-f-3 42) 45)) (error "compiled benchmark result mismatch"))
'BENCH-OK
