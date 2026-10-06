(import (scheme base) (scheme write))
;; Naive Fibonacci: deep two-way recursion, small-integer arithmetic,
;; closure calls.  Standard evaluator throughput probe.
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(write (fib 27))
(newline)
