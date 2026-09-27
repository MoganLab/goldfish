;; Native evaluator call and lexical-binding benchmark. Uses integer
;; monotonic nanoseconds because the native numeric tower is exact-only.

(import (scheme base) (scheme time))

(define lexical-step 7)

(define (tail-accumulate n)
  (let loop ((i n) (total 0))
    (if (= i 0)
        total
        (loop (- i 1) (+ total lexical-step)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (measure label thunk)
  (let ((result (thunk)))
    (display label)
    (display " result=")
    (display result)
    (newline)
    (do ((sample 0 (+ sample 1))) ((= sample 5))
      (let* ((start (monotonic-nanosecond))
             (value (thunk))
             (elapsed (- (monotonic-nanosecond) start)))
        (display "  ns=")
        (display elapsed)
        (display " result=")
        (display value)
        (newline)))))

(measure "tail lexical loop 20000" (lambda () (tail-accumulate 20000)))
(measure "recursive fib 24" (lambda () (fib 24)))
