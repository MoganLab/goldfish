;; Native continuation cost probe. Report raw monotonic nanoseconds: wall
;; measurements are too noisy here to treat as stable thresholds.

(import (scheme base) (scheme time))

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

(define (tail-loop count)
  (let loop ((i count) (sum 0))
    (if (= i 0)
        sum
        (loop (- i 1) (+ sum i)))))

(define (capture-loop count)
  (let loop ((i count) (sum 0))
    (if (= i 0)
        sum
        (loop (- i 1)
              (+ sum (call/cc (lambda (continuation) i)))))))

(define (deep-capture depth)
  (let descend ((i depth))
    (if (= i 0)
        (call/cc (lambda (continuation) 1))
        (+ 1 (descend (- i 1))))))

(define (deep-return depth)
  (let descend ((i depth))
    (if (= i 0)
        1
        (+ 1 (descend (- i 1))))))

(measure "tail loop x 2000" (lambda () (tail-loop 2000)))
(measure "call/cc capture x 2000" (lambda () (capture-loop 2000)))
(measure "nested return at depth 500" (lambda () (deep-return 500)))
(measure "call/cc capture at depth 500" (lambda () (deep-capture 500)))
(measure "nested return at depth 5000" (lambda () (deep-return 5000)))
(measure "call/cc capture at depth 5000" (lambda () (deep-capture 5000)))
