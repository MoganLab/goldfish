(import (scheme base) (scheme write))
;; Long tail-recursion loop over small integers: the tightest
;; eval-loop / int-arithmetic / less-than dispatch path.
(define (sum-loop n) (let loop ((i 0) (s 0)) (if (> i n) s (loop (+ i 1) (+ s i)))))
(write (sum-loop 2000000))
(newline)
